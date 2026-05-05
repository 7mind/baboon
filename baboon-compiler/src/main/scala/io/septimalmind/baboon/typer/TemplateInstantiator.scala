package io.septimalmind.baboon.typer

import io.septimalmind.baboon.parser.model.*
import io.septimalmind.baboon.parser.model.issues.{BaboonIssue, TyperIssue}
import io.septimalmind.baboon.typer.model.*
import izumi.functional.bio.{Error2, F}
import izumi.fundamentals.collections.nonempty.NEList

/** PR-29.5 typer-early pass: rewrites every `RawTLDef.Alias` whose RHS is a
  * `RawTypeRef.Constructor` pointing at a registered template into the
  * corresponding concrete `RawTLDef.{DTO|ADT|Contract|Service}` keyed by the
  * alias's name (locked decision #4 — monomorphisation id = alias id).
  *
  * Algorithm per spec §3.5 and plan §3.4:
  *   1. Walk every `RawTLDef.Alias` in the post-ADT-expansion member list.
  *   2. If the alias RHS is `RawTypeRef.Constructor(name, args, prefix)` and
  *      `(pkg, owner, name)` hits the registry, substitute.
  *   3. Validate arity (`args.size == template.typeParams.size` → matrix #3).
  *   4. Walk the cached raw body; for every `RawTypeRef.Simple(p, Nil)` where
  *      `p` is a type-parameter name, replace with the resolved argument.
  *      Walk `RawTypeRef.Constructor.params` and `RawTypeRef.AnyRef.underlying`
  *      recursively.
  *   5. If during the walk any `RawTypeRef.Constructor.name` resolves to a
  *      registered template (including self-reference), emit
  *      `TemplateInstantiationInForbiddenPosition` (spec §4, matrix #1 / decision #3).
  *   6. Synthesize the concrete `RawTLDef.{DTO|ADT|Contract|Service}` with
  *      - `name`   = alias name (locked decision #4)
  *      - `meta`   = alias meta
  *      - `derived` = alias's `derived` (locked decision #6); template body's
  *        `derived` must be empty (enforced in PR-29.7; here we merge for
  *        safety, preserving the alias-side annotation as primary)
  *      - `typeParams = Nil` (materialised type is no longer a template)
  *   7. Replace the `RawTLDef.Alias` with the synthesized concrete node.
  *   8. Aliases NOT pointing at a registered template pass through unchanged.
  *
  * Mirrors `AdtInheritanceExpander`'s class/trait shape and error-accumulation
  * pattern.
  */
trait TemplateInstantiator[F[+_, +_]] {
  def instantiate(
    pkg: Pkg,
    members: Seq[RawTLDef],
    registry: TemplateRegistry,
  ): F[NEList[BaboonIssue], Seq[RawTLDef]]

  /** PR-33.2-D05: pre-toposort validation. Walks every DTO/Identifier/Contract/ADT member list
    * and emits `TyperIssue.TemplateNotInstantiated` for any `+/-/^ Foo` arm whose head names a
    * registered template AND whose `args = None`. This must run BEFORE `BaboonTyper.order`
    * because `hardDepsOfRawDefn` reports bare refs as hard deps; the toposort then resolves
    * them through the regular scope tree (which excludes templates) and produces a
    * confusing `NameNotFound` instead of the precise diagnostic.
    */
  def validateNoBareTemplateRefs(
    pkg: Pkg,
    members: Seq[RawTLDef],
    registry: TemplateRegistry,
  ): F[NEList[BaboonIssue], Unit]
}

object TemplateInstantiator {

  /** Merge alias-side and template-body-side `RawNodeMeta` to produce the
    * synthesized concrete type's meta per spec §6 (Q1 lock).
    *
    * - The result's `pos` is the alias's `pos` (the user-visible site of the
    *   monomorphisation; existing tests anchor diagnostics there).
    * - The result's `docs.prefix` is computed as follows:
    *   - both alias and template prefix docs present: synthesize a raw doc
    *     whose body, after `DocFormat.cleanPrefix`, equals
    *     `aliasCleaned + "\n\n" + templateCleaned`.
    *   - only one present: use that one.
    *   - neither: `None`.
    * - `docs.suffix` is always `None` (alias and type declarations cannot
    *   carry postfix `//!` per spec §3.3).
    */
  def mergeAliasAndTemplateMeta(aliasMeta: RawNodeMeta, templateMeta: RawNodeMeta): RawNodeMeta = {
    val aliasPrefix    = aliasMeta.docs.prefix
    val templatePrefix = templateMeta.docs.prefix

    val mergedPrefix: Option[RawDocComment] = (aliasPrefix, templatePrefix) match {
      case (None, None)             => None
      case (Some(_), None)          => aliasPrefix
      case (None, Some(_))          => templatePrefix
      case (Some(aDoc), Some(tDoc)) =>
        // Build a synthetic raw doc whose `cleanPrefix` output equals
        // `aliasCleaned + "\n\n" + templateCleaned`. We construct the synthetic
        // body without any `*` continuation markers and without leading
        // whitespace so the `DocFormat` common-prefix calculation is a no-op
        // (degenerate empty prefix), and the cleaned text round-trips to the
        // expected merged form. The merged `pos` is the alias's `pos` since
        // that is the user-facing site for the synthesized type.
        val aliasCleaned    = DocFormat.cleanPrefix(aDoc.raw)
        val templateCleaned = DocFormat.cleanPrefix(tDoc.raw)
        val mergedBody      = s"$aliasCleaned\n\n$templateCleaned"
        val syntheticRaw    = s"/**\n$mergedBody\n*/"
        Some(RawDocComment(syntheticRaw, aDoc.pos))
    }

    aliasMeta.copy(docs = RawDocs(prefix = mergedPrefix, suffix = None))
  }

  /** Kind of structural-composition arm being lowered (PR-33.2 / M33). */
  private sealed trait ArmKind
  private object ArmKind {
    case object Plus  extends ArmKind
    case object Minus extends ArmKind
    case object Caret extends ArmKind
  }

  class Impl[F[+_, +_]: Error2] extends TemplateInstantiator[F] {

    // Canonical set of builtin constructor heads (collections + any).
    // Derived from TypeId.Builtins so that adding a new builtin keeps this in sync automatically.
    private val builtinConstructorNames: Set[String] = Set(
      TypeId.Builtins.map.name.name,
      TypeId.Builtins.opt.name.name,
      TypeId.Builtins.lst.name.name,
      TypeId.Builtins.set.name.name,
      TypeId.Builtins.any.name.name,
    )

    /** Maximum recursion depth for structural-arm template instantiation (PR-33.2 / M33).
      *
      * Each level represents one full substitution: `template Outer[U] { + Inner[U] }`
      * instantiated as `Outer[i32]` produces a body containing `+ Inner[i32]`, which is then
      * re-substituted as the next level. Pathological mutual-recursion shapes
      * (`A[U] = + B[U]; B[U] = + A[U]`) terminate via either the cycle-detection set OR this
      * depth limit (whichever fires first). PR-33.4 will refine the diagnostic; PR-33.2 only
      * needs to ensure termination.
      */
    private val structuralArmRecursionLimit: Int = 32

    override def instantiate(
      pkg: Pkg,
      members: Seq[RawTLDef],
      registry: TemplateRegistry,
    ): F[NEList[BaboonIssue], Seq[RawTLDef]] = {
      instantiateRecursive(pkg, members, registry, ownerForCurrent = Owner.Toplevel, nsPath = List.empty)
    }

    override def validateNoBareTemplateRefs(
      pkg: Pkg,
      members: Seq[RawTLDef],
      registry: TemplateRegistry,
    ): F[NEList[BaboonIssue], Unit] = {
      F.traverseAccumErrors_(members)(m => validateBareTemplateRefsInTLDef(pkg, m, registry, ownerForCurrent = Owner.Toplevel))
    }

    private def validateBareTemplateRefsInTLDef(
      pkg: Pkg,
      tldef: RawTLDef,
      registry: TemplateRegistry,
      ownerForCurrent: Owner,
    ): F[NEList[BaboonIssue], Unit] = {
      tldef match {
        case RawTLDef.DTO(_, dto) =>
          validateBareTemplateRefsInMembers(pkg, dto.members, registry, dto.name.name)
        case RawTLDef.Identifier(_, idi) =>
          validateBareTemplateRefsInMembers(pkg, idi.members, registry, idi.name.name)
        case RawTLDef.Contract(_, c) =>
          validateBareTemplateRefsInMembers(pkg, c.members, registry, c.name.name)
        case RawTLDef.ADT(_, adt) =>
          F.traverseAccumErrors_(adt.members) {
            case m: RawAdtMemberDto      => validateBareTemplateRefsInMembers(pkg, m.dto.members, registry, m.dto.name.name)
            case m: RawAdtMemberContract => validateBareTemplateRefsInMembers(pkg, m.contract.members, registry, m.contract.name.name)
            case _                       => F.unit
          }
        case RawTLDef.Namespace(ns) =>
          val nextOwner = ownerForCurrent match {
            case Owner.Toplevel => Owner.Ns(List(TypeName(ns.name.name)))
            case Owner.Ns(path) => Owner.Ns(path.toList :+ TypeName(ns.name.name))
            case adt: Owner.Adt => throw new IllegalStateException(s"Namespace declared inside an ADT scope is structurally impossible; got $adt")
          }
          F.traverseAccumErrors_(ns.defns)(m => validateBareTemplateRefsInTLDef(pkg, m, registry, nextOwner))
        case _ =>
          F.unit
      }
    }

    private def validateBareTemplateRefsInMembers(
      pkg: Pkg,
      members: Seq[RawDtoMember],
      registry: TemplateRegistry,
      receivingName: String,
    ): F[NEList[BaboonIssue], Unit] = {
      F.traverseAccumErrors_(members) {
        case p: RawDtoMember.ParentDef if p.args.isEmpty && refersToRegisteredTemplate(pkg, p.parent, registry) =>
          F.fail(BaboonIssue.of(TyperIssue.TemplateNotInstantiated(p.parent.path.last.name, receivingName, p.meta)))
        case u: RawDtoMember.UnparentDef if u.args.isEmpty && refersToRegisteredTemplate(pkg, u.parent, registry) =>
          F.fail(BaboonIssue.of(TyperIssue.TemplateNotInstantiated(u.parent.path.last.name, receivingName, u.meta)))
        case i: RawDtoMember.IntersectionDef if i.args.isEmpty && refersToRegisteredTemplate(pkg, i.parent, registry) =>
          F.fail(BaboonIssue.of(TyperIssue.TemplateNotInstantiated(i.parent.path.last.name, receivingName, i.meta)))
        case _ =>
          F.unit
      }
    }

    private def instantiateRecursive(
      pkg: Pkg,
      members: Seq[RawTLDef],
      registry: TemplateRegistry,
      ownerForCurrent: Owner,
      nsPath: List[TypeName],
    ): F[NEList[BaboonIssue], Seq[RawTLDef]] = {
      for {
        // Pass 1 (PR-29.5): replace alias-over-template members with concrete RawTLDef nodes.
        afterAlias <- F.traverseAccumErrors(members)(m => processMember(pkg, m, registry, ownerForCurrent, nsPath)).map(_.flatten)
        // Pass 2 (PR-33.2 / M33): lower `+/-/^ Template[Args]` structural arms inside DTO/
        // Contract/Identifier/ADT bodies via inline substitution (Approach M1 for `+`/`-`,
        // M3 for `^`). Decision documented in RawDtoMember.IntersectionFields scaladoc.
        afterStructural <- F.traverseAccumErrors(afterAlias)(m => lowerStructuralArmsInTLDef(pkg, m, registry, ownerForCurrent))
      } yield afterStructural
    }

    // ─── PR-33.2 / M33: structural-arm lowering ───────────────────────────────

    /** Walk a single TLDef and lower any `+/-/^ Template[Args]` arms inside its DTO / Contract
      * / Identifier / ADT body via inline substitution (Approach M1 for `+`/`-`, M3 for `^`).
      *
      * Architectural choice: M1 (pure inline) for `+` and `-`, M3 (one new sealed branch
      * `RawDtoMember.IntersectionFields`) for `^`. Rationale:
      *   - `+` and `-` operate on field-set semantics that map cleanly to existing FieldDef /
      *     UnfieldDef arms (translator path unchanged for those arms; equality-by-Field for `-`).
      *   - `^` requires a list of fields to survive to translator-time intersection. The existing
      *     IntersectionDef path resolves a ScopedRef against a registered DTO; inline substitution
      *     has no such id (M29 invariant: no synthetic id for template instantiations). We add ONE
      *     new sealed branch that carries the substituted RawFields directly.
      */
    private def lowerStructuralArmsInTLDef(
      pkg: Pkg,
      tldef: RawTLDef,
      registry: TemplateRegistry,
      ownerForCurrent: Owner,
    ): F[NEList[BaboonIssue], RawTLDef] = {
      tldef match {
        case dtoTL @ RawTLDef.DTO(_, dto) =>
          lowerStructuralArmsInMembers(pkg, dto.members, registry, ownerForCurrent, dto.name.name, depth = 0, cycleSet = Set.empty).map {
            newMembers => dtoTL.copy(value = dto.copy(members = newMembers))
          }
        case idTL @ RawTLDef.Identifier(_, idi) =>
          lowerStructuralArmsInMembers(pkg, idi.members, registry, ownerForCurrent, idi.name.name, depth = 0, cycleSet = Set.empty).map {
            newMembers => idTL.copy(value = idi.copy(members = newMembers))
          }
        case cTL @ RawTLDef.Contract(_, c) =>
          lowerStructuralArmsInMembers(pkg, c.members, registry, ownerForCurrent, c.name.name, depth = 0, cycleSet = Set.empty).map {
            newMembers => cTL.copy(value = c.copy(members = newMembers))
          }
        case adtTL @ RawTLDef.ADT(_, adt) =>
          // ADT branches' nested DTO/Contract bodies may carry `+/-/^ Template[Args]` arms.
          // Walk each branch's body.
          F.traverseAccumErrors(adt.members) {
            case m: RawAdtMemberDto =>
              lowerStructuralArmsInMembers(pkg, m.dto.members, registry, ownerForCurrent, m.dto.name.name, depth = 0, cycleSet = Set.empty).map {
                newMembers => m.copy(dto = m.dto.copy(members = newMembers))
              }
            case m: RawAdtMemberContract =>
              lowerStructuralArmsInMembers(pkg, m.contract.members, registry, ownerForCurrent, m.contract.name.name, depth = 0, cycleSet = Set.empty).map {
                newMembers => m.copy(contract = m.contract.copy(members = newMembers))
              }
            case other =>
              F.pure(other)
          }.map {
            newAdtMembers => adtTL.copy(value = adt.copy(members = newAdtMembers))
          }
        case nsTL @ RawTLDef.Namespace(ns) =>
          val nextOwner = Owner.Ns(nsPathFromOwner(ownerForCurrent) :+ TypeName(ns.name.name))
          F.traverseAccumErrors(ns.defns)(m => lowerStructuralArmsInTLDef(pkg, m, registry, nextOwner)).map {
            newDefns => nsTL.copy(value = ns.copy(defns = newDefns))
          }
        case other =>
          F.pure(other)
      }
    }

    /** Convert an `Owner` to its namespace path (List[TypeName]) for further nesting. */
    private def nsPathFromOwner(o: Owner): List[TypeName] = o match {
      case Owner.Toplevel => List.empty
      case Owner.Ns(path) => path.toList
      case Owner.Adt(_)   => List.empty // ADT-owned scopes don't compose with template registry keys.
    }

    /** Walk a member list and lower every `+/-/^ Template[Args]` arm via inline substitution.
      *
      * Recursion: when the substituted body contains another `+/-/^ Template[Args]` arm
      * (e.g. `template Outer[U] { + Inner[U] }` instantiated at `Outer[i32]` produces a body
      * with `+ Inner[i32]`), the lowered body is fed back through the same walk with a depth
      * counter and a cycle-detection set keyed by `(receiverName, templateName, argTuple)`.
      *
      * Termination guard: depth-limit (default 32) OR cycle-set hit. Either fires
      * `TyperIssue.CircularInheritance` per §3.f. PR-33.4 will refine the diagnostic.
      */
    private def lowerStructuralArmsInMembers(
      pkg: Pkg,
      members: Seq[RawDtoMember],
      registry: TemplateRegistry,
      ownerForCurrent: Owner,
      receivingName: String,
      depth: Int,
      cycleSet: Set[(String, String, String)],
    ): F[NEList[BaboonIssue], Seq[RawDtoMember]] = {
      F.flatTraverseAccumErrors(members) {
        case p: RawDtoMember.ParentDef if p.args.isDefined =>
          lowerOneArm(pkg, p.parent, p.args.get, p.meta, registry, ownerForCurrent, receivingName, depth, cycleSet, ArmKind.Plus)
        case u: RawDtoMember.UnparentDef if u.args.isDefined =>
          lowerOneArm(pkg, u.parent, u.args.get, u.meta, registry, ownerForCurrent, receivingName, depth, cycleSet, ArmKind.Minus)
        case i: RawDtoMember.IntersectionDef if i.args.isDefined =>
          lowerOneArm(pkg, i.parent, i.args.get, i.meta, registry, ownerForCurrent, receivingName, depth, cycleSet, ArmKind.Caret)
        case other =>
          F.pure(List(other))
      }
    }

    /** PR-33.2-D05: true iff the ScopedRef's last segment names a registered template under the
      * effective owner. Honours explicit prefix per PR-29.15.
      */
    private def refersToRegisteredTemplate(
      pkg: Pkg,
      ref: ScopedRef,
      registry: TemplateRegistry,
    ): Boolean = {
      val pathSegments = ref.path.toList.map(_.name)
      val (prefixSegments, headName) = (pathSegments.init, pathSegments.last)
      // For an unprefixed bare reference, the template might be registered under any owner in
      // the same package (the structural-arm position is in the receiver's scope, but a
      // sibling-namespace template could also be a forgotten-arguments candidate). Mirror the
      // same broad lookup used by `substituteTypeRef`'s matrix #1 check (pkg-wide, any-owner)
      // for the empty-prefix case; for the prefixed case, narrow to the prefix-derived owner.
      if (prefixSegments.isEmpty) {
        registry.templates.keys.exists {
          case (kPkg, _, tname) => kPkg == pkg && tname.name == headName
        }
      } else {
        val ownerForLookup = Owner.Ns(prefixSegments.map(TypeName(_)))
        registry.templates.contains((pkg, ownerForLookup, TypeName(headName)))
      }
    }

    /** Resolve one `+/-/^ Template[Args]` arm into a list of `RawDtoMember`s to splice into the
      * receiver's member list. Validates arity, substitutes the body, recursively lowers any
      * nested structural arms, and converts the result according to the operator.
      */
    private def lowerOneArm(
      pkg: Pkg,
      parent: ScopedRef,
      args: NEList[RawTypeRef],
      armMeta: RawNodeMeta,
      registry: TemplateRegistry,
      ownerForCurrent: Owner,
      receivingName: String,
      depth: Int,
      cycleSet: Set[(String, String, String)],
      armKind: ArmKind,
    ): F[NEList[BaboonIssue], List[RawDtoMember]] = {
      // Termination guard: depth limit OR cycle detection. Per §3.f, reuse CircularInheritance.
      // The existing `order` toposort emits CircularInheritance with a `ToposortError[TypeId.User]`
      // payload; we don't have that shape here so we fabricate a synthetic single-edge matrix
      // (receivingName -> templateName) so the printer's `niceList()` produces a meaningful
      // line instead of empty payload. PR-33.2-D01.
      val pathSegments = parent.path.toList.map(_.name)
      val headName     = pathSegments.last
      val syntheticMatrix = izumi.fundamentals.graphs.struct.AdjacencyList(
        Map[TypeId.User, Set[TypeId.User]](
          TypeId.User(pkg, Owner.Toplevel, TypeName(receivingName)) ->
            Set(TypeId.User(pkg, Owner.Toplevel, TypeName(headName)))
        )
      )
      if (depth >= structuralArmRecursionLimit) {
        F.fail(
          BaboonIssue.of(
            TyperIssue.CircularInheritance(
              error = izumi.fundamentals.graphs.ToposortError.UnexpectedLoop[TypeId.User](
                done   = Seq.empty,
                matrix = syntheticMatrix,
              ),
              meta = armMeta,
            )
          )
        )
      } else {
        // Resolve the head against the template registry. Honour explicit prefix (cross-namespace
        // per PR-29.15); otherwise fall back to the receiver's own owner.
        // The ScopedRef has a `path`: (segments...) where the last segment is the type name and
        // any preceding segments are namespace prefix.
        // (`pathSegments` and `headName` were captured earlier for the synthetic recursion-guard matrix.)
        val prefixSegments = pathSegments.init
        val ownerForLookup: Owner = if (prefixSegments.isEmpty) {
          ownerForCurrent
        } else {
          Owner.Ns(prefixSegments.map(TypeName(_)))
        }
        val key = (pkg, ownerForLookup, TypeName(headName))
        registry.templates.get(key) match {
          case None =>
            // Maybe it's a builtin or a non-template ref. Builtins don't make sense in
            // structural-arm position with args; emit NotATemplate. The aliasName carries the
            // receiving DTO's name for diagnostic context.
            F.fail(
              BaboonIssue.of(
                TyperIssue.NotATemplate(
                  head      = headName,
                  aliasName = receivingName,
                  meta      = armMeta,
                )
              )
            )
          case Some(body) =>
            val templateName = headName
            val argList      = args.toList
            // Arity check (matrix #3).
            if (argList.size != body.typeParams.size) {
              F.fail(
                BaboonIssue.of(
                  TyperIssue.TemplateArityMismatch(
                    templateName = templateName,
                    aliasName    = receivingName,
                    expected     = body.typeParams.size,
                    actual       = argList.size,
                    meta         = armMeta,
                  )
                )
              )
            } else {
              // Matrix #2 (PR-33.3-D02): structural-arm template arguments must not themselves
              // be template instantiations (spec §2.5.2). Mirrors `processMember`'s alias-RHS
              // matrix-#2 walk (lines 686-708) at the structural-arm position. For prefixed
              // forms (e.g. `ns.Foo[T]`), resolve the owner from the prefix so cross-namespace
              // template-in-arg references also produce the precise diagnostic.
              val badArg = argList.collectFirst {
                case argCtor: RawTypeRef.Constructor if {
                      val argOwner: Owner =
                        if (argCtor.prefix.isEmpty) ownerForCurrent
                        else Owner.Ns(argCtor.prefix.map(p => TypeName(p.name)))
                      registry.templates.keys.exists { case (kPkg, kOwner, tname) => tname.name == argCtor.name.name && kPkg == pkg && kOwner == argOwner }
                    } =>
                  argCtor.name.name
              }
              if (badArg.isDefined) {
                F.fail(
                  BaboonIssue.of(
                    TyperIssue.TemplateInstantiationInForbiddenPosition(
                      containingTemplateName = templateName,
                      instantiatedName       = badArg.get,
                      meta                   = armMeta,
                    )
                  )
                )
              } else {
              // Cycle key: (receiver, template, arg-string-repr).
              val argTupleKey = argList.map(_.toString).mkString(",")
              val cycleKey    = (receivingName, templateName, argTupleKey)
              if (cycleSet.contains(cycleKey)) {
                F.fail(
                  BaboonIssue.of(
                    TyperIssue.CircularInheritance(
                      error = izumi.fundamentals.graphs.ToposortError.UnexpectedLoop[TypeId.User](
                        done   = Seq.empty,
                        matrix = syntheticMatrix,
                      ),
                      meta = armMeta,
                    )
                  )
                )
              } else {
                // Substitute the body's members.
                val substMap: Map[String, RawTypeRef] = body.typeParams.map(_.name).zip(argList).toMap
                body.rawDefn match {
                  case RawTemplateDefn.Dto(rawDto) =>
                    for {
                      substMembers <- substituteMembers(rawDto.members, substMap, templateName, registry, armMeta, pkg)
                      // Recursive lowering: the substituted body may itself carry
                      // `+/-/^ Template[Args]` arms. Re-enter lowering with incremented depth
                      // and updated cycle set.
                      loweredMembers <- lowerStructuralArmsInMembers(
                        pkg,
                        substMembers,
                        registry,
                        ownerForCurrent,
                        receivingName,
                        depth + 1,
                        cycleSet + cycleKey,
                      )
                      result <- convertLoweredArm(loweredMembers, armKind, armMeta, templateName, receivingName)
                    } yield result
                  case RawTemplateDefn.Contract(rawC) =>
                    for {
                      substMembers <- substituteMembers(rawC.members, substMap, templateName, registry, armMeta, pkg)
                      loweredMembers <- lowerStructuralArmsInMembers(
                        pkg,
                        substMembers,
                        registry,
                        ownerForCurrent,
                        receivingName,
                        depth + 1,
                        cycleSet + cycleKey,
                      )
                      result <- convertLoweredArm(loweredMembers, armKind, armMeta, templateName, receivingName)
                    } yield result
                  case _ =>
                    // ADT or Service template in `+/-/^` position: not meaningful as a
                    // structural-composition arm. Reuse NotATemplate with the head name.
                    F.fail(
                      BaboonIssue.of(
                        TyperIssue.NotATemplate(
                          head      = headName,
                          aliasName = receivingName,
                          meta      = armMeta,
                        )
                      )
                    )
                }
              }
              }
            }
        }
      }
    }

    /** Convert a lowered (substituted, recursively-flattened) member list into the appropriate
      * splice for the operator:
      *   - `+`: keep the member list as-is (FieldDefs splice in directly).
      *   - `-`: convert each FieldDef into an UnfieldDef (the existing `removed` walk handles them
      *     by Field equality, identical to `- ParentDto`).
      *   - `^`: extract the FieldDef list, wrap as one IntersectionFields node.
      *
      * Non-FieldDef members (ContractRef, leftover ParentDef-with-args = None, etc.) are dropped
      * for `-` and `^` because intersection / removal only operates on field-by-field semantics.
      * For `+`, non-FieldDef members from a recursively-substituted template body are preserved
      * verbatim (e.g. a ContractRef from the template body becomes a ContractRef on the receiver).
      */
    private def convertLoweredArm(
      loweredMembers: Seq[RawDtoMember],
      armKind: ArmKind,
      armMeta: RawNodeMeta,
      templateName: String,
      receivingName: String,
    ): F[NEList[BaboonIssue], List[RawDtoMember]] = {
      armKind match {
        case ArmKind.Plus =>
          F.pure(loweredMembers.toList)
        case ArmKind.Minus =>
          // PR-33.2-D02: `-` is defined only over a flat field list. If the substituted body
          // contains any non-FieldDef member (e.g. a concrete `+ ParentRef`, ContractRef, etc.),
          // emit `TemplateBodyNotFlatForRemoval` rather than silently drop it — the dropped
          // contributor would mask a semantically incorrect removal.
          checkFlatOrFail(loweredMembers, "minus", armMeta, templateName, receivingName).map {
            fieldDefs =>
              fieldDefs.map(f => RawDtoMember.UnfieldDef(f.field, f.meta)).toList
          }
        case ArmKind.Caret =>
          // PR-33.2-D02: same flatness invariant as `-`. PR-33.2-D06: per-field meta is
          // preserved by carrying FieldDef instances (each retaining its own meta) through
          // IntersectionFields rather than only RawField.
          checkFlatOrFail(loweredMembers, "caret", armMeta, templateName, receivingName).map {
            fieldDefs =>
              List(RawDtoMember.IntersectionFields(fieldDefs, armMeta))
          }
      }
    }

    /** PR-33.2-D02: enforce that the substituted body for `-` or `^` contains only FieldDef
      * members. Returns the FieldDef list on success, fails with `TemplateBodyNotFlatForRemoval`
      * on the first non-FieldDef encountered.
      */
    private def checkFlatOrFail(
      loweredMembers: Seq[RawDtoMember],
      kind: String,
      armMeta: RawNodeMeta,
      templateName: String,
      receivingName: String,
    ): F[NEList[BaboonIssue], Seq[RawDtoMember.FieldDef]] = {
      val offending = loweredMembers.collectFirst {
        case m if !m.isInstanceOf[RawDtoMember.FieldDef] => m
      }
      offending match {
        case Some(m) =>
          F.fail(
            BaboonIssue.of(
              TyperIssue.TemplateBodyNotFlatForRemoval(
                templateName        = templateName,
                receivingName       = receivingName,
                kind                = kind,
                offendingMemberKind = m.getClass.getSimpleName,
                meta                = armMeta,
              )
            )
          )
        case None =>
          F.pure(loweredMembers.collect { case f: RawDtoMember.FieldDef => f })
      }
    }


    private def processMember(
      pkg: Pkg,
      member: RawTLDef,
      registry: TemplateRegistry,
      ownerForCurrent: Owner,
      nsPath: List[TypeName],
    ): F[NEList[BaboonIssue], List[RawTLDef]] = {
      member match {

        case aliasTL @ RawTLDef.Alias(root, alias) =>
          alias.target match {
            case ctor: RawTypeRef.Constructor =>
              // Resolve the template key. Honour explicit prefix if present; fall back to
              // the alias's own owner for same-package/same-namespace references.
              val key = resolveTemplateKey(pkg, ownerForCurrent, ctor)
              registry.templates.get(key) match {
                case Some(body) =>
                  instantiateAlias(pkg, root, alias, body, registry, key, ownerForCurrent).map(List(_))
                case None =>
                  // Constructor whose head doesn't hit the registry.
                  // If the head is a builtin collection (lst, set, opt, map) or `any`, pass
                  // through — BaboonTranslator will handle it. Otherwise the user wrote
                  // something like `type Y = i32[X]`, which is matrix #8.
                  val headName = ctor.name.name
                  // Derived from TypeId.Builtins canonical constants — single source of truth.
                  val isBuiltin = builtinConstructorNames.contains(headName)
                  if (isBuiltin) {
                    F.pure(List(aliasTL))
                  } else {
                    F.fail(
                      BaboonIssue.of(
                        TyperIssue.NotATemplate(
                          head      = headName,
                          aliasName = alias.name.name,
                          meta      = alias.meta,
                        )
                      )
                    )
                  }
              }
            case simple: RawTypeRef.Simple =>
              // Bare Simple reference (no brackets). Check whether the name refers to a
              // registered template. If so, the user forgot the type arguments — matrix #7.
              // For prefixed forms (e.g. `ns.Foo`), resolve the owner from the prefix so that
              // cross-namespace uninstantiated template references also produce the precise diagnostic.
              val lookupOwner: Owner = if (simple.prefix.isEmpty) {
                ownerForCurrent
              } else {
                Owner.Ns(simple.prefix.map(p => TypeName(p.name)))
              }
              val maybeTemplateKey = registry.templates.keys.find {
                case (kPkg, kOwner, tname) =>
                  tname.name == simple.name.name &&
                  kPkg == pkg &&
                  kOwner == lookupOwner
              }
              maybeTemplateKey match {
                case Some(_) =>
                  F.fail(
                    BaboonIssue.of(
                      TyperIssue.TemplateNotInstantiated(
                        templateName = simple.name.name,
                        aliasName    = alias.name.name,
                        meta         = alias.meta,
                      )
                    )
                  )
                case None =>
                  // Not a template — plain alias. Pass through.
                  F.pure(List(aliasTL))
              }
            case _ =>
              // AnyRef alias target — not a template instantiation. Pass through.
              F.pure(List(aliasTL))
          }

        case nsTL @ RawTLDef.Namespace(ns) =>
          val nextNsPath = nsPath :+ TypeName(ns.name.name)
          val nextOwner  = Owner.Ns(nextNsPath)
          instantiateRecursive(pkg, ns.defns, registry, nextOwner, nextNsPath).map {
            rewrittenChildren =>
              // If all children were templates and were removed, drop the namespace entirely.
              // Keeping an empty namespace would cause ScopeCannotBeEmpty in the scope-build phase.
              if (rewrittenChildren.isEmpty) Nil
              else List(nsTL.copy(value = ns.copy(defns = rewrittenChildren)))
          }

        case other =>
          F.pure(List(other))
      }
    }

    /** Instantiate one alias-over-template into the corresponding concrete `RawTLDef`. */
    private def instantiateAlias(
      pkg: Pkg,
      root: Boolean,
      alias: RawAlias,
      body: TemplateBody,
      registry: TemplateRegistry,
      templateKey: (Pkg, Owner, TypeName),
      ownerForCurrent: Owner,
    ): F[NEList[BaboonIssue], RawTLDef] = {
      val ctor = alias.target.asInstanceOf[RawTypeRef.Constructor]
      val args = ctor.params.toList

      // Arity check (matrix #3).
      if (args.size != body.typeParams.size) {
        F.fail(
          BaboonIssue.of(
            TyperIssue.TemplateArityMismatch(
              templateName = templateKey._3.name,
              aliasName    = alias.name.name,
              expected     = body.typeParams.size,
              actual       = args.size,
              meta         = alias.meta,
            )
          )
        )
      } else {
        // Matrix #2: type arguments must not themselves be template instantiations (spec §2.5.2).
        // Walk each argument; if it is a Constructor whose head names a registered template, reject.
        // For prefixed forms (e.g. `ns.Foo[T]`), resolve the owner from the prefix so that
        // cross-namespace template-in-arg references also produce the precise diagnostic.
        val badArg = args.collectFirst {
          case argCtor: RawTypeRef.Constructor if {
                val argOwner: Owner =
                  if (argCtor.prefix.isEmpty) ownerForCurrent
                  else Owner.Ns(argCtor.prefix.map(p => TypeName(p.name)))
                registry.templates.keys.exists { case (kPkg, kOwner, tname) => tname.name == argCtor.name.name && kPkg == pkg && kOwner == argOwner }
              } =>
            argCtor.name.name
        }
        badArg match {
          case Some(innerTemplate) =>
            F.fail(
              BaboonIssue.of(
                TyperIssue.TemplateInstantiationInForbiddenPosition(
                  containingTemplateName = templateKey._3.name,
                  instantiatedName       = innerTemplate,
                  meta                   = alias.meta,
                )
              )
            )
          case None =>
            val substMap: Map[String, RawTypeRef] =
              body.typeParams.map(_.name).zip(args).toMap

            // Per spec §6 (Q1 lock): the synthesized type's type-level doc is
            // `aliasCleaned + "\n\n" + templateCleaned` when both are present;
            // either alone when only one is. Field-level docs propagate
            // verbatim from the template body via the substitution walk.
            // We compute the merged meta once and reuse it for every variant.
            val templateMeta = body.rawDefn match {
              case RawTemplateDefn.Dto(r)      => r.meta
              case RawTemplateDefn.Adt(r)      => r.meta
              case RawTemplateDefn.Contract(r) => r.meta
              case RawTemplateDefn.Service(r)  => r.meta
            }
            val mergedMeta = TemplateInstantiator.mergeAliasAndTemplateMeta(alias.meta, templateMeta)

            // Walk the template body and apply substitution.
            body.rawDefn match {
              case RawTemplateDefn.Dto(raw) =>
                // Body-side derived is rejected at registry-build time (TemplateBodyCarriesDerived,
                // spec §5.3). By the time we reach this point the invariant holds.
                assert(raw.derived.isEmpty, s"template body '${raw.name.name}' carries derived — should have been rejected by TemplateRegistryBuilder")
                for {
                  rewrittenMembers <- substituteMembers(raw.members, substMap, templateKey._3.name, registry, alias.meta, pkg)
                } yield {
                  RawTLDef.DTO(
                    root,
                    raw.copy(
                      name       = alias.name,
                      members    = rewrittenMembers,
                      derived    = alias.derived,
                      meta       = mergedMeta,
                      typeParams = Nil,
                    ),
                  )
                }

              case RawTemplateDefn.Adt(raw) =>
                // Body-side derived is rejected at registry-build time (TemplateBodyCarriesDerived,
                // spec §5.3). By the time we reach this point the invariant holds.
                assert(raw.derived.isEmpty, s"template body '${raw.name.name}' carries derived — should have been rejected by TemplateRegistryBuilder")
                for {
                  rewrittenAdtMembers <- substituteAdtMembers(raw.members, substMap, templateKey._3.name, registry, alias.meta, pkg)
                } yield {
                  RawTLDef.ADT(
                    root,
                    raw.copy(
                      name       = alias.name,
                      members    = rewrittenAdtMembers,
                      derived    = alias.derived,
                      meta       = mergedMeta,
                      typeParams = Nil,
                    ),
                  )
                }

              case RawTemplateDefn.Contract(raw) =>
                for {
                  rewrittenMembers <- substituteMembers(raw.members, substMap, templateKey._3.name, registry, alias.meta, pkg)
                } yield {
                  RawTLDef.Contract(
                    root,
                    raw.copy(
                      name       = alias.name,
                      members    = rewrittenMembers,
                      meta       = mergedMeta,
                      typeParams = Nil,
                    ),
                  )
                }

              case RawTemplateDefn.Service(raw) =>
                for {
                  rewrittenFuncs <- substituteFuncs(raw.defns, substMap, templateKey._3.name, registry, alias.meta, pkg)
                } yield {
                  RawTLDef.Service(
                    root,
                    raw.copy(
                      name       = alias.name,
                      defns      = rewrittenFuncs,
                      meta       = mergedMeta,
                      typeParams = Nil,
                    ),
                  )
                }
            }
        }
      }
    }

    // ─── substitution helpers ─────────────────────────────────────────────────

    /** Substitute all `RawDtoMember` field types in a DTO/Contract member list. */
    private def substituteMembers(
      members: Seq[RawDtoMember],
      substMap: Map[String, RawTypeRef],
      templateName: String,
      registry: TemplateRegistry,
      meta: RawNodeMeta,
      pkg: Pkg,
    ): F[NEList[BaboonIssue], Seq[RawDtoMember]] = {
      F.traverseAccumErrors(members)(m => substituteDtoMember(m, substMap, templateName, registry, meta, pkg))
    }

    private def substituteDtoMember(
      member: RawDtoMember,
      substMap: Map[String, RawTypeRef],
      templateName: String,
      registry: TemplateRegistry,
      meta: RawNodeMeta,
      pkg: Pkg,
    ): F[NEList[BaboonIssue], RawDtoMember] = {
      member match {
        case f @ RawDtoMember.FieldDef(field, fm) =>
          substituteTypeRef(field.tpe, substMap, templateName, registry, fm, pkg).map {
            newTpe => f.copy(field = field.copy(tpe = newTpe))
          }
        case u @ RawDtoMember.UnfieldDef(field, fm) =>
          substituteTypeRef(field.tpe, substMap, templateName, registry, fm, pkg).map {
            newTpe => u.copy(field = field.copy(tpe = newTpe))
          }
        // PR-33.2 (M33): substitute the `args` of structural-composition arms when the template
        // body itself contains `+/-/^ Inner[U]`. After substitution the arm still carries
        // `args.isDefined`; it is the caller's job (`lowerStructuralArmsInMembers`) to recursively
        // lower it. We do NOT call `substituteTypeRef` here because that helper rejects any
        // RawTypeRef.Constructor whose head names a registered template (matrix #1, used to
        // forbid template instantiations in field position). For structural-arm args, template
        // instantiation IS forbidden in the same way (matrix #2 — args may not themselves be
        // template instantiations); rejecting them here keeps the invariant. So we substitute
        // each arg through the same `substituteTypeRef` that would catch a forbidden inner
        // template instantiation.
        case p @ RawDtoMember.ParentDef(_, _, Some(parentArgs)) =>
          F.traverseAccumErrors(parentArgs.toList)(a => substituteTypeRef(a, substMap, templateName, registry, meta, pkg)).map {
            newArgs => p.copy(args = Some(NEList.unsafeFrom(newArgs)))
          }
        case u @ RawDtoMember.UnparentDef(_, _, Some(parentArgs)) =>
          F.traverseAccumErrors(parentArgs.toList)(a => substituteTypeRef(a, substMap, templateName, registry, meta, pkg)).map {
            newArgs => u.copy(args = Some(NEList.unsafeFrom(newArgs)))
          }
        case i @ RawDtoMember.IntersectionDef(_, _, Some(parentArgs)) =>
          F.traverseAccumErrors(parentArgs.toList)(a => substituteTypeRef(a, substMap, templateName, registry, meta, pkg)).map {
            newArgs => i.copy(args = Some(NEList.unsafeFrom(newArgs)))
          }
        case other =>
          // ParentDef/UnparentDef/IntersectionDef with args = None, ContractRef,
          // IntersectionFields (typer-internal — never appears at substitution time in practice
          // because it is produced AFTER substitution by `lowerStructuralArmsInMembers`).
          F.pure(other)
      }
    }

    /** Substitute all `RawAdtMember` entries. Each `RawAdtMemberDto` and
      * `RawAdtMemberContract` carries a nested DTO/Contract whose field types
      * must also be substituted.
      */
    private def substituteAdtMembers(
      members: Seq[RawAdtMember],
      substMap: Map[String, RawTypeRef],
      templateName: String,
      registry: TemplateRegistry,
      meta: RawNodeMeta,
      pkg: Pkg,
    ): F[NEList[BaboonIssue], Seq[RawAdtMember]] = {
      F.traverseAccumErrors(members)(m => substituteAdtMember(m, substMap, templateName, registry, meta, pkg))
    }

    private def substituteAdtMember(
      member: RawAdtMember,
      substMap: Map[String, RawTypeRef],
      templateName: String,
      registry: TemplateRegistry,
      meta: RawNodeMeta,
      pkg: Pkg,
    ): F[NEList[BaboonIssue], RawAdtMember] = {
      member match {
        case m: RawAdtMemberDto =>
          substituteMembers(m.dto.members, substMap, templateName, registry, m.meta, pkg).map {
            newMembers => m.copy(dto = m.dto.copy(members = newMembers))
          }
        case m: RawAdtMemberContract =>
          substituteMembers(m.contract.members, substMap, templateName, registry, m.meta, pkg).map {
            newMembers => m.copy(contract = m.contract.copy(members = newMembers))
          }
        case other =>
          // Include/Exclude/Intersect arms — no type refs to substitute.
          F.pure(other)
      }
    }

    /** Substitute all `RawFunc` argument/return/error type refs in a service. */
    private def substituteFuncs(
      funcs: Seq[RawFunc],
      substMap: Map[String, RawTypeRef],
      templateName: String,
      registry: TemplateRegistry,
      meta: RawNodeMeta,
      pkg: Pkg,
    ): F[NEList[BaboonIssue], Seq[RawFunc]] = {
      F.traverseAccumErrors(funcs)(f => substituteFunc(f, substMap, templateName, registry, meta, pkg))
    }

    private def substituteFunc(
      func: RawFunc,
      substMap: Map[String, RawTypeRef],
      templateName: String,
      registry: TemplateRegistry,
      meta: RawNodeMeta,
      pkg: Pkg,
    ): F[NEList[BaboonIssue], RawFunc] = {
      F.traverseAccumErrors(func.sig)(arg => substituteArg(arg, substMap, templateName, registry, meta, pkg)).map {
        newSig => func.copy(sig = newSig)
      }
    }

    private def substituteArg(
      arg: RawFuncArg,
      substMap: Map[String, RawTypeRef],
      templateName: String,
      registry: TemplateRegistry,
      meta: RawNodeMeta,
      pkg: Pkg,
    ): F[NEList[BaboonIssue], RawFuncArg] = {
      arg match {
        case r @ RawFuncArg.Ref(ref, marker, fm) =>
          substituteTypeRef(ref, substMap, templateName, registry, fm, pkg).map {
            newRef => r.copy(ref = newRef)
          }
        case s: RawFuncArg.Struct =>
          // Nested struct definitions — walk the members if it's a DTO or Contract.
          s.defn match {
            case dto: RawDto =>
              substituteMembers(dto.members, substMap, templateName, registry, dto.meta, pkg).map {
                newMembers => s.copy(defn = dto.copy(members = newMembers))
              }
            case contract: RawContract =>
              substituteMembers(contract.members, substMap, templateName, registry, contract.meta, pkg).map {
                newMembers => s.copy(defn = contract.copy(members = newMembers))
              }
            case other =>
              F.pure(s.copy(defn = other))
          }
      }
    }

    /** Core recursive type-ref substitution.
      *
      * - `RawTypeRef.Simple(p, Nil)` where `p` is a key in `substMap` → replace.
      * - `RawTypeRef.Constructor(name, params, prefix)` → recurse into params.
      *   Additionally checks whether `name` resolves to a registered template
      *   (matrix #1 / spec §4): if so, emit `TemplateInstantiationInForbiddenPosition`.
      *   For prefixed refs (e.g. `ns.Foo[T]`), the owner is derived from the prefix
      *   so that cross-namespace in-body template instantiations also produce the precise
      *   diagnostic (PR-29.15 closes [PR-29.5-D04]).
      * - `RawTypeRef.AnyRef(qualifier, underlying)` → recurse into underlying if present.
      */
    private def substituteTypeRef(
      ref: RawTypeRef,
      substMap: Map[String, RawTypeRef],
      templateName: String,
      registry: TemplateRegistry,
      meta: RawNodeMeta,
      pkg: Pkg,
    ): F[NEList[BaboonIssue], RawTypeRef] = {
      ref match {
        case s @ RawTypeRef.Simple(name, Nil) =>
          substMap.get(name.name) match {
            case Some(replacement) => F.pure(replacement)
            case None              => F.pure(s)
          }

        case s: RawTypeRef.Simple =>
          // Simple ref with a prefix — cannot be a type-param placeholder (those have no prefix).
          F.pure(s)

        case ctor @ RawTypeRef.Constructor(name, params, prefix) =>
          // Check whether the constructor's head names a registered template (matrix #1).
          // For the empty-prefix case, restore the broad "any-owner same-pkg" lookup so that
          // sibling templates referenced by bare name inside a namespace body are caught.
          // Matrix #1 forbids any in-body instantiation of any template regardless of the
          // template's owner, so narrow Owner.Toplevel lookup was a regression.
          // For the non-empty-prefix case, use precise Owner.Ns(prefix) lookup.
          val maybeTemplateKey = if (prefix.isEmpty) {
            registry.templates.keys.find {
              case (kPkg, _, tname) => kPkg == pkg && tname.name == name.name
            }
          } else {
            val ctorOwner = Owner.Ns(prefix.map(p => TypeName(p.name)))
            registry.templates.keys.find {
              case (kPkg, kOwner, tname) => kPkg == pkg && kOwner == ctorOwner && tname.name == name.name
            }
          }
          maybeTemplateKey match {
            case Some(_) =>
              // Any RawTypeRef.Constructor over a template in field position is forbidden
              // (spec §4, matrix #1 / decision #3).
              F.fail(
                BaboonIssue.of(
                  TyperIssue.TemplateInstantiationInForbiddenPosition(
                    containingTemplateName = templateName,
                    instantiatedName       = name.name,
                    meta                   = meta,
                  )
                )
              )
            case None =>
              // Builtin collection or ordinary constructor — recurse into params.
              F.traverseAccumErrors(params.toList)(p => substituteTypeRef(p, substMap, templateName, registry, meta, pkg)).map {
                newParams =>
                  ctor.copy(params = NEList.unsafeFrom(newParams))
              }
          }

        case anyRef @ RawTypeRef.AnyRef(qualifier, underlying) =>
          underlying match {
            case Some(u) =>
              substituteTypeRef(u, substMap, templateName, registry, meta, pkg).map {
                newU => anyRef.copy(underlying = Some(newU))
              }
            case None =>
              F.pure(anyRef)
          }
      }
    }

    // ─── key resolution ───────────────────────────────────────────────────────

    /** Compute the registry lookup key for a constructor reference.
      *
      * If the constructor has a non-empty `prefix`, we synthesise an Owner from the prefix so
      * that namespace-qualified template references resolve correctly. For example, a reference
      * `ns.Foo[T]` in an alias inside package `pkg` maps to key
      * `(pkg, Owner.Ns(List(TypeName("ns"))), TypeName("Foo"))`.
      *
      * An empty prefix looks up under `ownerForCurrent`, which is `Owner.Toplevel` for
      * top-level aliases, or the enclosing namespace's owner for nested ones.
      */
    private def resolveTemplateKey(
      pkg: Pkg,
      ownerForCurrent: Owner,
      ctor: RawTypeRef.Constructor,
    ): (Pkg, Owner, TypeName) = {
      val owner: Owner = if (ctor.prefix.isEmpty) {
        ownerForCurrent
      } else {
        Owner.Ns(ctor.prefix.map(p => TypeName(p.name)))
      }
      (pkg, owner, TypeName(ctor.name.name))
    }
  }
}
