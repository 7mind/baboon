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

    override def instantiate(
      pkg: Pkg,
      members: Seq[RawTLDef],
      registry: TemplateRegistry,
    ): F[NEList[BaboonIssue], Seq[RawTLDef]] = {
      instantiateRecursive(pkg, members, registry, ownerForCurrent = Owner.Toplevel, nsPath = List.empty)
    }

    private def instantiateRecursive(
      pkg: Pkg,
      members: Seq[RawTLDef],
      registry: TemplateRegistry,
      ownerForCurrent: Owner,
      nsPath: List[TypeName],
    ): F[NEList[BaboonIssue], Seq[RawTLDef]] = {
      F.traverseAccumErrors(members)(m => processMember(pkg, m, registry, ownerForCurrent, nsPath)).map(_.flatten)
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
        case other =>
          // ParentDef, UnparentDef, IntersectionDef, ContractRef — body left unchanged here.
          // PR-33.1 added `args: Option[NEList[RawTypeRef]]` to ParentDef/UnparentDef/IntersectionDef;
          // those RawTypeRefs MUST be substituted by PR-33.2. This catch-all is correct for
          // PR-33.1 (parser-only) but PR-33.2 must add explicit arms for those three cases.
          // PR-33.2 hand-off index — ALL sites that silently drop `args` and need updating:
          //   1. HERE (TemplateInstantiator.substituteDtoMember) — add explicit arms for
          //      ParentDef, UnparentDef, IntersectionDef that substitute their `args` field.
          //   2. BaboonTranslator.scala — three `dtoParentToDefs` call-sites (ParentDef ~L244,
          //      UnparentDef ~L252, IntersectionDef ~L258): pass/thread `args` once the typer
          //      is ready to consume it.
          //   3. BaboonEnquiries.scala — three arms in `hardDepsOfRawDefn` (~L223-228):
          //      `Seq(d.parent)` must also include the type refs inside `d.args` once
          //      template-argument resolution is wired end-to-end.
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
