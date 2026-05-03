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
  *      `TemplateInstantiationInBody` (spec §4, matrix #1 / decision #3).
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

  class Impl[F[+_, +_]: Error2] extends TemplateInstantiator[F] {

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
                  instantiateAlias(pkg, root, alias, body, registry, key).map(List(_))
                case None =>
                  // Constructor whose head doesn't hit the registry.
                  // If the head is a builtin collection (lst, set, opt, map) or `any`, pass
                  // through — BaboonTranslator will handle it. Otherwise the user wrote
                  // something like `type Y = i32[X]`, which is matrix #8.
                  val headName = ctor.name.name
                  val isBuiltin = Set("lst", "set", "opt", "map", "any").contains(headName)
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
              val maybeTemplateKey = registry.templates.keys.find {
                case (kPkg, kOwner, tname) =>
                  tname.name == simple.name.name &&
                    kPkg == pkg &&
                    kOwner == ownerForCurrent &&
                    simple.prefix.isEmpty
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
              List(nsTL.copy(value = ns.copy(defns = rewrittenChildren)))
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
        val badArg = args.collectFirst {
          case argCtor: RawTypeRef.Constructor
              if registry.templates.keys.exists { case (_, _, tname) => tname.name == argCtor.name.name && argCtor.prefix.isEmpty } =>
            argCtor.name.name
        }
        badArg match {
          case Some(innerTemplate) =>
            F.fail(
              BaboonIssue.of(
                TyperIssue.TemplateInstantiationInBody(
                  containingTemplateName = templateKey._3.name,
                  instantiatedName       = innerTemplate,
                  meta                   = alias.meta,
                )
              )
            )
          case None =>
            val substMap: Map[String, RawTypeRef] =
              body.typeParams.map(_.name).zip(args).toMap

            // Walk the template body and apply substitution.
            body.rawDefn match {
              case RawTemplateDefn.Dto(raw) =>
                // Body-side derived is rejected at registry-build time (TemplateBodyCarriesDerived,
                // spec §5.3). By the time we reach this point the invariant holds.
                assert(raw.derived.isEmpty, s"template body '${raw.name.name}' carries derived — should have been rejected by TemplateRegistryBuilder")
                for {
                  rewrittenMembers <- substituteMembers(raw.members, substMap, templateKey._3.name, registry, alias.meta)
                } yield {
                  RawTLDef.DTO(
                    root,
                    raw.copy(
                      name       = alias.name,
                      members    = rewrittenMembers,
                      derived    = alias.derived,
                      meta       = alias.meta,
                      typeParams = Nil,
                    ),
                  )
                }

              case RawTemplateDefn.Adt(raw) =>
                // Body-side derived is rejected at registry-build time (TemplateBodyCarriesDerived,
                // spec §5.3). By the time we reach this point the invariant holds.
                assert(raw.derived.isEmpty, s"template body '${raw.name.name}' carries derived — should have been rejected by TemplateRegistryBuilder")
                for {
                  rewrittenAdtMembers <- substituteAdtMembers(raw.members, substMap, templateKey._3.name, registry, alias.meta)
                } yield {
                  RawTLDef.ADT(
                    root,
                    raw.copy(
                      name       = alias.name,
                      members    = rewrittenAdtMembers,
                      derived    = alias.derived,
                      meta       = alias.meta,
                      typeParams = Nil,
                    ),
                  )
                }

              case RawTemplateDefn.Contract(raw) =>
                for {
                  rewrittenMembers <- substituteMembers(raw.members, substMap, templateKey._3.name, registry, alias.meta)
                } yield {
                  RawTLDef.Contract(
                    root,
                    raw.copy(
                      name       = alias.name,
                      members    = rewrittenMembers,
                      meta       = alias.meta,
                      typeParams = Nil,
                    ),
                  )
                }

              case RawTemplateDefn.Service(raw) =>
                for {
                  rewrittenFuncs <- substituteFuncs(raw.defns, substMap, templateKey._3.name, registry, alias.meta)
                } yield {
                  RawTLDef.Service(
                    root,
                    raw.copy(
                      name       = alias.name,
                      defns      = rewrittenFuncs,
                      meta       = alias.meta,
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
    ): F[NEList[BaboonIssue], Seq[RawDtoMember]] = {
      F.traverseAccumErrors(members)(m => substituteDtoMember(m, substMap, templateName, registry, meta))
    }

    private def substituteDtoMember(
      member: RawDtoMember,
      substMap: Map[String, RawTypeRef],
      templateName: String,
      registry: TemplateRegistry,
      meta: RawNodeMeta,
    ): F[NEList[BaboonIssue], RawDtoMember] = {
      member match {
        case f @ RawDtoMember.FieldDef(field, fm) =>
          substituteTypeRef(field.tpe, substMap, templateName, registry, fm).map {
            newTpe => f.copy(field = field.copy(tpe = newTpe))
          }
        case u @ RawDtoMember.UnfieldDef(field, fm) =>
          substituteTypeRef(field.tpe, substMap, templateName, registry, fm).map {
            newTpe => u.copy(field = field.copy(tpe = newTpe))
          }
        case other =>
          // ParentDef, UnparentDef, IntersectionDef, ContractRef — no RawTypeRef to walk.
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
    ): F[NEList[BaboonIssue], Seq[RawAdtMember]] = {
      F.traverseAccumErrors(members)(m => substituteAdtMember(m, substMap, templateName, registry, meta))
    }

    private def substituteAdtMember(
      member: RawAdtMember,
      substMap: Map[String, RawTypeRef],
      templateName: String,
      registry: TemplateRegistry,
      meta: RawNodeMeta,
    ): F[NEList[BaboonIssue], RawAdtMember] = {
      member match {
        case m: RawAdtMemberDto =>
          substituteMembers(m.dto.members, substMap, templateName, registry, m.meta).map {
            newMembers => m.copy(dto = m.dto.copy(members = newMembers))
          }
        case m: RawAdtMemberContract =>
          substituteMembers(m.contract.members, substMap, templateName, registry, m.meta).map {
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
    ): F[NEList[BaboonIssue], Seq[RawFunc]] = {
      F.traverseAccumErrors(funcs)(f => substituteFunc(f, substMap, templateName, registry, meta))
    }

    private def substituteFunc(
      func: RawFunc,
      substMap: Map[String, RawTypeRef],
      templateName: String,
      registry: TemplateRegistry,
      meta: RawNodeMeta,
    ): F[NEList[BaboonIssue], RawFunc] = {
      F.traverseAccumErrors(func.sig)(arg => substituteArg(arg, substMap, templateName, registry, meta)).map {
        newSig => func.copy(sig = newSig)
      }
    }

    private def substituteArg(
      arg: RawFuncArg,
      substMap: Map[String, RawTypeRef],
      templateName: String,
      registry: TemplateRegistry,
      meta: RawNodeMeta,
    ): F[NEList[BaboonIssue], RawFuncArg] = {
      arg match {
        case r @ RawFuncArg.Ref(ref, marker, fm) =>
          substituteTypeRef(ref, substMap, templateName, registry, fm).map {
            newRef => r.copy(ref = newRef)
          }
        case s: RawFuncArg.Struct =>
          // Nested struct definitions — walk the members if it's a DTO or Contract.
          s.defn match {
            case dto: RawDto =>
              substituteMembers(dto.members, substMap, templateName, registry, dto.meta).map {
                newMembers => s.copy(defn = dto.copy(members = newMembers))
              }
            case contract: RawContract =>
              substituteMembers(contract.members, substMap, templateName, registry, contract.meta).map {
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
      *   (matrix #1 / spec §4): if so, emit `TemplateInstantiationInBody`.
      * - `RawTypeRef.AnyRef(qualifier, underlying)` → recurse into underlying if present.
      */
    private def substituteTypeRef(
      ref: RawTypeRef,
      substMap: Map[String, RawTypeRef],
      templateName: String,
      registry: TemplateRegistry,
      meta: RawNodeMeta,
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
          // We cannot do full scope resolution here (no scope context), but we can look up
          // in the registry using the same "owner = Toplevel" key used by TemplateRegistryBuilder.
          // Templates nested in namespaces are out of scope for PR-29.5 in-body check; the
          // flat (Toplevel) lookup catches the common case and the most dangerous one (self-ref).
          val maybeTemplateKey = registry.templates.keys.find {
            case (_, _, tname) => tname.name == name.name && prefix.isEmpty
          }
          maybeTemplateKey match {
            case Some(_) =>
              // Any RawTypeRef.Constructor over a template in field position is forbidden
              // (spec §4, matrix #1: "template instantiation in field position").
              F.fail(
                BaboonIssue.of(
                  TyperIssue.TemplateInstantiationInBody(
                    containingTemplateName = templateName,
                    instantiatedName       = name.name,
                    meta                   = meta,
                  )
                )
              )
            case None =>
              // Builtin collection or ordinary constructor — recurse into params.
              F.traverseAccumErrors(params.toList)(p => substituteTypeRef(p, substMap, templateName, registry, meta)).map {
                newParams =>
                  ctor.copy(params = NEList.unsafeFrom(newParams))
              }
          }

        case anyRef @ RawTypeRef.AnyRef(qualifier, underlying) =>
          underlying match {
            case Some(u) =>
              substituteTypeRef(u, substMap, templateName, registry, meta).map {
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
