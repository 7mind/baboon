package io.septimalmind.baboon.validator

import io.septimalmind.baboon.parser.model.{RawMemberMeta, RawNodeMeta}
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.parser.model.issues.VerificationIssue
import io.septimalmind.baboon.parser.model.issues.VerificationIssue.ConversionIssue
import io.septimalmind.baboon.typer.BaboonEnquiries
import io.septimalmind.baboon.typer.model.*
import io.septimalmind.baboon.typer.model.Conversion.FieldOp
import izumi.functional.bio.{Error2, F, ParallelErrorAccumulatingOps2}
import izumi.fundamentals.collections.nonempty.{NEList, NEMap}

trait BaboonValidator[F[+_, +_]] {
  def validate(
    family: BaboonFamily
  ): F[NEList[BaboonIssue], Unit]
}

object BaboonValidator {
  class BaboonValidatorImpl[F[+_, +_]: Error2: ParallelErrorAccumulatingOps2](
    enquiries: BaboonEnquiries
  ) extends BaboonValidator[F] {

    override def validate(
      family: BaboonFamily
    ): F[NEList[BaboonIssue], Unit] = {
      F.parTraverseAccumErrors_(family.domains.toSeq) {
        case (pkg, lineage) =>
          validateLineage(pkg, lineage)
      }
    }

    private def validateLineage(
      pkg: Pkg,
      lineage: BaboonLineage,
    ): F[NEList[BaboonIssue], Unit] = {
      assert(lineage.pkg == pkg)

      for {
        _ <- F.parTraverseAccumErrors_(lineage.versions.toSeq) {
          case (v, domain) =>
            validateDomain(v, domain)
        }
        _ <- validateEvolution(lineage.evolution, lineage.versions)
      } yield {}
    }

    private def validateDomain(
      version: Version,
      domain: Domain,
    ): F[NEList[BaboonIssue], Unit] = {
      assert(domain.version == version)

      for {
        _ <- checkMissingTypes(domain)
        _ <- checkConventions(domain)
        _ <- checkLoops(domain)
        _ <- checkUniqueness(domain)
        _ <- checkShape(domain)
        _ <- checkPathologicGenerics(domain)
        _ <- checkAnyFields(domain)
        _ <- checkRoots(domain)
      } yield {}

    }

    private def checkLoops(
      domain: Domain
    ): F[NEList[BaboonIssue], Unit] = {
      val filtered = domain.loops.map {
        l =>
          val filtered =
            l.loops.filterNot(
              _.loop.exists(terminatesLoop(_, domain, List.empty))
            )

          l.copy(loops = filtered)

      }
        .filterNot(_.loops.isEmpty)

      F.when(filtered.nonEmpty)(
        F.fail(BaboonIssue.of(VerificationIssue.ReferentialCyclesFound(domain, filtered)))
      )
    }

    private def terminatesLoop(id: TypeId, domain: Domain, path: List[TypeId]): Boolean = {
      val npath = path :+ id
      if (path.contains(id)) {
        false
      } else {
        domain.defs.meta.nodes(id) match {
          case _: DomainMember.Builtin => true
          case u: DomainMember.User =>
            u.defn match {
              case d: Typedef.Dto =>
                allFieldsTerminal(domain, npath, d.fields)
              case d: Typedef.Contract =>
                allFieldsTerminal(domain, npath, d.fields)
              case d: Typedef.Adt =>
                d.members.exists(terminatesLoop(_, domain, npath))
              case _: Typedef.Service =>
                false // TODO
              case _: Typedef.Enum    => true
              case _: Typedef.Foreign => true
            }
        }
      }
    }

    private def allFieldsTerminal(domain: Domain, npath: List[TypeId], f: List[Field]): Boolean = {
      f.map(_.tpe)
        .map(ref => ref.id) // we ignore generic options, BIGs termninate loops, this might change
        .forall(terminatesLoop(_, domain, npath))
    }

    private def checkRoots(
      domain: Domain
    ): F[NEList[BaboonIssue], Unit] = {
      val roots = domain.defs.meta.nodes.values.collect {
        case v: DomainMember.User if v.root => v.defn
      }.toSeq

      val badRoots = roots.collect { case d: Typedef.Enum => d.id }

      F.when(badRoots.nonEmpty)(
        F.fail(BaboonIssue.of(VerificationIssue.IncorrectRootFound(domain, badRoots)))
      )
    }

    private def checkMissingTypes(
      domain: Domain
    ): F[NEList[BaboonIssue], Unit] = {
      val allDeps = domain.defs.meta.nodes.values.flatMap {
        defn =>
          enquiries.fullDepsOfDefn(defn)
      }.toSet

      val allDefs = domain.defs.meta.nodes.keySet
      val missing = allDeps.diff(allDefs)
      F.when(missing.nonEmpty)(
        F.fail(BaboonIssue.of(VerificationIssue.MissingTypeDef(domain, missing)))
      )
    }

    private def checkUniqueness(
      domain: Domain
    ): F[NEList[BaboonIssue], Unit] = {
      for {
        _ <- {
          val dupes =
            domain.defs.meta.nodes.values.groupBy {
              case DomainMember.Builtin(id) => id.name.name
              case DomainMember.User(_, defn, _, _) =>
                defn.id.owner match {
                  case Owner.Toplevel =>
                    defn.id.name.name
                  case Owner.Adt(id) =>
                    s"${id.name.name}#${defn.id.name.name}"
                  case Owner.Ns(path) =>
                    s"//${path.map(_.name).mkString("/")}#${defn.id.name.name}"

                }
            }
              .filter(_._2.size > 1)
          F.when(dupes.nonEmpty)(
            F.fail(BaboonIssue.of(VerificationIssue.ConflictingTypeIds(domain, dupes)))
          )
        }
        _ <- F.traverseAccumErrors_(domain.defs.meta.nodes.values) {
          case _: DomainMember.Builtin =>
            F.unit
          case u: DomainMember.User =>
            u.defn match {
              case d: Typedef.Dto =>
                validateFields(u, d.fields)
              case e: Typedef.Enum =>
                val dupes =
                  e.members.groupBy(_.name.toLowerCase).filter(_._2.size > 1)
                F.when(dupes.nonEmpty)(
                  F.fail(BaboonIssue.of(VerificationIssue.ConflictingEnumBranches(e, dupes, u.meta)))
                )
              case a: Typedef.Adt =>
                val dupes =
                  a.members
                    .groupBy(_.name.name.toLowerCase)
                    .filter(_._2.size > 1)
                F.when(dupes.nonEmpty)(
                  F.fail(BaboonIssue.of(VerificationIssue.ConflictingAdtBranches(a, dupes, u.meta)))
                )
              case _: Typedef.Foreign =>
                F.unit
              case c: Typedef.Contract =>
                validateFields(u, c.fields)
              case _: Typedef.Service =>
                F.unit // TODO:
            }
        }
      } yield {}
    }

    private def validateFields(u: DomainMember.User, fields: List[Field]): F[NEList[BaboonIssue], Unit] = {
      // TODO: check that no contract fields were removed!!!

      val dupes =
        fields
          .groupBy(_.name.name.toLowerCase)
          .filter(_._2.size > 1)

      F.when(dupes.nonEmpty)(
        F.fail(BaboonIssue.of(VerificationIssue.ConflictingDtoFields(u.id, dupes, u.meta)))
      )
    }

    private def checkPathologicGenerics(
      domain: Domain
    ): F[NEList[BaboonIssue], Unit] = {
      for {
        _ <- F.traverseAccumErrors_(domain.defs.meta.nodes.values) {
          case _: DomainMember.Builtin =>
            F.unit
          case u: DomainMember.User =>
            u.defn match {
              case d: Typedef.Dto =>
                for {
                  _ <- checkDoubleOptions(d, u.meta)
                  _ <- checkComplexSetElements(d, u.meta)
                  _ <- checkComplexMapKeys(d, u.meta)
                } yield {}

              case _ =>
                F.unit
            }
        }
      } yield {}
    }

    private def checkDoubleOptions(
      dto: Typedef.Dto,
      meta: RawNodeMeta,
    ): F[NEList[BaboonIssue], Unit] = {
      def isDoubleOption(t: TypeRef, path: Seq[TypeId]): Boolean = {

        t match {
          case _: TypeRef.Scalar =>
            false
          case TypeRef.Constructor(id, args) =>
            if (id == TypeId.Builtins.opt && path.lastOption.contains(id)) {
              true
            } else {
              args.exists(a => isDoubleOption(a, path :+ id))
            }
          case _: TypeRef.Any =>
            // `any` carries no generics, so it cannot form a double-option chain. The PR 1.3
            // validator will handle `any`-specific rules (map key, derived[ueba]); PR 1.2 only
            // needs a safe pass-through here.
            false
        }
      }

      val badFields = dto.fields.filter(f => isDoubleOption(f.tpe, Seq.empty))

      F.when(badFields.nonEmpty)(
        F.fail(BaboonIssue.of(VerificationIssue.PathologicGenerics(dto, badFields, meta)))
      )
    }

    private def checkComplexSetElements(
      dto: Typedef.Dto,
      meta: RawNodeMeta,
    ): F[NEList[BaboonIssue], Unit] = {
      def isDoubleOption(t: TypeRef): Boolean = {
        t match {
          case TypeRef.Constructor(TypeId.Builtins.set, args) =>
            args.head match {
              case _: TypeRef.Constructor => true
              case _                      => false
            }
          case _ =>
            false
        }
      }

      val badFields = dto.fields.filter(f => isDoubleOption(f.tpe))

      F.when(badFields.nonEmpty)(
        F.fail(BaboonIssue.of(VerificationIssue.SetsCantContainGenerics(dto, badFields, meta)))
      )
    }

    private def checkComplexMapKeys(
      dto: Typedef.Dto,
      meta: RawNodeMeta,
    ): F[NEList[BaboonIssue], Unit] = {
      def isDoubleOption(t: TypeRef): Boolean = {
        t match {
          case TypeRef.Constructor(TypeId.Builtins.map, args) =>
            args.head match {
              case _: TypeRef.Constructor => true
              case _                      => false
            }
          case _ =>
            false
        }
      }

      val badFields = dto.fields.filter(f => isDoubleOption(f.tpe))

      F.when(badFields.nonEmpty)(
        F.fail(BaboonIssue.of(VerificationIssue.MapKeysShouldNotBeGeneric(dto, badFields, meta)))
      )
    }

    // Enforces the PR 1.3 validator rules for `any` / `AnyOpaque` fields.
    // See docs/drafts/20260424-1738-any-opaque-fields.md §"Validator rules".
    //
    // Rules:
    //   (1) Underlying T of `any[T]` (variants D1/D2/D3) must be a user-defined type
    //       (DTO/ADT/Enum) — not a builtin scalar, not a collection, not a nested `any`,
    //       not a foreign type. Foreign types have no intrinsic ueba codec the validator
    //       can reason about; this restriction may be relaxed in a future milestone if
    //       Foreign types gain a ueba-derivation mechanism.
    //   (2) Underlying T must carry `derived[ueba]`.
    //       TODO (M2+): also require `derived[json]` when JSON codegen is enabled for the
    //       containing type. PR 1.3 scope is ueba only.
    //   (3) `any` cannot appear as a map key (any nesting depth).
    //   (4) `any` cannot appear as a set element (any nesting depth).
    //   (5) Other positions (`opt[any]`, `lst[any]`, map value) are allowed.
    //
    // Coverage extends across all `Typedef.User` shapes that carry TypeRefs: Dto fields,
    // Contract fields, Adt own fields (flattened contracts), and Service method
    // sig/out/err. A Contract's fields are also visible on every inheriting DTO; both
    // sites report independently — duplication is acceptable and matches the style of
    // other validators in this file (e.g. `validateFields`).
    private def checkAnyFields(
      domain: Domain
    ): F[NEList[BaboonIssue], Unit] = {
      F.traverseAccumErrors_(domain.defs.meta.nodes.values) {
        case _: DomainMember.Builtin =>
          F.unit
        case u: DomainMember.User =>
          u.defn match {
            case d: Typedef.Dto =>
              checkAnyOnFields(domain, d, d.fields, u.meta)
            case c: Typedef.Contract =>
              checkAnyOnFields(domain, c, c.fields, u.meta)
            case a: Typedef.Adt =>
              checkAnyOnFields(domain, a, a.fields, u.meta)
            case s: Typedef.Service =>
              // Synthesise pseudo-fields whose names point at the offending method slot
              // (`<methodName>.sig`, `<methodName>.out`, `<methodName>.err`). The Field
              // type is the ergonomic carrier the issue printers expect; nothing here
              // is round-tripped through codegen.
              val pseudo: List[Field] = s.methods.flatMap { m =>
                val sigF = Field(FieldName(s"${m.name.name}.sig"), m.sig, None)
                val outF = m.out.map(t => Field(FieldName(s"${m.name.name}.out"), t, None))
                val errF = m.err.map(t => Field(FieldName(s"${m.name.name}.err"), t, None))
                List(sigF) ++ outF.toList ++ errF.toList
              }
              checkAnyOnFields(domain, s, pseudo, u.meta)
            case _ =>
              F.unit
          }
      }
    }

    private def checkAnyOnFields(
      domain: Domain,
      owner: Typedef.User,
      fields: List[Field],
      meta: RawNodeMeta,
    ): F[NEList[BaboonIssue], Unit] = {
      for {
        _ <- checkAnyUnderlying(domain, owner, fields, meta)
        _ <- checkAnyAsMapKey(owner, fields, meta)
        _ <- checkAnyAsSetElement(owner, fields, meta)
      } yield {}
    }

    private def checkAnyUnderlying(
      domain: Domain,
      owner: Typedef.User,
      fields: List[Field],
      meta: RawNodeMeta,
    ): F[NEList[BaboonIssue], Unit] = {
      // Collect every `TypeRef.Any(_, Some(u))` reachable through the field type.
      // We only inspect the shape of `u` — the typer has already ensured `u` is well-formed.
      def collectAnyUnderlyings(t: TypeRef): List[TypeRef] = t match {
        case _: TypeRef.Scalar => Nil
        case c: TypeRef.Constructor =>
          c.args.toList.flatMap(collectAnyUnderlyings)
        case TypeRef.Any(_, None) =>
          Nil
        case TypeRef.Any(_, Some(u)) =>
          u :: collectAnyUnderlyings(u)
      }

      // Underlying shape check: only `TypeRef.Scalar(user)` where the user type is a DTO,
      // ADT, or Enum passes. Builtins, collections, nested `any`, services, contracts, and
      // foreign types are rejected per the spec. Rationale: foreign types have no intrinsic
      // ueba codec the validator can reason about; this restriction may be relaxed in a
      // future milestone if Foreign types gain a ueba-derivation mechanism.
      def isUserDataType(t: TypeRef): Boolean = t match {
        case TypeRef.Scalar(id: TypeId.User) =>
          domain.defs.meta.nodes.get(id).exists {
            case DomainMember.User(_, defn, _, _) =>
              defn match {
                case _: Typedef.Dto  => true
                case _: Typedef.Adt  => true
                case _: Typedef.Enum => true
                case _               => false
              }
            case _ => false
          }
        case _ => false
      }

      val uebaDerivation = domain.derivationRequests
        .getOrElse(RawMemberMeta.Derived("ueba"), Set.empty[TypeId])

      def hasUeba(t: TypeRef): Boolean = t match {
        case TypeRef.Scalar(id: TypeId.User) => uebaDerivation.contains(id)
        case _                               => false
      }

      val notUserTypeFields = fields.filter {
        f => collectAnyUnderlyings(f.tpe).exists(u => !isUserDataType(u))
      }

      // Only check the ueba derivation for fields where the underlying IS a valid user type —
      // otherwise the "not user type" issue already fires and dominates.
      val lacksUebaFields = fields.filter {
        f =>
          val underlyings = collectAnyUnderlyings(f.tpe)
          underlyings.nonEmpty && underlyings.forall(isUserDataType) && underlyings.exists(u => !hasUeba(u))
      }

      for {
        _ <- F.when(notUserTypeFields.nonEmpty)(
          F.fail(BaboonIssue.of(VerificationIssue.AnyUnderlyingNotUserType(owner = owner, badFields = notUserTypeFields, meta = meta)))
        )
        _ <- F.when(lacksUebaFields.nonEmpty)(
          F.fail(BaboonIssue.of(VerificationIssue.AnyUnderlyingLacksUebaDerivation(owner = owner, badFields = lacksUebaFields, meta = meta)))
        )
      } yield {}
    }

    // Recursive — walks every nested map position and reports the field if any of them has
    // `any` in the key slot. Does not rely on `checkComplexMapKeys`, which is non-recursive
    // and only rejects builtin-collection keys at the top level.
    private def checkAnyAsMapKey(
      owner: Typedef.User,
      fields: List[Field],
      meta: RawNodeMeta,
    ): F[NEList[BaboonIssue], Unit] = {
      def anyAsMapKey(t: TypeRef): Boolean = t match {
        case TypeRef.Constructor(TypeId.Builtins.map, args) =>
          args.head.isInstanceOf[TypeRef.Any] || args.toList.exists(anyAsMapKey)
        case TypeRef.Constructor(_, args) =>
          args.toList.exists(anyAsMapKey)
        case _ => false
      }

      val badFields = fields.filter(f => anyAsMapKey(f.tpe))

      F.when(badFields.nonEmpty)(
        F.fail(BaboonIssue.of(VerificationIssue.AnyAsMapKey(owner = owner, badFields = badFields, meta = meta)))
      )
    }

    // Recursive — walks every nested set position and reports the field if any of them has
    // `any` as the element type.
    private def checkAnyAsSetElement(
      owner: Typedef.User,
      fields: List[Field],
      meta: RawNodeMeta,
    ): F[NEList[BaboonIssue], Unit] = {
      def anyAsSetElement(t: TypeRef): Boolean = t match {
        case TypeRef.Constructor(TypeId.Builtins.set, args) =>
          args.head.isInstanceOf[TypeRef.Any] || args.toList.exists(anyAsSetElement)
        case TypeRef.Constructor(_, args) =>
          args.toList.exists(anyAsSetElement)
        case _ => false
      }

      val badFields = fields.filter(f => anyAsSetElement(f.tpe))

      F.when(badFields.nonEmpty)(
        F.fail(BaboonIssue.of(VerificationIssue.AnyAsSetElement(owner = owner, badFields = badFields, meta = meta)))
      )
    }

    private def checkShape(
      domain: Domain
    ): F[NEList[BaboonIssue], Unit] = {
      F.traverseAccumErrors_(domain.defs.meta.nodes.values) {
        case _: DomainMember.Builtin =>
          F.unit
        case u: DomainMember.User =>
          u.defn match {
            case d: Typedef.Dto =>
              validateFieldShape(u, d.fields)
            case c: Typedef.Contract =>
              validateFieldShape(u, c.fields)
            case e: Typedef.Enum =>
              for {
                _ <- F.when(e.members.isEmpty)(
                  F.fail(BaboonIssue.of(VerificationIssue.EmptyEnumDef(e, u.meta)))
                )
                consts = e.members.toList.flatMap(_.const.toSeq).toSet
                _ <- F.when(
                  consts.nonEmpty && e.members.size != consts.size
                )(
                  F.fail(
                    NEList(
                      VerificationIssue.EitherAllOrNoneEnumMembersMustHaveConstants(e, u.meta): BaboonIssue
                    )
                  )
                )
                _ <- F.when(
                  consts.exists(i => i < Int.MinValue || i > Int.MaxValue)
                )(
                  F.fail(
                    NEList(
                      VerificationIssue.WrongEnumConstant(e, u.meta): BaboonIssue
                    )
                  )
                )
              } yield {}

            case a: Typedef.Adt =>
              F.when(a.members.isEmpty)(
                F.fail(BaboonIssue.of(VerificationIssue.EmptyAdtDef(a, u.meta)))
              )

            case _: Typedef.Foreign =>
              F.unit
            case _: Typedef.Service =>
              F.unit // TODO
          }
      }
    }

    private def validateFieldShape(u: DomainMember.User, f: List[Field]): F[NEList[BaboonIssue], Unit] = {
      for {
        badFields <- F.pure(
          f.map(_.name.name.toLowerCase)
            .filter(_ == u.id.name.name.toLowerCase)
        )
        _ <- F.when(badFields.nonEmpty)(
          F.fail(BaboonIssue.of(VerificationIssue.BadFieldNames(u.id, badFields, u.meta)))
        )
      } yield {}
    }

    private def checkConventions(
      domain: Domain
    ): F[NEList[BaboonIssue], Unit] = {
      F.traverseAccumErrors_(domain.defs.meta.nodes.values) {
        case _: DomainMember.Builtin =>
          F.unit
        case u: DomainMember.User =>
          F.when(u.id.name.name.head == '_')(
            F.fail(BaboonIssue.of(VerificationIssue.UnderscoredDefinitionRetained(u, u.meta)))
          )
      }
    }

    private def validateEvolution(
      evolution: BaboonEvolution,
      versions: NEMap[Version, Domain],
    ): F[NEList[BaboonIssue], Unit] = {

      // val latest = versions(evolution.latest)

      assert(evolution.diffs.keySet == evolution.rules.keySet)

      F.traverseAccumErrors_(evolution.diffs.toSeq) {
        case (v, diff) =>
          validateEvo(
            versions(v.to),
            versions(v.from),
            diff,
            evolution.rules(v),
          )
      }
    }

    private def validateEvo(
      next: Domain,
      prev: Domain,
      diff: BaboonDiff,
      ruleset: BaboonRuleset,
    ): F[NEList[BaboonIssue], Unit] = {
      val nextIds = next.defs.meta.nodes.keySet
      val diffIds = diff.diffs.keySet
      val prevIds = prev.defs.meta.nodes.collect {
        case (id: TypeId.User, _) => id: TypeId
      }.toSet
      // Diffs for changed types are keyed by the shared type ID (present in nextIds).
      // Diffs for renamed types are keyed by the OLD type ID (present in prevIds, not nextIds).
      // A truly orphan diff entry is one that belongs to neither version.
      val orphanDiffs        = diffIds.diff(nextIds).diff(prevIds)
      val conversionIds      = ruleset.conversions.map(_.sourceTpe).toSet[TypeId]
      val missingConversions = prevIds.diff(conversionIds)
      val extraConversions   = conversionIds.diff(prevIds)

      for {
        _ <- F.when(orphanDiffs.nonEmpty)(
          F.fail(BaboonIssue.of(VerificationIssue.MissingEvoDiff(prev, next, orphanDiffs)))
        )
        _ <- F.when(
          missingConversions.nonEmpty || extraConversions.nonEmpty
        )(
          F.fail(
            BaboonIssue.of(
              VerificationIssue.MissingEvoConversion(
                prev,
                next,
                missingConversions,
                extraConversions,
              )
            )
          )
        )
        _ <- F.traverseAccumErrors_(ruleset.conversions) {
          case _: Conversion.CustomConversionRequired =>
            F.unit
          case _: Conversion.NonDataTypeTypeNoConversion =>
            F.unit
          case c: Conversion.RemovedTypeNoConversion =>
            F.when(!diff.changes.removed.contains(c.sourceTpe))(
              F.fail(BaboonIssue.of(VerificationIssue.BrokenConversion(c)))
            )
          case c: Conversion.CopyEnumByName =>
            val o = prev.defs.meta.nodes(c.sourceTpe)
            val n = next.defs.meta.nodes(c.targetTpe)
            (o, n) match {
              case (
                    DomainMember.User(_, oe: Typedef.Enum, _, _),
                    DomainMember.User(_, ne: Typedef.Enum, _, _),
                  ) =>
                val oldNames       = oe.members.map(_.name).toSet
                val newNames       = ne.members.map(_.name).toSet
                val mappedOldNames = oldNames.map(name => c.memberMapping.getOrElse(name, name))
                F.when(
                  mappedOldNames.diff(newNames).nonEmpty
                )(
                  F.fail(
                    BaboonIssue.of(
                      VerificationIssue.IncorrectConversionApplication(
                        c,
                        o,
                        n,
                        ConversionIssue.RemovedEnumBranches,
                      )
                    )
                  )
                )
              case _ =>
                F.fail(
                  BaboonIssue.of(
                    VerificationIssue.IncorrectConversionApplication(
                      c,
                      o,
                      n,
                      ConversionIssue.TypeMismatch,
                    )
                  )
                )
            }

          case c: Conversion.DtoConversion =>
            val o = prev.defs.meta.nodes(c.sourceTpe)
            val n = next.defs.meta.nodes(c.targetTpe)
            (o, n) match {
              case (
                    DomainMember.User(_, od: Typedef.Dto, _, _),
                    DomainMember.User(_, nd: Typedef.Dto, _, _),
                  ) =>
                for {
                  newFieldNames <- F.pure(nd.fields.map(_.name).toSet)
                  oldFieldNames <- F.pure(od.fields.map(_.name).toSet)
                  removedFields <- F.pure(c.removed.map(_.name))
                  renamedSources = c.ops.collect {
                    case f: FieldOp.Rename => f.sourceFieldName
                    case f: FieldOp.Redef  => f.sourceFieldName
                  }.toSet
                  removals = oldFieldNames.diff(newFieldNames).diff(renamedSources)
                  _ <- F.when(removals != removedFields)(
                    F.fail(
                      BaboonIssue.of(
                        VerificationIssue.IncorrectConversionApplication(
                          c,
                          o,
                          n,
                          ConversionIssue.RemovedDtoFields,
                        )
                      )
                    )
                  )

                  transfer = c.ops.collect {
                    case f: FieldOp.Transfer => f.targetField.name
                  }.toSet
                  defaults = c.ops.collect {
                    case f: FieldOp.InitializeWithDefault => f.targetField.name
                  }.toSet
                  swap = c.ops.collect {
                    case f: FieldOp.SwapCollectionType => f.fieldName
                  }.toSet
                  precex = c.ops.collect {
                    case f: FieldOp.ExpandPrecision => f.fieldName
                  }.toSet
                  wrap = c.ops.collect {
                    case f: FieldOp.WrapIntoCollection => f.fieldName
                  }.toSet
                  renamed = c.ops.collect {
                    case f: FieldOp.Rename => f.targetField.name
                  }.toSet
                  redef = c.ops.collect {
                    case f: FieldOp.Redef => f.targetField.name
                  }.toSet
                  all = transfer ++ defaults ++ swap ++ wrap ++ precex ++ renamed ++ redef
                  _ <- F.when(newFieldNames != all) {
                    F.fail(
                      BaboonIssue.of(
                        VerificationIssue.IncorrectConversionApplication(
                          c,
                          o,
                          n,
                          ConversionIssue.IncorrectFields,
                        )
                      )
                    )
                  }
                  conflicts = Seq(
                    transfer.intersect(all.diff(transfer)),
                    defaults.intersect(all.diff(defaults)),
                    swap.intersect(all.diff(swap)),
                    wrap.intersect(all.diff(wrap)),
                    precex.intersect(all.diff(precex)),
                  )
                  _ <- F.when(conflicts.exists(_.nonEmpty))(
                    F.fail(
                      BaboonIssue.of(
                        VerificationIssue.IncorrectConversionApplication(
                          c,
                          o,
                          n,
                          ConversionIssue.ConflictingFields,
                        )
                      )
                    )
                  )
                } yield {}

              case _ =>
                F.fail(
                  BaboonIssue.of(
                    VerificationIssue.IncorrectConversionApplication(
                      c,
                      o,
                      n,
                      ConversionIssue.TypeMismatch,
                    )
                  )
                )
            }
          case c: Conversion.CopyAdtBranchByName =>
            val o = prev.defs.meta.nodes(c.sourceTpe)
            val n = next.defs.meta.nodes(c.targetTpe)
            (o, n) match {
              case (
                    DomainMember.User(_, oa: Typedef.Adt, _, _),
                    DomainMember.User(_, na: Typedef.Adt, _, _),
                  ) =>
                val oldNames = oa.members.map(_.name.name).toSet
                val newNames = na.members.map(_.name.name).toSet
                val mappedOldNames = oldNames.map {
                  name =>
                    c.branchMapping.get(name).map(_.name.name).getOrElse(name)
                }
                F.when(
                  mappedOldNames.diff(newNames).nonEmpty
                )(
                  F.fail(
                    BaboonIssue.of(
                      VerificationIssue.IncorrectConversionApplication(
                        c,
                        o,
                        n,
                        ConversionIssue.RemovedAdtBranches,
                      )
                    )
                  )
                )
              case _ =>
                F.fail(
                  BaboonIssue.of(
                    VerificationIssue.IncorrectConversionApplication(
                      c,
                      o,
                      n,
                      ConversionIssue.TypeMismatch,
                    )
                  )
                )
            }
        }
      } yield {}
    }

  }
}
