package io.septimalmind.baboon.validator

import io.septimalmind.baboon.parser.model.RawNodeMeta
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.parser.model.issues.BaboonIssue.ConversionIssue
import io.septimalmind.baboon.typer.BaboonEnquiries
import io.septimalmind.baboon.typer.model.*
import io.septimalmind.baboon.typer.model.Conversion.FieldOp
import izumi.functional.bio.{Error2, F, ParallelErrorAccumulatingOps2}
import izumi.fundamentals.collections.nonempty.{NEList, NEMap}

trait BaboonValidator[F[+_, +_]] {
  def validate(
    family: BaboonFamily
  ): F[NEList[BaboonIssue.VerificationIssue], Unit]
}

object BaboonValidator {
  class BaboonValidatorImpl[F[+_, +_]: Error2: ParallelErrorAccumulatingOps2](
    enquiries: BaboonEnquiries
  ) extends BaboonValidator[F] {

    override def validate(
      family: BaboonFamily
    ): F[NEList[BaboonIssue.VerificationIssue], Unit] = {
      F.parTraverseAccumErrors_(family.domains.toSeq) {
        case (pkg, lineage) =>
          validateLineage(pkg, lineage)
      }
    }

    private def validateLineage(
      pkg: Pkg,
      lineage: BaboonLineage,
    ): F[NEList[BaboonIssue.VerificationIssue], Unit] = {
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
    ): F[NEList[BaboonIssue.VerificationIssue], Unit] = {
      assert(domain.version == version)

      for {
        _ <- checkMissingTypes(domain)
        _ <- checkConventions(domain)
        _ <- checkLoops(domain)
        _ <- checkUniqueness(domain)
        _ <- checkShape(domain)
        _ <- checkPathologicGenerics(domain)
        _ <- checkRoots(domain)
      } yield {}

    }

    private def checkLoops(
      domain: Domain
    ): F[NEList[BaboonIssue.VerificationIssue], Unit] = {
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
        F.fail(NEList(BaboonIssue.ReferentialCyclesFound(domain, filtered)))
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
              case d: Typedef.Service =>
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
    ): F[NEList[BaboonIssue.VerificationIssue], Unit] = {
      val roots = domain.defs.meta.nodes.values.collect {
        case v: DomainMember.User if v.root => v.defn
      }.toSeq

      val badRoots = roots.collect { case d: Typedef.Enum => d.id }

      F.when(badRoots.nonEmpty)(
        F.fail(NEList(BaboonIssue.IncorrectRootFound(domain, badRoots)))
      )
    }

    private def checkMissingTypes(
      domain: Domain
    ): F[NEList[BaboonIssue.VerificationIssue], Unit] = {
      val allDeps = domain.defs.meta.nodes.values.flatMap {
        defn =>
          enquiries.fullDepsOfDefn(domain.defs.meta.nodes, defn)
      }.toSet

      val allDefs = domain.defs.meta.nodes.keySet
      val missing = allDeps.diff(allDefs)
      F.when(missing.nonEmpty)(
        F.fail(NEList(BaboonIssue.MissingTypeDef(domain, missing)))
      )
    }

    private def checkUniqueness(
      domain: Domain
    ): F[NEList[BaboonIssue.VerificationIssue], Unit] = {
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
            F.fail(NEList(BaboonIssue.ConflictingTypeIds(domain, dupes)))
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
                  F.fail(NEList(BaboonIssue.ConflictingEnumBranches(e, dupes, u.meta)))
                )
              case a: Typedef.Adt =>
                val dupes =
                  a.members
                    .groupBy(_.name.name.toLowerCase)
                    .filter(_._2.size > 1)
                F.when(dupes.nonEmpty)(
                  F.fail(NEList(BaboonIssue.ConflictingAdtBranches(a, dupes, u.meta)))
                )
              case _: Typedef.Foreign =>
                F.unit
              case c: Typedef.Contract =>
                validateFields(u, c.fields)
              case s: Typedef.Service =>
                F.unit // TODO:
            }
        }
      } yield {}
    }

    private def validateFields(u: DomainMember.User, fields: List[Field]): F[NEList[BaboonIssue.ConflictingDtoFields], Unit] = {
      // TODO: check that no contract fields were removed!!!

      val dupes =
        fields
          .groupBy(_.name.name.toLowerCase)
          .filter(_._2.size > 1)

      F.when(dupes.nonEmpty)(
        F.fail(NEList(BaboonIssue.ConflictingDtoFields(u.id, dupes, u.meta)))
      )
    }

    private def checkPathologicGenerics(
      domain: Domain
    ): F[NEList[BaboonIssue.VerificationIssue], Unit] = {
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
    ): F[NEList[BaboonIssue.PathologicGenerics], Unit] = {
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

        }
      }

      val badFields = dto.fields.filter(f => isDoubleOption(f.tpe, Seq.empty))

      F.when(badFields.nonEmpty)(
        F.fail(NEList(BaboonIssue.PathologicGenerics(dto, badFields, meta)))
      )
    }

    private def checkComplexSetElements(
      dto: Typedef.Dto,
      meta: RawNodeMeta,
    ): F[NEList[BaboonIssue.SetsCantContainGenerics], Unit] = {
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
        F.fail(NEList(BaboonIssue.SetsCantContainGenerics(dto, badFields, meta)))
      )
    }

    private def checkComplexMapKeys(
      dto: Typedef.Dto,
      meta: RawNodeMeta,
    ): F[NEList[BaboonIssue.MapKeysShouldNotBeGeneric], Unit] = {
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
        F.fail(NEList(BaboonIssue.MapKeysShouldNotBeGeneric(dto, badFields, meta)))
      )
    }

    private def checkShape(
      domain: Domain
    ): F[NEList[BaboonIssue.VerificationIssue], Unit] = {
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
                  F.fail(NEList(BaboonIssue.EmptyEnumDef(e, u.meta)))
                )
                consts = e.members.toList.flatMap(_.const.toSeq).toSet
                _ <- F.when(
                  consts.nonEmpty && e.members.size != consts.size
                )(
                  F.fail(
                    NEList(
                      BaboonIssue.EitherAllOrNoneEnumMembersMustHaveConstants(e, u.meta)
                    )
                  )
                )
                _ <- F.when(
                  consts.exists(i => i < Int.MinValue || i > Int.MaxValue)
                )(
                  F.fail(
                    NEList(
                      BaboonIssue.WrongEnumConstant(e, u.meta)
                    )
                  )
                )
              } yield {}

            case a: Typedef.Adt =>
              F.when(a.members.isEmpty)(
                F.fail(NEList(BaboonIssue.EmptyAdtDef(a, u.meta)))
              )

            case _: Typedef.Foreign =>
              F.unit
            case s: Typedef.Service =>
              F.unit // TODO
          }
      }
    }

    private def validateFieldShape(u: DomainMember.User, f: List[Field]): F[NEList[BaboonIssue.BadFieldNames], Unit] = {
      for {
        badFields <- F.pure(
          f.map(_.name.name.toLowerCase)
            .filter(_ == u.id.name.name.toLowerCase)
        )
        _ <- F.when(badFields.nonEmpty)(
          F.fail(NEList(BaboonIssue.BadFieldNames(u.id, badFields, u.meta)))
        )
      } yield {}
    }

    private def checkConventions(
      domain: Domain
    ): F[NEList[BaboonIssue.VerificationIssue], Unit] = {
      F.traverseAccumErrors_(domain.defs.meta.nodes.values) {
        case _: DomainMember.Builtin =>
          F.unit
        case u: DomainMember.User =>
          F.when(u.id.name.name.head == '_')(
            F.fail(NEList(BaboonIssue.UnderscoredDefinitionRetained(u, u.meta)))
          )
      }
    }

    private def validateEvolution(
      evolution: BaboonEvolution,
      versions: NEMap[Version, Domain],
    ): F[NEList[BaboonIssue.VerificationIssue], Unit] = {

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
    ): F[NEList[BaboonIssue.VerificationIssue], Unit] = {
      val nextIds      = next.defs.meta.nodes.keySet
      val diffIds      = diff.diffs.keySet
      val missingDiffs = nextIds.diff(diffIds)

      val prevIds = prev.defs.meta.nodes.collect {
        case (id: TypeId.User, _) => id: TypeId
      }.toSet
      val conversionIds      = ruleset.conversions.map(_.sourceTpe).toSet[TypeId]
      val missingConversions = prevIds.diff(conversionIds)
      val extraConversions   = conversionIds.diff(prevIds)

      for {
        _ <- F.when(missingDiffs.isEmpty)(
          F.fail(NEList(BaboonIssue.MissingEvoDiff(prev, next, missingDiffs)))
        )
        _ <- F.when(
          missingConversions.nonEmpty || extraConversions.nonEmpty
        )(
          F.fail(
            NEList(
              BaboonIssue.MissingEvoConversion(
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
              F.fail(NEList(BaboonIssue.BrokenConversion(c)))
            )
          case c: Conversion.CopyEnumByName =>
            val o = prev.defs.meta.nodes(c.sourceTpe)
            val n = next.defs.meta.nodes(c.sourceTpe)
            (o, n) match {
              case (
                    DomainMember.User(_, oe: Typedef.Enum, _, _),
                    DomainMember.User(_, ne: Typedef.Enum, _, _),
                  ) =>
                F.when(
                  oe.members.toSet.diff(ne.members.toSet).nonEmpty
                )(
                  F.fail(
                    NEList(
                      BaboonIssue.IncorrectConversionApplication(
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
                  NEList(
                    BaboonIssue.IncorrectConversionApplication(
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
            val n = next.defs.meta.nodes(c.sourceTpe)
            (o, n) match {
              case (
                    DomainMember.User(_, od: Typedef.Dto, _, _),
                    DomainMember.User(_, nd: Typedef.Dto, _, _),
                  ) =>
                for {
                  newFieldNames <- F.pure(nd.fields.map(_.name).toSet)
                  oldFieldNames <- F.pure(od.fields.map(_.name).toSet)
                  removedFields <- F.pure(c.removed.map(_.name))
                  removals       = oldFieldNames.diff(newFieldNames)
                  _ <- F.when(removals != removedFields)(
                    F.fail(
                      NEList(
                        BaboonIssue.IncorrectConversionApplication(
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
                  all = transfer ++ defaults ++ swap ++ wrap ++ precex
                  _ <- F.when(newFieldNames != all) {
                    F.fail(
                      NEList(
                        BaboonIssue.IncorrectConversionApplication(
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
                      NEList(
                        BaboonIssue.IncorrectConversionApplication(
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
                  NEList(
                    BaboonIssue.IncorrectConversionApplication(
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
            val n = next.defs.meta.nodes(c.sourceTpe)
            (o, n) match {
              case (
                    DomainMember.User(_, oa: Typedef.Adt, _, _),
                    DomainMember.User(_, na: Typedef.Adt, _, _),
                  ) =>
                F.when(
                  oa.members
                    .map(_.name)
                    .toSet
                    .diff(na.members.map(_.name).toSet)
                    .nonEmpty
                )(
                  F.fail(
                    NEList(
                      BaboonIssue.IncorrectConversionApplication(
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
                  NEList(
                    BaboonIssue.IncorrectConversionApplication(
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
