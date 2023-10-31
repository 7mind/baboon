package io.septimalmind.baboon.validator

import io.septimalmind.baboon.parser.model.RawNodeMeta
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.parser.model.issues.BaboonIssue.ConversionIssue
import io.septimalmind.baboon.typer.BaboonEnquiries
import io.septimalmind.baboon.typer.model.*
import io.septimalmind.baboon.typer.model.Conversion.FieldOp
import izumi.functional.IzEither.*
import izumi.fundamentals.collections.nonempty.{NEList, NEMap}
import izumi.fundamentals.graphs.struct.IncidenceMatrix
import izumi.fundamentals.graphs.tools.cycles.LoopDetector

trait BaboonValidator {
  def validate(
    family: BaboonFamily
  ): Either[NEList[BaboonIssue.VerificationIssue], Unit]
}

object BaboonValidator {
  class BaboonValidatorImpl(enquiries: BaboonEnquiries)
      extends BaboonValidator {
    override def validate(
      family: BaboonFamily
    ): Either[NEList[BaboonIssue.VerificationIssue], Unit] = {
      family.domains.toSeq.map {
        case (pkg, lineage) =>
          validateLineage(pkg, lineage)
      }.biSequence_
    }

    private def validateLineage(
      pkg: Pkg,
      lineage: BaboonLineage
    ): Either[NEList[BaboonIssue.VerificationIssue], Unit] = {
      assert(lineage.pkg == pkg)

      for {
        _ <- lineage.versions.toSeq.map {
          case (v, domain) =>
            validateDomain(v, domain)
        }.biSequence_
        _ <- validateEvolution(lineage.evolution, lineage.versions)
      } yield {}

    }

    private def validateDomain(
      version: Version,
      domain: Domain
    ): Either[NEList[BaboonIssue.VerificationIssue], Unit] = {
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
    ): Either[NEList[BaboonIssue.VerificationIssue], Unit] = {
      val depMatrix = IncidenceMatrix(domain.defs.meta.nodes.view.mapValues {
        defn =>
          enquiries.directDepsOf(defn)
      }.toMap)

      val loops =
        LoopDetector.Impl.findCyclesForNodes(depMatrix.links.keySet, depMatrix)
      Either.ifThenFail(loops.nonEmpty)(
        NEList(BaboonIssue.ReferentialCyclesFound(domain, loops))
      )
    }

    private def checkRoots(
      domain: Domain
    ): Either[NEList[BaboonIssue.VerificationIssue], Unit] = {
      val roots = domain.defs.meta.nodes.values.collect {
        case (v: DomainMember.User) if v.root => v.defn
      }.toSeq

      val badRoots = roots.collect { case d: Typedef.Enum => d.id }

      Either.ifThenFail(badRoots.nonEmpty)(
        NEList(BaboonIssue.IncorrectRootFound(domain, badRoots))
      )
    }

    private def checkMissingTypes(
      domain: Domain
    ): Either[NEList[BaboonIssue.VerificationIssue], Unit] = {
      val allDeps = domain.defs.meta.nodes.values.flatMap { defn =>
        enquiries.directDepsOf(defn)
      }.toSet

      val allDefs = domain.defs.meta.nodes.keySet
      val missing = allDeps.diff(allDefs)
      Either.ifThenFail(missing.nonEmpty)(
        NEList(BaboonIssue.MissingTypeDef(domain, missing))
      )
    }

    private def checkUniqueness(
      domain: Domain
    ): Either[NEList[BaboonIssue.VerificationIssue], Unit] = {
      for {
        _ <- {
          val dupes =
            domain.defs.meta.nodes.values
              .groupBy {
                case DomainMember.Builtin(id) => id.name.name
                case DomainMember.User(_, defn, _) =>
                  defn.id.owner match {
                    case Owner.Toplevel => defn.id.name.name
                    case Owner.Adt(id) =>
                      s"${id.name.name}#${defn.id.name.name}"
                  }
              }
              .filter(_._2.size > 1)
          Either.ifThenFail(dupes.nonEmpty)(
            NEList(BaboonIssue.ConflictingTypeIds(domain, dupes))
          )
        }
        _ <- domain.defs.meta.nodes.values.map {
          case _: DomainMember.Builtin =>
            Right(())
          case u: DomainMember.User =>
            u.defn match {
              case d: Typedef.Dto =>
                val dupes =
                  d.fields
                    .groupBy(_.name.name.toLowerCase)
                    .filter(_._2.size > 1)
                Either.ifThenFail(dupes.nonEmpty)(
                  NEList(BaboonIssue.ConflictingDtoFields(d, dupes, u.meta))
                )
              case e: Typedef.Enum =>
                val dupes =
                  e.members.groupBy(_.name.toLowerCase).filter(_._2.size > 1)
                Either.ifThenFail(dupes.nonEmpty)(
                  NEList(BaboonIssue.ConflictingEnumBranches(e, dupes, u.meta))
                )
              case a: Typedef.Adt =>
                val dupes =
                  a.members
                    .groupBy(_.name.name.toLowerCase)
                    .filter(_._2.size > 1)
                Either.ifThenFail(dupes.nonEmpty)(
                  NEList(BaboonIssue.ConflictingAdtBranches(a, dupes, u.meta))
                )

            }
        }.biSequence_
      } yield {}
    }

    private def checkPathologicGenerics(
      domain: Domain
    ): Either[NEList[BaboonIssue.VerificationIssue], Unit] = {
      for {
        _ <- domain.defs.meta.nodes.values.map {
          case _: DomainMember.Builtin =>
            Right(())
          case u: DomainMember.User =>
            u.defn match {
              case d: Typedef.Dto =>
                for {
                  _ <- checkDoubleOptions(d, u.meta)
                  _ <- checkComplexSetElements(d, u.meta)
                  _ <- checkComplexMapKeys(d, u.meta)
                } yield {}

              case _ =>
                Right(())

            }
        }.biSequence_
      } yield {}
    }

    private def checkDoubleOptions(
      dto: Typedef.Dto,
      meta: RawNodeMeta
    ): Either[NEList[BaboonIssue.PathologicGenerics], Unit] = {
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

      Either.ifThenFail(badFields.nonEmpty)(
        NEList(BaboonIssue.PathologicGenerics(dto, badFields, meta))
      )
    }

    private def checkComplexSetElements(
      dto: Typedef.Dto,
      meta: RawNodeMeta
    ): Either[NEList[BaboonIssue.SetsCantContainGenerics], Unit] = {
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

      Either.ifThenFail(badFields.nonEmpty)(
        NEList(BaboonIssue.SetsCantContainGenerics(dto, badFields, meta))
      )
    }

    private def checkComplexMapKeys(
      dto: Typedef.Dto,
      meta: RawNodeMeta
    ): Either[NEList[BaboonIssue.MapKeysShouldNotBeGeneric], Unit] = {
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

      Either.ifThenFail(badFields.nonEmpty)(
        NEList(BaboonIssue.MapKeysShouldNotBeGeneric(dto, badFields, meta))
      )
    }

    private def checkShape(
      domain: Domain
    ): Either[NEList[BaboonIssue.VerificationIssue], Unit] = {
      domain.defs.meta.nodes.values.map {
        case _: DomainMember.Builtin =>
          Right(())
        case u: DomainMember.User =>
          u.defn match {
            case _: Typedef.Dto =>
              Right(())
            case e: Typedef.Enum =>
              Either.ifThenFail(e.members.isEmpty)(
                NEList(BaboonIssue.EmptyEnumDef(e, u.meta))
              )
            case a: Typedef.Adt =>
              Either.ifThenFail(a.members.isEmpty)(
                NEList(BaboonIssue.EmptyAdtDef(a, u.meta))
              )
          }
      }.biSequence_
    }

    private def checkConventions(
      domain: Domain
    ): Either[NEList[BaboonIssue.VerificationIssue], Unit] = {
      domain.defs.meta.nodes.values.map {
        case _: DomainMember.Builtin =>
          Right(())
        case u: DomainMember.User =>
          Either.ifThenFail(u.id.name.name.head == '_')(
            NEList(BaboonIssue.UnderscoredDefinitionRetained(u, u.meta))
          )
      }.biSequence_
    }

    private def validateEvolution(
      evolution: BaboonEvolution,
      versions: NEMap[Version, Domain]
    ): Either[NEList[BaboonIssue.VerificationIssue], Unit] = {

      //val latest = versions(evolution.latest)

      assert(evolution.diffs.keySet == evolution.rules.keySet)

      evolution.diffs.toSeq.map {
        case (v, diff) =>
          validateEvo(
            versions(v.to),
            versions(v.from),
            diff,
            evolution.rules(v)
          )
      }.biSequence_
    }

    private def validateEvo(
      next: Domain,
      prev: Domain,
      diff: BaboonDiff,
      ruleset: BaboonRuleset
    ): Either[NEList[BaboonIssue.VerificationIssue], Unit] = {
      val nextIds = next.defs.meta.nodes.keySet
      val diffIds = diff.diffs.keySet
      val missingDiffs = nextIds.diff(diffIds)

      val prevIds = prev.defs.meta.nodes.collect {
        case (id: TypeId.User, _) => id: TypeId
      }.toSet
      val conversionIds = ruleset.conversions.map(_.sourceTpe).toSet[TypeId]
      val missingConversions = prevIds.diff(conversionIds)
      val extraConversions = conversionIds.diff(prevIds)

      for {
        _ <- Either.ifThenFail(missingDiffs.isEmpty)(
          NEList(BaboonIssue.MissingEvoDiff(prev, next, missingDiffs))
        )
        _ <- Either.ifThenFail(
          missingConversions.nonEmpty || extraConversions.nonEmpty
        )(
          NEList(
            BaboonIssue.MissingEvoConversion(
              prev,
              next,
              missingConversions,
              extraConversions
            )
          )
        )
        _ <- ruleset.conversions.map {
          case _: Conversion.CustomConversionRequired =>
            Right(())
          case c: Conversion.RemovedTypeNoConversion =>
            Either.ifThenFail(!diff.changes.removed.contains(c.sourceTpe))(
              NEList(BaboonIssue.BrokenConversion(c))
            )
          case c: Conversion.CopyEnumByName =>
            val o = prev.defs.meta.nodes(c.sourceTpe)
            val n = next.defs.meta.nodes(c.sourceTpe)
            (o, n) match {
              case (
                  DomainMember.User(_, oe: Typedef.Enum, _),
                  DomainMember.User(_, ne: Typedef.Enum, _)
                  ) =>
                Either.ifThenFail(
                  oe.members.toSet.diff(ne.members.toSet).nonEmpty
                )(
                  NEList(
                    BaboonIssue.IncorrectConversionApplication(
                      c,
                      o,
                      n,
                      ConversionIssue.RemovedEnumBranches
                    )
                  )
                )
              case _ =>
                Left(
                  NEList(
                    BaboonIssue.IncorrectConversionApplication(
                      c,
                      o,
                      n,
                      ConversionIssue.TypeMismatch
                    )
                  )
                )
            }

          case c: Conversion.DtoConversion =>
            val o = prev.defs.meta.nodes(c.sourceTpe)
            val n = next.defs.meta.nodes(c.sourceTpe)
            (o, n) match {
              case (
                  DomainMember.User(_, od: Typedef.Dto, _),
                  DomainMember.User(_, nd: Typedef.Dto, _)
                  ) =>
                for {
                  newFieldNames <- Right(nd.fields.map(_.name).toSet)
                  oldFieldNames <- Right(od.fields.map(_.name).toSet)
                  removedFields <- Right(c.removed.map(_.name))
                  removals = oldFieldNames.diff(newFieldNames)
                  _ <- Either.ifThenFail(removals != removedFields)(
                    NEList(
                      BaboonIssue.IncorrectConversionApplication(
                        c,
                        o,
                        n,
                        ConversionIssue.RemovedDtoFields
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
                  _ <- Either.ifThenFail(newFieldNames != all) {
                    NEList(
                      BaboonIssue.IncorrectConversionApplication(
                        c,
                        o,
                        n,
                        ConversionIssue.IncorrectFields
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
                  _ <- Either.ifThenFail(conflicts.exists(_.nonEmpty))(
                    NEList(
                      BaboonIssue.IncorrectConversionApplication(
                        c,
                        o,
                        n,
                        ConversionIssue.ConflictingFields
                      )
                    )
                  )
                } yield {}

              case _ =>
                Left(
                  NEList(
                    BaboonIssue.IncorrectConversionApplication(
                      c,
                      o,
                      n,
                      ConversionIssue.TypeMismatch
                    )
                  )
                )
            }
          case c: Conversion.CopyAdtBranchByName =>
            val o = prev.defs.meta.nodes(c.sourceTpe)
            val n = next.defs.meta.nodes(c.sourceTpe)
            (o, n) match {
              case (
                  DomainMember.User(_, oa: Typedef.Adt, _),
                  DomainMember.User(_, na: Typedef.Adt, _)
                  ) =>
                Either.ifThenFail(
                  oa.members
                    .map(_.name)
                    .toSet
                    .diff(na.members.map(_.name).toSet)
                    .nonEmpty
                )(
                  NEList(
                    BaboonIssue.IncorrectConversionApplication(
                      c,
                      o,
                      n,
                      ConversionIssue.RemovedAdtBranches
                    )
                  )
                )
              case _ =>
                Left(
                  NEList(
                    BaboonIssue.IncorrectConversionApplication(
                      c,
                      o,
                      n,
                      ConversionIssue.TypeMismatch
                    )
                  )
                )
            }
        }.biSequence_
      } yield {}
    }

  }
}
