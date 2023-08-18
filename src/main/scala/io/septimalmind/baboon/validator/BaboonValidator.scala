package io.septimalmind.baboon.validator

import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.parser.model.issues.BaboonIssue.ConversionIssue
import io.septimalmind.baboon.typer.BaboonEnquiries
import io.septimalmind.baboon.typer.model.*
import io.septimalmind.baboon.typer.model.Conversion.FieldOp
import izumi.functional.IzEitherAggregations.*
import izumi.fundamentals.collections.nonempty.{NonEmptyList, NonEmptyMap}
import izumi.fundamentals.graphs.struct.IncidenceMatrix
import izumi.fundamentals.graphs.tools.cycles.LoopDetector

object IzEitherTmp {
  @inline implicit final def EitherObjectExt(e: Either.type): EitherExt =
    new EitherExt(e)

  final class EitherExt(val e: Either.type) extends AnyVal {
    def failWhen[A](cond: Boolean)(fun: => A): Either[A, Unit] = {
      if (cond) {
        Left(fun)
      } else {
        Right(())
      }
    }
  }
}

trait BaboonValidator {
  def validate(
    family: BaboonFamily
  ): Either[NonEmptyList[BaboonIssue.VerificationIssue], Unit]
}

object BaboonValidator {
  class BaboonValidatorImpl() extends BaboonValidator {
    import IzEitherTmp.*
    private val enquiries = new BaboonEnquiries.BaboonEnquiriesImpl()

    override def validate(
      family: BaboonFamily
    ): Either[NonEmptyList[BaboonIssue.VerificationIssue], Unit] = {
      family.domains.toSeq.map {
        case (pkg, lineage) =>
          validateLineage(pkg, lineage)
      }.biAggregateVoid
    }

    private def validateLineage(
      pkg: Pkg,
      lineage: BaboonLineage
    ): Either[NonEmptyList[BaboonIssue.VerificationIssue], Unit] = {
      assert(lineage.pkg == pkg)

      for {
        _ <- lineage.versions.toSeq.map {
          case (v, domain) =>
            validateDomain(v, domain)
        }.biAggregateVoid
        _ <- validateEvolution(lineage.evolution, lineage.versions)
      } yield {}

    }

    private def validateDomain(
      version: Version,
      domain: Domain
    ): Either[NonEmptyList[BaboonIssue.VerificationIssue], Unit] = {
      assert(domain.version == version)

      for {
        _ <- checkMissingTypes(domain)
        _ <- checkLoops(domain)
        _ <- checkUniqueness(domain)
        _ <- checkShape(domain)
      } yield {}

    }

    private def checkLoops(
      domain: Domain
    ): Either[NonEmptyList[BaboonIssue.VerificationIssue], Unit] = {
      val depMatrix = IncidenceMatrix(domain.defs.meta.nodes.view.mapValues {
        defn =>
          enquiries.directDepsOf(defn)
      }.toMap)

      val loops =
        LoopDetector.Impl.findCyclesForNodes(depMatrix.links.keySet, depMatrix)
      Either.failWhen(loops.nonEmpty)(
        NonEmptyList(BaboonIssue.ReferentialCyclesFound(domain, loops))
      )
    }

    private def checkMissingTypes(
      domain: Domain
    ): Either[NonEmptyList[BaboonIssue.VerificationIssue], Unit] = {
      val allDeps = domain.defs.meta.nodes.values.flatMap { defn =>
        enquiries.directDepsOf(defn)
      }.toSet

      val allDefs = domain.defs.meta.nodes.keySet
      val missing = allDeps.diff(allDefs)
      Either.failWhen(missing.nonEmpty)(
        NonEmptyList(BaboonIssue.MissingTypeDef(domain, missing))
      )
    }

    private def checkUniqueness(
      domain: Domain
    ): Either[NonEmptyList[BaboonIssue.VerificationIssue], Unit] = {
      for {
        _ <- {
          val dupes =
            domain.defs.meta.nodes.values
              .groupBy {
                case DomainMember.Builtin(id) => id.name.name
                case DomainMember.User(_, defn) =>
                  defn.id.owner match {
                    case Owner.Toplevel => defn.id.name.name
                    case Owner.Adt(id) =>
                      s"${id.name.name}#${defn.id.name.name}"
                  }
              }
              .filter(_._2.size > 1)
          Either.failWhen(dupes.nonEmpty)(
            NonEmptyList(BaboonIssue.ConflictingTypeIds(domain, dupes))
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
                Either.failWhen(dupes.nonEmpty)(
                  NonEmptyList(BaboonIssue.ConflictingDtoFields(d, dupes))
                )
              case e: Typedef.Enum =>
                val dupes =
                  e.members.groupBy(_.name.toLowerCase).filter(_._2.size > 1)
                Either.failWhen(dupes.nonEmpty)(
                  NonEmptyList(BaboonIssue.ConflictingEnumBranches(e, dupes))
                )
              case a: Typedef.Adt =>
                val dupes =
                  a.members
                    .groupBy(_.name.name.toLowerCase)
                    .filter(_._2.size > 1)
                Either.failWhen(dupes.nonEmpty)(
                  NonEmptyList(BaboonIssue.ConflictingAdtBranches(a, dupes))
                )

            }
        }.biAggregateVoid
      } yield {}
    }

    private def checkShape(
      domain: Domain
    ): Either[NonEmptyList[BaboonIssue.VerificationIssue], Unit] = {
      domain.defs.meta.nodes.values.map {
        case _: DomainMember.Builtin =>
          Right(())
        case u: DomainMember.User =>
          u.defn match {
            case _: Typedef.Dto =>
              Right(())
            case e: Typedef.Enum =>
              Either.failWhen(e.members.isEmpty)(
                NonEmptyList(BaboonIssue.EmptyEnumDef(e))
              )
            case a: Typedef.Adt =>
              Either.failWhen(a.members.isEmpty)(
                NonEmptyList(BaboonIssue.EmptyAdtDef(a))
              )
          }
      }.biAggregateVoid
    }

    private def validateEvolution(
      evolution: BaboonEvolution,
      versions: NonEmptyMap[Version, Domain]
    ): Either[NonEmptyList[BaboonIssue.VerificationIssue], Unit] = {

      val latest = versions(evolution.latest)

      assert(evolution.diffs.keySet == evolution.rules.keySet)

      evolution.diffs.toSeq.map {
        case (v, diff) =>
          validateEvo(latest, versions(v), diff, evolution.rules(v))
      }.biAggregateVoid
    }

    private def validateEvo(
      next: Domain,
      prev: Domain,
      diff: BaboonDiff,
      ruleset: BaboonRuleset
    ): Either[NonEmptyList[BaboonIssue.VerificationIssue], Unit] = {
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
        _ <- Either.failWhen(missingDiffs.isEmpty)(
          NonEmptyList(BaboonIssue.MissingEvoDiff(prev, next, missingDiffs))
        )
        _ <- Either.failWhen(
          missingConversions.nonEmpty || extraConversions.nonEmpty
        )(
          NonEmptyList(
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
            Either.failWhen(!diff.changes.removed.contains(c.sourceTpe))(
              NonEmptyList(BaboonIssue.BrokenConversion(c))
            )
          case c: Conversion.CopyEnumByName =>
            val o = prev.defs.meta.nodes(c.sourceTpe)
            val n = prev.defs.meta.nodes(c.sourceTpe)
            (o, n) match {
              case (
                  DomainMember.User(_, oe: Typedef.Enum),
                  DomainMember.User(_, ne: Typedef.Enum)
                  ) =>
                Either.failWhen(
                  oe.members.toSet.diff(ne.members.toSet).nonEmpty
                )(
                  NonEmptyList(
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
                  NonEmptyList(
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
            val n = prev.defs.meta.nodes(c.sourceTpe)
            (o, n) match {
              case (
                  DomainMember.User(_, od: Typedef.Dto),
                  DomainMember.User(_, nd: Typedef.Dto)
                  ) =>
                for {
                  newFieldNames <- Right(nd.fields.map(_.name).toSet)
                  oldFieldNames <- Right(od.fields.map(_.name).toSet)
                  _ <- Either.failWhen(
                    newFieldNames
                      .diff(oldFieldNames)
                      .nonEmpty
                  )(
                    NonEmptyList(
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
                  wrap = c.ops.collect {
                    case f: FieldOp.WrapIntoCollection => f.fieldName
                  }.toSet
                  all = transfer ++ defaults ++ swap ++ wrap
                  _ <- Either.failWhen(newFieldNames != all)(
                    NonEmptyList(
                      BaboonIssue.IncorrectConversionApplication(
                        c,
                        o,
                        n,
                        ConversionIssue.IncorrectFields
                      )
                    )
                  )
                  conflicts = Seq(
                    transfer.intersect(all.diff(transfer)),
                    defaults.intersect(all.diff(defaults)),
                    swap.intersect(all.diff(swap)),
                    wrap.intersect(all.diff(wrap)),
                  )
                  _ <- Either.failWhen(conflicts.exists(_.nonEmpty))(
                    NonEmptyList(
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
                  NonEmptyList(
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
            val n = prev.defs.meta.nodes(c.sourceTpe)
            (o, n) match {
              case (
                  DomainMember.User(_, oa: Typedef.Adt),
                  DomainMember.User(_, na: Typedef.Adt)
                  ) =>
                Either.failWhen(
                  oa.members
                    .map(_.name)
                    .toSet
                    .diff(na.members.map(_.name).toSet)
                    .nonEmpty
                )(
                  NonEmptyList(
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
                  NonEmptyList(
                    BaboonIssue.IncorrectConversionApplication(
                      c,
                      o,
                      n,
                      ConversionIssue.TypeMismatch
                    )
                  )
                )
            }
        }.biAggregateVoid
      } yield {}
    }

  }
}
