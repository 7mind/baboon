package io.septimalmind.baboon.typer

import io.septimalmind.baboon.parser.model.issues.{BaboonIssue, EvolutionIssue}
import io.septimalmind.baboon.typer.model.*
import io.septimalmind.baboon.typer.model.Conversion.*
import io.septimalmind.baboon.util.BLogger
import izumi.functional.bio.{Error2, F}
import izumi.fundamentals.collections.nonempty.NEList
import izumi.fundamentals.platform.strings.TextTree.*

trait BaboonRules[F[+_, +_]] {
  def compute(
    prev: Domain,
    last: Domain,
    diff: BaboonDiff,
  ): F[NEList[BaboonIssue], BaboonRuleset]
}

object BaboonRules {

  class BaboonRulesImpl[F[+_, +_]: Error2](
    logger: BLogger,
    types: TypeInfo,
  ) extends BaboonRules[F] {

    override def compute(prev: Domain, last: Domain, diff: BaboonDiff): F[NEList[BaboonIssue], BaboonRuleset] = {
      for {
        conversions <- F.traverseAccumErrors(prev.defs.meta.nodes.collect {
          case (id: TypeId.User, DomainMember.User(_, defn, _, _)) =>
            (id, defn)
        }.toList) {
          case (id, defn) =>
            val unmodified  = diff.changes.unmodified.contains(id)
            val deepChanged = diff.changes.deepModified.contains(id)

            val sameLocalStruct = unmodified || deepChanged

            val shallowChanged = diff.changes.shallowModified.contains(id) || diff.changes.fullyModified
              .contains(id)

            defn match {
              case d: Typedef.Dto if sameLocalStruct =>
                F.pure(
                  DtoConversion(
                    id,
                    d.fields.map(f => FieldOp.Transfer(f)),
                    Set.empty,
                  )
                )
              case _: Typedef.Contract =>
                F.pure(NonDataTypeTypeNoConversion(id))
              case _: Typedef.Service =>
                F.pure(NonDataTypeTypeNoConversion(id))
              case _: Typedef.Enum if sameLocalStruct =>
                F.pure(CopyEnumByName(id))
              case oldDefn: Typedef.Adt if sameLocalStruct =>
                F.pure(CopyAdtBranchByName(id, oldDefn))

              case _ if diff.changes.removed.contains(id) =>
                F.pure(RemovedTypeNoConversion(id))

              case _: Typedef.Foreign if sameLocalStruct =>
                F.pure(NonDataTypeTypeNoConversion(id))

              case _: Typedef.Foreign =>
                F.pure(CustomConversionRequired(id, DerivationFailure.Foreign))

              case _: Typedef.Dto =>
                assert(shallowChanged)
                for {
                  ops <- diff.diffs(id) match {
                    case TypedefDiff.DtoDiff(ops) =>
                      F.pure(ops)
                    case o =>
                      F.fail(
                        BaboonIssue.of(EvolutionIssue.UnexpectedDiffType(o, "DTODiff"))
                      )
                  }

                  additions = ops.collect { case op: DtoOp.AddField => op }.toSet
                  removals = ops.collect {
                    case op: DtoOp.RemoveField => op.f
                  }.toSet

                  incompatibleAdditions = additions.filter {
                    op =>
                      op.f.tpe match {
                        case _: TypeRef.Scalar =>
                          true
                        case c: TypeRef.Constructor =>
                          !types.hasDefaultValue(c)
                      }
                  }
                  compatibleAdditions = additions.diff(incompatibleAdditions)
                  changes             = ops.collect { case op: DtoOp.ChangeField => op }.toSet
                  incompatibleChanges = changes.filter {
                    op =>
                      assert(op.f.tpe != op.newType)
                      !types.isCompatibleChange(op.f.tpe, op.newType)
                  }
//                  _               <- F.pure(if (incompatibleChanges.nonEmpty) println(s"!! ${defn.id}: $incompatibleChanges"))
                  initWithDefaults = compatibleAdditions.map(a => FieldOp.InitializeWithDefault(a.f))

                  wrap = changes
                    .map(op => (op.f.tpe, op.newType, op.f.name))
                    .collect {
                      case (o: TypeRef.Scalar, n: TypeRef.Constructor, name)
                          if types.canBeWrappedIntoCollection(
                            o,
                            n,
                          ) =>
                        FieldOp.WrapIntoCollection(name, o, n)
                    }

                  precex = changes
                    .map(op => (op.f.tpe, op.newType, op.f.name))
                    .collect {
                      case (o: TypeRef.Scalar, n: TypeRef.Scalar, name)
                          if types.isPrecisionExpansion(
                            o.id,
                            n.id,
                          ) =>
                        FieldOp.ExpandPrecision(name, o, n)
                    }

                  swap = changes
                    .map(op => (op.f.tpe, op.newType, op.f.name))
                    .collect {
                      case (
                            o: TypeRef.Constructor,
                            n: TypeRef.Constructor,
                            name,
                          ) if types.canChangeCollectionType(o, n) =>
                        FieldOp.SwapCollectionType(name, o, n)
                    }

                  keepFields = ops.collect {
                    case op: DtoOp.KeepField => FieldOp.Transfer(op.f)
                  }
                } yield {
                  assert(
                    wrap
                      .map(_.fieldName)
                      .intersect(incompatibleChanges.map(_.f.name))
                      .isEmpty
                  )
                  if (incompatibleChanges.nonEmpty || incompatibleAdditions.nonEmpty) {
                    CustomConversionRequired(id, DerivationFailure.IncompatibleFields(incompatibleChanges, incompatibleAdditions))
                  } else {
                    DtoConversion(
                      id,
                      keepFields ++ (wrap ++ precex ++ swap ++ initWithDefaults).toList,
                      removals,
                    )
                  }
                }

              case _: Typedef.Enum =>
                assert(shallowChanged)
                for {
                  incompatible <- diff.diffs(id) match {
                    case TypedefDiff.EnumDiff(ops) =>
                      F.pure(ops.collect { case r: EnumOp.RemoveBranch => r })
                    case o =>
                      F.fail(
                        BaboonIssue.of(EvolutionIssue.UnexpectedDiffType(o, "EnumDiff"))
                      )
                  }
                } yield {
                  if (incompatible.isEmpty) {
                    CopyEnumByName(id)
                  } else {
                    CustomConversionRequired(id, DerivationFailure.EnumBranchRemoved(incompatible))
                  }
                }

              case a: Typedef.Adt =>
                assert(shallowChanged)
                for {
                  incompatible <- diff.diffs(id) match {
                    case TypedefDiff.AdtDiff(ops) =>
                      F.pure(ops.collect { case r: AdtOp.RemoveBranch => r })
                    case o =>
                      F.fail(
                        BaboonIssue.of(EvolutionIssue.UnexpectedDiffType(o, "ADTDiff"))
                      )
                  }
                } yield {
                  if (incompatible.isEmpty) {
                    CopyAdtBranchByName(id, a)
                  } else {
                    CustomConversionRequired(id, DerivationFailure.AdtBranchRemoved(incompatible))
                  }
                }

            }
        }
      } yield {

        val total = conversions.size
        val user = conversions.collect {
          case c: CustomConversionRequired => c
        }.size
        val trivial = total - user

        logger.message(
          last.id.toString,
          q"${prev.version}->${last.version}: conversions: $total, derived: $trivial, to be implemented: $user",
        )

        BaboonRuleset(EvolutionStep(prev.version, last.version), conversions)
      }
    }

  }

}
