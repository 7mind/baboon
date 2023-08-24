package io.septimalmind.baboon.typer

import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.typer.model.*
import io.septimalmind.baboon.typer.model.Conversion.*
import izumi.functional.IzEitherAggregations.*
import izumi.fundamentals.collections.nonempty.NonEmptyList

trait BaboonRules {
  def compute(
    prev: Domain,
    last: Domain,
    diff: BaboonDiff
  ): Either[NonEmptyList[BaboonIssue.EvolutionIssue], BaboonRuleset]
}

object BaboonRules {

  class BaboonRulesImpl() extends BaboonRules {
    override def compute(
      prev: Domain,
      last: Domain,
      diff: BaboonDiff
    ): Either[NonEmptyList[BaboonIssue.EvolutionIssue], BaboonRuleset] = {
      for {
        conversions <- prev.defs.meta.nodes
          .collect {
            case (id: TypeId.User, DomainMember.User(_, defn)) =>
              (id, defn)
          }
          .map {
            case (id, defn) =>
              val unmodified = diff.changes.unmodified.contains(id)
              val deepChanged = diff.changes.deepModified.contains(id)

              val sameLocalStruct = unmodified || deepChanged

              val shallowChanged = diff.changes.shallowModified.contains(id) || diff.changes.fullyModified
                .contains(id)

              defn match {
                case d: Typedef.Dto if sameLocalStruct =>
                  Right(
                    DtoConversion(id, d.fields.map(f => FieldOp.Transfer(f)))
                  )
                case _: Typedef.Enum if sameLocalStruct =>
                  Right(CopyEnumByName(id))
                case oldDefn: Typedef.Adt if sameLocalStruct =>
                  Right(CopyAdtBranchByName(id, oldDefn))

                case _ if diff.changes.removed.contains(id) =>
                  Right(RemovedTypeNoConversion(id))

                case _: Typedef.Dto =>
                  assert(shallowChanged)
                  for {
                    ops <- diff.diffs(id) match {
                      case TypedefDiff.DtoDiff(ops) =>
                        Right(ops)
                      case o =>
                        Left(NonEmptyList(BaboonIssue.UnexpectedDiffType(o)))
                    }

                    additions = ops.collect { case op: DtoOp.AddField => op }.toSet
                    incompatibleAdditions = additions.filter { op =>
                      op.f.tpe match {
                        case _: TypeRef.Scalar =>
                          true
                        case c: TypeRef.Constructor =>
                          !TypeId.Builtins.hasDefaultValue(c)
                      }
                    }
                    compatibleAdditions = additions.diff(incompatibleAdditions)
                    changes = ops.collect { case op: DtoOp.ChangeField => op }.toSet
                    incompatibleChanges = changes.filter { op =>
                      assert(op.f.tpe != op.newType)
                      (op.f.tpe, op.newType) match {
                        case (_: TypeRef.Constructor, _: TypeRef.Scalar) =>
                          true
                        case (_: TypeRef.Scalar, _: TypeRef.Scalar) =>
                          // here we may support precision expansion
                          true
                        case (o: TypeRef.Scalar, n: TypeRef.Constructor) =>
                          !TypeId.Builtins.canBeWrappedIntoCollection(o, n)
                        case (o: TypeRef.Constructor, n: TypeRef.Constructor) =>
                          !TypeId.Builtins.canChangeCollectionType(o, n)
                      }
                    }
                    initWithDefaults = compatibleAdditions.map(
                      a => FieldOp.InitializeWithDefault(a.f)
                    )

                    wrap = changes
                      .map(op => (op.f.tpe, op.newType, op.f.name))
                      .collect {
                        case (o: TypeRef.Scalar, n: TypeRef.Constructor, name)
                            if TypeId.Builtins.canBeWrappedIntoCollection(
                              o,
                              n
                            ) =>
                          FieldOp.WrapIntoCollection(name, o, n)
                      }

                    swap = changes
                      .map(op => (op.f.tpe, op.newType, op.f.name))
                      .collect {
                        case (
                            o: TypeRef.Constructor,
                            n: TypeRef.Constructor,
                            name
                            )
                            if TypeId.Builtins.canChangeCollectionType(o, n) =>
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
                      CustomConversionRequired(id)
                    } else {
                      DtoConversion(
                        id,
                        keepFields ++ (wrap ++ swap ++ initWithDefaults).toList
                      )
                    }
                  }

                case _: Typedef.Enum =>
                  assert(shallowChanged)
                  for {
                    incompatible <- diff.diffs(id) match {
                      case TypedefDiff.EnumDiff(ops) =>
                        Right(ops.exists(_.isInstanceOf[EnumOp.RemoveBranch]))
                      case o =>
                        Left(NonEmptyList(BaboonIssue.UnexpectedDiffType(o)))
                    }
                  } yield {
                    if (incompatible) {
                      CustomConversionRequired(id)
                    } else {
                      CopyEnumByName(id)
                    }
                  }

                case a: Typedef.Adt =>
                  assert(shallowChanged)
                  for {
                    incompatible <- diff.diffs(id) match {
                      case TypedefDiff.AdtDiff(ops) =>
                        Right(ops.exists(_.isInstanceOf[AdtOp.RemoveBranch]))
                      case o =>
                        Left(NonEmptyList(BaboonIssue.UnexpectedDiffType(o)))
                    }
                  } yield {
                    if (incompatible) {
                      CustomConversionRequired(id)
                    } else {
                      CopyAdtBranchByName(id, a)
                    }
                  }

              }
          }
          .biAggregate
      } yield {
        BaboonRuleset(conversions.toList)
      }
    }
  }

}
