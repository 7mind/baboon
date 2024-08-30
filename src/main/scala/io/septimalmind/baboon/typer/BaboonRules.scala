package io.septimalmind.baboon.typer

import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.typer.model.*
import io.septimalmind.baboon.typer.model.Conversion.*
import io.septimalmind.baboon.util.BLogger
import izumi.functional.IzEither.*
import izumi.fundamentals.collections.nonempty.NEList
import izumi.fundamentals.platform.strings.TextTree.*

trait BaboonRules {
  def compute(
    prev: Domain,
    last: Domain,
    diff: BaboonDiff
  ): Either[NEList[BaboonIssue.EvolutionIssue], BaboonRuleset]
}

object BaboonRules {

  class BaboonRulesImpl(logger: BLogger) extends BaboonRules {
    override def compute(prev: Domain,
                         last: Domain,
                         diff: BaboonDiff,
    ): Either[NEList[BaboonIssue.EvolutionIssue], BaboonRuleset] = {
      for {
        conversions <- prev.defs.meta.nodes
          .collect {
            case (id: TypeId.User, DomainMember.User(_, defn, _)) =>
              (id, defn)
          }
          .toList
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
                    DtoConversion(
                      id,
                      d.fields.map(f => FieldOp.Transfer(f)),
                      Set.empty
                    )
                  )
                case _: Typedef.Contract =>
                  Right(NonDataTypeTypeNoConversion(id))
                case _: Typedef.Enum if sameLocalStruct =>
                  Right(CopyEnumByName(id))
                case oldDefn: Typedef.Adt if sameLocalStruct =>
                  Right(CopyAdtBranchByName(id, oldDefn))

                case _: Typedef.Foreign if sameLocalStruct =>
                  Right(CustomConversionRequired(id))

                case _ if diff.changes.removed.contains(id) =>
                  Right(RemovedTypeNoConversion(id))

                case _: Typedef.Foreign =>
                  Right(CustomConversionRequired(id))

                case _: Typedef.Dto =>
                  assert(shallowChanged)
                  for {
                    ops <- diff.diffs(id) match {
                      case TypedefDiff.DtoDiff(ops) =>
                        Right(ops)
                      case o =>
                        Left(
                          NEList(BaboonIssue.UnexpectedDiffType(o, "DTODiff"))
                        )
                    }

                    additions = ops.collect { case op: DtoOp.AddField => op }.toSet
                    removals = ops.collect {
                      case op: DtoOp.RemoveField => op.f
                    }.toSet

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
                        case (o: TypeRef.Scalar, n: TypeRef.Scalar) =>
                          // TODO: precex in collections
                          !TypeId.Builtins.isPrecisionExpansion(o.id, n.id)
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

                    precex = changes
                      .map(op => (op.f.tpe, op.newType, op.f.name))
                      .collect {
                        case (o: TypeRef.Scalar, n: TypeRef.Scalar, name)
                            if TypeId.Builtins.isPrecisionExpansion(
                              o.id,
                              n.id
                            ) =>
                          FieldOp.ExpandPrecision(name, o, n)
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
                        Right(ops.exists(_.isInstanceOf[EnumOp.RemoveBranch]))
                      case o =>
                        Left(
                          NEList(BaboonIssue.UnexpectedDiffType(o, "EnumDiff"))
                        )
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
                        Left(
                          NEList(BaboonIssue.UnexpectedDiffType(o, "ADTDiff"))
                        )
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
          .biSequence
      } yield {

        val total = conversions.size
        val user = conversions.collect {
          case c: CustomConversionRequired => c
        }.size
        val trivial = total - user

        logger.message(
          last.id.toString,
          q"${prev.version}->${last.version}: conversions: $total, derived: $trivial, to be implemented: $user"
        )

        BaboonRuleset(EvolutionStep(prev.version, last.version), conversions)
      }
    }

  }

}
