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
      // Build reverse lookup: old ID -> new ID for renames
      val renameTargets: Map[TypeId.User, TypeId.User] = diff.changes.renamed.map { case (newId, oldId) => (oldId, newId) }

      def computeBranchMapping(targetAdtId: TypeId.User): Map[String, TypeId.User] = {
        last.defs.meta.nodes(targetAdtId) match {
          case DomainMember.User(_, adt: Typedef.Adt, _, _) =>
            adt.members.map(m => (m.name.name, m)).toMap
          case _ =>
            Map.empty
        }
      }

      for {
        conversions <- F.traverseAccumErrors(prev.defs.meta.nodes.collect {
          case (id: TypeId.User, DomainMember.User(_, defn, _, _)) =>
            (id, defn)
        }.toList) {
          case (id, defn) =>
            // Check if this type was renamed to a new type
            val maybeTargetTpe = renameTargets.get(id)
            val isRenamed      = maybeTargetTpe.isDefined
            val targetTpe      = maybeTargetTpe.getOrElse(id)

            val unmodified  = diff.changes.unmodified.contains(id)
            val deepChanged = diff.changes.deepModified.contains(id)

            // For renamed types, check if there's a diff entry (structure may have changed along with rename)
            val hasDiff          = diff.diffs.contains(id)
            val sameLocalStruct  = (unmodified || deepChanged) && !isRenamed || (isRenamed && !hasDiff)

            val shallowChanged = diff.changes.shallowModified.contains(id) || diff.changes.fullyModified
              .contains(id) || (isRenamed && hasDiff)

            defn match {
              case d: Typedef.Dto if sameLocalStruct =>
                F.pure(
                  DtoConversion(
                    id,
                    d.fields.map(f => FieldOp.Transfer(f)),
                    Set.empty,
                    targetTpe,
                  )
                )
              case _: Typedef.Contract =>
                F.pure(NonDataTypeTypeNoConversion(id))
              case _: Typedef.Service =>
                F.pure(NonDataTypeTypeNoConversion(id))
              case _: Typedef.Enum if sameLocalStruct =>
                F.pure(CopyEnumByName(id, targetTpe))
              case oldDefn: Typedef.Adt if sameLocalStruct =>
                F.pure(CopyAdtBranchByName(id, oldDefn, targetTpe, computeBranchMapping(targetTpe)))

              case _ if diff.changes.removed.contains(id) && !isRenamed =>
                F.pure(RemovedTypeNoConversion(id))

              case _: Typedef.Foreign if sameLocalStruct =>
                F.pure(NonDataTypeTypeNoConversion(id))

              case _: Typedef.Foreign =>
                F.pure(CustomConversionRequired(id, DerivationFailure.Foreign, targetTpe))

              case _: Typedef.Dto =>
                assert(shallowChanged || isRenamed)
                for {
                  ops <- diff.diffs.get(id) match {
                    case Some(TypedefDiff.DtoDiff(ops)) =>
                      F.pure(ops)
                    case Some(o) =>
                      F.fail(
                        BaboonIssue.of(EvolutionIssue.UnexpectedDiffType(o, "DTODiff"))
                      )
                    case None if isRenamed =>
                      // Renamed without structural changes - treat all fields as transfers
                      val newDefn = last.defs.meta.nodes(targetTpe).asInstanceOf[DomainMember.User].defn.asInstanceOf[Typedef.Dto]
                      F.pure(newDefn.fields.map(f => DtoOp.KeepField(f, RefModification.Unchanged)).toList)
                    case None =>
                      F.fail(
                        BaboonIssue.of(EvolutionIssue.UnexpectedDiffType(TypedefDiff.DtoDiff(List.empty), "DTODiff"))
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

                  renames = ops.collect { case op: DtoOp.RenameField => op }.toSet

                  pureRenames = renames.filter(op => op.oldField.tpe == op.newField.tpe)
                  renamesWithTypeChange = renames.diff(pureRenames)

                  incompatibleRenames = renamesWithTypeChange.filter { op =>
                    !types.isCompatibleChange(op.oldField.tpe, op.newField.tpe)
                  }

                  pureRenameOps = pureRenames.map { op =>
                    FieldOp.Rename(op.oldField.name, op.newField)
                  }

                  redefWrap = renamesWithTypeChange
                    .map(op => (op.oldField.tpe, op.newField.tpe, op))
                    .collect {
                      case (o: TypeRef.Scalar, n: TypeRef.Constructor, op)
                          if types.canBeWrappedIntoCollection(o, n) =>
                        FieldOp.Redef(
                          op.oldField.name,
                          op.newField,
                          FieldOp.WrapIntoCollection(op.newField.name, o, n),
                        )
                    }

                  redefPrecex = renamesWithTypeChange
                    .map(op => (op.oldField.tpe, op.newField.tpe, op))
                    .collect {
                      case (o: TypeRef.Scalar, n: TypeRef.Scalar, op)
                          if types.isPrecisionExpansion(o.id, n.id) =>
                        FieldOp.Redef(
                          op.oldField.name,
                          op.newField,
                          FieldOp.ExpandPrecision(op.newField.name, o, n),
                        )
                    }

                  redefSwap = renamesWithTypeChange
                    .map(op => (op.oldField.tpe, op.newField.tpe, op))
                    .collect {
                      case (o: TypeRef.Constructor, n: TypeRef.Constructor, op)
                          if types.canChangeCollectionType(o, n) =>
                        FieldOp.Redef(
                          op.oldField.name,
                          op.newField,
                          FieldOp.SwapCollectionType(op.newField.name, o, n),
                        )
                    }

                  redefOps = redefWrap ++ redefPrecex ++ redefSwap
                } yield {
                  assert(
                    wrap
                      .map(_.fieldName)
                      .intersect(incompatibleChanges.map(_.f.name))
                      .isEmpty
                  )
                  if (incompatibleChanges.nonEmpty || incompatibleAdditions.nonEmpty) {
                    CustomConversionRequired(id, DerivationFailure.IncompatibleFields(incompatibleChanges, incompatibleAdditions), targetTpe)
                  } else if (incompatibleRenames.nonEmpty) {
                    CustomConversionRequired(id, DerivationFailure.IncompatibleRenames(incompatibleRenames), targetTpe)
                  } else {
                    DtoConversion(
                      id,
                      keepFields ++ pureRenameOps.toList ++ redefOps.toList ++ (wrap ++ precex ++ swap ++ initWithDefaults).toList,
                      removals,
                      targetTpe,
                    )
                  }
                }

              case _: Typedef.Enum =>
                assert(shallowChanged || isRenamed)
                for {
                  incompatible <- diff.diffs.get(id) match {
                    case Some(TypedefDiff.EnumDiff(ops)) =>
                      F.pure(ops.collect { case r: EnumOp.RemoveBranch => r })
                    case Some(o) =>
                      F.fail(
                        BaboonIssue.of(EvolutionIssue.UnexpectedDiffType(o, "EnumDiff"))
                      )
                    case None if isRenamed =>
                      F.pure(List.empty[EnumOp.RemoveBranch])
                    case None =>
                      F.fail(
                        BaboonIssue.of(EvolutionIssue.UnexpectedDiffType(TypedefDiff.EnumDiff(List.empty), "EnumDiff"))
                      )
                  }
                } yield {
                  if (incompatible.isEmpty) {
                    CopyEnumByName(id, targetTpe)
                  } else {
                    CustomConversionRequired(id, DerivationFailure.EnumBranchRemoved(incompatible), targetTpe)
                  }
                }

              case a: Typedef.Adt =>
                assert(shallowChanged || isRenamed)
                for {
                  incompatible <- diff.diffs.get(id) match {
                    case Some(TypedefDiff.AdtDiff(ops)) =>
                      F.pure(ops.collect { case r: AdtOp.RemoveBranch => r })
                    case Some(o) =>
                      F.fail(
                        BaboonIssue.of(EvolutionIssue.UnexpectedDiffType(o, "ADTDiff"))
                      )
                    case None if isRenamed =>
                      F.pure(List.empty[AdtOp.RemoveBranch])
                    case None =>
                      F.fail(
                        BaboonIssue.of(EvolutionIssue.UnexpectedDiffType(TypedefDiff.AdtDiff(List.empty), "ADTDiff"))
                      )
                  }
                } yield {
                  if (incompatible.isEmpty) {
                    CopyAdtBranchByName(id, a, targetTpe, computeBranchMapping(targetTpe))
                  } else {
                    CustomConversionRequired(id, DerivationFailure.AdtBranchRemoved(incompatible), targetTpe)
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
