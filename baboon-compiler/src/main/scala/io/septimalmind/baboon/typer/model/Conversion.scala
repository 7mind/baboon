package io.septimalmind.baboon.typer.model

sealed trait DerivationFailure

object DerivationFailure {
  case object Foreign extends DerivationFailure
  case class EnumBranchRemoved(op: List[EnumOp.RemoveBranch]) extends DerivationFailure
  case class AdtBranchRemoved(op: List[AdtOp.RemoveBranch]) extends DerivationFailure
  case class IncompatibleFields(incompatibleChanges: Set[DtoOp.ChangeField], incompatibleAdditions: Set[DtoOp.AddField]) extends DerivationFailure
  case class IncompatibleRenames(incompatibleRenames: Set[DtoOp.RenameField]) extends DerivationFailure
}

sealed trait Conversion {
  def sourceTpe: TypeId.User
}

sealed trait TargetedConversion extends Conversion {
  def targetTpe: TypeId.User
}

object Conversion {
  case class CustomConversionRequired(sourceTpe: TypeId.User, reason: DerivationFailure, targetTpe: TypeId.User) extends TargetedConversion

  case class RemovedTypeNoConversion(sourceTpe: TypeId.User) extends Conversion

  case class NonDataTypeTypeNoConversion(sourceTpe: TypeId.User) extends Conversion

  case class CopyEnumByName(sourceTpe: TypeId.User, targetTpe: TypeId.User) extends TargetedConversion

  case class DtoConversion(sourceTpe: TypeId.User, ops: List[FieldOp], removed: Set[Field], targetTpe: TypeId.User) extends TargetedConversion

  /** @param branchMapping Maps old branch name to new branch TypeId */
  case class CopyAdtBranchByName(
    sourceTpe: TypeId.User,
    oldDefn: Typedef.Adt,
    targetTpe: TypeId.User,
    branchMapping: Map[String, TypeId.User],
  ) extends TargetedConversion

  sealed trait FieldOp {
    def targetField: Field
  }

  object FieldOp {
    case class Transfer(targetField: Field) extends FieldOp

    case class InitializeWithDefault(targetField: Field) extends FieldOp

    case class Rename(sourceFieldName: FieldName, targetFieldDef: Field) extends FieldOp {
      def targetField: Field = targetFieldDef

      def sourceField: Field = Field(sourceFieldName, targetFieldDef.tpe, None)
    }

    case class Redef(sourceFieldName: FieldName, targetFieldDef: Field, modify: Modify) extends FieldOp {
      def targetField: Field = targetFieldDef

      def sourceField: Field = Field(sourceFieldName, modify.oldTpe, None)
    }

    sealed trait Modify extends FieldOp {
      def fieldName: FieldName
      def oldTpe: TypeRef
      def newTpe: TypeRef
      def sourceField: Field
    }

    case class WrapIntoCollection(fieldName: FieldName, oldTpe: TypeRef.Scalar, newTpe: TypeRef.Constructor) extends Modify {
      def targetField: Field = Field(fieldName, newTpe, None)
      def sourceField: Field = Field(fieldName, oldTpe, None)
    }

    case class SwapCollectionType(fieldName: FieldName, oldTpe: TypeRef.Constructor, newTpe: TypeRef.Constructor) extends Modify {
      def targetField: Field = Field(fieldName, newTpe, None)

      def sourceField: Field = Field(fieldName, oldTpe, None)
    }

    case class ExpandPrecision(fieldName: FieldName, oldTpe: TypeRef, newTpe: TypeRef) extends Modify {
      def targetField: Field = Field(fieldName, newTpe, None)

      def sourceField: Field = Field(fieldName, oldTpe, None)
    }
  }
}
