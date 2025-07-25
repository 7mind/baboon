package io.septimalmind.baboon.typer.model

sealed trait DerivationFailure

object DerivationFailure {
  case object Foreign extends DerivationFailure
  case class EnumBranchRemoved(op: List[EnumOp.RemoveBranch]) extends DerivationFailure
  case class AdtBranchRemoved(op: List[AdtOp.RemoveBranch]) extends DerivationFailure
  case class IncompatibleFields(incompatibleChanges: Set[DtoOp.ChangeField], incompatibleAdditions: Set[DtoOp.AddField]) extends DerivationFailure
}

sealed trait Conversion {
  def sourceTpe: TypeId.User
}

object Conversion {
  case class CustomConversionRequired(sourceTpe: TypeId.User, reason: DerivationFailure) extends Conversion

  case class RemovedTypeNoConversion(sourceTpe: TypeId.User) extends Conversion

  case class NonDataTypeTypeNoConversion(sourceTpe: TypeId.User) extends Conversion

  case class CopyEnumByName(sourceTpe: TypeId.User) extends Conversion

  case class DtoConversion(sourceTpe: TypeId.User, ops: List[FieldOp], removed: Set[Field]) extends Conversion

  case class CopyAdtBranchByName(sourceTpe: TypeId.User, oldDefn: Typedef.Adt) extends Conversion

  sealed trait FieldOp {
    def targetField: Field
  }

  object FieldOp {
    case class Transfer(targetField: Field) extends FieldOp

    // should applicable to collections only, don't break that
    case class InitializeWithDefault(targetField: Field) extends FieldOp

    case class WrapIntoCollection(fieldName: FieldName, oldTpe: TypeRef.Scalar, newTpe: TypeRef.Constructor) extends FieldOp {
      def targetField: Field = Field(fieldName, newTpe)
      def sourceField: Field = Field(fieldName, oldTpe)
    }

    case class SwapCollectionType(fieldName: FieldName, oldTpe: TypeRef.Constructor, newTpe: TypeRef.Constructor) extends FieldOp {
      def targetField: Field = Field(fieldName, newTpe)

      def sourceField: Field = Field(fieldName, oldTpe)
    }

    case class ExpandPrecision(fieldName: FieldName, oldTpe: TypeRef, newTpe: TypeRef) extends FieldOp {
      def targetField: Field = Field(fieldName, newTpe)

      def sourceField: Field = Field(fieldName, oldTpe)
    }
  }
}
