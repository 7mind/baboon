package io.septimalmind.baboon.typer.model

sealed trait Conversion {
  def sourceTpe: TypeId.User
}

object Conversion {
  case class CustomConversionRequired(sourceTpe: TypeId.User) extends Conversion

  case class RemovedTypeNoConversion(sourceTpe: TypeId.User) extends Conversion

  case class CopyEnumByName(sourceTpe: TypeId.User) extends Conversion

//  case class Wrap()

  case class DtoConversion(sourceTpe: TypeId.User, ops: List[FieldOp])
      extends Conversion

  case class CopyAdtBranchByName(sourceTpe: TypeId.User, oldDefn: Typedef.Adt)
      extends Conversion

  sealed trait FieldOp {
    def targetField: Field
  }

  object FieldOp {
    case class Transfer(targetField: Field) extends FieldOp
    case class InitializeWithDefault(targetField: Field) extends FieldOp

    case class WrapIntoCollection(fieldName: FieldName,
                                  oldTpe: TypeRef.Scalar,
                                  newTpe: TypeRef.Constructor,
    ) extends FieldOp {
      def targetField: Field = Field(fieldName, newTpe)
      def sourceField: Field = Field(fieldName, oldTpe)
    }

    case class SwapCollectionType(fieldName: FieldName,
                                  oldTpe: TypeRef.Constructor,
                                  newTpe: TypeRef.Constructor)
        extends FieldOp {
      def targetField: Field = Field(fieldName, newTpe)

      def sourceField: Field = Field(fieldName, oldTpe)
    }

    case class ExpandPrecision(fieldName: FieldName,
                               oldTpe: TypeRef,
                               newTpe: TypeRef,
    ) extends FieldOp {
      def targetField: Field = Field(fieldName, newTpe)

      def sourceField: Field = Field(fieldName, oldTpe)
    }

  }
}
