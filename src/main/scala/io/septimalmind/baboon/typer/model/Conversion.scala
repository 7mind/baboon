package io.septimalmind.baboon.typer.model

sealed trait Conversion {
  def sourceTpe: TypeId.User
}

object Conversion {
  case class CustomConversionRequired(sourceTpe: TypeId.User) extends Conversion

  case class RemovedTypeNoConversion(sourceTpe: TypeId.User) extends Conversion

  case class CopyEnumByName(sourceTpe: TypeId.User) extends Conversion

  case class Wrap(fieldName: FieldName,
                  oldTpe: TypeRef.Scalar,
                  newTpe: TypeRef.Constructor)

  case class SwapCollectionType(fieldName: FieldName,
                                oldTpe: TypeRef.Constructor,
                                newTpe: TypeRef.Constructor)

  case class DtoConversion(sourceTpe: TypeId.User,
                           transferFields: Set[Field],
                           wrapIntoCollection: Set[Wrap],
                           swapColllectionType: Set[SwapCollectionType],
                           initializeWithDefaults: Set[Field],
  ) extends Conversion

  case class CopyAdtBranchByName(sourceTpe: TypeId.User) extends Conversion
}
