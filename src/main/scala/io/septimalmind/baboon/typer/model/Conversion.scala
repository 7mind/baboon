package io.septimalmind.baboon.typer.model

sealed trait Conversion {
  def tpe: TypeId.User
}

object Conversion {
  case class CustomConversionRequired(tpe: TypeId.User) extends Conversion

  case class RemovedTypeNoConversion(tpe: TypeId.User) extends Conversion

  case class CopyEnumByName(tpe: TypeId.User) extends Conversion

  case class Wrap(fieldName: FieldName,
                  oldTpe: TypeRef.Scalar,
                  newTpe: TypeRef.Constructor)

  case class SwapCollectionType(fieldName: FieldName,
                                oldTpe: TypeRef.Constructor,
                                newTpe: TypeRef.Constructor)

  case class DtoConversion(tpe: TypeId.User,
                           transferFields: Set[Field],
                           wrapIntoCollection: Set[Wrap],
                           swapColllectionType: Set[SwapCollectionType],
                           initializeWithDefaults: Set[Field],
  ) extends Conversion

  case class CopyAdtBranchByName(tpe: TypeId.User) extends Conversion
}
