package io.septimalmind.baboon.parser.model

import izumi.fundamentals.collections.nonempty.NonEmptyList

sealed trait RawTypeRef

object RawTypeRef {
  case class Simple(name: TypeName) extends RawTypeRef
  case class Constructor(name: TypeName, params: NonEmptyList[RawTypeRef])
      extends RawTypeRef
}
