package io.septimalmind.baboon.parser.model

import izumi.fundamentals.collections.nonempty.NonEmptyList

sealed trait RawTypeRef

object RawTypeRef {
  case class Simple(name: RawTypeName) extends RawTypeRef
  case class Constructor(name: RawTypeName, params: NonEmptyList[RawTypeRef])
      extends RawTypeRef
}
