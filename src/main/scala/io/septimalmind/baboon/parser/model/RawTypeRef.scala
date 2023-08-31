package io.septimalmind.baboon.parser.model

import izumi.fundamentals.collections.nonempty.NEList

sealed trait RawTypeRef

object RawTypeRef {
  case class Simple(name: RawTypeName) extends RawTypeRef
  case class Constructor(name: RawTypeName, params: NEList[RawTypeRef])
      extends RawTypeRef
}
