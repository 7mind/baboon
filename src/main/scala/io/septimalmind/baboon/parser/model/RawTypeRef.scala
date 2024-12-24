package io.septimalmind.baboon.parser.model

import izumi.fundamentals.collections.nonempty.NEList

sealed trait RawTypeRef

object RawTypeRef {
  case class Simple(name: RawTypeName, prefix: List[RawTypeName]) extends RawTypeRef
  case class Constructor(name: RawTypeName, params: NEList[RawTypeRef], prefix: List[RawTypeName]) extends RawTypeRef
}
