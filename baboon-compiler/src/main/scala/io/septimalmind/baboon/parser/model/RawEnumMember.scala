package io.septimalmind.baboon.parser.model

import izumi.fundamentals.platform.cache.CachedProductHashcode

case class RawEnumMember(value: String, associated: Option[RawEnumConst], meta: RawNodeMeta) extends CachedProductHashcode {
  override def toString: String = value
}

sealed trait RawEnumConst
object RawEnumConst {
  case class RawInt(int: Long) extends RawEnumConst
}
