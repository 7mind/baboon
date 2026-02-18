package io.septimalmind.baboon.translator.rust

import io.septimalmind.baboon.translator.FQNSymbol
import izumi.fundamentals.collections.nonempty.NEList

sealed trait RsValue

object RsValue {
  case class RsCrateId(parts: NEList[String])

  object RsCrateId {
    def apply(pkg: String): RsCrateId =
      RsCrateId(NEList.unsafeFrom(pkg.split("::").toList))
  }

  case class RsType(
    crate: RsCrateId,
    name: String,
    fq: Boolean     = false,
    predef: Boolean = false,
  ) extends RsValue {
    def fullyQualified: RsType = this.copy(fq = true)
    def asName: RsTypeName     = RsTypeName(name)

    override def toString: String = s"${crate.parts.mkString("::")}::$name"
  }

  case class RsTypeName(name: String) extends RsValue

  implicit object FQNRsValue extends FQNSymbol[RsValue] {
    override def fullyQualified(value: RsValue): RsValue = value match {
      case t: RsType     => t.fullyQualified
      case n: RsTypeName => n
    }
  }
}
