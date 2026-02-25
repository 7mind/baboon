package io.septimalmind.baboon.translator.scl

import io.septimalmind.baboon.translator.FQNSymbol
import izumi.fundamentals.collections.nonempty.NEList

sealed trait ScValue

object ScValue {
  case class ScPackageId(parts: NEList[String])

  object ScPackageId {
    def apply(pkg: String): ScPackageId =
      ScPackageId(NEList.unsafeFrom(pkg.split('.').toList))
  }

  case class ScType(
    pkg: ScValue.ScPackageId,
    name: String,
    inObject: Option[String] = None,
    fq: Boolean              = false,
    predef: Boolean          = false,
  ) extends ScValue {
    def fullyQualified: ScType    = this.copy(fq = true)
    override def toString: String = s"${pkg.parts.mkString(".")}#$name"
  }

  implicit object FQNScValue extends FQNSymbol[ScValue] {
    override def fullyQualified(value: ScValue): ScValue = value match {
      case t: ScType => t.fullyQualified
    }
  }
}
