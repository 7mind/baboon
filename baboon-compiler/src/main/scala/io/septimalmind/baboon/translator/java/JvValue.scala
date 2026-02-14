package io.septimalmind.baboon.translator.java

import io.septimalmind.baboon.translator.FQNSymbol
import izumi.fundamentals.collections.nonempty.NEList

sealed trait JvValue

object JvValue {
  case class JvPackageId(parts: NEList[String])

  object JvPackageId {
    def apply(pkg: String): JvPackageId =
      JvPackageId(NEList.unsafeFrom(pkg.split('.').toList))
  }

  case class JvType(pkg: JvPackageId, name: String, fq: Boolean = false, predef: Boolean = false) extends JvValue {
    def fullyQualified: JvType = this.copy(fq = true)
    def asName: JvTypeName     = JvTypeName(name)

    override def toString: String = s"${pkg.parts.mkString(".")}#$name"
  }

  case class JvTypeName(name: String) extends JvValue

  implicit object FQNJvValue extends FQNSymbol[JvValue] {
    override def fullyQualified(value: JvValue): JvValue = value match {
      case t: JvType     => t.fullyQualified
      case n: JvTypeName => n
    }
  }
}
