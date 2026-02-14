package io.septimalmind.baboon.translator.kotlin

import io.septimalmind.baboon.translator.FQNSymbol
import izumi.fundamentals.collections.nonempty.NEList

sealed trait KtValue

object KtValue {
  case class KtPackageId(parts: NEList[String])

  object KtPackageId {
    def apply(pkg: String): KtPackageId =
      KtPackageId(NEList.unsafeFrom(pkg.split('.').toList))
  }

  case class KtType(pkg: KtPackageId, name: String, fq: Boolean = false, predef: Boolean = false) extends KtValue {
    def fullyQualified: KtType = this.copy(fq = true)
    def asName: KtTypeName     = KtTypeName(name)

    override def toString: String = s"${pkg.parts.mkString(".")}#$name"
  }

  case class KtTypeName(name: String) extends KtValue

  implicit object FQNKtValue extends FQNSymbol[KtValue] {
    override def fullyQualified(value: KtValue): KtValue = value match {
      case t: KtType     => t.fullyQualified
      case n: KtTypeName => n
    }
  }
}
