package io.septimalmind.baboon.translator.dart

import io.septimalmind.baboon.translator.FQNSymbol
import izumi.fundamentals.collections.nonempty.NEList

sealed trait DtValue

object DtValue {
  case class DtPackageId(parts: NEList[String])

  object DtPackageId {
    def apply(pkg: String): DtPackageId =
      DtPackageId(NEList.unsafeFrom(pkg.split('.').toList))
  }

  case class DtType(pkg: DtPackageId, name: String, fq: Boolean = false, predef: Boolean = false, importAs: Option[String] = None) extends DtValue {
    def fullyQualified: DtType = this.copy(fq = true)
    def asName: DtTypeName     = DtTypeName(name)

    override def toString: String = s"${pkg.parts.mkString(".")}#$name"
  }

  case class DtTypeName(name: String) extends DtValue

  implicit object FQNDtValue extends FQNSymbol[DtValue] {
    override def fullyQualified(value: DtValue): DtValue = value match {
      case t: DtType     => t.fullyQualified
      case n: DtTypeName => n
    }
  }
}
