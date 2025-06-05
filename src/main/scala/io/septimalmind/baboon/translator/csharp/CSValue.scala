package io.septimalmind.baboon.translator.csharp

import io.septimalmind.baboon.translator.FQNSymbol
import izumi.fundamentals.collections.nonempty.NEList

sealed trait CSValue

object CSValue {
  case class CSPackageId(parts: NEList[String], isStatic: Boolean = false)

  object CSPackageId {
    def apply(pkg: String): CSPackageId =
      CSPackageId(NEList.unsafeFrom(pkg.split('.').toList))
  }

  case class CSType(pkg: CSValue.CSPackageId, name: String, fq: Boolean) extends CSValue {
    def fullyQualified: CSType = this.copy(fq = true)
    def asName: CSTypeName     = CSTypeName(name)
  }

  case class CSTypeName(name: String) extends CSValue

  implicit object FQNCSValue extends FQNSymbol[CSValue] {
    override def fullyQualified(value: CSValue): CSValue = value match {
      case t: CSType     => t.fullyQualified
      case n: CSTypeName => n
    }
  }
}


