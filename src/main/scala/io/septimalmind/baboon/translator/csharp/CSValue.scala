package io.septimalmind.baboon.translator.csharp

import io.septimalmind.baboon.translator.FQNSymbol
import io.septimalmind.baboon.typer.model.{Domain, Pkg, TypeId, Version}
import izumi.fundamentals.collections.nonempty.NEList

sealed trait CSValue

object CSValue {
  case class CSPackageId(parts: NEList[String], isStatic: Boolean = false)

  object CSPackageId {
    def apply(pkg: String): CSPackageId =
      CSPackageId(NEList.unsafeFrom(pkg.split('.').toList))
  }

  sealed trait CSTypeOrigin
  object CSTypeOrigin {
    def apply(typeId: TypeId, domain: Domain): TypeInDomain = {
      TypeInDomain(typeId, domain.id, domain.version)
    }
    case class TypeInDomain(typeId: TypeId, pkg: Pkg, version: Version, derived: Boolean = false) extends CSTypeOrigin

    case object Other extends CSTypeOrigin
  }

  case class CSType(pkg: CSValue.CSPackageId, name: String, fq: Boolean, origin: CSTypeOrigin) extends CSValue {
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
