package io.septimalmind.baboon.translator.scl

import izumi.fundamentals.collections.nonempty.NEList

sealed trait ScValue

object ScValue {
  case class ScPackageId(parts: NEList[String])

  object ScPackageId {
    def apply(pkg: String): ScPackageId =
      ScPackageId(NEList.unsafeFrom(pkg.split('.').toList))
  }

  case class ScType(pkg: ScValue.ScPackageId, name: String, fq: Boolean) extends ScValue {
    def fullyQualified: ScType = this.copy(fq = true)
    def asName: ScTypeName     = ScTypeName(name)
  }

  case class ScTypeName(name: String) extends ScValue

}
