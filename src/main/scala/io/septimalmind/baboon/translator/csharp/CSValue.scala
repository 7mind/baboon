package io.septimalmind.baboon.translator.csharp

import izumi.fundamentals.collections.nonempty.NEList

sealed trait CSValue

object CSValue {
  case class CSPackageId(parts: NEList[String])

  case class CSType(pkg: CSValue.CSPackageId, name: String, fq: Boolean)
      extends CSValue {
    def fullyQualified: CSType = this.copy(fq = true)
  }

}
