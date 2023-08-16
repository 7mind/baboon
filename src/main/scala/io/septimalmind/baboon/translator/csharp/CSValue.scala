package io.septimalmind.baboon.translator.csharp

import izumi.fundamentals.collections.nonempty.NonEmptyList

sealed trait CSValue

object CSValue {
  case class CSPackageId(parts: NonEmptyList[String])

  case class CSType(pkg: CSValue.CSPackageId, name: String, fq: Boolean = false)
      extends CSValue

}
