package io.septimalmind.baboon.translator.scl
import io.septimalmind.baboon.translator.scl.ScValue.{ScPackageId, ScType}
import izumi.fundamentals.collections.nonempty.NEList

object ScTypes {
  val baboonRuntimePkg: ScPackageId = ScPackageId(NEList("_root_", "baboon", "runtime", "shared"))

  val baboonTypeCodecs: ScType = ScType(baboonRuntimePkg, "BaboonTypeCodecs")

  val iBaboonGenerated: ScType       = ScType(baboonRuntimePkg, "BaboonGenerated")
  val iBaboonAdtMemberMeta: ScType   = ScType(baboonRuntimePkg, "BaboonAdtMemberMeta")
  val iBaboonGeneratedLatest: ScType = ScType(baboonRuntimePkg, "BaboonGeneratedLatest")

  val scalaPkg: ScPackageId = ScPackageId(NEList("_root_", "scala"))

  val deprecated: ScType = ScType(scalaPkg, "deprecated")

  val scBoolean: ScType    = ScType(scalaPkg, "Boolean")
  val scByte: ScType       = ScType(scalaPkg, "Byte")
  val scShort: ScType      = ScType(scalaPkg, "Short")
  val scInt: ScType        = ScType(scalaPkg, "Int")
  val scLong: ScType       = ScType(scalaPkg, "Long")
  val scFloat: ScType      = ScType(scalaPkg, "Float")
  val scDouble: ScType     = ScType(scalaPkg, "Double")
  val scBigDecimal: ScType = ScType(scalaPkg, "BigDecimal")
  val scUnit: ScType       = ScType(scalaPkg, "Unit")

  // Scala collection types
  val scOption: ScType = ScType(scalaPkg, "Option")

  val scalaCollImmuPkg: ScPackageId = ScPackageId(NEList("_root_", "scala", "collection", "immutable"))
  val scList: ScType                = ScType(scalaCollImmuPkg, "List")
  val scSet: ScType                 = ScType(scalaCollImmuPkg, "Set")
  val scMap: ScType                 = ScType(scalaCollImmuPkg, "Map")

  val scalaUtilPkg: ScPackageId = ScPackageId(NEList("_root_", "scala", "util"))
  val scEither: ScType          = ScType(scalaUtilPkg, "Either")

  val javaUtilPkg: ScPackageId = ScPackageId(NEList("_root_", "java", "util"))
  val scUid: ScType            = ScType(javaUtilPkg, "UUID")

  val javaLangPkg: ScPackageId = ScPackageId(NEList("_root_", "java", "lang"))
  val scString: ScType         = ScType(javaLangPkg, "String")

  val javaTimePkg: ScPackageId = ScPackageId(NEList("_root_", "java", "time"))
  val scTime: ScType           = ScType(javaTimePkg, "OffsetDateTime")

}
