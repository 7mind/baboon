package io.septimalmind.baboon.translator.scl
import io.septimalmind.baboon.translator.scl.ScValue.{ScPackageId, ScType}
import izumi.fundamentals.collections.nonempty.NEList

import java.util.concurrent.atomic.AtomicReference

object ScTypes {
  val baboonRuntimePkg: ScPackageId = ScPackageId(NEList("_root_", "baboon", "runtime", "shared"))

  val baboonTypeCodecs: ScType = ScType(baboonRuntimePkg, "BaboonTypeCodecs")

  val iBaboonGenerated: ScType          = ScType(baboonRuntimePkg, "BaboonGenerated")
  val iBaboonAdtMemberMeta: ScType      = ScType(baboonRuntimePkg, "BaboonAdtMemberMeta")
  val iBaboonGeneratedLatest: ScType    = ScType(baboonRuntimePkg, "BaboonGeneratedLatest")
  val abstractBaboonConversion: ScType  = ScType(baboonRuntimePkg, "BaboonAbstractConversion")
  val abstractBaboonConversions: ScType = ScType(baboonRuntimePkg, "BaboonAbstractConversions")
  val abstractBaboonCodecs: ScType      = ScType(baboonRuntimePkg, "BaboonAbstractCodecs")
  val baboonEnum: ScType                = ScType(baboonRuntimePkg, "BaboonEnum")
  val baboonMeta: ScType                = ScType(baboonRuntimePkg, "BaboonMeta")
  val baboonFixture: ScType             = ScType(baboonRuntimePkg, "BaboonFixture")
  val baboonAdtFixture: ScType          = ScType(baboonRuntimePkg, "BaboonAdtFixture")
  val baboonRandom: ScType              = ScType(baboonRuntimePkg, "BaboonRandom")
  val baboonJsonCodec: ScType           = ScType(baboonRuntimePkg, "BaboonJsonCodec")
  val baboonBinCodec: ScType            = ScType(baboonRuntimePkg, "BaboonBinCodec")
  val baboonBinCodecIndexed: ScType     = ScType(baboonRuntimePkg, "BaboonBinCodecIndexed")
  val baboonTimeFormats: ScType         = ScType(baboonRuntimePkg, "BaboonTimeFormats")
  val baboonTools: ScType               = ScType(baboonRuntimePkg, "BaboonTools")
  val baboonCodecContext: ScType        = ScType(baboonRuntimePkg, "BaboonCodecContext")
  val baboonLazy: ScType                = ScType(baboonRuntimePkg, "Lazy")
  val baboonBinTools: ScType            = ScType(baboonRuntimePkg, "BaboonBinTools")

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

  val scalaCollMutPkg: ScPackageId = ScPackageId(NEList("_root_", "scala", "collection", "mutable"))
  val scMutMap: ScType             = ScType(scalaCollMutPkg, "Map")

  val scalaUtilPkg: ScPackageId = ScPackageId(NEList("_root_", "scala", "util"))
  val scEither: ScType          = ScType(scalaUtilPkg, "Either")
  val scRandom: ScType          = ScType(scalaUtilPkg, "Random")

  val javaUtilPkg: ScPackageId = ScPackageId(NEList("_root_", "java", "util"))
  val scUid: ScType            = ScType(javaUtilPkg, "UUID")

  val javaLangPkg: ScPackageId = ScPackageId(NEList("_root_", "java", "lang"))
  val scString: ScType         = ScType(javaLangPkg, "String")
  val genericException: ScType = ScType(javaLangPkg, "RuntimeException")

  val javaTimePkg: ScPackageId = ScPackageId(NEList("_root_", "java", "time"))
  val scTime: ScType           = ScType(javaTimePkg, "OffsetDateTime")

  val javaIoPkg: ScPackageId = ScPackageId(NEList("_root_", "java", "io"))

  val binaryInput: ScType           = ScType(javaIoPkg, "DataInputStream")
  val binaryOutput: ScType          = ScType(javaIoPkg, "DataOutputStream")
  val byteArrayOutputStream: ScType = ScType(javaIoPkg, "ByteArrayOutputStream")

  val scalatestPkg: ScPackageId = parseScPkg("org.scalatest.flatspec")
  val anyFlatSpec: ScType       = ScType(scalatestPkg, "AnyFlatSpec")

  def parsePkg(pkg: String): NEList[String] = NEList.unsafeFrom(pkg.split('.').toList)
  def parseScPkg(pkg: String): ScPackageId  = ScPackageId(parsePkg(pkg))
}
