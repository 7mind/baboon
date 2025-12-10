package io.septimalmind.baboon.translator.scl
import io.septimalmind.baboon.translator.scl.ScValue.{ScPackageId, ScType}
import izumi.fundamentals.collections.nonempty.NEList

object ScTypes {
  // baboon packages
  val baboonRuntimePkg: ScPackageId = parseScPkg("_root_.baboon.runtime.shared")
  val baboonFixturePkg: ScPackageId = parseScPkg("_root_.baboon.fixture")

  // baboon metadata
  val iBaboonGenerated: ScType       = ScType(baboonRuntimePkg, "BaboonGenerated")
  val iBaboonAdtMemberMeta: ScType   = ScType(baboonRuntimePkg, "BaboonAdtMemberMeta")
  val iBaboonGeneratedLatest: ScType = ScType(baboonRuntimePkg, "BaboonGeneratedLatest")
  val baboonEnum: ScType             = ScType(baboonRuntimePkg, "BaboonEnum")
  val baboonMeta: ScType             = ScType(baboonRuntimePkg, "BaboonMeta")

  // baboon codecs types
  val baboonJsonCodec: ScType                      = ScType(baboonRuntimePkg, "BaboonJsonCodec")
  val baboonJsonCodecBase: ScType                  = ScType(baboonRuntimePkg, "BaboonJsonCodec.Base")
  val baboonJsonCodecBaseGenerated: ScType         = ScType(baboonRuntimePkg, "BaboonJsonCodec.BaseGenerated")
  val baboonJsonCodecBaseGeneratedAdt: ScType      = ScType(baboonRuntimePkg, "BaboonJsonCodec.BaseGeneratedAdt")
  val baboonJsonCodecNoEncoder: ScType             = ScType(baboonRuntimePkg, "BaboonJsonCodec.NoEncoder")
  val baboonJsonCodecNoEncoderGenerated: ScType    = ScType(baboonRuntimePkg, "BaboonJsonCodec.NoEncoderGenerated")
  val baboonJsonCodecNoEncoderGeneratedAdt: ScType = ScType(baboonRuntimePkg, "BaboonJsonCodec.NoEncoderGeneratedAdt")

  val baboonBinCodec: ScType                      = ScType(baboonRuntimePkg, "BaboonBinCodec")
  val baboonBinCodecBase: ScType                  = ScType(baboonRuntimePkg, "BaboonBinCodec.Base")
  val baboonBinCodecBaseGenerated: ScType         = ScType(baboonRuntimePkg, "BaboonBinCodec.BaseGenerated")
  val baboonBinCodecBaseGeneratedAdt: ScType      = ScType(baboonRuntimePkg, "BaboonBinCodec.BaseGeneratedAdt")
  val baboonBinCodecNoEncoder: ScType             = ScType(baboonRuntimePkg, "BaboonBinCodec.NoEncoder")
  val baboonBinCodecNoEncoderGenerated: ScType    = ScType(baboonRuntimePkg, "BaboonBinCodec.NoEncoderGenerated")
  val baboonBinCodecNoEncoderGeneratedAdt: ScType = ScType(baboonRuntimePkg, "BaboonBinCodec.NoEncoderGeneratedAdt")

  // baboon types
  val baboonBinCodecIndexed: ScType           = ScType(baboonRuntimePkg, "BaboonBinCodecIndexed")
  val baboonCodecContext: ScType              = ScType(baboonRuntimePkg, "BaboonCodecContext")
  def abstractBaboonCodec(id: String): ScType = ScType(baboonRuntimePkg, s"AbstractBaboon${id}Codecs")
  val binaryInput: ScType                     = ScType(baboonRuntimePkg, "LEDataInputStream")
  val binaryOutput: ScType                    = ScType(baboonRuntimePkg, "LEDataOutputStream")
  val baboonTimeFormats: ScType               = ScType(baboonRuntimePkg, "BaboonTimeFormats")
  val baboonLazy: ScType                      = ScType(baboonRuntimePkg, "Lazy")
  val baboonBinTools: ScType                  = ScType(baboonRuntimePkg, "BaboonBinTools")
  val scByteString: ScType                    = ScType(baboonRuntimePkg, "ByteString")
  val baboonRandom: ScType                    = ScType(baboonFixturePkg, "BaboonRandom")

  // baboon conversions
  val baboonAbstractConversion  = ScType(baboonRuntimePkg, "AbstractConversion")
  val baboonAbstractConversions = ScType(baboonRuntimePkg, "AbstractBaboonConversions")

  val scalaPkg: ScPackageId = parseScPkg("_root_.scala")

  val deprecated: ScType = ScType(scalaPkg, "deprecated")

  val scBoolean: ScType    = ScType(scalaPkg, "Boolean", predef = true)
  val scByte: ScType       = ScType(scalaPkg, "Byte", predef = true)
  val scShort: ScType      = ScType(scalaPkg, "Short", predef = true)
  val scInt: ScType        = ScType(scalaPkg, "Int", predef = true)
  val scLong: ScType       = ScType(scalaPkg, "Long", predef = true)
  val scFloat: ScType      = ScType(scalaPkg, "Float", predef = true)
  val scDouble: ScType     = ScType(scalaPkg, "Double", predef = true)
  val scBigDecimal: ScType = ScType(scalaPkg, "BigDecimal", predef = true)
  val scUnit: ScType       = ScType(scalaPkg, "Unit", predef = true)
  val scArray: ScType      = ScType(scalaPkg, "Array", predef = true)

  // Scala collection types
  val scOption: ScType = ScType(scalaPkg, "Option")

  val scalaCollImmuPkg: ScPackageId = parseScPkg("scala.collection.immutable")
  val scList: ScType                = ScType(scalaCollImmuPkg, "List", predef = true)
  val scSet: ScType                 = ScType(scalaCollImmuPkg, "Set", predef = true)
  val scMap: ScType                 = ScType(scalaCollImmuPkg, "Map", predef = true)

  val scalaCollMutPkg: ScPackageId = parseScPkg("scala.collection.mutable")
  val scMutMap: ScType             = ScType(scalaCollMutPkg, "Map", predef = true)

  val scalaUtilPkg: ScPackageId = parseScPkg("scala.util")
  val scEither: ScType          = ScType(scalaUtilPkg, "Either", predef = true)

  val javaUtilPkg: ScPackageId = parseScPkg("java.util")
  val scUid: ScType            = ScType(javaUtilPkg, "UUID")

  val javaLangPkg: ScPackageId = parseScPkg("java.lang")
  val scString: ScType         = ScType(javaLangPkg, "String", predef = true)
  val genericException: ScType = ScType(javaLangPkg, "RuntimeException")
  val javaClass: ScType        = ScType(javaLangPkg, "Class", predef = true)

  val javaTimePkg: ScPackageId = parseScPkg("java.time")
  val scTime: ScType           = ScType(javaTimePkg, "OffsetDateTime")

  val javaIoPkg: ScPackageId = parseScPkg("java.io")

  val byteArrayOutputStream: ScType = ScType(javaIoPkg, "ByteArrayOutputStream")
  val javaFile: ScType              = ScType(javaIoPkg, "File")

  val scalatestPkg: ScPackageId = parseScPkg("org.scalatest.flatspec")
  val anyFlatSpec: ScType       = ScType(scalatestPkg, "AnyFlatSpec")

  val javaNioFilePkg = parseScPkg("java.nio.file")
  val javaNioFiles   = ScType(javaNioFilePkg, "Files")

  val javaNioCharsetPkg       = parseScPkg("java.nio.charset")
  val javaNioStandardCharsets = ScType(javaNioCharsetPkg, "StandardCharsets")

  def parsePkg(pkg: String): NEList[String] = NEList.unsafeFrom(pkg.split('.').toList)
  def parseScPkg(pkg: String): ScPackageId  = ScPackageId(parsePkg(pkg))
}
