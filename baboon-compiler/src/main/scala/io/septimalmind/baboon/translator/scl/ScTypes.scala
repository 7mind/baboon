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

  val baboonDecodeLong                  = ScType(baboonRuntimePkg, "BaboonJsonCodec.decodeLong")
  val baboonDecodeByte: ScType          = ScType(baboonRuntimePkg, "BaboonJsonCodec.decodeByte")
  val baboonDecodeShort: ScType         = ScType(baboonRuntimePkg, "BaboonJsonCodec.decodeShort")
  val baboonDecodeInt: ScType           = ScType(baboonRuntimePkg, "BaboonJsonCodec.decodeInt")
  val baboonDecodeTsu: ScType           = ScType(baboonRuntimePkg, "BaboonJsonCodec.decodeTsu")
  val baboonDecodeTso: ScType           = ScType(baboonRuntimePkg, "BaboonJsonCodec.decodeTso")
  val baboonDecodeByteString: ScType    = ScType(baboonRuntimePkg, "BaboonJsonCodec.decodeByteString")
  val baboonDecodeKeyBoolean: ScType    = ScType(baboonRuntimePkg, "BaboonJsonCodec.decodeKeyBoolean")
  val baboonDecodeKeyFloat: ScType      = ScType(baboonRuntimePkg, "BaboonJsonCodec.decodeKeyFloat")
  val baboonDecodeKeyBigDecimal: ScType = ScType(baboonRuntimePkg, "BaboonJsonCodec.decodeKeyBigDecimal")
  val baboonDecodeKeyTso: ScType        = ScType(baboonRuntimePkg, "BaboonJsonCodec.decodeKeyTso")
  val baboonDecodeKeyTsu: ScType        = ScType(baboonRuntimePkg, "BaboonJsonCodec.decodeKeyTsu")
  val baboonDecodeKeyByteString: ScType = ScType(baboonRuntimePkg, "BaboonJsonCodec.decodeKeyByteString")

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

  // scala

  val scalaPkg: ScPackageId = parseScPkg("_root_.scala")

  val deprecated: ScType = ScType(scalaPkg, "deprecated", predef = true)

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
  val scOption: ScType = ScType(scalaPkg, "Option", predef = true)

  // immutable collections
  val scalaCollImmuPkg: ScPackageId = parseScPkg("scala.collection.immutable")
  val scList: ScType                = ScType(scalaCollImmuPkg, "List", predef = true)
  val scSet: ScType                 = ScType(scalaCollImmuPkg, "Set", predef = true)
  val scMap: ScType                 = ScType(scalaCollImmuPkg, "Map", predef = true)

  // mutable collections
  val scalaCollMutPkg: ScPackageId = parseScPkg("scala.collection.mutable")
  val scMutMap: ScType             = ScType(scalaCollMutPkg, "Map", predef = true)

  // util
  val scalaUtilPkg: ScPackageId = parseScPkg("scala.util")
  val scEither: ScType          = ScType(scalaUtilPkg, "Either", predef = true)
  val scTry: ScType             = ScType(scalaUtilPkg, "Try")

  // scalatest
  val scalatestPkg: ScPackageId = parseScPkg("org.scalatest.flatspec")
  val anyFlatSpec: ScType       = ScType(scalatestPkg, "AnyFlatSpec")

  // java

  // util
  val javaUtilPkg: ScPackageId = parseScPkg("java.util")
  val scUid: ScType            = ScType(javaUtilPkg, "UUID")

  // lang
  val javaLangPkg: ScPackageId             = parseScPkg("java.lang")
  val scString: ScType                     = ScType(javaLangPkg, "String", predef = true)
  val genericException: ScType             = ScType(javaLangPkg, "RuntimeException")
  val javaClass: ScType                    = ScType(javaLangPkg, "Class", predef = true)
  val javaThrowable: ScType                = ScType(javaLangPkg, "Throwable", predef = true)
  val javaIllegalArgumentException: ScType = ScType(javaLangPkg, "IllegalArgumentException", predef = true)

  // time
  val javaTimePkg: ScPackageId = parseScPkg("java.time")
  val scTime: ScType           = ScType(javaTimePkg, "OffsetDateTime")

  // io
  val javaIoPkg: ScPackageId        = parseScPkg("java.io")
  val byteArrayOutputStream: ScType = ScType(javaIoPkg, "ByteArrayOutputStream")
  val javaFile: ScType              = ScType(javaIoPkg, "File")

  // nio
  val javaNioFilePkg = parseScPkg("java.nio.file")
  val javaNioFiles   = ScType(javaNioFilePkg, "Files")

  val javaNioCharsetPkg       = parseScPkg("java.nio.charset")
  val javaNioStandardCharsets = ScType(javaNioCharsetPkg, "StandardCharsets")

  // circe
  val scalaCirce           = parseScPkg("io.circe")
  val scalaCirceDecoder    = parseScPkg("io.circe.Decoder")
  val scalaCirceKeyDecoder = parseScPkg("io.circe.KeyDecoder")

  val circeJson       = ScType(scalaCirce, "Json")
  val circeKeyDecoder = ScType(scalaCirce, "KeyDecoder")

  val circeDecodeBoolean    = ScType(scalaCirceDecoder, "decodeBoolean")
  val circeDecodeFloat      = ScType(scalaCirceDecoder, "decodeFloat")
  val circeDecodeDouble     = ScType(scalaCirceDecoder, "decodeDouble")
  val circeDecodeUuid       = ScType(scalaCirceDecoder, "decodeUUID")
  val circeDecodeString     = ScType(scalaCirceDecoder, "decodeString")
  val circeDecodeBigDecimal = ScType(scalaCirceDecoder, "decodeBigDecimal")

  val circeDecodeOption = ScType(scalaCirceDecoder, "decodeOption")
  val circeDecodeList   = ScType(scalaCirceDecoder, "decodeList")
  val circeDecodeSet    = ScType(scalaCirceDecoder, "decodeSet")
  val circeDecodeMap    = ScType(scalaCirceDecoder, "decodeMap")

  val circeDecodeKeyByte   = ScType(scalaCirceKeyDecoder, "decodeKeyByte")
  val circeDecodeKeyShort  = ScType(scalaCirceKeyDecoder, "decodeKeyShort")
  val circeDecodeKeyInt    = ScType(scalaCirceKeyDecoder, "decodeKeyInt")
  val circeDecodeKeyLong   = ScType(scalaCirceKeyDecoder, "decodeKeyLong")
  val circeDecodeKeyDouble = ScType(scalaCirceKeyDecoder, "decodeKeyDouble")
  val circeDecodeKeyUUID   = ScType(scalaCirceKeyDecoder, "decodeKeyUUID")
  val circeDecodeKeyString = ScType(scalaCirceKeyDecoder, "decodeKeyString")

  def parsePkg(pkg: String): NEList[String] = NEList.unsafeFrom(pkg.split('.').toList)
  def parseScPkg(pkg: String): ScPackageId  = ScPackageId(parsePkg(pkg))
}
