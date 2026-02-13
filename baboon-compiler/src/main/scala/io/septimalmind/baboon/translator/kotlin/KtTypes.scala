package io.septimalmind.baboon.translator.kotlin

import io.septimalmind.baboon.translator.kotlin.KtValue.{KtPackageId, KtType}
import izumi.fundamentals.collections.nonempty.NEList

object KtTypes {
  // baboon packages
  val baboonRuntimePkg: KtPackageId = parseKtPkg("baboon.runtime.shared")
  val baboonFixturePkg: KtPackageId = parseKtPkg("baboon.fixture")

  // baboon metadata
  val iBaboonGenerated: KtType       = KtType(baboonRuntimePkg, "BaboonGenerated")
  val iBaboonAdtMemberMeta: KtType   = KtType(baboonRuntimePkg, "BaboonAdtMemberMeta")
  val iBaboonGeneratedLatest: KtType = KtType(baboonRuntimePkg, "BaboonGeneratedLatest")
  val baboonEnum: KtType             = KtType(baboonRuntimePkg, "BaboonEnum")
  val baboonMeta: KtType             = KtType(baboonRuntimePkg, "BaboonMeta")

  // baboon codecs types
  val baboonJsonCodec: KtType              = KtType(baboonRuntimePkg, "BaboonJsonCodec")
  val baboonJsonCodecBase: KtType                    = KtType(baboonRuntimePkg, "BaboonJsonCodec.Base")
  val baboonJsonCodecBaseGenerated: KtType           = KtType(baboonRuntimePkg, "BaboonJsonCodec.BaseGenerated")
  val baboonJsonCodecBaseGeneratedAdt: KtType        = KtType(baboonRuntimePkg, "BaboonJsonCodec.BaseGeneratedAdt")
  val baboonJsonCodecNoEncoder: KtType               = KtType(baboonRuntimePkg, "BaboonJsonCodec.NoEncoder")
  val baboonJsonCodecNoEncoderGenerated: KtType      = KtType(baboonRuntimePkg, "BaboonJsonCodec.NoEncoderGenerated")
  val baboonJsonCodecNoEncoderGeneratedAdt: KtType   = KtType(baboonRuntimePkg, "BaboonJsonCodec.NoEncoderGeneratedAdt")

  val baboonBinCodec: KtType                    = KtType(baboonRuntimePkg, "BaboonBinCodec")
  val baboonBinCodecBase: KtType                = KtType(baboonRuntimePkg, "BaboonBinCodec.Base")
  val baboonBinCodecNoEncoder: KtType           = KtType(baboonRuntimePkg, "BaboonBinCodec.NoEncoder")
  val baboonBinCodecBaseGenerated: KtType       = KtType(baboonRuntimePkg, "BaboonBinCodec.BaseGenerated")
  val baboonBinCodecNoEncoderGenerated: KtType  = KtType(baboonRuntimePkg, "BaboonBinCodec.NoEncoderGenerated")
  val baboonBinCodecBaseGeneratedAdt: KtType    = KtType(baboonRuntimePkg, "BaboonBinCodec.BaseGeneratedAdt")
  val baboonBinCodecNoEncoderGeneratedAdt: KtType = KtType(baboonRuntimePkg, "BaboonBinCodec.NoEncoderGeneratedAdt")

  // baboon types
  val baboonBinCodecIndexed: KtType        = KtType(baboonRuntimePkg, "BaboonBinCodecIndexed")
  val baboonCodecContext: KtType           = KtType(baboonRuntimePkg, "BaboonCodecContext")
  def abstractBaboonCodec(id: String): KtType = KtType(baboonRuntimePkg, s"AbstractBaboon${id}Codecs")
  val binaryInput: KtType                  = KtType(baboonRuntimePkg, "LEDataInputStream")
  val binaryOutput: KtType                 = KtType(baboonRuntimePkg, "LEDataOutputStream")
  val baboonTimeFormats: KtType            = KtType(baboonRuntimePkg, "BaboonTimeFormats")
  val baboonLazy: KtType                   = KtType(baboonRuntimePkg, "Lazy")
  val baboonBinTools: KtType               = KtType(baboonRuntimePkg, "BaboonBinTools")
  val ktByteString: KtType                 = KtType(baboonRuntimePkg, "ByteString")
  val baboonRandom: KtType                 = KtType(baboonFixturePkg, "BaboonRandom")
  val baboonRandomFactory: KtType          = KtType(baboonFixturePkg, "BaboonRandomFactory")

  // baboon service wiring types
  val baboonMethodId: KtType        = KtType(baboonRuntimePkg, "BaboonMethodId")
  val baboonWiringError: KtType     = KtType(baboonRuntimePkg, "BaboonWiringError")
  val baboonWiringException: KtType = KtType(baboonRuntimePkg, "BaboonWiringException")

  // baboon conversions
  val baboonAbstractConversion: KtType  = KtType(baboonRuntimePkg, "AbstractConversion")
  val baboonAbstractConversions: KtType = KtType(baboonRuntimePkg, "AbstractBaboonConversions")

  // kotlin
  val kotlinPkg: KtPackageId = parseKtPkg("kotlin")

  val ktBoolean: KtType    = KtType(kotlinPkg, "Boolean", predef = true)
  val ktByte: KtType        = KtType(kotlinPkg, "Byte", predef = true)
  val ktShort: KtType       = KtType(kotlinPkg, "Short", predef = true)
  val ktInt: KtType         = KtType(kotlinPkg, "Int", predef = true)
  val ktLong: KtType        = KtType(kotlinPkg, "Long", predef = true)
  val ktUByte: KtType       = KtType(kotlinPkg, "UByte", predef = true)
  val ktUShort: KtType      = KtType(kotlinPkg, "UShort", predef = true)
  val ktUInt: KtType        = KtType(kotlinPkg, "UInt", predef = true)
  val ktULong: KtType       = KtType(kotlinPkg, "ULong", predef = true)
  val ktFloat: KtType       = KtType(kotlinPkg, "Float", predef = true)
  val ktDouble: KtType      = KtType(kotlinPkg, "Double", predef = true)
  val ktUnit: KtType        = KtType(kotlinPkg, "Unit", predef = true)
  val ktByteArray: KtType   = KtType(kotlinPkg, "ByteArray", predef = true)
  val ktAny: KtType         = KtType(kotlinPkg, "Any", predef = true)

  // kotlin collections
  val ktList: KtType   = KtType(kotlinPkg, "List", predef = true)
  val ktSet: KtType    = KtType(kotlinPkg, "Set", predef = true)
  val ktMap: KtType    = KtType(kotlinPkg, "Map", predef = true)

  val kotlinCollPkg: KtPackageId = parseKtPkg("kotlin.collections")
  val ktMutableMap: KtType       = KtType(kotlinCollPkg, "mutableMapOf", predef = true)

  // java
  val javaLangPkg: KtPackageId             = parseKtPkg("java.lang")
  val ktString: KtType                     = KtType(javaLangPkg, "String", predef = true)
  val genericException: KtType             = KtType(javaLangPkg, "RuntimeException", predef = true)
  val javaClass: KtType                    = KtType(javaLangPkg, "Class", predef = true)
  val javaIllegalArgumentException: KtType = KtType(javaLangPkg, "IllegalArgumentException", predef = true)

  val javaUtilPkg: KtPackageId = parseKtPkg("java.util")
  val ktUid: KtType            = KtType(javaUtilPkg, "UUID")

  val javaMathPkg: KtPackageId = parseKtPkg("java.math")
  val ktBigDecimal: KtType     = KtType(javaMathPkg, "BigDecimal")

  val javaTimePkg: KtPackageId = parseKtPkg("java.time")
  val ktTime: KtType           = KtType(javaTimePkg, "OffsetDateTime")

  val javaIoPkg: KtPackageId         = parseKtPkg("java.io")
  val byteArrayOutputStream: KtType  = KtType(javaIoPkg, "ByteArrayOutputStream")
  val byteArrayInputStream: KtType   = KtType(javaIoPkg, "ByteArrayInputStream")
  val javaFile: KtType               = KtType(javaIoPkg, "File")

  val javaNioFilePkg: KtPackageId = parseKtPkg("java.nio.file")
  val javaNioFiles: KtType        = KtType(javaNioFilePkg, "Files")

  val javaNioCharsetPkg: KtPackageId = parseKtPkg("java.nio.charset")
  val javaNioStandardCharsets: KtType = KtType(javaNioCharsetPkg, "StandardCharsets")

  // kotlinx.serialization.json
  val kotlinxJsonPkg: KtPackageId   = parseKtPkg("kotlinx.serialization.json")
  val jsonElement: KtType           = KtType(kotlinxJsonPkg, "JsonElement")
  val jsonObject: KtType            = KtType(kotlinxJsonPkg, "JsonObject")
  val jsonArray: KtType             = KtType(kotlinxJsonPkg, "JsonArray")
  val jsonPrimitive: KtType         = KtType(kotlinxJsonPkg, "JsonPrimitive")
  val jsonNull: KtType              = KtType(kotlinxJsonPkg, "JsonNull")
  val buildJsonObject: KtType       = KtType(kotlinxJsonPkg, "buildJsonObject")
  val buildJsonArray: KtType        = KtType(kotlinxJsonPkg, "buildJsonArray")
  val kotlinxJson: KtType           = KtType(kotlinxJsonPkg, "Json")

  // kotlin.test
  val kotlinTestPkg: KtPackageId   = parseKtPkg("kotlin.test")
  val ktTestAnnotation: KtType     = KtType(kotlinTestPkg, "Test")
  val ktAssertEquals: KtType       = KtType(kotlinTestPkg, "assertEquals")

  def parsePkg(pkg: String): NEList[String] = NEList.unsafeFrom(pkg.split('.').toList)
  def parseKtPkg(pkg: String): KtPackageId  = KtPackageId(parsePkg(pkg))
}
