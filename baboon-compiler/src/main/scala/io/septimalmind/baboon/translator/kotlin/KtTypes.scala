package io.septimalmind.baboon.translator.kotlin

import io.septimalmind.baboon.translator.kotlin.KtValue.{KtPackageId, KtType}
import izumi.fundamentals.collections.nonempty.NEList

class KtTypes(val multiplatform: Boolean) {
  import KtTypes.*

  // Platform-dependent type mappings
  val ktUid: KtType = if (multiplatform) {
    KtType(parseKtPkg("kotlin.uuid"), "Uuid")
  } else {
    KtType(javaUtilPkg, "UUID")
  }

  val ktBigDecimal: KtType = if (multiplatform) {
    KtType(baboonRuntimePkg, "BaboonDecimal")
  } else {
    KtType(javaMathPkg, "BigDecimal")
  }

  // tsu maps to Instant (both JVM and KMP can parse, but different packages)
  val ktTimeTsu: KtType = if (multiplatform) {
    KtType(parseKtPkg("kotlinx.datetime"), "Instant")
  } else {
    KtType(javaTimePkg, "OffsetDateTime")
  }

  // tso maps to OffsetDateTime (JVM) or BaboonOffsetDateTime (KMP)
  val ktTimeTso: KtType = if (multiplatform) {
    KtType(baboonRuntimePkg, "BaboonOffsetDateTime")
  } else {
    KtType(javaTimePkg, "OffsetDateTime")
  }

  // Binary I/O types
  val binaryInput: KtType = if (multiplatform) {
    KtType(baboonRuntimePkg, "BaboonBinaryReader")
  } else {
    KtType(baboonRuntimePkg, "LEDataInputStream")
  }

  val binaryOutput: KtType = if (multiplatform) {
    KtType(baboonRuntimePkg, "BaboonBinaryWriter")
  } else {
    KtType(baboonRuntimePkg, "LEDataOutputStream")
  }

  // ByteArray stream types (JVM only, KMP uses BaboonBinaryWriter/Reader directly)
  val byteArrayOutputStream: KtType = if (multiplatform) {
    KtType(baboonRuntimePkg, "BaboonBinaryWriter") // KMP: use writer directly
  } else {
    KtType(javaIoPkg, "ByteArrayOutputStream")
  }

  val byteArrayInputStream: KtType = if (multiplatform) {
    KtType(baboonRuntimePkg, "BaboonBinaryReader") // KMP: use reader directly
  } else {
    KtType(javaIoPkg, "ByteArrayInputStream")
  }

  // ADT metadata: Class<*> on JVM, KClass<*> on KMP
  val javaClass: KtType = if (multiplatform) {
    KtType(parseKtPkg("kotlin.reflect"), "KClass")
  } else {
    KtType(javaLangPkg, "Class", predef = true)
  }

  // Expression for getting class reference
  val classRefSuffix: String = if (multiplatform) "::class" else "::class.java"
}

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
  val baboonJsonCodec: KtType                      = KtType(baboonRuntimePkg, "BaboonJsonCodec")
  val baboonJsonCodecBase: KtType                  = KtType(baboonRuntimePkg, "BaboonJsonCodec.Base")
  val baboonJsonCodecBaseGenerated: KtType         = KtType(baboonRuntimePkg, "BaboonJsonCodec.BaseGenerated")
  val baboonJsonCodecBaseGeneratedAdt: KtType      = KtType(baboonRuntimePkg, "BaboonJsonCodec.BaseGeneratedAdt")
  val baboonJsonCodecNoEncoder: KtType             = KtType(baboonRuntimePkg, "BaboonJsonCodec.NoEncoder")
  val baboonJsonCodecNoEncoderGenerated: KtType    = KtType(baboonRuntimePkg, "BaboonJsonCodec.NoEncoderGenerated")
  val baboonJsonCodecNoEncoderGeneratedAdt: KtType = KtType(baboonRuntimePkg, "BaboonJsonCodec.NoEncoderGeneratedAdt")

  val baboonBinCodec: KtType                      = KtType(baboonRuntimePkg, "BaboonBinCodec")
  val baboonBinCodecBase: KtType                  = KtType(baboonRuntimePkg, "BaboonBinCodec.Base")
  val baboonBinCodecNoEncoder: KtType             = KtType(baboonRuntimePkg, "BaboonBinCodec.NoEncoder")
  val baboonBinCodecBaseGenerated: KtType         = KtType(baboonRuntimePkg, "BaboonBinCodec.BaseGenerated")
  val baboonBinCodecNoEncoderGenerated: KtType    = KtType(baboonRuntimePkg, "BaboonBinCodec.NoEncoderGenerated")
  val baboonBinCodecBaseGeneratedAdt: KtType      = KtType(baboonRuntimePkg, "BaboonBinCodec.BaseGeneratedAdt")
  val baboonBinCodecNoEncoderGeneratedAdt: KtType = KtType(baboonRuntimePkg, "BaboonBinCodec.NoEncoderGeneratedAdt")

  // baboon types
  val baboonBinCodecIndexed: KtType           = KtType(baboonRuntimePkg, "BaboonBinCodecIndexed")
  val baboonCodecContext: KtType              = KtType(baboonRuntimePkg, "BaboonCodecContext")
  def abstractBaboonCodec(id: String): KtType = KtType(baboonRuntimePkg, s"AbstractBaboon${id}Codecs")
  val baboonTimeFormats: KtType               = KtType(baboonRuntimePkg, "BaboonTimeFormats")
  val baboonLazy: KtType                      = KtType(baboonRuntimePkg, "Lazy")
  val baboonBinTools: KtType                  = KtType(baboonRuntimePkg, "BaboonBinTools")
  val ktByteString: KtType                    = KtType(baboonRuntimePkg, "ByteString")
  val baboonRandom: KtType                    = KtType(baboonFixturePkg, "BaboonRandom")
  val baboonRandomFactory: KtType             = KtType(baboonFixturePkg, "BaboonRandomFactory")

  // baboon service wiring types
  val baboonMethodId: KtType        = KtType(baboonRuntimePkg, "BaboonMethodId")
  val baboonWiringError: KtType     = KtType(baboonRuntimePkg, "BaboonWiringError")
  val baboonWiringException: KtType = KtType(baboonRuntimePkg, "BaboonWiringException")

  // baboon conversions
  val baboonAbstractConversion: KtType  = KtType(baboonRuntimePkg, "AbstractConversion")
  val baboonAbstractConversions: KtType = KtType(baboonRuntimePkg, "AbstractBaboonConversions")

  // kotlin
  val kotlinPkg: KtPackageId = parseKtPkg("kotlin")

  val ktBoolean: KtType   = KtType(kotlinPkg, "Boolean", predef = true)
  val ktByte: KtType      = KtType(kotlinPkg, "Byte", predef = true)
  val ktShort: KtType     = KtType(kotlinPkg, "Short", predef = true)
  val ktInt: KtType       = KtType(kotlinPkg, "Int", predef = true)
  val ktLong: KtType      = KtType(kotlinPkg, "Long", predef = true)
  val ktUByte: KtType     = KtType(kotlinPkg, "UByte", predef = true)
  val ktUShort: KtType    = KtType(kotlinPkg, "UShort", predef = true)
  val ktUInt: KtType      = KtType(kotlinPkg, "UInt", predef = true)
  val ktULong: KtType     = KtType(kotlinPkg, "ULong", predef = true)
  val ktFloat: KtType     = KtType(kotlinPkg, "Float", predef = true)
  val ktDouble: KtType    = KtType(kotlinPkg, "Double", predef = true)
  val ktUnit: KtType      = KtType(kotlinPkg, "Unit", predef = true)
  val ktByteArray: KtType = KtType(kotlinPkg, "ByteArray", predef = true)
  val ktAny: KtType       = KtType(kotlinPkg, "Any", predef = true)
  val ktString: KtType    = KtType(kotlinPkg, "String", predef = true)

  // kotlin collections
  val ktList: KtType = KtType(kotlinPkg, "List", predef = true)
  val ktSet: KtType  = KtType(kotlinPkg, "Set", predef = true)
  val ktMap: KtType  = KtType(kotlinPkg, "Map", predef = true)

  val kotlinCollPkg: KtPackageId = parseKtPkg("kotlin.collections")
  val ktMutableMap: KtType       = KtType(kotlinCollPkg, "mutableMapOf", predef = true)

  // java (used by JVM mode; KMP mode replaces these via instance members)
  val javaLangPkg: KtPackageId             = parseKtPkg("java.lang")
  val genericException: KtType             = KtType(javaLangPkg, "RuntimeException", predef = true)
  val javaIllegalArgumentException: KtType = KtType(javaLangPkg, "IllegalArgumentException", predef = true)

  val javaUtilPkg: KtPackageId = parseKtPkg("java.util")
  val javaMathPkg: KtPackageId = parseKtPkg("java.math")
  val javaTimePkg: KtPackageId = parseKtPkg("java.time")

  val javaIoPkg: KtPackageId = parseKtPkg("java.io")
  val javaFile: KtType       = KtType(javaIoPkg, "File")

  val javaNioFilePkg: KtPackageId = parseKtPkg("java.nio.file")
  val javaNioFiles: KtType        = KtType(javaNioFilePkg, "Files")

  val javaNioCharsetPkg: KtPackageId  = parseKtPkg("java.nio.charset")
  val javaNioStandardCharsets: KtType = KtType(javaNioCharsetPkg, "StandardCharsets")

  // kotlinx.serialization.json
  val kotlinxJsonPkg: KtPackageId = parseKtPkg("kotlinx.serialization.json")
  val jsonElement: KtType         = KtType(kotlinxJsonPkg, "JsonElement")
  val jsonObject: KtType          = KtType(kotlinxJsonPkg, "JsonObject")
  val jsonArray: KtType           = KtType(kotlinxJsonPkg, "JsonArray")
  val jsonPrimitive: KtType       = KtType(kotlinxJsonPkg, "JsonPrimitive")
  val jsonNull: KtType            = KtType(kotlinxJsonPkg, "JsonNull")
  val buildJsonObject: KtType     = KtType(kotlinxJsonPkg, "buildJsonObject")
  val buildJsonArray: KtType      = KtType(kotlinxJsonPkg, "buildJsonArray")
  val kotlinxJson: KtType         = KtType(kotlinxJsonPkg, "Json")

  // kotlin.test
  val kotlinTestPkg: KtPackageId = parseKtPkg("kotlin.test")
  val ktTestAnnotation: KtType   = KtType(kotlinTestPkg, "Test")
  val ktAssertEquals: KtType     = KtType(kotlinTestPkg, "assertEquals")

  def parsePkg(pkg: String): NEList[String] = NEList.unsafeFrom(pkg.split('.').toList)
  def parseKtPkg(pkg: String): KtPackageId  = KtPackageId(parsePkg(pkg))
}
