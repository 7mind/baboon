package io.septimalmind.baboon.translator.java

import io.septimalmind.baboon.translator.java.JvValue.{JvPackageId, JvType}
import izumi.fundamentals.collections.nonempty.NEList

object JvTypes {
  // baboon packages
  val baboonRuntimePkg: JvPackageId = parseJvPkg("baboon.runtime.shared")
  val baboonFixturePkg: JvPackageId = parseJvPkg("baboon.fixture")

  // baboon metadata
  val iBaboonGenerated: JvType       = JvType(baboonRuntimePkg, "BaboonGenerated")
  val iBaboonAdtMemberMeta: JvType   = JvType(baboonRuntimePkg, "BaboonAdtMemberMeta")
  val iBaboonGeneratedLatest: JvType = JvType(baboonRuntimePkg, "BaboonGeneratedLatest")
  val baboonEnum: JvType             = JvType(baboonRuntimePkg, "BaboonEnum")
  val baboonMeta: JvType             = JvType(baboonRuntimePkg, "BaboonMeta")

  // baboon codecs types
  val baboonJsonCodec: JvType                      = JvType(baboonRuntimePkg, "BaboonJsonCodec")
  val baboonJsonCodecBase: JvType                  = JvType(baboonRuntimePkg, "BaboonJsonCodec.Base")
  val baboonJsonCodecBaseGenerated: JvType         = JvType(baboonRuntimePkg, "BaboonJsonCodec.BaseGenerated")
  val baboonJsonCodecBaseGeneratedAdt: JvType      = JvType(baboonRuntimePkg, "BaboonJsonCodec.BaseGeneratedAdt")
  val baboonJsonCodecNoEncoder: JvType             = JvType(baboonRuntimePkg, "BaboonJsonCodec.NoEncoder")
  val baboonJsonCodecNoEncoderGenerated: JvType    = JvType(baboonRuntimePkg, "BaboonJsonCodec.NoEncoderGenerated")
  val baboonJsonCodecNoEncoderGeneratedAdt: JvType = JvType(baboonRuntimePkg, "BaboonJsonCodec.NoEncoderGeneratedAdt")

  val baboonBinCodec: JvType                      = JvType(baboonRuntimePkg, "BaboonBinCodec")
  val baboonBinCodecBase: JvType                  = JvType(baboonRuntimePkg, "BaboonBinCodec.Base")
  val baboonBinCodecNoEncoder: JvType             = JvType(baboonRuntimePkg, "BaboonBinCodec.NoEncoder")
  val baboonBinCodecBaseGenerated: JvType         = JvType(baboonRuntimePkg, "BaboonBinCodec.BaseGenerated")
  val baboonBinCodecNoEncoderGenerated: JvType    = JvType(baboonRuntimePkg, "BaboonBinCodec.NoEncoderGenerated")
  val baboonBinCodecBaseGeneratedAdt: JvType      = JvType(baboonRuntimePkg, "BaboonBinCodec.BaseGeneratedAdt")
  val baboonBinCodecNoEncoderGeneratedAdt: JvType = JvType(baboonRuntimePkg, "BaboonBinCodec.NoEncoderGeneratedAdt")

  // baboon types
  val baboonBinCodecIndexed: JvType      = JvType(baboonRuntimePkg, "BaboonBinCodecIndexed")
  val baboonCodecContext: JvType         = JvType(baboonRuntimePkg, "BaboonCodecContext")
  def abstractBaboonCodec(id: String): JvType = JvType(baboonRuntimePkg, s"AbstractBaboon${id}Codecs")
  val binaryInput: JvType               = JvType(baboonRuntimePkg, "LEDataInputStream")
  val binaryOutput: JvType              = JvType(baboonRuntimePkg, "LEDataOutputStream")
  val baboonTimeFormats: JvType          = JvType(baboonRuntimePkg, "BaboonTimeFormats")
  val baboonLazy: JvType                 = JvType(baboonRuntimePkg, "Lazy")
  val baboonBinTools: JvType             = JvType(baboonRuntimePkg, "BaboonBinTools")
  val jvByteString: JvType               = JvType(baboonRuntimePkg, "ByteString")
  val baboonRandom: JvType               = JvType(baboonFixturePkg, "BaboonRandom")
  val baboonRandomFactory: JvType        = JvType(baboonFixturePkg, "BaboonRandomFactory")

  // baboon service wiring types
  val baboonMethodId: JvType        = JvType(baboonRuntimePkg, "BaboonMethodId")
  val baboonWiringError: JvType     = JvType(baboonRuntimePkg, "BaboonWiringError")
  val baboonWiringException: JvType = JvType(baboonRuntimePkg, "BaboonWiringException")

  // baboon conversions
  val baboonAbstractConversion: JvType  = JvType(baboonRuntimePkg, "AbstractConversion")
  val baboonAbstractConversions: JvType = JvType(baboonRuntimePkg, "AbstractBaboonConversions")

  // java.lang
  val javaLangPkg: JvPackageId             = parseJvPkg("java.lang")
  val jvBoolean: JvType                    = JvType(javaLangPkg, "boolean", predef = true)
  val jvByte: JvType                       = JvType(javaLangPkg, "byte", predef = true)
  val jvShort: JvType                      = JvType(javaLangPkg, "short", predef = true)
  val jvInt: JvType                        = JvType(javaLangPkg, "int", predef = true)
  val jvLong: JvType                       = JvType(javaLangPkg, "long", predef = true)
  val jvFloat: JvType                      = JvType(javaLangPkg, "float", predef = true)
  val jvDouble: JvType                     = JvType(javaLangPkg, "double", predef = true)
  val jvString: JvType                     = JvType(javaLangPkg, "String", predef = true)
  val jvObject: JvType                     = JvType(javaLangPkg, "Object", predef = true)
  val jvVoid: JvType                       = JvType(javaLangPkg, "void", predef = true)
  val genericException: JvType             = JvType(javaLangPkg, "RuntimeException", predef = true)
  val javaClass: JvType                    = JvType(javaLangPkg, "Class", predef = true)
  val javaIllegalArgumentException: JvType = JvType(javaLangPkg, "IllegalArgumentException", predef = true)

  // Boxed primitives (for generics)
  val jvBoxedBoolean: JvType = JvType(javaLangPkg, "Boolean", predef = true)
  val jvBoxedByte: JvType    = JvType(javaLangPkg, "Byte", predef = true)
  val jvBoxedShort: JvType   = JvType(javaLangPkg, "Short", predef = true)
  val jvBoxedInteger: JvType = JvType(javaLangPkg, "Integer", predef = true)
  val jvBoxedLong: JvType    = JvType(javaLangPkg, "Long", predef = true)
  val jvBoxedFloat: JvType   = JvType(javaLangPkg, "Float", predef = true)
  val jvBoxedDouble: JvType  = JvType(javaLangPkg, "Double", predef = true)

  // java.util
  val javaUtilPkg: JvPackageId = parseJvPkg("java.util")
  val jvUid: JvType            = JvType(javaUtilPkg, "UUID")
  val jvList: JvType           = JvType(javaUtilPkg, "List")
  val jvSet: JvType            = JvType(javaUtilPkg, "Set")
  val jvMap: JvType            = JvType(javaUtilPkg, "Map")
  val jvOptional: JvType       = JvType(javaUtilPkg, "Optional")
  val jvArrayList: JvType      = JvType(javaUtilPkg, "ArrayList")
  val jvLinkedHashSet: JvType  = JvType(javaUtilPkg, "LinkedHashSet")
  val jvLinkedHashMap: JvType  = JvType(javaUtilPkg, "LinkedHashMap")
  val jvArrays: JvType         = JvType(javaUtilPkg, "Arrays")

  // java.math
  val javaMathPkg: JvPackageId = parseJvPkg("java.math")
  val jvBigDecimal: JvType     = JvType(javaMathPkg, "BigDecimal")

  // java.time
  val javaTimePkg: JvPackageId   = parseJvPkg("java.time")
  val jvOffsetDateTime: JvType   = JvType(javaTimePkg, "OffsetDateTime")
  val jvInstant: JvType          = JvType(javaTimePkg, "Instant")
  val jvZoneOffset: JvType       = JvType(javaTimePkg, "ZoneOffset")

  // java.io
  val javaIoPkg: JvPackageId         = parseJvPkg("java.io")
  val byteArrayOutputStream: JvType  = JvType(javaIoPkg, "ByteArrayOutputStream")
  val byteArrayInputStream: JvType   = JvType(javaIoPkg, "ByteArrayInputStream")
  val javaFile: JvType               = JvType(javaIoPkg, "File")

  // java.nio
  val javaNioFilePkg: JvPackageId    = parseJvPkg("java.nio.file")
  val javaNioFiles: JvType           = JvType(javaNioFilePkg, "Files")
  val javaNioPath: JvType            = JvType(javaNioFilePkg, "Path")

  val javaNioCharsetPkg: JvPackageId    = parseJvPkg("java.nio.charset")
  val javaNioStandardCharsets: JvType   = JvType(javaNioCharsetPkg, "StandardCharsets")

  // Jackson types
  val jacksonPkg: JvPackageId          = parseJvPkg("com.fasterxml.jackson.databind")
  val jacksonNodePkg: JvPackageId      = parseJvPkg("com.fasterxml.jackson.databind.node")
  val jsonNode: JvType                 = JvType(jacksonPkg, "JsonNode")
  val objectMapper: JvType             = JvType(jacksonPkg, "ObjectMapper")
  val objectNode: JvType               = JvType(jacksonNodePkg, "ObjectNode")
  val arrayNode: JvType                = JvType(jacksonNodePkg, "ArrayNode")
  val textNode: JvType                 = JvType(jacksonNodePkg, "TextNode")
  val intNode: JvType                  = JvType(jacksonNodePkg, "IntNode")
  val longNode: JvType                 = JvType(jacksonNodePkg, "LongNode")
  val shortNode: JvType                = JvType(jacksonNodePkg, "ShortNode")
  val floatNode: JvType                = JvType(jacksonNodePkg, "FloatNode")
  val doubleNode: JvType               = JvType(jacksonNodePkg, "DoubleNode")
  val booleanNode: JvType              = JvType(jacksonNodePkg, "BooleanNode")
  val nullNode: JvType                 = JvType(jacksonNodePkg, "NullNode")
  val jsonNodeFactory: JvType          = JvType(jacksonNodePkg, "JsonNodeFactory")

  // JUnit
  val junitPkg: JvPackageId    = parseJvPkg("org.junit.jupiter.api")
  val jvTestAnnotation: JvType = JvType(junitPkg, "Test")
  val jvAssertions: JvType     = JvType(junitPkg, "Assertions")

  // java.util.function
  val javaFunctionPkg: JvPackageId = parseJvPkg("java.util.function")
  val jvSupplier: JvType           = JvType(javaFunctionPkg, "Supplier")

  def parsePkg(pkg: String): NEList[String] = NEList.unsafeFrom(pkg.split('.').toList)
  def parseJvPkg(pkg: String): JvPackageId  = JvPackageId(parsePkg(pkg))
}
