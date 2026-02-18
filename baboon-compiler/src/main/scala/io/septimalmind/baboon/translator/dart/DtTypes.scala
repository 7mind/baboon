package io.septimalmind.baboon.translator.dart

import io.septimalmind.baboon.translator.dart.DtValue.{DtPackageId, DtType}
import izumi.fundamentals.collections.nonempty.NEList

object DtTypes {
  // baboon packages
  val baboonRuntimePkg: DtPackageId = parseDtPkg("baboon.runtime.shared")
  val baboonFixturePkg: DtPackageId = parseDtPkg("baboon.fixture")

  // baboon metadata
  val iBaboonGenerated: DtType       = DtType(baboonRuntimePkg, "BaboonGenerated")
  val iBaboonAdtMemberMeta: DtType   = DtType(baboonRuntimePkg, "BaboonAdtMemberMeta")
  val iBaboonGeneratedLatest: DtType = DtType(baboonRuntimePkg, "BaboonGeneratedLatest")
  val baboonMeta: DtType             = DtType(baboonRuntimePkg, "BaboonMeta")

  // baboon codecs types
  val baboonJsonCodec: DtType                      = DtType(baboonRuntimePkg, "BaboonJsonCodec")
  val baboonJsonCodecBase: DtType                  = DtType(baboonRuntimePkg, "BaboonJsonCodecBase")
  val baboonJsonCodecBaseGenerated: DtType         = DtType(baboonRuntimePkg, "BaboonJsonCodecBaseGenerated")
  val baboonJsonCodecBaseGeneratedAdt: DtType      = DtType(baboonRuntimePkg, "BaboonJsonCodecBaseGeneratedAdt")
  val baboonJsonCodecNoEncoder: DtType             = DtType(baboonRuntimePkg, "BaboonJsonCodecNoEncoder")
  val baboonJsonCodecNoEncoderGenerated: DtType    = DtType(baboonRuntimePkg, "BaboonJsonCodecNoEncoderGenerated")
  val baboonJsonCodecNoEncoderGeneratedAdt: DtType = DtType(baboonRuntimePkg, "BaboonJsonCodecNoEncoderGeneratedAdt")

  val baboonBinCodec: DtType                      = DtType(baboonRuntimePkg, "BaboonBinCodec")
  val baboonBinCodecBase: DtType                  = DtType(baboonRuntimePkg, "BaboonBinCodecBase")
  val baboonBinCodecNoEncoder: DtType             = DtType(baboonRuntimePkg, "BaboonBinCodecNoEncoder")
  val baboonBinCodecBaseGenerated: DtType         = DtType(baboonRuntimePkg, "BaboonBinCodecBaseGenerated")
  val baboonBinCodecNoEncoderGenerated: DtType    = DtType(baboonRuntimePkg, "BaboonBinCodecNoEncoderGenerated")
  val baboonBinCodecBaseGeneratedAdt: DtType      = DtType(baboonRuntimePkg, "BaboonBinCodecBaseGeneratedAdt")
  val baboonBinCodecNoEncoderGeneratedAdt: DtType = DtType(baboonRuntimePkg, "BaboonBinCodecNoEncoderGeneratedAdt")

  // baboon types
  val baboonBinCodecIndexed: DtType = DtType(baboonRuntimePkg, "BaboonBinCodecIndexed")
  val baboonCodecContext: DtType    = DtType(baboonRuntimePkg, "BaboonCodecContext")
  val baboonTimeFormats: DtType     = DtType(baboonRuntimePkg, "BaboonTimeFormats")
  val baboonBinTools: DtType        = DtType(baboonRuntimePkg, "BaboonBinTools")
  val baboonBinWriter: DtType       = DtType(baboonRuntimePkg, "BaboonBinWriter")
  val baboonBinReader: DtType       = DtType(baboonRuntimePkg, "BaboonBinReader")
  val baboonByteStringTools: DtType = DtType(baboonRuntimePkg, "BaboonByteStringTools")
  val baboonDecimal: DtType         = DtType(baboonRuntimePkg, "BaboonDecimal")
  val baboonDateTimeOffset: DtType  = DtType(baboonRuntimePkg, "BaboonDateTimeOffset")
  val baboonRandom: DtType          = DtType(baboonFixturePkg, "BaboonRandom")
  val baboonRandomFactory: DtType   = DtType(baboonFixturePkg, "BaboonRandomFactory")

  // baboon service wiring types
  val baboonMethodId: DtType        = DtType(baboonRuntimePkg, "BaboonMethodId")
  val baboonWiringError: DtType     = DtType(baboonRuntimePkg, "BaboonWiringError")
  val baboonWiringException: DtType = DtType(baboonRuntimePkg, "BaboonWiringException")

  // baboon conversions
  val baboonAbstractConversion: DtType  = DtType(baboonRuntimePkg, "AbstractConversion")
  val baboonAbstractConversions: DtType = DtType(baboonRuntimePkg, "AbstractBaboonConversions")

  // Dart predef types
  val dartCorePkg: DtPackageId = parseDtPkg("dart.core")
  val dtBool: DtType           = DtType(dartCorePkg, "bool", predef = true)
  val dtInt: DtType            = DtType(dartCorePkg, "int", predef = true)
  val dtDouble: DtType         = DtType(dartCorePkg, "double", predef = true)
  val dtString: DtType         = DtType(dartCorePkg, "String", predef = true)
  val dtObject: DtType         = DtType(dartCorePkg, "Object", predef = true)
  val dtVoid: DtType           = DtType(dartCorePkg, "void", predef = true)
  val dtList: DtType           = DtType(dartCorePkg, "List", predef = true)
  val dtSet: DtType            = DtType(dartCorePkg, "Set", predef = true)
  val dtMap: DtType            = DtType(dartCorePkg, "Map", predef = true)
  val dtDynamic: DtType        = DtType(dartCorePkg, "dynamic", predef = true)

  // dart:typed_data
  val dartTypedDataPkg: DtPackageId = parseDtPkg("dart.typed_data")
  val dtUint8List: DtType           = DtType(dartTypedDataPkg, "Uint8List")
  val dtByteData: DtType            = DtType(dartTypedDataPkg, "ByteData")

  // dart:convert
  val dartConvertPkg: DtPackageId = parseDtPkg("dart.convert")

  // dart:io
  val dartIoPkg: DtPackageId = parseDtPkg("dart.io")
  val dtFile: DtType         = DtType(dartIoPkg, "File")

  // Dart DateTime (from dart:core)
  val dtDateTime: DtType = DtType(dartCorePkg, "DateTime", predef = true)

  def parsePkg(pkg: String): NEList[String] = NEList.unsafeFrom(pkg.split('.').toList)
  def parseDtPkg(pkg: String): DtPackageId  = DtPackageId(parsePkg(pkg))
}
