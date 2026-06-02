package io.septimalmind.baboon.translator.swift

import io.septimalmind.baboon.translator.swift.SwValue.{SwPackageId, SwType}
import izumi.fundamentals.collections.nonempty.NEList

object SwTypes {
  // baboon packages
  val baboonRuntimePkg: SwPackageId = parseSwPkg("baboon.runtime.shared")
  val baboonFixturePkg: SwPackageId = parseSwPkg("baboon.fixture")

  // baboon metadata
  val iBaboonGenerated: SwType       = SwType(baboonRuntimePkg, "BaboonGenerated")
  val iBaboonAdtMemberMeta: SwType   = SwType(baboonRuntimePkg, "BaboonAdtMemberMeta")
  val iBaboonGeneratedLatest: SwType = SwType(baboonRuntimePkg, "BaboonGeneratedLatest")
  val iBaboonMetaProvider: SwType    = SwType(baboonRuntimePkg, "BaboonMetaProvider")
  val baboonMeta: SwType             = SwType(baboonRuntimePkg, "BaboonMeta")

  // baboon JSON codecs types
  val baboonJsonCodec: SwType                      = SwType(baboonRuntimePkg, "BaboonJsonCodec")
  val baboonJsonCodecBase: SwType                  = SwType(baboonRuntimePkg, "BaboonJsonCodecBase")
  val baboonJsonCodecBaseGenerated: SwType         = SwType(baboonRuntimePkg, "BaboonJsonCodecBaseGenerated")
  val baboonJsonCodecBaseGeneratedAdt: SwType      = SwType(baboonRuntimePkg, "BaboonJsonCodecBaseGeneratedAdt")
  val baboonJsonCodecNoEncoder: SwType             = SwType(baboonRuntimePkg, "BaboonJsonCodecNoEncoder")
  val baboonJsonCodecNoEncoderGenerated: SwType    = SwType(baboonRuntimePkg, "BaboonJsonCodecNoEncoderGenerated")
  val baboonJsonCodecNoEncoderGeneratedAdt: SwType = SwType(baboonRuntimePkg, "BaboonJsonCodecNoEncoderGeneratedAdt")

  // baboon UEBA codecs types
  val baboonBinCodec: SwType                      = SwType(baboonRuntimePkg, "BaboonBinCodec")
  val baboonBinCodecBase: SwType                  = SwType(baboonRuntimePkg, "BaboonBinCodecBase")
  val baboonBinCodecNoEncoder: SwType             = SwType(baboonRuntimePkg, "BaboonBinCodecNoEncoder")
  val baboonBinCodecBaseGenerated: SwType         = SwType(baboonRuntimePkg, "BaboonBinCodecBaseGenerated")
  val baboonBinCodecNoEncoderGenerated: SwType    = SwType(baboonRuntimePkg, "BaboonBinCodecNoEncoderGenerated")
  val baboonBinCodecBaseGeneratedAdt: SwType      = SwType(baboonRuntimePkg, "BaboonBinCodecBaseGeneratedAdt")
  val baboonBinCodecNoEncoderGeneratedAdt: SwType = SwType(baboonRuntimePkg, "BaboonBinCodecNoEncoderGeneratedAdt")

  // baboon types
  val baboonBinCodecIndexed: SwType = SwType(baboonRuntimePkg, "BaboonBinCodecIndexed")
  val baboonCodecContext: SwType    = SwType(baboonRuntimePkg, "BaboonCodecContext")
  val baboonTimeFormats: SwType     = SwType(baboonRuntimePkg, "BaboonTimeFormats")
  // identifier repr helpers (PR-57c)
  val baboonIdRepr: SwType          = SwType(baboonRuntimePkg, "BaboonIdentifierRepr")
  val baboonIdReprCursor: SwType    = SwType(baboonRuntimePkg, "BaboonIdentifierRepr.Cursor")
  val baboonEither: SwType          = SwType(baboonRuntimePkg, "BaboonEither")
  val baboonBinTools: SwType        = SwType(baboonRuntimePkg, "BaboonBinTools")
  val baboonBinWriter: SwType       = SwType(baboonRuntimePkg, "BaboonBinWriter")
  val baboonBinReader: SwType       = SwType(baboonRuntimePkg, "BaboonBinReader")
  val baboonByteStringTools: SwType = SwType(baboonRuntimePkg, "BaboonByteStringTools")
  val baboonDecimal: SwType         = SwType(baboonRuntimePkg, "BaboonDecimal")
  val baboonDateTimeOffset: SwType  = SwType(baboonRuntimePkg, "BaboonDateTimeOffset")
  val baboonRandom: SwType          = SwType(baboonFixturePkg, "BaboonRandom")
  val baboonRandomFactory: SwType   = SwType(baboonFixturePkg, "BaboonRandomFactory")

  // baboon service wiring types
  val baboonMethodId: SwType        = SwType(baboonRuntimePkg, "BaboonMethodId")
  val baboonWiringError: SwType     = SwType(baboonRuntimePkg, "BaboonWiringError")
  val baboonWiringException: SwType = SwType(baboonRuntimePkg, "BaboonWiringException")
  val ibaboonJsonService: SwType    = SwType(baboonRuntimePkg, "IBaboonJsonService")
  val ibaboonUebaService: SwType    = SwType(baboonRuntimePkg, "IBaboonUebaService")
  // Context-carrying service protocols (emitted for abstract/`type` service-context modes).
  val ibaboonJsonServiceCtx: SwType = SwType(baboonRuntimePkg, "IBaboonJsonServiceCtx")
  val ibaboonUebaServiceCtx: SwType = SwType(baboonRuntimePkg, "IBaboonUebaServiceCtx")
  val anyJsonService: SwType        = SwType(baboonRuntimePkg, "AnyJsonService")
  val anyUebaService: SwType        = SwType(baboonRuntimePkg, "AnyUebaService")

  // baboon conversions
  val baboonAbstractConversion: SwType  = SwType(baboonRuntimePkg, "AbstractConversion")
  val baboonAbstractConversions: SwType = SwType(baboonRuntimePkg, "AbstractBaboonConversions")

  // baboon `any`-feature surface types (PR 9.2). All live in the bundled `BaboonRuntime` Swift
  // module — no per-file imports needed (the module ships `baboon_runtime.swift`,
  // `BaboonAnyOpaque.swift`, and `BaboonCodecsFacade.swift` together).
  val baboonAnyOpaque: SwType      = SwType(baboonRuntimePkg, "AnyOpaque")
  val baboonAnyMeta: SwType        = SwType(baboonRuntimePkg, "AnyMeta")
  val baboonAnyMetaCodec: SwType   = SwType(baboonRuntimePkg, "AnyMetaCodec")
  val baboonCodecsFacade: SwType   = SwType(baboonRuntimePkg, "BaboonCodecsFacade")
  val baboonCodecException: SwType = SwType(baboonRuntimePkg, "BaboonCodecException")

  // Swift predef types
  val swStdlibPkg: SwPackageId     = parseSwPkg("swift.stdlib")
  val swFoundationPkg: SwPackageId = parseSwPkg("swift.foundation")
  val swBool: SwType               = SwType(swStdlibPkg, "Bool", predef = true)
  val swInt8: SwType               = SwType(swStdlibPkg, "Int8", predef = true)
  val swInt16: SwType              = SwType(swStdlibPkg, "Int16", predef = true)
  val swInt32: SwType              = SwType(swStdlibPkg, "Int32", predef = true)
  val swInt64: SwType              = SwType(swStdlibPkg, "Int64", predef = true)
  val swUInt8: SwType              = SwType(swStdlibPkg, "UInt8", predef = true)
  val swUInt16: SwType             = SwType(swStdlibPkg, "UInt16", predef = true)
  val swUInt32: SwType             = SwType(swStdlibPkg, "UInt32", predef = true)
  val swUInt64: SwType             = SwType(swStdlibPkg, "UInt64", predef = true)
  val swFloat: SwType              = SwType(swStdlibPkg, "Float", predef = true)
  val swDouble: SwType             = SwType(swStdlibPkg, "Double", predef = true)
  val swString: SwType             = SwType(swStdlibPkg, "String", predef = true)
  val swArray: SwType              = SwType(swStdlibPkg, "Array", predef = true)
  val swSet: SwType                = SwType(swStdlibPkg, "Set", predef = true)
  val swDictionary: SwType         = SwType(swStdlibPkg, "Dictionary", predef = true)
  val swAny: SwType                = SwType(swStdlibPkg, "Any", predef = true)
  val swVoid: SwType               = SwType(swStdlibPkg, "Void", predef = true)

  // Foundation types (not predef - need import Foundation)
  val swData: SwType = SwType(swFoundationPkg, "Data")
  val swUUID: SwType = SwType(swFoundationPkg, "UUID")
  val swDate: SwType = SwType(swFoundationPkg, "Date")

  def parsePkg(pkg: String): NEList[String] = NEList.unsafeFrom(pkg.split('.').toList)
  def parseSwPkg(pkg: String): SwPackageId  = SwPackageId(parsePkg(pkg))
}
