package io.septimalmind.baboon.translator.typescript

import io.septimalmind.baboon.translator.typescript.TsValue.{TsModuleId, TsType}

object TsTypes {
  // baboon modules
  val tsBaboonRuntimeShared: TsModuleId = TsModuleId("BaboonSharedRuntime")
  val tsBaboonAnyOpaqueModule: TsModuleId = TsModuleId("BaboonAnyOpaque")
  val tsFixtureShared: TsModuleId       = TsModuleId("BaboonSharedFixture")

  // node modules
  val nodeFsModule: TsModuleId = TsModuleId("fs")

  // TypeScript primitives (predef - don't need imports)
  val tsBoolean: TsType = TsType.predef("boolean")
  val tsNumber: TsType  = TsType.predef("number")
  val tsString: TsType  = TsType.predef("string")
  val tsBytes: TsType   = TsType.predef("Uint8Array")
  val tsArray: TsType   = TsType.predef("Array")
  val tsMap: TsType     = TsType.predef("Map")
  val tsRecord: TsType  = TsType.predef("Record")
  val tsSet: TsType     = TsType.predef("Set")
  val tsDate: TsType    = TsType.predef("Date")
  val tsBigInt: TsType  = TsType.predef("bigint")

  // ts types
  val tsReadFile: TsType = TsType(nodeFsModule, "readFileSync")

  // any-feature runtime types (BaboonAnyOpaque module)
  val tsBaboonAnyOpaque: TsType        = TsType(tsBaboonAnyOpaqueModule, "AnyOpaque", typeOnly = true)
  val tsBaboonAnyMeta: TsType          = TsType(tsBaboonAnyOpaqueModule, "AnyMeta", typeOnly = true)
  val tsBaboonCreateAnyMeta: TsType    = TsType(tsBaboonAnyOpaqueModule, "createAnyMeta")
  val tsBaboonAnyOpaqueUebaCtor: TsType = TsType(tsBaboonAnyOpaqueModule, "anyOpaqueUeba")
  val tsBaboonAnyOpaqueJsonCtor: TsType = TsType(tsBaboonAnyOpaqueModule, "anyOpaqueJson")
  val tsBaboonAnyMetaCodec: TsType     = TsType(tsBaboonAnyOpaqueModule, "AnyMetaCodec")

  // any-feature failures (BaboonSharedRuntime module)
  val tsBaboonEncoderFailure: TsType = TsType(tsBaboonRuntimeShared, "BaboonEncoderFailure")
  val tsBaboonDecoderFailure: TsType = TsType(tsBaboonRuntimeShared, "BaboonDecoderFailure")

  // Runtime types
  val tsBaboonDecimal: TsType             = TsType(tsBaboonRuntimeShared, "BaboonDecimal")
  val tsBaboonDateTimeUtc: TsType         = TsType(tsBaboonRuntimeShared, "BaboonDateTimeUtc")
  val tsBaboonDateTimeOffset: TsType      = TsType(tsBaboonRuntimeShared, "BaboonDateTimeOffset")
  val tsBaboonCodecContext: TsType        = TsType(tsBaboonRuntimeShared, "BaboonCodecContext")
  val tsBaboonBinWriter: TsType           = TsType(tsBaboonRuntimeShared, "BaboonBinWriter")
  val tsBaboonBinReader: TsType           = TsType(tsBaboonRuntimeShared, "BaboonBinReader")
  val tsBinTools: TsType                  = TsType(tsBaboonRuntimeShared, "BinTools")
  val tsBaboonGenerated: TsType           = TsType(tsBaboonRuntimeShared, "BaboonGenerated", typeOnly = true)
  val tsBaboonGeneratedLatest: TsType     = TsType(tsBaboonRuntimeShared, "BaboonGeneratedLatest", typeOnly = true)
  val tsBaboonAdtMemberMeta: TsType       = TsType(tsBaboonRuntimeShared, "BaboonAdtMemberMeta", typeOnly = true)
  val tsAbstractConversion: TsType        = TsType(tsBaboonRuntimeShared, "AbstractConversion", typeOnly = true)
  val tsAbstractBaboonConversions: TsType = TsType(tsBaboonRuntimeShared, "AbstractBaboonConversions", typeOnly = true)
  val tsBaboonLazy: TsType                = TsType(tsBaboonRuntimeShared, "Lazy")

  // Service wiring types
  val baboonMethodId: TsType        = TsType(tsBaboonRuntimeShared, "BaboonMethodId", typeOnly = true)
  val baboonWiringError: TsType     = TsType(tsBaboonRuntimeShared, "BaboonWiringError", typeOnly = true)
  val baboonWiringException: TsType = TsType(tsBaboonRuntimeShared, "BaboonWiringException")
  val baboonEither: TsType          = TsType(tsBaboonRuntimeShared, "BaboonEither", typeOnly = true)

  // Fixture types
  val baboonRandom: TsType = TsType(tsFixtureShared, "BaboonRandom")
}
