package io.septimalmind.baboon.translator.typescript

import io.septimalmind.baboon.translator.typescript.TsValue.{TsModuleId, TsType}

object TsTypes {
  // baboon modules
  val tsBaboonRuntimeShared: TsModuleId = TsModuleId("BaboonSharedRuntime")
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
  val tsSet: TsType     = TsType.predef("Set")
  val tsBigInt: TsType  = TsType.predef("bigint")

  // ts types
  val tsReadFile: TsType = TsType(nodeFsModule, "readFileSync")

  // Runtime types
  val tsBaboonDecimal: TsType             = TsType(tsBaboonRuntimeShared, "BaboonDecimal")
  val tsBaboonDateTimeUtc: TsType         = TsType(tsBaboonRuntimeShared, "BaboonDateTimeUtc")
  val tsBaboonDateTimeOffset: TsType      = TsType(tsBaboonRuntimeShared, "BaboonDateTimeOffset")
  val tsBaboonCodecContext: TsType        = TsType(tsBaboonRuntimeShared, "BaboonCodecContext")
  val tsBaboonBinWriter: TsType           = TsType(tsBaboonRuntimeShared, "BaboonBinWriter")
  val tsBaboonBinReader: TsType           = TsType(tsBaboonRuntimeShared, "BaboonBinReader")
  val tsBinTools: TsType                  = TsType(tsBaboonRuntimeShared, "BinTools")
  val tsBaboonGenerated: TsType           = TsType(tsBaboonRuntimeShared, "BaboonGenerated")
  val tsBaboonGeneratedLatest: TsType     = TsType(tsBaboonRuntimeShared, "BaboonGeneratedLatest")
  val tsBaboonAdtMemberMeta: TsType       = TsType(tsBaboonRuntimeShared, "BaboonAdtMemberMeta")
  val tsAbstractConversion: TsType        = TsType(tsBaboonRuntimeShared, "AbstractConversion")
  val tsAbstractBaboonConversions: TsType = TsType(tsBaboonRuntimeShared, "AbstractBaboonConversions")
  val tsBaboonLazy: TsType                = TsType(tsBaboonRuntimeShared, "Lazy")

  // Service wiring types
  val baboonMethodId: TsType        = TsType(tsBaboonRuntimeShared, "BaboonMethodId")
  val baboonWiringError: TsType     = TsType(tsBaboonRuntimeShared, "BaboonWiringError")
  val baboonWiringException: TsType = TsType(tsBaboonRuntimeShared, "BaboonWiringException")
  val baboonEither: TsType          = TsType(tsBaboonRuntimeShared, "BaboonEither")

  // Fixture types
  val baboonRandom: TsType = TsType(tsFixtureShared, "BaboonRandom")
}
