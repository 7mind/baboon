package io.septimalmind.baboon.translator.typescript

import io.septimalmind.baboon.translator.typescript.TsValue.{TsModuleId, TsType}
import izumi.fundamentals.collections.nonempty.NEList

object TsTypes {
  val runtimeModule: TsModuleId = TsModuleId(NEList("baboon_runtime"))
  val fixtureModule: TsModuleId = TsModuleId(NEList("baboon_fixture"))
  val predefModule: TsModuleId  = TsModuleId(NEList("predef"))

  // TypeScript primitives (predef - don't need imports)
  val tsBoolean: TsType    = TsType(predefModule, "boolean", predef = true)
  val tsNumber: TsType     = TsType(predefModule, "number", predef = true)
  val tsBigint: TsType     = TsType(predefModule, "bigint", predef = true)
  val tsString: TsType     = TsType(predefModule, "string", predef = true)
  val tsUint8Array: TsType = TsType(predefModule, "Uint8Array", predef = true)

  // Runtime types
  val baboonDecimal: TsType             = TsType(runtimeModule, "BaboonDecimal")
  val baboonDateTimeUtc: TsType         = TsType(runtimeModule, "BaboonDateTimeUtc")
  val baboonDateTimeOffset: TsType      = TsType(runtimeModule, "BaboonDateTimeOffset")
  val baboonCodecContext: TsType        = TsType(runtimeModule, "BaboonCodecContext")
  val baboonBinWriter: TsType           = TsType(runtimeModule, "BaboonBinWriter")
  val baboonBinReader: TsType           = TsType(runtimeModule, "BaboonBinReader")
  val binTools: TsType                  = TsType(runtimeModule, "BinTools")
  val baboonGenerated: TsType           = TsType(runtimeModule, "BaboonGenerated")
  val baboonGeneratedLatest: TsType     = TsType(runtimeModule, "BaboonGeneratedLatest")
  val baboonAdtMemberMeta: TsType       = TsType(runtimeModule, "BaboonAdtMemberMeta")
  val abstractConversion: TsType        = TsType(runtimeModule, "AbstractConversion")
  val abstractBaboonConversions: TsType = TsType(runtimeModule, "AbstractBaboonConversions")

  // Service wiring types
  val baboonMethodId: TsType        = TsType(runtimeModule, "BaboonMethodId")
  val baboonWiringError: TsType     = TsType(runtimeModule, "BaboonWiringError")
  val baboonWiringException: TsType = TsType(runtimeModule, "BaboonWiringException")
  val baboonEither: TsType          = TsType(runtimeModule, "BaboonEither")

  // Fixture types
  val baboonRandom: TsType = TsType(fixtureModule, "BaboonRandom")
}
