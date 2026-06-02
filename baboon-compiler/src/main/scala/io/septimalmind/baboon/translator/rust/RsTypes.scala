package io.septimalmind.baboon.translator.rust

import io.septimalmind.baboon.translator.rust.RsValue.{RsCrateId, RsType}
import izumi.fundamentals.collections.nonempty.NEList

class RsTypes(val cratePrefix: String) {
  private def parseCrate(path: String): RsCrateId = RsCrateId(NEList.unsafeFrom(path.split("::").toList))

  // baboon runtime
  val baboonRuntimeCrate: RsCrateId   = parseCrate(s"$cratePrefix::baboon_runtime")
  val baboonFixtureCrate: RsCrateId   = parseCrate(s"$cratePrefix::baboon_fixture")
  val baboonAnyOpaqueCrate: RsCrateId = parseCrate(s"$cratePrefix::any_opaque")
  val baboonIdReprCrate: RsCrateId    = parseCrate(s"$cratePrefix::baboon_identifier_repr")

  // baboon any-opaque types (PR 4.1 runtime)
  val baboonAnyOpaque: RsType = RsType(baboonAnyOpaqueCrate, "AnyOpaque")

  // baboon identifier repr helpers (PR-57c). The Cursor type is referenced
  // by emitted parseRepr code as a value/type. Module-level free functions
  // (escape_str, parse_*, etc.) are emitted as plain `crate::baboon_identifier_repr::*`
  // strings.
  val baboonIdReprCursor: RsType = RsType(baboonIdReprCrate, "Cursor")

  // baboon metadata markers
  val baboonGenerated: RsType       = RsType(baboonRuntimeCrate, "BaboonGenerated")
  val baboonGeneratedLatest: RsType = RsType(baboonRuntimeCrate, "BaboonGeneratedLatest")
  val baboonAdtMemberMeta: RsType   = RsType(baboonRuntimeCrate, "BaboonAdtMemberMeta")
  val baboonMeta: RsType            = RsType(baboonRuntimeCrate, "BaboonMeta")

  // baboon codec types
  val baboonCodecContext: RsType = RsType(baboonRuntimeCrate, "BaboonCodecContext")
  val baboonBinEncode: RsType    = RsType(baboonRuntimeCrate, "BaboonBinEncode")
  val baboonBinDecode: RsType    = RsType(baboonRuntimeCrate, "BaboonBinDecode")
  val baboonBinTools: RsType     = RsType(baboonRuntimeCrate, "bin_tools")
  val baboonTimeFormats: RsType  = RsType(baboonRuntimeCrate, "time_formats")

  // baboon service wiring types
  val baboonServiceWiringCrate: RsCrateId = parseCrate(s"$cratePrefix::baboon_service_wiring")
  val baboonMethodId: RsType              = RsType(baboonServiceWiringCrate, "BaboonMethodId")
  val baboonWiringError: RsType           = RsType(baboonServiceWiringCrate, "BaboonWiringError")
  val ibaboonJsonService: RsType          = RsType(baboonServiceWiringCrate, "IBaboonJsonService")
  val ibaboonUebaService: RsType          = RsType(baboonServiceWiringCrate, "IBaboonUebaService")
  val baboonJsonMuxer: RsType             = RsType(baboonServiceWiringCrate, "JsonMuxer")
  val baboonUebaMuxer: RsType             = RsType(baboonServiceWiringCrate, "UebaMuxer")
  // Context-carrying variants (emitted only when a service.context mode is
  // active). Additive — the context-free traits/muxers above are unchanged.
  val ibaboonJsonServiceCtx: RsType       = RsType(baboonServiceWiringCrate, "IBaboonJsonServiceCtx")
  val ibaboonUebaServiceCtx: RsType       = RsType(baboonServiceWiringCrate, "IBaboonUebaServiceCtx")
  val baboonJsonMuxerCtx: RsType          = RsType(baboonServiceWiringCrate, "JsonMuxerCtx")
  val baboonUebaMuxerCtx: RsType          = RsType(baboonServiceWiringCrate, "UebaMuxerCtx")

  // baboon conversions
  val baboonAbstractConversion: RsType  = RsType(baboonRuntimeCrate, "AbstractConversion")
  val baboonAbstractConversions: RsType = RsType(baboonRuntimeCrate, "AbstractBaboonConversions")

  // baboon random/fixture
  val baboonRandom: RsType = RsType(baboonFixtureCrate, "BaboonRandom")

  // serde helpers in runtime
  val hexBytesSerde: RsType    = RsType(baboonRuntimeCrate, "hex_bytes")
  val decimalSerde: RsType     = RsType(baboonRuntimeCrate, "decimal_as_number")
  val optHexBytesSerde: RsType = RsType(baboonRuntimeCrate, "opt_hex_bytes")
  val optDecimalSerde: RsType  = RsType(baboonRuntimeCrate, "opt_decimal_as_number")
}

object RsTypes {
  // Rust std
  val stdCrate: RsCrateId = RsTypes.parseCrate("std")

  // Rust primitive types (predef - don't need imports)
  val rsBool: RsType   = RsType(stdCrate, "bool", predef = true)
  val rsI8: RsType     = RsType(stdCrate, "i8", predef = true)
  val rsI16: RsType    = RsType(stdCrate, "i16", predef = true)
  val rsI32: RsType    = RsType(stdCrate, "i32", predef = true)
  val rsI64: RsType    = RsType(stdCrate, "i64", predef = true)
  val rsU8: RsType     = RsType(stdCrate, "u8", predef = true)
  val rsU16: RsType    = RsType(stdCrate, "u16", predef = true)
  val rsU32: RsType    = RsType(stdCrate, "u32", predef = true)
  val rsU64: RsType    = RsType(stdCrate, "u64", predef = true)
  val rsF32: RsType    = RsType(stdCrate, "f32", predef = true)
  val rsF64: RsType    = RsType(stdCrate, "f64", predef = true)
  val rsString: RsType = RsType(stdCrate, "String", predef = true)

  // Rust std collections
  val stdCollectionsCrate: RsCrateId = RsTypes.parseCrate("std::collections")
  val rsBTreeMap: RsType             = RsType(stdCollectionsCrate, "BTreeMap")
  val rsBTreeSet: RsType             = RsType(stdCollectionsCrate, "BTreeSet")

  // Rust std types
  val rsVec: RsType    = RsType(stdCrate, "Vec", predef = true)
  val rsOption: RsType = RsType(stdCrate, "Option", predef = true)
  val rsResult: RsType = RsType(stdCrate, "Result", predef = true)
  val rsBox: RsType    = RsType(stdCrate, "Box", predef = true)

  // External crates
  val uuidCrate: RsCrateId = RsTypes.parseCrate("uuid")
  val rsUuid: RsType       = RsType(uuidCrate, "Uuid")

  val chronoCrate: RsCrateId  = RsTypes.parseCrate("chrono")
  val rsDateTimeUtc: RsType   = RsType(chronoCrate, "DateTime<chrono::Utc>")
  val rsDateTimeFixed: RsType = RsType(chronoCrate, "DateTime<chrono::FixedOffset>")
  // For rendering simple type names in generics
  val rsChronoUtc: RsType         = RsType(chronoCrate, "Utc")
  val rsChronoFixedOffset: RsType = RsType(chronoCrate, "FixedOffset")
  val rsChronoDateTime: RsType    = RsType(chronoCrate, "DateTime")

  val decimalCrate: RsCrateId = RsTypes.parseCrate("rust_decimal")
  val rsDecimal: RsType       = RsType(decimalCrate, "Decimal")

  // serde
  val serdeCrate: RsCrateId     = RsTypes.parseCrate("serde")
  val rsSerialize: RsType       = RsType(serdeCrate, "Serialize")
  val rsDeserialize: RsType     = RsType(serdeCrate, "Deserialize")
  val serdeJsonCrate: RsCrateId = RsTypes.parseCrate("serde_json")
  val rsSerdeJsonValue: RsType  = RsType(serdeJsonCrate, "Value")

  def parseCrate(path: String): RsCrateId = RsCrateId(NEList.unsafeFrom(path.split("::").toList))
}
