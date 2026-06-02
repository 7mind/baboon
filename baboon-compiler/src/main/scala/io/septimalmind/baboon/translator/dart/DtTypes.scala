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
  // PR-F: BaboonAdtMember is the runtime-active interface with an instance getter
  // (`String get baboonAdtTypeIdentifier`) used by `BaboonTypeMeta.from(_, useAdtIdentifier: true)`.
  // BaboonAdtMemberMeta is the static-only marker kept for back-compat; new ADT branches
  // implement BaboonAdtMember so the instance @override is valid and the runtime check fires.
  val iBaboonAdtMember: DtType       = DtType(baboonRuntimePkg, "BaboonAdtMember")
  val iBaboonGeneratedLatest: DtType = DtType(baboonRuntimePkg, "BaboonGeneratedLatest")
  val iBaboonMetaProvider: DtType    = DtType(baboonRuntimePkg, "BaboonMetaProvider")
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

  // baboon `any`-feature types (PR 8.2). The runtime ships these in two separate Dart files —
  // `baboon_any_opaque.dart` (sealed `AnyOpaque` + `AnyMeta` + `AnyMetaCodec`) and
  // `baboon_codecs_facade.dart` (concrete `BaboonCodecsFacade` carrying `jsonToUebaBytes` /
  // `uebaToJson`). The base abstract type `BaboonCodecsFacadeBase` lives in `baboon_runtime.dart`
  // (already imported) so `ctx.facade` typechecks; the codec downcasts to the concrete facade
  // when invoking the cross-format helpers.
  val baboonAnyOpaquePkg: DtPackageId = parseDtPkg("baboon.any.opaque")
  val baboonAnyOpaque: DtType         = DtType(baboonAnyOpaquePkg, "AnyOpaque")
  val baboonAnyOpaqueUeba: DtType     = DtType(baboonAnyOpaquePkg, "AnyOpaqueUeba")
  val baboonAnyOpaqueJson: DtType     = DtType(baboonAnyOpaquePkg, "AnyOpaqueJson")
  val baboonAnyMeta: DtType           = DtType(baboonAnyOpaquePkg, "AnyMeta")
  val baboonAnyMetaCodec: DtType      = DtType(baboonAnyOpaquePkg, "AnyMetaCodec")

  val baboonCodecsFacadePkg: DtPackageId = parseDtPkg("baboon.codecs.facade")
  val baboonCodecsFacade: DtType         = DtType(baboonCodecsFacadePkg, "BaboonCodecsFacade")

  // facade-failure types — already in `baboon_runtime.dart`.
  val baboonEncoderFailure: DtType = DtType(baboonRuntimePkg, "BaboonEncoderFailure")
  val baboonDecoderFailure: DtType = DtType(baboonRuntimePkg, "BaboonDecoderFailure")
  val baboonLeft: DtType           = DtType(baboonRuntimePkg, "BaboonLeft")
  val baboonRight: DtType          = DtType(baboonRuntimePkg, "BaboonRight")
  val baboonCodecException: DtType = DtType(baboonRuntimePkg, "BaboonCodecException")

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
  // Identifier-repr runtime helpers (PR-57d)
  val baboonIdReprPkg: DtPackageId = parseDtPkg("baboon.identifier.repr")
  val baboonIdRepr: DtType         = DtType(baboonIdReprPkg, "BaboonIdRepr")
  val baboonIdReprCursor: DtType   = DtType(baboonIdReprPkg, "BaboonIdReprCursor")
  val baboonEither: DtType         = DtType(baboonRuntimePkg, "BaboonEither")
  val baboonRandom: DtType         = DtType(baboonFixturePkg, "BaboonRandom")
  val baboonRandomFactory: DtType  = DtType(baboonFixturePkg, "BaboonRandomFactory")

  // baboon service wiring types
  val baboonMethodId: DtType        = DtType(baboonRuntimePkg, "BaboonMethodId")
  val baboonWiringError: DtType     = DtType(baboonRuntimePkg, "BaboonWiringError")
  val baboonWiringException: DtType = DtType(baboonRuntimePkg, "BaboonWiringException")
  // Cross-domain Muxer entry-point interfaces — see `// --- Service muxers ---`
  // in `baboon_runtime.dart`. Each per-service wrapper class generated by
  // DtServiceWiringTranslator implements one of these.
  val baboonJsonServiceIface: DtType = DtType(baboonRuntimePkg, "IBaboonJsonService")
  val baboonUebaServiceIface: DtType = DtType(baboonRuntimePkg, "IBaboonUebaService")
  val baboonJsonMuxer: DtType        = DtType(baboonRuntimePkg, "JsonMuxer")
  val baboonUebaMuxer: DtType        = DtType(baboonRuntimePkg, "UebaMuxer")
  // Context-carrying Muxer entry-point interfaces — emitted only when a
  // service.context mode (`abstract`/`type`) is active. The per-service wrapper
  // class implements one of these (service context supplied per-invoke).
  val baboonJsonServiceCtxIface: DtType = DtType(baboonRuntimePkg, "IBaboonJsonServiceCtx")
  val baboonUebaServiceCtxIface: DtType = DtType(baboonRuntimePkg, "IBaboonUebaServiceCtx")

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
  // Top-level helpers in dart:convert — referenced by service-wiring codegen.
  // Carrying them as DtType symbols makes the file-level import-trigger pick
  // them up so the emitted `import 'dart:convert';` lands automatically.
  // `predef = false` keeps them in the import-trigger set (dart:convert is not
  // implicitly imported, unlike dart:core); the rendered form is just the bare
  // identifier (`jsonEncode` / `jsonDecode`) since `fq = false` is the default.
  val dtJsonEncode: DtType = DtType(dartConvertPkg, "jsonEncode")
  val dtJsonDecode: DtType = DtType(dartConvertPkg, "jsonDecode")

  // dart:io
  val dartIoPkg: DtPackageId = parseDtPkg("dart.io")
  val dtFile: DtType         = DtType(dartIoPkg, "File")

  // Dart DateTime (from dart:core)
  val dtDateTime: DtType = DtType(dartCorePkg, "DateTime", predef = true)

  def parsePkg(pkg: String): NEList[String] = NEList.unsafeFrom(pkg.split('.').toList)
  def parseDtPkg(pkg: String): DtPackageId  = DtPackageId(parsePkg(pkg))
}
