// NOTE: This test references generated DTO/codec symbols (`Holder`, `Inner`, `Holder_UebaCodec`,
// ...) which are produced into this stub only when codegen runs against the full model set
// (including `any-ok/`). Running `dart test` directly from the source tree without codegen will
// fail with missing symbols; run from the codegen'd copy under `target/`.
//
// Round-trip and cross-format tests for `any` fields (issue #69 PR 8.4 / M8 close).
// Mirrors Scala's AnyRoundTripSpec (PR 2.4) / C# (PR 3.4) / Rust (PR 4.3) / Kotlin (PR 5.4) /
// Java (PR 6.4) / TypeScript (PR 7.4). Exercises the `any-ok` fixture's six DSL variants
// (A=any, B=any[domain:this], C=any[domain:current], D1=any[Inner], D2=any[domain:this,Inner],
// D3=any[domain:current,Inner]) plus the three nested positions (opt/lst/map-value).

import 'dart:typed_data';

import 'package:baboon_runtime/baboon_any_opaque.dart';
import 'package:baboon_runtime/baboon_codecs_facade.dart';
import 'package:baboon_runtime/baboon_runtime.dart';
import 'package:test/test.dart';

import '../../lib/my/ok/holder.dart';
import '../../lib/my/ok/inner.dart';

// ===== Test scaffolding ========================================================================

const String DOMAIN_ID = 'my.ok';
const String VERSION_STR = '1.0.0';
const String INNER_TYPE = 'my.ok/:#Inner';

// AnyMeta builders for each variant — populating only the bits the kind byte claims.
AnyMeta metaA([String typeid = 'opaque.Type']) =>
    AnyMeta(0x07, DOMAIN_ID, VERSION_STR, typeid);
AnyMeta metaB([String typeid = 'opaque.Type']) =>
    AnyMeta(0x03, null, VERSION_STR, typeid);
AnyMeta metaC([String typeid = 'opaque.Type']) =>
    AnyMeta(0x01, null, null, typeid);
AnyMeta metaD1() => AnyMeta(0x06, DOMAIN_ID, VERSION_STR, null);
AnyMeta metaD2() => AnyMeta(0x02, null, VERSION_STR, null);
AnyMeta metaD3() => AnyMeta(0x00, null, null, null);

const Inner SAMPLE_INNER = Inner(x: 42);

// Generated Dart codecs use the runtime's expected `(ctx, writer, value)` ordering for UEBA
// encode (unlike TS) — no argument-order shim is needed. The base classes
// `BaboonJsonCodec`/`BaboonBinCodec` `implements BaboonCodecData` and codegen now emits
// instance getters delegating to the data class statics, so the registry's lazy factory cast
// works directly on the generated `*_JsonCodec.instance`/`*_UebaCodec.instance` singletons.
// These adapters remain to wire the abstract `BaboonGenerated`-typed registry interface to the
// concrete typed codec; mirrors the `_StubBinCodec`/`_StubJsonCodec` pattern used in
// `any_meta_codec_test.dart`.

class _InnerBinAdapter extends BaboonBinCodecBase<BaboonGenerated> implements BaboonCodecData {
  @override
  String get baboonDomainVersion => Inner.baboonDomainVersion;
  @override
  String get baboonDomainIdentifier => Inner.baboonDomainIdentifier;
  @override
  String get baboonTypeIdentifier => Inner.baboonTypeIdentifier;
  @override
  void encode(BaboonCodecContext ctx, BaboonBinWriter writer, BaboonGenerated value) =>
      Inner_UebaCodec.instance.encode(ctx, writer, value as Inner);
  @override
  BaboonGenerated decode(BaboonCodecContext ctx, BaboonBinReader reader) =>
      Inner_UebaCodec.instance.decode(ctx, reader);
}

class _HolderBinAdapter extends BaboonBinCodecBase<BaboonGenerated> implements BaboonCodecData {
  @override
  String get baboonDomainVersion => Holder.baboonDomainVersion;
  @override
  String get baboonDomainIdentifier => Holder.baboonDomainIdentifier;
  @override
  String get baboonTypeIdentifier => Holder.baboonTypeIdentifier;
  @override
  void encode(BaboonCodecContext ctx, BaboonBinWriter writer, BaboonGenerated value) =>
      Holder_UebaCodec.instance.encode(ctx, writer, value as Holder);
  @override
  BaboonGenerated decode(BaboonCodecContext ctx, BaboonBinReader reader) =>
      Holder_UebaCodec.instance.decode(ctx, reader);
}

class _InnerJsonAdapter extends BaboonJsonCodecBase<BaboonGenerated> implements BaboonCodecData {
  @override
  String get baboonDomainVersion => Inner.baboonDomainVersion;
  @override
  String get baboonDomainIdentifier => Inner.baboonDomainIdentifier;
  @override
  String get baboonTypeIdentifier => Inner.baboonTypeIdentifier;
  @override
  Object? encode(BaboonCodecContext ctx, BaboonGenerated value) =>
      Inner_JsonCodec.instance.encode(ctx, value as Inner);
  @override
  BaboonGenerated decode(BaboonCodecContext ctx, dynamic wire) =>
      Inner_JsonCodec.instance.decode(ctx, wire);
}

class _HolderJsonAdapter extends BaboonJsonCodecBase<BaboonGenerated> implements BaboonCodecData {
  @override
  String get baboonDomainVersion => Holder.baboonDomainVersion;
  @override
  String get baboonDomainIdentifier => Holder.baboonDomainIdentifier;
  @override
  String get baboonTypeIdentifier => Holder.baboonTypeIdentifier;
  @override
  Object? encode(BaboonCodecContext ctx, BaboonGenerated value) =>
      Holder_JsonCodec.instance.encode(ctx, value as Holder);
  @override
  BaboonGenerated decode(BaboonCodecContext ctx, dynamic wire) =>
      Holder_JsonCodec.instance.decode(ctx, wire);
}

class _MyOkUebaCodecs extends AbstractBaboonUebaCodecs {
  _MyOkUebaCodecs() {
    register(Inner.baboonTypeIdentifier, () => _InnerBinAdapter());
    register(Holder.baboonTypeIdentifier, () => _HolderBinAdapter());
  }
}

class _MyOkJsonCodecs extends AbstractBaboonJsonCodecs {
  _MyOkJsonCodecs() {
    register(Inner.baboonTypeIdentifier, () => _InnerJsonAdapter());
    register(Holder.baboonTypeIdentifier, () => _HolderJsonAdapter());
  }
}

class _MyOkConversions extends AbstractBaboonConversions {
  @override
  List<String> get versionsFrom => [];
  @override
  String get versionTo => VERSION_STR;
}

class _MyOkMeta extends BaboonMeta {
  @override
  List<String> sameInVersions(String typeId) => [VERSION_STR];
}

// Fresh per-test facade: registers Holder/Inner codecs from the my.ok domain so the
// cross-format helpers and decodeAny can resolve `(domain, version, typeid)` triples.
BaboonCodecsFacade _freshFacade() {
  final facade = BaboonCodecsFacade();
  final dv = BaboonDomainVersion(DOMAIN_ID, VERSION_STR);
  facade.register(
    dv,
    codecsJson: () => _MyOkJsonCodecs(),
    codecsBin: () => _MyOkUebaCodecs(),
    conversions: () => _MyOkConversions(),
    meta: () => _MyOkMeta(),
  );
  return facade;
}

// Encode an Inner via the generated UEBA codec — used when constructing AnyOpaqueUeba payloads
// so cross-convert tests have a real Inner to deserialize.
Uint8List _innerToUebaBytes(Inner inner) {
  final w = BaboonBinWriter();
  Inner_UebaCodec.instance.encode(BaboonCodecContext.compact, w, inner);
  return w.toBytes();
}

Object? _innerToJson(Inner inner) =>
    Inner_JsonCodec.instance.encode(BaboonCodecContext.compact, inner);

// Build a complete Holder with one AnyOpaqueUeba per variant. UEBA round-trips natively
// (no facade needed for encode/decode — facade is only consulted for cross-convert).
Holder _buildUebaHolder() {
  final innerBytes = _innerToUebaBytes(SAMPLE_INNER);
  return Holder(
    fAny: AnyOpaqueUeba(metaA(), Uint8List.fromList([1, 2, 3])),
    fDomainThis: AnyOpaqueUeba(metaB(), Uint8List.fromList([4, 5])),
    fDomainCurrent: AnyOpaqueUeba(metaC(), Uint8List.fromList([6])),
    fUnderlying: AnyOpaqueUeba(metaD1(), innerBytes),
    fThisUnderlying: AnyOpaqueUeba(metaD2(), innerBytes),
    fCurrentUnderlying: AnyOpaqueUeba(metaD3(), innerBytes),
    fOpt: AnyOpaqueUeba(metaA(), Uint8List.fromList([7])),
    fLst: [AnyOpaqueUeba(metaD1(), innerBytes)],
    fMapValue: {'k1': AnyOpaqueUeba(metaA(), Uint8List.fromList([8]))},
  );
}

// Build a Holder using AnyOpaqueJson branches everywhere with arbitrary inner JSON content.
// Used as the "all native JSON branch" baseline for JSON round-trip tests.
Holder _buildJsonNativeHolder() {
  final arbitrary = {'payload': 42};
  final innerJson = _innerToJson(SAMPLE_INNER);
  return Holder(
    fAny: AnyOpaqueJson(metaA(), arbitrary),
    fDomainThis: AnyOpaqueJson(metaB(), arbitrary),
    fDomainCurrent: AnyOpaqueJson(metaC(), arbitrary),
    fUnderlying: AnyOpaqueJson(metaD1(), innerJson),
    fThisUnderlying: AnyOpaqueJson(metaD2(), innerJson),
    fCurrentUnderlying: AnyOpaqueJson(metaD3(), innerJson),
    fOpt: AnyOpaqueJson(metaA(), arbitrary),
    fLst: [AnyOpaqueJson(metaD1(), innerJson)],
    fMapValue: {'k1': AnyOpaqueJson(metaA(), arbitrary)},
  );
}

// Build a Holder using AnyOpaqueJson branches with REAL Inner JSON for D variants and
// typeid=INNER_TYPE for A/B/C so cross-convert can resolve Inner via the registered facade.
Holder _buildJsonHolderForCrossConvert() {
  final innerJson = _innerToJson(SAMPLE_INNER);
  return Holder(
    fAny: AnyOpaqueJson(metaA(INNER_TYPE), innerJson),
    fDomainThis: AnyOpaqueJson(metaB(INNER_TYPE), innerJson),
    fDomainCurrent: AnyOpaqueJson(metaC(INNER_TYPE), innerJson),
    fUnderlying: AnyOpaqueJson(metaD1(), innerJson),
    fThisUnderlying: AnyOpaqueJson(metaD2(), innerJson),
    fCurrentUnderlying: AnyOpaqueJson(metaD3(), innerJson),
    fOpt: AnyOpaqueJson(metaA(INNER_TYPE), innerJson),
    fLst: [AnyOpaqueJson(metaD1(), innerJson)],
    fMapValue: {'k1': AnyOpaqueJson(metaA(INNER_TYPE), innerJson)},
  );
}

Uint8List _encodeUebaBytes(Holder value, BaboonCodecContext ctx) {
  final w = BaboonBinWriter();
  Holder_UebaCodec.instance.encode(ctx, w, value);
  return w.toBytes();
}

Holder _decodeUebaBytes(
  Uint8List bytes, [
  BaboonCodecContext ctx = BaboonCodecContext.compact,
]) =>
    Holder_UebaCodec.instance.decode(ctx, BaboonBinReader(bytes));

int _readI32Le(Uint8List data, int offset) =>
    (data[offset] & 0xFF) |
    ((data[offset + 1] & 0xFF) << 8) |
    ((data[offset + 2] & 0xFF) << 16) |
    ((data[offset + 3] & 0xFF) << 24);

void main() {
  // ===== 1. Per-variant UEBA round-trip =====================================================

  group('UEBA round-trip', () {
    test('compact mode preserves all six variants + nested positions', () {
      final original = _buildUebaHolder();
      final bytes = _encodeUebaBytes(original, BaboonCodecContext.compact);
      final decoded = _decodeUebaBytes(bytes, BaboonCodecContext.compact);
      expect(decoded, equals(original));
    });

    test('indexed mode preserves content (Dart Holder emits real per-field offset blocks)', () {
      // Mirrors Java/Kotlin/Rust/TS per-variant indexed-mode coverage. Dart's Holder_UebaCodec
      // emits a header byte 0x01 then per-field (offset, length) i32 prefixes followed by the
      // contiguous payload buffer; decode reads the prefix block then linearly decodes the data.
      final original = _buildUebaHolder();
      final bytes = _encodeUebaBytes(original, BaboonCodecContext.indexed);
      final decoded = _decodeUebaBytes(bytes, BaboonCodecContext.indexed);
      expect(decoded, equals(original));
    });

    test('UEBA decode yields AnyOpaqueUeba branch with matching kind bytes', () {
      final original = _buildUebaHolder();
      final bytes = _encodeUebaBytes(original, BaboonCodecContext.compact);
      final decoded = _decodeUebaBytes(bytes);
      expect(decoded.fAny, isA<AnyOpaqueUeba>());
      expect(decoded.fAny.meta.kind, 0x07);
      expect(decoded.fDomainThis.meta.kind, 0x03);
      expect(decoded.fDomainCurrent.meta.kind, 0x01);
      expect(decoded.fUnderlying.meta.kind, 0x06);
      expect(decoded.fThisUnderlying.meta.kind, 0x02);
      expect(decoded.fCurrentUnderlying.meta.kind, 0x00);
    });
  });

  // ===== 2. Per-variant JSON round-trip =====================================================

  group('JSON round-trip', () {
    test('preserves all six variants + nested positions', () {
      final original = _buildJsonNativeHolder();
      final json = Holder_JsonCodec.instance.encode(BaboonCodecContext.compact, original);
      final decoded = Holder_JsonCodec.instance.decode(BaboonCodecContext.compact, json);
      expect(decoded, equals(original));
    });

    test('JSON decode yields AnyOpaqueJson branch with matching kind bytes', () {
      final original = _buildJsonNativeHolder();
      final json = Holder_JsonCodec.instance.encode(BaboonCodecContext.compact, original);
      final decoded = Holder_JsonCodec.instance.decode(BaboonCodecContext.compact, json);
      expect(decoded.fAny, isA<AnyOpaqueJson>());
      expect(decoded.fAny.meta.kind, 0x07);
      expect(decoded.fDomainThis.meta.kind, 0x03);
      expect(decoded.fDomainCurrent.meta.kind, 0x01);
      expect(decoded.fUnderlying.meta.kind, 0x06);
      expect(decoded.fThisUnderlying.meta.kind, 0x02);
      expect(decoded.fCurrentUnderlying.meta.kind, 0x00);
    });
  });

  // ===== 3. Cross-format conversion via facade ==============================================

  group('Cross-format conversion via facade', () {
    test('JSON-branch Holder encodes to UEBA bytes and round-trips identically', () {
      // _buildJsonHolderForCrossConvert uses AnyOpaqueJson branches for all fields with real Inner
      // JSON; encoding to UEBA forces jsonToUebaBytes per field. After decode the branches are
      // AnyOpaqueUeba. Re-encoding the now-UEBA-branched holder (no facade) must produce
      // identical bytes — proves the cross-converted bytes match a native UEBA encode of the same
      // value.
      final facade = _freshFacade();
      final ctxWithFacade = BaboonCodecContext.withFacade(false, facade);
      final original = _buildJsonHolderForCrossConvert();
      final bytes = _encodeUebaBytes(original, ctxWithFacade);
      final decoded = _decodeUebaBytes(bytes);

      final rebytes = _encodeUebaBytes(decoded, BaboonCodecContext.compact);
      expect(bytes, equals(rebytes));
    });

    test('UEBA-branch Holder encodes to JSON envelope (variant kinds preserved)', () {
      // _buildUebaHolder uses AnyOpaqueUeba branches everywhere; encoding to JSON triggers
      // uebaToJson for each field. For untyped variants A/B/C the wire meta carries typeid;
      // we substitute typeid=INNER_TYPE so the registered Inner codec resolves and the bytes
      // deserialize as Inner. D variants resolve via static fallbacks emitted by codec gen.
      final facade = _freshFacade();
      final ctxWithFacade = BaboonCodecContext.withFacade(false, facade);
      final innerBytes = _innerToUebaBytes(SAMPLE_INNER);
      final base = _buildUebaHolder();
      final crossable = Holder(
        fAny: AnyOpaqueUeba(metaA(INNER_TYPE), innerBytes),
        fDomainThis: AnyOpaqueUeba(metaB(INNER_TYPE), innerBytes),
        fDomainCurrent: AnyOpaqueUeba(metaC(INNER_TYPE), innerBytes),
        fUnderlying: base.fUnderlying,
        fThisUnderlying: base.fThisUnderlying,
        fCurrentUnderlying: base.fCurrentUnderlying,
        fOpt: AnyOpaqueUeba(metaA(INNER_TYPE), innerBytes),
        fLst: base.fLst,
        fMapValue: {'k1': AnyOpaqueUeba(metaA(INNER_TYPE), innerBytes)},
      );
      final json = Holder_JsonCodec.instance.encode(ctxWithFacade, crossable);
      // Sanity-decode the JSON to ensure the envelope is well-formed.
      final decoded = Holder_JsonCodec.instance.decode(BaboonCodecContext.compact, json);
      expect(decoded.fAny, isA<AnyOpaqueJson>());
      expect(decoded.fAny.meta.kind, 0x07);
      expect(decoded.fUnderlying.meta.kind, 0x06);
      // D3: kind=0x00, statics filled
      expect(decoded.fCurrentUnderlying.meta.kind, 0x00);
    });

    test('D3 isolated field cross-converts via static fallbacks (PR-06-D01 Dart analog)', () {
      // PR-06-D01 (Dart analog) regression: D3 has all-None meta on wire; the codec generator
      // emits (currentDomain, currentVersion, underlyingFqid) as static fallbacks. Without these
      // the facade cannot resolve and cross-convert fails.
      final facade = _freshFacade();
      final ctxWithFacade = BaboonCodecContext.withFacade(false, facade);
      final innerJson = _innerToJson(SAMPLE_INNER);
      final base = _buildUebaHolder();
      final mixed = Holder(
        fAny: base.fAny,
        fDomainThis: base.fDomainThis,
        fDomainCurrent: base.fDomainCurrent,
        fUnderlying: base.fUnderlying,
        fThisUnderlying: base.fThisUnderlying,
        fCurrentUnderlying: AnyOpaqueJson(metaD3(), innerJson),
        fOpt: base.fOpt,
        fLst: base.fLst,
        fMapValue: base.fMapValue,
      );
      // No throw on encode means jsonToUebaBytes succeeded for the D3 field (statics resolved).
      final bytes = _encodeUebaBytes(mixed, ctxWithFacade);
      final decoded = _decodeUebaBytes(bytes);
      expect(decoded.fCurrentUnderlying.meta.kind, 0x00);
      expect(decoded.fCurrentUnderlying, isA<AnyOpaqueUeba>());
      final blob = (decoded.fCurrentUnderlying as AnyOpaqueUeba).bytes;
      final inner = Inner_UebaCodec.instance.decode(BaboonCodecContext.compact, BaboonBinReader(blob));
      expect(inner, equals(SAMPLE_INNER));
    });
  });

  // ===== 4. facade.decodeAny end-to-end =====================================================

  group('facade.decodeAny', () {
    test('resolves UEBA Inner payload to typed Inner', () {
      final facade = _freshFacade();
      final meta = AnyMeta(0x07, DOMAIN_ID, VERSION_STR, INNER_TYPE);
      final opaque = AnyOpaqueUeba(meta, _innerToUebaBytes(SAMPLE_INNER));
      final result = facade.decodeAny(opaque);
      expect(result, isA<BaboonRight>());
      expect((result as BaboonRight).value, equals(SAMPLE_INNER));
    });

    test('resolves JSON Inner payload to typed Inner', () {
      final facade = _freshFacade();
      final meta = AnyMeta(0x07, DOMAIN_ID, VERSION_STR, INNER_TYPE);
      final opaque = AnyOpaqueJson(meta, _innerToJson(SAMPLE_INNER));
      final result = facade.decodeAny(opaque);
      expect(result, isA<BaboonRight>());
      expect((result as BaboonRight).value, equals(SAMPLE_INNER));
    });
  });

  // ===== 5. Forward-compat: trailing meta-extension bytes inside meta-length window =========

  group('Forward-compat', () {
    test('extra meta-extension bytes are skipped on UEBA decode', () {
      // Encode a Holder normally, then surgically patch the FIRST any-field's meta-length to
      // claim 5 extra bytes, and splice 5 bytes into the meta block. The decoder must consume
      // the meta, observe the gap (anyMetaLen - bytesRead), skip them, and continue parsing.
      final original = _buildUebaHolder();
      final bytes = _encodeUebaBytes(original, BaboonCodecContext.compact);

      // Layout of the first any-field on the wire (Compact, useIndices=false):
      // [1 byte header][i32 anyLength][i32 anyMetaLen][... metaBytes ...][... blob ...]
      const headerLen = 1;
      const anyLengthOffset = headerLen;
      const anyMetaLenOffset = headerLen + 4;
      const anyMetaStartOffset = headerLen + 4 + 4;
      final origAnyLength = _readI32Le(bytes, anyLengthOffset);
      final origAnyMetaLen = _readI32Le(bytes, anyMetaLenOffset);

      final extension = Uint8List.fromList([0x11, 0x22, 0x33, 0x44, 0x55]);
      final newAnyMetaLen = origAnyMetaLen + extension.length;
      final newAnyLength = origAnyLength + extension.length;

      final origMetaSlice = Uint8List.sublistView(bytes, anyMetaStartOffset, anyMetaStartOffset + origAnyMetaLen);
      final origBlobAndRest = Uint8List.sublistView(bytes, anyMetaStartOffset + origAnyMetaLen);

      final patched = BaboonBinWriter();
      patched.writeU8(bytes[0]);
      patched.writeI32(newAnyLength);
      patched.writeI32(newAnyMetaLen);
      patched.writeAll(origMetaSlice);
      patched.writeAll(extension);
      patched.writeAll(origBlobAndRest);

      final decoded = _decodeUebaBytes(patched.toBytes());
      expect(decoded, equals(original));
    });
  });

  // ===== 6. Fail-fast: missing-facade cross-convert =========================================

  group('Fail-fast missing-facade', () {
    test('encoding JSON-branch any into UEBA without facade throws', () {
      final base = _buildUebaHolder();
      final mixed = Holder(
        fAny: AnyOpaqueJson(metaA(), {'x': 1}),
        fDomainThis: base.fDomainThis,
        fDomainCurrent: base.fDomainCurrent,
        fUnderlying: base.fUnderlying,
        fThisUnderlying: base.fThisUnderlying,
        fCurrentUnderlying: base.fCurrentUnderlying,
        fOpt: base.fOpt,
        fLst: base.fLst,
        fMapValue: base.fMapValue,
      );
      expect(
        () {
          final w = BaboonBinWriter();
          Holder_UebaCodec.instance.encode(BaboonCodecContext.compact, w, mixed);
        },
        throwsA(isA<BaboonEncoderFailure>()),
      );
    });

    test('encoding UEBA-branch any into JSON without facade throws', () {
      final base = _buildJsonNativeHolder();
      final mixed = Holder(
        fAny: AnyOpaqueUeba(metaA(), Uint8List.fromList([1, 2])),
        fDomainThis: base.fDomainThis,
        fDomainCurrent: base.fDomainCurrent,
        fUnderlying: base.fUnderlying,
        fThisUnderlying: base.fThisUnderlying,
        fCurrentUnderlying: base.fCurrentUnderlying,
        fOpt: base.fOpt,
        fLst: base.fLst,
        fMapValue: base.fMapValue,
      );
      expect(
        () {
          Holder_JsonCodec.instance.encode(BaboonCodecContext.compact, mixed);
        },
        throwsA(isA<BaboonEncoderFailure>()),
      );
    });
  });

  // ===== 7. JSON envelope shape lock-in =====================================================

  group('JSON envelope shape', () {
    test('envelope carries \$ak (+ optional \$ad/\$av/\$at) and \$c content key', () {
      // Sanity: the JSON envelope produced by the codec embeds the AnyMeta keys ($ak, $ad?,
      // $av?, $at?) alongside the $c content key. Any change to the envelope that drops one
      // of these would break cross-language interop.
      final original = _buildJsonNativeHolder();
      final Map<String, dynamic> token = Holder_JsonCodec.instance.encode(BaboonCodecContext.compact, original);

      // fAny variant A → all four meta keys + $c present.
      final anyField = token['fAny'] as Map<String, dynamic>;
      expect(anyField[AnyMetaCodec.ANY_KIND_KEY], 0x07);
      expect(anyField.containsKey(AnyMetaCodec.ANY_DOMAIN_KEY), isTrue);
      expect(anyField.containsKey(AnyMetaCodec.ANY_VERSION_KEY), isTrue);
      expect(anyField.containsKey(AnyMetaCodec.ANY_TYPEID_KEY), isTrue);
      expect(anyField.containsKey(AnyMetaCodec.ANY_CONTENT_KEY), isTrue);

      // fCurrentUnderlying variant D3 → only $ak + $c (kind 0x00, no other meta on wire).
      final d3 = token['fCurrentUnderlying'] as Map<String, dynamic>;
      expect(d3[AnyMetaCodec.ANY_KIND_KEY], 0x00);
      expect(d3.containsKey(AnyMetaCodec.ANY_CONTENT_KEY), isTrue);
      expect(d3.containsKey(AnyMetaCodec.ANY_DOMAIN_KEY), isFalse);
      expect(d3.containsKey(AnyMetaCodec.ANY_VERSION_KEY), isFalse);
      expect(d3.containsKey(AnyMetaCodec.ANY_TYPEID_KEY), isFalse);

      // Sanity: the variant-A envelope keys appear exactly as documented (regression-proof key
      // list).
      final expected = {
        AnyMetaCodec.ANY_KIND_KEY,
        AnyMetaCodec.ANY_DOMAIN_KEY,
        AnyMetaCodec.ANY_VERSION_KEY,
        AnyMetaCodec.ANY_TYPEID_KEY,
        AnyMetaCodec.ANY_CONTENT_KEY,
      };
      expect(anyField.keys.toSet(), equals(expected));
    });
  });
}
