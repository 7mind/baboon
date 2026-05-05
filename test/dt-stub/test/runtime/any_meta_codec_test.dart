// Hand-written runtime tests for `BaboonAnyOpaque` (`AnyMeta` / `AnyMetaCodec`),
// `BaboonCodecsFacade`, and the `BaboonCodecContext` extension. Mirrors the Java
// `AnyMetaCodecTest.java` (PR 6.1, 41 tests) and the TS `AnyMetaCodec.test.ts` (PR 7.1, 54
// tests). Lives under `test/runtime/` rather than at `test/` root so codegen does not stomp it
// (codegen writes per-domain subfolders like `test/pkg0/`).
//
// Defects exercised: PR-04-D01, PR-04-D02, PR-05-D01, PR-05-D08, PR-06-D01, PR-07-D02,
// PR-08-D01, PR-08-D02, PR-12-D01.

import 'dart:typed_data';

import 'package:baboon_runtime/baboon_any_opaque.dart';
import 'package:baboon_runtime/baboon_codecs_facade.dart';
import 'package:baboon_runtime/baboon_runtime.dart';
import 'package:test/test.dart';

Uint8List _writeBin(AnyMeta meta) {
  final w = BaboonBinWriter();
  AnyMetaCodec.writeBin(meta, w);
  return w.toBytes();
}

AnyMeta _readBin(Uint8List bytes) => AnyMetaCodec.readBin(BaboonBinReader(bytes));

(AnyMeta, int) _readBinWithLength(Uint8List bytes) =>
    AnyMetaCodec.readBinWithLength(BaboonBinReader(bytes));

// --- Stub codecs/conversions for facade dispatch tests --------------------------------------

class _StubGenerated implements BaboonGenerated, BaboonMetaProvider {
  @override
  String get baboonDomainVersion => '1.0.0';
  @override
  String get baboonDomainIdentifier => 'dom';
  @override
  String get baboonTypeIdentifier => 'T';
  @override
  List<String> get baboonSameInVersions => ['1.0.0'];
}

class _StubAdtBranchGenerated implements BaboonGenerated, BaboonMetaProvider, BaboonAdtMember {
  @override
  String get baboonAdtTypeIdentifier => 'AdtT';
  @override
  String get baboonDomainVersion => '1.0.0';
  @override
  String get baboonDomainIdentifier => 'dom';
  @override
  String get baboonTypeIdentifier => 'BranchT';
  @override
  List<String> get baboonSameInVersions => ['1.0.0'];
}

class _StubBinCodec extends BaboonBinCodecBase<BaboonGenerated> implements BaboonCodecData {
  @override
  String get baboonDomainVersion => '1.0.0';
  @override
  String get baboonDomainIdentifier => 'dom';
  @override
  String get baboonTypeIdentifier => 'T';
  @override
  void encode(BaboonCodecContext ctx, BaboonBinWriter writer, BaboonGenerated value) {
    writer.writeU8(0x42);
  }
  @override
  BaboonGenerated decode(BaboonCodecContext ctx, BaboonBinReader reader) {
    reader.readU8();
    return _StubGenerated();
  }
}

class _StubJsonCodec extends BaboonJsonCodecBase<BaboonGenerated> implements BaboonCodecData {
  @override
  String get baboonDomainVersion => '1.0.0';
  @override
  String get baboonDomainIdentifier => 'dom';
  @override
  String get baboonTypeIdentifier => 'T';
  @override
  Object? encode(BaboonCodecContext ctx, BaboonGenerated value) => 'ok';
  @override
  BaboonGenerated decode(BaboonCodecContext ctx, Object? wire) => _StubGenerated();
}

class _StubBinCodecBranch extends BaboonBinCodecBase<BaboonGenerated> implements BaboonCodecData {
  final String typeId;
  _StubBinCodecBranch(this.typeId);
  @override
  String get baboonDomainVersion => '1.0.0';
  @override
  String get baboonDomainIdentifier => 'dom';
  @override
  String get baboonTypeIdentifier => typeId;
  @override
  void encode(BaboonCodecContext ctx, BaboonBinWriter writer, BaboonGenerated value) {
    writer.writeU8(0x42);
  }
  @override
  BaboonGenerated decode(BaboonCodecContext ctx, BaboonBinReader reader) {
    reader.readU8();
    return _StubAdtBranchGenerated();
  }
}

class _StubJsonCodecBranch extends BaboonJsonCodecBase<BaboonGenerated> implements BaboonCodecData {
  final String typeId;
  _StubJsonCodecBranch(this.typeId);
  @override
  String get baboonDomainVersion => '1.0.0';
  @override
  String get baboonDomainIdentifier => 'dom';
  @override
  String get baboonTypeIdentifier => typeId;
  @override
  Object? encode(BaboonCodecContext ctx, BaboonGenerated value) => 'ok';
  @override
  BaboonGenerated decode(BaboonCodecContext ctx, Object? wire) => _StubAdtBranchGenerated();
}

class _StubMeta extends BaboonMeta {
  final List<String> _versions;
  _StubMeta(this._versions);
  @override
  List<String> sameInVersions(String typeId) => _versions;
}

class _StubConversions extends AbstractBaboonConversions {
  @override
  List<String> get versionsFrom => [];
  @override
  String get versionTo => '1.0.0';
}

void main() {
  // ===== AnyMeta invariants ===================================================================

  group('AnyMeta invariants', () {
    test('rejects bit-mismatch domain (kind 0x07, domain null)', () {
      // PR-04-D01: validator enforces all four rules at construction.
      expect(() => AnyMeta(0x07, null, '1.0.0', 'T'), throwsArgumentError);
    });

    test('rejects bit-mismatch version (kind 0x03, version null)', () {
      expect(() => AnyMeta(0x03, null, null, 'T'), throwsArgumentError);
    });

    test('rejects bit-mismatch typeid (kind 0x01, typeid null)', () {
      expect(() => AnyMeta(0x01, null, null, null), throwsArgumentError);
    });

    test('rejects extra domain without bit (kind 0x00, domain set)', () {
      expect(() => AnyMeta(0x00, 'd', null, null), throwsArgumentError);
    });

    test('rejects reserved kind 0x04 (PR-04-D01)', () {
      expect(() => AnyMeta(0x04, 'd', null, null), throwsArgumentError);
    });

    test('rejects reserved kind 0x05 (PR-04-D01)', () {
      expect(() => AnyMeta(0x05, 'd', null, 'T'), throwsArgumentError);
    });

    test('rejects out-of-range kind (PR-12-D01: u8 sanity check)', () {
      expect(() => AnyMeta(-1, null, null, null), throwsArgumentError);
      expect(() => AnyMeta(0x100, null, null, null), throwsArgumentError);
    });
  });

  // ===== Binary round-trip per kind ===========================================================

  group('AnyMetaCodec binary round-trip', () {
    test('kind A (0x07) full triple', () {
      final meta = AnyMeta(0x07, 'dom', '1.0.0', 'Type');
      expect(_readBin(_writeBin(meta)), meta);
    });
    test('kind B (0x03) version+typeid', () {
      final meta = AnyMeta(0x03, null, '1.0.0', 'Type');
      expect(_readBin(_writeBin(meta)), meta);
    });
    test('kind C (0x01) typeid only', () {
      final meta = AnyMeta(0x01, null, null, 'Type');
      expect(_readBin(_writeBin(meta)), meta);
    });
    test('kind D1 (0x06) domain+version', () {
      final meta = AnyMeta(0x06, 'dom', '1.0.0', null);
      expect(_readBin(_writeBin(meta)), meta);
    });
    test('kind D2 (0x02) version only', () {
      final meta = AnyMeta(0x02, null, '1.0.0', null);
      expect(_readBin(_writeBin(meta)), meta);
    });
    test('kind D3 (0x00) bare', () {
      final meta = AnyMeta(0x00, null, null, null);
      expect(_readBin(_writeBin(meta)), meta);
    });
  });

  // ===== Binary edge cases ====================================================================

  group('AnyMetaCodec binary edge cases', () {
    test('empty strings', () {
      final meta = AnyMeta(0x07, '', '', '');
      expect(_readBin(_writeBin(meta)), meta);
    });

    test('non-ASCII UTF-8', () {
      final meta = AnyMeta(0x07, 'δωμάιν', '1.0.0', 'τύπος');
      expect(_readBin(_writeBin(meta)), meta);
    });

    test('128-byte string triggers multi-byte ULEB128 length', () {
      final big = 'x' * 128;
      final meta = AnyMeta(0x07, big, '1.0.0', 'T');
      expect(_readBin(_writeBin(meta)), meta);
    });

    test('readBinWithLength reports exact bytes consumed (PR-05-D01)', () {
      final meta = AnyMeta(0x07, 'dom', '1.0.0', 'Type');
      final base = _writeBin(meta);
      final padded = Uint8List(base.length + 5);
      padded.setRange(0, base.length, base);
      padded.setRange(base.length, base.length + 5, [0xAA, 0xBB, 0xCC, 0xDD, 0xEE]);

      final (decoded, n) = _readBinWithLength(padded);
      expect(decoded, meta);
      expect(n, base.length);
    });
  });

  // ===== JSON round-trip ======================================================================

  group('AnyMetaCodec JSON round-trip', () {
    test('all six kinds round-trip via writeJson/readJson', () {
      final all = <AnyMeta>[
        AnyMeta(0x07, 'd', '1.0.0', 'T'),
        AnyMeta(0x03, null, '1.0.0', 'T'),
        AnyMeta(0x01, null, null, 'T'),
        AnyMeta(0x06, 'd', '1.0.0', null),
        AnyMeta(0x02, null, '1.0.0', null),
        AnyMeta(0x00, null, null, null),
      ];
      for (final meta in all) {
        final wire = AnyMetaCodec.writeJson(meta);
        final result = AnyMetaCodec.readJson(wire);
        expect(result, isA<BaboonRight<BaboonCodecException, AnyMeta>>());
        expect((result as BaboonRight).value, meta);
      }
    });

    test('readJson Left on non-object input (PR-04-D02)', () {
      expect(AnyMetaCodec.readJson('not-an-object'), isA<BaboonLeft>());
      expect(AnyMetaCodec.readJson(null), isA<BaboonLeft>());
      expect(AnyMetaCodec.readJson([]), isA<BaboonLeft>());
    });

    test('readJson Left on missing kind', () {
      final result = AnyMetaCodec.readJson({AnyMetaCodec.ANY_DOMAIN_KEY: 'dom'});
      expect(result, isA<BaboonLeft>());
    });

    test('readJson Left on non-numeric kind', () {
      final result = AnyMetaCodec.readJson({AnyMetaCodec.ANY_KIND_KEY: 'seven'});
      expect(result, isA<BaboonLeft>());
    });

    test('readJson Left on incomplete-for-kind (0x07 + only typeid)', () {
      final result = AnyMetaCodec.readJson({
        AnyMetaCodec.ANY_KIND_KEY: 0x07,
        AnyMetaCodec.ANY_TYPEID_KEY: 'T',
      });
      expect(result, isA<BaboonLeft>());
    });

    test('readJson Left on forbidden-for-kind (0x00 + domain present)', () {
      final result = AnyMetaCodec.readJson({
        AnyMetaCodec.ANY_KIND_KEY: 0x00,
        AnyMetaCodec.ANY_DOMAIN_KEY: 'dom',
      });
      expect(result, isA<BaboonLeft>());
    });

    test('writeJson always returns plain Map<String, dynamic>', () {
      final wire = AnyMetaCodec.writeJson(AnyMeta(0x00, null, null, null));
      expect(wire, isA<Map<String, dynamic>>());
      expect(wire[AnyMetaCodec.ANY_KIND_KEY], 0);
    });

    test('JSON envelope keys are dollar-prefixed literals (PR-19-D01)', () {
      // The embedSources macro reads the file verbatim — Dart string interpolation must NOT
      // turn `$ak` into a variable lookup. We use raw strings so the literal `$` survives.
      expect(AnyMetaCodec.ANY_KIND_KEY, r'$ak');
      expect(AnyMetaCodec.ANY_DOMAIN_KEY, r'$ad');
      expect(AnyMetaCodec.ANY_VERSION_KEY, r'$av');
      expect(AnyMetaCodec.ANY_TYPEID_KEY, r'$at');
      expect(AnyMetaCodec.ANY_CONTENT_KEY, r'$c');
    });
  });

  // ===== AnyOpaque content equality (PR-05-D08) ===============================================

  group('AnyOpaque content equality', () {
    test('AnyOpaqueUeba equals by content (Uint8List == is reference identity)', () {
      final meta = AnyMeta(0x00, null, null, null);
      final x = AnyOpaqueUeba(meta, Uint8List.fromList([1, 2, 3]));
      final y = AnyOpaqueUeba(meta, Uint8List.fromList([1, 2, 3]));
      expect(x == y, isTrue);
      expect(x.hashCode, y.hashCode);

      final z = AnyOpaqueUeba(meta, Uint8List.fromList([1, 2, 4]));
      expect(x == z, isFalse);
    });

    test('AnyOpaqueJson equals by content (deep equal)', () {
      final meta = AnyMeta(0x00, null, null, null);
      final x = AnyOpaqueJson(meta, {'a': 1, 'b': 'z'});
      final y = AnyOpaqueJson(meta, {'a': 1, 'b': 'z'});
      expect(x == y, isTrue);

      final z = AnyOpaqueJson(meta, {'a': 2, 'b': 'z'});
      expect(x == z, isFalse);
    });

    test('AnyOpaqueUeba and AnyOpaqueJson are not equal even with same meta', () {
      final meta = AnyMeta(0x00, null, null, null);
      final ueba = AnyOpaqueUeba(meta, Uint8List(0));
      final json = AnyOpaqueJson(meta, null);
      // Sealed-class branches: they're never equal across variants.
      expect(ueba == (json as Object), isFalse);
    });
  });

  // ===== BaboonCodecContext extension =========================================================

  group('BaboonCodecContext withFacade', () {
    test('compact has null facade and useIndices=false', () {
      expect(BaboonCodecContext.compact.facade, isNull);
      expect(BaboonCodecContext.compact.useIndices, isFalse);
    });

    test('indexed has null facade and useIndices=true', () {
      expect(BaboonCodecContext.indexed.facade, isNull);
      expect(BaboonCodecContext.indexed.useIndices, isTrue);
    });

    test('withFacade exposes the supplied facade', () {
      final facade = BaboonCodecsFacade();
      final ctx = BaboonCodecContext.withFacade(true, facade);
      expect(ctx.facade, same(facade));
      expect(ctx.useIndices, isTrue);
    });
  });

  // ===== Cross-format helpers (no codecs registered) ==========================================

  group('BaboonCodecsFacade cross-format helpers (incomplete meta / no codec)', () {
    test('jsonToUebaBytes returns Left on incomplete meta (PR-06-D01)', () {
      final facade = BaboonCodecsFacade();
      final meta = AnyMeta(0x00, null, null, null);
      final r = facade.jsonToUebaBytes(meta, {});
      expect(r, isA<BaboonLeft>());
    });

    test('uebaToJson returns Left on incomplete meta', () {
      final facade = BaboonCodecsFacade();
      final meta = AnyMeta(0x00, null, null, null);
      final r = facade.uebaToJson(meta, Uint8List.fromList([1, 2, 3]));
      expect(r, isA<BaboonLeft>());
    });

    test('jsonToUebaBytes returns Left on no codec', () {
      final facade = BaboonCodecsFacade();
      final meta = AnyMeta(0x07, 'dom', '1.0.0', 'T');
      final r = facade.jsonToUebaBytes(meta, {});
      expect(r, isA<BaboonLeft>());
      expect((r as BaboonLeft).value, isA<BaboonCodecNotFound>());
    });

    test('uebaToJson returns Left on no codec', () {
      final facade = BaboonCodecsFacade();
      final meta = AnyMeta(0x07, 'dom', '1.0.0', 'T');
      final r = facade.uebaToJson(meta, Uint8List.fromList([0]));
      expect(r, isA<BaboonLeft>());
      expect((r as BaboonLeft).value, isA<BaboonCodecNotFound>());
    });

    test('decodeAny returns Left on incomplete meta', () {
      final facade = BaboonCodecsFacade();
      final meta = AnyMeta(0x00, null, null, null);
      final opaque = AnyOpaqueUeba(meta, Uint8List.fromList([0]));
      final r = facade.decodeAny(opaque);
      expect(r, isA<BaboonLeft>());
    });
  });

  // ===== Static-fallback semantics (PR-06-D01) ================================================

  group('Static-fallback semantics', () {
    test('kind 0x00 with statics resolves past meta-validation', () {
      final facade = BaboonCodecsFacade();
      final meta = AnyMeta(0x00, null, null, null);
      final r = facade.jsonToUebaBytes(
        meta,
        {},
        staticDomain: 'dom',
        staticVersion: '1.0.0',
        staticTypeid: 'T',
      );
      // Failure must be CodecNotFound (synthesised meta is complete) rather than
      // "AnyMeta requires ..." (which would mean fallback didn't fire).
      expect(r, isA<BaboonLeft>());
      expect((r as BaboonLeft).value, isA<BaboonCodecNotFound>());
    });

    test('wire meta domain overrides static (wire wins)', () {
      final facade = BaboonCodecsFacade();
      final meta = AnyMeta(0x07, 'dom-wire', '1.0.0', 'T');
      final r = facade.jsonToUebaBytes(
        meta,
        {},
        staticDomain: 'dom-static',
        staticVersion: '0.9.0',
        staticTypeid: 'Other',
      );
      expect(r, isA<BaboonLeft>());
      final msg = (r as BaboonLeft).value.message;
      expect(msg, contains('dom-wire'));
    });
  });

  // ===== Single-version-domain regression (PR-07-D02) =========================================

  group('getCodec single-version-domain (PR-07-D02)', () {
    test('uebaToJson succeeds when only a single version is registered', () {
      final facade = BaboonCodecsFacade();
      final dv = BaboonDomainVersion('dom', '1.0.0');
      final binCodecs = AbstractBaboonUebaCodecs();
      binCodecs.register('T', () => _StubBinCodec());
      final jsonCodecs = AbstractBaboonJsonCodecs();
      jsonCodecs.register('T', () => _StubJsonCodec());
      final meta = _StubMeta(['1.0.0']);
      final conv = _StubConversions();

      facade.register(
        dv,
        codecsJson: () => jsonCodecs,
        codecsBin: () => binCodecs,
        conversions: () => conv,
        meta: () => meta,
      );

      final any = AnyMeta(0x07, 'dom', '1.0.0', 'T');
      final payload = Uint8List.fromList([0x42]);
      final r = facade.uebaToJson(any, payload);
      expect(r, isA<BaboonRight>());
    });

    test('verify rejects empty registry (PR-08-D02 fail-fast analog)', () {
      final facade = BaboonCodecsFacade();
      expect(() => facade.verify(), throwsA(isA<BaboonException>()));
    });
  });

  // ===== encodeToBin / encodeToJson useAdtIdentifier plumbing (PR-19-D02) =====================

  group('encodeToBin / encodeToJson useAdtIdentifier plumbing', () {
    BaboonCodecsFacade makeFacade() {
      final facade = BaboonCodecsFacade();
      final dv = BaboonDomainVersion('dom', '1.0.0');
      final binCodecs = AbstractBaboonUebaCodecs();
      binCodecs.register('BranchT', () => _StubBinCodecBranch('BranchT'));
      binCodecs.register('AdtT', () => _StubBinCodecBranch('AdtT'));
      final jsonCodecs = AbstractBaboonJsonCodecs();
      jsonCodecs.register('BranchT', () => _StubJsonCodecBranch('BranchT'));
      jsonCodecs.register('AdtT', () => _StubJsonCodecBranch('AdtT'));
      final meta = _StubMeta(['1.0.0']);
      final conv = _StubConversions();
      facade.register(
        dv,
        codecsJson: () => jsonCodecs,
        codecsBin: () => binCodecs,
        conversions: () => conv,
        meta: () => meta,
      );
      return facade;
    }

    test('encodeToBin defaults to concrete branch typeid', () {
      final facade = makeFacade();
      final r = facade.encodeToBin(BaboonCodecContext.compact, _StubAdtBranchGenerated());
      expect(r, isA<BaboonRight>());
      final bytes = (r as BaboonRight).value as Uint8List;
      final meta = BaboonTypeMeta.readMetaBin(BaboonBinReader(bytes));
      expect(meta, isNotNull);
      expect(meta!.typeIdentifier, 'BranchT');
    });

    test('encodeToBin with useAdtIdentifier=true uses the ADT typeid', () {
      final facade = makeFacade();
      final r = facade.encodeToBin(
        BaboonCodecContext.compact,
        _StubAdtBranchGenerated(),
        useAdtIdentifier: true,
      );
      expect(r, isA<BaboonRight>());
      final bytes = (r as BaboonRight).value as Uint8List;
      final meta = BaboonTypeMeta.readMetaBin(BaboonBinReader(bytes));
      expect(meta, isNotNull);
      expect(meta!.typeIdentifier, 'AdtT');
    });

    test('encodeToJson defaults to concrete branch typeid', () {
      final facade = makeFacade();
      final r = facade.encodeToJson(_StubAdtBranchGenerated());
      expect(r, isA<BaboonRight>());
      final wire = (r as BaboonRight).value as Map<String, dynamic>;
      expect(wire[r'$t'], 'BranchT');
    });

    test('encodeToJson with useAdtIdentifier=true uses the ADT typeid', () {
      final facade = makeFacade();
      final r = facade.encodeToJson(_StubAdtBranchGenerated(), useAdtIdentifier: true);
      expect(r, isA<BaboonRight>());
      final wire = (r as BaboonRight).value as Map<String, dynamic>;
      expect(wire[r'$t'], 'AdtT');
    });
  });

  // ===== BaboonTypeMeta JSON $mv handling (PR-08-D01) =========================================

  group('BaboonTypeMeta.readMetaJson \$mv handling', () {
    test('accepts absent \$mv (defaults to v1)', () {
      final meta = BaboonTypeMeta.readMetaJson({r'$d': 'dom', r'$v': '1.0.0', r'$t': 'T'});
      expect(meta, isNotNull);
      expect(meta!.domainIdentifier, 'dom');
    });

    test('accepts explicit \$mv="16"', () {
      final meta = BaboonTypeMeta.readMetaJson({
        r'$mv': '16', r'$d': 'dom', r'$v': '1.0.0', r'$t': 'T',
      });
      expect(meta, isNotNull);
    });

    test('rejects numeric \$mv=1 (PR-22-D01: spec says string-only; Java/TS enforce)', () {
      // Cross-runtime parity: $mv must be a JSON string. Java/TS reject numerics; Dart now matches.
      final meta = BaboonTypeMeta.readMetaJson({
        r'$mv': 1, r'$d': 'dom', r'$v': '1.0.0', r'$t': 'T',
      });
      expect(meta, isNull);
    });

    test('rejects \$mv="2" (forward-compat: future meta versions return null)', () {
      final meta = BaboonTypeMeta.readMetaJson({
        r'$mv': '2', r'$d': 'dom', r'$v': '1.0.0', r'$t': 'T',
      });
      expect(meta, isNull);
    });
  });

  // ===== BaboonVersion sanity =================================================================

  group('BaboonVersion', () {
    test('parses x.y.z and compares numerically', () {
      final a = BaboonVersion.from('1.2.3');
      final b = BaboonVersion.from('1.2.4');
      expect(a.compareTo(b), lessThan(0));
      expect(b.compareTo(a), greaterThan(0));
      expect(a.compareTo(BaboonVersion.from('1.2.3')), 0);
    });

    test('rejects non-x.y.z input', () {
      expect(() => BaboonVersion.from('1.2'), throwsA(isA<BaboonException>()));
      expect(() => BaboonVersion.from('a.b.c'), throwsA(isA<BaboonException>()));
    });
  });

  // ===== Typed exception kind discrimination ==================================================

  group('BaboonCodecException hierarchy', () {
    test('subclasses are distinguishable via runtimeType / pattern matching', () {
      final BaboonCodecException dec = BaboonDecoderFailure('x');
      expect(dec, isA<BaboonDecoderFailure>());
      expect(dec, isA<BaboonCodecException>());

      final BaboonCodecException nf = BaboonCodecNotFound('y');
      expect(nf, isA<BaboonCodecNotFound>());
      expect(nf, isNot(isA<BaboonDecoderFailure>()));
    });
  });
}
