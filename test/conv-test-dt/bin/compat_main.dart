import 'dart:convert';
import 'dart:io';
import 'dart:typed_data';

import 'package:baboon_runtime/baboon_any_opaque.dart';
import 'package:baboon_runtime/baboon_codecs_facade.dart';
import 'package:baboon_runtime/baboon_runtime.dart';
import 'package:conv_test_dt/generated/convtest/testpkg/all_basic_types.dart';
import 'package:conv_test_dt/generated/convtest/testpkg/any_showcase.dart';
import 'package:conv_test_dt/generated/convtest/testpkg/baboon_codecs_json.dart';
import 'package:conv_test_dt/generated/convtest/testpkg/baboon_codecs_ueba.dart';
import 'package:conv_test_dt/generated/convtest/testpkg/inner_payload.dart';
import 'package:conv_test_dt/generated/convtest/testpkg/composite_id.dart';
import 'package:conv_test_dt/generated/convtest/testpkg/item_id.dart';
import 'package:conv_test_dt/generated/convtest/testpkg/point_id.dart';
import 'package:conv_test_dt/generated/convtest/testpkg/wire_enum.dart';
// PR-I.1d (M24 Phase 3.1) — Custom-foreign KeyCodec hook fixture. Stringy
// FStr foreign + ItemKey wrapper + ForeignKeyHolder round-trip exercises the
// generated FStr_KeyCodecHost identity default impl.
import 'package:conv_test_dt/generated/convtest/m24foreign/foreign_key_holder.dart';
import 'package:conv_test_dt/generated/convtest/m24foreign/item_key.dart';
// PR-26.5 (M26) — non-string builtin map-key cross-language fixture.
import 'package:conv_test_dt/generated/convtest/m26builtinkeys/builtin_map_key_holder.dart';
// PR-29.10 (M29) — monomorphised-template cross-language wire-format fixture.
import 'package:conv_test_dt/generated/convtest/m29ok/m29_ok_holder.dart';
import 'package:conv_test_dt/generated/convtest/m29ok/int_page.dart';
import 'package:conv_test_dt/generated/convtest/m29ok/str_page.dart';
import 'package:conv_test_dt/generated/convtest/m29ok/item.dart';
import 'package:conv_test_dt/generated/convtest/m29ok/item_page.dart';
import 'package:conv_test_dt/generated/convtest/m29ok/int_str_envelope.dart';

const String _domainId = 'convtest.testpkg';
const String _domainVer = '2.0.0';
const String _innerTypeId = 'convtest.testpkg/:#InnerPayload';

List<InnerPayload> _expectedInnerPayloads() => [
      InnerPayload(label: 'variant-A', count: 1),
      InnerPayload(label: 'variant-B', count: 2),
      InnerPayload(label: 'variant-C', count: 3),
      InnerPayload(label: 'variant-D1', count: 4),
      InnerPayload(label: 'variant-D2', count: 5),
      InnerPayload(label: 'variant-D3', count: 6),
      InnerPayload(label: 'opt-any', count: 7),
      InnerPayload(label: 'lst-any-0', count: 8),
    ];

Uint8List _uebaBytes(InnerPayload p) {
  final w = BaboonBinWriter();
  InnerPayload_UebaCodec.instance.encode(BaboonCodecContext.compact, w, p);
  return w.toBytes();
}

Object? _asJson(InnerPayload p) =>
    InnerPayload_JsonCodec.instance.encode(BaboonCodecContext.compact, p);

// Facade with convtest.testpkg/2.0.0 codecs registered so the cross-format encoder can resolve
// `(domain, version, typeid)` triples for jsonToUebaBytes / uebaToJson conversions.
BaboonCodecsFacade _freshFacade() {
  final facade = BaboonCodecsFacade();
  facade.registerCodecs(
    BaboonDomainVersion(_domainId, _domainVer),
    codecsJson: () => BaboonCodecsJson(),
    codecsBin: () => BaboonCodecsUeba(),
  );
  return facade;
}

// Canonical AnyShowcase fixture mirroring Scala/C#/Java: A/B/C as AnyOpaqueJson, D1/D2/D3 as
// AnyOpaqueUeba, optAny on the JSON branch, lstAny on the UEBA branch. Each wire format encoder
// will cross-convert the off-branch payloads via the facade — exercising both directions.
AnyShowcase _createSampleAnyShowcase() {
  final p = _expectedInnerPayloads();
  return AnyShowcase(
    vAnyA: AnyOpaqueJson(AnyMeta(0x07, _domainId, _domainVer, _innerTypeId), _asJson(p[0])),
    vAnyB: AnyOpaqueJson(AnyMeta(0x03, null, _domainVer, _innerTypeId), _asJson(p[1])),
    vAnyC: AnyOpaqueJson(AnyMeta(0x01, null, null, _innerTypeId), _asJson(p[2])),
    vAnyD1: AnyOpaqueUeba(AnyMeta(0x06, _domainId, _domainVer, null), _uebaBytes(p[3])),
    vAnyD2: AnyOpaqueUeba(AnyMeta(0x02, null, _domainVer, null), _uebaBytes(p[4])),
    vAnyD3: AnyOpaqueUeba(AnyMeta(0x00, null, null, null), _uebaBytes(p[5])),
    optAny: AnyOpaqueJson(AnyMeta(0x07, _domainId, _domainVer, _innerTypeId), _asJson(p[6])),
    lstAny: [AnyOpaqueUeba(AnyMeta(0x06, _domainId, _domainVer, null), _uebaBytes(p[7]))],
  );
}

void writeJsonAny(BaboonCodecContext ctx, AnyShowcase data, String outputDir) {
  Directory(outputDir).createSync(recursive: true);
  final json = AnyShowcase_JsonCodec.instance.encode(ctx, data);
  final jsonStr = jsonEncode(json);
  final f = File('$outputDir/any-showcase.json');
  f.writeAsStringSync(jsonStr);
  print('Written JSON to ${f.absolute.path}');
}

void writeUebaAny(BaboonCodecContext ctx, AnyShowcase data, String outputDir) {
  Directory(outputDir).createSync(recursive: true);
  final w = BaboonBinWriter();
  AnyShowcase_UebaCodec.instance.encode(ctx, w, data);
  final bytes = w.toBytes();
  final f = File('$outputDir/any-showcase.ueba');
  f.writeAsBytesSync(bytes);
  print('Written UEBA to ${f.absolute.path}');
}

InnerPayload _decodeInner(AnyOpaque o) {
  if (o is AnyOpaqueUeba) {
    final r = BaboonBinReader(o.bytes);
    return InnerPayload_UebaCodec.instance.decode(BaboonCodecContext.compact, r);
  }
  if (o is AnyOpaqueJson) {
    return InnerPayload_JsonCodec.instance.decode(BaboonCodecContext.compact, o.json);
  }
  throw StateError('unexpected AnyOpaque subclass: ${o.runtimeType}');
}

List<InnerPayload> _decodeAllPayloads(AnyShowcase v) {
  final opt = v.optAny;
  if (opt == null) throw StateError('optAny was null; expected non-null');
  if (v.lstAny.isEmpty) throw StateError('lstAny was empty; expected one element');
  return [
    _decodeInner(v.vAnyA),
    _decodeInner(v.vAnyB),
    _decodeInner(v.vAnyC),
    _decodeInner(v.vAnyD1),
    _decodeInner(v.vAnyD2),
    _decodeInner(v.vAnyD3),
    _decodeInner(opt),
    _decodeInner(v.lstAny.first),
  ];
}

void readAndVerifyAnyShowcase(String filePath) {
  final ctx = BaboonCodecContext.defaultCtx;
  AnyShowcase data;
  try {
    if (filePath.endsWith('.json')) {
      final jsonStr = File(filePath).readAsStringSync();
      final json = jsonDecode(jsonStr);
      data = AnyShowcase_JsonCodec.instance.decode(ctx, json);
    } else {
      final bytes = File(filePath).readAsBytesSync();
      final r = BaboonBinReader(Uint8List.fromList(bytes));
      data = AnyShowcase_UebaCodec.instance.decode(ctx, r);
    }
  } catch (e) {
    stderr.writeln('AnyShowcase deserialization failed: $e');
    exit(1);
  }
  try {
    final expected = _expectedInnerPayloads();
    final decoded = _decodeAllPayloads(data);
    for (var i = 0; i < expected.length; i++) {
      if (expected[i] != decoded[i]) {
        stderr.writeln('AnyShowcase payload $i mismatch: expected ${expected[i]}, got ${decoded[i]}');
        exit(1);
      }
    }
  } catch (e) {
    stderr.writeln('AnyShowcase decode failed: $e');
    exit(1);
  }
  print('OK');
}

AllBasicTypes createSampleData() {
  return AllBasicTypes(
    vi8: 42,
    vi16: 1234,
    vi32: 123456,
    vi64: 123456789,
    vu8: 200,
    vu16: 50000,
    vu32: 3000000000,
    vu64: 10000000000,
    vf32: 3.14159,
    vf64: 2.718281828,
    vf128: BaboonDecimal('123456789.987654321'),
    vstr: 'Hello, Baboon!',
    vbstr: Uint8List.fromList(
        [0x48, 0x65, 0x6C, 0x6C, 0x6F, 0x20, 0x42, 0x79, 0x74, 0x65, 0x73]),
    vuid: '12345678-1234-5678-1234-567812345678',
    vbit: true,
    vtsu: DateTime.utc(2024, 6, 15, 12, 30, 45, 123),
    vtso: BaboonDateTimeOffset(
      epochMillis:
          DateTime.utc(2024, 6, 15, 12, 30, 45, 987).millisecondsSinceEpoch,
      offsetMillis: 7200000,
      kind: 'offset',
    ),
    voptStr: 'optional value',
    vlstI32: [1, 2, 3, 4, 5],
    vsetStr: {'apple', 'banana', 'cherry'},
    vmapStrI32: {'one': 1, 'two': 2, 'three': 3},
    voptLst: ['nested', 'list', 'values'],
    vlstOpt: [10, null, 20, 30],
    vmapLst: {
      'numbers': [1, 2, 3],
      'more': [4, 5, 6]
    },
    // Non-Pascal-case enum member; canonical JSON wire form is "Cafe" (PR-35-D06 regression guard).
    vWireEnum: WireEnum.Cafe,
    // Identifier (PR-57e). Wire form is `{"x": 42, "y": -7}` on JSON and two
    // i32 LE values on UEBA — byte-identical to a `data` of the same shape
    // per docs/spec/identifier-repr.md §1.3 / §7.
    vPointId: PointId(x: 42, y: -7),
    // PR-61 (M19.3) — id types as JSON map keys. Per PR-60 (M19.2) all id
    // types — single- or multi-field — use canonical repr toString as the
    // key form: e.g. `ItemId:2.0.0#v:00000000-0000-0000-0000-000000000001`.
    // Canonical deterministic uuids ensure cross-language byte-identity.
    vmapItemIdU32: {
      ItemId(v: '00000000-0000-0000-0000-000000000001'): 1,
      ItemId(v: '00000000-0000-0000-0000-000000000002'): 2,
    },
    vmapCompositeIdU32: {
      CompositeId(
        tenant: '00000000-0000-0000-0000-0000000000aa',
        user:   '00000000-0000-0000-0000-0000000000bb',
      ): 100,
      CompositeId(
        tenant: '00000000-0000-0000-0000-0000000000cc',
        user:   '00000000-0000-0000-0000-0000000000dd',
      ): 200,
    },
  );
}

void writeJson(AllBasicTypes data, String outputDir) {
  Directory(outputDir).createSync(recursive: true);
  final ctx = BaboonCodecContext.defaultCtx;
  final jsonData = AllBasicTypes_JsonCodec.instance.encode(ctx, data);
  final jsonStr = jsonEncode(jsonData);
  final jsonFile = File('$outputDir/all-basic-types.json');
  jsonFile.writeAsStringSync(jsonStr);
  print('Written JSON to ${jsonFile.absolute.path}');
}

void writeUeba(AllBasicTypes data, String outputDir) {
  Directory(outputDir).createSync(recursive: true);
  final ctx = BaboonCodecContext.defaultCtx;
  final writer = BaboonBinWriter();
  AllBasicTypes_UebaCodec.instance.encode(ctx, writer, data);
  final uebaBytes = writer.toBytes();
  final uebaFile = File('$outputDir/all-basic-types.ueba');
  uebaFile.writeAsBytesSync(uebaBytes);
  print('Written UEBA to ${uebaFile.absolute.path}');
}

void readAndVerify(String filePath) {
  if (filePath.endsWith('any-showcase.json') || filePath.endsWith('any-showcase.ueba')) {
    readAndVerifyAnyShowcase(filePath);
    return;
  }
  if (filePath.endsWith('m29-ok.json') || filePath.endsWith('m29-ok.ueba')) {
    readAndVerifyM29Ok(filePath);
    return;
  }
  final ctx = BaboonCodecContext.defaultCtx;
  AllBasicTypes data;

  try {
    if (filePath.endsWith('.json')) {
      final jsonStr = File(filePath).readAsStringSync();
      final json = jsonDecode(jsonStr);
      data = AllBasicTypes_JsonCodec.instance.decode(ctx, json);
    } else if (filePath.endsWith('.ueba')) {
      final bytes = File(filePath).readAsBytesSync();
      final reader = BaboonBinReader(Uint8List.fromList(bytes));
      data = AllBasicTypes_UebaCodec.instance.decode(ctx, reader);
    } else {
      stderr.writeln('Unknown file extension: $filePath');
      exit(1);
    }
  } catch (e) {
    stderr.writeln('Deserialization failed: $e');
    exit(1);
  }

  if (data.vstr != 'Hello, Baboon!') {
    stderr.writeln("vstr mismatch: expected 'Hello, Baboon!', got '${data.vstr}'");
    exit(1);
  }
  if (data.vi32 != 123456) {
    stderr.writeln('vi32 mismatch: expected 123456, got ${data.vi32}');
    exit(1);
  }
  if (!data.vbit) {
    stderr.writeln('vbit mismatch: expected true, got ${data.vbit}');
    exit(1);
  }

  // Roundtrip
  try {
    if (filePath.endsWith('.json')) {
      final reEncoded = AllBasicTypes_JsonCodec.instance.encode(ctx, data);
      final reDecoded = AllBasicTypes_JsonCodec.instance.decode(ctx, reEncoded);
      if (data != reDecoded) {
        stderr.writeln('JSON roundtrip mismatch');
        exit(1);
      }
    } else {
      final writer = BaboonBinWriter();
      AllBasicTypes_UebaCodec.instance.encode(ctx, writer, data);
      final reReader = BaboonBinReader(writer.toBytes());
      final reDecoded = AllBasicTypes_UebaCodec.instance.decode(ctx, reReader);
      if (data != reDecoded) {
        stderr.writeln('UEBA roundtrip mismatch');
        exit(1);
      }
    }
  } catch (e) {
    stderr.writeln('Roundtrip failed: $e');
    exit(1);
  }

  print('OK');
}

// PR-57e (M18.4e) — cross-language identifier repr (toString) byte-identity.
// Per spec §7 the repr/toString form is a separate invariant from the JSON/UEBA wire bytes;
// we write it as a per-language artifact so the Scala-side test can assert all 10 backends
// produce byte-identical output for the same canonical PointId value.
void writePointIdRepr(PointId pid, String outputDir) {
  Directory(outputDir).createSync(recursive: true);
  final f = File('$outputDir/point-id.txt');
  // No trailing newline — exact byte match across all languages.
  f.writeAsStringSync(pid.toString());
  print('Written repr to ${f.absolute.path}');
}

// PR-I.1d (M24 Phase 3.1) — Custom-foreign KeyCodec hook canonical fixture.
// Map keys go through FStr_KeyCodecHost (default identity impl for the stringy
// foreign), so the wire form is `{"m":{"alpha":"v1","beta":"v2"}}`.
ForeignKeyHolder _createForeignKeyHolderSample() {
  return ForeignKeyHolder(m: {
    ItemKey(v: 'alpha'): 'v1',
    ItemKey(v: 'beta'): 'v2',
  });
}

void writeForeignKeyHolderJson(BaboonCodecContext ctx, ForeignKeyHolder data, String outputDir) {
  Directory(outputDir).createSync(recursive: true);
  final json = ForeignKeyHolder_JsonCodec.instance.encode(ctx, data);
  // Compact (no indent) so the byte-identity assertion against the canonical
  // wire form `{"m":{"alpha":"v1","beta":"v2"}}` matches across backends.
  final jsonStr = jsonEncode(json);
  final f = File('$outputDir/m24-foreign-keycodec.json');
  f.writeAsStringSync(jsonStr);
  print('Written JSON to ${f.absolute.path}');
}

void runLegacy() {
  final sampleData = createSampleData();
  final sampleAny = _createSampleAnyShowcase();
  final baseDir = Directory('../../target/compat-test').absolute.path;
  final facadeCtx = BaboonCodecContext.withFacade(false, _freshFacade());

  writeJson(sampleData, '$baseDir/dart-json');
  writeUeba(sampleData, '$baseDir/dart-ueba');
  writeJsonAny(facadeCtx, sampleAny, '$baseDir/dart-json');
  writeUebaAny(facadeCtx, sampleAny, '$baseDir/dart-ueba');
  writePointIdRepr(sampleData.vPointId, '$baseDir/dart-repr');
  writeForeignKeyHolderJson(BaboonCodecContext.defaultCtx, _createForeignKeyHolderSample(), '$baseDir/dart-json');
  writeBuiltinMapKeyHolderJson(BaboonCodecContext.defaultCtx, _createBuiltinMapKeyHolderSample(), '$baseDir/dart-json');
  writeBuiltinMapKeyHolderUeba(BaboonCodecContext.defaultCtx, _createBuiltinMapKeyHolderSample(), '$baseDir/dart-ueba');

  print('Dart serialization complete!');
}

// PR-26.5 (M26) — non-string builtin map-key cross-language fixture.
// PR-28.4 (M28) — extended with mu64 + mtso (mf64 deferred).
BuiltinMapKeyHolder _createBuiltinMapKeyHolderSample() {
  // PR-28.4: non-UTC tso offset (PR-28.3 ±HH:MM canonicalisation).
  // Wall-clock 2026-05-02T12:00:00.123+05:30 = instant 2026-05-02T06:30:00.123Z.
  final tsoIst = BaboonDateTimeOffset(
    epochMillis:
        DateTime.utc(2026, 5, 2, 6, 30, 0, 123).millisecondsSinceEpoch,
    offsetMillis: (5 * 3600 + 30 * 60) * 1000,
    kind: 'offset',
  );
  return BuiltinMapKeyHolder(
    mi32: {42: 'v32'},
    mi64: {9223372036854775807: 'vmax'},
    mu32: {7: 'vu32'},
    // PR-28.4 (M28): u64 = -1 (signed-int encoding of UInt64.MaxValue).
    // Dart u64 maps to dt `int` (signed 64-bit); encoder converts via
    // BigInt.from(_).toUnsigned(64).toString() to canonical
    // "18446744073709551615".
    mu64: {-1: 'vu64max'},
    mbit: {true: 'vt'},
    muid: {'00000000-0000-0000-0000-000000000001': 'vid'},
    mtso: {tsoIst: 'vtso_ist'},
  );
}

void writeBuiltinMapKeyHolderJson(BaboonCodecContext ctx, BuiltinMapKeyHolder data, String outputDir) {
  Directory(outputDir).createSync(recursive: true);
  final json = BuiltinMapKeyHolder_JsonCodec.instance.encode(ctx, data);
  final jsonStr = jsonEncode(json);
  final f = File('$outputDir/m26-builtin-map-keys.json');
  f.writeAsStringSync(jsonStr);
  print('Written JSON to ${f.absolute.path}');
}

void writeBuiltinMapKeyHolderUeba(BaboonCodecContext ctx, BuiltinMapKeyHolder data, String outputDir) {
  Directory(outputDir).createSync(recursive: true);
  final writer = BaboonBinWriter();
  BuiltinMapKeyHolder_UebaCodec.instance.encode(ctx, writer, data);
  final f = File('$outputDir/m26-builtin-map-keys.ueba');
  f.writeAsBytesSync(writer.toBytes());
  print('Written UEBA to ${f.absolute.path}');
}

// PR-29.10 (M29) — monomorphised-template cross-language wire-format fixture.
M29OkHolder _createM29OkSample() {
  return M29OkHolder(
    intPage: IntPage(items: [1, 2, 3], total: 3),
    strPage: StrPage(items: ['hello', 'world'], total: 2),
    itemPage: ItemPage(items: [Item(name: 'apple', price: 1.5), Item(name: 'banana', price: 0.75)], total: 2),
    okEnvelope: Ok(value: 42),
    errEnvelope: Err(error: 'oops'),
  );
}

void writeM29OkJson(BaboonCodecContext ctx, M29OkHolder data, String outputDir) {
  Directory(outputDir).createSync(recursive: true);
  final json = M29OkHolder_JsonCodec.instance.encode(ctx, data);
  final jsonStr = jsonEncode(json);
  final f = File('$outputDir/m29-ok.json');
  f.writeAsStringSync(jsonStr);
  print('Written JSON to ${f.absolute.path}');
}

void writeM29OkUeba(BaboonCodecContext ctx, M29OkHolder data, String outputDir) {
  Directory(outputDir).createSync(recursive: true);
  final writer = BaboonBinWriter();
  M29OkHolder_UebaCodec.instance.encode(ctx, writer, data);
  final f = File('$outputDir/m29-ok.ueba');
  f.writeAsBytesSync(writer.toBytes());
  print('Written UEBA to ${f.absolute.path}');
}

void readAndVerifyM29Ok(String filePath) {
  final ctx = BaboonCodecContext.defaultCtx;
  M29OkHolder data;
  try {
    if (filePath.endsWith('.json')) {
      final jsonStr = File(filePath).readAsStringSync();
      final json = jsonDecode(jsonStr);
      data = M29OkHolder_JsonCodec.instance.decode(ctx, json);
    } else {
      final bytes = File(filePath).readAsBytesSync();
      final reader = BaboonBinReader(Uint8List.fromList(bytes));
      data = M29OkHolder_UebaCodec.instance.decode(ctx, reader);
    }
  } catch (e) {
    stderr.writeln('M29OkHolder deserialization failed: $e');
    exit(1);
  }
  if (data.intPage.total != 3) {
    stderr.writeln('M29OkHolder intPage.total mismatch: expected 3, got ${data.intPage.total}');
    exit(1);
  }
  if (data.okEnvelope is! Ok) {
    stderr.writeln('M29OkHolder okEnvelope is not Ok');
    exit(1);
  }
  if (data.errEnvelope is! Err) {
    stderr.writeln('M29OkHolder errEnvelope is not Err');
    exit(1);
  }
  print('OK');
}

void main(List<String> args) {
  if (args.length >= 3 && args[0] == 'write') {
    final outputDir = args[1];
    final format = args[2];
    final sampleData = createSampleData();
    final sampleAny = _createSampleAnyShowcase();
    final facadeCtx = BaboonCodecContext.withFacade(false, _freshFacade());
    final m29Sample = _createM29OkSample();
    switch (format) {
      case 'json':
        writeJson(sampleData, outputDir);
        writeJsonAny(facadeCtx, sampleAny, outputDir);
        writeForeignKeyHolderJson(BaboonCodecContext.defaultCtx, _createForeignKeyHolderSample(), outputDir);
        writeM29OkJson(BaboonCodecContext.defaultCtx, m29Sample, outputDir);
        break;
      case 'ueba':
        writeUeba(sampleData, outputDir);
        writeUebaAny(facadeCtx, sampleAny, outputDir);
        writeM29OkUeba(BaboonCodecContext.defaultCtx, m29Sample, outputDir);
        break;
      default:
        stderr.writeln('Unknown format: $format');
        exit(1);
    }
  } else if (args.length >= 2 && args[0] == 'read') {
    readAndVerify(args[1]);
  } else {
    runLegacy();
  }
}
