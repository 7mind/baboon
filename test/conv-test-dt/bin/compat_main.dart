import 'dart:convert';
import 'dart:io';
import 'dart:typed_data';

import 'package:baboon_runtime/baboon_any_opaque.dart';
import 'package:baboon_runtime/baboon_runtime.dart';
import 'package:conv_test_dt/generated/convtest/testpkg/all_basic_types.dart';
import 'package:conv_test_dt/generated/convtest/testpkg/any_showcase.dart';
import 'package:conv_test_dt/generated/convtest/testpkg/inner_payload.dart';

const String _domainId = 'convtest.testpkg';
const String _domainVer = '2.0.0';
const String _innerTypeId = 'convtest.testpkg/:#InnerPayload';

// PR 13.2 / PR-26-D02: Dart facade is currently broken — `AbstractBaboonCodecs.register` casts
// the registered codec to `BaboonCodecData` but Dart codec classes do not implement that
// interface. Cross-format encoding via `BaboonCodecsFacade.uebaToJson`/`jsonToUebaBytes`
// therefore fails. To unblock cross-language interop testing, the Dart fixture uses
// AnyOpaqueJson for the JSON wire emission and AnyOpaqueUeba for the UEBA wire emission so the
// codec never needs facade-resolution. The defect is logged for follow-up; cross-format coverage
// is exercised by Scala/C#/Java/Kotlin/KMP/TS where the facade works.

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

AnyMeta _metaA() => AnyMeta(0x07, _domainId, _domainVer, _innerTypeId);
AnyMeta _metaB() => AnyMeta(0x03, null, _domainVer, _innerTypeId);
AnyMeta _metaC() => AnyMeta(0x01, null, null, _innerTypeId);
AnyMeta _metaD1() => AnyMeta(0x06, _domainId, _domainVer, null);
AnyMeta _metaD2() => AnyMeta(0x02, null, _domainVer, null);
AnyMeta _metaD3() => AnyMeta(0x00, null, null, null);
AnyMeta _metaOpt() => AnyMeta(0x07, _domainId, _domainVer, _innerTypeId);
AnyMeta _metaLst() => AnyMeta(0x06, _domainId, _domainVer, null);

AnyShowcase _createSampleAnyShowcaseJson() {
  final p = _expectedInnerPayloads();
  return AnyShowcase(
    vAnyA: AnyOpaqueJson(_metaA(), _asJson(p[0])),
    vAnyB: AnyOpaqueJson(_metaB(), _asJson(p[1])),
    vAnyC: AnyOpaqueJson(_metaC(), _asJson(p[2])),
    vAnyD1: AnyOpaqueJson(_metaD1(), _asJson(p[3])),
    vAnyD2: AnyOpaqueJson(_metaD2(), _asJson(p[4])),
    vAnyD3: AnyOpaqueJson(_metaD3(), _asJson(p[5])),
    optAny: AnyOpaqueJson(_metaOpt(), _asJson(p[6])),
    lstAny: [AnyOpaqueJson(_metaLst(), _asJson(p[7]))],
  );
}

AnyShowcase _createSampleAnyShowcaseUeba() {
  final p = _expectedInnerPayloads();
  return AnyShowcase(
    vAnyA: AnyOpaqueUeba(_metaA(), _uebaBytes(p[0])),
    vAnyB: AnyOpaqueUeba(_metaB(), _uebaBytes(p[1])),
    vAnyC: AnyOpaqueUeba(_metaC(), _uebaBytes(p[2])),
    vAnyD1: AnyOpaqueUeba(_metaD1(), _uebaBytes(p[3])),
    vAnyD2: AnyOpaqueUeba(_metaD2(), _uebaBytes(p[4])),
    vAnyD3: AnyOpaqueUeba(_metaD3(), _uebaBytes(p[5])),
    optAny: AnyOpaqueUeba(_metaOpt(), _uebaBytes(p[6])),
    lstAny: [AnyOpaqueUeba(_metaLst(), _uebaBytes(p[7]))],
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

void runLegacy() {
  final sampleData = createSampleData();
  final baseDir = Directory('../../target/compat-test').absolute.path;
  final ctx = BaboonCodecContext.defaultCtx;

  writeJson(sampleData, '$baseDir/dart-json');
  writeUeba(sampleData, '$baseDir/dart-ueba');
  writeJsonAny(ctx, _createSampleAnyShowcaseJson(), '$baseDir/dart-json');
  writeUebaAny(ctx, _createSampleAnyShowcaseUeba(), '$baseDir/dart-ueba');

  print('Dart serialization complete!');
}

void main(List<String> args) {
  if (args.length >= 3 && args[0] == 'write') {
    final outputDir = args[1];
    final format = args[2];
    final sampleData = createSampleData();
    final ctx = BaboonCodecContext.defaultCtx;
    switch (format) {
      case 'json':
        writeJson(sampleData, outputDir);
        writeJsonAny(ctx, _createSampleAnyShowcaseJson(), outputDir);
        break;
      case 'ueba':
        writeUeba(sampleData, outputDir);
        writeUebaAny(ctx, _createSampleAnyShowcaseUeba(), outputDir);
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
