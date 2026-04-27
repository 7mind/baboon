import 'dart:convert';
import 'dart:io';
import 'dart:typed_data';

import 'package:baboon_runtime/baboon_any_opaque.dart';
import 'package:baboon_runtime/baboon_runtime.dart';
import 'package:conv_test_dt/generated/convtest/testpkg/all_basic_types.dart';
import 'package:conv_test_dt/generated/convtest/testpkg/any_showcase.dart';
import 'package:conv_test_dt/generated/convtest/testpkg/inner_payload.dart';
import 'package:test/test.dart';

final baseDir = Directory('../../target/compat-test').absolute.path;
final ctx = BaboonCodecContext.defaultCtx;

AllBasicTypes readJsonFile(String source) {
  final file = File('$baseDir/$source-json/all-basic-types.json');
  final jsonStr = file.readAsStringSync();
  final json = jsonDecode(jsonStr);
  return AllBasicTypes_JsonCodec.instance.decode(ctx, json);
}

AllBasicTypes readUebaFile(String source) {
  final file = File('$baseDir/$source-ueba/all-basic-types.ueba');
  final bytes = file.readAsBytesSync();
  final reader = BaboonBinReader(Uint8List.fromList(bytes));
  return AllBasicTypes_UebaCodec.instance.decode(ctx, reader);
}

void main() {
  final sources = [
    'dart',
    'scala',
    'cs',
    'rust',
    'python',
    'typescript',
    'kotlin',
    'java',
    'swift'
  ];

  group('JSON cross-language compatibility', () {
    for (final source in sources) {
      test('decode $source JSON', () {
        final file = File('$baseDir/$source-json/all-basic-types.json');
        if (!file.existsSync()) {
          print('Skipping $source JSON - file not found');
          return;
        }
        final decoded = readJsonFile(source);
        final reEncoded =
            AllBasicTypes_JsonCodec.instance.encode(ctx, decoded);
        final reDecoded =
            AllBasicTypes_JsonCodec.instance.decode(ctx, reEncoded);
        expect(reDecoded, equals(decoded));
      });
    }
  });

  group('UEBA cross-language compatibility', () {
    for (final source in sources) {
      test('decode $source UEBA', () {
        final file = File('$baseDir/$source-ueba/all-basic-types.ueba');
        if (!file.existsSync()) {
          print('Skipping $source UEBA - file not found');
          return;
        }
        final decoded = readUebaFile(source);
        final writer = BaboonBinWriter();
        AllBasicTypes_UebaCodec.instance.encode(ctx, writer, decoded);
        final reReader = BaboonBinReader(writer.toBytes());
        final reDecoded =
            AllBasicTypes_UebaCodec.instance.decode(ctx, reReader);
        expect(reDecoded, equals(decoded));
      });
    }
  });

  // ---------------------------------------------------------------------------
  // AnyShowcase cross-language tests (M13 / PR 13.2)
  // ---------------------------------------------------------------------------

  final expectedAnyPayloads = <InnerPayload>[
    InnerPayload(label: 'variant-A', count: 1),
    InnerPayload(label: 'variant-B', count: 2),
    InnerPayload(label: 'variant-C', count: 3),
    InnerPayload(label: 'variant-D1', count: 4),
    InnerPayload(label: 'variant-D2', count: 5),
    InnerPayload(label: 'variant-D3', count: 6),
    InnerPayload(label: 'opt-any', count: 7),
    InnerPayload(label: 'lst-any-0', count: 8),
  ];

  AnyShowcase readAnyShowcaseJson(String source) {
    final file = File('$baseDir/$source-json/any-showcase.json');
    final jsonStr = file.readAsStringSync();
    final json = jsonDecode(jsonStr);
    return AnyShowcase_JsonCodec.instance.decode(ctx, json);
  }

  AnyShowcase readAnyShowcaseUeba(String source) {
    final file = File('$baseDir/$source-ueba/any-showcase.ueba');
    final bytes = file.readAsBytesSync();
    final r = BaboonBinReader(Uint8List.fromList(bytes));
    return AnyShowcase_UebaCodec.instance.decode(ctx, r);
  }

  InnerPayload decodeInner(AnyOpaque o) {
    if (o is AnyOpaqueUeba) {
      final r = BaboonBinReader(o.bytes);
      return InnerPayload_UebaCodec.instance.decode(BaboonCodecContext.compact, r);
    }
    if (o is AnyOpaqueJson) {
      return InnerPayload_JsonCodec.instance.decode(BaboonCodecContext.compact, o.json);
    }
    throw StateError('unexpected AnyOpaque subclass: ${o.runtimeType}');
  }

  List<InnerPayload> decodeAllPayloads(AnyShowcase v) {
    final opt = v.optAny;
    if (opt == null) throw StateError('optAny was null');
    if (v.lstAny.isEmpty) throw StateError('lstAny was empty');
    return [
      decodeInner(v.vAnyA),
      decodeInner(v.vAnyB),
      decodeInner(v.vAnyC),
      decodeInner(v.vAnyD1),
      decodeInner(v.vAnyD2),
      decodeInner(v.vAnyD3),
      decodeInner(opt),
      decodeInner(v.lstAny.first),
    ];
  }

  void assertAnyShowcase(String source, String fmt, AnyShowcase v) {
    final decoded = decodeAllPayloads(v);
    expect(decoded.length, equals(expectedAnyPayloads.length),
        reason: '$source $fmt count');
    for (var i = 0; i < expectedAnyPayloads.length; i++) {
      expect(decoded[i], equals(expectedAnyPayloads[i]),
          reason: '$source $fmt payload $i mismatch');
    }
  }

  group('AnyShowcase cross-language', () {
    for (final source in sources) {
      test('decode $source any-showcase JSON', () {
        final file = File('$baseDir/$source-json/any-showcase.json');
        if (!file.existsSync()) {
          print('Skipping $source any-showcase JSON - file not found');
          return;
        }
        assertAnyShowcase(source, 'JSON', readAnyShowcaseJson(source));
      });
      test('decode $source any-showcase UEBA', () {
        final file = File('$baseDir/$source-ueba/any-showcase.ueba');
        if (!file.existsSync()) {
          print('Skipping $source any-showcase UEBA - file not found');
          return;
        }
        assertAnyShowcase(source, 'UEBA', readAnyShowcaseUeba(source));
      });
    }

    test('any-showcase UEBA byte-identical: dart vs scala', () {
      final dartBytes = File('$baseDir/dart-ueba/any-showcase.ueba').readAsBytesSync();
      final scalaBytes = File('$baseDir/scala-ueba/any-showcase.ueba').readAsBytesSync();
      expect(dartBytes, equals(scalaBytes), reason: 'Dart and Scala UEBA bytes diverged');
    });
  });
}
