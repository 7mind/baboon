import 'dart:convert';
import 'dart:io';
import 'dart:typed_data';

import 'package:baboon_runtime/baboon_runtime.dart';
import 'package:conv_test_dt/generated/convtest/testpkg/all_basic_types.dart';
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
    'java'
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
}
