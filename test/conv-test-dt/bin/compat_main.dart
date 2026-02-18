import 'dart:convert';
import 'dart:io';
import 'dart:typed_data';

import 'package:baboon_runtime/baboon_runtime.dart';
import 'package:conv_test_dt/generated/convtest/testpkg/all_basic_types.dart';

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

  writeJson(sampleData, '$baseDir/dart-json');
  writeUeba(sampleData, '$baseDir/dart-ueba');

  print('Dart serialization complete!');
}

void main(List<String> args) {
  if (args.length >= 3 && args[0] == 'write') {
    final outputDir = args[1];
    final format = args[2];
    final sampleData = createSampleData();
    switch (format) {
      case 'json':
        writeJson(sampleData, outputDir);
        break;
      case 'ueba':
        writeUeba(sampleData, outputDir);
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
