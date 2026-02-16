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

void main() {
  final sampleData = createSampleData();
  final ctx = BaboonCodecContext.defaultCtx;

  final baseDir = Directory('../../target/compat-test').absolute.path;
  final dartJsonDir = '$baseDir/dart-json';
  final dartUebaDir = '$baseDir/dart-ueba';

  Directory(dartJsonDir).createSync(recursive: true);
  Directory(dartUebaDir).createSync(recursive: true);

  final jsonData = AllBasicTypes_JsonCodec.instance.encode(ctx, sampleData);
  final jsonStr = jsonEncode(jsonData);
  final jsonFile = File('$dartJsonDir/all-basic-types.json');
  jsonFile.writeAsStringSync(jsonStr);
  print('Written JSON to ${jsonFile.absolute.path}');

  final writer = BaboonBinWriter();
  AllBasicTypes_UebaCodec.instance.encode(ctx, writer, sampleData);
  final uebaBytes = writer.toBytes();
  final uebaFile = File('$dartUebaDir/all-basic-types.ueba');
  uebaFile.writeAsBytesSync(uebaBytes);
  print('Written UEBA to ${uebaFile.absolute.path}');

  print('Dart serialization complete!');
}
