// `bytes` JSON wire-form decoding (GitHub 7mind/baboon#102, #103): hex digits, with ASCII
// whitespace, `:` and `-` ignored as separators; anything else, or an odd digit count, is a
// BaboonDecoderFailure — never wrong bytes, truncation or an unrelated exception type.
//
// The codec-level group uses the identifier.ok fixture (id Mixed { ..., payload: bytes, ... });
// generated symbols are produced by mdl :test-gen-regular-adt.

import 'dart:typed_data';

import 'package:baboon_runtime/baboon_runtime.dart';
import 'package:dt_stub/identifier/ok/mixed.dart';
import 'package:test/test.dart';

const _validVectors = <String, List<int>>{
  '': [],
  'ABCD': [0xAB, 0xCD],
  'abcd': [0xAB, 0xCD],
  'aBcD': [0xAB, 0xCD],
  'AB CD': [0xAB, 0xCD],
  'AB:CD': [0xAB, 0xCD],
  'AB-CD': [0xAB, 0xCD],
  'A B C D': [0xAB, 0xCD],
  '\tAB\r\nCD ': [0xAB, 0xCD],
  'A:B-C D': [0xAB, 0xCD],
  '\u000BAB\u000CCD': [0xAB, 0xCD],
  '00FF7f': [0x00, 0xFF, 0x7F],
  ' - : ': [],
};

const _invalidVectors = <String>[
  'ZZ',
  'AQ',
  '+1',
  '0x01',
  'q80=',
  'ABC',
  'A',
  '-1',
  ' 1',
  '٣٣',
  'ＡＡ',
  'aéb',
  'AB CD',
];

void main() {
  group('BaboonByteStringTools.fromHexString', () {
    _validVectors.forEach((input, expected) {
      test('decodes ${input.codeUnits}', () {
        expect(BaboonByteStringTools.fromHexString(input), equals(expected));
      });
    });

    for (final input in _invalidVectors) {
      test('rejects ${input.codeUnits}', () {
        expect(() => BaboonByteStringTools.fromHexString(input), throwsA(isA<BaboonDecoderFailure>()));
      });
    }

    test('names the offending character', () {
      expect(
        () => BaboonByteStringTools.fromHexString('AQ'),
        throwsA(isA<BaboonDecoderFailure>().having((e) => e.message, 'message', contains("'Q'"))),
      );
    });

    test('names the odd digit count', () {
      expect(
        () => BaboonByteStringTools.fromHexString('ABC'),
        throwsA(isA<BaboonDecoderFailure>().having((e) => e.message, 'message', contains('(3)'))),
      );
    });
  });

  group('bytes field through a generated JSON codec', () {
    final ctx = BaboonCodecContext.compact;
    final codec = Mixed_JsonCodec.instance;

    Map<String, dynamic> wireWithPayload(Object payload) {
      final src = Mixed(
        active: true,
        id: 'de7b9e1e-5c93-45fe-beec-da99994f629a',
        payload: Uint8List(0),
        created: DateTime.utc(2026, 4, 29, 12, 34, 56, 789),
        scheduled: BaboonDateTimeOffset(
          epochMillis: DateTime.utc(2026, 4, 29, 10, 34, 56, 789).millisecondsSinceEpoch,
          offsetMillis: 2 * 3600000,
          kind: 'offset',
        ),
      );
      final wire = Map<String, dynamic>.from(codec.encode(ctx, src) as Map);
      wire['payload'] = payload;
      return wire;
    }

    _validVectors.forEach((input, expected) {
      test('decodes payload ${input.codeUnits}', () {
        expect(codec.decode(ctx, wireWithPayload(input)).payload, equals(expected));
      });
    });

    for (final input in _invalidVectors) {
      test('rejects payload ${input.codeUnits}', () {
        expect(() => codec.decode(ctx, wireWithPayload(input)), throwsA(isA<BaboonDecoderFailure>()));
      });
    }
  });
}
