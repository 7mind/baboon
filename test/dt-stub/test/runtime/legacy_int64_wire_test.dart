// 64-bit integers are written as decimal strings (docs/json-codecs.md, "64-bit integers").
// Readers stay lenient about the JSON-number form an older compiler produced, and that
// leniency is what keeps documents written before the change readable.
//
// Nothing writes numbers any more, so without this test the number arm of every decoder is
// dead as far as the suite is concerned.
//
// Uses the identifier.ok fixture (id LongId { x: i64 }; id UInts { a: u08, b: u16, c: u32,
// d: u64 }); generated symbols are produced by mdl :test-gen-regular-adt.

import 'dart:convert';

import 'package:baboon_runtime/baboon_runtime.dart';
import 'package:dt_stub/identifier/ok/long_id.dart';
import 'package:dt_stub/identifier/ok/u_ints.dart';
import 'package:test/test.dart';

void main() {
  group('64-bit integers decode from both wire forms', () {
    final ctx = BaboonCodecContext.compact;

    test('i64 decodes from the legacy numeric form', () {
      final decoded = LongId_JsonCodec.instance
          .decode(ctx, jsonDecode('{"x":-9007199254740991}'));
      expect(decoded.x, equals(-9007199254740991));
    });

    test('i64 decodes from the string form', () {
      final decoded = LongId_JsonCodec.instance
          .decode(ctx, jsonDecode('{"x":"-9223372036854775808"}'));
      expect(decoded.x, equals(-9223372036854775808));
    });

    test('u64 decodes from the legacy numeric form', () {
      final decoded = UInts_JsonCodec.instance
          .decode(ctx, jsonDecode('{"a":1,"b":2,"c":3,"d":42}'));
      expect(decoded.d, equals(42));
    });

    test('u64 decodes from the string form', () {
      final decoded = UInts_JsonCodec.instance
          .decode(ctx, jsonDecode('{"a":1,"b":2,"c":3,"d":"42"}'));
      expect(decoded.d, equals(42));
    });
  });
}
