import 'dart:convert';
import 'dart:io';

import 'package:baboon_runtime/baboon_runtime.dart';
import 'package:conv_test_dt/generated/convtest/m24foreign/foreign_key_holder.dart';
import 'package:conv_test_dt/generated/convtest/m24foreign/item_key.dart';
import 'package:test/test.dart';

// ----------------------------------------------------------------------------
// PR-I.1d (M24 Phase 3.1) — Custom-foreign `<Foreign>_KeyCodec` extension hook
// (Dart mirror).
//
// Mirrors the Scala reference (Test_CrossLanguageCompat.scala), Java mirror
// (CrossLanguageTest.java), and C# mirror (Test_CrossLanguageCompat.cs):
// round-trip the Dart-emitted m24-foreign-keycodec.json through
// ForeignKeyHolder_JsonCodec and assert byte-identity (PR-I-D02 pattern
// guidance) of the encoded JSON string against the canonical wire form
// `{"m":{"alpha":"v1","beta":"v2"}}`.
// ----------------------------------------------------------------------------

void main() {
  final baseDir = Directory('../../target/compat-test').absolute.path;
  final ctx = BaboonCodecContext.defaultCtx;

  test('m24ForeignKeyCodecRoundTripDart', () {
    final file = File('$baseDir/dart-json/m24-foreign-keycodec.json');
    expect(file.existsSync(), isTrue,
        reason: 'Dart m24-foreign-keycodec fixture not found: ${file.path}');
    final jsonStr = file.readAsStringSync();
    final json = jsonDecode(jsonStr);
    final decoded = ForeignKeyHolder_JsonCodec.instance.decode(ctx, json);
    final expected = ForeignKeyHolder(m: {
      ItemKey(v: 'alpha'): 'v1',
      ItemKey(v: 'beta'): 'v2',
    });
    expect(decoded, equals(expected), reason: 'round-trip diverged');
  });

  test('m24ForeignKeyCodecCanonicalWireForm', () {
    final sample = ForeignKeyHolder(m: {
      ItemKey(v: 'alpha'): 'v1',
      ItemKey(v: 'beta'): 'v2',
    });
    final encoded = ForeignKeyHolder_JsonCodec.instance.encode(ctx, sample);
    final actual = jsonEncode(encoded);
    const expected = '{"m":{"alpha":"v1","beta":"v2"}}';
    expect(actual, equals(expected), reason: 'FStr_KeyCodec wire form diverged');
  });
}
