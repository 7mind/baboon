// PR-26.2 (M26) — KeyCodec Host last-wins concurrency contract regression test.
//
// Asserts FStr_KeyCodecHost.register(impl) overwrites the previously registered
// impl (last-wins). Dart already used a `static var` mutable singleton
// pre-PR-26.2; this test pins that behavior across future refactors.
//
// Generated symbols are produced by mdl :test-gen-regular-adt under
// target/test-regular/dt-stub/.

import 'dart:convert';

import 'package:baboon_runtime/baboon_runtime.dart';
import 'package:dt_stub/my/ok/m19/foreign/f_str.dart';
import 'package:dt_stub/my/ok/m19/foreign/holder.dart';
import 'package:dt_stub/my/ok/m19/foreign/item_key.dart';
import 'package:test/test.dart';

class _PrefixCodec implements FStr_KeyCodec {
  final String tag;
  _PrefixCodec(this.tag);
  @override
  String encodeKey(String value) => '$tag:$value';
  @override
  String decodeKey(String s) {
    final pfx = '$tag:';
    return s.startsWith(pfx) ? s.substring(pfx.length) : s;
  }
}

class _IdentityCodec implements FStr_KeyCodec {
  @override
  String encodeKey(String value) => value;
  @override
  String decodeKey(String s) => s;
}

void main() {
  group('PR-26.2 — KeyCodec Host last-wins', () {
    // PR-26.2-D01: restore identity impl after each test so the global
    // FStr_KeyCodecHost singleton does not leak a PrefixCodec into sibling
    // tests sharing the isolate. Runs even on assertion failure.
    tearDown(() {
      FStr_KeyCodecHost.register(_IdentityCodec());
    });

    test('register(B) after register(A) → encode observes B (NOT A)', () {
      final ctx = BaboonCodecContext.compact;
      final original = Holder(m: {ItemKey(v: 'k'): 'v'});

      FStr_KeyCodecHost.register(_PrefixCodec('A'));
      final encodedA = jsonEncode(Holder_JsonCodec.instance.encode(ctx, original));
      expect(encodedA, contains('A:k'),
          reason: 'expected A: prefix in encoded wire form');

      FStr_KeyCodecHost.register(_PrefixCodec('B'));
      final encodedB = jsonEncode(Holder_JsonCodec.instance.encode(ctx, original));
      expect(encodedB, contains('B:k'),
          reason: 'PR-26.2 last-wins regression: expected B: prefix after re-register');
      expect(encodedB, isNot(contains('A:k')),
          reason: 'PR-26.2 last-wins regression: A: prefix still present after B re-register');
    });
  });
}
