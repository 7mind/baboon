// `bytes` is a `Uint8List`, whose `==` is identity: a decoded `map[bytes, _]` must still be
// indexable by a fresh `Uint8List` with the same content.
//
// Uses the testpkg.pkg0 fixtures; generated symbols are produced by mdl :test-gen-regular-adt.

import 'dart:typed_data';

import 'package:baboon_runtime/baboon_runtime.dart';
import 'package:dt_stub/testpkg/pkg0/t1_d1.dart';
import 'package:dt_stub/testpkg/pkg0/t1_d2.dart';
import 'package:test/test.dart';

void main() {
  Uint8List key() => Uint8List.fromList([0xAB, 0xCD]);

  final value = T1_D1(
    f0: T1_D2(f: 's'),
    f1: 1,
    f3: 't',
    f4: Uint8List.fromList([0x01]),
    f6: {key(): 'v'},
  );

  test('JSON-decoded bytes-keyed map is indexable by content', () {
    final ctx = BaboonCodecContext.compact;
    final decoded = T1_D1_JsonCodec.instance.decode(ctx, T1_D1_JsonCodec.instance.encode(ctx, value));
    expect(decoded.f6[key()], 'v');
    expect(decoded.f6.containsKey(key()), isTrue);
  });

  for (final ctx in [BaboonCodecContext.compact, BaboonCodecContext.indexed]) {
    test('UEBA-decoded bytes-keyed map is indexable by content (indexed=${ctx.useIndices})', () {
      final writer = BaboonBinWriter();
      T1_D1_UebaCodec.instance.encode(ctx, writer, value);
      final decoded = T1_D1_UebaCodec.instance.decode(ctx, BaboonBinReader(writer.toBytes()));
      expect(decoded.f6[key()], 'v');
      expect(decoded.f6.containsKey(key()), isTrue);
    });
  }
}
