import 'dart:typed_data';
import 'package:baboon_runtime/baboon_any_opaque.dart';
import 'package:baboon_runtime/baboon_runtime.dart';
import 'package:test/test.dart';

void main() {
  final ctx = BaboonCodecContext.defaultCtx;
  final metas = [
    AnyMeta(7, 'domain', '1.0.0', 'type'),
    AnyMeta(3, null, '1.0.0', 'type'),
    AnyMeta(1, null, null, 'type'),
    AnyMeta(6, 'domain', '1.0.0', null),
    AnyMeta(2, null, '1.0.0', null),
    AnyMeta(0, null, null, null),
  ];
  test('shared helpers preserve all six native envelope kinds and nullable JSON', () {
    for (final meta in metas) {
      final json = AnyOpaqueJson(meta, null);
      final encoded = encodeAnyJsonField(ctx, meta.kind, null, null, null, json);
      expect(decodeAnyJsonField(meta.kind, encoded), json);
      final binary = AnyOpaqueUeba(meta, Uint8List.fromList([0, 1, 255]));
      final writer = BaboonBinWriter();
      encodeAnyUebaField(ctx, writer, meta.kind, null, null, null, binary);
      expect(decodeAnyUebaField(BaboonBinReader(writer.toBytes()), meta.kind), binary);
    }
  });
  test('shared helpers preserve missing facade and malformed framing failures', () {
    final meta = metas.last;
    expect(() => encodeAnyJsonField(ctx, 0, null, null, null, AnyOpaqueUeba(meta, Uint8List(0))), throwsA(isA<BaboonEncoderFailure>()));
    expect(() => encodeAnyUebaField(ctx, BaboonBinWriter(), 0, null, null, null, AnyOpaqueJson(meta, null)), throwsA(isA<BaboonEncoderFailure>()));
    expect(() => decodeAnyJsonField(0, <String, Object?>{r'$ak': 0}), throwsA(isA<BaboonDecoderFailure>()));
    final writer = BaboonBinWriter()..writeI32(-1);
    expect(() => decodeAnyUebaField(BaboonBinReader(writer.toBytes()), 0), throwsA(isA<BaboonDecoderFailure>()));
    expect(() => encodeAnyJsonField(ctx, 1, null, null, null, AnyOpaqueJson(meta, null)), throwsA(isA<BaboonEncoderFailure>()));
  });
}
