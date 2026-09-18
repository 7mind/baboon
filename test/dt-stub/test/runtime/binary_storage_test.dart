import 'dart:typed_data';
import 'package:baboon_runtime/baboon_runtime.dart';
import 'package:test/test.dart';

void main() {
  test('zero-capacity writers grow and writer append does not expose storage', () {
    final source = BaboonBinWriter(0)..writeI32(42);
    final target = BaboonBinWriter(1)..writeU8(99);
    target.writeBuffer(source);
    source.writeU8(77);
    expect(target.toBytes(), [99, 42, 0, 0, 0]);
    target.writeBuffer(target);
    expect(target.toBytes(), [99, 42, 0, 0, 0, 99, 42, 0, 0, 0]);
  });

  test('primitive writes preserve little-endian bytes across capacity growth', () {
    final writer = BaboonBinWriter(1);
    writer.writeI8(-128);
    writer.writeU16(0xffff);
    writer.writeI16(-32768);
    writer.writeU32(0xffffffff);
    writer.writeI32(-2147483648);
    writer.writeU64(-1);
    writer.writeI64(-9223372036854775808);
    writer.writeF32(-0.0);
    writer.writeF64(double.infinity);
    expect(writer.toBytes(), [
      0x80, 0xff, 0xff, 0, 0x80, 0xff, 0xff, 0xff, 0xff, 0, 0, 0, 0x80,
      0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
      0, 0, 0, 0, 0, 0, 0, 0x80,
      0, 0, 0, 0x80, 0, 0, 0, 0, 0, 0, 0xf0, 0x7f,
    ]);
    final snapshot = writer.toBytes();
    snapshot[0] = 0;
    writer.writeU8(42);
    expect(writer.toBytes()[0], 0x80);
    expect(snapshot.length, 41);
  });

  test('reader extractions own their bytes and honor nonzero-offset input', () {
    final backing = Uint8List.fromList([99, 2, 0, 0, 0, 10, 20, 30, 99]);
    final reader = BaboonBinReader(Uint8List.sublistView(backing, 1, 8));
    final bytes = reader.readBytes();
    final tail = reader.readNBytes(1);
    backing[5] = 88;
    bytes[1] = 77;
    expect(bytes, [10, 77]);
    expect(backing[6], 20);
    expect(tail, [30]);
    expect(reader.position, 7);
  });

  test('malformed raw extraction leaves the cursor at its original position', () {
    final reader = BaboonBinReader(Uint8List.fromList([1]));
    expect(() => reader.readNBytes(2), throwsRangeError);
    expect(reader.position, 0);
  });
}
