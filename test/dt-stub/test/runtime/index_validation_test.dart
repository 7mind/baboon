import 'package:baboon_runtime/baboon_runtime.dart';
import 'package:test/test.dart';

class TwoEntries with BaboonBinCodecIndexed {
  @override
  int get indexElementsCount => 2;
}

void main() {
  test('invalid entries throw normal errors without relying on assertions', () {
    for (final entries in [
      [0, 0, 1, 1],
      [-1, 1, 1, 1],
      [0, -1, 1, 1],
      [0, 2, 1, 1],
      [2147483647, 1, 0, 1]
    ]) {
      final writer = BaboonBinWriter()..writeU8(1);
      for (final value in entries) {
        writer.writeI32(value);
      }
      expect(
          () => TwoEntries().readIndex(
              BaboonCodecContext.compact, BaboonBinReader(writer.toBytes())),
          throwsFormatException);
    }
  });

  test('valid entries retain gaps and leave the payload unconsumed', () {
    final writer = BaboonBinWriter()..writeU8(1);
    for (final value in [1, 2, 5, 3]) {
      writer.writeI32(value);
    }
    writer.writeU8(42);
    final reader = BaboonBinReader(writer.toBytes());
    final entries = TwoEntries().readIndex(BaboonCodecContext.compact, reader);
    expect(entries.map((entry) => [entry.offset, entry.length]), [
      [1, 2],
      [5, 3]
    ]);
    expect(reader.readU8(), 42);
  });
}
