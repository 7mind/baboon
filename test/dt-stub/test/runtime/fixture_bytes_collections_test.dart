import 'dart:math';
import 'dart:typed_data';

import 'package:baboon_runtime/baboon_fixture.dart';
import 'package:test/test.dart';

class _MaxIntRandom implements Random {
  @override
  bool nextBool() => false;

  @override
  double nextDouble() => 0.0;

  @override
  int nextInt(int max) => max - 1;
}

void main() {
  Uint8List key() => Uint8List.fromList([136]);

  test(
      'map fixtures deduplicate byte keys by content and retain the last value',
      () {
    final random = BaboonRandom(_MaxIntRandom());
    var value = 0;
    final result = random.mkMap(key, () => ++value);

    expect(result, hasLength(1));
    expect(result[key()], value);
  });

  test('set fixtures deduplicate byte elements by content', () {
    final random = BaboonRandom(_MaxIntRandom());
    final result = random.mkSet(key);

    expect(result, hasLength(1));
    expect(result.contains(key()), isTrue);
  });
}
