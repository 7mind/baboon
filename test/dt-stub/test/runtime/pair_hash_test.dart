import 'package:baboon_runtime/baboon_runtime.dart';
import 'package:test/test.dart';

void main() {
  test('fixed-pair hashing preserves the iterable hash algorithm', () {
    final values = <Object?>[null, 0, -1, 9223372036854775807, '', 'x', true];
    for (final left in values) {
      for (final right in values) {
        expect(Object.hash(left, right), Object.hashAll([left, right]));
      }
    }
    const method = BaboonMethodId('service', 'method');
    expect(method.hashCode, Object.hashAll(['service', 'method']));
    final data = {'one': [1, 2], 'two': <String>{'a', 'b'}};
    expect(baboonDeepHashCode(data), Object.hashAllUnordered(data.entries.map(
      (entry) => Object.hashAll([baboonDeepHashCode(entry.key), baboonDeepHashCode(entry.value)]),
    )));
  });
}
