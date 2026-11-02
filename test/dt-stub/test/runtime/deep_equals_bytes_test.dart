import 'dart:typed_data';

import 'package:baboon_runtime/baboon_runtime.dart';
import 'package:test/test.dart';

// `bytes` is a `Uint8List`, whose `==` is identity: equal content in distinct instances must
// still compare equal as a map key or set element.
void main() {
  Uint8List b(List<int> v) => Uint8List.fromList(v);

  test('maps keyed by bytes compare by key content', () {
    final a = {b([0xAB, 0xCD]): 'v', b([0x01]): 'w'};
    expect(baboonDeepEquals(a, {b([0x01]): 'w', b([0xAB, 0xCD]): 'v'}), isTrue);
    expect(baboonDeepHashCode(a), baboonDeepHashCode({b([0x01]): 'w', b([0xAB, 0xCD]): 'v'}));
    expect(baboonDeepEquals(a, {b([0xAB, 0xCD]): 'x', b([0x01]): 'w'}), isFalse);
    expect(baboonDeepEquals(a, {b([0xAB, 0xCE]): 'v', b([0x01]): 'w'}), isFalse);
  });

  test('sets of bytes compare by element content', () {
    expect(baboonDeepEquals({b([1, 2]), b([3])}, {b([3]), b([1, 2])}), isTrue);
    expect(baboonDeepEquals({b([1, 2]), b([3])}, {b([3]), b([1, 3])}), isFalse);
  });
}
