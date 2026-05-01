// PR-F (M24) — cross-language malformed map-key error consistency.
//
// Verifies that decoding a JSON object whose map-key cannot be parsed back into the
// id type throws BaboonDecoderFailure with message containing "malformed key".
// Replaces the prior unchecked `as BaboonRight<...>` cast.
//
// Uses the my.ok.m19.singleid fixture; generated symbols are produced by
// mdl :test-gen-regular-adt under target/test-regular/dt-stub/.

import 'package:baboon_runtime/baboon_runtime.dart';
import 'package:dt_stub/my/ok/m19/singleid/holder.dart';
import 'package:test/test.dart';

void main() {
  group('PR-F (M24) — malformed map-key throws BaboonDecoderFailure', () {
    test('Holder JSON decode throws BaboonDecoderFailure for malformed map key', () {
      final ctx = BaboonCodecContext.compact;
      final badJson = {
        'm': {'not_a_valid_id': 'v'},
      };
      expect(
        () => Holder_JsonCodec.instance.decode(ctx, badJson),
        throwsA(isA<BaboonDecoderFailure>()
            .having((e) => e.message, 'message', contains('malformed key'))),
      );
    });
  });
}
