# PR-F (M24) — cross-language malformed map-key error consistency.
#
# Verifies that decoding a JSON object whose map-key cannot be parsed back into the
# id type raises BaboonCodecException with message containing "malformed key".
# Replaces the prior silent `.value` attribute access on a Left.
#
# Uses the my.ok.m19.singleid fixture (id ItemId { v: uid }; root data Holder { m: map[ItemId, str] }).
# Generated symbols are produced by mdl :test-gen-regular-adt under target/test-regular/py-stub/.

import unittest

from BaboonDefinitions.Generated.baboon_exceptions import BaboonCodecException
from BaboonDefinitions.Generated.baboon_codecs import BaboonCodecContext
from BaboonDefinitions.Generated.my.ok.m19.singleid.Holder import Holder_JsonCodec


class MapKeyMalformedRuntimeTest(unittest.TestCase):
    def test_holder_json_decode_raises_decoder_failure_for_malformed_map_key(self):
        bad_json = '{"m":{"not_a_valid_id":"v"}}'
        ctx = BaboonCodecContext.Compact
        with self.assertRaises(BaboonCodecException) as cm:
            Holder_JsonCodec.instance().decode(ctx, bad_json)
        self.assertIn("malformed key", str(cm.exception))
        self.assertIn("DecoderFailure:", str(cm.exception))


if __name__ == "__main__":
    unittest.main()
