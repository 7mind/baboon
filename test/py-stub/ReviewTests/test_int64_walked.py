# `i64` / `u64` are decimal JSON strings in every position (docs/json-codecs.md), including the
# values of fields that the explicit JSON walker encodes instead of pydantic (issue #107).

import json
import unittest

from BaboonDefinitions.Generated.baboon_codecs import BaboonCodecContext
from BaboonDefinitions.Generated.review.python.int64walked.IntKey import IntKey
from BaboonDefinitions.Generated.review.python.int64walked.WalkedInts import WalkedInts, WalkedInts_JsonCodec

BIG = 2**62 + 1
UBIG = 2**64 - 1


class Int64WalkedTest(unittest.TestCase):
    def setUp(self):
        key = IntKey(v="a")
        self.value = WalkedInts(n=BIG, ki={key: BIG}, ku={key: UBIG}, kl={key: [BIG, -BIG]}, ko={key: UBIG})
        self.key = str(key)

    def test_walked_int64_values_are_strings(self):
        encoded = json.loads(WalkedInts_JsonCodec.instance().encode(BaboonCodecContext.Default, self.value))
        self.assertEqual({
            "n": str(BIG),
            "ki": {self.key: str(BIG)},
            "ku": {self.key: str(UBIG)},
            "kl": {self.key: [str(BIG), str(-BIG)]},
            "ko": {self.key: str(UBIG)},
        }, encoded)

    def test_walked_int64_round_trip(self):
        codec = WalkedInts_JsonCodec.instance()
        decoded = codec.decode(BaboonCodecContext.Default, codec.encode(BaboonCodecContext.Default, self.value))
        self.assertEqual(self.value, decoded)


if __name__ == "__main__":
    unittest.main()
