# Builtin map keys inside fields that the explicit JSON walker encodes (an ADT map value) must be
# turned into JSON object keys; `json.dumps` rejects datetime, UUID and Decimal keys.

import json
import unittest
from datetime import datetime, timedelta, timezone
from decimal import Decimal
from uuid import UUID

from BaboonDefinitions.Generated.baboon_codecs import BaboonCodecContext
from BaboonDefinitions.Generated.review.python.walkedkeys.KeyedShape import Spot
from BaboonDefinitions.Generated.review.python.walkedkeys.WalkedKeys import WalkedKeys, WalkedKeys_JsonCodec


class WalkedKeysTest(unittest.TestCase):
    def test_builtin_keys_round_trip(self):
        spot = Spot(r=1)
        value = WalkedKeys(
            kt={datetime(2026, 1, 2, 3, 4, 5, 6000, tzinfo=timezone.utc): spot},
            ko={datetime(2026, 1, 2, 3, 4, 5, tzinfo=timezone(timedelta(hours=2))): spot},
            ku={UUID("00112233-4455-6677-8899-aabbccddeeff"): spot},
            kd={Decimal("1.5"): spot},
            kl={2**62 + 1: spot},
            kb={True: spot},
        )
        codec = WalkedKeys_JsonCodec.instance()
        encoded = codec.encode(BaboonCodecContext.Default, value)
        self.assertEqual(["00112233-4455-6677-8899-aabbccddeeff"], list(json.loads(encoded)["ku"]))
        self.assertEqual(["1.5"], list(json.loads(encoded)["kd"]))
        self.assertEqual(value, codec.decode(BaboonCodecContext.Default, encoded))


if __name__ == "__main__":
    unittest.main()
