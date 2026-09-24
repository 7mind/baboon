# 64-bit integers are written as decimal strings (docs/json-codecs.md, "64-bit integers").
# Readers stay lenient about the JSON-number form an older compiler produced, and that
# leniency is what keeps documents written before the change readable.
#
# Nothing writes numbers any more, so without this test the number arm of every decoder is
# dead as far as the suite is concerned.
#
# Uses the identifier.ok fixture (id LongId { x: i64 }; id UInts { a: u08, b: u16, c: u32,
# d: u64 }); generated symbols are produced by mdl :test-gen-regular-adt.

import unittest

from BaboonDefinitions.Generated.baboon_codecs import BaboonCodecContext
from BaboonDefinitions.Generated.identifier.ok.LongId import LongId_JsonCodec
from BaboonDefinitions.Generated.identifier.ok.UInts import UInts_JsonCodec


class LegacyInt64WireTest(unittest.TestCase):
    ctx = BaboonCodecContext.Compact

    def test_i64_decodes_from_the_legacy_numeric_form(self):
        decoded = LongId_JsonCodec.instance().decode(self.ctx, '{"x":-9007199254740991}')
        self.assertEqual(-9007199254740991, decoded.x)

    def test_i64_decodes_from_the_string_form(self):
        decoded = LongId_JsonCodec.instance().decode(self.ctx, '{"x":"-9223372036854775808"}')
        self.assertEqual(-9223372036854775808, decoded.x)

    def test_u64_decodes_from_the_legacy_numeric_form(self):
        decoded = UInts_JsonCodec.instance().decode(self.ctx, '{"a":1,"b":2,"c":3,"d":42}')
        self.assertEqual(42, decoded.d)

    def test_u64_decodes_from_the_string_form(self):
        decoded = UInts_JsonCodec.instance().decode(self.ctx, '{"a":1,"b":2,"c":3,"d":"42"}')
        self.assertEqual(42, decoded.d)


if __name__ == "__main__":
    unittest.main()
