# JSON wire form of `bytes` (issues #102 / #103): hex digits, with ASCII space, \t, \n, \v,
# \f, \r, ':' and '-' ignored anywhere; anything else, or an odd digit count, is a decode
# failure. Every backend runs the same vectors.
#
# The codec-level cases use testpkg.pkg0 T1_D1 (f4: bytes, f5: opt[bytes]); generated symbols
# are produced by mdl :test-gen-regular-adt.

import json
import unittest

from BaboonDefinitions.Generated.baboon_codecs import BaboonCodecContext
from BaboonDefinitions.Generated.baboon_runtime_shared import bytes_from_hex
from BaboonDefinitions.Generated.testpkg.pkg0.T1_D1 import T1_D1_JsonCodec

VALID = [
    ("", b""),
    ("ABCD", b"\xab\xcd"),
    ("abcd", b"\xab\xcd"),
    ("aBcD", b"\xab\xcd"),
    ("AB CD", b"\xab\xcd"),
    ("AB:CD", b"\xab\xcd"),
    ("AB-CD", b"\xab\xcd"),
    ("A B C D", b"\xab\xcd"),
    ("\tAB\r\nCD ", b"\xab\xcd"),
    ("A:B-C D", b"\xab\xcd"),
    ("\vA\fB", b"\xab"),
    ("00FF7f", b"\x00\xff\x7f"),
    (" - : ", b""),
]

INVALID = [
    "ZZ", "AQ", "+1", "0x01", "q80=", "ABC", "A", "-1",
    "٣٣", "ＡＡ", "aéb", "AB CD",
    "AB\t1CD", "AB1\nCD",
]


def t1_d1_wire(f4, f5=None):
    return json.dumps({"f0": {"f": "x", "f1": None}, "f1": 1, "f3": "s", "f4": f4, "f5": f5})


class BytesHexTest(unittest.TestCase):
    ctx = BaboonCodecContext.Compact

    def test_runtime_accepts_valid_hex(self):
        for text, expected in VALID:
            with self.subTest(text=text):
                self.assertEqual(expected, bytes_from_hex(text))

    def test_runtime_rejects_invalid_hex(self):
        for text in INVALID:
            with self.subTest(text=text), self.assertRaises(ValueError):
                bytes_from_hex(text)

    def test_json_codec_accepts_valid_hex(self):
        for text, expected in VALID:
            with self.subTest(text=text):
                decoded = T1_D1_JsonCodec.instance().decode(self.ctx, t1_d1_wire(text, text))
                self.assertEqual(expected, decoded.f4)
                self.assertEqual(expected, decoded.f5)

    def test_json_codec_rejects_invalid_hex(self):
        for text in INVALID:
            with self.subTest(field="f4", text=text), self.assertRaises(ValueError):
                T1_D1_JsonCodec.instance().decode(self.ctx, t1_d1_wire(text))
            with self.subTest(field="f5", text=text), self.assertRaises(ValueError):
                T1_D1_JsonCodec.instance().decode(self.ctx, t1_d1_wire("", text))

    def test_json_codec_writes_lowercase_hex(self):
        decoded = T1_D1_JsonCodec.instance().decode(self.ctx, t1_d1_wire("00FF80", "AB"))
        encoded = json.loads(T1_D1_JsonCodec.instance().encode(self.ctx, decoded))
        self.assertEqual("00ff80", encoded["f4"])
        self.assertEqual("ab", encoded["f5"])


if __name__ == "__main__":
    unittest.main()
