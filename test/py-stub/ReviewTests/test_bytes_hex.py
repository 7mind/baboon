# JSON wire form of `bytes` in every position (issues #102 / #103): hex on encode; on decode hex
# digits with ASCII space, \t, \n, \v, \f, \r, ':' and '-' ignored anywhere, everything else a
# decode failure. Covers container elements and map keys of a DTO without a direct `bytes`
# field, the explicit JSON walker, and the service wiring / client scalar path.

import json
import unittest

from BaboonDefinitions.Generated.baboon_codecs import BaboonCodecContext
from BaboonDefinitions.Generated.baboon_service_wiring import BaboonMethodId, BaboonWiringException, DecoderFailed
from BaboonDefinitions.Generated.review.python.byteshex.BlobKey import BlobKey
from BaboonDefinitions.Generated.review.python.byteshex.BlobShape import Dot
from BaboonDefinitions.Generated.review.python.byteshex.BlobService_Client import BlobServiceClient
from BaboonDefinitions.Generated.review.python.byteshex.BlobService_Wiring import invoke_json_BlobService
from BaboonDefinitions.Generated.review.python.byteshex.NestedBlobs import NestedBlobs, NestedBlobs_JsonCodec
from BaboonDefinitions.Generated.review.python.byteshex.WalkedBlobs import WalkedBlobs, WalkedBlobs_JsonCodec

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

BLOB = b"\x00\xff\x80"
CTX = BaboonCodecContext.Compact
KEY = BlobKey(v="k")


def nested_wire(ob=None, lb=(), sb=(), mv=None, mk=None):
    return json.dumps({"ob": ob, "lb": list(lb), "sb": list(sb), "mv": mv or {}, "mk": mk or {}})


def walked_wire(b="", mv=None, ma=None):
    return json.dumps({"b": b, "mv": mv or {}, "ma": ma or {}})


def decode_nested(wire):
    return NestedBlobs_JsonCodec.instance().decode(CTX, wire)


def decode_walked(wire):
    return WalkedBlobs_JsonCodec.instance().decode(CTX, wire)


DECODERS = {
    "opt element": lambda s: decode_nested(nested_wire(ob=s)).ob,
    "lst element": lambda s: decode_nested(nested_wire(lb=[s])).lb[0],
    "set element": lambda s: next(iter(decode_nested(nested_wire(sb=[s])).sb)),
    "map value": lambda s: decode_nested(nested_wire(mv={"k": s})).mv["k"],
    "map key": lambda s: next(iter(decode_nested(nested_wire(mk={s: "v"})).mk)),
    "walked field": lambda s: decode_walked(walked_wire(b=s)).b,
    "walked map value": lambda s: decode_walked(walked_wire(mv={str(KEY): s})).mv[KEY],
    "walked map key": lambda s: next(iter(decode_walked(walked_wire(ma={s: {"Dot": {"r": 1}}})).ma)),
}


class EchoBlobs:
    def blob(self, value):
        return value


class BytesHexTest(unittest.TestCase):
    def test_nested_bytes_encode_as_hex(self):
        value = NestedBlobs(ob=BLOB, lb=[BLOB], sb={BLOB}, mv={"k": BLOB}, mk={BLOB: "v"})
        encoded = json.loads(NestedBlobs_JsonCodec.instance().encode(CTX, value))
        self.assertEqual({"ob": "00ff80", "lb": ["00ff80"], "sb": ["00ff80"], "mv": {"k": "00ff80"}, "mk": {"00ff80": "v"}}, encoded)
        self.assertEqual(value, decode_nested(json.dumps(encoded)))

    def test_walked_bytes_encode_as_hex(self):
        value = WalkedBlobs(b=BLOB, mv={KEY: BLOB}, ma={BLOB: Dot(r=1)})
        encoded = json.loads(WalkedBlobs_JsonCodec.instance().encode(CTX, value))
        self.assertEqual({"b": "00ff80", "mv": {str(KEY): "00ff80"}, "ma": {"00ff80": {"Dot": {"r": 1}}}}, encoded)
        self.assertEqual(value, decode_walked(json.dumps(encoded)))

    def test_every_position_accepts_valid_hex(self):
        for position, decode in DECODERS.items():
            for text, expected in VALID:
                with self.subTest(position=position, text=text):
                    self.assertEqual(expected, decode(text))

    def test_every_position_rejects_invalid_hex(self):
        for position, decode in DECODERS.items():
            for text in INVALID:
                with self.subTest(position=position, text=text), self.assertRaises(ValueError):
                    decode(text)

    def test_service_wiring_decodes_hex(self):
        method = BaboonMethodId("BlobService", "blob")
        for text, expected in VALID:
            with self.subTest(text=text):
                self.assertEqual(json.dumps(expected.hex()), invoke_json_BlobService(method, json.dumps(text), EchoBlobs(), CTX))
        for text in INVALID:
            with self.subTest(text=text), self.assertRaises(BaboonWiringException) as caught:
                invoke_json_BlobService(method, json.dumps(text), EchoBlobs(), CTX)
            self.assertIsInstance(caught.exception.error, DecoderFailed)

    def test_service_client_decodes_hex(self):
        for text, expected in VALID:
            with self.subTest(text=text):
                client = BlobServiceClient(lambda *_: b"", lambda _s, _m, _d, text=text: json.dumps(text), CTX)
                self.assertEqual(expected, client.blob_json(BLOB))
        for text in INVALID:
            with self.subTest(text=text), self.assertRaises(ValueError):
                BlobServiceClient(lambda *_: b"", lambda _s, _m, _d, text=text: json.dumps(text), CTX).blob_json(BLOB)


if __name__ == "__main__":
    unittest.main()
