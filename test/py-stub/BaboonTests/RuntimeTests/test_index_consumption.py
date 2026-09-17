import struct
import unittest
from io import BytesIO

from BaboonDefinitions.Generated.baboon_codecs import BaboonBinCodecIndexed, BaboonCodecContext
from BaboonDefinitions.Generated.baboon_runtime_shared import LEDataInputStream


class TwoEntries(BaboonBinCodecIndexed):
    def index_elements_count(self, ctx):
        return 2


class IndexConsumptionTest(unittest.TestCase):
    def test_consumption_matches_materialization(self):
        codec = TwoEntries()
        for wire, count in ((b"\x00payload", 0), (b"\x01" + struct.pack("<IIII", 0, 2, 2, 3) + b"payload", 2)):
            with self.subTest(count=count):
                materialized = LEDataInputStream(BytesIO(wire))
                consumed = LEDataInputStream(BytesIO(wire))
                entries = codec.read_index(BaboonCodecContext.Compact, materialized)
                self.assertEqual(count, len(entries))
                self.assertEqual(count, codec.consume_index(BaboonCodecContext.Compact, consumed))
                self.assertEqual(materialized.stream.tell(), consumed.stream.tell())
                self.assertEqual(b"payload", consumed.stream.read())

    def test_both_paths_reject_invalid_indices(self):
        if not __debug__:
            self.skipTest("index validation uses assertions")
        codec = TwoEntries()
        for fields in ((0, 0, 0, 1), (0, 3, 2, 1)):
            for read in (codec.read_index, codec.consume_index):
                with self.subTest(fields=fields, read=read.__name__):
                    with self.assertRaises(AssertionError):
                        read(BaboonCodecContext.Compact, LEDataInputStream(BytesIO(b"\x01" + struct.pack("<IIII", *fields))))

    def test_both_paths_reject_truncated_indices(self):
        codec = TwoEntries()
        for read in (codec.read_index, codec.consume_index):
            with self.assertRaises(struct.error):
                read(BaboonCodecContext.Compact, LEDataInputStream(BytesIO(b"\x01\x00")))
