import json
import unittest
from datetime import datetime, timezone, timedelta
from decimal import Decimal
from io import BytesIO
from uuid import UUID

from BaboonDefinitions.Generated.baboon_codecs import BaboonCodecContext
from BaboonDefinitions.Generated.baboon_runtime_shared import LEDataOutputStream
from BaboonDefinitions.Generated.baboon_service_wiring import BaboonMethodId, BaboonWiringException, DecoderFailed
from BaboonDefinitions.Generated.review.python.scalars.ScalarValues import ScalarValues
from BaboonDefinitions.Generated.review.python.scalars.ScalarService_Wiring import invoke_json_ScalarService, invoke_ueba_ScalarService


class ScalarProbe:
    def __init__(self, values, validate):
        self.values = values
        self.validate = validate

    def _invoke(self, name, value):
        expected = getattr(self.values, name)
        if self.validate and not isinstance(value, type(expected)):
            raise AssertionError(f"{name}: expected {type(expected).__name__}, got {type(value).__name__}")
        return expected

    def blob(self, value): return self._invoke("blob", value)
    def identifier(self, value): return self._invoke("identifier", value)
    def amount(self, value): return self._invoke("amount", value)
    def timestamp(self, value): return self._invoke("timestamp", value)
    def offset(self, value): return self._invoke("offset", value)


class ScalarRpcTest(unittest.TestCase):
    def setUp(self):
        self.values = ScalarValues(
            blob=b"\x00\xff\x80", identifier=UUID("00112233-4455-6677-8899-aabbccddeeff"),
            amount=Decimal("123.456789"), timestamp=datetime(2026, 9, 16, 12, 30, tzinfo=timezone.utc),
            offset=datetime(2026, 9, 16, 12, 30, tzinfo=timezone(timedelta(hours=2))),
        )
        self.fields = json.loads(self.values.codec_json().encode(BaboonCodecContext.Default, self.values))

    def test_json_decode_supplies_declared_native_types(self):
        probe = ScalarProbe(self.values, True)
        for name, field in self.fields.items():
            with self.subTest(name=name):
                actual = invoke_json_ScalarService(BaboonMethodId("ScalarService", name), json.dumps(field), probe, BaboonCodecContext.Default)
                self.assertEqual(field, json.loads(actual))

    def test_json_encode_matches_equivalent_dto_fields(self):
        probe = ScalarProbe(self.values, False)
        for name, field in self.fields.items():
            with self.subTest(name=name):
                actual = invoke_json_ScalarService(BaboonMethodId("ScalarService", name), json.dumps(field), probe, BaboonCodecContext.Default)
                self.assertEqual(field, json.loads(actual))

    def test_ueba_preserves_existing_scalar_encoding(self):
        probe = ScalarProbe(self.values, True)
        methods = {"blob": "write_bytes", "identifier": "write_uuid", "amount": "write_f128", "timestamp": "write_datetime", "offset": "write_datetime"}
        for name, method in methods.items():
            with self.subTest(name=name):
                stream = BytesIO()
                getattr(LEDataOutputStream(stream), method)(getattr(self.values, name))
                wire = stream.getvalue()
                self.assertEqual(wire, invoke_ueba_ScalarService(BaboonMethodId("ScalarService", name), wire, probe, BaboonCodecContext.Default))

    def test_malformed_scalar_json_is_a_decoder_failure(self):
        for name in self.fields:
            with self.subTest(name=name), self.assertRaises(BaboonWiringException) as caught:
                invoke_json_ScalarService(BaboonMethodId("ScalarService", name), "{", ScalarProbe(self.values, False), BaboonCodecContext.Default)
            self.assertIsInstance(caught.exception.error, DecoderFailed)
