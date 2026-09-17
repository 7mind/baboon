import json
import unittest
from io import BytesIO

from BaboonDefinitions.Generated.baboon_any_opaque import AnyMeta, AnyOpaqueJson, AnyOpaqueUeba
from BaboonDefinitions.Generated.baboon_codecs import BaboonCodecContext
from BaboonDefinitions.Generated.baboon_runtime_shared import LEDataOutputStream
from BaboonDefinitions.Generated.review.python.fields.ContractPayload import ContractPayload
from BaboonDefinitions.Generated.review.python.fields.Inner import Inner
from BaboonDefinitions.Generated.review.python.fields.domain_facade import DomainReviewPythonFieldsFacade


class FieldPlanTest(unittest.TestCase):
    def test_walked_contract_field_bypasses_pydantic_binary_serialization(self):
        inner = Inner(value=255)
        stream = BytesIO()
        Inner.codec_ueba().encode(BaboonCodecContext.Default, LEDataOutputStream(stream), inner)
        value = ContractPayload(payload=AnyOpaqueUeba(
            AnyMeta(7, inner.baboon_domain_identifier, inner.baboon_domain_version, inner.baboon_type_identifier),
            stream.getvalue(),
        ))
        ctx = BaboonCodecContext.with_facade(False, DomainReviewPythonFieldsFacade())
        wire = ContractPayload.codec_json().encode(ctx, value)
        self.assertEqual({"value": 255}, json.loads(wire)["payload"]["$c"])

    def test_walked_contract_field_uses_backing_attribute(self):
        value = ContractPayload(payload=AnyOpaqueJson(AnyMeta(7, "example", "1.0.0", "example/:#Value"), {"x": 42}))
        codec = ContractPayload.codec_json()
        wire = codec.encode(BaboonCodecContext.Default, value)
        self.assertEqual({"payload"}, set(json.loads(wire)))
        self.assertEqual(value, codec.decode(BaboonCodecContext.Default, wire))
