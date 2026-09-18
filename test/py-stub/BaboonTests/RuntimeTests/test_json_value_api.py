import copy
import json
import unittest

from BaboonDefinitions.Generated.baboon_any_opaque import AnyMeta, AnyOpaqueJson
from BaboonDefinitions.Generated.baboon_codecs import BaboonCodecContext
from BaboonDefinitions.Generated.baboon_service_wiring import BaboonRight
from BaboonDefinitions.Generated.my.ok.Holder import Holder
from BaboonDefinitions.Generated.my.ok.Inner import Inner
from BaboonDefinitions.Generated.reserved.words.KindEnum import KindEnum, KindEnum_JsonCodec
from BaboonDefinitions.Generated.reserved.words.domain_facade import DomainReservedWordsFacade


class JsonValueApiTest(unittest.TestCase):
    def test_pydantic_codec_value_adapter(self):
        codec = Inner.codec_json()
        value = Inner(x=42)
        ctx = BaboonCodecContext.Default
        self.assertEqual({"x": 42}, codec.encode_value(ctx, value))
        self.assertEqual(value, codec.decode_value(ctx, {"x": 42}))
        self.assertEqual(json.loads(codec.encode(ctx, value)), codec.encode_value(ctx, value))

    def test_enum_native_string_is_not_json_text(self):
        codec = KindEnum_JsonCodec.instance()
        self.assertEqual("Type", codec.encode_value(BaboonCodecContext.Default, KindEnum.Type))
        self.assertEqual(KindEnum.Type, codec.decode_value(BaboonCodecContext.Default, "Type"))
        facade = DomainReservedWordsFacade()
        meta = AnyMeta(7, "reserved.words", "1.0.0", "reserved.words/:#KindEnum")
        result = facade.decode_any_value(AnyOpaqueJson(meta, "Type"))
        self.assertIsInstance(result, BaboonRight)
        self.assertEqual(KindEnum.Type, result.value)
        legacy = facade.decode_any(AnyOpaqueJson(meta, '"Type"'))
        self.assertIsInstance(legacy, BaboonRight)
        self.assertEqual(KindEnum.Type, legacy.value)

    def test_explicit_walker_preserves_native_values_and_input(self):
        for payload in (None, "literal", {"nested": [None, "text"]}):
            with self.subTest(payload=payload):
                value = Holder(
                    fAny=AnyOpaqueJson(AnyMeta(7, "my.ok", "1.0.0", "my.ok/:#Inner"), payload),
                    fDomainThis=AnyOpaqueJson(AnyMeta(3, None, "1.0.0", "my.ok/:#Inner"), payload),
                    fDomainCurrent=AnyOpaqueJson(AnyMeta(1, None, None, "my.ok/:#Inner"), payload),
                    fUnderlying=AnyOpaqueJson(AnyMeta(6, "my.ok", "1.0.0", None), payload),
                    fThisUnderlying=AnyOpaqueJson(AnyMeta(2, None, "1.0.0", None), payload),
                    fCurrentUnderlying=AnyOpaqueJson(AnyMeta(0, None, None, None), payload),
                    fOpt=None, fLst=[], fMapValue={},
                )
                codec = Holder.codec_json()
                ctx = BaboonCodecContext.Default
                wire = codec.encode_value(ctx, value)
                original = copy.deepcopy(wire)
                self.assertEqual(json.loads(codec.encode(ctx, value)), wire)
                self.assertEqual(value, codec.decode_value(ctx, wire))
                self.assertEqual(original, wire)
