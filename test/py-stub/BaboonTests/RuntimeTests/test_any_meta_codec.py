# NOTE: This test references generated runtime symbols (AnyMeta, AnyMetaCodec,
# BaboonCodecsFacade, BaboonCodecException, ...) which are copied into this stub
# only by the py-stub codegen path (rsync + codegen into target/test-regular/py-stub/).
# Running directly from the source tree may fail with missing symbols; run from the
# codegen'd copy with `python3 -m unittest discover -s BaboonTests/RuntimeTests`.

import json
import unittest
from io import BytesIO

from BaboonDefinitions.Generated.baboon_any_opaque import (
    ANY_CONTENT_KEY,
    ANY_DOMAIN_KEY,
    ANY_KIND_KEY,
    ANY_TYPEID_KEY,
    ANY_VERSION_KEY,
    AnyMeta,
    AnyMetaCodec,
    AnyOpaqueJson,
    AnyOpaqueUeba,
)
from BaboonDefinitions.Generated.baboon_codecs import (
    AbstractBaboonJsonCodecs,
    AbstractBaboonUebaCodecs,
    BaboonCodecContext,
)
from BaboonDefinitions.Generated.baboon_codecs_facade import BaboonCodecsFacade
from BaboonDefinitions.Generated.baboon_exceptions import BaboonCodecException
from BaboonDefinitions.Generated.baboon_runtime_shared import (
    BaboonDomainVersion,
    BaboonMeta,
    LEDataInputStream,
    LEDataOutputStream,
)
from BaboonDefinitions.Generated.baboon_service_wiring import (
    BaboonLeft,
    BaboonRight,
)


# (kind, domain?, version?, typeid?) for all six locked meta-kind bytes.
ALL_VARIANTS = [
    (0x07, "com.example.dom", "1.2.3", "MyType"),  # A
    (0x03, None, "1.2.3", "MyType"),                # B
    (0x01, None, None, "MyType"),                   # C
    (0x06, "com.example.dom", "1.2.3", None),       # D1
    (0x02, None, "1.2.3", None),                    # D2
    (0x00, None, None, None),                       # D3
]


def _bytes(meta: AnyMeta) -> bytes:
    buf = BytesIO()
    out = LEDataOutputStream(buf)
    AnyMetaCodec.write_bin(out, meta)
    return buf.getvalue()


class AnyMetaConstructionInvariantsTest(unittest.TestCase):
    # Bitmask consistency: kind bit set <=> field present. 6 valid combinations.

    def test_variant_a_constructs(self):
        m = AnyMeta(0x07, "d", "v", "t")
        self.assertEqual(m.kind, 0x07)

    def test_variant_b_constructs(self):
        m = AnyMeta(0x03, None, "v", "t")
        self.assertIsNone(m.domain)

    def test_variant_c_constructs(self):
        m = AnyMeta(0x01, None, None, "t")
        self.assertEqual(m.typeid, "t")

    def test_variant_d1_constructs(self):
        m = AnyMeta(0x06, "d", "v", None)
        self.assertIsNone(m.typeid)

    def test_variant_d2_constructs(self):
        m = AnyMeta(0x02, None, "v", None)
        self.assertEqual(m.version, "v")

    def test_variant_d3_constructs(self):
        m = AnyMeta(0x00, None, None, None)
        self.assertEqual(m.kind, 0x00)

    def test_kind_0x07_requires_domain_present(self):
        with self.assertRaises(ValueError):
            AnyMeta(0x07, None, "v", "t")

    def test_kind_0x07_requires_version_present(self):
        with self.assertRaises(ValueError):
            AnyMeta(0x07, "d", None, "t")

    def test_kind_0x07_requires_typeid_present(self):
        with self.assertRaises(ValueError):
            AnyMeta(0x07, "d", "v", None)

    def test_kind_0x03_forbids_domain(self):
        with self.assertRaises(ValueError):
            AnyMeta(0x03, "d", "v", "t")

    def test_kind_0x01_forbids_version(self):
        with self.assertRaises(ValueError):
            AnyMeta(0x01, None, "v", "t")

    def test_kind_0x06_forbids_typeid(self):
        with self.assertRaises(ValueError):
            AnyMeta(0x06, "d", "v", "t")

    def test_kind_0x00_forbids_all_three(self):
        with self.assertRaises(ValueError):
            AnyMeta(0x00, "d", None, None)
        with self.assertRaises(ValueError):
            AnyMeta(0x00, None, "v", None)
        with self.assertRaises(ValueError):
            AnyMeta(0x00, None, None, "t")

    def test_reserved_kind_0x04_rejected(self):
        with self.assertRaises(ValueError):
            AnyMeta(0x04, "d", None, None)

    def test_reserved_kind_0x05_rejected(self):
        with self.assertRaises(ValueError):
            AnyMeta(0x05, "d", None, "t")

    def test_kind_above_0x07_rejected(self):
        with self.assertRaises(ValueError):
            AnyMeta(0x08, "d", "v", "t")


class AnyMetaCodecBinaryTest(unittest.TestCase):
    def test_round_trips_all_six_variants(self):
        for kind, domain, version, typeid in ALL_VARIANTS:
            with self.subTest(kind=hex(kind)):
                meta = AnyMeta(kind, domain, version, typeid)
                wire = _bytes(meta)
                reader = LEDataInputStream(BytesIO(wire))
                round_trip = AnyMetaCodec.read_bin(reader)
                self.assertEqual(round_trip, meta)

    def test_kind_byte_is_first(self):
        meta = AnyMeta(0x07, "d", "v", "t")
        wire = _bytes(meta)
        self.assertEqual(wire[0], 0x07)

    def test_d3_emits_only_one_byte(self):
        meta = AnyMeta(0x00, None, None, None)
        self.assertEqual(_bytes(meta), bytes([0x00]))

    def test_read_bin_with_length_reports_size(self):
        meta = AnyMeta(0x07, "d", "v", "t")
        wire = _bytes(meta)
        reader = LEDataInputStream(BytesIO(wire))
        round_trip, length = AnyMetaCodec.read_bin_with_length(reader)
        self.assertEqual(round_trip, meta)
        self.assertEqual(length, len(wire))

    def test_read_bin_with_length_d3_one_byte(self):
        meta = AnyMeta(0x00, None, None, None)
        reader = LEDataInputStream(BytesIO(_bytes(meta)))
        _round, length = AnyMetaCodec.read_bin_with_length(reader)
        self.assertEqual(length, 1)

    def test_forward_compat_skip_trailer_via_length(self):
        # Write meta, then 5 trailer bytes. read_bin_with_length reports the original length;
        # the caller can advance past the trailer using the reported size.
        meta = AnyMeta(0x07, "d", "v", "t")
        wire = _bytes(meta)
        full = wire + bytes([0xFF, 0xFE, 0xFD, 0xFC, 0xFB])
        reader = LEDataInputStream(BytesIO(full))
        round_trip, consumed = AnyMetaCodec.read_bin_with_length(reader)
        self.assertEqual(round_trip, meta)
        self.assertEqual(consumed, len(wire))
        # Manually skip trailer bytes from the underlying stream:
        trailer = reader.stream.read(5)
        self.assertEqual(trailer, bytes([0xFF, 0xFE, 0xFD, 0xFC, 0xFB]))


class AnyMetaCodecJsonTest(unittest.TestCase):
    def test_write_json_variant_a_has_five_keys(self):
        meta = AnyMeta(0x07, "com.x", "1.0.0", "T")
        obj = AnyMetaCodec.write_json(meta)
        self.assertEqual(set(obj.keys()), {ANY_KIND_KEY, ANY_DOMAIN_KEY, ANY_VERSION_KEY, ANY_TYPEID_KEY})
        self.assertEqual(obj[ANY_KIND_KEY], 0x07)
        self.assertEqual(obj[ANY_DOMAIN_KEY], "com.x")
        self.assertEqual(obj[ANY_VERSION_KEY], "1.0.0")
        self.assertEqual(obj[ANY_TYPEID_KEY], "T")

    def test_write_json_variant_d3_has_only_kind(self):
        meta = AnyMeta(0x00, None, None, None)
        obj = AnyMetaCodec.write_json(meta)
        self.assertEqual(set(obj.keys()), {ANY_KIND_KEY})
        self.assertEqual(obj[ANY_KIND_KEY], 0x00)

    def test_write_json_variant_b(self):
        meta = AnyMeta(0x03, None, "1.0.0", "T")
        obj = AnyMetaCodec.write_json(meta)
        self.assertEqual(set(obj.keys()), {ANY_KIND_KEY, ANY_VERSION_KEY, ANY_TYPEID_KEY})
        self.assertNotIn(ANY_DOMAIN_KEY, obj)

    def test_write_json_variant_c(self):
        meta = AnyMeta(0x01, None, None, "T")
        obj = AnyMetaCodec.write_json(meta)
        self.assertEqual(set(obj.keys()), {ANY_KIND_KEY, ANY_TYPEID_KEY})

    def test_write_json_variant_d1(self):
        meta = AnyMeta(0x06, "d", "v", None)
        obj = AnyMetaCodec.write_json(meta)
        self.assertEqual(set(obj.keys()), {ANY_KIND_KEY, ANY_DOMAIN_KEY, ANY_VERSION_KEY})

    def test_write_json_variant_d2(self):
        meta = AnyMeta(0x02, None, "v", None)
        obj = AnyMetaCodec.write_json(meta)
        self.assertEqual(set(obj.keys()), {ANY_KIND_KEY, ANY_VERSION_KEY})

    def test_round_trip_all_six_variants(self):
        for kind, domain, version, typeid in ALL_VARIANTS:
            with self.subTest(kind=hex(kind)):
                meta = AnyMeta(kind, domain, version, typeid)
                obj = AnyMetaCodec.write_json(meta)
                # Round-trip via JSON serialisation to catch dict-non-stringifiability.
                parsed = json.loads(json.dumps(obj))
                result = AnyMetaCodec.read_json(parsed)
                self.assertIsInstance(result, BaboonRight, msg=str(result))
                self.assertEqual(result.value, meta)

    def test_read_json_rejects_non_object(self):
        result = AnyMetaCodec.read_json("not-an-object")
        self.assertIsInstance(result, BaboonLeft)
        self.assertIsInstance(result.value, BaboonCodecException)

    def test_read_json_rejects_missing_kind(self):
        result = AnyMetaCodec.read_json({})
        self.assertIsInstance(result, BaboonLeft)

    def test_read_json_rejects_string_kind(self):
        result = AnyMetaCodec.read_json({ANY_KIND_KEY: "0x07"})
        self.assertIsInstance(result, BaboonLeft)

    def test_read_json_rejects_bool_kind(self):
        # bool is an int subclass in Python — must not be accepted.
        result = AnyMetaCodec.read_json({ANY_KIND_KEY: True})
        self.assertIsInstance(result, BaboonLeft)

    def test_read_json_rejects_extra_field_violating_kind(self):
        # Variant D3: kind=0x00 but with a domain key present.
        result = AnyMetaCodec.read_json({ANY_KIND_KEY: 0x00, ANY_DOMAIN_KEY: "d"})
        self.assertIsInstance(result, BaboonLeft)

    def test_read_json_rejects_missing_required_field(self):
        # Variant A: kind=0x07 but missing typeid.
        result = AnyMetaCodec.read_json({
            ANY_KIND_KEY: 0x07,
            ANY_DOMAIN_KEY: "d",
            ANY_VERSION_KEY: "v",
        })
        self.assertIsInstance(result, BaboonLeft)

    def test_read_json_rejects_reserved_kind_0x04(self):
        result = AnyMetaCodec.read_json({ANY_KIND_KEY: 0x04, ANY_DOMAIN_KEY: "d"})
        self.assertIsInstance(result, BaboonLeft)

    def test_read_json_rejects_reserved_kind_0x05(self):
        result = AnyMetaCodec.read_json({ANY_KIND_KEY: 0x05, ANY_DOMAIN_KEY: "d", ANY_TYPEID_KEY: "t"})
        self.assertIsInstance(result, BaboonLeft)


class AnyOpaqueDataclassTest(unittest.TestCase):
    def test_ueba_carries_meta_and_bytes(self):
        meta = AnyMeta(0x07, "d", "v", "t")
        op = AnyOpaqueUeba(meta, b"\x01\x02\x03")
        self.assertEqual(op.meta, meta)
        self.assertEqual(op.bytes, b"\x01\x02\x03")

    def test_json_carries_meta_and_payload(self):
        meta = AnyMeta(0x00, None, None, None)
        op = AnyOpaqueJson(meta, {"key": 42})
        self.assertEqual(op.json, {"key": 42})

    def test_ueba_equality(self):
        meta = AnyMeta(0x07, "d", "v", "t")
        a = AnyOpaqueUeba(meta, b"\x01")
        b = AnyOpaqueUeba(meta, b"\x01")
        self.assertEqual(a, b)

    def test_ueba_unequal_with_diff_bytes(self):
        meta = AnyMeta(0x07, "d", "v", "t")
        a = AnyOpaqueUeba(meta, b"\x01")
        b = AnyOpaqueUeba(meta, b"\x02")
        self.assertNotEqual(a, b)

    def test_dataclass_frozen(self):
        meta = AnyMeta(0x00, None, None, None)
        op = AnyOpaqueUeba(meta, b"")
        with self.assertRaises(Exception):
            op.bytes = b"\x99"  # frozen dataclass


class BaboonCodecContextSingletonTest(unittest.TestCase):
    def test_compact_singleton_identity(self):
        self.assertIs(BaboonCodecContext.Compact, BaboonCodecContext.Compact)
        self.assertIs(BaboonCodecContext.Compact, BaboonCodecContext.compact())

    def test_indexed_singleton_identity(self):
        self.assertIs(BaboonCodecContext.Indexed, BaboonCodecContext.Indexed)
        self.assertIs(BaboonCodecContext.Indexed, BaboonCodecContext.indexed())

    def test_default_aliases_compact(self):
        self.assertIs(BaboonCodecContext.Default, BaboonCodecContext.Compact)
        self.assertIs(BaboonCodecContext.Default, BaboonCodecContext.default())

    def test_compact_use_indices_false(self):
        self.assertFalse(BaboonCodecContext.Compact.use_indices)

    def test_indexed_use_indices_true(self):
        self.assertTrue(BaboonCodecContext.Indexed.use_indices)

    def test_compact_no_facade(self):
        self.assertIsNone(BaboonCodecContext.Compact.facade)

    def test_with_facade_threads_facade(self):
        sentinel = object()
        ctx = BaboonCodecContext.with_facade(False, sentinel)
        self.assertIs(ctx.facade, sentinel)
        self.assertFalse(ctx.use_indices)
        # New ctx — not the singleton.
        self.assertIsNot(ctx, BaboonCodecContext.Compact)


# ---- Stub codec for facade smoke tests ---------------------------------------------------


class _StubBinCodec:
    def __init__(self):
        self.baboon_domain_version = "1.0.0"
        self.baboon_domain_identifier = "smoke.dom"
        self.baboon_type_identifier = "smoke.dom/:#Stub"

    def encode(self, ctx, writer, value):
        writer.write_str(value["x"])

    def decode(self, ctx, reader):
        return {"x": reader.read_string()}


class _StubJsonCodec:
    def __init__(self):
        self.baboon_domain_version = "1.0.0"
        self.baboon_domain_identifier = "smoke.dom"
        self.baboon_type_identifier = "smoke.dom/:#Stub"

    def encode(self, ctx, value):
        # Match real codegen contract: encode returns a JSON-text string
        # (real codecs use pydantic.model_dump_json).
        return json.dumps({"x": value["x"]})

    def decode(self, ctx, value):
        # Match real codegen contract: decode takes a JSON-text string
        # (real codecs use pydantic.model_validate_json). Parse it, then
        # rebuild the stub return shape.
        parsed = json.loads(value)
        return {"x": parsed["x"]}


class _StubBinCodecs(AbstractBaboonUebaCodecs):
    def __init__(self):
        super().__init__()
        self.register("smoke.dom/:#Stub", lambda: _StubBinCodec())


class _StubJsonCodecs(AbstractBaboonJsonCodecs):
    def __init__(self):
        super().__init__()
        self.register("smoke.dom/:#Stub", lambda: _StubJsonCodec())


class _StubMeta(BaboonMeta):
    @property
    def same_in_versions(self):
        return ["1.0.0"]


def _make_facade() -> BaboonCodecsFacade:
    f = BaboonCodecsFacade()
    dv = BaboonDomainVersion("smoke.dom", "1.0.0")
    f.register(
        dv,
        codecs_json=lambda: _StubJsonCodecs(),
        codecs_bin=lambda: _StubBinCodecs(),
        meta=lambda: _StubMeta(),
    )
    return f


class BaboonCodecsFacadeAnyTest(unittest.TestCase):
    def test_decode_any_ueba_round_trip(self):
        facade = _make_facade()
        meta = AnyMeta(0x07, "smoke.dom", "1.0.0", "smoke.dom/:#Stub")
        # Encode a stub payload directly via the codec to produce the inner blob.
        inner_buf = BytesIO()
        inner_writer = LEDataOutputStream(inner_buf)
        _StubBinCodec().encode(BaboonCodecContext.Compact, inner_writer, {"x": "hello"})
        inner_bytes = inner_buf.getvalue()

        opaque = AnyOpaqueUeba(meta, inner_bytes)
        result = facade.decode_any(opaque)
        self.assertIsInstance(result, BaboonRight, msg=str(result))
        self.assertEqual(result.value, {"x": "hello"})

    def test_decode_any_json_round_trip(self):
        facade = _make_facade()
        meta = AnyMeta(0x07, "smoke.dom", "1.0.0", "smoke.dom/:#Stub")
        # decode_any passes opaque.json directly to codec.decode; real codecs call
        # pydantic.model_validate_json which requires a JSON-text string. Match that contract.
        opaque = AnyOpaqueJson(meta, json.dumps({"x": "world"}))
        result = facade.decode_any(opaque)
        self.assertIsInstance(result, BaboonRight, msg=str(result))
        self.assertEqual(result.value, {"x": "world"})

    def test_decode_any_returns_left_for_partial_meta(self):
        # Variant D3: no domain/version/typeid. decode_any has no static fallbacks.
        facade = _make_facade()
        meta = AnyMeta(0x00, None, None, None)
        opaque = AnyOpaqueJson(meta, {"x": "irrelevant"})
        result = facade.decode_any(opaque)
        self.assertIsInstance(result, BaboonLeft)

    def test_json_to_ueba_bytes_with_full_meta(self):
        facade = _make_facade()
        meta = AnyMeta(0x07, "smoke.dom", "1.0.0", "smoke.dom/:#Stub")
        result = facade.json_to_ueba_bytes(meta, {"x": "hi"})
        self.assertIsInstance(result, BaboonRight, msg=str(result))
        # Round-trip back via ueba_to_json. Real codegen's encode returns a JSON-text
        # string (pydantic model_dump_json), so ueba_to_json's BaboonRight wraps a string.
        rev = facade.ueba_to_json(meta, result.value)
        self.assertIsInstance(rev, BaboonRight)
        self.assertEqual(json.loads(rev.value), {"x": "hi"})

    def test_json_to_ueba_bytes_with_static_fallbacks(self):
        # Variant D3 wire meta + full static fallbacks.
        facade = _make_facade()
        meta = AnyMeta(0x00, None, None, None)
        result = facade.json_to_ueba_bytes(
            meta,
            {"x": "fallback"},
            static_domain="smoke.dom",
            static_version="1.0.0",
            static_typeid="smoke.dom/:#Stub",
        )
        self.assertIsInstance(result, BaboonRight, msg=str(result))

    def test_static_fallback_does_not_override_wire_meta(self):
        # Wire meta has full A; static_domain points elsewhere — wire wins.
        facade = _make_facade()
        meta = AnyMeta(0x07, "smoke.dom", "1.0.0", "smoke.dom/:#Stub")
        result = facade.json_to_ueba_bytes(
            meta,
            {"x": "wire-wins"},
            static_domain="other.dom",  # would fail if it overrode
            static_version="2.0.0",
            static_typeid="other.dom/:#Other",
        )
        self.assertIsInstance(result, BaboonRight, msg=str(result))

    def test_ueba_to_json_unknown_domain_returns_left(self):
        facade = _make_facade()
        meta = AnyMeta(0x07, "unknown.dom", "1.0.0", "unknown.dom/:#X")
        result = facade.ueba_to_json(meta, b"\x00")
        self.assertIsInstance(result, BaboonLeft)

    def test_pr_07_d02_single_version_domain_resolves(self):
        # Direct test of the PR-07-D02 fix: non-exact lookup at the ONLY registered version.
        facade = _make_facade()
        meta = AnyMeta(0x07, "smoke.dom", "1.0.0", "smoke.dom/:#Stub")
        # decode_any uses exact=False; without the PR-07-D02 fix this would raise CodecNotFound.
        # opaque.json must be JSON-text (pydantic model_validate_json contract).
        opaque = AnyOpaqueJson(meta, json.dumps({"x": "ok"}))
        result = facade.decode_any(opaque)
        self.assertIsInstance(result, BaboonRight, msg=str(result))


if __name__ == "__main__":
    unittest.main()
