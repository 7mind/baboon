# PR 10.4 (issue #69 phase 10.4 / closes M10): hand-written round-trip and cross-format tests for
# `any` fields. Mirrors Scala's AnyRoundTripSpec (PR 2.4) / C# (PR 3.4) / Rust (PR 4.3) /
# Kotlin (PR 5.4) / Java (PR 6.4) / TypeScript (PR 7.4) / Dart (PR 8.4) / Swift (PR 9.4).
# Exercises the `any-ok` fixture's six DSL variants:
#   A=any                         -> kind 0x07
#   B=any[domain:this]            -> kind 0x03
#   C=any[domain:current]         -> kind 0x01
#   D1=any[Inner]                 -> kind 0x06
#   D2=any[domain:this, Inner]    -> kind 0x02
#   D3=any[domain:current, Inner] -> kind 0x00
# plus the three nested positions (opt/lst/map-value).
#
# NOTE: This test references generated MyOk symbols (Holder, Inner, Holder_JsonCodec,
# Holder_UEBACodec, ...) produced into this stub only by the py-stub codegen path
# (rsync + codegen into target/test-regular/py-stub/). Run from the codegen'd copy:
#   cd target/test-regular/py-stub && \
#     python3 -m unittest BaboonTests.RuntimeTests.test_any_round_trip
#
# Asymmetry note: Python's `Inner_JsonCodec.encode` returns `str` (Pydantic `model_dump_json`),
# unlike Scala/Java which return a JSON tree. The generated `Holder_JsonCodec._encode_any_field`
# stores `value.json` directly as the `$c` content (so a dict round-trips natively); cross-format
# `ueba_to_json` returns the `str` JSON tree of the inner codec which the helper then re-parses
# with `json.loads` to embed it as a structured value (see `Holder.py` line 80). The
# `decode_any` helper for the JSON branch passes `opaque.json` through to `Inner_JsonCodec.decode`
# which expects a `str` — so the JSON-branch decode_any test must supply a JSON string, not a dict.

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
from BaboonDefinitions.Generated.baboon_service_wiring import BaboonLeft, BaboonRight

from BaboonDefinitions.Generated.my.ok.Holder import (
    Holder,
    Holder_JsonCodec,
    Holder_UEBACodec,
)
from BaboonDefinitions.Generated.my.ok.Inner import (
    Inner,
    Inner_JsonCodec,
    Inner_UEBACodec,
)


# ---- Constants ----------------------------------------------------------------------------

DOMAIN_ID = "my.ok"
VERSION_STR = "1.0.0"
INNER_TYPE = "my.ok/:#Inner"
SAMPLE_INNER = Inner(x=42)


# ---- Per-variant AnyMeta builders ---------------------------------------------------------


def metaA(typeid: str = "opaque.Type") -> AnyMeta:
    return AnyMeta(0x07, DOMAIN_ID, VERSION_STR, typeid)


def metaB(typeid: str = "opaque.Type") -> AnyMeta:
    return AnyMeta(0x03, None, VERSION_STR, typeid)


def metaC(typeid: str = "opaque.Type") -> AnyMeta:
    return AnyMeta(0x01, None, None, typeid)


def metaD1() -> AnyMeta:
    return AnyMeta(0x06, DOMAIN_ID, VERSION_STR, None)


def metaD2() -> AnyMeta:
    return AnyMeta(0x02, None, VERSION_STR, None)


def metaD3() -> AnyMeta:
    return AnyMeta(0x00, None, None, None)


# ---- Inner serialisation helpers ----------------------------------------------------------


def inner_to_ueba_bytes(inner: Inner) -> bytes:
    buf = BytesIO()
    writer = LEDataOutputStream(buf)
    Inner_UEBACodec.instance().encode(BaboonCodecContext.compact(), writer, inner)
    return buf.getvalue()


def inner_to_json_string(inner: Inner) -> str:
    """Returns the JSON-text payload (Inner_JsonCodec.encode emits a `str` via Pydantic)."""
    return Inner_JsonCodec.instance().encode(BaboonCodecContext.default(), inner)


def inner_to_json_dict(inner: Inner) -> dict:
    """JSON-tree-shape dict; matches what `_encode_any_field` cross-convert produces under `$c`."""
    return json.loads(inner_to_json_string(inner))


# ---- Codec adapters wiring the abstract facade `register` triple --------------------------
#
# Mirror the adapter convention from PR 10.1's test_any_meta_codec.py: the abstract `register`
# expects a registry that registers a `(typeid -> factory)` map. We register the generated
# codecs (Inner_*, Holder_*) directly via their `instance` factories — the generated codec
# classes already declare the `baboon_domain_version`/`baboon_domain_identifier`
# /`baboon_type_identifier` triple needed for the facade's lookups.


class _MyOkUebaCodecs(AbstractBaboonUebaCodecs):
    def __init__(self):
        super().__init__()
        self.register(INNER_TYPE, Inner_UEBACodec.instance)
        self.register("my.ok/:#Holder", Holder_UEBACodec.instance)


class _MyOkJsonCodecs(AbstractBaboonJsonCodecs):
    def __init__(self):
        super().__init__()
        self.register(INNER_TYPE, Inner_JsonCodec.instance)
        self.register("my.ok/:#Holder", Holder_JsonCodec.instance)


class _MyOkMeta(BaboonMeta):
    """Compact local stub: facade's max-compat-version path needs a `same_in_versions(typeid)`
    method-style accessor (despite `BaboonMeta` declaring it as a `@property`). For our
    single-version registrations the path is never hit (PR-07-D02 handles equality), but we
    expose both shapes defensively.
    """

    @property
    def same_in_versions(self):
        return lambda _typeid: [VERSION_STR]


class _NoConversions:
    """Conversion registry stub — no inter-version conversions are required for single-version
    registrations. Required because `BaboonCodecsFacade.verify` checks `versions_conversions`.
    The any-feature tests do not call `verify`/`convert`, so the methods are unused here.
    """


def fresh_facade() -> BaboonCodecsFacade:
    facade = BaboonCodecsFacade()
    dv = BaboonDomainVersion(DOMAIN_ID, VERSION_STR)
    facade.register(
        dv,
        codecs_json=lambda: _MyOkJsonCodecs(),
        codecs_bin=lambda: _MyOkUebaCodecs(),
        conversions=lambda: _NoConversions(),
        meta=lambda: _MyOkMeta(),
    )
    return facade


# ---- Holder builders ----------------------------------------------------------------------


def build_ueba_holder() -> Holder:
    inner_bytes = inner_to_ueba_bytes(SAMPLE_INNER)
    return Holder(
        fAny=AnyOpaqueUeba(metaA(), b"\x01\x02\x03"),
        fDomainThis=AnyOpaqueUeba(metaB(), b"\x04\x05"),
        fDomainCurrent=AnyOpaqueUeba(metaC(), b"\x06"),
        fUnderlying=AnyOpaqueUeba(metaD1(), inner_bytes),
        fThisUnderlying=AnyOpaqueUeba(metaD2(), inner_bytes),
        fCurrentUnderlying=AnyOpaqueUeba(metaD3(), inner_bytes),
        fOpt=AnyOpaqueUeba(metaA(), b"\x07"),
        fLst=[AnyOpaqueUeba(metaD1(), inner_bytes)],
        fMapValue={"k1": AnyOpaqueUeba(metaA(), b"\x08")},
    )


def build_json_native_holder() -> Holder:
    """JSON-branch baseline — payloads are arbitrary dicts (round-trips natively without facade)."""
    arbitrary = {"payload": 42}
    inner_dict = inner_to_json_dict(SAMPLE_INNER)
    return Holder(
        fAny=AnyOpaqueJson(metaA(), arbitrary),
        fDomainThis=AnyOpaqueJson(metaB(), arbitrary),
        fDomainCurrent=AnyOpaqueJson(metaC(), arbitrary),
        fUnderlying=AnyOpaqueJson(metaD1(), inner_dict),
        fThisUnderlying=AnyOpaqueJson(metaD2(), inner_dict),
        fCurrentUnderlying=AnyOpaqueJson(metaD3(), inner_dict),
        fOpt=AnyOpaqueJson(metaA(), arbitrary),
        fLst=[AnyOpaqueJson(metaD1(), inner_dict)],
        fMapValue={"k1": AnyOpaqueJson(metaA(), arbitrary)},
    )


def build_json_holder_for_cross_convert() -> Holder:
    """JSON branches with REAL Inner JSON-tree dicts and typeid=INNER_TYPE for A/B/C variants
    so `json_to_ueba_bytes` cross-convert can resolve and decode the inner payload.
    The codec's `_encode_any_field` cross-path passes `value.json` to `json_to_ueba_bytes`,
    which calls `Inner_JsonCodec.decode(ctx, value.json)` — Inner_JsonCodec.decode expects a
    JSON-text `str`, not a dict (Pydantic `model_validate_json`). So we pass the JSON-text
    payload as the value.json content here.
    """
    inner_str = inner_to_json_string(SAMPLE_INNER)
    return Holder(
        fAny=AnyOpaqueJson(metaA(INNER_TYPE), inner_str),
        fDomainThis=AnyOpaqueJson(metaB(INNER_TYPE), inner_str),
        fDomainCurrent=AnyOpaqueJson(metaC(INNER_TYPE), inner_str),
        fUnderlying=AnyOpaqueJson(metaD1(), inner_str),
        fThisUnderlying=AnyOpaqueJson(metaD2(), inner_str),
        fCurrentUnderlying=AnyOpaqueJson(metaD3(), inner_str),
        fOpt=AnyOpaqueJson(metaA(INNER_TYPE), inner_str),
        fLst=[AnyOpaqueJson(metaD1(), inner_str)],
        fMapValue={"k1": AnyOpaqueJson(metaA(INNER_TYPE), inner_str)},
    )


# ---- Encode / decode helpers ---------------------------------------------------------------


def encode_ueba_bytes(holder: Holder, ctx: BaboonCodecContext) -> bytes:
    buf = BytesIO()
    writer = LEDataOutputStream(buf)
    Holder_UEBACodec.instance().encode(ctx, writer, holder)
    return buf.getvalue()


def decode_ueba_bytes(data: bytes, ctx: BaboonCodecContext = None) -> Holder:
    if ctx is None:
        ctx = BaboonCodecContext.compact()
    return Holder_UEBACodec.instance().decode(ctx, LEDataInputStream(BytesIO(data)))


def encode_json(holder: Holder, ctx: BaboonCodecContext = None) -> str:
    if ctx is None:
        ctx = BaboonCodecContext.default()
    return Holder_JsonCodec.instance().encode(ctx, holder)


def decode_json(text: str, ctx: BaboonCodecContext = None) -> Holder:
    if ctx is None:
        ctx = BaboonCodecContext.default()
    return Holder_JsonCodec.instance().decode(ctx, text)


# ===== Tests ===============================================================================


class AnyRoundTripTests(unittest.TestCase):

    # ----- 1. Per-variant UEBA round-trip ------------------------------------------------

    def test_ueba_compact_round_trip_preserves_all_six_variants_plus_nested_positions(self):
        original = build_ueba_holder()
        wire = encode_ueba_bytes(original, BaboonCodecContext.compact())
        decoded = decode_ueba_bytes(wire, BaboonCodecContext.compact())
        self.assertEqual(decoded, original)

    def test_ueba_indexed_round_trip_preserves_content(self):
        original = build_ueba_holder()
        wire = encode_ueba_bytes(original, BaboonCodecContext.indexed())
        decoded = decode_ueba_bytes(wire, BaboonCodecContext.indexed())
        self.assertEqual(decoded, original)

    def test_ueba_decode_yields_ueba_branch_with_matching_kind_bytes(self):
        original = build_ueba_holder()
        wire = encode_ueba_bytes(original, BaboonCodecContext.compact())
        decoded = decode_ueba_bytes(wire)
        self.assertIsInstance(decoded.fAny, AnyOpaqueUeba)
        self.assertEqual(decoded.fAny.meta.kind, 0x07)
        self.assertEqual(decoded.fDomainThis.meta.kind, 0x03)
        self.assertEqual(decoded.fDomainCurrent.meta.kind, 0x01)
        self.assertEqual(decoded.fUnderlying.meta.kind, 0x06)
        self.assertEqual(decoded.fThisUnderlying.meta.kind, 0x02)
        self.assertEqual(decoded.fCurrentUnderlying.meta.kind, 0x00)

    # ----- 2. Per-variant JSON round-trip -------------------------------------------------

    def test_json_round_trip_preserves_all_six_variants_plus_nested_positions(self):
        original = build_json_native_holder()
        text = encode_json(original)
        decoded = decode_json(text)
        self.assertEqual(decoded, original)

    def test_json_decode_yields_json_branch_with_matching_kind_bytes(self):
        original = build_json_native_holder()
        text = encode_json(original)
        decoded = decode_json(text)
        self.assertIsInstance(decoded.fAny, AnyOpaqueJson)
        self.assertEqual(decoded.fAny.meta.kind, 0x07)
        self.assertEqual(decoded.fDomainThis.meta.kind, 0x03)
        self.assertEqual(decoded.fDomainCurrent.meta.kind, 0x01)
        self.assertEqual(decoded.fUnderlying.meta.kind, 0x06)
        self.assertEqual(decoded.fThisUnderlying.meta.kind, 0x02)
        self.assertEqual(decoded.fCurrentUnderlying.meta.kind, 0x00)

    # ----- 3. Cross-format conversion via facade -----------------------------------------

    def test_cross_format_json_holder_encodes_to_ueba_bytewise_canonical(self):
        # `build_json_holder_for_cross_convert` uses AnyOpaqueJson everywhere with real Inner
        # JSON; encoding to UEBA forces `json_to_ueba_bytes` per field. After decode, branches
        # are AnyOpaqueUeba. Re-encoding the now-UEBA holder (no facade) must produce identical
        # bytes — proves cross-converted bytes match a native UEBA encode of the same value.
        facade = fresh_facade()
        ctx_with_facade = BaboonCodecContext.with_facade(False, facade)
        original = build_json_holder_for_cross_convert()
        wire = encode_ueba_bytes(original, ctx_with_facade)
        decoded = decode_ueba_bytes(wire)

        rewire = encode_ueba_bytes(decoded, BaboonCodecContext.compact())
        self.assertEqual(wire, rewire)

    def test_cross_format_ueba_holder_encodes_to_json_envelope_kinds_preserved(self):
        # `build_ueba_holder`-style with all UEBA branches; encoding to JSON triggers
        # `ueba_to_json` for each field. For untyped variants A/B/C the wire meta carries
        # typeid; we substitute typeid=INNER_TYPE so the registered Inner codec resolves and
        # the bytes deserialize as Inner. D variants resolve via static fallbacks emitted by
        # codec gen.
        facade = fresh_facade()
        ctx_with_facade = BaboonCodecContext.with_facade(False, facade)
        inner_bytes = inner_to_ueba_bytes(SAMPLE_INNER)
        crossable = Holder(
            fAny=AnyOpaqueUeba(metaA(INNER_TYPE), inner_bytes),
            fDomainThis=AnyOpaqueUeba(metaB(INNER_TYPE), inner_bytes),
            fDomainCurrent=AnyOpaqueUeba(metaC(INNER_TYPE), inner_bytes),
            fUnderlying=AnyOpaqueUeba(metaD1(), inner_bytes),
            fThisUnderlying=AnyOpaqueUeba(metaD2(), inner_bytes),
            fCurrentUnderlying=AnyOpaqueUeba(metaD3(), inner_bytes),
            fOpt=AnyOpaqueUeba(metaA(INNER_TYPE), inner_bytes),
            fLst=[AnyOpaqueUeba(metaD1(), inner_bytes)],
            fMapValue={"k1": AnyOpaqueUeba(metaA(INNER_TYPE), inner_bytes)},
        )
        text = Holder_JsonCodec.instance().encode(ctx_with_facade, crossable)
        decoded = decode_json(text)
        self.assertIsInstance(decoded.fAny, AnyOpaqueJson)
        self.assertEqual(decoded.fAny.meta.kind, 0x07)
        self.assertEqual(decoded.fUnderlying.meta.kind, 0x06)
        self.assertEqual(decoded.fCurrentUnderlying.meta.kind, 0x00)

    def test_cross_format_d3_isolated_field_resolves_via_static_fallbacks(self):
        # PR-06-D01 (Python analog): D3 has all-None meta on wire; the codec generator emits
        # (currentDomain, currentVersion, underlyingFqid) as static fallbacks. Without these
        # the facade cannot resolve and cross-convert fails. PR 10.2 + 10.3 emit the fallback
        # table; this test exercises the D3 path end-to-end through `json_to_ueba_bytes`.
        facade = fresh_facade()
        ctx_with_facade = BaboonCodecContext.with_facade(False, facade)
        inner_str = inner_to_json_string(SAMPLE_INNER)
        base = build_ueba_holder()
        mixed = Holder(
            fAny=base.fAny,
            fDomainThis=base.fDomainThis,
            fDomainCurrent=base.fDomainCurrent,
            fUnderlying=base.fUnderlying,
            fThisUnderlying=base.fThisUnderlying,
            fCurrentUnderlying=AnyOpaqueJson(metaD3(), inner_str),
            fOpt=base.fOpt,
            fLst=base.fLst,
            fMapValue=base.fMapValue,
        )
        # No raise on encode means `json_to_ueba_bytes` succeeded for the D3 field
        # (statics resolved).
        wire = encode_ueba_bytes(mixed, ctx_with_facade)
        decoded = decode_ueba_bytes(wire)
        self.assertEqual(decoded.fCurrentUnderlying.meta.kind, 0x00)
        self.assertIsInstance(decoded.fCurrentUnderlying, AnyOpaqueUeba)
        # Sanity-decode the cross-converted bytes via Inner_UEBACodec to prove fidelity.
        inner_reader = LEDataInputStream(BytesIO(decoded.fCurrentUnderlying.bytes))
        inner_decoded = Inner_UEBACodec.instance().decode(BaboonCodecContext.compact(), inner_reader)
        self.assertEqual(inner_decoded, SAMPLE_INNER)

    # ----- 4. facade.decode_any end-to-end ------------------------------------------------

    def test_facade_decode_any_resolves_ueba_inner_payload_to_typed_inner(self):
        facade = fresh_facade()
        meta = AnyMeta(0x07, DOMAIN_ID, VERSION_STR, INNER_TYPE)
        opaque = AnyOpaqueUeba(meta, inner_to_ueba_bytes(SAMPLE_INNER))
        result = facade.decode_any(opaque)
        self.assertIsInstance(result, BaboonRight, msg=str(result))
        self.assertEqual(result.value, SAMPLE_INNER)

    def test_facade_decode_any_resolves_json_inner_payload_to_typed_inner(self):
        # `Inner_JsonCodec.decode` expects a JSON-text `str` (Pydantic `model_validate_json`),
        # so the JSON branch payload here is the JSON-text serialisation of the Inner.
        facade = fresh_facade()
        meta = AnyMeta(0x07, DOMAIN_ID, VERSION_STR, INNER_TYPE)
        opaque = AnyOpaqueJson(meta, inner_to_json_string(SAMPLE_INNER))
        result = facade.decode_any(opaque)
        self.assertIsInstance(result, BaboonRight, msg=str(result))
        self.assertEqual(result.value, SAMPLE_INNER)

    # ----- 5. Forward-compat: trailing meta-extension bytes inside meta-length window ----

    def test_forward_compat_extra_meta_extension_bytes_skipped_on_ueba_decode(self):
        # Encode a Holder normally, then surgically patch the FIRST any-field's meta-length to
        # claim 5 extra bytes, and splice 5 bytes into the meta block. The decoder must consume
        # the meta, observe the gap (any_meta_length - bytes_read), skip them, and continue.
        # Layout of the first any-field on the wire (Compact, use_indices=False):
        #   [1 byte header][i32 anyLength][i32 anyMetaLen][... metaBytes ...][... blob ...]
        original = build_ueba_holder()
        wire = encode_ueba_bytes(original, BaboonCodecContext.compact())

        header_len = 1
        any_length_offset = header_len
        any_meta_len_offset = header_len + 4
        any_meta_start_offset = header_len + 4 + 4

        def read_i32_le(b: bytes, off: int) -> int:
            return int.from_bytes(b[off:off + 4], "little", signed=True)

        orig_any_length = read_i32_le(wire, any_length_offset)
        orig_any_meta_len = read_i32_le(wire, any_meta_len_offset)

        extension = bytes([0x11, 0x22, 0x33, 0x44, 0x55])
        new_any_meta_len = orig_any_meta_len + len(extension)
        new_any_length = orig_any_length + len(extension)

        orig_meta_slice = wire[any_meta_start_offset:any_meta_start_offset + orig_any_meta_len]
        orig_blob_and_rest = wire[any_meta_start_offset + orig_any_meta_len:]

        patched_buf = BytesIO()
        patched_writer = LEDataOutputStream(patched_buf)
        patched_writer.write(wire[:1])  # header byte
        patched_writer.write_i32(new_any_length)
        patched_writer.write_i32(new_any_meta_len)
        patched_writer.write(orig_meta_slice)
        patched_writer.write(extension)
        patched_writer.write(orig_blob_and_rest)

        decoded = decode_ueba_bytes(patched_buf.getvalue())
        self.assertEqual(decoded, original)

    # ----- 6. Fail-fast: missing facade on cross-format path ------------------------------

    def test_json_encode_with_ueba_payload_no_facade_raises(self):
        # Plain default ctx (no facade). JSON encoder hits an AnyOpaqueUeba branch and must
        # raise rather than silently dropping the cross-convert. `BaboonCodecException` is the
        # exception class — `EncoderFailure` is a static factory method returning an instance,
        # so we match the base class.
        holder = build_ueba_holder()
        with self.assertRaises(BaboonCodecException):
            Holder_JsonCodec.instance().encode(BaboonCodecContext.default(), holder)

    def test_ueba_encode_with_json_payload_no_facade_raises(self):
        holder = build_json_native_holder()
        buf = BytesIO()
        writer = LEDataOutputStream(buf)
        with self.assertRaises(BaboonCodecException):
            Holder_UEBACodec.instance().encode(BaboonCodecContext.default(), writer, holder)

    # ----- 7. JSON envelope shape lock-in -------------------------------------------------

    def test_json_envelope_shape_carries_locked_keys(self):
        # Sanity: the JSON envelope produced by the codec embeds the AnyMeta keys ($ak, $ad?,
        # $av?, $at?) alongside the $c content key. Any change to the envelope that drops one
        # of these would break cross-language interop.
        original = build_json_native_holder()
        text = encode_json(original)
        token = json.loads(text)

        # fAny variant A — all four meta keys + $c present.
        any_field = token["fAny"]
        self.assertEqual(any_field[ANY_KIND_KEY], 0x07)
        self.assertIn(ANY_DOMAIN_KEY, any_field)
        self.assertIn(ANY_VERSION_KEY, any_field)
        self.assertIn(ANY_TYPEID_KEY, any_field)
        self.assertIn(ANY_CONTENT_KEY, any_field)
        expected = {ANY_KIND_KEY, ANY_DOMAIN_KEY, ANY_VERSION_KEY, ANY_TYPEID_KEY, ANY_CONTENT_KEY}
        self.assertEqual(set(any_field.keys()), expected)

        # fCurrentUnderlying variant D3 — only $ak + $c (kind 0x00, no other meta on wire).
        d3 = token["fCurrentUnderlying"]
        self.assertEqual(d3[ANY_KIND_KEY], 0x00)
        self.assertIn(ANY_CONTENT_KEY, d3)
        self.assertNotIn(ANY_DOMAIN_KEY, d3)
        self.assertNotIn(ANY_VERSION_KEY, d3)
        self.assertNotIn(ANY_TYPEID_KEY, d3)


if __name__ == "__main__":
    unittest.main()
