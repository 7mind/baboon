import io
import json
import sys
from datetime import datetime, timezone, timedelta
from decimal import Decimal
from pathlib import Path
from uuid import UUID

from Generated.baboon_any_opaque import AnyMeta, AnyOpaque, AnyOpaqueJson, AnyOpaqueUeba
from Generated.baboon_codecs import AbstractBaboonJsonCodecs, AbstractBaboonUebaCodecs, BaboonCodecContext
from Generated.baboon_codecs_facade import BaboonCodecsFacade
from Generated.baboon_runtime_shared import BaboonDomainVersion, LEDataOutputStream, LEDataInputStream
from Generated.convtest.testpkg.AllBasicTypes import AllBasicTypes, AllBasicTypes_JsonCodec, AllBasicTypes_UEBACodec
from Generated.convtest.testpkg.AnyShowcase import AnyShowcase, AnyShowcase_JsonCodec, AnyShowcase_UEBACodec
from Generated.convtest.testpkg.InnerPayload import InnerPayload, InnerPayload_JsonCodec, InnerPayload_UEBACodec
from Generated.convtest.testpkg.WireEnum import WireEnum
from Generated.convtest.testpkg.PointId import PointId
from Generated.convtest.testpkg.ItemId import ItemId
from Generated.convtest.testpkg.CompositeId import CompositeId
# PR-I.2 (M24 Phase 3.2) — Custom-foreign KeyCodec hook fixture. Stringy
# FStr foreign + ItemKey wrapper + ForeignKeyHolder round-trip exercises the
# generated FStr_KeyCodecHost identity default impl.
from Generated.convtest.m24foreign.ForeignKeyHolder import ForeignKeyHolder, ForeignKeyHolder_JsonCodec
from Generated.convtest.m24foreign.ItemKey import ItemKey

DOMAIN_ID = "convtest.testpkg"
DOMAIN_VER = "2.0.0"
INNER_TYPE_ID = "convtest.testpkg/:#InnerPayload"


# PR 13.2 — define a local codec aggregator that imports only the AnyShowcase / InnerPayload /
# AllBasicTypes codecs. Python's generated `convtest.testpkg.baboon_runtime` module has a
# pre-existing import-time `AttributeError` from a malformed `v1_0_0.abs.core.OldAbsAdt` reference
# (logged as a separate Python defect; not introduced by PR 13.2). Avoiding that module lets us
# wire the facade with the codecs the AnyShowcase cross-format encoder needs.

class _LocalBaboonCodecsJson(AbstractBaboonJsonCodecs):
    def __init__(self):
        super().__init__()
        self.register("convtest.testpkg/:#AllBasicTypes", AllBasicTypes_JsonCodec.instance)
        self.register("convtest.testpkg/:#InnerPayload", InnerPayload_JsonCodec.instance)
        self.register("convtest.testpkg/:#AnyShowcase", AnyShowcase_JsonCodec.instance)


class _LocalBaboonCodecsUeba(AbstractBaboonUebaCodecs):
    def __init__(self):
        super().__init__()
        self.register("convtest.testpkg/:#AllBasicTypes", AllBasicTypes_UEBACodec.instance)
        self.register("convtest.testpkg/:#InnerPayload", InnerPayload_UEBACodec.instance)
        self.register("convtest.testpkg/:#AnyShowcase", AnyShowcase_UEBACodec.instance)


def fresh_facade() -> BaboonCodecsFacade:
    f = BaboonCodecsFacade()
    f.register(
        BaboonDomainVersion(DOMAIN_ID, DOMAIN_VER),
        codecs_json=lambda: _LocalBaboonCodecsJson(),
        codecs_bin=lambda: _LocalBaboonCodecsUeba(),
    )
    return f


def expected_inner_payloads():
    return [
        InnerPayload(label="variant-A", count=1),
        InnerPayload(label="variant-B", count=2),
        InnerPayload(label="variant-C", count=3),
        InnerPayload(label="variant-D1", count=4),
        InnerPayload(label="variant-D2", count=5),
        InnerPayload(label="variant-D3", count=6),
        InnerPayload(label="opt-any", count=7),
        InnerPayload(label="lst-any-0", count=8),
    ]


def _ueba_bytes(p):
    ms = io.BytesIO()
    w = LEDataOutputStream(ms)
    InnerPayload_UEBACodec.instance().encode(BaboonCodecContext.compact(), w, p)
    return ms.getvalue()


def _as_json(p):
    # InnerPayload_JsonCodec.encode returns a JSON string; we need the inner JSON object so it
    # can be embedded as `$c` payload. Decode the string back via json.loads.
    s = InnerPayload_JsonCodec.instance().encode(BaboonCodecContext.compact(), p)
    return json.loads(s)


def _meta_a(): return AnyMeta(0x07, DOMAIN_ID, DOMAIN_VER, INNER_TYPE_ID)
def _meta_b(): return AnyMeta(0x03, None, DOMAIN_VER, INNER_TYPE_ID)
def _meta_c(): return AnyMeta(0x01, None, None, INNER_TYPE_ID)
def _meta_d1(): return AnyMeta(0x06, DOMAIN_ID, DOMAIN_VER, None)
def _meta_d2(): return AnyMeta(0x02, None, DOMAIN_VER, None)
def _meta_d3(): return AnyMeta(0x00, None, None, None)
def _meta_opt(): return AnyMeta(0x07, DOMAIN_ID, DOMAIN_VER, INNER_TYPE_ID)
def _meta_lst(): return AnyMeta(0x06, DOMAIN_ID, DOMAIN_VER, None)


# PR 13.2 / PR-26-D03: Python's facade `json_to_ueba_bytes` calls `json_codec.decode(ctx, dict)`
# but the generated `InnerPayload_JsonCodec.decode` expects a JSON string (it goes through
# `model_validate_json`). The facade's cross-format helper is pre-existing-broken in Python.
# To unblock cross-language interop testing we build same-branch fixtures (all-Json for the
# JSON wire, all-Ueba for the UEBA wire) so the codec never traverses the facade.

def create_sample_any_showcase_json() -> AnyShowcase:
    p = expected_inner_payloads()
    return AnyShowcase(
        vAnyA=AnyOpaqueJson(meta=_meta_a(), json=_as_json(p[0])),
        vAnyB=AnyOpaqueJson(meta=_meta_b(), json=_as_json(p[1])),
        vAnyC=AnyOpaqueJson(meta=_meta_c(), json=_as_json(p[2])),
        vAnyD1=AnyOpaqueJson(meta=_meta_d1(), json=_as_json(p[3])),
        vAnyD2=AnyOpaqueJson(meta=_meta_d2(), json=_as_json(p[4])),
        vAnyD3=AnyOpaqueJson(meta=_meta_d3(), json=_as_json(p[5])),
        optAny=AnyOpaqueJson(meta=_meta_opt(), json=_as_json(p[6])),
        lstAny=[AnyOpaqueJson(meta=_meta_lst(), json=_as_json(p[7]))],
    )


def create_sample_any_showcase_ueba() -> AnyShowcase:
    p = expected_inner_payloads()
    return AnyShowcase(
        vAnyA=AnyOpaqueUeba(meta=_meta_a(), bytes=_ueba_bytes(p[0])),
        vAnyB=AnyOpaqueUeba(meta=_meta_b(), bytes=_ueba_bytes(p[1])),
        vAnyC=AnyOpaqueUeba(meta=_meta_c(), bytes=_ueba_bytes(p[2])),
        vAnyD1=AnyOpaqueUeba(meta=_meta_d1(), bytes=_ueba_bytes(p[3])),
        vAnyD2=AnyOpaqueUeba(meta=_meta_d2(), bytes=_ueba_bytes(p[4])),
        vAnyD3=AnyOpaqueUeba(meta=_meta_d3(), bytes=_ueba_bytes(p[5])),
        optAny=AnyOpaqueUeba(meta=_meta_opt(), bytes=_ueba_bytes(p[6])),
        lstAny=[AnyOpaqueUeba(meta=_meta_lst(), bytes=_ueba_bytes(p[7]))],
    )


def write_json_any(ctx, data, output_dir):
    json_str = AnyShowcase_JsonCodec.instance().encode(ctx, data)
    p = Path(output_dir) / "any-showcase.json"
    with open(p, "w", encoding="utf-8") as f:
        f.write(json_str)
    print(f"Written JSON to {p}")


def write_ueba_any(ctx, data, output_dir):
    ms = io.BytesIO()
    w = LEDataOutputStream(ms)
    AnyShowcase_UEBACodec.instance().encode(ctx, w, data)
    p = Path(output_dir) / "any-showcase.ueba"
    with open(p, "wb") as f:
        f.write(ms.getvalue())
    print(f"Written UEBA to {p}")


def decode_inner(o):
    if isinstance(o, AnyOpaqueUeba):
        ms = io.BytesIO(o.bytes)
        r = LEDataInputStream(ms)
        return InnerPayload_UEBACodec.instance().decode(BaboonCodecContext.compact(), r)
    if isinstance(o, AnyOpaqueJson):
        return InnerPayload_JsonCodec.instance().decode(BaboonCodecContext.compact(), json.dumps(o.json))
    raise RuntimeError(f"unexpected AnyOpaque subclass: {type(o)}")


def decode_all_payloads(v):
    opt = v.optAny
    if opt is None:
        raise RuntimeError("optAny was None; expected non-None")
    if not v.lstAny:
        raise RuntimeError("lstAny was empty; expected one element")
    return [
        decode_inner(v.vAnyA),
        decode_inner(v.vAnyB),
        decode_inner(v.vAnyC),
        decode_inner(v.vAnyD1),
        decode_inner(v.vAnyD2),
        decode_inner(v.vAnyD3),
        decode_inner(opt),
        decode_inner(v.lstAny[0]),
    ]


def read_and_verify_any_showcase(file_path):
    ctx = BaboonCodecContext.default()
    fp = Path(file_path)
    try:
        if fp.suffix == ".json":
            with open(fp, "r", encoding="utf-8") as f:
                json_str = f.read()
            data = AnyShowcase_JsonCodec.instance().decode(ctx, json_str)
        else:
            with open(fp, "rb") as f:
                ueba_bytes = f.read()
            r = LEDataInputStream(io.BytesIO(ueba_bytes))
            data = AnyShowcase_UEBACodec.instance().decode(ctx, r)
    except Exception as e:
        print(f"AnyShowcase deserialization failed: {e}", file=sys.stderr)
        sys.exit(1)
    try:
        expected = expected_inner_payloads()
        decoded = decode_all_payloads(data)
        for i, (exp, got) in enumerate(zip(expected, decoded)):
            if exp != got:
                print(f"AnyShowcase payload {i} mismatch: expected {exp}, got {got}", file=sys.stderr)
                sys.exit(1)
    except Exception as e:
        print(f"AnyShowcase decode failed: {e}", file=sys.stderr)
        sys.exit(1)
    print("OK")


def create_sample_data():
    return AllBasicTypes(
        vi8=42,
        vi16=1234,
        vi32=123456,
        vi64=123456789,
        vu8=200,
        vu16=50000,
        vu32=3000000000,
        vu64=10000000000,
        vf32=3.14159,
        vf64=2.718281828,
        vf128=Decimal("123456789.987654321"),
        vstr="Hello, Baboon!",
        vbstr=b'Hello Bytes',
        vuid=UUID("12345678-1234-5678-1234-567812345678"),
        vbit=True,
        vtsu=datetime(2024, 6, 15, 12, 30, 45, 123000, tzinfo=timezone.utc),
        vtso=datetime(2024, 6, 15, 14, 30, 45, 987000, tzinfo=timezone(timedelta(hours=2))),
        voptStr="optional value",
        vlstI32=[1, 2, 3, 4, 5],
        vsetStr={"apple", "banana", "cherry"},
        vmapStrI32={"one": 1, "two": 2, "three": 3},
        voptLst=["nested", "list", "values"],
        vlstOpt=[10, None, 20, 30],
        vmapLst={"numbers": [1, 2, 3], "more": [4, 5, 6]},
        # Non-Pascal-case enum member; canonical JSON wire form is "Cafe" (PR-35-D06 regression guard).
        vWireEnum=WireEnum.Cafe,
        # Identifier (PR-57e). Wire form is `{"x": 42, "y": -7}` on JSON and two
        # i32 LE values on UEBA — byte-identical to a `data` of the same shape
        # per docs/spec/identifier-repr.md §1.3 / §7.
        vPointId=PointId(x=42, y=-7),
        # PR-61 (M19.3) — id types as JSON map keys. Per PR-60 (M19.2) all id
        # types — single- or multi-field — use canonical repr __str__ as the
        # key form: e.g. `ItemId:2.0.0#v:00000000-0000-0000-0000-000000000001`.
        # Canonical deterministic uuids ensure cross-language byte-identity.
        vmapItemIdU32={
            ItemId(v=UUID("00000000-0000-0000-0000-000000000001")): 1,
            ItemId(v=UUID("00000000-0000-0000-0000-000000000002")): 2,
        },
        vmapCompositeIdU32={
            CompositeId(
                tenant=UUID("00000000-0000-0000-0000-0000000000aa"),
                user=UUID("00000000-0000-0000-0000-0000000000bb"),
            ): 100,
            CompositeId(
                tenant=UUID("00000000-0000-0000-0000-0000000000cc"),
                user=UUID("00000000-0000-0000-0000-0000000000dd"),
            ): 200,
        },
    )


def write_json(ctx, data, output_dir):
    json_str = AllBasicTypes_JsonCodec.instance().encode(ctx, data)
    json_file_path = Path(output_dir) / "all-basic-types.json"
    with open(json_file_path, "w", encoding="utf-8") as f:
        f.write(json_str)
    print(f"Written JSON to {json_file_path}")


def write_ueba(ctx, data, output_dir):
    memory_stream = io.BytesIO()
    ueba_writer = LEDataOutputStream(memory_stream)
    AllBasicTypes_UEBACodec.instance().encode(ctx, ueba_writer, data)
    ueba_bytes = memory_stream.getvalue()
    ueba_file_path = Path(output_dir) / "all-basic-types.ueba"
    with open(ueba_file_path, "wb") as f:
        f.write(ueba_bytes)
    print(f"Written UEBA to {ueba_file_path}")


def read_and_verify(file_path):
    if file_path.endswith("any-showcase.json") or file_path.endswith("any-showcase.ueba"):
        read_and_verify_any_showcase(file_path)
        return
    ctx = BaboonCodecContext.default()
    fp = Path(file_path)

    try:
        if fp.suffix == ".json":
            with open(fp, "r", encoding="utf-8") as f:
                json_str = f.read()
            data = AllBasicTypes_JsonCodec.instance().decode(ctx, json_str)
        elif fp.suffix == ".ueba":
            with open(fp, "rb") as f:
                ueba_bytes = f.read()
            memory_stream = io.BytesIO(ueba_bytes)
            ueba_reader = LEDataInputStream(memory_stream)
            data = AllBasicTypes_UEBACodec.instance().decode(ctx, ueba_reader)
        else:
            print(f"Unknown file extension: {fp.suffix}", file=sys.stderr)
            sys.exit(1)
    except Exception as e:
        print(f"Deserialization failed: {e}", file=sys.stderr)
        sys.exit(1)

    if data.vstr != "Hello, Baboon!":
        print(f"vstr mismatch: expected 'Hello, Baboon!', got '{data.vstr}'", file=sys.stderr)
        sys.exit(1)
    if data.vi32 != 123456:
        print(f"vi32 mismatch: expected 123456, got {data.vi32}", file=sys.stderr)
        sys.exit(1)
    if not data.vbit:
        print(f"vbit mismatch: expected True, got {data.vbit}", file=sys.stderr)
        sys.exit(1)

    # Roundtrip
    try:
        if fp.suffix == ".json":
            re_encoded = AllBasicTypes_JsonCodec.instance().encode(ctx, data)
            re_decoded = AllBasicTypes_JsonCodec.instance().decode(ctx, re_encoded)
            if data != re_decoded:
                print("JSON roundtrip mismatch", file=sys.stderr)
                sys.exit(1)
        else:
            ms = io.BytesIO()
            w = LEDataOutputStream(ms)
            AllBasicTypes_UEBACodec.instance().encode(ctx, w, data)
            re_bytes = ms.getvalue()
            ms2 = io.BytesIO(re_bytes)
            r = LEDataInputStream(ms2)
            re_decoded = AllBasicTypes_UEBACodec.instance().decode(ctx, r)
            if data != re_decoded:
                print("UEBA roundtrip mismatch", file=sys.stderr)
                sys.exit(1)
    except Exception as e:
        print(f"Roundtrip failed: {e}", file=sys.stderr)
        sys.exit(1)

    print("OK")


# PR-57e (M18.4e) — cross-language identifier repr (__repr__) byte-identity.
# Per spec §7 the repr form is a separate invariant from the JSON/UEBA wire bytes;
# we write it as a per-language artifact so the Scala-side test can assert all 10 backends
# produce byte-identical output for the same canonical PointId value.
def write_point_id_repr(pid, output_dir):
    p = Path(output_dir) / "point-id.txt"
    # No trailing newline — exact byte match across all languages. Python's __repr__ is the
    # canonical parseable form for an identifier (matches spec convention).
    with open(p, "w", encoding="utf-8") as f:
        f.write(repr(pid))
    print(f"Written repr to {p}")


# PR-I.2 (M24 Phase 3.2) — Custom-foreign KeyCodec hook canonical fixture.
# Map keys go through FStr_KeyCodecHost (default identity impl for the stringy
# foreign), so the wire form is `{"m":{"alpha":"v1","beta":"v2"}}`.
def create_foreign_key_holder_sample() -> ForeignKeyHolder:
    return ForeignKeyHolder(m={
        ItemKey(v="alpha"): "v1",
        ItemKey(v="beta"): "v2",
    })


def write_foreign_key_holder_json(ctx, data, output_dir):
    json_str = ForeignKeyHolder_JsonCodec.instance().encode(ctx, data)
    p = Path(output_dir) / "m24-foreign-keycodec.json"
    with open(p, "w", encoding="utf-8") as f:
        f.write(json_str)
    print(f"Written JSON to {p}")


def run_legacy():
    sample_data = create_sample_data()
    ctx = BaboonCodecContext.default()

    base_dir = Path("../../target/compat-test").resolve()
    json_dir = base_dir / "python-json"
    ueba_dir = base_dir / "python-ueba"
    repr_dir = base_dir / "python-repr"

    json_dir.mkdir(parents=True, exist_ok=True)
    ueba_dir.mkdir(parents=True, exist_ok=True)
    repr_dir.mkdir(parents=True, exist_ok=True)

    write_json(ctx, sample_data, str(json_dir))
    write_ueba(ctx, sample_data, str(ueba_dir))
    write_json_any(ctx, create_sample_any_showcase_json(), str(json_dir))
    write_ueba_any(ctx, create_sample_any_showcase_ueba(), str(ueba_dir))
    write_point_id_repr(sample_data.vPointId, str(repr_dir))
    write_foreign_key_holder_json(ctx, create_foreign_key_holder_sample(), str(json_dir))

    print("Python serialization complete!")


if __name__ == "__main__":
    args = sys.argv[1:]
    if len(args) >= 1 and args[0] == "write":
        output_dir = args[1]
        fmt = args[2]
        Path(output_dir).mkdir(parents=True, exist_ok=True)
        sample_data = create_sample_data()
        ctx = BaboonCodecContext.default()
        if fmt == "json":
            write_json(ctx, sample_data, output_dir)
            write_json_any(ctx, create_sample_any_showcase_json(), output_dir)
            write_foreign_key_holder_json(ctx, create_foreign_key_holder_sample(), output_dir)
        elif fmt == "ueba":
            write_ueba(ctx, sample_data, output_dir)
            write_ueba_any(ctx, create_sample_any_showcase_ueba(), output_dir)
        else:
            print(f"Unknown format: {fmt}", file=sys.stderr)
            sys.exit(1)
    elif len(args) >= 1 and args[0] == "read":
        read_and_verify(args[1])
    else:
        run_legacy()
