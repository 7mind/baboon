from abc import ABC
from dataclasses import dataclass
from typing import Any, Optional, Tuple

from .baboon_exceptions import BaboonCodecException
from .baboon_runtime_shared import LEDataInputStream, LEDataOutputStream
from .baboon_service_wiring import BaboonEither, BaboonLeft, BaboonRight


# Locked envelope keys for `any` JSON wire format.
ANY_KIND_KEY = "$ak"
ANY_DOMAIN_KEY = "$ad"
ANY_VERSION_KEY = "$av"
ANY_TYPEID_KEY = "$at"
ANY_CONTENT_KEY = "$c"

# Bitmask layout of the `meta-kind` byte: bit 2 = domain, bit 1 = version, bit 0 = typeid.
DOMAIN_BIT = 0x04
VERSION_BIT = 0x02
TYPEID_BIT = 0x01

# Six locked meta-kind variants. 0x04/0x05 reserved per PR-04-D01.
VALID_KINDS = frozenset({0x00, 0x01, 0x02, 0x03, 0x06, 0x07})


@dataclass(frozen=True)
class AnyMeta:
    """`AnyMeta` carries the locked four-byte/six-kind meta envelope for `any`-typed payloads.

    Invariants enforced at construction:
      - bit 2 (DOMAIN_BIT, 0x04) set <=> domain is not None
      - bit 1 (VERSION_BIT, 0x02) set <=> version is not None
      - bit 0 (TYPEID_BIT, 0x01) set <=> typeid is not None
      - kind must be one of {0x00, 0x01, 0x02, 0x03, 0x06, 0x07} — 0x04/0x05 reserved (PR-04-D01).

    Construction with a reserved or mismatched kind raises `ValueError`.
    """

    kind: int
    domain: Optional[str]
    version: Optional[str]
    typeid: Optional[str]

    def __post_init__(self) -> None:
        kind = self.kind & 0xFF
        if kind != self.kind:
            # Tolerate signed-byte sources by canonicalising to 0..255.
            object.__setattr__(self, "kind", kind)

        domain_bit_set = (kind & DOMAIN_BIT) != 0
        if domain_bit_set != (self.domain is not None):
            raise ValueError(
                f"AnyMeta: domain presence ({self.domain is not None}) does not match kind 0x{kind:02x} bit 2"
            )

        version_bit_set = (kind & VERSION_BIT) != 0
        if version_bit_set != (self.version is not None):
            raise ValueError(
                f"AnyMeta: version presence ({self.version is not None}) does not match kind 0x{kind:02x} bit 1"
            )

        typeid_bit_set = (kind & TYPEID_BIT) != 0
        if typeid_bit_set != (self.typeid is not None):
            raise ValueError(
                f"AnyMeta: typeid presence ({self.typeid is not None}) does not match kind 0x{kind:02x} bit 0"
            )

        if kind not in VALID_KINDS:
            raise ValueError(
                f"AnyMeta: reserved or invalid meta-kind byte: 0x{kind:02x}"
            )


class AnyOpaque(ABC):
    """`AnyOpaque` is the language-surface ADT for `any`-typed fields. Instances carry a meta
    envelope plus a payload in either binary (UEBA) or JSON form. See spec for the locked
    meta-kind table.

    Python's idiomatic sealed-base — `AnyOpaqueUeba` / `AnyOpaqueJson` are the only intended
    subclasses.
    """

    meta: AnyMeta


@dataclass(frozen=True)
class AnyOpaqueUeba(AnyOpaque):
    """UEBA-formed payload. `bytes` carries the inner blob (no length prefix); the framing
    `[length:i32][meta-length:i32][...]` is added by the codec emission helpers.
    """

    meta: AnyMeta
    bytes: bytes


@dataclass(frozen=True)
class AnyOpaqueJson(AnyOpaque):
    """JSON-formed payload. `json` is whatever `json.loads` would produce for the inner content
    — typically `dict[str, Any]` for an object payload, but can be any JSON-decodable type
    including `None` for a JSON-null content.
    """

    meta: AnyMeta
    json: Any


class AnyMetaCodec:
    """Static helper for the `AnyMeta` wire codec. Binary side trusts the wire and raises on
    bad input (PR-04-D02); JSON side is user-facing and threads errors through `BaboonEither`.
    """

    DOMAIN_BIT: int = DOMAIN_BIT
    VERSION_BIT: int = VERSION_BIT
    TYPEID_BIT: int = TYPEID_BIT

    ANY_KIND_KEY: str = ANY_KIND_KEY
    ANY_DOMAIN_KEY: str = ANY_DOMAIN_KEY
    ANY_VERSION_KEY: str = ANY_VERSION_KEY
    ANY_TYPEID_KEY: str = ANY_TYPEID_KEY
    ANY_CONTENT_KEY: str = ANY_CONTENT_KEY

    VALID_KINDS = VALID_KINDS

    @staticmethod
    def write_bin(writer: LEDataOutputStream, meta: AnyMeta) -> None:
        writer.write_ubyte(meta.kind & 0xFF)
        if meta.domain is not None:
            writer.write_str(meta.domain)
        if meta.version is not None:
            writer.write_str(meta.version)
        if meta.typeid is not None:
            writer.write_str(meta.typeid)

    @staticmethod
    def read_bin(reader: LEDataInputStream) -> AnyMeta:
        kind = reader.read_ubyte()
        domain = reader.read_string() if (kind & DOMAIN_BIT) != 0 else None
        version = reader.read_string() if (kind & VERSION_BIT) != 0 else None
        typeid = reader.read_string() if (kind & TYPEID_BIT) != 0 else None
        return AnyMeta(kind, domain, version, typeid)

    @staticmethod
    def read_bin_with_length(reader: LEDataInputStream) -> Tuple[AnyMeta, int]:
        """Reads meta and reports the number of bytes consumed. Callers that know the on-wire
        `meta-length` window can skip any trailing bytes left in the window — that's how the
        wire format keeps forward-compat with future meta extensions (PR-05-D01).
        """
        before_pos = reader.stream.tell()
        meta = AnyMetaCodec.read_bin(reader)
        after_pos = reader.stream.tell()
        return meta, after_pos - before_pos

    @staticmethod
    def write_json(meta: AnyMeta) -> dict:
        """Always returns a `dict`. The JSON encoder envelope build relies on this invariant
        (PR-06-D08) — adding `$c` content into a non-object would silently lose the key.
        """
        result: dict = {ANY_KIND_KEY: meta.kind & 0xFF}
        if meta.domain is not None:
            result[ANY_DOMAIN_KEY] = meta.domain
        if meta.version is not None:
            result[ANY_VERSION_KEY] = meta.version
        if meta.typeid is not None:
            result[ANY_TYPEID_KEY] = meta.typeid
        return result

    @staticmethod
    def read_json(value: Any) -> BaboonEither:
        """Returns `BaboonEither` to mirror PR-04-D02: binary decode trusts the wire and raises
        on bad input; JSON decode is user-facing and threads errors as `BaboonEither`.
        """
        if not isinstance(value, dict):
            return BaboonLeft(
                BaboonCodecException.DecoderFailure(
                    f"AnyMetaCodec.read_json: expected JSON object, got {type(value).__name__}"
                )
            )

        kind_token = value.get(ANY_KIND_KEY)
        # Reject booleans (Python `bool` is an `int` subclass) and non-int payloads.
        if isinstance(kind_token, bool) or not isinstance(kind_token, int):
            return BaboonLeft(
                BaboonCodecException.DecoderFailure(
                    f"AnyMetaCodec.read_json: missing or non-integer '{ANY_KIND_KEY}' field"
                )
            )
        kind = kind_token & 0xFF

        domain_result = AnyMetaCodec._read_opt_string(value, ANY_DOMAIN_KEY, kind, DOMAIN_BIT, "domain")
        if isinstance(domain_result, BaboonLeft):
            return domain_result
        domain = domain_result.value

        version_result = AnyMetaCodec._read_opt_string(value, ANY_VERSION_KEY, kind, VERSION_BIT, "version")
        if isinstance(version_result, BaboonLeft):
            return version_result
        version = version_result.value

        typeid_result = AnyMetaCodec._read_opt_string(value, ANY_TYPEID_KEY, kind, TYPEID_BIT, "typeid")
        if isinstance(typeid_result, BaboonLeft):
            return typeid_result
        typeid = typeid_result.value

        try:
            return BaboonRight(AnyMeta(kind, domain, version, typeid))
        except ValueError as e:
            return BaboonLeft(
                BaboonCodecException.DecoderFailure(
                    f"AnyMetaCodec.read_json: invalid meta: {e}"
                )
            )

    @staticmethod
    def _read_opt_string(
        json_obj: dict,
        key: str,
        kind: int,
        bit: int,
        name: str,
    ) -> BaboonEither:
        present = (kind & bit) != 0
        token = json_obj.get(key)
        value = token if isinstance(token, str) else None

        if present and value is not None:
            return BaboonRight(value)
        if not present and value is None:
            return BaboonRight(None)
        if present:
            return BaboonLeft(
                BaboonCodecException.DecoderFailure(
                    f"AnyMetaCodec.read_json: kind 0x{kind:02x} requires '{key}' ({name}) but it is missing"
                )
            )
        return BaboonLeft(
            BaboonCodecException.DecoderFailure(
                f"AnyMetaCodec.read_json: kind 0x{kind:02x} forbids '{key}' ({name}) but it is present"
            )
        )
