



import random
import re
import string
import struct
import warnings
from io import BytesIO
from abc import ABC, abstractmethod
from dataclasses import dataclass
from datetime import datetime, timedelta, timezone
from decimal import Decimal
from functools import wraps
from typing import TypeVar, Generic, Callable, Optional, Any, ClassVar
from uuid import UUID, uuid4

from pydantic import BaseModel, ConfigDict

from .baboon_exceptions import BaboonCodecException

T = TypeVar("T")


class BaboonGenerated(ABC):
    @property
    @abstractmethod
    def baboon_domain_identifier(self) -> str:
        raise NotImplementedError

    @property
    @abstractmethod
    def baboon_domain_version(self) -> str:
        raise NotImplementedError

    @property
    @abstractmethod
    def baboon_type_identifier(self) -> str:
        raise NotImplementedError

    @property
    @abstractmethod
    def baboon_same_in_versions(self) -> list[str]:
        raise NotImplementedError

    # Forward-readability: newer domain versions whose encoded data THIS version's
    # codec can decode, mapped to the guarantee tier
    # ("identical" | "prefix-any-mode" | "prefix-compact" | "json-additive").
    # The prefix-* tiers hold only for top-level framed UEBA reads where the caller
    # discards the cursor after decoding. Non-abstract default keeps hand-written
    # stubs working; generated classes override it with a ClassVar.
    @property
    def baboon_forward_readable(self) -> dict[str, str]:
        return {}

    # Writer-side inverse of baboon_forward_readable: guarantee tier -> oldest domain
    # version whose codec can decode THIS version's encoding of this type. The
    # "identical" bound equals baboon_same_in_versions[0]; the "json-additive" bound is
    # published as `$rv`, the prefix-* bounds feed the binary envelope. Abstract: the
    # envelope writer fails fast when a tier is missing, so every implementation must
    # provide all four (generated classes do so with a ClassVar).
    @property
    @abstractmethod
    def baboon_min_reader_versions(self) -> dict[str, str]:
        raise NotImplementedError

class BaboonAdtMemberMeta(ABC):
    @property
    @abstractmethod
    def baboon_adt_type_identifier(self) -> str:
        raise NotImplementedError

    @property
    @abstractmethod
    def baboon_adt_type(self) -> type:
        raise NotImplementedError


class BaboonMeta(ABC):
    @property
    @abstractmethod
    def same_in_versions(self) -> list[str]:
        raise NotImplementedError

    # Forward-readability per type: newer version -> guarantee tier (see
    # BaboonGenerated.baboon_forward_readable). Non-abstract default keeps
    # hand-written stubs working; generated BaboonMetadata overrides it.
    def forward_readable_versions(self, type_id_string: str) -> dict[str, str]:
        return {}


class BaboonGeneratedLatest(BaboonGenerated):
    pass


def deprecated(message):
    def decorator(cls):
        original_init = cls.__init__

        @wraps(original_init)
        def new_init(self, *args, **kwargs):
            warnings.warn(
                message,
                DeprecationWarning,
                stacklevel=2
            )
            original_init(self, *args, **kwargs)

        cls.__init__ = new_init
        return cls

    return decorator

class Fixture:
    @staticmethod
    def next_byte() -> int:
        return random.randint(-100, 100)

    @staticmethod
    def next_ubyte() -> int:
        return random.randint(0, 200)

    @staticmethod
    def next_i16() -> int:
        return random.randint(-500, 500)

    @staticmethod
    def next_u16() -> int:
        return random.randint(0, 1000)

    @staticmethod
    def next_i32() -> int:
        return random.randint(-16384, 16384)

    @staticmethod
    def next_u32(u=16384) -> int:
        return random.randint(0, u)

    @staticmethod
    def next_i64() -> int:
        return random.randint(-32768, 32768)

    @staticmethod
    def next_u64() -> int:
        return random.randint(0, 32768)

    @staticmethod
    def next_f32() -> float:
        val = random.uniform(-16384.0, 16384.0)
        return struct.unpack('<f', struct.pack('<f', val))[0]

    @staticmethod
    def next_f64() -> float:
        return random.uniform(-16384.0, 16384.0)

    @staticmethod
    def next_f128() -> Decimal:
        value = random.uniform(0, 1)
        return Decimal(str(round(value, 10)))

    @staticmethod
    def next_bool() -> bool:
        return random.choice([True, False])

    @staticmethod
    def next_string() -> str:
        return ''.join(random.choices(string.ascii_letters + string.digits, k=20))

    @staticmethod
    def next_datetime() -> datetime:
        start = datetime(1970, 1, 1, tzinfo=timezone.utc)
        end = datetime(2100, 1, 1, tzinfo=timezone.utc)

        delta = end - start
        random_seconds = random.randrange(int(delta.total_seconds()))
        return start + timedelta(seconds=random_seconds)

    @staticmethod
    def next_uuid() -> UUID:
        return uuid4()

    @staticmethod
    def next_bytes() -> bytes:
        return bytes([random.randint(0, 255) for _ in range(16)])

    @staticmethod
    def next_list(f):
        return [f() for _ in range(10)]

    @staticmethod
    def next_set(f):
        return {f() for _ in range(10)}

    @staticmethod
    def next_dict(kf, vf):
        return {kf(): vf() for _ in range(10)}

    @staticmethod
    def next_optional(vf):
        return vf()

    @staticmethod
    def next_random_enum(e):
        return random.choice(list(e))

    @staticmethod
    def oneof(l: list[T]) -> T:
        return random.choice(l)

CS_EPOCH_DIFF_MS = 62_135_596_800_000

class LEDataOutputStream:
    def __init__(self, stream: BytesIO):
        self.stream = stream

    def write(self, b: bytes):
        self.stream.write(b)

    def write_byte(self, b: int):
        self.stream.write(struct.pack("<b", b))

    def write_ubyte(self, b: int):
        self.stream.write(struct.pack("<B", b))

    def write_i16(self, i: int):
        self.stream.write(struct.pack("<h", i))

    def write_u16(self, i: int):
        self.stream.write(struct.pack("<H", i))

    def write_i32(self, i: int):
        self.stream.write(struct.pack("<i", i))

    def write_u32(self, i: int):
        self.stream.write(struct.pack("<I", i))

    def write_i64(self, i: int):
        self.stream.write(struct.pack("<q", i))

    def write_u64(self, i: int):
        self.stream.write(struct.pack("<Q", i))

    def write_f32(self, f: float):
        self.stream.write(struct.pack("<f", f))

    def write_f64(self, f: float):
        self.stream.write(struct.pack("<d", f))

    def write_f128(self, f: Decimal):
        if abs(f.as_tuple().exponent) > 28:
            f = f.quantize(Decimal("1.0000000000000000000000000000"))

        sign, digits_tuple, exponent = f.as_tuple()
        mantissa = 0
        for d in digits_tuple:
            mantissa = mantissa * 10 + d

        scale = 0
        if exponent < 0:
            scale = -exponent
        elif exponent > 0:
            mantissa *= (10 ** exponent)
            scale = 0

        if mantissa >= (1 << 96):
            raise ValueError(f"Decimal value {f} is too large for C# Decimal (96-bit limit).")

        flags = (scale << 16)
        if sign == 1:
            flags |= 0x80000000

        lo = mantissa & 0xFFFFFFFF
        mid = (mantissa >> 32) & 0xFFFFFFFF
        hi = (mantissa >> 64) & 0xFFFFFFFF

        self.write_u32(lo)
        self.write_u32(mid)
        self.write_u32(hi)
        self.write_u32(flags)

    def write_bool(self, b: bool):
        self.stream.write(struct.pack("?", b))

    def write_uuid(self, u: UUID):
        self.stream.write(u.bytes_le)

    def write_str(self, s: str):
        bytes_data = s.encode("utf-8")
        value = len(bytes_data)
        while True:
            current_byte = value & 0x7F
            value >>= 7
            if value != 0:
                current_byte |= 0x80
            self.write_byte(current_byte)
            if value == 0:
                break
        self.stream.write(bytes_data)

    def write_datetime(self, d: datetime):
        if d.tzinfo is None:
            d = d.replace(tzinfo=timezone.utc)

        off = d.utcoffset()
        offset_ms = 0
        if off:
            offset_ms = (off.days * 86400 + off.seconds) * 1000 + off.microseconds // 1000

        unix_epoch = datetime(1970, 1, 1, tzinfo=timezone.utc)
        delta = d - unix_epoch

        unix_utc_ms = (
                (delta.days * 86_400 * 1000) +
                (delta.seconds * 1000) +
                (delta.microseconds // 1000)
        )

        cs_utc_millis_0001 = unix_utc_ms + CS_EPOCH_DIFF_MS
        cs_local_millis_0001 = cs_utc_millis_0001 + offset_ms

        self.write_i64(cs_local_millis_0001)
        self.write_i64(offset_ms)

        # docs/ueba-format.md: kind is a pure function of the offset (1 = UTC, 0 = otherwise)
        kind = 1 if offset_ms == 0 else 0
        self.write_byte(kind)

    def write_bytes(self, b: bytes):
        self.write_u32(len(b))
        self.stream.write(b)

    def write_optional(self, o, f):
        if o is None:
            self.write_byte(0)
        else:
            self.write_byte(1)
            f(o)

    def write_seq(self, c, f):
        self.write_i32(len(c))
        for i in c:
            f(i)

    def write_dict(self, d, kf, vf):
        self.write_i32(len(d))
        for k,v in d.items():
            kf(k)
            vf(v)

class LEDataInputStream:
    def __init__(self, stream : BytesIO):
        self.stream = stream

    def read_byte(self) -> int:
        return struct.unpack("<b", self.stream.read(1))[0]

    def read_ubyte(self) -> int:
        return struct.unpack("<B", self.stream.read(1))[0]

    def read_i16(self) -> int:
        return struct.unpack("<h", self.stream.read(2))[0]

    def read_u16(self) -> int:
        return struct.unpack("<H", self.stream.read(2))[0]

    def read_i32(self) -> int:
        return struct.unpack("<i", self.stream.read(4))[0]

    def read_u32(self) -> int:
        return struct.unpack("<I", self.stream.read(4))[0]

    def read_i64(self) -> int:
        return struct.unpack("<q", self.stream.read(8))[0]

    def read_u64(self) -> int:
        return struct.unpack("<Q", self.stream.read(8))[0]

    def read_f32(self) -> float:
        return struct.unpack("<f", self.stream.read(4))[0]

    def read_f64(self) -> float:
        return struct.unpack("<d", self.stream.read(8))[0]

    def read_f128(self) -> Decimal:
        lo = self.read_u32()
        mid = self.read_u32()
        hi = self.read_u32()
        flags = self.read_u32()

        # combine into 96-bit integer
        mantissa = lo | (mid << 32) | (hi << 64)

        scale = (flags >> 16) & 0xFF
        sign = (flags >> 31) & 1

        value = Decimal(mantissa) / (Decimal(10) ** scale)
        return -value if sign else value

    def read_bool(self) -> bool:
        return struct.unpack("?", self.stream.read(1))[0]

    def read_uuid(self) -> UUID:
        return UUID(bytes_le = self.stream.read(16))

    def read_string(self) -> str:
        length = 0
        shift = 0
        while True:
            byte_read = self.read_byte() & 0xFF
            length |= (byte_read & 0x7F) << shift
            if (byte_read & 0x80) == 0:
                break
            shift += 7
        buffer = self.stream.read(length)
        return buffer.decode("utf-8")

    def read_datetime(self) -> datetime:
        cs_local_millis_0001 = self.read_i64()
        offset_millis = self.read_i64()
        kind = self.read_byte()

        cs_utc_millis_0001 = cs_local_millis_0001 - offset_millis
        unix_utc_millis = cs_utc_millis_0001 - CS_EPOCH_DIFF_MS
        tz = timezone(timedelta(milliseconds=offset_millis))
        unix_epoch = datetime(1970, 1, 1, tzinfo=timezone.utc)
        dt_utc = unix_epoch + timedelta(milliseconds=unix_utc_millis)

        return dt_utc.astimezone(tz)

    def read_bytes(self) -> bytes:
        length = self.read_u32()
        return self.stream.read(length)

class Lazy(Generic[T]):
    def __init__(self, factory: Callable[[], T]):
        self._factory = factory
        self._value: T | None = None

    @property
    def value(self) -> T:
        if self._value is None:
            self._value = self._factory()
        return self._value

    @property
    def is_value_created(self) -> bool:
        return self._value_ref is not None

class BaboonSingleton(ABC, Generic[T]):
    _lazy_instance: Lazy[T]

    @classmethod
    def instance(cls) -> T:
        return cls._lazy_instance.value


@dataclass(frozen=True)
class Version:
    major: int
    minor: int
    patch: int

    @staticmethod
    def from_str(version: str) -> 'Version':
        chunks = version.split('.')

        if len(chunks) == 0:
            raise Exception(f"Expected to have version in format x.[y].[z], got {version}")

        try:
            major = int(chunks[0].strip())
        except (ValueError, IndexError):
            raise Exception(f"Expected to have version in format x.[y].[z], got {version}. Invalid major value.")

        try:
            minor = int(chunks[1].strip())
        except (ValueError, IndexError):
            raise Exception(f"Expected to have version in format x.[y].[z], got {version}. Invalid minor value.")

        try:
            patch = int(chunks[2].strip())
        except (ValueError, IndexError):
            raise Exception(f"Expected to have version in format x.[y].[z], got {version}. Invalid patch value.")

        return Version(major, minor, patch)

    def __lt__(self, other: 'Version') -> bool:
        return (self.major, self.minor, self.patch) < (other.major, other.minor, other.patch)

    def __le__(self, other: 'Version') -> bool:
        return (self.major, self.minor, self.patch) <= (other.major, other.minor, other.patch)

    def __gt__(self, other: 'Version') -> bool:
        return (self.major, self.minor, self.patch) > (other.major, other.minor, other.patch)

    def __ge__(self, other: 'Version') -> bool:
        return (self.major, self.minor, self.patch) >= (other.major, other.minor, other.patch)

    def __eq__(self, other: object) -> bool:
        if not isinstance(other, Version):
            return False
        return (self.major, self.minor, self.patch) == (other.major, other.minor, other.patch)

    def __hash__(self) -> int:
        return hash((self.major, self.minor, self.patch))


@dataclass(frozen=True)
class BaboonDomainVersion:
    domain_identifier: str
    domain_version: str

    def __post_init__(self):
        object.__setattr__(self, '_lazy_version', Lazy(lambda: Version.from_str(self.domain_version)))

    @property
    def version(self) -> Version:
        return self._lazy_version.value

    def __hash__(self):
        return hash((self.domain_identifier, self.domain_version))

    def __eq__(self, other):
        if not isinstance(other, BaboonDomainVersion):
            return False
        return (self.domain_identifier == other.domain_identifier and
                self.domain_version == other.domain_version)


class BaboonTypeMeta(BaseModel):
    meta_version: int
    domain_identifier: str
    domain_version: str
    domain_version_min_compat: str
    type_identifier: str
    # Oldest domain version whose JSON codec can decode the payload under the json-additive
    # contract (tolerant key lookup; fields unknown to that version are dropped). Always
    # <= domain_version_min_compat. Published as `$rv` when it differs from the (effective)
    # minCompat; the binary v1 envelope does not carry it. Empty means "= min_compat".
    domain_version_readable_min: str = ""

    JSON_READABLE_TIER: ClassVar[str] = "json-additive"
    # Tier keys of the UEBA prefix bounds in `baboon_min_reader_versions`, per index mode.
    UEBA_PREFIX_COMPACT_TIER: ClassVar[str] = "prefix-compact"
    UEBA_PREFIX_ANY_MODE_TIER: ClassVar[str] = "prefix-any-mode"

    model_config = ConfigDict(
        frozen=True,
        ser_json_bytes='hex',
        val_json_bytes='hex'
    )

    @property
    def version(self) -> BaboonDomainVersion:
        return BaboonDomainVersion(self.domain_identifier, self.domain_version)

    @property
    def version_min_compat(self) -> Optional[BaboonDomainVersion]:
        if not self.domain_version_min_compat or self.domain_version_min_compat == self.domain_version:
            return None
        return BaboonDomainVersion(self.domain_identifier, self.domain_version_min_compat)

    @property
    def effective_readable_min(self) -> str:
        return self.domain_version_readable_min or self.domain_version_min_compat

    @property
    def version_readable_min(self) -> Optional[BaboonDomainVersion]:
        if not self.domain_version_readable_min:
            return self.version_min_compat
        if self.domain_version_readable_min == self.domain_version:
            return None
        return BaboonDomainVersion(self.domain_identifier, self.domain_version_readable_min)

    @staticmethod
    def from_instance(value: BaboonGenerated) -> 'BaboonTypeMeta':
        """Codecs discovery with ADTs check to ensure that ADTs is encoded with a codec type desired by the user.

        - If user is trying to encode ADT branch with the base type we should encode it with ADT meta header:
            `Encode<ADT>(new ADT.A())` -> {"A": {<encoded A>}}
        - If user is trying to encode ADT branch with its own type we should encode only branch, without any meta:
            `Encode<ADT.A>(new ADT.A())` -> {<encoded A>}
        """
        if isinstance(value, BaboonAdtMemberMeta):
            type_identifier = value.baboon_adt_type_identifier
        else:
            type_identifier = value.baboon_type_identifier

        min_compat = value.baboon_same_in_versions[0]
        readable_min = value.baboon_min_reader_versions.get(BaboonTypeMeta.JSON_READABLE_TIER)
        if readable_min is None:
            raise BaboonCodecException.EncoderFailure(
                f"baboon_min_reader_versions lacks '{BaboonTypeMeta.JSON_READABLE_TIER}' for type {value.baboon_type_identifier}")
        return BaboonTypeMeta(
            meta_version=BaboonTypeMetaCodec.META_VERSION,
            domain_identifier=value.baboon_domain_identifier,
            domain_version=value.baboon_domain_version,
            domain_version_min_compat=min_compat,
            type_identifier=type_identifier,
            domain_version_readable_min=readable_min,
        )

    def write_bin(self, writer: LEDataOutputStream) -> None:
        BaboonTypeMetaCodec.write_bin(self, writer)

    def write_json(self) -> dict[str, Any]:
        return BaboonTypeMetaCodec.write_json(self)

    @staticmethod
    def read_meta(reader: LEDataInputStream) -> Optional['BaboonTypeMeta']:
        return BaboonTypeMetaCodec.read_meta_bin(reader)

    @staticmethod
    def read_meta_json(value: dict[str, Any]) -> Optional['BaboonTypeMeta']:
        return BaboonTypeMetaCodec.read_meta_json(value)


class BaboonTypeMetaCodec:
    META_VERSION_1: int = 1
    META_VERSION_2: int = 2
    # Layout written by default (binary) and always (JSON `$mv`).
    META_VERSION: int = META_VERSION_1

    # v2 flags byte (codec-envelope.md §2.1.3): bit 0 -- min_compat follows; bit 1 -- readable_min follows.
    _V2_FLAG_MIN_COMPAT: int = 0x01
    _V2_FLAG_READABLE_MIN: int = 0x02
    _V2_FLAGS_MASK: int = _V2_FLAG_MIN_COMPAT | _V2_FLAG_READABLE_MIN

    META_VERSION_KEY = "$mv"
    DOMAIN_IDENTIFIER_KEY = "$d"
    DOMAIN_VERSION_KEY = "$v"
    DOMAIN_VERSION_MIN_COMPAT_KEY = "$uv"
    DOMAIN_VERSION_READABLE_KEY = "$rv"
    TYPE_IDENTIFIER_KEY = "$t"

    @staticmethod
    def write_bin(meta: BaboonTypeMeta, writer: LEDataOutputStream) -> None:
        if meta.meta_version == BaboonTypeMetaCodec.META_VERSION_1:
            BaboonTypeMetaCodec._write_bin_v1(meta, writer)
        elif meta.meta_version == BaboonTypeMetaCodec.META_VERSION_2:
            BaboonTypeMetaCodec._write_bin_v2(meta, writer)
        else:
            raise BaboonCodecException.EncoderFailure(f"Unsupported binary envelope meta_version {meta.meta_version}")

    @staticmethod
    def _write_bin_v2(meta: BaboonTypeMeta, writer: LEDataOutputStream) -> None:
        # v2: `02 | domain_id | domain_version | flags | [min_compat] | [readable_min] | type_id`; each bound
        # is elided exactly as in JSON (min_compat when == domain_version, readable_min when == effective min_compat)
        min_compat = meta.domain_version_min_compat or meta.domain_version
        readable_min = meta.domain_version_readable_min or min_compat
        has_min_compat = min_compat != meta.domain_version
        has_readable_min = readable_min != min_compat
        writer.write_byte(BaboonTypeMetaCodec.META_VERSION_2)
        writer.write_str(meta.domain_identifier)
        writer.write_str(meta.domain_version)
        writer.write_byte((BaboonTypeMetaCodec._V2_FLAG_MIN_COMPAT if has_min_compat else 0)
                          | (BaboonTypeMetaCodec._V2_FLAG_READABLE_MIN if has_readable_min else 0))
        if has_min_compat:
            writer.write_str(min_compat)
        if has_readable_min:
            writer.write_str(readable_min)
        writer.write_str(meta.type_identifier)

    @staticmethod
    def _write_bin_v1(meta: BaboonTypeMeta, writer: LEDataOutputStream) -> None:
        # PR-23-D03 fix (PR 10.4): pre-existing bugs.
        #   1) `writer.write_string(writer, ...)` — there is no `write_string` method on
        #      `LEDataOutputStream`; the correct API is `write_str(s)` (single arg). The
        #      double-`writer` argument also passed `writer` where a `str` was required.
        #   2) `writer.write_i32(...)` for the meta version — the symmetric reader uses
        #      `read_byte()` and compares with `1`, so the version prefix is a single byte,
        #      not a 4-byte i32. Mirrors `BaboonMetaCodec.writeBin` in Scala/C#/Java/etc.
        writer.write_byte(BaboonTypeMetaCodec.META_VERSION_1)
        writer.write_str(meta.domain_identifier)
        writer.write_str(meta.domain_version)

        if meta.domain_version == meta.domain_version_min_compat:
            writer.write_byte(0)
        else:
            writer.write_byte(1)
            writer.write_str(meta.domain_version_min_compat)

        writer.write_str(meta.type_identifier)

    @staticmethod
    def write_json(meta: BaboonTypeMeta) -> dict[str, Any]:
        # MFACADE-PR-3: always emit `$mv` as a JSON number so envelopes are
        # self-identifying without out-of-band knowledge (proposal §10.6 (a)).
        json_obj: dict[str, Any] = {
            BaboonTypeMetaCodec.META_VERSION_KEY: BaboonTypeMetaCodec.META_VERSION,
            BaboonTypeMetaCodec.DOMAIN_IDENTIFIER_KEY: meta.domain_identifier,
            BaboonTypeMetaCodec.DOMAIN_VERSION_KEY: meta.domain_version,
            BaboonTypeMetaCodec.TYPE_IDENTIFIER_KEY: meta.type_identifier,
        }

        if meta.domain_version != meta.domain_version_min_compat:
            json_obj[BaboonTypeMetaCodec.DOMAIN_VERSION_MIN_COMPAT_KEY] = meta.domain_version_min_compat
        # `$rv` is elided when it equals the effective `$uv`: unchanged types emit no new bytes
        if meta.domain_version_readable_min and meta.domain_version_readable_min != meta.domain_version_min_compat:
            json_obj[BaboonTypeMetaCodec.DOMAIN_VERSION_READABLE_KEY] = meta.domain_version_readable_min

        return json_obj

    @staticmethod
    def read_meta_bin(reader: LEDataInputStream) -> Optional[BaboonTypeMeta]:
        try:
            meta_version = reader.read_byte()
            if meta_version == BaboonTypeMetaCodec.META_VERSION_1:
                return BaboonTypeMetaCodec._read_meta_v1_bin(reader)
            if meta_version == BaboonTypeMetaCodec.META_VERSION_2:
                return BaboonTypeMetaCodec._read_meta_v2_bin(reader)
            return None
        except Exception:
            return None

    @staticmethod
    def _read_meta_v2_bin(reader: LEDataInputStream) -> Optional[BaboonTypeMeta]:
        try:
            domain_identifier = reader.read_string()
            domain_version = reader.read_string()
            flags = reader.read_byte()
            # unknown flag bits are illegal; a lenient reader would misparse the strings that follow
            if flags & ~BaboonTypeMetaCodec._V2_FLAGS_MASK:
                return None
            min_compat = reader.read_string() if flags & BaboonTypeMetaCodec._V2_FLAG_MIN_COMPAT else domain_version
            readable_min = reader.read_string() if flags & BaboonTypeMetaCodec._V2_FLAG_READABLE_MIN else min_compat
            type_identifier = reader.read_string()
            return BaboonTypeMeta(
                meta_version=BaboonTypeMetaCodec.META_VERSION_2,
                domain_identifier=domain_identifier,
                domain_version=domain_version,
                domain_version_min_compat=min_compat,
                type_identifier=type_identifier,
                domain_version_readable_min=readable_min,
            )
        except Exception:
            return None

    @staticmethod
    def read_meta_json(json_obj: dict[str, Any]) -> Optional[BaboonTypeMeta]:
        # MFACADE-PR-3: accept $mv as either a JSON number (int) or a string
        # (back-compat with M28-vintage fixtures); both must equal META_VERSION_1.
        # MFACADE-PR-7-D11: explicit `$mv: null` is rejected (distinct from absent —
        # `in` vs `.get()` returning None). Decided against per-backend asymmetry
        # in favour of the strict-everywhere interpretation.
        if BaboonTypeMetaCodec.META_VERSION_KEY in json_obj:
            mv_node = json_obj[BaboonTypeMetaCodec.META_VERSION_KEY]
            if mv_node is None:
                return None  # explicit null = malformed
            if isinstance(mv_node, bool):
                return None
            if isinstance(mv_node, int):
                mv = mv_node
            elif isinstance(mv_node, str):
                if not re.fullmatch(r'-?[0-9]+', mv_node):
                    return None
                try:
                    mv = int(mv_node)
                except (ValueError, TypeError):
                    return None
            else:
                return None
            if mv != BaboonTypeMetaCodec.META_VERSION_1:
                return None
        return BaboonTypeMetaCodec._read_meta_v1_json(json_obj)

    @staticmethod
    def _read_meta_v1_bin(reader: LEDataInputStream) -> Optional[BaboonTypeMeta]:
        try:
            domain_identifier = reader.read_string()
            domain_version = reader.read_string()

            has_min_compat = reader.read_byte()
            # codec-envelope.md §2.1: only 0x00 (elided) and 0x01 (present) are legal; anything else is rejected
            if has_min_compat not in (0, 1):
                return None
            if has_min_compat == 1:
                domain_version_min_compat = reader.read_string()
            else:
                domain_version_min_compat = domain_version

            type_identifier = reader.read_string()

            return BaboonTypeMeta(
                meta_version=BaboonTypeMetaCodec.META_VERSION_1,
                domain_identifier=domain_identifier,
                domain_version=domain_version,
                domain_version_min_compat=domain_version_min_compat,
                type_identifier=type_identifier,
            )
        except Exception:
            return None

    @staticmethod
    def _read_meta_v1_json(json_obj: dict[str, Any]) -> Optional[BaboonTypeMeta]:
        try:
            domain_identifier = json_obj.get(BaboonTypeMetaCodec.DOMAIN_IDENTIFIER_KEY)
            domain_version = json_obj.get(BaboonTypeMetaCodec.DOMAIN_VERSION_KEY)
            type_identifier = json_obj.get(BaboonTypeMetaCodec.TYPE_IDENTIFIER_KEY)

            if not all([domain_identifier, domain_version, type_identifier]):
                return None

            domain_version_min_compat = json_obj.get(
                BaboonTypeMetaCodec.DOMAIN_VERSION_MIN_COMPAT_KEY,
                domain_version
            )
            domain_version_readable_min = json_obj.get(
                BaboonTypeMetaCodec.DOMAIN_VERSION_READABLE_KEY,
                domain_version_min_compat
            )

            return BaboonTypeMeta(
                meta_version=BaboonTypeMetaCodec.META_VERSION_1,
                domain_identifier=domain_identifier,
                domain_version=domain_version,
                domain_version_min_compat=domain_version_min_compat,
                type_identifier=type_identifier,
                domain_version_readable_min=domain_version_readable_min,
            )
        except Exception:
            return None


def domain_version(g: BaboonGenerated) -> BaboonDomainVersion:
    return BaboonDomainVersion(g.baboon_domain_identifier, g.baboon_domain_version)


def baboon_unmodified_since_version(g: BaboonGenerated) -> str:
    return g.baboon_same_in_versions[0]


def unmodified_since_version(meta: BaboonMeta, type_id: str) -> str:
    # NOTE: The abstract BaboonMeta.same_in_versions property returns list[str] per
    # its declaration, but concrete implementations (e.g. generated BaboonMetadata)
    # return a callable(typeId) -> list[str] (matching C#/Scala sameInVersions(typeId)).
    # We call it as a callable per the C# reference signature.
    return meta.same_in_versions(type_id)[0]