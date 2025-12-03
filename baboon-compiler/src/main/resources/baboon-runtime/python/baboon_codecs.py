from datetime import datetime, timedelta, timezone
from abc import ABC, abstractmethod
from typing import TypeVar, Generic
from decimal import Decimal
from io import BytesIO
from uuid import UUID
import struct

from pydantic import BaseModel

T = TypeVar("T")

class IBaboonCodecData(ABC):
    @abstractmethod
    def baboon_domain_version(self) -> str:
        raise NotImplementedError

    @abstractmethod
    def baboon_domain_identifier(self) -> str:
        raise NotImplementedError

    @abstractmethod
    def baboon_type_identifier(self) -> str:
        raise NotImplementedError

class BaboonJsonCodec(IBaboonCodecData, Generic[T]):
    @abstractmethod
    def encode(self, obj: T) -> str:
        raise NotImplementedError

    @abstractmethod
    def decode(self, json_str: str) -> T:
        raise NotImplementedError

class BaboonUEBACodec(IBaboonCodecData, Generic[T]):
    @abstractmethod
    def encode(self, ctx: 'BaboonCodecContext', wire: 'LEDataOutputStream', obj: T):
        raise NotImplementedError

    @abstractmethod
    def decode(self, ctx: 'BaboonCodecContext', wire: 'LEDataInputStream') -> T:
        raise NotImplementedError

class BaboonCodecContext:
    def __init__(self, use_indices: bool):
        self.use_indices = use_indices

    @classmethod
    def indexed(cls):
        return cls(True)

    @classmethod
    def compact(cls):
        return cls(False)

    @classmethod
    def default(cls):
        return cls(False)

class BaboonIndexEntry(BaseModel):
    offset: int
    length: int

class BaboonBinCodecIndexed(ABC):
    @abstractmethod
    def index_elements_count(self, ctx: BaboonCodecContext) -> int: ...

    def read_index(self, ctx: BaboonCodecContext, wire: 'LEDataInputStream') -> list[BaboonIndexEntry]:
        header = wire.read_byte()
        is_indexed = (header & 0b00000001) != 0
        result: list[BaboonIndexEntry] = []

        prev_offset = 0
        prev_len = 0

        if is_indexed:
            left = self.index_elements_count(ctx)
            while left > 0:
                offset = wire.read_u32()
                length = wire.read_u32()

                assert length > 0, "Length must be positive"
                assert offset >= prev_offset + prev_len, f"Offset violation: {offset} < {prev_offset + prev_len}"

                result.append(BaboonIndexEntry(offset=offset, length=length))
                left -= 1
                prev_offset = offset
                prev_len = length

        return result

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

        kind = 1 if offset_ms == 0 else 2
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

    def read_str(self) -> str:
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

class AbstractBaboonCodecs:
    def __init__(self):
        self._codecs = {}

    def register(self, codec_id: str, impl: IBaboonCodecData):
        self._codecs[codec_id] = impl

    def find(self, codec_id: str) -> IBaboonCodecData:
        return self._codecs[codec_id]

    def try_find(self, codec_id: str) -> tuple[bool, object | None]:
        value = self._codecs.get(codec_id)
        if value is not None:
            return True, value
        else:
            return False, None

class AbstractBaboonJsonCodecs(AbstractBaboonCodecs):
    pass

class AbstractBaboonUebaCodecs(AbstractBaboonCodecs):
    pass