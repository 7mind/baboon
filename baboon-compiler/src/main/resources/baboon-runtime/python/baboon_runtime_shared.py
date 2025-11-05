

from datetime import datetime, timedelta, timezone
from typing import TypeVar, Generic, ClassVar
from abc import ABC, abstractmethod
from pydantic import BaseModel, ConfigDict
from uuid import UUID, uuid4
from decimal import Decimal
from functools import wraps
import struct
import warnings
import random
import string

T = TypeVar("T")
To = TypeVar("To")
From = TypeVar("From")


class BaboonMeta:
    pass


class IBaboonGenerated:
    pass


class IBaboonGeneratedLatest:
    pass


class IBaboonAdtMemberMeta:
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


class ConversionKey(BaseModel):
    from_t: type
    to: type

    model_config = ConfigDict(
        frozen=True
    )


class GenericConversion(ABC):
    pass


class BaboonAbstractConversions(GenericConversion):
    def __init__(self):
        self.registry = {}

    def register(self, conversion, type_from: type, to_type: type):
        key = ConversionKey(from_t=type_from, to=to_type)
        self.registry[key] = conversion

    def convert_with_context(self, context, from_value, type_from, to_type):
        key = ConversionKey(from_t=type_from, to=to_type)
        return self.registry[key].do_convert(context, self, from_value)

    @abstractmethod
    def versions_from(self) -> list[str]:
        raise NotImplementedError

    @abstractmethod
    def version_to(self) -> str:
        raise NotImplementedError


class BaboonAbstractConversion(ABC, Generic[From, To]):
    @abstractmethod
    def do_convert(self, ctx, conversions: BaboonAbstractConversions, cfrom: From) -> To:
        raise NotImplementedError

    @staticmethod
    @abstractmethod
    def version_from() -> str:
        raise NotImplementedError

    @staticmethod
    @abstractmethod
    def version_to() -> str:
        raise NotImplementedError

    @staticmethod
    @abstractmethod
    def type_id() -> type:
        raise NotImplementedError