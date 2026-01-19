



from abc import ABC, abstractmethod
from typing import TypeVar, Generic

from pydantic import BaseModel

from .baboon_runtime_shared import BaboonSingleton, BaboonGenerated, BaboonAdtMemberMeta

T = TypeVar("T")
TWire = TypeVar("TWire")
TIn = TypeVar("TIn")
TOut = TypeVar("TOut")
TCodec = TypeVar("TCodec")

class BaboonCodecData(ABC):
    @property
    @abstractmethod
    def baboon_domain_version(self) -> str:
        raise NotImplementedError

    @property
    @abstractmethod
    def baboon_domain_identifier(self) -> str:
        raise NotImplementedError

    @property
    @abstractmethod
    def baboon_type_identifier(self) -> str:
        raise NotImplementedError

class BaboonCodec(BaboonCodecData, Generic[T]):
    @property
    @abstractmethod
    def target_type(self) -> type:
        raise NotImplementedError

class BaboonValueCodec(BaboonCodec[T], Generic[T, TWire]):
    def encode(self, context: 'BaboonCodecContext', value: T) -> TWire:
        raise NotImplementedError

    def decode(self, context: 'BaboonCodecContext', wire: TWire) -> T:
        raise NotImplementedError

class BaboonStreamCodec(BaboonCodec[T], Generic[T, TIn, TOut]):
    def encode(self, context: 'BaboonCodecContext', writer: TOut, instance: T):
        raise NotImplementedError

    def decode(self, context: 'BaboonCodecContext', wire: TIn) -> T:
        raise NotImplementedError

class BaboonJsonCodec(BaboonValueCodec[T, TCodec], BaboonSingleton[TCodec]):
    pass

class BaboonBinCodec(BaboonStreamCodec[T, 'LEDataInputStream', 'LEDataOutputStream'], BaboonSingleton[TCodec]):
    pass

class BaboonJsonCodecBase(BaboonJsonCodec[T, TCodec]):
    pass

class BaboonJsonCodecGenerated(BaboonJsonCodecBase[T, TCodec]):
    def encode_baboon(self, ctx: 'BaboonCodecContext', value: BaboonGenerated) -> str:
        if type(value) == self.target_type:
            return self.encode(ctx, value)
        else:
            raise ValueError(f"Expected to have {self.target_type} but got {type(value)}")

class BaboonJsonCodecGeneratedAdt(BaboonJsonCodecGenerated[T, TCodec], BaboonAdtMemberMeta):
    pass

class NoJsonEncoder(BaboonJsonCodecBase[T, TCodec]):
    def encode(self, ctx: 'BaboonCodecContext', instance: T) -> str:
        if self is not self.instance:
            return self.instance.encode(ctx, instance)
        raise RuntimeError(
            f"Type {self.baboon_type_identifier}@{self.baboon_domain_version} "
            f"is deprecated, encoder was not generated"
        )

class NoJsonEncoderGenerated(BaboonJsonCodecGenerated[T, TCodec]):
    def encode(self, ctx: 'BaboonCodecContext', instance: T) -> str:
        if self is not self.instance:
            return self.instance.encode(ctx, instance)
        raise RuntimeError(
            f"Type {self.baboon_type_identifier}@{self.baboon_domain_version} "
            f"is deprecated, encoder was not generated"
        )

class NoJsonEncoderGeneratedAdt(BaboonJsonCodecGeneratedAdt[T, TCodec]):
    def encode(self, ctx: 'BaboonCodecContext', instance: T) -> str:
        if self is not self.instance:
            return self.instance.encode(ctx, instance)
        raise RuntimeError(
            f"Type {self.baboon_type_identifier}@{self.baboon_domain_version} "
            f"is deprecated, encoder was not generated"
        )

class BaboonBinCodecBase(BaboonBinCodec[T, TCodec], BaboonSingleton[TCodec], Generic[T, TCodec]):
    pass

class BaboonBinCodecGenerated(BaboonBinCodecBase[T, TCodec]):
    def encode_baboon(self, ctx: 'BaboonCodecContext', writer: 'LEDataOutputStream', value: BaboonGenerated):
        if type(value) == self.target_type:
            return self.encode(ctx, writer, value)
        else:
            raise ValueError(f"Expected to have {self.target_type} but got {type(value)}")

class BaboonBinCodecGeneratedAdt(BaboonBinCodecGenerated[T, TCodec], BaboonAdtMemberMeta):
    pass

class NoBinEncoder(BaboonBinCodecBase[T, TCodec]):
    def encode(self, ctx: 'BaboonCodecContext', writer: 'LEDataOutputStream', instance: T):
        if self is not self.instance:
            return self.instance.encode(ctx, instance)
        raise RuntimeError(
            f"Type {self.baboon_type_identifier}@{self.baboon_domain_version} "
            f"is deprecated, encoder was not generated"
        )

class NoBinEncoderGenerated(BaboonBinCodecGenerated[T, TCodec]):
    def encode(self, ctx: 'BaboonCodecContext', writer: 'LEDataOutputStream', instance: T):
        if self is not self.instance:
            return self.instance.encode(ctx, instance)
        raise RuntimeError(
            f"Type {self.baboon_type_identifier}@{self.baboon_domain_version} "
            f"is deprecated, encoder was not generated"
        )

class NoBinEncoderGeneratedAdt(BaboonBinCodecGeneratedAdt[T, TCodec]):
    def encode(self, ctx: 'BaboonCodecContext', writer: 'LEDataOutputStream', instance: T):
        if self is not self.instance:
            return self.instance.encode(ctx, instance)
        raise RuntimeError(
            f"Type {self.baboon_type_identifier}@{self.baboon_domain_version} "
            f"is deprecated, encoder was not generated"
        )

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

class AbstractBaboonCodecs:
    def __init__(self):
        self._codecs = {}

    def register(self, codec_id: str, impl):
        self._codecs[codec_id] = impl

    def find(self, codec_id: str) -> BaboonCodecData:
        return self._codecs[codec_id]()

    def try_find(self, codec_id: str) -> tuple[bool, BaboonCodecData | None]:
        value = self._codecs.get(codec_id)
        if value is not None:
            return True, value()
        else:
            return False, None

class AbstractBaboonJsonCodecs(AbstractBaboonCodecs):
    pass

class AbstractBaboonUebaCodecs(AbstractBaboonCodecs):
    pass