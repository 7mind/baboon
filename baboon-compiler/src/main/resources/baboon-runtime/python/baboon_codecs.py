



from abc import ABC, abstractmethod
import json
from enum import Enum
from typing import Any, Optional, TypeVar, Generic

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
    def encode_value(self, context: 'BaboonCodecContext', value: T) -> Any:
        return json.loads(self.encode(context, value))

    def decode_value(self, context: 'BaboonCodecContext', value: Any) -> T:
        return self.decode(context, json.dumps(value))

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

class ForwardWritePolicy(Enum):
    """Which lower bound the WRITER publishes as the UEBA envelope's `domain_version_min_compat`
    (the v1 binary envelope has a single bound slot; see docs/forward-compat.md, "Envelope
    integration (UEBA)").

    - STRICT: the byte-identical bound (`baboon_same_in_versions[0]`) -- the default.
    - TOLERANT: the prefix-read bound for the chosen index mode (`prefix-compact` for compact
      payloads, `prefix-any-mode` for indexed ones). Readers older than the writer then decode the
      payload with their newest codec, dropping the appended fields they do not know. A reader
      cannot distinguish such an envelope from a byte-identical one, so re-encoding intermediaries
      must run at the writer's version or newer.
    """
    STRICT = "strict"
    TOLERANT = "tolerant"


class BaboonEnvelopeVersion(Enum):
    """Which top-level binary envelope layout the WRITER emits (docs/spec/codec-envelope.md §2.1).

    - V1 (default): single bound slot (`domain_version_min_compat`), value chosen by `ForwardWritePolicy`.
    - V2: JSON-equivalent layout carrying both the byte-identical bound and the prefix-read bound for
      the payload's index mode; the reader's `ForwardReadPolicy` then applies to binary exactly as it
      does to JSON. Only readers that know v2 can decode it.
    """
    V1 = "v1"
    V2 = "v2"


class BaboonCodecContext:
    # `Indexed`/`Compact`/`Default` are stable class-attribute singletons assigned after the
    # class body. Generator-emitted code may use `ctx is BaboonCodecContext.Indexed`-style
    # identity checks (mirrors the cross-language pattern used in TS/Dart/Swift/Kotlin/Java);
    # the existing `compact()`/`indexed()`/`default()` classmethods are retained for backwards
    # compatibility and now return the same singleton instances.
    Indexed: 'BaboonCodecContext'
    Compact: 'BaboonCodecContext'
    Default: 'BaboonCodecContext'

    def __init__(self, use_indices: bool, facade: Optional[Any] = None,
                 forward_write_policy: ForwardWritePolicy = ForwardWritePolicy.STRICT,
                 envelope_version: BaboonEnvelopeVersion = BaboonEnvelopeVersion.V1):
        self.use_indices = use_indices
        self.forward_write_policy = forward_write_policy
        self.envelope_version = envelope_version
        # `facade` is threaded through generated codec calls so the `any`-feature cross-format
        # conversion (UEBA <-> JSON) can resolve codecs by `(domain, version, typeid)` from an
        # `AnyMeta` envelope. `None` for the bare `Compact`/`Indexed` singletons; `with_facade`
        # is the single intended construction path for ctxes that thread a facade. See PR 10.1
        # plumbing (Q6 option (a) — same shape as Scala/C#/Rust/Kotlin/Java/TS/Dart/Swift).
        self.facade = facade

    @classmethod
    def indexed(cls) -> 'BaboonCodecContext':
        return cls.Indexed

    @classmethod
    def compact(cls) -> 'BaboonCodecContext':
        return cls.Compact

    @classmethod
    def default(cls) -> 'BaboonCodecContext':
        return cls.Default

    @classmethod
    def with_facade(cls, use_indices: bool, facade) -> 'BaboonCodecContext':
        return cls(use_indices, facade)

    @classmethod
    def custom(cls, use_indices: bool, forward_write_policy: ForwardWritePolicy,
               envelope_version: BaboonEnvelopeVersion, facade) -> 'BaboonCodecContext':
        """Fully specified context: index mode, writer-side forward policy, envelope layout and optional facade."""
        return cls(use_indices, facade, forward_write_policy, envelope_version)


# Stable singletons — `is`-equality preserved across all uses (PR 10.1).
BaboonCodecContext.Indexed = BaboonCodecContext(True)
BaboonCodecContext.Compact = BaboonCodecContext(False)
BaboonCodecContext.Default = BaboonCodecContext.Compact

class BaboonIndexEntry(BaseModel):
    offset: int
    length: int

class BaboonBinCodecIndexed(ABC):
    @abstractmethod
    def index_elements_count(self, ctx: BaboonCodecContext) -> int: ...

    def read_index(self, ctx: BaboonCodecContext, wire: 'LEDataInputStream') -> list[BaboonIndexEntry]:
        result: list[BaboonIndexEntry] = []
        self._read_index(ctx, wire, result)
        return result

    def consume_index(self, ctx: BaboonCodecContext, wire: 'LEDataInputStream') -> int:
        return self._read_index(ctx, wire, None)

    def _read_index(self, ctx: BaboonCodecContext, wire: 'LEDataInputStream', entries: Optional[list[BaboonIndexEntry]]) -> int:
        header = wire.read_byte()
        is_indexed = (header & 0b00000001) != 0
        count = 0

        prev_offset = 0
        prev_len = 0

        if is_indexed:
            left = self.index_elements_count(ctx)
            while left > 0:
                offset = wire.read_i32()
                length = wire.read_i32()

                if length <= 0:
                    raise ValueError(f"Invalid UEBA index length: {length}")
                if offset < prev_offset + prev_len:
                    raise ValueError(f"Invalid UEBA index offset: {offset}")

                if entries is not None:
                    entries.append(BaboonIndexEntry(offset=offset, length=length))
                count += 1
                left -= 1
                prev_offset = offset
                prev_len = length

        return count

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
