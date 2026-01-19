

from abc import ABC, abstractmethod
from dataclasses import dataclass
from typing import Generic, TypeVar, Type

from BaboonDefinitions.Generated.baboon_runtime_shared import BaboonGenerated, BaboonAdtMemberMeta

To = TypeVar("To")
From = TypeVar("From")


class Conversion(ABC):
    @property
    @abstractmethod
    def type_from(self) -> type:
        raise NotImplementedError

    @property
    @abstractmethod
    def version_from(self) -> str:
        raise NotImplementedError

    @property
    @abstractmethod
    def type_to(self) -> type:
        raise NotImplementedError

    @property
    @abstractmethod
    def version_to(self) -> str:
        raise NotImplementedError

    @property
    @abstractmethod
    def type_id(self) -> str:
        raise NotImplementedError


class BaboonGeneratedConversion(Conversion):
    @abstractmethod
    def convert_baboon(self, context, conversions: 'AbstractBaboonConversions',
                       convert_from: BaboonGenerated) -> BaboonGenerated:
        raise NotImplementedError


class AbstractConversion(BaboonGeneratedConversion, Generic[From, To]):
    def __init__(self, from_type: type[From], to_type: type[To]):
        self._type_from = from_type
        self._type_to = to_type

    def validate_baboon_type(self, obj):
        if isinstance(obj, BaboonGenerated):
            tid = self.type_id
            conversion_type_is_exact = tid == obj.baboon_type_identifier
            if isinstance(obj, BaboonAdtMemberMeta):
                conversion_type_is_adt_type = tid == obj.baboon_adt_type_identifier
                if not conversion_type_is_exact and not conversion_type_is_adt_type:
                    raise ValueError(
                        f"Provided instance is adt={obj.baboon_adt_type_identifier} "
                        f"exact={obj.baboon_adt_type} one of which must be {tid}"
                    )

    def convert(self, context, conversions: 'AbstractBaboonConversions', convert_from: From) -> To:
        self.validate_baboon_type(convert_from)
        result = self.do_convert(context, conversions, convert_from)
        self.validate_baboon_type(result)
        return result

    def do_convert(self, context, conversions: 'AbstractBaboonConversions', convert_from: From) -> To:
        raise NotImplementedError

    def convert_baboon(self, context, conversions: 'AbstractBaboonConversions',
                       convert_from: BaboonGenerated) -> BaboonGenerated:
        if type(convert_from) is not self.type_from:
            raise ValueError(
                f"Can't use IBaboonGeneratedConversion interface when 'from' is not of type {self._type_from}"
            )

        result = self.do_convert(context, conversions, convert_from)

        if not isinstance(result, BaboonGenerated):
            raise ValueError(
                f"Can't use IBaboonGeneratedConversion interface for non IBaboonGenerated return type To = {self._type_to}"
            )

        return result

    @property
    def type_from(self) -> type:
        return self._type_from

    @property
    def type_to(self) -> type:
        return self._type_to


@dataclass(frozen=True)
class ConversionKey:
    type_from: type
    type_to: type

    def __str__(self) -> str:
        return f"{self.type_from.__name__}=>{self.type_to.__name__}"


class AbstractBaboonConversions:
    def __init__(self):
        self._conversions: dict[ConversionKey, Conversion] = {}
        self._type_conversions: dict[type, list[Conversion]] = {}

    def register(self, conversion: AbstractConversion[From, To]):
        key = ConversionKey(conversion.type_from, conversion.type_to)
        self._conversions[key] = conversion
        from_list = self._type_conversions.get(conversion.type_from, [])
        self._type_conversions[conversion.type_from] = from_list + [conversion]

    def convert_with_context(self, context, from_obj: BaboonGenerated, conversion: Conversion) -> BaboonGenerated:
        return conversion.convert(context, self, from_obj)

    def convert(self, from_obj: BaboonGenerated, conversion: Conversion) -> BaboonGenerated:
        return self.convert_with_context(None, from_obj, conversion)

    def convert_by_type(self, context, from_obj: From, from_type: Type[From], to_type: Type[To]) -> To:
        key = ConversionKey(from_type , to_type)
        conv = self._conversions[key]
        return conv.convert(context, self, from_obj)

    def find_conversions(self, value: BaboonGenerated) -> list[Conversion]:
        conversions = self._type_conversions.get(type(value))
        if conversions is not None:
            return conversions
        if isinstance(value, BaboonAdtMemberMeta):
            return self._type_conversions.get(value.baboon_adt_type, [])
        return []

    @property
    def versions_from(self) -> list[str]:
        raise NotImplementedError

    @property
    def version_to(self) -> str:
        raise NotImplementedError