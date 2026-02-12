from dataclasses import dataclass
from typing import Any


@dataclass(frozen=True)
class BaboonMethodId:
    service_name: str
    method_name: str


class BaboonWiringError:
    pass


class NoMatchingMethod(BaboonWiringError):
    def __init__(self, method: BaboonMethodId):
        self.method = method

    def __repr__(self):
        return f"NoMatchingMethod({self.method})"


class DecoderFailed(BaboonWiringError):
    def __init__(self, method: BaboonMethodId, exception: Exception):
        self.method = method
        self.exception = exception

    def __repr__(self):
        return f"DecoderFailed({self.method}, {self.exception})"


class EncoderFailed(BaboonWiringError):
    def __init__(self, method: BaboonMethodId, exception: Exception):
        self.method = method
        self.exception = exception

    def __repr__(self):
        return f"EncoderFailed({self.method}, {self.exception})"


class CallFailed(BaboonWiringError):
    def __init__(self, method: BaboonMethodId, domain_error: Any):
        self.method = method
        self.domain_error = domain_error

    def __repr__(self):
        return f"CallFailed({self.method}, {self.domain_error})"


class BaboonWiringException(Exception):
    def __init__(self, error: BaboonWiringError):
        super().__init__(str(error))
        self.error = error


class BaboonEither:
    pass


class BaboonLeft(BaboonEither):
    def __init__(self, value):
        self.value = value

    def __repr__(self):
        return f"BaboonLeft({self.value!r})"


class BaboonRight(BaboonEither):
    def __init__(self, value):
        self.value = value

    def __repr__(self):
        return f"BaboonRight({self.value!r})"
