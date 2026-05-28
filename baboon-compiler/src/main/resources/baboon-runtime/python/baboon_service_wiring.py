from dataclasses import dataclass
from typing import Any, Awaitable, Dict, Generic, Protocol, TypeVar, Union, runtime_checkable


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


class NoMatchingService(BaboonWiringError):
    def __init__(self, method: BaboonMethodId):
        self.method = method

    def __repr__(self):
        return f"NoMatchingService({self.method})"


class DuplicateService(BaboonWiringError):
    def __init__(self, service_name: str):
        self.service_name = service_name

    def __repr__(self):
        return f"DuplicateService({self.service_name!r})"


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


# --- Service muxers ---
#
# Cross-domain composable dispatch. A muxer holds a set of services from any
# model(s) and routes an `(method, data, ctx)` call to the right one by
# `method.service_name`. The R type parameter encodes the return shape so the
# same class supports both sync and async generated services — for sync code
# use `JsonMuxer[str]` / `UebaMuxer[bytes]`; for async code use
# `JsonMuxer[Awaitable[str]]` / `UebaMuxer[Awaitable[bytes]]`. The per-service
# wrapper classes emitted alongside `invoke_json_X` / `invoke_ueba_X` carry
# the matching parameterisation. The muxer is intentionally a single class
# rather than two (sync / async) — generic in R via PEP 484 type-vars; the
# wrapper class's `invoke` annotation picks the concrete return type.
#
# Python `Protocol` (PEP 544) gives us structural typing so the generated
# wrapper classes can satisfy `IBaboonJsonService` / `IBaboonUebaService`
# without an explicit `class X(IBaboonJsonService)` declaration — purely by
# having the right attributes.

R = TypeVar("R")


@runtime_checkable
class IBaboonJsonService(Protocol[R]):
    service_name: str

    def invoke(self, method: BaboonMethodId, data: str, ctx) -> R: ...


@runtime_checkable
class IBaboonUebaService(Protocol[R]):
    service_name: str

    def invoke(self, method: BaboonMethodId, data: bytes, ctx) -> R: ...


class JsonMuxer(Generic[R]):
    """Cross-domain JSON dispatcher. Routes `(method, data, ctx)` to a registered
    service whose `service_name` matches `method.service_name`. Strict fail-fast
    on duplicate registration or unknown service.
    """

    def __init__(self, *services: IBaboonJsonService[R]) -> None:
        self._table: Dict[str, IBaboonJsonService[R]] = {}
        for s in services:
            self.register(s)

    def register(self, service: IBaboonJsonService[R]) -> None:
        if service.service_name in self._table:
            raise BaboonWiringException(DuplicateService(service.service_name))
        self._table[service.service_name] = service

    def invoke(self, method: BaboonMethodId, data: str, ctx) -> R:
        service = self._table.get(method.service_name)
        if service is None:
            raise BaboonWiringException(NoMatchingService(method))
        return service.invoke(method, data, ctx)

    def service_names(self) -> list:
        return list(self._table.keys())


class UebaMuxer(Generic[R]):
    """Cross-domain UEBA dispatcher. Routes `(method, data, ctx)` to a registered
    service whose `service_name` matches `method.service_name`. Strict fail-fast
    on duplicate registration or unknown service.
    """

    def __init__(self, *services: IBaboonUebaService[R]) -> None:
        self._table: Dict[str, IBaboonUebaService[R]] = {}
        for s in services:
            self.register(s)

    def register(self, service: IBaboonUebaService[R]) -> None:
        if service.service_name in self._table:
            raise BaboonWiringException(DuplicateService(service.service_name))
        self._table[service.service_name] = service

    def invoke(self, method: BaboonMethodId, data: bytes, ctx) -> R:
        service = self._table.get(method.service_name)
        if service is None:
            raise BaboonWiringException(NoMatchingService(method))
        return service.invoke(method, data, ctx)

    def service_names(self) -> list:
        return list(self._table.keys())
