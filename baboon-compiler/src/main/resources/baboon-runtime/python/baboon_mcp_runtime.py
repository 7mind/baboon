# Additive MCP server runtime (decisions ledger M1; contract:
# docs/research/mcp-dispatch-runtime-contract.md). Emitted only when
# `--python-generate-mcp-server=true`; the service-wiring runtime in
# baboon_service_wiring.py is unchanged. These types are STATIC (no per-model
# templating) -- the only per-model code is the generated `<Service>McpServer`
# and its tool-registry literals.
#
# The MCP dispatch surface is transport-abstract: it implements the JSON-RPC
# method state machine + tool dispatch ONLY. Bytes-in/bytes-out (stdio framing,
# Streamable-HTTP bodies) is an injected adapter the generated surface never
# contains -- mirroring the abstract-context service contract, which supplies
# `Ctx` per invocation rather than baking an I/O loop into the wrapper.

import json
from abc import ABC, abstractmethod
from dataclasses import dataclass
from typing import Any, Callable, Dict, Generic, List, Optional, TypeVar

from .baboon_service_wiring import BaboonLeft, BaboonMethodId, BaboonRight, BaboonWiringError

Ctx = TypeVar("Ctx")

# JSON-RPC / MCP protocol constants (wire contract K4).
MCP_PROTOCOL_VERSION = "2025-06-18"

JSONRPC_PARSE_ERROR = -32700
JSONRPC_INVALID_REQUEST = -32600
JSONRPC_METHOD_NOT_FOUND = -32601
JSONRPC_INVALID_PARAMS = -32602
JSONRPC_INTERNAL_ERROR = -32603


@dataclass
class McpServerInfo:
    name: str
    version: str


@dataclass
class McpToolEntry:
    name: str
    method: BaboonMethodId
    input_schema: Any  # pre-parsed JSON object (dict)
    description: Optional[str] = None


# --- Per-connection state (adapter-owned) ---
#
# The "initialized" precondition (reject `tools/*` before a successful
# `initialize`) is per-connection state; a connection is a transport concept.
# The latch therefore lives in this tiny value the adapter creates per
# connection, NOT as ambient mutable state inside the server object (which stays
# immutable and shareable across concurrent connections).
class McpSession:
    def __init__(self) -> None:
        self.initialized: bool = False


# --- Dispatch interface ---
#
# The single generated entrypoint, analogous to `IBaboonJsonServiceCtx[Ctx, R]`.
# It is NOT R-parametric (an MCP response is always a dict value);
# the only free type parameter is the caller's `Ctx` -- the SAME `Ctx` the
# service-wiring contract threads. `handle` is synchronous and performs no I/O.
# Returns None for an accepted notification (no reply).
class IBaboonMcpServer(ABC, Generic[Ctx]):
    @abstractmethod
    def handle(
        self,
        request: Dict[str, Any],
        session: McpSession,
        ctx: Ctx,
        codec_ctx: Any,
    ) -> Optional[Dict[str, Any]]:
        raise NotImplementedError


# The JSON `tools/call` delegate type the generated server accepts:
# (method, data, ctx, codec_ctx) -> BaboonEither[BaboonWiringError, str]
McpJsonInvoke = Callable[[BaboonMethodId, str, Any, Any], Any]


# --- Transport-abstract dispatch base ---
#
# Shared `handle` state machine. The generated `<Service>McpServer` extends this
# with its fixed `server_info`, ordered tool registry, and `invoke_json` delegate.
# All JSON-RPC method strings ("tools/list" ...) and result keys ("protocolVersion",
# "inputSchema" ...) are literal lowercase strings, NOT subject to any per-language
# symbol casing.
class AbstractBaboonMcpServer(IBaboonMcpServer[Ctx]):
    @property
    @abstractmethod
    def server_info(self) -> McpServerInfo:
        raise NotImplementedError

    @property
    @abstractmethod
    def tools(self) -> List[McpToolEntry]:
        raise NotImplementedError

    @abstractmethod
    def invoke_json(
        self,
        method: BaboonMethodId,
        data: str,
        ctx: Ctx,
        codec_ctx: Any,
    ) -> Any:  # BaboonEither[BaboonWiringError, str]
        raise NotImplementedError

    def _by_name(self) -> Dict[str, McpToolEntry]:
        return {t.name: t for t in self.tools}

    def handle(
        self,
        request: Dict[str, Any],
        session: McpSession,
        ctx: Ctx,
        codec_ctx: Any,
    ) -> Optional[Dict[str, Any]]:
        req_id = request.get("id")
        method = request.get("method", "")
        params = request.get("params")

        if method == "initialize":
            pv = None
            if isinstance(params, dict):
                pv = params.get("protocolVersion")
            if pv is None:
                return self._error_response(req_id, JSONRPC_INVALID_PARAMS, "initialize: missing protocolVersion")
            session.initialized = True
            return {
                "jsonrpc": "2.0",
                "id": req_id,
                "result": {
                    "protocolVersion": MCP_PROTOCOL_VERSION,
                    "capabilities": {"tools": {}},
                    "serverInfo": {
                        "name": self.server_info.name,
                        "version": self.server_info.version,
                    },
                },
            }

        elif method == "notifications/initialized":
            # Notification -- no response.
            return None

        elif method == "tools/list":
            if not session.initialized:
                return self._error_response(req_id, JSONRPC_INVALID_REQUEST, "tools/list before initialize")
            tools_list = []
            for t in self.tools:
                entry: Dict[str, Any] = {"name": t.name, "inputSchema": t.input_schema}
                if t.description is not None:
                    entry["description"] = t.description
                tools_list.append(entry)
            return {
                "jsonrpc": "2.0",
                "id": req_id,
                "result": {"tools": tools_list},
            }

        elif method == "tools/call":
            if not session.initialized:
                return self._error_response(req_id, JSONRPC_INVALID_REQUEST, "tools/call before initialize")
            if not isinstance(params, dict):
                return self._error_response(req_id, JSONRPC_INVALID_PARAMS, "tools/call: missing params")
            name = params.get("name")
            if not isinstance(name, str):
                return self._error_response(req_id, JSONRPC_INVALID_PARAMS, "tools/call: missing tool name")
            entry = self._by_name().get(name)
            if entry is None:
                return self._error_response(req_id, JSONRPC_INVALID_PARAMS, f"tools/call: unknown tool '{name}'")
            args = params.get("arguments") or {}
            args_json = json.dumps(args)
            result = self.invoke_json(entry.method, args_json, ctx, codec_ctx)
            if isinstance(result, BaboonRight):
                return {
                    "jsonrpc": "2.0",
                    "id": req_id,
                    "result": {
                        "content": [{"type": "text", "text": result.value}],
                        "isError": False,
                    },
                }
            else:
                # Channel B: a valid protocol call whose domain payload failed.
                return {
                    "jsonrpc": "2.0",
                    "id": req_id,
                    "result": {
                        "content": [{"type": "text", "text": self._describe_wiring_error(result.value)}],
                        "isError": True,
                    },
                }

        else:
            return self._error_response(req_id, JSONRPC_METHOD_NOT_FOUND, f"Method not found: {method}")

    def _error_response(self, req_id: Any, code: int, message: str) -> Dict[str, Any]:
        return {
            "jsonrpc": "2.0",
            "id": req_id,
            "error": {"code": code, "message": message},
        }

    def _describe_wiring_error(self, e: BaboonWiringError) -> str:
        return repr(e)


# --- Async dispatch interface ---
#
# Async sibling of `IBaboonMcpServer`, selected when the backend is generated
# with `--py-async-services=true`. The errors-mode wiring entry `invoke_json_X`
# is then an `async def` returning a coroutine that yields
# `BaboonEither[BaboonWiringError, str]`, so the `tools/call` dispatch must
# `await` the delegate; `handle` is therefore a coroutine itself. The integrator
# awaits the returned coroutine.
class IBaboonAsyncMcpServer(ABC, Generic[Ctx]):
    @abstractmethod
    async def handle(
        self,
        request: Dict[str, Any],
        session: McpSession,
        ctx: Ctx,
        codec_ctx: Any,
    ) -> Optional[Dict[str, Any]]:
        raise NotImplementedError


# The async JSON `tools/call` delegate type the generated async server accepts:
# (method, data, ctx, codec_ctx) -> Awaitable[BaboonEither[BaboonWiringError, str]]
McpJsonInvokeAsync = Callable[[BaboonMethodId, str, Any, Any], Any]


# --- Async transport-abstract dispatch base ---
#
# Async sibling of `AbstractBaboonMcpServer`. Selected when the backend is
# generated with `--py-async-services=true`: the `tools/call` dispatch awaits the
# async `invoke_json` (whose result is the async wiring's awaitable
# `BaboonEither[..]`), so `handle` itself is an `async def`. The synchronous
# method state machine (initialize / tools/list, the error mapping, the wire
# constants) is identical to the sync base; only the single `tools/call` hop
# awaits.
class AbstractAsyncBaboonMcpServer(IBaboonAsyncMcpServer[Ctx]):
    @property
    @abstractmethod
    def server_info(self) -> McpServerInfo:
        raise NotImplementedError

    @property
    @abstractmethod
    def tools(self) -> List[McpToolEntry]:
        raise NotImplementedError

    @abstractmethod
    async def invoke_json(
        self,
        method: BaboonMethodId,
        data: str,
        ctx: Ctx,
        codec_ctx: Any,
    ) -> Any:  # Awaitable[BaboonEither[BaboonWiringError, str]]
        raise NotImplementedError

    def _by_name(self) -> Dict[str, McpToolEntry]:
        return {t.name: t for t in self.tools}

    async def handle(
        self,
        request: Dict[str, Any],
        session: McpSession,
        ctx: Ctx,
        codec_ctx: Any,
    ) -> Optional[Dict[str, Any]]:
        req_id = request.get("id")
        method = request.get("method", "")
        params = request.get("params")

        if method == "initialize":
            pv = None
            if isinstance(params, dict):
                pv = params.get("protocolVersion")
            if pv is None:
                return self._error_response(req_id, JSONRPC_INVALID_PARAMS, "initialize: missing protocolVersion")
            session.initialized = True
            return {
                "jsonrpc": "2.0",
                "id": req_id,
                "result": {
                    "protocolVersion": MCP_PROTOCOL_VERSION,
                    "capabilities": {"tools": {}},
                    "serverInfo": {
                        "name": self.server_info.name,
                        "version": self.server_info.version,
                    },
                },
            }

        elif method == "notifications/initialized":
            # Notification -- no response.
            return None

        elif method == "tools/list":
            if not session.initialized:
                return self._error_response(req_id, JSONRPC_INVALID_REQUEST, "tools/list before initialize")
            tools_list = []
            for t in self.tools:
                entry: Dict[str, Any] = {"name": t.name, "inputSchema": t.input_schema}
                if t.description is not None:
                    entry["description"] = t.description
                tools_list.append(entry)
            return {
                "jsonrpc": "2.0",
                "id": req_id,
                "result": {"tools": tools_list},
            }

        elif method == "tools/call":
            if not session.initialized:
                return self._error_response(req_id, JSONRPC_INVALID_REQUEST, "tools/call before initialize")
            if not isinstance(params, dict):
                return self._error_response(req_id, JSONRPC_INVALID_PARAMS, "tools/call: missing params")
            name = params.get("name")
            if not isinstance(name, str):
                return self._error_response(req_id, JSONRPC_INVALID_PARAMS, "tools/call: missing tool name")
            entry = self._by_name().get(name)
            if entry is None:
                return self._error_response(req_id, JSONRPC_INVALID_PARAMS, f"tools/call: unknown tool '{name}'")
            args = params.get("arguments") or {}
            args_json = json.dumps(args)
            result = await self.invoke_json(entry.method, args_json, ctx, codec_ctx)
            if isinstance(result, BaboonRight):
                return {
                    "jsonrpc": "2.0",
                    "id": req_id,
                    "result": {
                        "content": [{"type": "text", "text": result.value}],
                        "isError": False,
                    },
                }
            else:
                # Channel B: a valid protocol call whose domain payload failed.
                return {
                    "jsonrpc": "2.0",
                    "id": req_id,
                    "result": {
                        "content": [{"type": "text", "text": self._describe_wiring_error(result.value)}],
                        "isError": True,
                    },
                }

        else:
            return self._error_response(req_id, JSONRPC_METHOD_NOT_FOUND, f"Method not found: {method}")

    def _error_response(self, req_id: Any, code: int, message: str) -> Dict[str, Any]:
        return {
            "jsonrpc": "2.0",
            "id": req_id,
            "error": {"code": code, "message": message},
        }

    def _describe_wiring_error(self, e: BaboonWiringError) -> str:
        return repr(e)
