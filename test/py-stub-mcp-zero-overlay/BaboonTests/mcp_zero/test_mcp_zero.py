# T178 / D40 — Python zero-service MCP overlay test (RED baseline).
#
# Purpose: prove the ZERO-SERVICE MCP runtime contract. The zero-service model
# (mcp-stub-zero-services-ok) declares @root types but NO RPC block, so
# servicesOf(domain) is empty and NO per-service `<Service>McpServer` module is
# generated. With zero services the ONLY source of the MCP runtime symbols
# (McpMuxer / McpServerInfo / McpSession) is the STATIC runtime module
# `baboon_mcp_runtime.py`.
#
# RED (pre-fix): the current generator emits NO `baboon_mcp_runtime.py` for a
# zero-service model, so the import below fails and the test is collected as an
# ImportError / ModuleNotFoundError:
# "No module named 'BaboonDefinitions.Generated.baboon_mcp_runtime'". That
# failure IS the D40 reproduction.
#
# GREEN (post-fix, later task): once the generator emits the static MCP runtime
# even when servicesOf(domain) is empty, this file imports cleanly and the
# runtime assertions below pass — an empty muxer lists zero tools and rejects any
# tools/call with JSON-RPC -32602.
#
# Assertion discipline: explicit `raise AssertionError` on failure — unconditional.

import unittest

from BaboonDefinitions.Generated.baboon_codecs import BaboonCodecContext

# COMPILE-TIME / IMPORT-TIME contract: importing McpMuxer / McpServerInfo /
# McpSession requires the static runtime module to exist. With zero services
# there is no generated <Service>McpServer module — these symbols resolve ONLY
# from baboon_mcp_runtime.py.
from BaboonDefinitions.Generated.baboon_mcp_runtime import (
    McpMuxer,
    McpServerInfo,
    McpSession,
)

JSONRPC_INVALID_PARAMS = -32602


def _make_empty_muxer():
    return McpMuxer(McpServerInfo(name="ZeroEndpoint", version="1.0.0"))


def _inited_session(mux, codec_ctx):
    session = McpSession()
    mux.handle(
        {
            "jsonrpc": "2.0",
            "id": 0,
            "method": "initialize",
            "params": {
                "protocolVersion": "2025-06-18",
                "capabilities": {},
                "clientInfo": {"name": "t", "version": "0"},
            },
        },
        session,
        None,
        codec_ctx,
    )
    mux.handle({"jsonrpc": "2.0", "method": "notifications/initialized"}, session, None, codec_ctx)
    return session


class ZeroServiceMcpTests(unittest.TestCase):
    # RUNTIME contract (a): tools/list on an empty muxer returns an empty array.
    def test_tools_list_is_empty(self):
        codec_ctx = BaboonCodecContext.Default
        mux = _make_empty_muxer()
        session = _inited_session(mux, codec_ctx)

        resp = mux.handle({"jsonrpc": "2.0", "id": 1, "method": "tools/list"}, session, None, codec_ctx)
        if resp is None:
            raise AssertionError("tools/list must return a response, got None")
        if resp.get("error") is not None:
            raise AssertionError("tools/list must not return an error")
        tools = resp["result"]["tools"]
        if not isinstance(tools, list):
            raise AssertionError("result must carry a tools array")
        if len(tools) != 0:
            raise AssertionError("empty muxer MUST list zero tools, got %d" % len(tools))

    # RUNTIME contract (b): an unknown tool call yields JSON-RPC -32602.
    def test_unknown_tool_call_code_32602(self):
        codec_ctx = BaboonCodecContext.Default
        mux = _make_empty_muxer()
        session = _inited_session(mux, codec_ctx)

        resp = mux.handle(
            {
                "jsonrpc": "2.0",
                "id": 2,
                "method": "tools/call",
                "params": {"name": "anything_at_all", "arguments": {}},
            },
            session,
            None,
            codec_ctx,
        )
        if resp is None:
            raise AssertionError("tools/call must return a response, got None")
        if resp.get("result") is not None:
            raise AssertionError("no result expected for unknown tool")
        error = resp.get("error")
        if error is None:
            raise AssertionError("unknown tool on empty muxer MUST produce a Channel-A error")
        if error["code"] != JSONRPC_INVALID_PARAMS:
            raise AssertionError("unknown-tool error code MUST be -32602 (InvalidParams), got %s" % error["code"])
        if not error.get("message"):
            raise AssertionError("error.message must be non-empty")


if __name__ == "__main__":
    unittest.main()
