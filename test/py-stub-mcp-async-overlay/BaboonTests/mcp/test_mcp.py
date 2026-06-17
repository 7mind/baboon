# T60/T61 (D24/G11) — Python ASYNC-MCP RUNTIME lane (GREEN after the T61 fix).
#
# Async sibling of test/py-stub-mcp-overlay/BaboonTests/mcp/test_mcp.py, generated
# with BOTH --py-generate-mcp-server=true AND --py-async-services=true.
#
# THE DEFECT (D24) and ITS FIX (T61):
#   Under --py-async-services=true the generated errors-mode wiring entry
#     `invoke_json_McpTools(...)` is an `async def`
#     (PyServiceWiringTranslator.scala:36-39 + :793 — asyncPrefix = "async ").
#   Pre-T61 the generated MCP server `McpToolsMcpServer` was NOT async-aware: its
#     `invoke_json` and the inherited `AbstractBaboonMcpServer.handle` both called
#     the delegate SYNCHRONOUSLY, so the `async def` returned an un-awaited
#     coroutine object (neither BaboonRight nor BaboonLeft).
#   T61 threads `asyncServices` into PyMcpServerGenerator: under the flag the
#     generated server extends `AbstractAsyncBaboonMcpServer`, its `invoke_json`
#     is an `async def` that `await`s the delegate, and the inherited async
#     `handle` is itself a coroutine that awaits the dispatch. The integrator
#     awaits the coroutine returned by `handle`.
#
# EXPECTED GREEN (post-fix T61):
#   `test_ping_returns_ok_true_async` awaits a real tools/call and asserts on the
#   AWAITED JSON result (ok == True) — no coroutine object, no
#   `RuntimeWarning: coroutine ... was never awaited`. The sync lane
#   (test-py-mcp) is unaffected.
#
# Run from py-stub/:
#   python3 -m unittest BaboonTests.mcp.test_mcp

import asyncio
import inspect
import json
import unittest

from BaboonDefinitions.Generated.baboon_codecs import BaboonCodecContext
from BaboonDefinitions.Generated.baboon_mcp_runtime import McpSession
from BaboonDefinitions.Generated.mcp.stub.BaboonServiceRt import BaboonServiceRtDefault
from BaboonDefinitions.Generated.mcp.stub.McpTools import McpTools
from BaboonDefinitions.Generated.mcp.stub.McpToolsMcpServer import McpToolsMcpServer
from BaboonDefinitions.Generated.mcp.stub.McpTools_Wiring import invoke_json_McpTools
from BaboonDefinitions.Generated.mcp.stub.mcptools.listcollections.Out import Out as ListCollectionsOut
from BaboonDefinitions.Generated.mcp.stub.mcptools.submitcomposite.Out import Out as SubmitCompositeOut
from BaboonDefinitions.Generated.mcp.stub.mcptools.processshape.Out import Out as ProcessShapeOut
from BaboonDefinitions.Generated.mcp.stub.mcptools.processtagged.Out import Out as ProcessTaggedOut
from BaboonDefinitions.Generated.mcp.stub.mcptools.pagepoints.Out import Out as PagePointsOut
from BaboonDefinitions.Generated.mcp.stub.mcptools.ping.Out import Out as PingOut
from BaboonDefinitions.Generated.mcp.stub.mcptools.describepricing.Out import Out as DescribePricingOut


# ---------------------------------------------------------------------------
# Stub McpTools service. Under --py-async-services=true the generated McpTools
# interface methods are `async def` (PyDefnTranslator.scala:443). The stub must
# therefore define `async def` methods returning ok=true (T7 §3 convention).
# ---------------------------------------------------------------------------

class _StubMcpTools(McpTools):
    async def listCollections(self, arg): return ListCollectionsOut(ok=True)
    async def submitComposite(self, arg): return SubmitCompositeOut(ok=True)
    async def processShape(self, arg): return ProcessShapeOut(ok=True)
    async def processTagged(self, arg): return ProcessTaggedOut(ok=True)
    async def pagePoints(self, arg): return PagePointsOut(ok=True)
    async def ping(self, arg): return PingOut(ok=True)
    async def describePricing(self, arg): return DescribePricingOut(ok=True)


def _make_server():
    rt = BaboonServiceRtDefault()
    stub = _StubMcpTools()

    # Under --py-async-services=true the generated McpToolsMcpServer extends the
    # async runtime base and takes an ASYNC delegate
    # (Awaitable[BaboonEither[..]]). Returning the coroutine from
    # `invoke_json_McpTools` (an `async def`) is correct: the server's async
    # `invoke_json` / `handle` await it.
    def _async_invoke(method, data, ctx, codec_context):
        return invoke_json_McpTools(method, data, stub, rt, codec_context)

    return McpToolsMcpServer(_async_invoke)


async def _init_session(server, session, codec_ctx):
    await server.handle(
        {
            "jsonrpc": "2.0",
            "id": 0,
            "method": "initialize",
            "params": {
                "protocolVersion": "2025-06-18",
                "capabilities": {},
                "clientInfo": {"name": "test-client", "version": "0.0.1"},
            },
        },
        session,
        None,
        codec_ctx,
    )
    await server.handle(
        {"jsonrpc": "2.0", "method": "notifications/initialized"},
        session,
        None,
        codec_ctx,
    )


async def _send(server, session, codec_ctx, req):
    resp = await server.handle(req, session, None, codec_ctx)
    if resp is None:
        raise AssertionError(f"Expected a response for {req['method']!r} but got None")
    return resp


class Sec3AsyncToolsCallTests(unittest.TestCase):

    def test_ping_returns_ok_true_async(self):
        # THE GREEN POINT (post-T61).
        #
        # A real tools/call round-trips through the async dispatch and yields the
        # AWAITED JSON result {"ok": true}. The async MCP server awaits the
        # `async def` delegate, so `tools/call` returns the awaited Either value
        # (NOT a coroutine object), and the runtime maps BaboonRight → Channel-A
        # success.
        async def scenario():
            codec_ctx = BaboonCodecContext.Default
            server = _make_server()
            session = McpSession()
            await _init_session(server, session, codec_ctx)

            call = {
                "jsonrpc": "2.0",
                "id": 3,
                "method": "tools/call",
                "params": {"name": "McpTools_ping", "arguments": {"seqno": 42, "label": "hello"}},
            }
            return await _send(server, session, codec_ctx, call)

        resp = asyncio.run(scenario())

        self.assertEqual(resp["id"], 3)
        self.assertNotIn("error", resp, "Unexpected Channel-A error on async ping call")

        result = resp["result"]
        content = result["content"]
        self.assertEqual(len(content), 1, "content must have exactly one element")
        self.assertEqual(content[0]["type"], "text")

        text = content[0]["text"]

        # Right-reason guard: the body must NOT be a coroutine repr (the pre-T61
        # un-awaited-coroutine symptom). It must be the awaited JSON result.
        if "coroutine" in text or "<coroutine object" in text:
            raise AssertionError(
                "D24 un-awaited-coroutine defect: the async MCP server returned a "
                f"coroutine object instead of the awaited JSON result. text = {text!r}; "
                f"isError = {result.get('isError')!r}."
            )

        payload = json.loads(text)
        self.assertEqual(payload["ok"], True, "ok must be true (awaited async dispatch result)")

        is_error = result.get("isError")
        if is_error is not None and is_error is not False:
            raise AssertionError(f"isError must be false or absent, got {is_error!r}")

    def test_invoke_delegate_is_a_coroutine_function(self):
        # Structural control: documents that the generated wiring entry is
        # `async def`, so the server MUST await it (which the async server now
        # does). Would need revisiting if the fix changed the wiring shape.
        self.assertTrue(
            inspect.iscoroutinefunction(invoke_json_McpTools),
            "Under --py-async-services=true the generated invoke_json_McpTools "
            "MUST be an async def (coroutine function).",
        )

    def test_handle_is_a_coroutine_function(self):
        # Structural control: the generated async MCP server's inherited `handle`
        # is a coroutine function (async def), so the integrator awaits it. This
        # pins the fix that converted the server's dispatch path to async.
        server = _make_server()
        self.assertTrue(
            inspect.iscoroutinefunction(server.handle),
            "Under --py-async-services=true the generated MCP server's `handle` "
            "MUST be an async def (coroutine function) that awaits the delegate.",
        )

    def test_awaited_dispatch_yields_ok_true_json(self):
        # Control: when the delegate IS awaited directly, the dispatch yields
        # awaited JSON {"ok": true}. Proves the generated wiring + stub are
        # correct independent of the server.
        rt = BaboonServiceRtDefault()
        stub = _StubMcpTools()
        codec_ctx = BaboonCodecContext.Default

        from BaboonDefinitions.Generated.baboon_service_wiring import BaboonMethodId

        method = BaboonMethodId("McpTools", "ping")
        args_json = json.dumps({"seqno": 42, "label": "hello"})

        awaited = asyncio.run(invoke_json_McpTools(method, args_json, stub, rt, codec_ctx))

        # awaited is a BaboonRight/BaboonLeft (Either) — extract the JSON value.
        value = getattr(awaited, "value", awaited)
        payload = json.loads(value)
        self.assertEqual(payload["ok"], True, "awaited dispatch must yield ok=true JSON")


if __name__ == "__main__":
    unittest.main()
