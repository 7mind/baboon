# T60 (D24/G11) — Python ASYNC-MCP RUNTIME red-repro overlay test.
#
# Async sibling of test/py-stub-mcp-overlay/BaboonTests/mcp/test_mcp.py, generated
# with BOTH --py-generate-mcp-server=true AND --py-async-services=true.
#
# THE DEFECT (D24) — silent un-awaited coroutine at RUNTIME:
#   Under --py-async-services=true the generated errors-mode wiring entry
#     `invoke_json_McpTools(...)` is an `async def`
#     (PyServiceWiringTranslator.scala:36-39 + :793 — asyncPrefix = "async ").
#   The generated MCP server `McpToolsMcpServer` is NOT async-aware: its
#     `invoke_json` (PyMcpServerGenerator.scala:147) and the inherited
#     `AbstractBaboonMcpServer.handle` both call the delegate SYNCHRONOUSLY
#     (baboon_mcp_runtime.py:177 — `result = self.invoke_json(...)`).
#   Python's MCP delegate type is dynamic (`Callable[..., object]`), so this
#   does NOT fail to compile — calling the `async def` synchronously SILENTLY
#   returns a COROUTINE OBJECT (never awaited). The server then runs
#     `isinstance(result, BaboonRight)` → False (a coroutine is neither
#     BaboonRight nor BaboonLeft), drops into the Channel-B branch, and emits
#     `repr(coroutine)` (e.g. "<coroutine object invoke_json_McpTools at 0x..>")
#     as `content[0].text` with isError=True. That text is NOT JSON.
#
# EXPECTED RED (today, pre-fix T61):
#   `test_ping_returns_ok_true_async` performs a real tools/call and asserts on
#   the AWAITED JSON result (ok == True). It FAILS because the sync delegate call
#   returns a coroutine: the body is a coroutine repr (non-JSON) inside an
#   isError=True Channel-B result. The assertion pinpoints the un-awaited-coroutine
#   symptom (NOT an import/harness error). The sync lane (test-py-mcp) is unaffected.
#
# After the Python async-MCP backend fix (T61) threads asyncServices into the MCP
# server generator (async server base that awaits the delegate), this lane goes
# GREEN. DO NOT wire it green here — this task only reproduces the defect.
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


def _make_server():
    rt = BaboonServiceRtDefault()
    stub = _StubMcpTools()

    # The generated McpToolsMcpServer requires a SYNC delegate
    # `Callable[[BaboonMethodId, str, Ctx, object], object]`. Because the
    # underlying `invoke_json_McpTools` is an `async def`, calling it (without
    # await) returns a coroutine object — exactly what the un-awaited-coroutine
    # defect feeds into the server's sync dispatch.
    def _fake_invoke(method, data, ctx, codec_context):
        return invoke_json_McpTools(method, data, stub, rt, codec_context)

    return McpToolsMcpServer(_fake_invoke)


def _init_session(server, session, codec_ctx):
    server.handle(
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
    server.handle(
        {"jsonrpc": "2.0", "method": "notifications/initialized"},
        session,
        None,
        codec_ctx,
    )


def _send(server, session, codec_ctx, req):
    resp = server.handle(req, session, None, codec_ctx)
    if resp is None:
        raise AssertionError(f"Expected a response for {req['method']!r} but got None")
    return resp


class Sec3AsyncToolsCallTests(unittest.TestCase):

    def test_ping_returns_ok_true_async(self):
        # THE RED-REPRO POINT.
        #
        # A real tools/call must round-trip through the async dispatch and yield
        # the AWAITED JSON result {"ok": true}. Today the sync MCP server calls
        # the `async def` delegate without awaiting it. The returned coroutine is
        # neither BaboonRight nor BaboonLeft, so the runtime's sync dispatch
        # (baboon_mcp_runtime.py: `isinstance(result, BaboonRight)` → False, then
        # `_describe_wiring_error(result.value)`) raises AttributeError /
        # TypeError on the coroutine, OR — depending on the runtime version —
        # places the coroutine's repr into a non-JSON Channel-B body. Both are the
        # same un-awaited-coroutine symptom; the assertions below pinpoint it.
        codec_ctx = BaboonCodecContext.Default
        server = _make_server()
        session = McpSession()
        _init_session(server, session, codec_ctx)

        call = {
            "jsonrpc": "2.0",
            "id": 3,
            "method": "tools/call",
            "params": {"name": "McpTools_ping", "arguments": {"seqno": 42, "label": "hello"}},
        }

        try:
            resp = _send(server, session, codec_ctx, call)
        except (AttributeError, TypeError) as e:
            # The sync server reached `result.value` / isinstance on a coroutine.
            raise AssertionError(
                "D24 un-awaited-coroutine defect reproduced: the sync MCP server "
                "called the `async def` invoke_json_McpTools delegate WITHOUT awaiting "
                "it, so `tools/call` dispatch received a coroutine object (neither "
                "BaboonRight nor BaboonLeft) and the runtime raised "
                f"{type(e).__name__}: {e}. Expected the awaited JSON result {{'ok': true}}."
            ) from e

        self.assertEqual(resp["id"], 3)
        self.assertNotIn("error", resp, "Unexpected Channel-A error on async ping call")

        result = resp["result"]
        content = result["content"]
        self.assertEqual(len(content), 1, "content must have exactly one element")
        self.assertEqual(content[0]["type"], "text")

        text = content[0]["text"]

        # Pinpoint the un-awaited-coroutine symptom for a RIGHT-reason failure:
        # the body is the repr of a coroutine object instead of awaited JSON.
        if "coroutine" in text or "<coroutine object" in text:
            raise AssertionError(
                "D24 un-awaited-coroutine defect reproduced: the sync MCP server "
                "called the `async def` invoke_json_McpTools delegate WITHOUT awaiting "
                "it, so tools/call returned a coroutine object instead of the awaited "
                f"JSON result. content[0].text = {text!r}; "
                f"isError = {result.get('isError')!r}."
            )

        # If the body is not the coroutine repr it must be awaited JSON — parse it.
        # (json.loads of a coroutine repr raises JSONDecodeError, which is also a
        # right-reason RED for the non-JSON Channel-B body symptom.)
        try:
            payload = json.loads(text)
        except json.JSONDecodeError as e:
            raise AssertionError(
                "D24 un-awaited-coroutine defect reproduced: tools/call body is not "
                f"JSON (un-awaited coroutine / non-JSON Channel-B body). text = {text!r}; "
                f"isError = {result.get('isError')!r}; json error = {e}."
            )

        self.assertEqual(payload["ok"], True, "ok must be true (awaited async dispatch result)")

        is_error = result.get("isError")
        if is_error is not None and is_error is not False:
            raise AssertionError(f"isError must be false or absent, got {is_error!r}")

    def test_invoke_delegate_is_a_coroutine_function(self):
        # Structural pin (not the primary RED): documents that the generated
        # wiring entry is `async def`, so any SYNCHRONOUS call to it (as the
        # generated MCP server performs) yields a coroutine object. This makes
        # the un-awaited-coroutine hazard explicit and would itself need
        # revisiting if the fix changed the wiring shape rather than the server.
        self.assertTrue(
            inspect.iscoroutinefunction(invoke_json_McpTools),
            "Under --py-async-services=true the generated invoke_json_McpTools "
            "MUST be an async def (coroutine function); the sync MCP server "
            "dispatch therefore returns an un-awaited coroutine.",
        )

    def test_awaited_dispatch_yields_ok_true_json(self):
        # Control: when the delegate IS awaited (as the post-T61 fix server will
        # do), the dispatch yields awaited JSON {"ok": true}. This proves the
        # generated wiring + stub are correct and that the RED above is caused
        # SOLELY by the server's missing await — not by a broken async stub or
        # codec. Runs the coroutine directly via asyncio.run.
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
