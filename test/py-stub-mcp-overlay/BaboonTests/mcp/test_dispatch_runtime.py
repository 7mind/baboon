import asyncio
import unittest

from BaboonDefinitions.Generated.baboon_mcp_runtime import (
    AbstractBaboonMcpServer, AbstractAsyncBaboonMcpServer,
    McpMuxer, AsyncMcpMuxer, McpServerInfo, McpToolEntry, McpSession,
    BaboonMcpWiringException,
)
from BaboonDefinitions.Generated.baboon_service_wiring import BaboonMethodId, BaboonRight, BaboonLeft


class SyncServer(AbstractBaboonMcpServer):
    def __init__(self):
        self.tool_name = "Service_call"

    @property
    def server_info(self):
        return McpServerInfo("Service", "1.0.0")

    @property
    def tools(self):
        return [McpToolEntry(self.tool_name, BaboonMethodId("Service", "call"), {"type": "object"}, "description")]

    def invoke_json(self, method, data, ctx, codec_ctx):
        if ctx == "error":
            return BaboonLeft("rejected")
        return BaboonRight(data)


class AsyncServer(AbstractAsyncBaboonMcpServer):
    server_info = SyncServer.server_info
    tools = SyncServer.tools
    __init__ = SyncServer.__init__

    async def invoke_json(self, method, data, ctx, codec_ctx):
        return SyncServer.invoke_json(self, method, data, ctx, codec_ctx)


class McpDispatchRuntimeTest(unittest.TestCase):
    def test_per_service_error_hooks_remain_effective(self):
        class Customized(SyncServer):
            def _error_response(self, req_id, code, message):
                return {"custom": code}

            def _describe_wiring_error(self, error):
                return "custom description"

        server = Customized()
        session = McpSession()
        self.assertEqual({"custom": -32602}, server.handle({"method": "initialize"}, session, None, None))
        self.assertEqual({"custom": -32600}, server.handle({"method": "tools/call"}, session, None, None))
        session.initialized = True
        result = server.handle({"method": "tools/call", "params": {"name": "Service_call"}}, session, "error", None)
        self.assertEqual("custom description", result["result"]["content"][0]["text"])

    def variants(self):
        sync = SyncServer()
        async_server = AsyncServer()
        return (
            (sync, False),
            (async_server, True),
            (McpMuxer(sync.server_info, sync), False),
            (AsyncMcpMuxer(async_server.server_info, async_server), True),
        )

    def invoke(self, server, is_async, request, session, ctx=None):
        result = server.handle(request, session, ctx, None)
        return asyncio.run(result) if is_async else result

    def test_protocol_responses_match_across_four_dispatchers(self):
        for server, is_async in self.variants():
            with self.subTest(server=type(server).__name__):
                session = McpSession()
                call = lambda req, ctx=None: self.invoke(server, is_async, req, session, ctx)
                before = call({"id": 1, "method": "tools/list"})
                self.assertEqual(-32600, before["error"]["code"])
                invalid_init = call({"id": 2, "method": "initialize", "params": {}})
                self.assertEqual(-32602, invalid_init["error"]["code"])
                self.assertFalse(session.initialized)
                initialized = call({"id": 3, "method": "initialize", "params": {"protocolVersion": "2025-06-18"}})
                self.assertEqual({"name": "Service", "version": "1.0.0"}, initialized["result"]["serverInfo"])
                self.assertIsNone(call({"method": "notifications/initialized"}))
                tools = call({"id": 4, "method": "tools/list"})["result"]["tools"]
                self.assertEqual([{"name": "Service_call", "inputSchema": {"type": "object"}, "description": "description"}], tools)
                for params in (None, {}, {"name": 42}, {"name": "unknown"}):
                    response = call({"id": 5, "method": "tools/call", "params": params})
                    self.assertEqual(-32602, response["error"]["code"])
                request = {"id": 6, "method": "tools/call", "params": {"name": "Service_call", "arguments": {"x": 1}}}
                self.assertEqual({"content": [{"type": "text", "text": '{"x": 1}'}], "isError": False}, call(request)["result"])
                self.assertEqual({"content": [{"type": "text", "text": "'rejected'"}], "isError": True}, call(request, "error")["result"])
                self.assertEqual(-32601, call({"id": 7, "method": "unknown"})["error"]["code"])

    def test_external_servers_keep_dynamic_tools(self):
        for server, is_async in ((SyncServer(), False), (AsyncServer(), True)):
            session = McpSession()
            session.initialized = True
            server.tool_name = "renamed"
            response = self.invoke(server, is_async, {"id": 1, "method": "tools/call", "params": {"name": "renamed"}}, session)
            self.assertFalse(response["result"]["isError"])

    def test_duplicate_registration_is_rejected(self):
        for server, is_async in self.variants()[2:]:
            with self.assertRaises(BaboonMcpWiringException):
                server.register(AsyncServer() if is_async else SyncServer())
