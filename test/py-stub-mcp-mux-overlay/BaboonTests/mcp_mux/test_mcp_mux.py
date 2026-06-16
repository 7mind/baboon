# T108 -- Python MCP muxer round-trip overlay test (sync).
#
# Exercises `McpMuxer` by composing two FRESHLY GENERATED
# <Service>McpServer instances produced from the T102 `mcp-mux-stub-ok/`
# model (UserService + OrderService). Composition is done strictly through
# the public T114 routable surface:
#   - the muxer ctor takes objects with .server_info / .tools / .route_tool_call,
#   - the test NEVER subclasses a <Service>McpServer,
#   - the test NEVER calls a member server's own handle().
#
# Generated code lands in `BaboonDefinitions/Generated/` (the isolated dir
# set by the `test-gen-py-mcp-mux` mdl action). No committed generated fixtures.
#
# Four asserted muxer behaviours (T108 acceptance):
#   1. tools/list -> UNION of both services' tools in registration-then-
#                   declaration order;
#   2. tools/call -> routes the flat tool name to the correct owning
#                   service -- proven for a tool of EACH service:
#                   UserService_getUser (Channel-A Right, isError:False),
#                   OrderService_cancelOrder (Channel-A Right, isError:False);
#   3. register a server with a colliding tool name -> raises
#      BaboonMcpWiringException with DuplicateTool error;
#   4. unknown tool name -> -32602 "unknown tool" response (NoMatchingTool).
#
# Assertion discipline:
#   All assertions use unconditional unittest.TestCase assertion methods,
#   which raise AssertionError unconditionally. No `if`-guarded checks.
#
# Run from py-stub/:
#   python3 -m unittest BaboonTests.mcp_mux.test_mcp_mux

import json
import unittest

from BaboonDefinitions.Generated.baboon_codecs import BaboonCodecContext
from BaboonDefinitions.Generated.baboon_mcp_runtime import (
    McpSession,
    McpServerInfo,
    McpMuxer,
    BaboonMcpWiringException,
    DuplicateTool,
)
from BaboonDefinitions.Generated.mcp.mux.stub.BaboonServiceRt import BaboonServiceRtDefault
from BaboonDefinitions.Generated.mcp.mux.stub.UserService_Wiring import invoke_json_UserService
from BaboonDefinitions.Generated.mcp.mux.stub.UserServiceMcpServer import UserServiceMcpServer
from BaboonDefinitions.Generated.mcp.mux.stub.OrderService_Wiring import invoke_json_OrderService
from BaboonDefinitions.Generated.mcp.mux.stub.OrderServiceMcpServer import OrderServiceMcpServer

# --- UserService interface and stub ---
# The generated UserService interface lives in the service wiring file.
# We define a minimal concrete implementation here.

# UserProfile fields: userId: str, email: str, status: UserStatus
# UserService_Wiring expects invoke_json_UserService(method, data, impl, rt, codec_ctx)

from BaboonDefinitions.Generated.mcp.mux.stub.UserProfile import UserProfile
from BaboonDefinitions.Generated.mcp.mux.stub.UserStatus import UserStatus
from BaboonDefinitions.Generated.mcp.mux.stub.userservice.createuser.Out import Out as CreateUserOut
from BaboonDefinitions.Generated.mcp.mux.stub.userservice.getuser.Out import Out as GetUserOut
from BaboonDefinitions.Generated.mcp.mux.stub.OrderSummary import OrderSummary
from BaboonDefinitions.Generated.mcp.mux.stub.OrderStatus import OrderStatus
from BaboonDefinitions.Generated.mcp.mux.stub.orderservice.placeorder.Out import Out as PlaceOrderOut
from BaboonDefinitions.Generated.mcp.mux.stub.orderservice.cancelorder.Out import Out as CancelOrderOut

# The union of both services' tools in registration-then-declaration order:
# UserService registered first (createUser, getUser declared in that order),
# OrderService second (placeOrder, cancelOrder).
EXPECTED_UNION = [
    "UserService_createUser",
    "UserService_getUser",
    "OrderService_placeOrder",
    "OrderService_cancelOrder",
]

MERGED_INFO = McpServerInfo(name="MergedEndpoint", version="1.0.0")


# ---------------------------------------------------------------------------
# Stub service implementations
# ---------------------------------------------------------------------------

class _StubUserService:
    def createUser(self, arg):
        return CreateUserOut(profile=UserProfile(userId="u1", email="a@b.c", status=UserStatus.Active))

    def getUser(self, arg):
        return GetUserOut(profile=UserProfile(userId="u1", email="a@b.c", status=UserStatus.Active))


class _StubOrderService:
    def placeOrder(self, arg):
        return PlaceOrderOut(summary=OrderSummary(orderId="o1", status=OrderStatus.Confirmed, total=10.0))

    def cancelOrder(self, arg):
        return CancelOrderOut(ok=True)


# ---------------------------------------------------------------------------
# Delegate factories (errors-mode invokeJson -- the same delegate the
# integrator would bind to the generated McpServer).
# ---------------------------------------------------------------------------

def _make_user_delegate():
    rt = BaboonServiceRtDefault()
    stub = _StubUserService()
    codec_ctx = BaboonCodecContext.Default

    def _delegate(method, data, ctx, cc):
        return invoke_json_UserService(method, data, stub, rt, cc)

    return _delegate


def _make_order_delegate():
    rt = BaboonServiceRtDefault()
    stub = _StubOrderService()
    codec_ctx = BaboonCodecContext.Default

    def _delegate(method, data, ctx, cc):
        return invoke_json_OrderService(method, data, stub, rt, cc)

    return _delegate


# ---------------------------------------------------------------------------
# Server and muxer factories
# ---------------------------------------------------------------------------

def _make_user_server():
    return UserServiceMcpServer(_make_user_delegate())


def _make_order_server():
    return OrderServiceMcpServer(_make_order_delegate())


def _make_muxer():
    return McpMuxer(MERGED_INFO, _make_user_server(), _make_order_server())


# ---------------------------------------------------------------------------
# Test helpers
# ---------------------------------------------------------------------------

def _init_session(mux, session, codec_ctx):
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


def _init_and_list():
    codec_ctx = BaboonCodecContext.Default
    mux = _make_muxer()
    session = McpSession()
    _init_session(mux, session, codec_ctx)
    resp = mux.handle(
        {"jsonrpc": "2.0", "id": 1, "method": "tools/list"},
        session, None, codec_ctx,
    )
    if resp is None:
        raise AssertionError("tools/list must return a response, got None")
    tools = resp["result"]["tools"]
    return tools, resp, mux, session, codec_ctx


def _call(mux, session, codec_ctx, name, args):
    resp = mux.handle(
        {
            "jsonrpc": "2.0",
            "id": 99,
            "method": "tools/call",
            "params": {"name": name, "arguments": args},
        },
        session, None, codec_ctx,
    )
    if resp is None:
        raise AssertionError(f"tools/call must return a response for tool {name!r}, got None")
    return resp


# ---------------------------------------------------------------------------
# Test 1: tools/list returns union in registration-then-declaration order
# ---------------------------------------------------------------------------

class Test1ToolsList(unittest.TestCase):

    def test_union_in_registration_then_declaration_order(self):
        tools, resp, *_ = _init_and_list()

        self.assertNotIn("error", resp, "tools/list must not return an error")
        self.assertEqual(len(tools), 4, "MUST be exactly 4 tools (2 per service)")

        names = [t["name"] for t in tools]
        self.assertEqual(names, EXPECTED_UNION, "Tool names must be in registration-then-declaration order")

    def test_negative_control_union_not_interleaved(self):
        # Proves the ordering assertion is live: a wrong order fails.
        tools, *_ = _init_and_list()
        names = [t["name"] for t in tools]
        self.assertNotEqual(names, [
            "UserService_createUser",
            "OrderService_placeOrder",
            "UserService_getUser",
            "OrderService_cancelOrder",
        ], "Interleaved order must not match")


# ---------------------------------------------------------------------------
# Test 2: tools/call routes to the correct owning service
# ---------------------------------------------------------------------------

class Test2Routing(unittest.TestCase):

    def test_user_service_get_user_channel_a_right(self):
        tools, _, mux, session, codec_ctx = _init_and_list()

        resp = _call(mux, session, codec_ctx, "UserService_getUser", {"userId": "u1"})

        self.assertEqual(resp["id"], 99)
        self.assertNotIn("error", resp, "UserService_getUser must not return a JSON-RPC error")
        result = resp["result"]
        self.assertEqual(result.get("isError"), False, "isError must be False for Channel-A Right")
        content = result["content"]
        self.assertEqual(len(content), 1)
        self.assertEqual(content[0]["type"], "text")
        # The JSON payload proves UserService handled it (profile.userId present).
        payload = json.loads(content[0]["text"])
        self.assertIn("profile", payload, "UserService_getUser result must contain 'profile'")

    def test_order_service_cancel_order_channel_a_right(self):
        tools, _, mux, session, codec_ctx = _init_and_list()

        resp = _call(mux, session, codec_ctx, "OrderService_cancelOrder", {"orderId": "o1", "reason": None})

        self.assertNotIn("error", resp, "OrderService_cancelOrder must not return a JSON-RPC error")
        result = resp["result"]
        self.assertEqual(result.get("isError"), False, "isError must be False for Channel-A Right")
        content = result["content"]
        self.assertEqual(len(content), 1)
        # The JSON payload proves OrderService handled it (ok field present).
        payload = json.loads(content[0]["text"])
        self.assertEqual(payload.get("ok"), True, "cancelOrder result must have ok=True")

    def test_order_service_place_order_decode_failure_channel_b(self):
        # items=None causes a decode failure in the OrderService wiring (items
        # is a required list field). The wiring wraps it as DecoderFailed -> Left
        # -> Channel-B (isError=True).
        tools, _, mux, session, codec_ctx = _init_and_list()

        resp = _call(mux, session, codec_ctx, "OrderService_placeOrder", {"userId": "u1", "items": None})

        self.assertNotIn("error", resp, "Channel-B must be a result, not a JSON-RPC error")
        result = resp["result"]
        self.assertEqual(result.get("isError"), True, "isError must be True for Channel-B decode failure")
        content = result["content"]
        self.assertGreater(len(content), 0, "content must have at least one element")
        self.assertGreater(len(content[0]["text"]), 0, "error text must be non-empty")


# ---------------------------------------------------------------------------
# Test 3: DuplicateTool raised on collision
# ---------------------------------------------------------------------------

class Test3DuplicateTool(unittest.TestCase):

    def test_duplicate_tool_on_ctor_collision(self):
        # Registering UserService twice declares UserService_createUser / _getUser twice.
        raised = None
        try:
            McpMuxer(MERGED_INFO, _make_user_server(), _make_user_server())
        except BaboonMcpWiringException as e:
            raised = e
        self.assertIsNotNone(raised, "Must raise BaboonMcpWiringException for duplicate tool")
        self.assertIsInstance(raised.error, DuplicateTool, "error must be DuplicateTool")
        self.assertEqual(raised.error.tool_name, "UserService_createUser",
                         "First collision must be UserService_createUser")

    def test_duplicate_tool_on_register(self):
        mux = McpMuxer(MERGED_INFO, _make_user_server())
        raised = None
        try:
            mux.register(_make_user_server())
        except BaboonMcpWiringException as e:
            raised = e
        self.assertIsNotNone(raised, "Must raise BaboonMcpWiringException on register collision")
        self.assertIsInstance(raised.error, DuplicateTool, "error must be DuplicateTool")


# ---------------------------------------------------------------------------
# Test 4: NoMatchingTool -> -32602 error response
# ---------------------------------------------------------------------------

class Test4NoMatchingTool(unittest.TestCase):

    def test_unknown_tool_returns_minus32602(self):
        tools, _, mux, session, codec_ctx = _init_and_list()

        resp = _call(mux, session, codec_ctx, "UserService_nope", {})

        # NoMatchingTool surfaces as a JSON-RPC error (NOT a result).
        self.assertIn("error", resp, "Unknown tool must produce a JSON-RPC error")
        self.assertNotIn("result", resp, "No result expected for unknown tool")
        self.assertEqual(resp["error"]["code"], -32602,
                         "Unknown tool error code MUST be -32602 (InvalidParams)")
        self.assertGreater(len(resp["error"]["message"]), 0, "error message must be non-empty")


if __name__ == "__main__":
    unittest.main()
