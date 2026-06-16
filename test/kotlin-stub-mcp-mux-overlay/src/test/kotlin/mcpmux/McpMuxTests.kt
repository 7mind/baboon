// T110 — Kotlin MCP muxer round-trip overlay test.
//
// Exercises `AbstractMcpMuxer<Ctx>` by composing two FRESHLY GENERATED
// `<Service>McpServer` instances produced from the T102 `mcp-mux-stub-ok/`
// model (UserService + OrderService). Composition is done strictly through
// the public T114 routable surface:
//   - the muxer ctor takes `IBaboonRoutableMcpServer<Ctx>` members,
//   - the test NEVER subclasses a `<Service>McpServer`,
//   - the test NEVER calls a member server's own `handle()`.
//
// Generated code lands in `src/main/kotlin/generated-main/` (the isolated
// dir set by the `test-gen-kt-mcp-mux` mdl action). No committed generated
// fixtures.
//
// Kotlin MCP is SYNC ONLY — no async muxer lane (R112 criticism 3).
// The `--service-result-no-errors=false` / Either mode matches the existing
// kt MCP lane flags.
//
// Four asserted muxer behaviours (T110 acceptance):
//   1. tools/list  -> UNION of both services' tools in registration-then-
//                    declaration order;
//   2. tools/call  -> routes the flat tool name to the correct owning
//                    service — proven for a tool of EACH service:
//                    UserService_getUser (Channel-A Right, isError=false),
//                    OrderService_cancelOrder (Channel-A Right, isError=false);
//   3. register a server with a colliding tool name -> throws
//      BaboonMcpWiringException{DuplicateTool};
//   4. unknown tool name -> -32602 "unknown tool" response (NoMatchingTool).
//
// Assertion discipline (T7 §5.1):
//   All assertions use JUnit Jupiter assert* which throw unconditionally on
//   failure. No conditional guards around assertions.
package mcpmux

import baboon.runtime.shared.AbstractMcpMuxer
import baboon.runtime.shared.BaboonCodecContext
import baboon.runtime.shared.BaboonMcpWiringError
import baboon.runtime.shared.BaboonMcpWiringException
import baboon.runtime.shared.BaboonMethodId
import baboon.runtime.shared.BaboonWiringError
import baboon.runtime.shared.IBaboonRoutableMcpServer
import baboon.runtime.shared.JsonRpcRequest
import baboon.runtime.shared.JsonRpcResponse
import baboon.runtime.shared.McpServerInfo
import baboon.runtime.shared.McpSession
import kotlinx.serialization.json.Json
import kotlinx.serialization.json.JsonPrimitive
import kotlinx.serialization.json.boolean
import kotlinx.serialization.json.jsonArray
import kotlinx.serialization.json.jsonObject
import kotlinx.serialization.json.jsonPrimitive
import mcp.mux.stub.BaboonServiceRtDefault
import mcp.mux.stub.IBaboonServiceRt
import mcp.mux.stub.OrderService
import mcp.mux.stub.OrderServiceMcpServer
import mcp.mux.stub.OrderServiceWiring
import mcp.mux.stub.OrderStatus
import mcp.mux.stub.OrderSummary
import mcp.mux.stub.UserProfile
import mcp.mux.stub.UserService
import mcp.mux.stub.UserServiceMcpServer
import mcp.mux.stub.UserServiceWiring
import mcp.mux.stub.UserStatus
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertFalse
import org.junit.jupiter.api.Assertions.assertNotNull
import org.junit.jupiter.api.Assertions.assertNull
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.Test

// ---------------------------------------------------------------------------
// Stub service implementations: every method returns minimal valid data.
// ---------------------------------------------------------------------------

private class StubUserService : UserService {
    override fun createUser(arg: mcp.mux.stub.userservice.createuser.In): mcp.mux.stub.userservice.createuser.Out =
        mcp.mux.stub.userservice.createuser.Out(profile = UserProfile(userId = "u1", email = "a@b.c", status = UserStatus.Active))

    override fun getUser(arg: mcp.mux.stub.userservice.getuser.In): mcp.mux.stub.userservice.getuser.Out =
        mcp.mux.stub.userservice.getuser.Out(profile = UserProfile(userId = "u1", email = "a@b.c", status = UserStatus.Active))
}

private class StubOrderService : OrderService {
    override fun placeOrder(arg: mcp.mux.stub.orderservice.placeorder.In): mcp.mux.stub.orderservice.placeorder.Out =
        mcp.mux.stub.orderservice.placeorder.Out(summary = OrderSummary(orderId = "o1", status = OrderStatus.Confirmed, total = 10.0))

    override fun cancelOrder(arg: mcp.mux.stub.orderservice.cancelorder.In): mcp.mux.stub.orderservice.cancelorder.Out =
        mcp.mux.stub.orderservice.cancelorder.Out(ok = true)
}

// ---------------------------------------------------------------------------
// Test class
// ---------------------------------------------------------------------------

class McpMuxTests {
    private val codecCtx: BaboonCodecContext = BaboonCodecContext.Default
    private val rt: IBaboonServiceRt = BaboonServiceRtDefault

    private val stubUser = StubUserService()
    private val stubOrder = StubOrderService()

    // The union of both services' tools in registration-then-declaration order:
    // UserService registered first (createUser, getUser declared in that order),
    // OrderService second (placeOrder, cancelOrder).
    private val expectedUnion = listOf(
        "UserService_createUser",
        "UserService_getUser",
        "OrderService_placeOrder",
        "OrderService_cancelOrder",
    )

    private val mergedInfo = McpServerInfo(name = "MergedEndpoint", version = "1.0.0")

    private fun makeUserServer(): IBaboonRoutableMcpServer<Unit?> =
        UserServiceMcpServer { method, data, _, cc -> UserServiceWiring.invokeJson(method, data, stubUser, rt, cc) }

    private fun makeOrderServer(): IBaboonRoutableMcpServer<Unit?> =
        OrderServiceMcpServer { method, data, _, cc -> OrderServiceWiring.invokeJson(method, data, stubOrder, rt, cc) }

    private fun makeMuxer(): AbstractMcpMuxer<Unit?> =
        AbstractMcpMuxer(mergedInfo, makeUserServer(), makeOrderServer())

    // Helper: send one JSON-RPC request and assert a response was returned.
    private fun send(mux: AbstractMcpMuxer<Unit?>, session: McpSession, req: JsonRpcRequest): JsonRpcResponse {
        val resp = mux.handle(req, session, null, codecCtx)
        checkNotNull(resp) {
            "Expected a response for \"${req.method}\" but got null (notification not expected here)"
        }
        return resp
    }

    private fun makeInitReq(id: Long): JsonRpcRequest =
        JsonRpcRequest(
            id = JsonPrimitive(id),
            method = "initialize",
            params = Json.parseToJsonElement("""{"protocolVersion":"2025-06-18","capabilities":{},"clientInfo":{"name":"test-client","version":"0.0.1"}}"""),
        )

    private fun initSession(mux: AbstractMcpMuxer<Unit?>, session: McpSession) {
        mux.handle(makeInitReq(0L), session, null, codecCtx)
        mux.handle(JsonRpcRequest(null, "notifications/initialized", null), session, null, codecCtx)
    }

    private fun initedSession(mux: AbstractMcpMuxer<Unit?>): McpSession {
        val session = McpSession()
        initSession(mux, session)
        return session
    }

    private fun callTool(mux: AbstractMcpMuxer<Unit?>, session: McpSession, toolName: String, argsJson: String): JsonRpcResponse =
        send(mux, session, JsonRpcRequest(
            id = JsonPrimitive(99L),
            method = "tools/call",
            params = Json.parseToJsonElement("""{"name":"$toolName","arguments":$argsJson}"""),
        ))

    // ---------------------------------------------------------------------------
    // §1 — tools/list returns UNION in registration-then-declaration order
    // ---------------------------------------------------------------------------

    @Test
    fun sec1_toolsList_unionInRegistrationThenDeclarationOrder() {
        val mux = makeMuxer()
        val session = initedSession(mux)

        val resp = send(mux, session, JsonRpcRequest(
            id = JsonPrimitive(1L),
            method = "tools/list",
            params = null,
        ))

        assertNull(resp.error, "tools/list must not produce an error")
        val tools = resp.result!!.jsonObject["tools"]!!.jsonArray.map { it.jsonObject }
        assertEquals(4, tools.size, "MUST be exactly 4 tools from both services")

        val names = tools.map { it["name"]!!.jsonPrimitive.content }
        assertEquals(expectedUnion, names, "Tool order MUST be registration-then-declaration order")
    }

    @Test
    fun sec1_toolsList_notInterleaved_negativeControl() {
        // Negative control: the WRONG interleaved order must not match.
        val mux = makeMuxer()
        val session = initedSession(mux)

        val resp = send(mux, session, JsonRpcRequest(
            id = JsonPrimitive(1L),
            method = "tools/list",
            params = null,
        ))

        val names = resp.result!!.jsonObject["tools"]!!.jsonArray.map { it.jsonObject["name"]!!.jsonPrimitive.content }
        val wrongOrder = listOf(
            "UserService_createUser",
            "OrderService_placeOrder",
            "UserService_getUser",
            "OrderService_cancelOrder",
        )
        assertFalse(names == wrongOrder, "NEGATIVE CONTROL: interleaved order must NOT match")
    }

    // ---------------------------------------------------------------------------
    // §2 — tools/call routes to the correct owning service
    // ---------------------------------------------------------------------------

    @Test
    fun sec2_routeUserServiceGetUser_channelA_isErrorFalse() {
        val mux = makeMuxer()
        val session = initedSession(mux)

        val resp = callTool(mux, session, "UserService_getUser", """{"userId":"u1"}""")

        assertNull(resp.error, "Unexpected Channel-A error for UserService_getUser")
        val result = resp.result!!.jsonObject
        val isError = result["isError"]
        assertTrue(isError == null || !isError.jsonPrimitive.boolean, "isError must be false for Channel-A")

        val content = result["content"]!!.jsonArray
        assertEquals(1, content.size, "content must have exactly one element")
        assertEquals("text", content[0].jsonObject["type"]!!.jsonPrimitive.content)

        // The JSON payload proves UserService handled it (profile field present).
        val text = content[0].jsonObject["text"]!!.jsonPrimitive.content
        val payload = Json.parseToJsonElement(text).jsonObject
        assertNotNull(payload["profile"], "result should contain a 'profile' field (UserService result)")
    }

    @Test
    fun sec2_routeOrderServiceCancelOrder_channelA_isErrorFalse() {
        val mux = makeMuxer()
        val session = initedSession(mux)

        val resp = callTool(mux, session, "OrderService_cancelOrder", """{"orderId":"o1","reason":null}""")

        assertNull(resp.error, "Unexpected Channel-A error for OrderService_cancelOrder")
        val result = resp.result!!.jsonObject
        val isError = result["isError"]
        assertTrue(isError == null || !isError.jsonPrimitive.boolean, "isError must be false for Channel-A")

        val content = result["content"]!!.jsonArray
        assertEquals(1, content.size, "content must have exactly one element")
        val text = content[0].jsonObject["text"]!!.jsonPrimitive.content
        val payload = Json.parseToJsonElement(text).jsonObject
        assertEquals(true, payload["ok"]!!.jsonPrimitive.boolean, "ok must be true (OrderService result)")
    }

    @Test
    fun sec2_routeOrderServicePlaceOrder_channelB_isErrorTrue_whenBadArgs() {
        // null for items (a required lst[OrderItem]) causes a decoder failure
        // -> BaboonWiringError.DecoderFailed -> Left -> Channel-B (isError=true).
        val mux = makeMuxer()
        val session = initedSession(mux)

        val resp = callTool(mux, session, "OrderService_placeOrder", """{"userId":"u1","items":null}""")

        // Channel B: MUST be a result (not error) with isError=true.
        assertNotNull(resp.result, "Channel-B: result must be present")
        assertNull(resp.error, "Channel-B: must not be a JSON-RPC error")
        assertEquals(true, resp.result!!.jsonObject["isError"]!!.jsonPrimitive.boolean,
            "isError MUST be true for Channel-B decode failure")
    }

    // ---------------------------------------------------------------------------
    // §3 — DuplicateTool on collision
    // ---------------------------------------------------------------------------

    @Test
    fun sec3_duplicateTool_thrownOnCollidingToolViaCtor() {
        // Second UserServiceMcpServer re-declares UserService_createUser / UserService_getUser.
        var caught: BaboonMcpWiringException? = null
        try {
            AbstractMcpMuxer<Unit?>(mergedInfo, makeUserServer(), makeUserServer())
        } catch (e: BaboonMcpWiringException) {
            caught = e
        }
        assertNotNull(caught, "BaboonMcpWiringException must be thrown for duplicate tool registration")
        val err = caught!!.error
        assertTrue(err is BaboonMcpWiringError.DuplicateTool,
            "Expected DuplicateTool error, got $err")
        assertEquals("UserService_createUser", (err as BaboonMcpWiringError.DuplicateTool).toolName)
    }

    @Test
    fun sec3_duplicateTool_thrownOnCollidingToolViaRegister() {
        val mux = AbstractMcpMuxer<Unit?>(mergedInfo, makeUserServer())
        var caught: BaboonMcpWiringException? = null
        try {
            mux.register(makeUserServer())
        } catch (e: BaboonMcpWiringException) {
            caught = e
        }
        assertNotNull(caught, "BaboonMcpWiringException must be thrown when calling register() with duplicate tool")
        assertTrue(caught!!.error is BaboonMcpWiringError.DuplicateTool,
            "Expected DuplicateTool, got ${caught.error}")
    }

    // ---------------------------------------------------------------------------
    // §4 — NoMatchingTool on unknown tool name
    // ---------------------------------------------------------------------------

    @Test
    fun sec4_noMatchingTool_channelAError_code32602() {
        // NEGATIVE CONTROL: if the muxer returned success for an unknown tool,
        // assertNotNull(resp.error) would fail, proving the check is live.
        val mux = makeMuxer()
        val session = initedSession(mux)

        val resp = callTool(mux, session, "UserService_nope", "{}")

        // MUST be a Channel-A error (-32602), not a result.
        assertNotNull(resp.error, "Unknown tool must produce a Channel-A error")
        assertNull(resp.result, "No result expected for unknown tool")
        assertEquals(-32602, resp.error!!.code, "Unknown tool error code MUST be -32602")
        assertTrue(resp.error!!.message.isNotEmpty(), "error.message must be non-empty")
    }
}
