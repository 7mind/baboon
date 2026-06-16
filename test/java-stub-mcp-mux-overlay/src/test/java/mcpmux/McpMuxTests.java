// T111 — Java MCP muxer round-trip overlay test (sync).
//
// Exercises `AbstractMcpMuxer<Ctx>` by composing two FRESHLY GENERATED
// `<Service>McpServer` instances produced from the T102 `mcp-mux-stub-ok/`
// model (UserService + OrderService). Composition is done strictly through
// the public T114 routable surface:
//   - the muxer ctor takes IBaboonRoutableMcpServer<Ctx> members,
//   - the test NEVER subclasses a <Service>McpServer,
//   - the test NEVER calls a member server's own handle().
//
// Generated code lands in `src/main/java/generated-main/` (the isolated
// dir set by the `test-gen-jv-mcp-mux` mdl action). No committed generated fixtures.
//
// Four asserted muxer behaviours (T111 acceptance):
//   1. tools/list  -> UNION of both services' tools in registration-then-
//                     declaration order;
//   2. tools/call  -> routes the flat tool name to the correct owning
//                     service — proven for a tool of EACH service:
//                     UserService_getUser (Channel-A Right, isError=false),
//                     OrderService_cancelOrder (Channel-A Right, isError=false);
//   3. register a server with a colliding tool name -> throws
//      BaboonMcpWiringException(DuplicateTool);
//   4. unknown tool name -> -32602 "unknown tool" response (NoMatchingTool).
//
// Assertion discipline: all assertions use JUnit Jupiter assertEquals/
// assertTrue/assertNotNull which throw unconditionally on failure.
// No debug-only assertions are used.
//
// SYNC ONLY — Java MCP has no async variant.
package mcpmux;

import baboon.runtime.shared.AbstractMcpMuxer;
import baboon.runtime.shared.BaboonCodecContext;
import baboon.runtime.shared.BaboonEither;
import baboon.runtime.shared.BaboonMcpWiringError;
import baboon.runtime.shared.BaboonMcpWiringException;
import baboon.runtime.shared.BaboonMethodId;
import baboon.runtime.shared.BaboonWiringError;
import baboon.runtime.shared.IBaboonRoutableMcpServer;
import baboon.runtime.shared.JsonRpcRequest;
import baboon.runtime.shared.JsonRpcResponse;
import baboon.runtime.shared.McpServerInfo;
import baboon.runtime.shared.McpSession;
import baboon.runtime.shared.McpToolEntry;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import mcp.mux.stub.IBaboonServiceRt;
import mcp.mux.stub.OrderService;
import mcp.mux.stub.OrderServiceMcpServer;
import mcp.mux.stub.OrderServiceWiring;
import mcp.mux.stub.OrderSummary;
import mcp.mux.stub.OrderStatus;
import mcp.mux.stub.UserProfile;
import mcp.mux.stub.UserService;
import mcp.mux.stub.UserServiceMcpServer;
import mcp.mux.stub.UserServiceWiring;
import mcp.mux.stub.UserStatus;

import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;

import java.util.List;

public class McpMuxTests {

    private static final ObjectMapper MAPPER = new ObjectMapper();

    // ---------------------------------------------------------------------------
    // IBaboonServiceRt implementation for BaboonEither (errors mode).
    // ---------------------------------------------------------------------------
    private static final IBaboonServiceRt DEFAULT_RT = new IBaboonServiceRt() {
        @Override
        public <L, R> BaboonEither<L, R> pure(R value) {
            return BaboonEither.right(value);
        }
        @Override
        public <L, R> BaboonEither<L, R> fail(L error) {
            return BaboonEither.left(error);
        }
        @Override
        public <A, B, C> BaboonEither<C, B> leftMap(BaboonEither<A, B> value, java.util.function.Function<A, C> f) {
            if (value instanceof BaboonEither.Left<A, B> l) {
                return BaboonEither.left(f.apply(l.value()));
            } else if (value instanceof BaboonEither.Right<A, B> r) {
                return BaboonEither.right(r.value());
            } else {
                throw new IllegalStateException("BaboonEither unknown variant: " + value);
            }
        }
        @Override
        public <A, B, C> BaboonEither<A, C> flatMap(BaboonEither<A, B> value, java.util.function.Function<B, BaboonEither<A, C>> f) {
            if (value instanceof BaboonEither.Right<A, B> r) {
                return f.apply(r.value());
            } else if (value instanceof BaboonEither.Left<A, B> l) {
                return BaboonEither.left(l.value());
            } else {
                throw new IllegalStateException("BaboonEither unknown variant: " + value);
            }
        }
    };

    // ---------------------------------------------------------------------------
    // Stub service implementations: every method returns minimal valid data.
    // ---------------------------------------------------------------------------
    private static final class StubUserService implements UserService {
        @Override
        public mcp.mux.stub.userservice.createuser.Out createUser(mcp.mux.stub.userservice.createuser.In arg) {
            return new mcp.mux.stub.userservice.createuser.Out(
                new UserProfile("u1", "a@b.c", UserStatus.Active)
            );
        }
        @Override
        public mcp.mux.stub.userservice.getuser.Out getUser(mcp.mux.stub.userservice.getuser.In arg) {
            return new mcp.mux.stub.userservice.getuser.Out(
                java.util.Optional.of(new UserProfile("u1", "a@b.c", UserStatus.Active))
            );
        }
    }

    private static final class StubOrderService implements OrderService {
        @Override
        public mcp.mux.stub.orderservice.placeorder.Out placeOrder(mcp.mux.stub.orderservice.placeorder.In arg) {
            return new mcp.mux.stub.orderservice.placeorder.Out(
                new OrderSummary("o1", OrderStatus.Confirmed, 10.0)
            );
        }
        @Override
        public mcp.mux.stub.orderservice.cancelorder.Out cancelOrder(mcp.mux.stub.orderservice.cancelorder.In arg) {
            return new mcp.mux.stub.orderservice.cancelorder.Out(true);
        }
    }

    // ---------------------------------------------------------------------------
    // Test fixtures
    // ---------------------------------------------------------------------------
    private final BaboonCodecContext codecCtx = BaboonCodecContext.Default;
    private final StubUserService  stubUser  = new StubUserService();
    private final StubOrderService stubOrder = new StubOrderService();

    private final McpServerInfo mergedInfo = new McpServerInfo("MergedEndpoint", "1.0.0");

    // The union of both services' tools in registration-then-declaration order:
    // UserService registered first (createUser, getUser declared in that order),
    // OrderService second (placeOrder, cancelOrder).
    private static final List<String> EXPECTED_UNION = List.of(
        "UserService_createUser",
        "UserService_getUser",
        "OrderService_placeOrder",
        "OrderService_cancelOrder"
    );

    private IBaboonRoutableMcpServer<Void> makeUserServer() {
        return new UserServiceMcpServer<>(
            (method, data, ctx, cc) -> UserServiceWiring.invokeJson(method, data, stubUser, DEFAULT_RT, cc)
        );
    }

    private IBaboonRoutableMcpServer<Void> makeOrderServer() {
        return new OrderServiceMcpServer<>(
            (method, data, ctx, cc) -> OrderServiceWiring.invokeJson(method, data, stubOrder, DEFAULT_RT, cc)
        );
    }

    private AbstractMcpMuxer<Void> makeMuxer() {
        return new AbstractMcpMuxer<>(mergedInfo, makeUserServer(), makeOrderServer());
    }

    private void initSession(AbstractMcpMuxer<Void> mux, McpSession session) throws Exception {
        mux.handle(
            new JsonRpcRequest(
                MAPPER.readTree("1"),
                "initialize",
                MAPPER.readTree("{\"protocolVersion\":\"2025-06-18\",\"capabilities\":{},\"clientInfo\":{\"name\":\"test-client\",\"version\":\"0.0.1\"}}")
            ),
            session, null, codecCtx
        );
        mux.handle(
            new JsonRpcRequest(null, "notifications/initialized", null),
            session, null, codecCtx
        );
    }

    private JsonRpcResponse send(AbstractMcpMuxer<Void> mux, McpSession session, JsonRpcRequest req) {
        JsonRpcResponse resp = mux.handle(req, session, null, codecCtx);
        assertNotNull(resp, "Expected a response for \"" + req.method + "\" but got null");
        return resp;
    }

    private JsonRpcResponse callTool(AbstractMcpMuxer<Void> mux, McpSession session, String toolName, String argsJson)
            throws Exception {
        return send(mux, session, new JsonRpcRequest(
            MAPPER.readTree("99"),
            "tools/call",
            MAPPER.readTree("{\"name\":\"" + toolName + "\",\"arguments\":" + argsJson + "}")
        ));
    }

    // ---------------------------------------------------------------------------
    // §1 — tools/list returns UNION in registration-then-declaration order
    // ---------------------------------------------------------------------------

    @Test
    public void sec1_toolsList_returnsUnionInRegistrationThenDeclarationOrder() throws Exception {
        var mux     = makeMuxer();
        var session = new McpSession();
        initSession(mux, session);

        var resp = send(mux, session,
            new JsonRpcRequest(MAPPER.readTree("2"), "tools/list", null));

        assertEquals(2, resp.id.intValue(), "id must be 2");
        assertNull(resp.error, "Expected no error for tools/list");
        assertNotNull(resp.result, "result must be present");

        var toolsArray = resp.result.get("tools");
        assertNotNull(toolsArray, "tools array must be present");
        assertEquals(4, toolsArray.size(), "MUST be exactly 4 tools (union of UserService + OrderService)");

        // Exact position assertions (registration-then-declaration order).
        for (int i = 0; i < 4; i++) {
            assertEquals(EXPECTED_UNION.get(i), toolsArray.get(i).get("name").textValue(),
                "Tool at index " + i + " must be " + EXPECTED_UNION.get(i));
        }
    }

    @Test
    public void sec1_toolsList_negativeControl_wrongOrderNotAccepted() throws Exception {
        var mux     = makeMuxer();
        var session = new McpSession();
        initSession(mux, session);

        var resp = send(mux, session,
            new JsonRpcRequest(MAPPER.readTree("2"), "tools/list", null));

        var toolsArray = resp.result.get("tools");
        // This interleaved order must NOT be equal to the actual order.
        List<String> wrongOrder = List.of(
            "UserService_createUser",
            "OrderService_placeOrder",
            "UserService_getUser",
            "OrderService_cancelOrder"
        );
        var actualNames = new java.util.ArrayList<String>();
        toolsArray.forEach(t -> actualNames.add(t.get("name").textValue()));
        assertNotEquals(wrongOrder, actualNames,
            "NEGATIVE CONTROL: interleaved order must NOT match actual registration-then-declaration order");
    }

    // ---------------------------------------------------------------------------
    // §2 — tools/call routes to the correct owning service
    // ---------------------------------------------------------------------------

    @Test
    public void sec2_toolsCall_userServiceGetUser_channelA_right() throws Exception {
        var mux     = makeMuxer();
        var session = new McpSession();
        initSession(mux, session);

        var resp = callTool(mux, session, "UserService_getUser", "{\"userId\":\"u1\"}");

        assertEquals(99, resp.id.intValue());
        assertNull(resp.error, "Unexpected error on UserService_getUser call");

        var result  = resp.result;
        var isError = result.get("isError");
        assertTrue(isError == null || !isError.booleanValue(), "isError must be false or absent for Channel-A");

        var content = result.get("content");
        assertEquals(1, content.size(), "content must have exactly one element");
        assertEquals("text", content.get(0).get("type").textValue());

        // The JSON payload proves UserService handled it (profile present).
        var payload = MAPPER.readTree(content.get(0).get("text").textValue());
        assertNotNull(payload.get("profile"), "result should contain a 'profile' field (UserService result)");
    }

    @Test
    public void sec2_toolsCall_orderServiceCancelOrder_channelA_right() throws Exception {
        var mux     = makeMuxer();
        var session = new McpSession();
        initSession(mux, session);

        var resp = callTool(mux, session, "OrderService_cancelOrder",
            "{\"orderId\":\"o1\",\"reason\":null}");

        assertEquals(99, resp.id.intValue());
        assertNull(resp.error, "Unexpected error on OrderService_cancelOrder call");

        var result  = resp.result;
        var isError = result.get("isError");
        assertTrue(isError == null || !isError.booleanValue(), "isError must be false or absent for Channel-A");

        var content = result.get("content");
        assertEquals(1, content.size());
        var payload = MAPPER.readTree(content.get(0).get("text").textValue());
        // OrderService_cancelOrder returns {ok: true}
        assertTrue(payload.get("ok").booleanValue(), "ok must be true");
    }

    @Test
    public void sec2_toolsCall_channelB_decodeFailure_isErrorTrue() throws Exception {
        // Channel-B trigger: send placeOrder with an OrderItem that has missing required
        // int field `quantity`. The Java decoder tries to read `quantity` (primitive int)
        // from a node where the key is absent — this produces a NullPointerException,
        // which the wiring catch-block wraps as BaboonWiringError.DecoderFailed and
        // OrderServiceWiring.invokeJson returns Left.
        // The muxer produces Channel-B: result with isError=true.
        var mux     = makeMuxer();
        var session = new McpSession();
        initSession(mux, session);

        var resp = callTool(mux, session, "OrderService_placeOrder",
            "{\"userId\":\"u1\",\"items\":[{\"productId\":\"p1\",\"unitPrice\":9.99}]}");

        assertEquals(99, resp.id.intValue());
        // Channel B: MUST be a result (not error) with isError=true.
        assertNotNull(resp.result, "Channel-B: result must be present");
        assertNull(resp.error, "Channel-B: must not be a JSON-RPC error");
        assertTrue(resp.result.get("isError").booleanValue(),
            "isError MUST be true for Channel-B decode failure");
    }

    // ---------------------------------------------------------------------------
    // §3 — DuplicateTool on collision
    // ---------------------------------------------------------------------------

    @Test
    public void sec3_duplicateTool_viaCtor_throwsBaboonMcpWiringException() {
        // Second UserServiceMcpServer re-declares UserService_createUser / UserService_getUser.
        BaboonMcpWiringException thrown = null;
        try {
            new AbstractMcpMuxer<>(mergedInfo, makeUserServer(), makeUserServer());
        } catch (BaboonMcpWiringException e) {
            thrown = e;
        }
        assertNotNull(thrown, "BaboonMcpWiringException must be thrown for duplicate tool registration");
        assertInstanceOf(BaboonMcpWiringError.DuplicateTool.class, thrown.getError(),
            "Error must be DuplicateTool");
        assertEquals("UserService_createUser",
            ((BaboonMcpWiringError.DuplicateTool) thrown.getError()).toolName(),
            "First duplicate tool is UserService_createUser");
    }

    @Test
    public void sec3_duplicateTool_viaRegister_throwsBaboonMcpWiringException() {
        var mux = new AbstractMcpMuxer<>(mergedInfo, makeUserServer());
        BaboonMcpWiringException thrown = null;
        try {
            mux.register(makeUserServer());
        } catch (BaboonMcpWiringException e) {
            thrown = e;
        }
        assertNotNull(thrown, "BaboonMcpWiringException must be thrown when calling register() with duplicate tool");
        assertInstanceOf(BaboonMcpWiringError.DuplicateTool.class, thrown.getError(),
            "Error must be DuplicateTool");
    }

    // ---------------------------------------------------------------------------
    // §4 — NoMatchingTool on unknown tool name
    // ---------------------------------------------------------------------------

    @Test
    public void sec4_noMatchingTool_unknown_channelA_error_code32602() throws Exception {
        // NEGATIVE CONTROL: if the muxer returned success for an unknown tool, the
        // assertNotNull(resp.error) below would fail.
        var mux     = makeMuxer();
        var session = new McpSession();
        initSession(mux, session);

        var resp = callTool(mux, session, "UserService_nope", "{}");

        assertEquals(99, resp.id.intValue());
        // MUST be a Channel-A error, not a result.
        assertNotNull(resp.error, "Unknown tool must produce a Channel-A error");
        assertNull(resp.result, "No result expected for unknown tool");
        // §4: code MUST be -32602 (InvalidParams — unknown tool)
        assertEquals(-32602, resp.error.code, "Unknown tool error code MUST be -32602");
        assertFalse(resp.error.message.isEmpty(), "error.message must be non-empty");
    }
}
