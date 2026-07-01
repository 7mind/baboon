// T178 / D40 — Java zero-service MCP overlay test (RED baseline).
//
// Purpose: prove the ZERO-SERVICE MCP runtime contract. The zero-service model
// (mcp-stub-zero-services-ok) declares @root types but NO RPC block, so
// servicesOf(domain) is empty and NO per-service `<Service>McpServer` class is
// generated. With zero services the ONLY source of the MCP runtime types
// (AbstractMcpMuxer / IBaboonRoutableMcpServer / McpServerInfo / McpSession /
// JsonRpcRequest / JsonRpcResponse / JsonRpcError / McpProtocol) is the STATIC
// runtime — the 14 `baboon.runtime.shared` MCP `.java` files.
//
// RED (pre-fix): the current generator emits NONE of those MCP runtime files for
// a zero-service model, so the imports below are unresolved and THIS FILE FAILS
// TO COMPILE (`package baboon.runtime.shared does not exist` / `cannot find
// symbol AbstractMcpMuxer`). That compile failure IS the D40 reproduction.
//
// GREEN (post-fix, later task): once the generator emits the static MCP runtime
// even when servicesOf(domain) is empty, this file compiles and the runtime
// assertions below pass — an empty muxer lists zero tools and rejects any
// tools/call with JSON-RPC -32602.
//
// Assertion discipline: JUnit 5 assertions / explicit throw — unconditional.

package mcpzero;

import baboon.runtime.shared.AbstractMcpMuxer;
import baboon.runtime.shared.BaboonCodecContext;
import baboon.runtime.shared.JsonRpcRequest;
import baboon.runtime.shared.JsonRpcResponse;
import baboon.runtime.shared.McpServerInfo;
import baboon.runtime.shared.McpSession;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class McpZeroTests {
    private static final ObjectMapper MAPPER = new ObjectMapper();
    private final BaboonCodecContext codecCtx = BaboonCodecContext.Default;

    private static JsonNode json(String s) {
        try {
            return MAPPER.readTree(s);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    // COMPILE-TIME contract: constructing AbstractMcpMuxer<Ctx> with ZERO
    // registered servers requires the static runtime to exist. With zero services
    // there is no generated <Service>McpServer to import — these types resolve
    // ONLY from the static baboon.runtime.shared MCP files.
    private AbstractMcpMuxer<Object> makeEmptyMuxer() {
        return new AbstractMcpMuxer<>(new McpServerInfo("ZeroEndpoint", "1.0.0"));
    }

    private McpSession initedSession(AbstractMcpMuxer<Object> mux) {
        McpSession session = new McpSession();
        mux.handle(new JsonRpcRequest(
                json("0"), "initialize",
                json("{\"protocolVersion\":\"2025-06-18\",\"capabilities\":{},\"clientInfo\":{\"name\":\"t\",\"version\":\"0\"}}")
        ), session, null, codecCtx);
        mux.handle(new JsonRpcRequest(null, "notifications/initialized", null),
                session, null, codecCtx);
        return session;
    }

    // RUNTIME contract (a): tools/list on an empty muxer returns an empty array.
    @Test
    public void zeroServices_toolsList_isEmpty() {
        AbstractMcpMuxer<Object> mux = makeEmptyMuxer();
        McpSession session = initedSession(mux);

        JsonRpcResponse resp = mux.handle(
                new JsonRpcRequest(json("1"), "tools/list", null),
                session, null, codecCtx);

        assertNotNull(resp, "tools/list must return a response");
        assertNull(resp.error, "tools/list must not return an error");

        JsonNode tools = resp.result.get("tools");
        assertNotNull(tools, "tools/list result must carry a tools array");
        assertTrue(tools.isArray(), "tools must be a JSON array");
        assertEquals(0, tools.size(), "empty muxer MUST list zero tools");
    }

    // RUNTIME contract (b): an unknown tool call yields JSON-RPC -32602.
    @Test
    public void zeroServices_unknownToolCall_code32602() {
        AbstractMcpMuxer<Object> mux = makeEmptyMuxer();
        McpSession session = initedSession(mux);

        JsonRpcResponse resp = mux.handle(
                new JsonRpcRequest(json("2"), "tools/call",
                        json("{\"name\":\"anything_at_all\",\"arguments\":{}}")),
                session, null, codecCtx);

        assertNotNull(resp, "tools/call must return a response");
        assertNotNull(resp.error, "unknown tool on empty muxer MUST produce a Channel-A error");
        assertNull(resp.result, "no result expected for unknown tool");
        assertEquals(-32602, resp.error.code, "unknown-tool error code MUST be -32602 (InvalidParams)");
        assertTrue(resp.error.message != null && !resp.error.message.isEmpty(),
                "error.message must be non-empty");
    }
}
