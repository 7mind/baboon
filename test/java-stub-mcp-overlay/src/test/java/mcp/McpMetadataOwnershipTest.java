package mcp;

import baboon.runtime.shared.*;
import com.fasterxml.jackson.databind.node.JsonNodeFactory;
import com.fasterxml.jackson.databind.node.ObjectNode;
import java.util.List;
import java.util.Map;
import mcp.stub.McpToolsMcpServer;
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;

class McpMetadataOwnershipTest {
    private final JsonNodeFactory json = JsonNodeFactory.instance;

    private JsonRpcRequest call(String name) {
        return new JsonRpcRequest(json.numberNode(7), "tools/call", json.objectNode().put("name", name));
    }

    @Test
    void generatedSchemasRemainIsolatedAcrossAccessorsResponsesAndServers() {
        var server = new McpToolsMcpServer<String>((method, data, ctx, codecCtx) -> BaboonEither.right(ctx + ":" + method.methodName()));
        var other = new McpToolsMcpServer<String>((method, data, ctx, codecCtx) -> BaboonEither.right(data));
        var first = server.tools();
        var second = server.tools();
        assertEquals(7, first.size());
        assertNotSame(first.get(0).inputSchema, second.get(0).inputSchema);
        assertThrows(UnsupportedOperationException.class, () -> first.add(first.get(0)));
        ((ObjectNode) first.get(0).inputSchema).put("mutated", true);
        ((ObjectNode) first.get(0).inputSchema.get("properties")).put("nestedMutation", true);
        assertFalse(second.get(0).inputSchema.has("mutated"));
        assertFalse(second.get(0).inputSchema.get("properties").has("nestedMutation"));
        assertFalse(server.tools().get(0).inputSchema.has("mutated"));
        assertFalse(other.tools().get(0).inputSchema.has("mutated"));
        var session = new McpSession();
        session.initialized = true;
        var request = new JsonRpcRequest(json.numberNode(8), "tools/list", null);
        var response = server.handle(request, session, "ctx", BaboonCodecContext.Default);
        ((ObjectNode) response.result.get("tools").get(0).get("inputSchema")).put("responseMutation", true);
        assertFalse(server.handle(request, session, "ctx", BaboonCodecContext.Default).result.get("tools").get(0).get("inputSchema").has("responseMutation"));
        var invoked = server.handle(call("McpTools_ping"), session, "ctx", BaboonCodecContext.Default);
        assertNull(invoked.error);
        assertEquals("ctx:ping", invoked.result.get("content").get(0).get("text").textValue());
        assertNotNull(server.handle(call("McpTools_ping"), new McpSession(), "ctx", BaboonCodecContext.Default).error);
    }

    @Test
    void generatedLookupIsOneImmutableSchemaFreeIndex() throws Exception {
        var server = new McpToolsMcpServer<String>((method, data, ctx, codecCtx) -> BaboonEither.right(data));
        var accessor = server.getClass().getDeclaredMethod("toolMethods");
        accessor.setAccessible(true);
        Map<?, ?> index = (Map<?, ?>) accessor.invoke(server);
        assertSame(index, accessor.invoke(server));
        assertEquals(7, index.size());
        assertTrue(index.values().stream().allMatch(value -> value instanceof BaboonMethodId));
        assertThrows(UnsupportedOperationException.class, index::clear);
    }

    @Test
    void extensibleRuntimeRetainsDynamicToolsDuplicateLastWinsAndNullMethods() {
        class DynamicServer extends AbstractBaboonMcpServer<String> {
            int reads;
            List<McpToolEntry> current = List.of(new McpToolEntry("first", new BaboonMethodId("service", "a"), json.objectNode()));
            @Override public McpServerInfo serverInfo() { return new McpServerInfo("dynamic", "1"); }
            @Override public List<McpToolEntry> tools() { reads++; return current; }
            @Override protected BaboonEither<BaboonWiringError, String> invokeJson(BaboonMethodId method, String data, String ctx, BaboonCodecContext codecCtx) {
                return BaboonEither.right(method == null ? "null-method" : method.methodName());
            }
        }
        var server = new DynamicServer();
        var session = new McpSession();
        session.initialized = true;
        assertNull(server.handle(call("first"), session, "ctx", BaboonCodecContext.Default).error);
        assertEquals(1, server.reads);
        server.current = List.of(
            new McpToolEntry("second", new BaboonMethodId("service", "a"), json.objectNode()),
            new McpToolEntry("second", new BaboonMethodId("service", "b"), json.objectNode())
        );
        var response = server.handle(call("second"), session, "ctx", BaboonCodecContext.Default);
        assertEquals("b", response.result.get("content").get(0).get("text").textValue());
        assertNotNull(server.handle(call("first"), session, "ctx", BaboonCodecContext.Default).error);
        server.current = List.of(new McpToolEntry("null", null, json.objectNode()));
        assertEquals("null-method", server.handle(call("null"), session, "ctx", BaboonCodecContext.Default).result.get("content").get(0).get("text").textValue());
    }
}
