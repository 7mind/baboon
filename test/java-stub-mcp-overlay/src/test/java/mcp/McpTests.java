// T15 — Java MCP round-trip overlay test.
//
// Drives the generated McpToolsMcpServer<Ctx> through the canonical T7 scenario
// (docs/research/mcp-roundtrip-scenario.md) using an entirely in-process fake
// transport. No stdio or HTTP is involved.
//
// Assertion discipline (T7 §5.1):
//   - All assertions use JUnit Jupiter assertEquals/assertTrue/assertNotNull which
//     throw unconditionally on failure. No debug-only assertions are used.
//
// Jackson JSON Schema validation tier (K1 — T15 tier):
//   Part (a) — well-formedness: each returned inputSchema is re-parsed through
//     Jackson ObjectMapper.readTree() without throwing, proving it is well-formed JSON.
//   Part (b) — structural equality: each returned inputSchema is asserted
//     STRUCTURALLY EQUAL to the corresponding T7 §2.3 reference value (key-by-key
//     recursive comparison via Jackson JsonNode). `required` arrays are compared
//     as SETS per §5.4. `$defs` keys are compared by lookup (key-order-insensitive
//     via Jackson ObjectNode iteration).
//
// Negative controls (T7 §5.2):
//   - §4.1 (unknown tool → -32602): if the server returned success for
//     McpTools_nonexistent the assertions below would fail.
//   - §4.2 (malformed decode → Channel-B isError=true): if isError were false
//     this test would fail.
//   - Schema structural-equality negative control (sec2_k1_negativeControl):
//     a deliberately-wrong reference must make the comparator return false;
//     the test FAILS if the comparator erroneously returns true.
//
// Channel-B trigger (§4.2): send `McpTools_ping` with missing required field
//   `seqno`. The generated `McpTools_ping_In_JsonCodec.decode` accesses a
//   required int field that is absent, causing a NullPointerException caught by
//   the wiring as DecoderFailed.
package mcp;

import baboon.runtime.shared.BaboonCodecContext;
import baboon.runtime.shared.BaboonEither;
import baboon.runtime.shared.BaboonMethodId;
import baboon.runtime.shared.BaboonWiringError;
import baboon.runtime.shared.JsonRpcError;
import baboon.runtime.shared.JsonRpcRequest;
import baboon.runtime.shared.JsonRpcResponse;
import baboon.runtime.shared.McpSession;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import mcp.stub.IBaboonServiceRt;
import mcp.stub.McpTools;
import mcp.stub.McpToolsMcpServer;
import mcp.stub.McpToolsWiring;

import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;

import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

// ---------------------------------------------------------------------------
// T7 §2.3 reference inputSchema values (authoritative: McpInputSchemaEmitter
// golden test McpInputSchemaEmissionTest.scala + mcp-roundtrip-scenario.md).
// These are embedded as Jackson JsonNode constants parsed from JSON literals.
// ---------------------------------------------------------------------------

public class McpTests {

    private static final ObjectMapper MAPPER = new ObjectMapper();

    // ---------------------------------------------------------------------------
    // Parse a JSON string into a Jackson JsonNode at class-load time.
    // Throws RuntimeException on malformed JSON — if these blow up, the
    // embedded reference literals in this test are wrong.
    // ---------------------------------------------------------------------------
    private static JsonNode ref(String json) {
        try {
            return MAPPER.readTree(json);
        } catch (Exception e) {
            throw new RuntimeException("BUG: bad reference JSON literal in McpTests: " + e.getMessage(), e);
        }
    }

    // Tool 1: McpTools_listCollections — list/set/map[str]/map[enum-key]
    private static final JsonNode REF_LIST_COLLECTIONS = ref(
        "{\"$schema\":\"https://json-schema.org/draft/2020-12/schema\"," +
        "\"type\":\"object\"," +
        "\"properties\":{" +
          "\"tags\":{\"type\":\"array\",\"items\":{\"type\":\"string\"}}," +
          "\"uniqueIds\":{\"type\":\"array\",\"items\":{\"type\":\"integer\",\"format\":\"int64\"},\"uniqueItems\":true}," +
          "\"labels\":{\"type\":\"object\",\"additionalProperties\":{\"type\":\"string\"}}," +
          "\"byColor\":{\"type\":\"object\",\"additionalProperties\":{\"type\":\"string\"}," +
            "\"propertyNames\":{\"type\":\"string\",\"enum\":[\"Red\",\"Green\",\"Blue\"]}}" +
        "}," +
        "\"required\":[\"tags\",\"uniqueIds\",\"labels\",\"byColor\"]," +
        "\"$defs\":{\"mcp_stub_Color\":{\"type\":\"string\",\"enum\":[\"Red\",\"Green\",\"Blue\"]}}" +
        "}"
    );

    // Tool 2: McpTools_submitComposite — nested DTO + opt[DTO] + enum + foreign-string
    private static final JsonNode REF_SUBMIT_COMPOSITE = ref(
        "{\"$schema\":\"https://json-schema.org/draft/2020-12/schema\"," +
        "\"type\":\"object\"," +
        "\"properties\":{" +
          "\"nested\":{\"$ref\":\"#/$defs/mcp_stub_Nested\"}," +
          "\"maybePoint\":{\"oneOf\":[{\"$ref\":\"#/$defs/mcp_stub_Point\"},{\"type\":\"null\"}]}," +
          "\"color\":{\"$ref\":\"#/$defs/mcp_stub_Color\"}," +
          "\"fancy\":{\"type\":\"string\"}" +
        "}," +
        "\"required\":[\"nested\",\"color\",\"fancy\"]," +
        "\"$defs\":{" +
          "\"mcp_stub_Color\":{\"type\":\"string\",\"enum\":[\"Red\",\"Green\",\"Blue\"]}," +
          "\"mcp_stub_Nested\":{\"type\":\"object\",\"properties\":{" +
            "\"point\":{\"$ref\":\"#/$defs/mcp_stub_Point\"}," +
            "\"color\":{\"$ref\":\"#/$defs/mcp_stub_Color\"}," +
            "\"label\":{\"oneOf\":[{\"type\":\"string\"},{\"type\":\"null\"}]}" +
          "},\"required\":[\"point\",\"color\"]}," +
          "\"mcp_stub_Point\":{\"type\":\"object\",\"properties\":{" +
            "\"x\":{\"type\":\"integer\",\"format\":\"int32\"}," +
            "\"y\":{\"type\":\"integer\",\"format\":\"int32\"}" +
          "},\"required\":[\"x\",\"y\"]}" +
        "}" +
        "}"
    );

    // Tool 3: McpTools_processShape — ADT oneOf + recursive Tree
    private static final JsonNode REF_PROCESS_SHAPE = ref(
        "{\"$schema\":\"https://json-schema.org/draft/2020-12/schema\"," +
        "\"type\":\"object\"," +
        "\"properties\":{" +
          "\"shape\":{\"$ref\":\"#/$defs/mcp_stub_Shape\"}," +
          "\"tree\":{\"$ref\":\"#/$defs/mcp_stub_Tree\"}" +
        "}," +
        "\"required\":[\"shape\",\"tree\"]," +
        "\"$defs\":{" +
          "\"mcp_stub_Shape\":{\"oneOf\":[{\"$ref\":\"#/$defs/mcp_stub_Shape_Circle\"},{\"$ref\":\"#/$defs/mcp_stub_Shape_Rect\"}]}," +
          "\"mcp_stub_Tree\":{\"type\":\"object\",\"properties\":{" +
            "\"value\":{\"type\":\"integer\",\"format\":\"int32\"}," +
            "\"left\":{\"oneOf\":[{\"$ref\":\"#/$defs/mcp_stub_Tree\"},{\"type\":\"null\"}]}," +
            "\"children\":{\"type\":\"array\",\"items\":{\"$ref\":\"#/$defs/mcp_stub_Tree\"}}" +
          "},\"required\":[\"value\",\"children\"]}," +
          "\"mcp_stub_Shape_Circle\":{\"type\":\"object\",\"properties\":{" +
            "\"radius\":{\"type\":\"number\",\"format\":\"double\"}" +
          "},\"required\":[\"radius\"]}," +
          "\"mcp_stub_Shape_Rect\":{\"type\":\"object\",\"properties\":{" +
            "\"w\":{\"type\":\"number\",\"format\":\"double\"}," +
            "\"h\":{\"type\":\"number\",\"format\":\"double\"}" +
          "},\"required\":[\"w\",\"h\"]}" +
        "}" +
        "}"
    );

    // Tool 4: McpTools_pagePoints — template-instantiation alias PointPage = Page[Point]
    private static final JsonNode REF_PAGE_POINTS = ref(
        "{\"$schema\":\"https://json-schema.org/draft/2020-12/schema\"," +
        "\"type\":\"object\"," +
        "\"properties\":{" +
          "\"page\":{\"$ref\":\"#/$defs/mcp_stub_PointPage\"}" +
        "}," +
        "\"required\":[\"page\"]," +
        "\"$defs\":{" +
          "\"mcp_stub_Point\":{\"type\":\"object\",\"properties\":{" +
            "\"x\":{\"type\":\"integer\",\"format\":\"int32\"}," +
            "\"y\":{\"type\":\"integer\",\"format\":\"int32\"}" +
          "},\"required\":[\"x\",\"y\"]}," +
          "\"mcp_stub_PointPage\":{\"type\":\"object\",\"properties\":{" +
            "\"items\":{\"type\":\"array\",\"items\":{\"$ref\":\"#/$defs/mcp_stub_Point\"}}," +
            "\"total\":{\"type\":\"integer\",\"format\":\"int32\",\"minimum\":0}" +
          "},\"required\":[\"items\",\"total\"]}" +
        "}" +
        "}"
    );

    // Tool 5: McpTools_ping — scalar-only, no $defs
    private static final JsonNode REF_PING = ref(
        "{\"$schema\":\"https://json-schema.org/draft/2020-12/schema\"," +
        "\"type\":\"object\"," +
        "\"properties\":{" +
          "\"seqno\":{\"type\":\"integer\",\"format\":\"int32\"}," +
          "\"label\":{\"type\":\"string\"}" +
        "}," +
        "\"required\":[\"seqno\",\"label\"]" +
        "}"
    );

    // ---------------------------------------------------------------------------
    // Structural equality helper (T7 §5.4):
    //   - JsonObject: compare by key lookup (key-order-insensitive).
    //   - JsonArray for key "required": compare as a set (order-insensitive).
    //   - All other JsonArrays: compare element-by-element (ordered).
    //   - JsonPrimitive / JsonNull: standard .equals() equality.
    // ---------------------------------------------------------------------------
    private static boolean schemasStructurallyEqual(JsonNode actual, JsonNode expected) {
        return schemasStructurallyEqualImpl(actual, expected, false);
    }

    private static boolean schemasStructurallyEqualImpl(JsonNode actual, JsonNode expected, boolean inRequiredKey) {
        if (inRequiredKey) {
            // Both must be arrays; compare as sets of textual elements.
            if (!actual.isArray() || !expected.isArray()) return false;
            Set<String> aSet = new HashSet<>();
            Set<String> eSet = new HashSet<>();
            actual.forEach(n -> aSet.add(n.toString()));
            expected.forEach(n -> eSet.add(n.toString()));
            return aSet.equals(eSet);
        }
        if (actual.isObject() && expected.isObject()) {
            if (actual.size() != expected.size()) return false;
            Iterator<Map.Entry<String, JsonNode>> fields = expected.fields();
            while (fields.hasNext()) {
                Map.Entry<String, JsonNode> entry = fields.next();
                String key = entry.getKey();
                JsonNode expVal = entry.getValue();
                JsonNode actVal = actual.get(key);
                if (actVal == null) return false;
                if (!schemasStructurallyEqualImpl(actVal, expVal, "required".equals(key))) return false;
            }
            return true;
        }
        if (actual.isArray() && expected.isArray()) {
            if (actual.size() != expected.size()) return false;
            for (int i = 0; i < actual.size(); i++) {
                if (!schemasStructurallyEqualImpl(actual.get(i), expected.get(i), false)) return false;
            }
            return true;
        }
        return actual.equals(expected);
    }

    // ---------------------------------------------------------------------------
    // Stub McpTools service: every method returns ok=true (T7 §3 convention).
    // ---------------------------------------------------------------------------
    private static final class StubMcpTools implements McpTools {
        @Override
        public mcp.stub.mcptools.listcollections.Out listCollections(mcp.stub.mcptools.listcollections.In arg) {
            return new mcp.stub.mcptools.listcollections.Out(true);
        }
        @Override
        public mcp.stub.mcptools.submitcomposite.Out submitComposite(mcp.stub.mcptools.submitcomposite.In arg) {
            return new mcp.stub.mcptools.submitcomposite.Out(true);
        }
        @Override
        public mcp.stub.mcptools.processshape.Out processShape(mcp.stub.mcptools.processshape.In arg) {
            return new mcp.stub.mcptools.processshape.Out(true);
        }
        @Override
        public mcp.stub.mcptools.pagepoints.Out pagePoints(mcp.stub.mcptools.pagepoints.In arg) {
            return new mcp.stub.mcptools.pagepoints.Out(true);
        }
        @Override
        public mcp.stub.mcptools.ping.Out ping(mcp.stub.mcptools.ping.In arg) {
            return new mcp.stub.mcptools.ping.Out(true);
        }
    }

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
                // Right: no left value to map, just re-wrap the right value unchanged
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
                // Left: propagate the left value unchanged
                return BaboonEither.left(l.value());
            } else {
                throw new IllegalStateException("BaboonEither unknown variant: " + value);
            }
        }
    };

    // ---------------------------------------------------------------------------
    // MCP test fixtures
    // ---------------------------------------------------------------------------
    private final BaboonCodecContext codecCtx = BaboonCodecContext.Default;
    private final StubMcpTools stub = new StubMcpTools();

    private McpToolsMcpServer<Void> makeServer() {
        return new McpToolsMcpServer<>(
            (method, data, ctx, cc) -> McpToolsWiring.invokeJson(method, data, stub, DEFAULT_RT, cc)
        );
    }

    private JsonRpcResponse send(McpToolsMcpServer<Void> server, McpSession session, JsonRpcRequest req) {
        JsonRpcResponse resp = server.handle(req, session, null, codecCtx);
        assertNotNull(resp, "Expected a response for \"" + req.method + "\" but got null");
        return resp;
    }

    private void initSession(McpToolsMcpServer<Void> server, McpSession session) throws Exception {
        server.handle(
            new JsonRpcRequest(
                MAPPER.readTree("1"),
                "initialize",
                MAPPER.readTree("{\"protocolVersion\":\"2025-06-18\",\"capabilities\":{},\"clientInfo\":{\"name\":\"test-client\",\"version\":\"0.0.1\"}}")
            ),
            session, null, codecCtx
        );
        server.handle(
            new JsonRpcRequest(null, "notifications/initialized", null),
            session, null, codecCtx
        );
    }

    private record ToolsListResult(List<JsonNode> tools, JsonRpcResponse resp) {}

    private ToolsListResult initAndList() throws Exception {
        var server = makeServer();
        var session = new McpSession();
        initSession(server, session);
        var resp = send(server, session,
            new JsonRpcRequest(MAPPER.readTree("2"), "tools/list", null));
        var toolsArray = resp.result.get("tools");
        var tools = new java.util.ArrayList<JsonNode>();
        toolsArray.forEach(tools::add);
        return new ToolsListResult(tools, resp);
    }

    // ---------------------------------------------------------------------------
    // §1 — initialize
    // ---------------------------------------------------------------------------

    @Test
    public void sec1_initialize_responseIsCorrect() throws Exception {
        var server = makeServer();
        var session = new McpSession();

        var resp = send(server, session,
            new JsonRpcRequest(
                MAPPER.readTree("1"),
                "initialize",
                MAPPER.readTree("{\"protocolVersion\":\"2025-06-18\",\"capabilities\":{},\"clientInfo\":{\"name\":\"test-client\",\"version\":\"0.0.1\"}}")
            ));

        assertEquals(1, resp.id.intValue(), "id must be 1");
        assertNull(resp.error, "Expected no error for initialize");
        assertNotNull(resp.result, "result must be present");

        assertEquals("2025-06-18", resp.result.get("protocolVersion").textValue(),
            "protocolVersion must be 2025-06-18");

        var caps = resp.result.get("capabilities");
        assertEquals(1, caps.size(), "capabilities must have exactly one key");
        assertNotNull(caps.get("tools"), "capabilities.tools must be present");
        assertEquals(0, caps.get("tools").size(), "capabilities.tools must be {}");

        var info = resp.result.get("serverInfo");
        assertFalse(info.get("name").textValue().isEmpty(), "serverInfo.name must be non-empty");
        assertFalse(info.get("version").textValue().isEmpty(), "serverInfo.version must be non-empty");
    }

    @Test
    public void sec1_initializedNotification_producesNoResponse() throws Exception {
        var server = makeServer();
        var session = new McpSession();

        // Initialize first.
        server.handle(
            new JsonRpcRequest(
                MAPPER.readTree("0"),
                "initialize",
                MAPPER.readTree("{\"protocolVersion\":\"2025-06-18\",\"capabilities\":{},\"clientInfo\":{\"name\":\"t\",\"version\":\"0\"}}")
            ),
            session, null, codecCtx
        );

        // notifications/initialized has no id — must return null.
        var notifResp = server.handle(
            new JsonRpcRequest(null, "notifications/initialized", null),
            session, null, codecCtx
        );
        assertNull(notifResp, "notifications/initialized MUST produce no response");
    }

    // ---------------------------------------------------------------------------
    // §2 — tools/list + inputSchema validation (K1 tier)
    // ---------------------------------------------------------------------------

    @Test
    public void sec2_toolsList_exactlyFiveToolsInDeclarationOrder() throws Exception {
        var r = initAndList();
        var tools = r.tools();
        var resp = r.resp();

        assertEquals(2, resp.id.intValue(), "id must be 2");
        assertNull(resp.error);
        assertEquals(5, tools.size(), "MUST be exactly 5 tools");

        // Exact position assertions (model declaration order, T7 §0).
        // DELIBERATE-NEGATIVE-CONTROL: replacing "McpTools_ping" with "McpTools_WRONG"
        // on the next line makes this test fail, proving position[4] check is live.
        assertEquals("McpTools_listCollections", tools.get(0).get("name").textValue());
        assertEquals("McpTools_submitComposite", tools.get(1).get("name").textValue());
        assertEquals("McpTools_processShape",    tools.get(2).get("name").textValue());
        assertEquals("McpTools_pagePoints",       tools.get(3).get("name").textValue());
        assertEquals("McpTools_ping",             tools.get(4).get("name").textValue());

        // No "nextCursor" key (§2.2)
        assertNull(resp.result.get("nextCursor"), "nextCursor must not be present");

        // No "description" key for any tool (stub model has no doc comments)
        for (var t : tools) {
            assertNull(t.get("description"), "Tool " + t.get("name") + " must have no description");
        }
    }

    @Test
    public void sec2_eachInputSchema_hasDraft202012SchemaUri() throws Exception {
        var r = initAndList();
        for (var t : r.tools()) {
            var schema = t.get("inputSchema");
            assertEquals(
                "https://json-schema.org/draft/2020-12/schema",
                schema.get("$schema").textValue(),
                "Tool " + t.get("name") + ": $schema must be the Draft 2020-12 URI"
            );
        }
    }

    @Test
    public void sec2_k1_allInputSchemas_areWellFormedJson_viaJackson() throws Exception {
        // K1 part (a) — well-formedness gate: each inputSchema must re-parse through
        // Jackson ObjectMapper.readTree() without throwing.
        // A malformed schema would cause a JsonProcessingException here.
        var r = initAndList();
        for (var t : r.tools()) {
            String schemaJson = t.get("inputSchema").toString();
            // Must not throw — proves the schema is well-formed JSON.
            JsonNode reparsed = MAPPER.readTree(schemaJson);
            assertNotNull(reparsed, "Tool " + t.get("name") + ": schema must not be null after re-parse");
        }
    }

    @Test
    public void sec2_k1_allFiveTools_structuralEqualityToT7Reference() throws Exception {
        // K1 part (b) — structural equality to T7 §2.3 reference.
        // Each returned inputSchema is re-parsed via Jackson (codec-divergence
        // coverage) and compared key-by-key recursively to the embedded T7 reference.
        // "required" arrays are compared as SETS per T7 §5.4.
        // "$defs" keys are compared by lookup (key-order-insensitive via Jackson).
        var r = initAndList();
        var tools = r.tools();

        List<JsonNode> references = List.of(
            REF_LIST_COLLECTIONS,  // tools[0] = McpTools_listCollections
            REF_SUBMIT_COMPOSITE,  // tools[1] = McpTools_submitComposite
            REF_PROCESS_SHAPE,     // tools[2] = McpTools_processShape
            REF_PAGE_POINTS,       // tools[3] = McpTools_pagePoints
            REF_PING               // tools[4] = McpTools_ping
        );

        for (int i = 0; i < 5; i++) {
            String toolName = tools.get(i).get("name").textValue();
            // Re-parse through Jackson to exercise codec round-trip.
            JsonNode actual   = MAPPER.readTree(tools.get(i).get("inputSchema").toString());
            JsonNode expected = references.get(i);
            assertTrue(
                schemasStructurallyEqual(actual, expected),
                "Tool " + toolName + " (index " + i + "): inputSchema is not structurally equal to T7 §2.3 reference.\n" +
                "  actual:   " + tools.get(i).get("inputSchema") + "\n" +
                "  expected: " + expected
            );
        }
    }

    @Test
    public void sec2_k1_negativeControl_wrongReferenceDetectedByComparator() throws Exception {
        // NEGATIVE CONTROL (T7 §5.2 / schema structure):
        // Asserts that the structural comparator DETECTS a wrong reference.
        // If schemasStructurallyEqual erroneously returns true for an incorrect
        // schema, this test FAILS — proving the comparator is live.
        //
        // Wrong reference: ping schema with an extra top-level field "extra":"bad".
        JsonNode wrongRef = ref(
            "{\"$schema\":\"https://json-schema.org/draft/2020-12/schema\"," +
            "\"type\":\"object\"," +
            "\"properties\":{" +
              "\"seqno\":{\"type\":\"integer\",\"format\":\"int32\"}," +
              "\"label\":{\"type\":\"string\"}" +
            "}," +
            "\"required\":[\"seqno\",\"label\"]," +
            "\"extra\":\"bad\"" +
            "}"
        );
        var r = initAndList();
        JsonNode actualPing = MAPPER.readTree(r.tools().get(4).get("inputSchema").toString());

        // The comparator MUST return false for the wrong reference.
        assertFalse(
            schemasStructurallyEqual(actualPing, wrongRef),
            "NEGATIVE CONTROL FAILED: schemasStructurallyEqual returned true for a deliberately-wrong " +
            "reference (extra field 'extra'). The comparator is not functioning correctly."
        );

        // And MUST return true for the correct reference.
        assertTrue(
            schemasStructurallyEqual(actualPing, REF_PING),
            "Positive case failed after negative control: ping schema must equal REF_PING."
        );
    }

    // ---------------------------------------------------------------------------
    // §3 — tools/call (success paths)
    // ---------------------------------------------------------------------------

    @Test
    public void sec3_ping_returnsOkTrue() throws Exception {
        var server = makeServer();
        var session = new McpSession();
        initSession(server, session);

        var resp = send(server, session,
            new JsonRpcRequest(
                MAPPER.readTree("3"),
                "tools/call",
                MAPPER.readTree("{\"name\":\"McpTools_ping\",\"arguments\":{\"seqno\":42,\"label\":\"hello\"}}")
            ));

        assertEquals(3, resp.id.intValue());
        assertNull(resp.error, "Unexpected error on ping call");

        var result = resp.result;
        var content = result.get("content");
        assertEquals(1, content.size(), "content must have exactly one element");
        assertEquals("text", content.get(0).get("type").textValue());

        var payload = MAPPER.readTree(content.get(0).get("text").textValue());
        assertTrue(payload.get("ok").booleanValue(), "ok must be true");

        // isError MUST be false
        var isError = result.get("isError");
        assertTrue(isError == null || !isError.booleanValue(), "isError must be false or absent");
    }

    @Test
    public void sec3_listCollections_returnsOkTrue() throws Exception {
        var server = makeServer();
        var session = new McpSession();
        initSession(server, session);

        var resp = send(server, session,
            new JsonRpcRequest(
                MAPPER.readTree("4"),
                "tools/call",
                // D6/T30: byColor is map[Color,str]; Java encodes/decodes it as a string-keyed
                // object with enum wire-name keys. Send a NON-EMPTY object conforming to the
                // inputSchema (exercises the enum key-codec path).
                MAPPER.readTree("{\"name\":\"McpTools_listCollections\",\"arguments\":{\"tags\":[\"a\",\"b\"],\"uniqueIds\":[1,2],\"labels\":{\"k\":\"v\"},\"byColor\":{\"Green\":\"ok\",\"Red\":\"stop\"}}}")
            ));

        assertEquals(4, resp.id.intValue());
        assertNull(resp.error, "Unexpected error on listCollections call");

        var result = resp.result;
        var content = result.get("content");
        assertEquals(1, content.size());
        assertEquals("text", content.get(0).get("type").textValue());

        var payload = MAPPER.readTree(content.get(0).get("text").textValue());
        assertTrue(payload.get("ok").booleanValue(), "ok must be true");

        var isError = result.get("isError");
        assertTrue(isError == null || !isError.booleanValue(), "isError must be false or absent");
    }

    // ---------------------------------------------------------------------------
    // §4 — tools/call (error paths) — primary negative controls
    // ---------------------------------------------------------------------------

    @Test
    public void sec4_unknownTool_channelAError_code32602() throws Exception {
        // NEGATIVE CONTROL (T7 §5.2): if the server returned a success result for
        // McpTools_nonexistent, assertNotNull(resp.error) would fail.
        var server = makeServer();
        var session = new McpSession();
        initSession(server, session);

        var resp = send(server, session,
            new JsonRpcRequest(
                MAPPER.readTree("5"),
                "tools/call",
                MAPPER.readTree("{\"name\":\"McpTools_nonexistent\",\"arguments\":{}}")
            ));

        assertEquals(5, resp.id.intValue());
        // MUST be a Channel-A error, not a result.
        assertNotNull(resp.error, "Unknown tool must produce a Channel-A error");
        assertNull(resp.result, "No result expected for unknown tool");
        // §4.1: code MUST be -32602 (InvalidParams — unknown tool)
        assertEquals(-32602, resp.error.code, "Unknown tool error code MUST be -32602");
        assertFalse(resp.error.message.isEmpty(), "error.message must be non-empty");
    }

    @Test
    public void sec4_decodeFailure_channelB_isErrorTrue() throws Exception {
        // NEGATIVE CONTROL: if isError were false this test would fail.
        //
        // Channel-B trigger: send ping with missing required "seqno".
        // The generated codec tries to read the required `seqno` field which is
        // absent, causing a NullPointerException. The wiring catch-block wraps it
        // as BaboonWiringError.DecoderFailed and McpToolsWiring.invokeJson returns Left.
        // The MCP server produces Channel-B: result with isError=true.
        var server = makeServer();
        var session = new McpSession();
        initSession(server, session);

        var resp = send(server, session,
            new JsonRpcRequest(
                MAPPER.readTree("6"),
                "tools/call",
                MAPPER.readTree("{\"name\":\"McpTools_ping\",\"arguments\":{\"label\":\"missing-seqno\"}}")
            ));

        assertEquals(6, resp.id.intValue());
        // Channel B: MUST be a result (not error) with isError=true.
        assertNotNull(resp.result, "Channel-B: result must be present");
        assertNull(resp.error, "Channel-B: must not be a JSON-RPC error");

        assertTrue(resp.result.get("isError").booleanValue(),
            "isError MUST be true for Channel-B decode failure");

        var content = resp.result.get("content");
        assertTrue(content.size() > 0, "content must have at least one element");
        assertEquals("text", content.get(0).get("type").textValue());
        assertFalse(content.get(0).get("text").textValue().isEmpty(),
            "content[0].text must be non-empty");
    }
}
