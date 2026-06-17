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
import baboon.runtime.shared.IBaboonRoutableMcpServer;
import baboon.runtime.shared.JsonRpcError;
import baboon.runtime.shared.JsonRpcRequest;
import baboon.runtime.shared.JsonRpcResponse;
import baboon.runtime.shared.McpServerInfo;
import baboon.runtime.shared.McpSession;
import baboon.runtime.shared.McpToolEntry;
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

    // Tool 4: McpTools_processTagged — contract-bearing ADT (T26/D11). `Tagged` is
    // `is HasId`; the HasId contract carries `id: str`, merged into every branch DTO
    // at typing time. Each branch $defs entry has `id` + own field, both required,
    // NO allOf. Branch order in oneOf is declaration order: TagA then TagB.
    private static final JsonNode REF_PROCESS_TAGGED = ref(
        "{\"$schema\":\"https://json-schema.org/draft/2020-12/schema\"," +
        "\"type\":\"object\"," +
        "\"properties\":{" +
          "\"tagged\":{\"$ref\":\"#/$defs/mcp_stub_Tagged\"}" +
        "}," +
        "\"required\":[\"tagged\"]," +
        "\"$defs\":{" +
          "\"mcp_stub_Tagged\":{\"oneOf\":[{\"$ref\":\"#/$defs/mcp_stub_Tagged_TagA\"},{\"$ref\":\"#/$defs/mcp_stub_Tagged_TagB\"}]}," +
          "\"mcp_stub_Tagged_TagA\":{\"type\":\"object\",\"properties\":{" +
            "\"id\":{\"type\":\"string\"}," +
            "\"tag\":{\"type\":\"string\"}" +
          "},\"required\":[\"id\",\"tag\"]}," +
          "\"mcp_stub_Tagged_TagB\":{\"type\":\"object\",\"properties\":{" +
            "\"id\":{\"type\":\"string\"}," +
            "\"weight\":{\"type\":\"integer\",\"format\":\"int32\"}" +
          "},\"required\":[\"id\",\"weight\"]}" +
        "}" +
        "}"
    );

    // Tool 5: McpTools_pagePoints — template-instantiation alias PointPage = Page[Point]
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

    // Tool 6: McpTools_ping — scalar-only, no $defs
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

    // Tool 7: McpTools_describePricing — single scalar field tier: str (D34/T125)
    private static final JsonNode REF_DESCRIBE_PRICING = ref(
        "{\"$schema\":\"https://json-schema.org/draft/2020-12/schema\"," +
        "\"type\":\"object\"," +
        "\"properties\":{" +
          "\"tier\":{\"type\":\"string\"}" +
        "}," +
        "\"required\":[\"tier\"]" +
        "}"
    );

    // T128: Expected description for McpTools_describePricing.
    // This is the output of McpDocs.flatten on the multi-line /** ... */ doc in
    // mcp_stub.baboon: DocFormat.cleanPrefix strips " * " leading prefix and
    // collapses leading/trailing blank lines, preserving internal blank line.
    // Hazard chars: literal $ (dollar), " (double-quote), \\ (two backslashes).
    private static final String DESCRIBE_PRICING_DESCRIPTION =
        "Returns the fee schedule for the requested service tier.\n" +
        "Base cost is $5 per call; \"premium\" tier costs $20 per call.\n" +
        "\n" +
        "Pass the tier name using the \\\\ delimiter convention documented in\n" +
        "the API guide (e.g. \"standard\\\\premium\").";

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
        public mcp.stub.mcptools.processtagged.Out processTagged(mcp.stub.mcptools.processtagged.In arg) {
            return new mcp.stub.mcptools.processtagged.Out(true);
        }
        @Override
        public mcp.stub.mcptools.pagepoints.Out pagePoints(mcp.stub.mcptools.pagepoints.In arg) {
            return new mcp.stub.mcptools.pagepoints.Out(true);
        }
        @Override
        public mcp.stub.mcptools.ping.Out ping(mcp.stub.mcptools.ping.In arg) {
            return new mcp.stub.mcptools.ping.Out(true);
        }
        @Override
        public mcp.stub.mcptools.describepricing.Out describePricing(mcp.stub.mcptools.describepricing.In arg) {
            return new mcp.stub.mcptools.describepricing.Out(true);
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
    public void sec2_toolsList_exactlySevenToolsInDeclarationOrder() throws Exception {
        var r = initAndList();
        var tools = r.tools();
        var resp = r.resp();

        assertEquals(2, resp.id.intValue(), "id must be 2");
        assertNull(resp.error);
        assertEquals(7, tools.size(), "MUST be exactly 7 tools");

        // Exact position assertions (model declaration order, T7 §0).
        // processTagged is declared between processShape and pagePoints (T26/D11),
        // so it occupies index 3 and shifts pagePoints→4, ping→5.
        // describePricing (D34/T125) is declared after ping at index 6.
        // DELIBERATE-NEGATIVE-CONTROL: replacing "McpTools_ping" with "McpTools_WRONG"
        // on the next line makes this test fail, proving position[5] check is live.
        assertEquals("McpTools_listCollections", tools.get(0).get("name").textValue());
        assertEquals("McpTools_submitComposite", tools.get(1).get("name").textValue());
        assertEquals("McpTools_processShape",    tools.get(2).get("name").textValue());
        assertEquals("McpTools_processTagged",   tools.get(3).get("name").textValue());
        assertEquals("McpTools_pagePoints",       tools.get(4).get("name").textValue());
        assertEquals("McpTools_ping",             tools.get(5).get("name").textValue());
        assertEquals("McpTools_describePricing",  tools.get(6).get("name").textValue());

        // No "nextCursor" key (§2.2)
        assertNull(resp.result.get("nextCursor"), "nextCursor must not be present");

        // T119: McpTools_ping carries a single-line doc comment in mcp_stub.baboon.
        // T125/D34: McpTools_describePricing carries a multi-line doc comment.
        // Both documented tools must expose their text as "description".
        // Every undocumented tool must have no description key.
        final String pingDescription = "Liveness probe returning a fixed acknowledgement token.";
        for (var t : tools) {
            String toolName = t.get("name").textValue();
            if ("McpTools_ping".equals(toolName)) {
                assertNotNull(t.get("description"), "Tool " + toolName + " must carry its doc-comment description");
                assertEquals(pingDescription, t.get("description").textValue(),
                    "Tool " + toolName + " must carry its doc-comment description");
            } else if ("McpTools_describePricing".equals(toolName)) {
                // T128: unconditional throw on mismatch — proves $, ", \, \n survive.
                String actualDesc = t.get("description") != null ? t.get("description").textValue() : null;
                if (!DESCRIBE_PRICING_DESCRIPTION.equals(actualDesc)) {
                    throw new AssertionError(
                        "T128: McpTools_describePricing description round-trip FAILED.\n" +
                        "Expected: " + DESCRIBE_PRICING_DESCRIPTION.replace("\n", "\\n") + "\n" +
                        "Actual:   " + (actualDesc != null ? actualDesc.replace("\n", "\\n") : "null")
                    );
                }
            } else {
                assertNull(t.get("description"), "Tool " + toolName + " must have no description");
            }
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
    public void sec2_k1_allSevenTools_structuralEqualityToT7Reference() throws Exception {
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
            REF_PROCESS_TAGGED,    // tools[3] = McpTools_processTagged
            REF_PAGE_POINTS,       // tools[4] = McpTools_pagePoints
            REF_PING,              // tools[5] = McpTools_ping
            REF_DESCRIBE_PRICING   // tools[6] = McpTools_describePricing
        );

        for (int i = 0; i < 7; i++) {
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
        JsonNode actualPing = MAPPER.readTree(r.tools().get(5).get("inputSchema").toString());

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

    @Test
    public void sec3_processTagged_returnsOkTrue() throws Exception {
        // T26/D11: processTagged dispatch with a Tagged TagA value.
        // ADT wire format under --jv-wrapped-adt-branch-codecs=false is the
        // branch-discriminated object {"TagA":{...}} (the codec wraps the branch;
        // the inputSchema oneOf is a separate structural view). Tagged carries no
        // foreign type, so no FFancyStr codec registration is needed.
        var server = makeServer();
        var session = new McpSession();
        initSession(server, session);

        var resp = send(server, session,
            new JsonRpcRequest(
                MAPPER.readTree("7"),
                "tools/call",
                MAPPER.readTree("{\"name\":\"McpTools_processTagged\",\"arguments\":{\"tagged\":{\"TagA\":{\"id\":\"abc\",\"tag\":\"hello\"}}}}")
            ));

        assertEquals(7, resp.id.intValue());
        assertNull(resp.error, "Unexpected error on processTagged call");

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

    // ---------------------------------------------------------------------------
    // T114 — PUBLIC routable-server surface (the muxer composition seam).
    //
    // Acceptance: given a generated <Service>McpServer instance, a sibling object
    // reads its serverInfo + tool list and invokes one tool by flat name
    //   - via the PUBLIC interface IBaboonRoutableMcpServer<Ctx>,
    //   - WITHOUT subclassing the server,
    //   - WITHOUT calling handle().
    // This proves the muxer can build the union tools/list, the tool-name->owner
    // table, and route a tools/call into the owning server reusing Channel-A/B.
    // ---------------------------------------------------------------------------

    @Test
    public void t114_publicSurface_serverInfoAndToolsReadableWithoutHandle() throws Exception {
        // Bind the concrete server purely through the PUBLIC interface — a sibling
        // muxer would hold exactly this static type, never the concrete subclass.
        IBaboonRoutableMcpServer<Void> routable = makeServer();

        McpServerInfo info = routable.serverInfo();
        assertEquals("McpTools", info.name, "serverInfo.name must be readable publicly");
        assertFalse(info.version.isEmpty(), "serverInfo.version must be non-empty");

        List<McpToolEntry> tools = routable.tools();
        assertEquals(6, tools.size(), "public tool registry must list all 6 tools");
        // Declaration order (matches §2 tools/list).
        assertEquals("McpTools_listCollections", tools.get(0).name);
        assertEquals("McpTools_submitComposite", tools.get(1).name);
        assertEquals("McpTools_processShape",    tools.get(2).name);
        assertEquals("McpTools_processTagged",   tools.get(3).name);
        assertEquals("McpTools_pagePoints",      tools.get(4).name);
        assertEquals("McpTools_ping",            tools.get(5).name);
        for (var t : tools) {
            assertEquals("McpTools", t.method.serviceName(), "method.serviceName must be McpTools");
            assertFalse(t.method.methodName().isEmpty(), "method.methodName must be non-empty");
        }
    }

    @Test
    public void t114_publicSurface_routeToolCall_channelA_right() throws Exception {
        // Muxer flow: look up the entry by flat name, then route by its method via
        // the PUBLIC routeToolCall — no handle(), no subclassing.
        IBaboonRoutableMcpServer<Void> routable = makeServer();

        McpToolEntry entry = null;
        for (var t : routable.tools()) {
            if (t.name.equals("McpTools_ping")) {
                entry = t;
            }
        }
        assertNotNull(entry, "McpTools_ping entry must be resolvable from the public registry");

        BaboonEither<BaboonWiringError, String> result =
            routable.routeToolCall(entry.method, "{\"seqno\":42,\"label\":\"hi\"}", null, codecCtx);
        // Stub ping returns ok=true → Right (Channel-A).
        assertTrue(result instanceof BaboonEither.Right, "routeToolCall must return Right for a valid call");
        var payload = MAPPER.readTree(((BaboonEither.Right<BaboonWiringError, String>) result).value());
        assertTrue(payload.get("ok").booleanValue(), "ok must be true");
    }

    @Test
    public void t114_publicSurface_routeToolCall_channelB_left() throws Exception {
        // NEGATIVE CONTROL: ping with missing required "seqno" makes the wiring
        // decoder throw → invokeJson returns Left. The public dispatch entry
        // surfaces that Left unchanged for the muxer to map to Channel-B.
        IBaboonRoutableMcpServer<Void> routable = makeServer();

        McpToolEntry entry = null;
        for (var t : routable.tools()) {
            if (t.name.equals("McpTools_ping")) {
                entry = t;
            }
        }
        assertNotNull(entry);

        BaboonEither<BaboonWiringError, String> result =
            routable.routeToolCall(entry.method, "{\"label\":\"missing-seqno\"}", null, codecCtx);
        assertTrue(result instanceof BaboonEither.Left, "routeToolCall must surface a decode failure as Left");
    }
}
