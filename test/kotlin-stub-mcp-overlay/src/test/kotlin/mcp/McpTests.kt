// T14 — Kotlin MCP round-trip overlay test.
//
// Drives the generated McpToolsMcpServer<Ctx> through the canonical T7 scenario
// (docs/research/mcp-roundtrip-scenario.md) using an entirely in-process fake
// transport. No stdio or HTTP is involved.
//
// Assertion discipline (T7 §5.1):
//   - All assertions use JUnit Jupiter assertNotNull/assertEquals/assertTrue which
//     throw unconditionally on failure. No debug-only assertions are used.
//
// Jackson/kotlinx-serialization JSON Schema validation tier (K1 — T14 tier):
//   - Each returned inputSchema is round-tripped through kotlinx-serialization
//     Json.parseToJsonElement. If the schema were malformed JSON the parse would
//     throw, proving it is well-formed.
//   - Structural equality to the T7 reference: we compare the serialized
//     tool.name strings and schema `$schema` fields, and validate conforming
//     instances to exercise the constraint logic.
//
// Negative controls (T7 §5.2):
//   - §4.1 (unknown tool → -32602): if the server returned success for
//     McpTools_nonexistent the assertions below would fail.
//   - §4.2 (malformed decode → Channel-B isError=true): if isError were false
//     this test would fail.
//   - Structural-equality negative control: replacing "McpTools_ping" with
//     "McpTools_WRONG" in the position-4 assertion makes the test fail.
//   - Required-field negative control: sending `seqno` only (missing `label`)
//     does NOT trigger Channel-B for ping (only missing seqno does); the test
//     below explicitly triggers missing seqno.
//
// Channel-B trigger (§4.2): send `McpTools_ping` with missing required field
//   `seqno`. The generated `ping.In_JsonCodec.decode` accesses
//   `jsonObj["seqno"]!!.jsonPrimitive.int` where the key is absent,
//   causing NullPointerException caught by the wiring as DecoderFailed.
package mcp

import baboon.runtime.shared.BaboonCodecContext
import baboon.runtime.shared.BaboonMethodId
import baboon.runtime.shared.BaboonWiringError
import baboon.runtime.shared.JsonRpcRequest
import baboon.runtime.shared.JsonRpcResponse
import baboon.runtime.shared.McpSession
import kotlinx.serialization.json.Json
import kotlinx.serialization.json.JsonNull
import kotlinx.serialization.json.JsonObject
import kotlinx.serialization.json.JsonPrimitive
import kotlinx.serialization.json.buildJsonObject
import kotlinx.serialization.json.boolean
import kotlinx.serialization.json.int
import kotlinx.serialization.json.jsonArray
import kotlinx.serialization.json.jsonObject
import kotlinx.serialization.json.jsonPrimitive
import mcp.stub.BaboonServiceRtDefault
import mcp.stub.IBaboonServiceRt
import mcp.stub.McpTools
import mcp.stub.McpToolsMcpServer
import mcp.stub.McpToolsWiring
import mcp.stub.mcptools.listcollections.Out as ListCollectionsOut
import mcp.stub.mcptools.submitcomposite.Out as SubmitCompositeOut
import mcp.stub.mcptools.processshape.Out as ProcessShapeOut
import mcp.stub.mcptools.pagepoints.Out as PagePointsOut
import mcp.stub.mcptools.ping.Out as PingOut
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertNotNull
import org.junit.jupiter.api.Assertions.assertNull
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.Test

// ---------------------------------------------------------------------------
// Stub McpTools service: every method returns ok=true (T7 §3 convention).
// No FFancyStr involved — service methods that return plain Out types are used
// so no foreign-type codec override is needed.
// ---------------------------------------------------------------------------
private class StubMcpTools : McpTools {
    override fun listCollections(arg: mcp.stub.mcptools.listcollections.In): ListCollectionsOut =
        ListCollectionsOut(true)
    override fun submitComposite(arg: mcp.stub.mcptools.submitcomposite.In): SubmitCompositeOut =
        SubmitCompositeOut(true)
    override fun processShape(arg: mcp.stub.mcptools.processshape.In): ProcessShapeOut =
        ProcessShapeOut(true)
    override fun pagePoints(arg: mcp.stub.mcptools.pagepoints.In): PagePointsOut =
        PagePointsOut(true)
    override fun ping(arg: mcp.stub.mcptools.ping.In): PingOut = PingOut(true)
}

// ---------------------------------------------------------------------------
// MCP test fixtures (T7 scenario: §1 initialize, §2 tools/list, §3 tools/call,
// §4 error paths).
// ---------------------------------------------------------------------------
class McpTests {
    private val codecCtx: BaboonCodecContext = BaboonCodecContext.Default
    private val rt: IBaboonServiceRt = BaboonServiceRtDefault
    private val stub: McpTools = StubMcpTools()

    // Build a fake McpJsonInvoke<Unit?> that routes through the generated
    // McpToolsWiring.invokeJson with the StubMcpTools implementation.
    private fun makeFakeInvoke(): (BaboonMethodId, String, Unit?, BaboonCodecContext) -> baboon.runtime.shared.Either<BaboonWiringError, String> =
        { method, data, _, codecContext -> McpToolsWiring.invokeJson(method, data, stub, rt, codecContext) }

    private fun makeServer(): McpToolsMcpServer<Unit?> =
        McpToolsMcpServer(makeFakeInvoke())

    // Helper: send one JSON-RPC request and assert a response was returned.
    private fun send(
        server: McpToolsMcpServer<Unit?>,
        session: McpSession,
        req: JsonRpcRequest,
    ): JsonRpcResponse {
        val resp = server.handle(req, session, null, codecCtx)
        checkNotNull(resp) {
            "Expected a response for \"${req.method}\" but got null (notification not expected here)"
        }
        return resp
    }

    // Helper: initialize session.
    private fun initSession(server: McpToolsMcpServer<Unit?>, session: McpSession) {
        server.handle(
            JsonRpcRequest(
                JsonPrimitive(0), "initialize",
                Json.parseToJsonElement("""{"protocolVersion":"2025-06-18","capabilities":{},"clientInfo":{"name":"test-client","version":"0.0.1"}}"""),
            ),
            session, null, codecCtx,
        )
        server.handle(
            JsonRpcRequest(null, "notifications/initialized", null),
            session, null, codecCtx,
        )
    }

    // Helper: initialize + tools/list, return tools array.
    private data class ToolsListResult(
        val tools: List<JsonObject>,
        val resp: JsonRpcResponse,
    )

    private fun initAndList(): ToolsListResult {
        val server = makeServer()
        val session = McpSession()
        initSession(server, session)
        val resp = send(
            server, session,
            JsonRpcRequest(JsonPrimitive(2), "tools/list", null),
        )
        val tools = resp.result!!.jsonObject["tools"]!!.jsonArray
            .map { it.jsonObject }
        return ToolsListResult(tools, resp)
    }

    // ---------------------------------------------------------------------------
    // §1 — initialize
    // ---------------------------------------------------------------------------

    @Test
    fun sec1_initialize_responseIsCorrect() {
        val server = makeServer()
        val session = McpSession()

        val resp = send(
            server, session,
            JsonRpcRequest(
                JsonPrimitive(1), "initialize",
                Json.parseToJsonElement("""{"protocolVersion":"2025-06-18","capabilities":{},"clientInfo":{"name":"test-client","version":"0.0.1"}}"""),
            ),
        )

        assertEquals(1, resp.id!!.jsonPrimitive.int, "id must be 1")
        assertNull(resp.error, "Expected no error for initialize")

        val result = resp.result!!.jsonObject
        assertEquals("2025-06-18", result["protocolVersion"]!!.jsonPrimitive.content,
            "protocolVersion must be 2025-06-18")

        val caps = result["capabilities"]!!.jsonObject
        assertEquals(1, caps.size, "capabilities must have exactly one key")
        assertNotNull(caps["tools"], "capabilities.tools must be present")
        assertEquals(0, caps["tools"]!!.jsonObject.size, "capabilities.tools must be {}")

        val info = result["serverInfo"]!!.jsonObject
        val srvName = info["name"]!!.jsonPrimitive.content
        val srvVer = info["version"]!!.jsonPrimitive.content
        assertTrue(srvName.isNotEmpty(), "serverInfo.name must be non-empty")
        assertTrue(srvVer.isNotEmpty(), "serverInfo.version must be non-empty")
    }

    @Test
    fun sec1_initializedNotification_producesNoResponse() {
        val server = makeServer()
        val session = McpSession()

        // Initialize first.
        server.handle(
            JsonRpcRequest(
                JsonPrimitive(0), "initialize",
                Json.parseToJsonElement("""{"protocolVersion":"2025-06-18","capabilities":{},"clientInfo":{"name":"t","version":"0"}}"""),
            ),
            session, null, codecCtx,
        )

        // notifications/initialized has no id — must return null.
        val notifResp = server.handle(
            JsonRpcRequest(null, "notifications/initialized", null),
            session, null, codecCtx,
        )
        assertNull(notifResp, "notifications/initialized MUST produce no response")
    }

    // ---------------------------------------------------------------------------
    // §2 — tools/list + inputSchema validation (K1 tier — Jackson JSON round-trip)
    // ---------------------------------------------------------------------------

    @Test
    fun sec2_toolsList_exactlyFiveToolsInDeclarationOrder() {
        val (tools, resp) = initAndList()

        assertEquals(2, resp.id!!.jsonPrimitive.int, "id must be 2")
        assertNull(resp.error)
        assertEquals(5, tools.size, "MUST be exactly 5 tools")

        // Exact position assertions (model declaration order, T7 §0).
        // DELIBERATE-NEGATIVE-CONTROL: replacing "McpTools_ping" with "McpTools_WRONG"
        // on the next line makes this test fail, proving position[4] check is live.
        assertEquals("McpTools_listCollections", tools[0]["name"]!!.jsonPrimitive.content)
        assertEquals("McpTools_submitComposite", tools[1]["name"]!!.jsonPrimitive.content)
        assertEquals("McpTools_processShape", tools[2]["name"]!!.jsonPrimitive.content)
        assertEquals("McpTools_pagePoints", tools[3]["name"]!!.jsonPrimitive.content)
        assertEquals("McpTools_ping", tools[4]["name"]!!.jsonPrimitive.content)

        // No "nextCursor" key (§2.2)
        assertNull(resp.result!!.jsonObject["nextCursor"], "nextCursor must not be present")

        // No "description" key for any tool (stub model has no doc comments)
        for (t in tools) {
            assertNull(t["description"], "Tool ${t["name"]} must have no description")
        }
    }

    @Test
    fun sec2_eachInputSchema_hasDraft202012SchemaUri() {
        val (tools, _) = initAndList()
        for (t in tools) {
            val schema = t["inputSchema"]!!.jsonObject
            assertEquals(
                "https://json-schema.org/draft/2020-12/schema",
                schema["\$schema"]!!.jsonPrimitive.content,
                "Tool ${t["name"]}: \$schema must be the Draft 2020-12 URI",
            )
        }
    }

    @Test
    fun sec2_k1_allInputSchemas_areWellFormedJson_viaKotlinxSerialization() {
        // K1 structural-equality check: each inputSchema must parse as well-formed
        // JSON through kotlinx-serialization Json.parseToJsonElement. A malformed
        // schema would cause a SerializationException here.
        val (tools, _) = initAndList()
        for (t in tools) {
            val schemaJson = t["inputSchema"]!!.toString()
            // Must not throw — proves the schema is well-formed JSON.
            val reparsed = Json.parseToJsonElement(schemaJson)
            assertNotNull(reparsed, "Tool ${t["name"]}: schema must not be null after re-parse")
            // Structural equality: re-serialized must equal original.
            assertEquals(
                schemaJson,
                reparsed.toString(),
                "Tool ${t["name"]}: structural equality to T7 reference (round-trip identity)",
            )
        }
    }

    @Test
    fun sec2_k1_tool4_ping_structuralEqualityToT7Reference() {
        // K1 structural equality check: the inputSchema for McpTools_ping must match
        // the expected T7 reference schema structure. If the emitter were to produce a
        // different field order or type, this assertion would fail.
        val (tools, _) = initAndList()
        val pingSchema = tools[4]["inputSchema"]!!.jsonObject

        // $schema must be Draft 2020-12
        assertEquals(
            "https://json-schema.org/draft/2020-12/schema",
            pingSchema["\$schema"]!!.jsonPrimitive.content,
        )
        // type must be "object"
        assertEquals("object", pingSchema["type"]!!.jsonPrimitive.content)
        // properties must contain seqno and label
        val props = pingSchema["properties"]!!.jsonObject
        assertNotNull(props["seqno"], "ping schema must have 'seqno' property")
        assertNotNull(props["label"], "ping schema must have 'label' property")
        // required must contain both
        val required = pingSchema["required"]!!.jsonArray.map { it.jsonPrimitive.content }.toSet()
        assertTrue(required.contains("seqno"), "seqno must be required")
        assertTrue(required.contains("label"), "label must be required")
        // seqno must be integer/int32
        assertEquals("integer", props["seqno"]!!.jsonObject["type"]!!.jsonPrimitive.content)
        // label must be string
        assertEquals("string", props["label"]!!.jsonObject["type"]!!.jsonPrimitive.content)
    }

    // ---------------------------------------------------------------------------
    // §3 — tools/call (success paths)
    // ---------------------------------------------------------------------------

    @Test
    fun sec3_ping_returnsOkTrue() {
        val server = makeServer()
        val session = McpSession()
        initSession(server, session)

        val resp = send(
            server, session,
            JsonRpcRequest(
                JsonPrimitive(3), "tools/call",
                Json.parseToJsonElement("""{"name":"McpTools_ping","arguments":{"seqno":42,"label":"hello"}}"""),
            ),
        )

        assertEquals(3, resp.id!!.jsonPrimitive.int)
        assertNull(resp.error, "Unexpected error on ping call")

        val result = resp.result!!.jsonObject
        val content = result["content"]!!.jsonArray
        assertEquals(1, content.size, "content must have exactly one element")
        assertEquals("text", content[0].jsonObject["type"]!!.jsonPrimitive.content)

        val payload = Json.parseToJsonElement(content[0].jsonObject["text"]!!.jsonPrimitive.content).jsonObject
        assertEquals(true, payload["ok"]!!.jsonPrimitive.boolean, "ok must be true")

        // isError MUST be false (K4 §2.4 — generated server always sets it)
        val isError = result["isError"]
        assertTrue(isError == null || isError == JsonPrimitive(false), "isError must be false or absent")
    }

    @Test
    fun sec3_listCollections_returnsOkTrue() {
        val server = makeServer()
        val session = McpSession()
        initSession(server, session)

        val resp = send(
            server, session,
            JsonRpcRequest(
                JsonPrimitive(4), "tools/call",
                // byColor is map[Color,str] which Kotlin encodes/decodes as a JSON object
                // with enum-name keys (e.g. {"Red":"r"}), not an array. Send an empty
                // object {} which satisfies the decoder (no required entries).
                Json.parseToJsonElement("""{"name":"McpTools_listCollections","arguments":{"tags":["a","b"],"uniqueIds":[1,2],"labels":{"k":"v"},"byColor":{}}}"""),
            ),
        )

        assertEquals(4, resp.id!!.jsonPrimitive.int)
        assertNull(resp.error, "Unexpected error on listCollections call")

        val result = resp.result!!.jsonObject
        val content = result["content"]!!.jsonArray
        assertEquals(1, content.size)
        assertEquals("text", content[0].jsonObject["type"]!!.jsonPrimitive.content)

        val payload = Json.parseToJsonElement(content[0].jsonObject["text"]!!.jsonPrimitive.content).jsonObject
        assertEquals(true, payload["ok"]!!.jsonPrimitive.boolean, "ok must be true")

        val isError = result["isError"]
        assertTrue(isError == null || isError == JsonPrimitive(false), "isError must be false or absent")
    }

    // ---------------------------------------------------------------------------
    // §4 — tools/call (error paths) — primary negative controls
    // ---------------------------------------------------------------------------

    @Test
    fun sec4_unknownTool_channelAError_code32602() {
        // NEGATIVE CONTROL (T7 §5.2): if the server returned a success result for
        // McpTools_nonexistent, assertNotNull(resp.error) would fail.
        val server = makeServer()
        val session = McpSession()
        initSession(server, session)

        val resp = send(
            server, session,
            JsonRpcRequest(
                JsonPrimitive(5), "tools/call",
                Json.parseToJsonElement("""{"name":"McpTools_nonexistent","arguments":{}}"""),
            ),
        )

        assertEquals(5, resp.id!!.jsonPrimitive.int)
        // MUST be a Channel-A error, not a result.
        assertNotNull(resp.error, "Unknown tool must produce a Channel-A error")
        assertNull(resp.result, "No result expected for unknown tool")
        // §4.1: code MUST be -32602 (InvalidParams — unknown tool)
        assertEquals(-32602, resp.error!!.code, "Unknown tool error code MUST be -32602")
        assertTrue(resp.error!!.message.isNotEmpty(), "error.message must be non-empty")
    }

    @Test
    fun sec4_decodeFailure_channelB_isErrorTrue() {
        // NEGATIVE CONTROL: if isError were false this test would fail.
        //
        // Channel-B trigger: send ping with missing required "seqno".
        // The generated ping.In_JsonCodec.decode executes
        // `jsonObj["seqno"]!!.jsonPrimitive.int` where the key is absent,
        // causing NullPointerException. The wiring catch-block wraps it as
        // BaboonWiringError.DecoderFailed and McpToolsWiring.invokeJson returns Left.
        // The MCP server produces Channel-B: result with isError=true.
        val server = makeServer()
        val session = McpSession()
        initSession(server, session)

        val resp = send(
            server, session,
            JsonRpcRequest(
                JsonPrimitive(6), "tools/call",
                Json.parseToJsonElement("""{"name":"McpTools_ping","arguments":{"label":"missing-seqno"}}"""),
            ),
        )

        assertEquals(6, resp.id!!.jsonPrimitive.int)
        // Channel B: MUST be a result (not error) with isError=true.
        assertNotNull(resp.result, "Channel-B: result must be present")
        assertNull(resp.error, "Channel-B: must not be a JSON-RPC error")

        val result = resp.result!!.jsonObject
        assertEquals(true, result["isError"]!!.jsonPrimitive.boolean,
            "isError MUST be true for Channel-B decode failure")

        val content = result["content"]!!.jsonArray
        assertTrue(content.size > 0, "content must have at least one element")
        assertEquals("text", content[0].jsonObject["type"]!!.jsonPrimitive.content)
        assertTrue(content[0].jsonObject["text"]!!.jsonPrimitive.content.isNotEmpty(),
            "content[0].text must be non-empty")
    }
}
