// T178 / D40 — Kotlin zero-service MCP overlay test (RED baseline).
//
// Purpose: prove the ZERO-SERVICE MCP runtime contract. The zero-service model
// (mcp-stub-zero-services-ok) declares @root types but NO RPC block, so
// servicesOf(domain) is empty and NO per-service `<Service>McpServer` is
// generated. With zero services the ONLY source of the MCP runtime types
// (AbstractMcpMuxer / IBaboonRoutableMcpServer / McpServerInfo / McpSession /
// JsonRpcRequest / JsonRpcResponse / JsonRpcErrorCodes) is the STATIC runtime
// file `BaboonMcpRuntime.kt`.
//
// RED (pre-fix): the current generator emits NO `BaboonMcpRuntime.kt` for a
// zero-service model, so the imports below are unresolved and `gradle test`
// FAILS to compile ("unresolved reference: AbstractMcpMuxer" / package
// baboon.runtime.shared missing the MCP types). That failure IS the D40
// reproduction.
//
// GREEN (post-fix, later task): once the generator emits the static MCP runtime
// even when servicesOf(domain) is empty, this file compiles and the runtime
// assertions below pass — an empty muxer lists zero tools and rejects any
// tools/call with JSON-RPC -32602.
//
// Assertion discipline: explicit throw on failure — unconditional.

package mcpzero

import baboon.runtime.shared.AbstractMcpMuxer
import baboon.runtime.shared.BaboonCodecContext
import baboon.runtime.shared.JsonRpcErrorCodes
import baboon.runtime.shared.JsonRpcRequest
import baboon.runtime.shared.JsonRpcResponse
import baboon.runtime.shared.McpServerInfo
import baboon.runtime.shared.McpSession
import kotlinx.serialization.json.JsonPrimitive
import kotlinx.serialization.json.buildJsonObject
import kotlinx.serialization.json.jsonArray
import kotlinx.serialization.json.jsonObject
import kotlin.test.Test

class McpZeroTests {
    private val codecCtx = BaboonCodecContext.Default

    // COMPILE-TIME contract: constructing AbstractMcpMuxer<Ctx> with ZERO
    // registered servers requires the static runtime file to exist. With zero
    // services there is no generated <Service>McpServer to import — these types
    // resolve ONLY from BaboonMcpRuntime.kt.
    private fun makeEmptyMuxer(): AbstractMcpMuxer<Any?> =
        AbstractMcpMuxer(McpServerInfo("ZeroEndpoint", "1.0.0"))

    private fun initedSession(mux: AbstractMcpMuxer<Any?>): McpSession {
        val session = McpSession()
        mux.handle(
            JsonRpcRequest(
                JsonPrimitive(0), "initialize",
                buildJsonObject {
                    put("protocolVersion", JsonPrimitive("2025-06-18"))
                    put("capabilities", buildJsonObject { })
                    put("clientInfo", buildJsonObject {
                        put("name", JsonPrimitive("t"))
                        put("version", JsonPrimitive("0"))
                    })
                }
            ),
            session, null, codecCtx
        )
        mux.handle(JsonRpcRequest(null, "notifications/initialized", null), session, null, codecCtx)
        return session
    }

    // RUNTIME contract (a): tools/list on an empty muxer returns an empty array.
    @Test
    fun zeroServices_toolsList_isEmpty() {
        val mux = makeEmptyMuxer()
        val session = initedSession(mux)

        val resp: JsonRpcResponse = mux.handle(
            JsonRpcRequest(JsonPrimitive(1), "tools/list", null),
            session, null, codecCtx
        ) ?: throw AssertionError("tools/list must return a response")

        if (resp.error != null) throw AssertionError("tools/list must not return an error")
        val result = resp.result ?: throw AssertionError("tools/list must carry a result")
        val tools = result.jsonObject["tools"]?.jsonArray
            ?: throw AssertionError("result must carry a tools array")
        if (tools.size != 0) throw AssertionError("empty muxer MUST list zero tools, got ${tools.size}")
    }

    // RUNTIME contract (b): an unknown tool call yields JSON-RPC -32602.
    @Test
    fun zeroServices_unknownToolCall_code32602() {
        val mux = makeEmptyMuxer()
        val session = initedSession(mux)

        val resp: JsonRpcResponse = mux.handle(
            JsonRpcRequest(
                JsonPrimitive(2), "tools/call",
                buildJsonObject {
                    put("name", JsonPrimitive("anything_at_all"))
                    put("arguments", buildJsonObject { })
                }
            ),
            session, null, codecCtx
        ) ?: throw AssertionError("tools/call must return a response")

        if (resp.result != null) throw AssertionError("no result expected for unknown tool")
        val error = resp.error ?: throw AssertionError("unknown tool on empty muxer MUST produce a Channel-A error")
        if (error.code != JsonRpcErrorCodes.INVALID_PARAMS) {
            throw AssertionError("unknown-tool error code MUST be -32602 (InvalidParams), got ${error.code}")
        }
        if (error.message.isEmpty()) throw AssertionError("error.message must be non-empty")
    }
}
