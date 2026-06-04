package baboon.runtime.shared

// Additive MCP server runtime (decisions ledger M1; contract:
// docs/research/mcp-dispatch-runtime-contract.md). Emitted only when
// `--kt-generate-mcp-server=true`; the service-wiring runtime in
// BaboonServiceWiring.kt is unchanged. These types are STATIC (no per-model
// templating) — the only per-model code is the generated `<Service>McpServer`
// and its tool-registry literals.
//
// The MCP dispatch surface is transport-abstract: it implements the JSON-RPC
// method state machine + tool dispatch ONLY. Bytes-in/bytes-out (stdio framing,
// Streamable-HTTP bodies) is an injected adapter the generated surface never
// contains — mirroring the abstract-context service contract, which supplies
// `Ctx` per invocation rather than baking an I/O loop into the wrapper.

import kotlinx.serialization.json.Json
import kotlinx.serialization.json.JsonElement
import kotlinx.serialization.json.JsonNull
import kotlinx.serialization.json.JsonObject
import kotlinx.serialization.json.JsonArray
import kotlinx.serialization.json.JsonPrimitive
import kotlinx.serialization.json.buildJsonObject
import kotlinx.serialization.json.buildJsonArray
import kotlinx.serialization.json.jsonObject
import kotlinx.serialization.json.jsonPrimitive
import kotlinx.serialization.json.contentOrNull

// --- JSON-RPC value types (already parsed from bytes by the adapter) ---
//
// A notification carries no `id`; the server produces no response for it.
// `params`/`result`/`error.data` are the language's existing JSON value
// type (Kotlin: `JsonElement?`) — the MCP runtime reuses what the JSON
// codecs already speak rather than introducing a new model.

data class JsonRpcRequest(
    val id: JsonElement?,
    val method: String,
    val params: JsonElement?,
)

data class JsonRpcError(
    val code: Int,
    val message: String,
    val data: JsonElement? = null,
)

data class JsonRpcResponse(
    val id: JsonElement?,
    val result: JsonElement? = null,
    val error: JsonRpcError? = null,
)

// JSON-RPC / MCP protocol constants (wire contract K4).
object McpProtocol {
    const val VERSION = "2025-06-18"
}

object JsonRpcErrorCodes {
    const val PARSE_ERROR = -32700
    const val INVALID_REQUEST = -32600
    const val METHOD_NOT_FOUND = -32601
    const val INVALID_PARAMS = -32602
    const val INTERNAL_ERROR = -32603
}

// --- Per-connection state (adapter-owned) ---
//
// The "initialized" precondition (reject `tools/*` before a successful
// `initialize`) is per-connection state; a connection is a transport concept.
// The latch therefore lives in this tiny value the adapter creates per
// connection, NOT as ambient mutable state inside the server object (which stays
// immutable and shareable across concurrent connections).
class McpSession {
    var initialized: Boolean = false
}

// --- Tool registry ---
//
// One entry per Baboon method bound to a server. `inputSchema` is the
// precomputed, self-contained JSON Schema (from the shared T5 emitter), carried
// as a constant value — the runtime does not compute schemas.
data class McpToolEntry(
    val name: String,
    val method: BaboonMethodId,
    val inputSchema: JsonElement,
    val description: String? = null,
)

data class McpServerInfo(
    val name: String,
    val version: String,
)

// --- Dispatch interface ---
//
// The single generated entrypoint, analogous to `IBaboonJsonServiceCtx<Ctx,R>`.
// It is NOT R-parametric (an MCP response is always a `JsonRpcResponse` value);
// the only free type parameter is the caller's `Ctx` — the SAME `Ctx` the
// service-wiring contract threads. `handle` is synchronous and performs no I/O.
// It returns `null` for an accepted notification (no reply).
interface IBaboonMcpServer<Ctx> {
    fun handle(request: JsonRpcRequest, session: McpSession, ctx: Ctx, codecCtx: BaboonCodecContext): JsonRpcResponse?
}

// The JSON `tools/call` delegate the generated server supplies: it routes
// one tool invocation into the already-generated service dispatch (the
// errors-mode `invokeJson`, which returns the service-result container). The
// MCP layer turns that `Either<BaboonWiringError, String>` into Channel-A /
// Channel-B per the wire contract (K4 §3). The codecs are reached exclusively
// through this delegate; the MCP runtime holds no codec logic itself.
typealias McpJsonInvoke<Ctx> = (method: BaboonMethodId, data: String, ctx: Ctx, codecCtx: BaboonCodecContext) -> Either<BaboonWiringError, String>

// --- Transport-abstract dispatch base ---
//
// Shared `handle` state machine. The generated `<Service>McpServer` extends this
// with its fixed `serverInfo`, ordered tool registry, and `invokeJson` delegate.
// All JSON-RPC method strings ("tools/list" …) and result keys ("protocolVersion",
// "inputSchema" …) are literal lowercase strings, NOT subject to any per-language
// symbol casing.
abstract class AbstractBaboonMcpServer<Ctx> : IBaboonMcpServer<Ctx> {
    protected abstract val serverInfo: McpServerInfo
    protected abstract val tools: List<McpToolEntry>
    protected abstract fun invokeJson(method: BaboonMethodId, data: String, ctx: Ctx, codecCtx: BaboonCodecContext): Either<BaboonWiringError, String>

    private fun byName(): Map<String, McpToolEntry> {
        val m = mutableMapOf<String, McpToolEntry>()
        for (t in tools) m[t.name] = t
        return m
    }

    override fun handle(request: JsonRpcRequest, session: McpSession, ctx: Ctx, codecCtx: BaboonCodecContext): JsonRpcResponse? {
        val id = request.id
        return when (request.method) {
            "initialize" -> {
                val params = request.params
                val pv = if (params is JsonObject) params["protocolVersion"] else null
                if (params == null || pv == null) {
                    errorResponse(id, JsonRpcErrorCodes.INVALID_PARAMS, "initialize: missing protocolVersion")
                } else {
                    session.initialized = true
                    val result = buildJsonObject {
                        put("protocolVersion", JsonPrimitive(McpProtocol.VERSION))
                        put("capabilities", buildJsonObject { put("tools", buildJsonObject { }) })
                        put("serverInfo", buildJsonObject {
                            put("name", JsonPrimitive(serverInfo.name))
                            put("version", JsonPrimitive(serverInfo.version))
                        })
                    }
                    JsonRpcResponse(id, result)
                }
            }
            "notifications/initialized" -> null
            "tools/list" -> {
                if (!session.initialized) {
                    errorResponse(id, JsonRpcErrorCodes.INVALID_REQUEST, "tools/list before initialize")
                } else {
                    val toolsArray = buildJsonArray {
                        for (t in tools) {
                            val entry = buildJsonObject {
                                put("name", JsonPrimitive(t.name))
                                put("inputSchema", t.inputSchema)
                                t.description?.let { put("description", JsonPrimitive(it)) }
                            }
                            add(entry)
                        }
                    }
                    JsonRpcResponse(id, buildJsonObject { put("tools", toolsArray) })
                }
            }
            "tools/call" -> {
                if (!session.initialized) {
                    errorResponse(id, JsonRpcErrorCodes.INVALID_REQUEST, "tools/call before initialize")
                } else {
                    val paramsObj = request.params as? JsonObject
                    val nameEl = paramsObj?.get("name")
                    if (nameEl == null || nameEl is JsonNull) {
                        errorResponse(id, JsonRpcErrorCodes.INVALID_PARAMS, "tools/call: missing tool name")
                    } else {
                        val toolName = (nameEl as? JsonPrimitive)?.contentOrNull
                            ?: return errorResponse(id, JsonRpcErrorCodes.INVALID_PARAMS, "tools/call: tool name must be a string")
                        val entry = byName()[toolName]
                            ?: return errorResponse(id, JsonRpcErrorCodes.INVALID_PARAMS, "tools/call: unknown tool '$toolName'")
                        val argsEl = paramsObj["arguments"] ?: buildJsonObject { }
                        val argsJson = Json.encodeToString(JsonElement.serializer(), argsEl)
                        val result = invokeJson(entry.method, argsJson, ctx, codecCtx)
                        when (result) {
                            is Either.Right -> {
                                val content = buildJsonArray {
                                    add(buildJsonObject {
                                        put("type", JsonPrimitive("text"))
                                        put("text", JsonPrimitive(result.value))
                                    })
                                }
                                JsonRpcResponse(id, buildJsonObject {
                                    put("content", content)
                                    put("isError", JsonPrimitive(false))
                                })
                            }
                            is Either.Left -> {
                                // Channel B: a valid protocol call whose domain payload failed.
                                val content = buildJsonArray {
                                    add(buildJsonObject {
                                        put("type", JsonPrimitive("text"))
                                        put("text", JsonPrimitive(describeWiringError(result.value)))
                                    })
                                }
                                JsonRpcResponse(id, buildJsonObject {
                                    put("content", content)
                                    put("isError", JsonPrimitive(true))
                                })
                            }
                        }
                    }
                }
            }
            else -> errorResponse(id, JsonRpcErrorCodes.METHOD_NOT_FOUND, "Method not found: ${request.method}")
        }
    }

    protected fun errorResponse(id: JsonElement?, code: Int, message: String): JsonRpcResponse {
        return JsonRpcResponse(id, null, JsonRpcError(code, message))
    }

    protected open fun describeWiringError(e: BaboonWiringError): String {
        return e.toString()
    }
}
