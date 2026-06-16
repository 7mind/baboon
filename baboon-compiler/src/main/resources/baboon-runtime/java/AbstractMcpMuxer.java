package baboon.runtime.shared;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.BooleanNode;
import com.fasterxml.jackson.databind.node.ObjectNode;

import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

// Cross-service MCP muxer (tasks:T111; contract:
// docs/research/mcp-muxer-runtime-contract.md).
//
// `AbstractMcpMuxer<Ctx>` composes several `<Service>McpServer<Ctx>` instances
// behind ONE MCP endpoint so a single connection serves the union of their
// tools. It is the MCP-tier sibling of `JsonMuxer`: where `JsonMuxer` keys a
// `LinkedHashMap<serviceName, IBaboonJsonService>` and routes by
// `method.serviceName`, the muxer keys a name→owning-server table and routes
// by the inbound flat MCP tool name (contract §1).
//
// Composition seam: it depends ONLY on the PUBLIC `IBaboonRoutableMcpServer<Ctx>`
// surface (tasks:T114) — it reads each server's `serverInfo()` and `tools()`,
// and routes each `tools/call` via the public `routeToolCall`. It NEVER reads
// protected members and NEVER calls a server's `handle()` (a member's `handle`
// resolves only its OWN tools and returns "unknown tool" for any cross-service
// name — contract §4). The muxer owns the JSON-RPC envelope; each member owns
// its domain dispatch.
//
// SYNC ONLY — Java MCP has no async variant.
//
// The `handle` state machine is the SAME shape as the per-service base, with
// three arms differing only in operating over the union: `tools/list` returns
// the union, `tools/call` routes by tool name to the owning server, and
// `initialize` returns a single merged `serverInfo` supplied to the ctor.
public class AbstractMcpMuxer<Ctx> implements IBaboonMcpServer<Ctx> {

    private static final ObjectMapper MAPPER = new ObjectMapper();

    // Registration-order-preserving tables: built at registration (contract §2),
    // never per request. LinkedHashMap preserves insertion order.
    private final Map<String, IBaboonRoutableMcpServer<Ctx>> route = new LinkedHashMap<>();
    private final Map<String, McpToolEntry>                  entries = new LinkedHashMap<>();
    private final McpServerInfo                              mergedServerInfo;

    // varargs ctor mirrors JsonMuxer(...services). `mergedServerInfo` is the
    // composed endpoint's single identity returned by `initialize` (§3.1).
    @SafeVarargs
    public AbstractMcpMuxer(McpServerInfo mergedServerInfo, IBaboonRoutableMcpServer<Ctx>... servers) {
        this.mergedServerInfo = mergedServerInfo;
        for (IBaboonRoutableMcpServer<Ctx> s : servers) {
            register(s);
        }
    }

    // Folds the server's declaration-ordered tools() into the union table;
    // throws BaboonMcpWiringException(DuplicateTool) on a tool-name collision
    // across servers — the exact MCP-tier analogue of JsonMuxer.register
    // throwing BaboonWiringException(DuplicateService).
    public void register(IBaboonRoutableMcpServer<Ctx> server) {
        for (McpToolEntry t : server.tools()) {
            if (route.containsKey(t.name)) {
                throw new BaboonMcpWiringException(new BaboonMcpWiringError.DuplicateTool(t.name));
            }
            route.put(t.name, server);
            entries.put(t.name, t);
        }
    }

    @Override
    public JsonRpcResponse handle(JsonRpcRequest request, McpSession session, Ctx ctx, BaboonCodecContext codecCtx) {
        JsonNode id = request.id;
        switch (request.method) {
            case "initialize": {
                JsonNode params = request.params;
                JsonNode pv = (params != null && params.isObject()) ? params.get("protocolVersion") : null;
                if (params == null || pv == null) {
                    return errorResponse(id, McpProtocol.INVALID_PARAMS, "initialize: missing protocolVersion");
                }
                session.initialized = true;
                ObjectNode caps = MAPPER.createObjectNode();
                caps.set("tools", MAPPER.createObjectNode());
                ObjectNode serverInfoNode = MAPPER.createObjectNode();
                serverInfoNode.put("name", mergedServerInfo.name);
                serverInfoNode.put("version", mergedServerInfo.version);
                ObjectNode result = MAPPER.createObjectNode();
                result.put("protocolVersion", McpProtocol.VERSION);
                result.set("capabilities", caps);
                result.set("serverInfo", serverInfoNode);
                return JsonRpcResponse.ok(id, result);
            }
            case "notifications/initialized": {
                return null;
            }
            case "tools/list": {
                if (!session.initialized) {
                    return errorResponse(id, McpProtocol.INVALID_REQUEST, "tools/list before initialize");
                }
                ArrayNode toolsArray = MAPPER.createArrayNode();
                for (McpToolEntry t : entries.values()) {
                    ObjectNode entry = MAPPER.createObjectNode();
                    entry.put("name", t.name);
                    entry.set("inputSchema", t.inputSchema);
                    if (t.description != null) {
                        entry.put("description", t.description);
                    }
                    toolsArray.add(entry);
                }
                ObjectNode result = MAPPER.createObjectNode();
                result.set("tools", toolsArray);
                return JsonRpcResponse.ok(id, result);
            }
            case "tools/call": {
                if (!session.initialized) {
                    return errorResponse(id, McpProtocol.INVALID_REQUEST, "tools/call before initialize");
                }
                JsonNode paramsNode = request.params;
                JsonNode nameEl = (paramsNode != null && paramsNode.isObject()) ? paramsNode.get("name") : null;
                if (nameEl == null || nameEl.isNull()) {
                    return errorResponse(id, McpProtocol.INVALID_PARAMS, "tools/call: missing tool name");
                }
                if (!nameEl.isTextual()) {
                    return errorResponse(id, McpProtocol.INVALID_PARAMS, "tools/call: tool name must be a string");
                }
                String toolName = nameEl.textValue();
                IBaboonRoutableMcpServer<Ctx> server = route.get(toolName);
                if (server == null) {
                    // NoMatchingTool: surfaced as the SAME wire response the per-service
                    // base uses for an unknown tool (-32602, "unknown tool '<name>'"),
                    // so the bytes are identical whether one server or the muxer rejects.
                    return errorResponse(id, McpProtocol.INVALID_PARAMS, "tools/call: unknown tool '" + toolName + "'");
                }
                McpToolEntry entry = entries.get(toolName);
                JsonNode argsEl = (paramsNode != null && paramsNode.isObject() && paramsNode.has("arguments"))
                        ? paramsNode.get("arguments")
                        : MAPPER.createObjectNode();
                String argsJson;
                try {
                    argsJson = MAPPER.writeValueAsString(argsEl);
                } catch (Exception e) {
                    return errorResponse(id, McpProtocol.INTERNAL_ERROR, "tools/call: failed to serialize arguments: " + e.getMessage());
                }
                BaboonEither<BaboonWiringError, String> callResult = server.routeToolCall(entry.method, argsJson, ctx, codecCtx);
                if (callResult instanceof BaboonEither.Right<BaboonWiringError, String> right) {
                    ArrayNode content = MAPPER.createArrayNode();
                    ObjectNode textItem = MAPPER.createObjectNode();
                    textItem.put("type", "text");
                    textItem.put("text", right.value());
                    content.add(textItem);
                    ObjectNode result = MAPPER.createObjectNode();
                    result.set("content", content);
                    result.set("isError", BooleanNode.FALSE);
                    return JsonRpcResponse.ok(id, result);
                } else {
                    @SuppressWarnings("unchecked")
                    BaboonEither.Left<BaboonWiringError, String> left = (BaboonEither.Left<BaboonWiringError, String>) callResult;
                    // Channel B: a valid protocol call whose domain payload failed.
                    ArrayNode content = MAPPER.createArrayNode();
                    ObjectNode textItem = MAPPER.createObjectNode();
                    textItem.put("type", "text");
                    textItem.put("text", describeWiringError(left.value()));
                    content.add(textItem);
                    ObjectNode result = MAPPER.createObjectNode();
                    result.set("content", content);
                    result.set("isError", BooleanNode.TRUE);
                    return JsonRpcResponse.ok(id, result);
                }
            }
            default: {
                return errorResponse(id, McpProtocol.METHOD_NOT_FOUND, "Method not found: " + request.method);
            }
        }
    }

    // Ordered list of all registered tools in registration-then-declaration order.
    public List<McpToolEntry> tools() {
        return Collections.unmodifiableList(new ArrayList<>(entries.values()));
    }

    protected JsonRpcResponse errorResponse(JsonNode id, int code, String message) {
        return JsonRpcResponse.err(id, new JsonRpcError(code, message));
    }

    protected String describeWiringError(BaboonWiringError e) {
        return e.toString();
    }
}
