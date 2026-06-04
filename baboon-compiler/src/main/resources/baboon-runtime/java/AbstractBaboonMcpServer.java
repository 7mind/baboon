package baboon.runtime.shared;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.BooleanNode;
import com.fasterxml.jackson.databind.node.ObjectNode;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

// Transport-abstract dispatch base.
//
// Shared `handle` state machine. The generated `<Service>McpServer` extends this
// with its fixed `serverInfo`, ordered tool registry, and `invokeJson` delegate.
// All JSON-RPC method strings ("tools/list" ...) and result keys
// ("protocolVersion", "inputSchema" ...) are literal lowercase strings, NOT
// subject to any per-language symbol casing.
public abstract class AbstractBaboonMcpServer<Ctx> implements IBaboonMcpServer<Ctx> {
    protected static final ObjectMapper MAPPER = new ObjectMapper();

    protected abstract McpServerInfo serverInfo();
    protected abstract List<McpToolEntry> tools();
    protected abstract BaboonEither<BaboonWiringError, String> invokeJson(
            BaboonMethodId method, String data, Ctx ctx, BaboonCodecContext codecCtx);

    private Map<String, McpToolEntry> byName() {
        Map<String, McpToolEntry> m = new HashMap<>();
        for (McpToolEntry t : tools()) {
            m.put(t.name, t);
        }
        return Collections.unmodifiableMap(m);
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
                serverInfoNode.put("name", serverInfo().name);
                serverInfoNode.put("version", serverInfo().version);
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
                for (McpToolEntry t : tools()) {
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
                McpToolEntry entry = byName().get(toolName);
                if (entry == null) {
                    return errorResponse(id, McpProtocol.INVALID_PARAMS, "tools/call: unknown tool '" + toolName + "'");
                }
                JsonNode argsEl = (paramsNode != null && paramsNode.isObject() && paramsNode.has("arguments"))
                        ? paramsNode.get("arguments")
                        : MAPPER.createObjectNode();
                String argsJson;
                try {
                    argsJson = MAPPER.writeValueAsString(argsEl);
                } catch (Exception e) {
                    return errorResponse(id, McpProtocol.INTERNAL_ERROR, "tools/call: failed to serialize arguments: " + e.getMessage());
                }
                BaboonEither<BaboonWiringError, String> callResult = invokeJson(entry.method, argsJson, ctx, codecCtx);
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

    protected JsonRpcResponse errorResponse(JsonNode id, int code, String message) {
        return JsonRpcResponse.err(id, new JsonRpcError(code, message));
    }

    protected String describeWiringError(BaboonWiringError e) {
        return e.toString();
    }
}
