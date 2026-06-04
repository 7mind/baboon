package baboon.runtime.shared;

import com.fasterxml.jackson.databind.JsonNode;

// JSON-RPC response value type (MCP transport contract).
public final class JsonRpcResponse {
    public final JsonNode id;
    public final JsonNode result;
    public final JsonRpcError error;

    public JsonRpcResponse(JsonNode id, JsonNode result, JsonRpcError error) {
        this.id = id;
        this.result = result;
        this.error = error;
    }

    public static JsonRpcResponse ok(JsonNode id, JsonNode result) {
        return new JsonRpcResponse(id, result, null);
    }

    public static JsonRpcResponse err(JsonNode id, JsonRpcError error) {
        return new JsonRpcResponse(id, null, error);
    }
}
