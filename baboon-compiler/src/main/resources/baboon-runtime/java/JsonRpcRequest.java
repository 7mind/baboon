package baboon.runtime.shared;

import com.fasterxml.jackson.databind.JsonNode;

// JSON-RPC request value type (MCP transport contract).
// A notification carries no `id`; the server produces no response for it.
public final class JsonRpcRequest {
    public final JsonNode id;
    public final String method;
    public final JsonNode params;

    public JsonRpcRequest(JsonNode id, String method, JsonNode params) {
        this.id = id;
        this.method = method;
        this.params = params;
    }
}
