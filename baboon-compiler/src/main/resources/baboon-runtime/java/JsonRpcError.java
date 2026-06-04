package baboon.runtime.shared;

import com.fasterxml.jackson.databind.JsonNode;

// JSON-RPC error value type (MCP transport contract).
public final class JsonRpcError {
    public final int code;
    public final String message;
    public final JsonNode data;

    public JsonRpcError(int code, String message, JsonNode data) {
        this.code = code;
        this.message = message;
        this.data = data;
    }

    public JsonRpcError(int code, String message) {
        this(code, message, null);
    }
}
