package baboon.runtime.shared;

// JSON-RPC / MCP protocol constants (wire contract K4).
public final class McpProtocol {
    private McpProtocol() {}
    public static final String VERSION = "2025-06-18";
    public static final int PARSE_ERROR = -32700;
    public static final int INVALID_REQUEST = -32600;
    public static final int METHOD_NOT_FOUND = -32601;
    public static final int INVALID_PARAMS = -32602;
    public static final int INTERNAL_ERROR = -32603;
}
