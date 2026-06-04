package baboon.runtime.shared;

// MCP server identity (name + model version). Returned in the `initialize` response.
public final class McpServerInfo {
    public final String name;
    public final String version;

    public McpServerInfo(String name, String version) {
        this.name = name;
        this.version = version;
    }
}
