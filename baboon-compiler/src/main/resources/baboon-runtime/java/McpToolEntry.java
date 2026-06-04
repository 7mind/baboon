package baboon.runtime.shared;

import com.fasterxml.jackson.databind.JsonNode;

// One entry per Baboon method bound to a server. `inputSchema` is the
// precomputed, self-contained JSON Schema (from the shared T5 emitter), carried
// as a constant value — the runtime does not compute schemas.
public final class McpToolEntry {
    public final String name;
    public final BaboonMethodId method;
    public final JsonNode inputSchema;
    public final String description;

    public McpToolEntry(String name, BaboonMethodId method, JsonNode inputSchema, String description) {
        this.name = name;
        this.method = method;
        this.inputSchema = inputSchema;
        this.description = description;
    }

    public McpToolEntry(String name, BaboonMethodId method, JsonNode inputSchema) {
        this(name, method, inputSchema, null);
    }
}
