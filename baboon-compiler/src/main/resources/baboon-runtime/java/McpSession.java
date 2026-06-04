package baboon.runtime.shared;

// Per-connection MCP session state (adapter-owned).
//
// The "initialized" precondition (reject `tools/*` before a successful
// `initialize`) is per-connection state; a connection is a transport concept.
// The latch therefore lives in this tiny value the adapter creates per
// connection, NOT as ambient mutable state inside the server object (which stays
// immutable and shareable across concurrent connections).
public final class McpSession {
    public volatile boolean initialized = false;
}
