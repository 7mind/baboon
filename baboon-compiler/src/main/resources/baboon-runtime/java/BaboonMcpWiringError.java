package baboon.runtime.shared;

// MCP-muxer error taxonomy (tasks:T111; contract §6).
//
// The cross-service MCP muxer composes several <Service>McpServer instances
// behind one endpoint. `DuplicateTool` is the MCP-tier analogue of the
// service-wiring `DuplicateService` (a registration-time tool-name collision);
// `NoMatchingTool` is the analogue of `NoMatchingService` (a dispatch-time
// unknown tool name). They mirror that taxonomy and carrier exactly, but live in
// this MCP-only runtime file rather than the always-shipped service-wiring
// runtime so that with `--jv-generate-mcp-server` absent the generated output
// is byte-identical to the pre-MCP baseline (no MCP types leak into the
// unconditional runtime).
public sealed interface BaboonMcpWiringError {
    record DuplicateTool(String toolName) implements BaboonMcpWiringError {}
    record NoMatchingTool(String toolName) implements BaboonMcpWiringError {}
}
