package baboon.runtime.shared;

// Transport-abstract MCP server dispatch interface.
//
// The single generated entrypoint, analogous to `IBaboonJsonServiceCtx<Ctx,R>`.
// It is NOT R-parametric (an MCP response is always a `JsonRpcResponse` value);
// the only free type parameter is the caller's `Ctx` — the SAME `Ctx` the
// service-wiring contract threads. `handle` is synchronous and performs no I/O.
// It returns `null` for an accepted notification (no reply).
public interface IBaboonMcpServer<Ctx> {
    JsonRpcResponse handle(JsonRpcRequest request, McpSession session, Ctx ctx, BaboonCodecContext codecCtx);
}
