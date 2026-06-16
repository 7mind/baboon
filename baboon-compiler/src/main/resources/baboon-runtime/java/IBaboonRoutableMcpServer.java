package baboon.runtime.shared;

import java.util.List;

// PUBLIC routable-server surface (tasks:T114) — the composition seam the
// cross-service MCP muxer (AbstractMcpMuxer) depends on.
//
// A sibling muxer needs to (1) read each server's identity, (2) read its
// declaration-ordered tool registry to build the union tools/list and the
// tool-name -> owner table, and (3) route a single tools/call into the owning
// server reusing its existing Channel-A/Channel-B mapping unchanged. In the
// inheritance-based base those three inputs were `protected`/`private`, so a
// sibling could not compose them portably. This interface promotes exactly
// those three inputs to a stable PUBLIC surface; the muxer depends on the
// interface, not on concrete `<Service>McpServer` subclasses.
//
// `routeToolCall` is the public name for the per-service routable dispatch
// entry: it is the SAME code path `handle()` already drives for its own
// `tools/call` arm (`invokeJson`), exposed so the muxer reuses Channel-A/
// Channel-B mapping verbatim rather than reimplementing it. The muxer NEVER
// calls a server's `handle()`. `handle()` itself is unchanged.
public interface IBaboonRoutableMcpServer<Ctx> {
    McpServerInfo serverInfo();

    List<McpToolEntry> tools();

    BaboonEither<BaboonWiringError, String> routeToolCall(
            BaboonMethodId method, String data, Ctx ctx, BaboonCodecContext codecCtx);
}
