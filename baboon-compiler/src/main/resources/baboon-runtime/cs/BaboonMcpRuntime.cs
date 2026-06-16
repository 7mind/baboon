#nullable enable

// Additive MCP server runtime (decisions ledger M1; contract:
// docs/research/mcp-dispatch-runtime-contract.md). Emitted only when
// `--cs-generate-mcp-server=true`; the service-wiring runtime in
// BaboonServiceWiring.cs is unchanged. These types are STATIC (no per-model
// templating) — the only per-model code is the generated `<Service>McpServer`
// and its tool-registry literals.
//
// The MCP dispatch surface is transport-abstract: it implements the JSON-RPC
// method state machine + tool dispatch ONLY. Bytes-in/bytes-out (stdio framing,
// Streamable-HTTP bodies) is an injected adapter the generated surface never
// contains — mirroring the abstract-context service contract, which supplies
// `Ctx` per invocation rather than baking an I/O loop into the wrapper.

using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using Newtonsoft.Json.Linq;

// ReSharper disable UnusedTypeParameter
// ReSharper disable CheckNamespace
// ReSharper disable UnusedAutoPropertyAccessor.Global
// ReSharper disable MemberCanBePrivate.Global
// ReSharper disable UnusedMember.Global
// ReSharper disable UnusedType.Global
// ReSharper disable InconsistentNaming
// ReSharper disable ClassCanBeSealed.Global
// ReSharper disable ConvertToPrimaryConstructor
// ReSharper disable MemberCanBeProtected.Global
// ReSharper disable ArrangeNamespaceBody

namespace Baboon.Runtime.Shared
{
    // --- JSON-RPC value types (already parsed from bytes by the adapter) ---
    //
    // A notification carries no `Id`; the server produces no response for it.
    // `Params`/`Result`/`Error.Data` are the language's existing JSON value
    // type (C#: Newtonsoft `JToken`) — the MCP runtime reuses what the JSON
    // codecs already speak rather than introducing a new model.

    public sealed record JsonRpcRequest(JToken? Id, string Method, JToken? Params);

    public sealed record JsonRpcError(int Code, string Message, JToken? Data = null);

    public sealed record JsonRpcResponse(JToken? Id, JToken? Result = null, JsonRpcError? Error = null);

    // JSON-RPC / MCP protocol constants (wire contract K4).
    public static class McpProtocol
    {
        public const string Version = "2025-06-18";
    }

    public static class JsonRpcErrorCodes
    {
        public const int ParseError = -32700;
        public const int InvalidRequest = -32600;
        public const int MethodNotFound = -32601;
        public const int InvalidParams = -32602;
        public const int InternalError = -32603;
    }

    // --- Per-connection state (adapter-owned) ---
    //
    // The "initialized" precondition (reject `tools/*` before a successful
    // `initialize`) is per-connection state; a connection is a transport
    // concept. The latch therefore lives in this tiny value the adapter creates
    // per connection, NOT as ambient mutable state inside the server object
    // (which stays immutable and shareable across concurrent connections).
    public sealed class McpSession
    {
        public bool Initialized { get; set; }
    }

    // --- Tool registry ---
    //
    // One entry per Baboon method bound to a server. `InputSchema` is the
    // precomputed, self-contained JSON Schema (from the shared T5 emitter),
    // carried as a constant value — the runtime does not compute schemas.
    public sealed record McpToolEntry(string Name, BaboonMethodId Method, JToken InputSchema, string? Description = null);

    public sealed record McpServerInfo(string Name, string Version);

    // --- Dispatch interface ---
    //
    // The single generated entrypoint, analogous to `IBaboonJsonServiceCtx<Ctx,R>`.
    // It is NOT R-parametric (an MCP response is always a `JsonRpcResponse`
    // value); the only free type parameter is the caller's `Ctx` — the SAME
    // `Ctx` the service-wiring contract threads. `Handle` is synchronous and
    // performs no I/O. It returns `null` for an accepted notification (no reply).
    public interface IBaboonMcpServer<in Ctx>
    {
        JsonRpcResponse? Handle(JsonRpcRequest request, McpSession session, Ctx ctx, BaboonCodecContext codecCtx);
    }

    // Async sibling of `IBaboonMcpServer`, used when the backend is generated with
    // `--cs-async-services=true`. `Handle` returns `Task<JsonRpcResponse?>` because
    // the `tools/call` dispatch must `await` the async `InvokeJson` delegate.
    public interface IBaboonMcpServerAsync<in Ctx>
    {
        Task<JsonRpcResponse?> Handle(JsonRpcRequest request, McpSession session, Ctx ctx, BaboonCodecContext codecCtx);
    }

    // --- PUBLIC routable-server surface (tasks:T114) ---
    //
    // The composition seam the cross-service MCP muxer (AbstractMcpMuxer) depends
    // on. A sibling muxer reads each server's identity (ServerInfo) and its
    // declaration-ordered registry (Tools) to build the union tools/list and the
    // tool-name -> owner table, and routes a single tools/call into the owning
    // server via RouteToolCall — reusing its existing Channel-A/Channel-B mapping
    // unchanged. Those inputs were `protected` on the base, so a sibling could
    // not compose them; this interface promotes exactly them to a stable PUBLIC
    // surface. The muxer depends on the interface, NEVER on `Handle`.
    //
    // `RouteToolCall` is the public name for the per-service dispatch entry that
    // `Handle` already drives for its own `tools/call` arm (`InvokeJson`).
    public interface IBaboonRoutableMcpServer<in Ctx>
    {
        McpServerInfo ServerInfo { get; }
        IReadOnlyList<McpToolEntry> Tools { get; }
        Either<BaboonWiringError, string> RouteToolCall(BaboonMethodId method, string data, Ctx ctx, BaboonCodecContext codecCtx);
    }

    // Async sibling of `IBaboonRoutableMcpServer`, for `--cs-async-services=true`:
    // `RouteToolCall` returns `Task<Either<..>>`.
    public interface IBaboonRoutableMcpServerAsync<in Ctx>
    {
        McpServerInfo ServerInfo { get; }
        IReadOnlyList<McpToolEntry> Tools { get; }
        Task<Either<BaboonWiringError, string>> RouteToolCall(BaboonMethodId method, string data, Ctx ctx, BaboonCodecContext codecCtx);
    }

    // The JSON `tools/call` delegate the generated server supplies: it routes
    // one tool invocation into the already-generated service dispatch (the
    // errors-mode `InvokeJson`, which returns the service-result container). The
    // MCP layer turns that `Either<BaboonWiringError, string>` into Channel-A /
    // Channel-B per the wire contract (K4 §3). The codecs are reached
    // exclusively through this delegate; the MCP runtime holds no codec logic
    // itself.
    public delegate Either<BaboonWiringError, string> McpJsonInvoke<in Ctx>(
        BaboonMethodId method,
        string data,
        Ctx ctx,
        BaboonCodecContext codecCtx);

    // Async sibling of `McpJsonInvoke`, selected when the C# backend is generated
    // with `--cs-async-services=true`. The generated errors-mode wiring entry
    // (`<Service>Wiring.InvokeJson`) then returns `Task<Either<..>>`, which binds
    // to this delegate directly. The SYNC `McpJsonInvoke` above is left untouched
    // so the non-async generated MCP server stays byte-identical.
    public delegate Task<Either<BaboonWiringError, string>> McpJsonInvokeAsync<in Ctx>(
        BaboonMethodId method,
        string data,
        Ctx ctx,
        BaboonCodecContext codecCtx);

    // --- Transport-abstract dispatch base ---
    //
    // Shared `Handle` state machine. The generated `<Service>McpServer` extends
    // this with its fixed `ServerInfo`, ordered tool registry, and `InvokeJson`
    // delegate. All JSON-RPC method strings ("tools/list" …) and result keys
    // ("protocolVersion", "inputSchema" …) are literal lowercase strings, NOT
    // subject to any per-language symbol casing (the PascalCase convention
    // applies only to internal C# symbols, never to the wire).
    public abstract class AbstractBaboonMcpServer<Ctx> : IBaboonMcpServer<Ctx>, IBaboonRoutableMcpServer<Ctx>
    {
        // PUBLIC routable-server surface (tasks:T114): the muxer reads ServerInfo
        // / Tools and routes via RouteToolCall, never via the private ByName()
        // and never via Handle.
        public abstract McpServerInfo ServerInfo { get; }
        public abstract IReadOnlyList<McpToolEntry> Tools { get; }
        protected abstract Either<BaboonWiringError, string> InvokeJson(BaboonMethodId method, string data, Ctx ctx, BaboonCodecContext codecCtx);

        // PUBLIC dispatch entry (tasks:T114): the same path Handle drives for its
        // own tools/call arm, exposed for the muxer to reuse Channel-A/B unchanged.
        public Either<BaboonWiringError, string> RouteToolCall(BaboonMethodId method, string data, Ctx ctx, BaboonCodecContext codecCtx)
        {
            return InvokeJson(method, data, ctx, codecCtx);
        }

        private Dictionary<string, McpToolEntry> ByName()
        {
            var m = new Dictionary<string, McpToolEntry>();
            foreach (var t in Tools)
            {
                m[t.Name] = t;
            }
            return m;
        }

        public JsonRpcResponse? Handle(JsonRpcRequest request, McpSession session, Ctx ctx, BaboonCodecContext codecCtx)
        {
            var id = request.Id;
            switch (request.Method)
            {
                case "initialize":
                {
                    var pv = request.Params?["protocolVersion"];
                    if (request.Params == null || pv == null)
                    {
                        return ErrorResponse(id, JsonRpcErrorCodes.InvalidParams, "initialize: missing protocolVersion");
                    }
                    session.Initialized = true;
                    var result = new JObject
                    {
                        ["protocolVersion"] = McpProtocol.Version,
                        ["capabilities"] = new JObject { ["tools"] = new JObject() },
                        ["serverInfo"] = new JObject { ["name"] = ServerInfo.Name, ["version"] = ServerInfo.Version },
                    };
                    return new JsonRpcResponse(id, result);
                }
                case "notifications/initialized":
                    return null;
                case "tools/list":
                {
                    if (!session.Initialized)
                    {
                        return ErrorResponse(id, JsonRpcErrorCodes.InvalidRequest, "tools/list before initialize");
                    }
                    var tools = new JArray();
                    foreach (var t in Tools)
                    {
                        var entry = new JObject
                        {
                            ["name"] = t.Name,
                            ["inputSchema"] = t.InputSchema,
                        };
                        if (t.Description != null)
                        {
                            entry["description"] = t.Description;
                        }
                        tools.Add(entry);
                    }
                    return new JsonRpcResponse(id, new JObject { ["tools"] = tools });
                }
                case "tools/call":
                {
                    if (!session.Initialized)
                    {
                        return ErrorResponse(id, JsonRpcErrorCodes.InvalidRequest, "tools/call before initialize");
                    }
                    var name = request.Params?["name"];
                    if (name == null || name.Type != JTokenType.String)
                    {
                        return ErrorResponse(id, JsonRpcErrorCodes.InvalidParams, "tools/call: missing tool name");
                    }
                    var toolName = name.Value<string>()!;
                    if (!ByName().TryGetValue(toolName, out var entry))
                    {
                        return ErrorResponse(id, JsonRpcErrorCodes.InvalidParams, $"tools/call: unknown tool '{toolName}'");
                    }
                    var argsToken = request.Params?["arguments"] ?? new JObject();
                    var argsJson = argsToken.ToString(Newtonsoft.Json.Formatting.None);
                    var result = InvokeJson(entry.Method, argsJson, ctx, codecCtx);
                    if (result.IsRight)
                    {
                        var content = new JArray { new JObject { ["type"] = "text", ["text"] = result.GetRight() } };
                        return new JsonRpcResponse(id, new JObject { ["content"] = content, ["isError"] = false });
                    }
                    else
                    {
                        // Channel B: a valid protocol call whose domain payload failed.
                        var content = new JArray { new JObject { ["type"] = "text", ["text"] = DescribeWiringError(result.GetLeft()) } };
                        return new JsonRpcResponse(id, new JObject { ["content"] = content, ["isError"] = true });
                    }
                }
                default:
                    return ErrorResponse(id, JsonRpcErrorCodes.MethodNotFound, $"Method not found: {request.Method}");
            }
        }

        protected JsonRpcResponse ErrorResponse(JToken? id, int code, string message)
        {
            return new JsonRpcResponse(id, null, new JsonRpcError(code, message));
        }

        protected virtual string DescribeWiringError(BaboonWiringError e)
        {
            return e.ToString();
        }
    }

    // --- MCP-muxer error taxonomy (tasks:T106; contract §6) ---
    //
    // The cross-service MCP muxer composes several <Service>McpServer instances
    // behind one endpoint. `DuplicateTool` is the MCP-tier analogue of the
    // service-wiring `DuplicateService` (a registration-time tool-name collision);
    // `NoMatchingTool` is the analogue of `NoMatchingService` (a dispatch-time
    // unknown tool name). They mirror that taxonomy and carrier exactly, but live in
    // this MCP-only runtime file rather than the always-shipped service-wiring
    // runtime (BaboonServiceWiring.cs) so that with `--cs-generate-mcp-server`
    // absent the generated output is byte-identical to the pre-MCP baseline (no MCP
    // types leak into the unconditional runtime). `BaboonMcpWiringException` is the
    // MCP-tier carrier — the structural twin of `BaboonWiringException` (a thrown
    // programmer error), so `DuplicateTool` propagates to the integrator exactly as
    // `DuplicateService` does, just from the MCP-only runtime.
    public enum BaboonMcpWiringErrorTag
    {
        DuplicateTool,
        NoMatchingTool,
    }

    public sealed record BaboonMcpWiringError(BaboonMcpWiringErrorTag Tag, string ToolName);

    public sealed class BaboonMcpWiringException : Exception
    {
        public BaboonMcpWiringError Error { get; }
        public BaboonMcpWiringException(BaboonMcpWiringError error)
            : base($"{{\"tag\":\"{error.Tag}\",\"toolName\":\"{error.ToolName}\"}}")
        {
            Error = error;
        }
    }

    // --- Cross-service MCP muxer (tasks:T106; contract:
    // docs/research/mcp-muxer-runtime-contract.md) ---
    //
    // `AbstractMcpMuxer<Ctx>` composes several `<Service>McpServer<Ctx>` instances
    // behind ONE MCP endpoint so a single connection serves the union of their
    // tools. It is the MCP-tier sibling of `JsonMuxer`: where `JsonMuxer` keys
    // by service name, the muxer keys `Dictionary<toolName, owningServer>` and
    // routes by the inbound flat MCP tool name (contract §1).
    //
    // Composition seam: it depends ONLY on the PUBLIC `IBaboonRoutableMcpServer<Ctx>`
    // surface (tasks:T114) — it reads each server's `ServerInfo` and `Tools`, and
    // routes each `tools/call` via the public `RouteToolCall`. It NEVER reads
    // protected members and NEVER calls a server's `Handle` (a member's Handle
    // resolves only its OWN tools and returns "unknown tool" for any cross-service
    // name — contract §4). The muxer owns the JSON-RPC envelope; each member owns
    // its domain dispatch.
    //
    // The `Handle` state machine is the SAME shape as the per-service base, with
    // three arms differing only in operating over the union: `tools/list` returns
    // the union, `tools/call` routes by tool name to the owning server, and
    // `initialize` returns a single merged `ServerInfo` supplied to the ctor.
    public class AbstractMcpMuxer<Ctx> : IBaboonMcpServer<Ctx>
    {
        // Registration order preserved (insertion-ordered Dictionary / list —
        // JsonMuxer LinkedHashMap precedent). `_route`/`_entries` are built at
        // registration (contract §2), never per request.
        private readonly List<IBaboonRoutableMcpServer<Ctx>> _servers = new List<IBaboonRoutableMcpServer<Ctx>>();
        private readonly Dictionary<string, IBaboonRoutableMcpServer<Ctx>> _route = new Dictionary<string, IBaboonRoutableMcpServer<Ctx>>();
        private readonly Dictionary<string, McpToolEntry> _entries = new Dictionary<string, McpToolEntry>();
        // Insertion-ordered tool-name list for tools/list iteration.
        private readonly List<string> _toolOrder = new List<string>();
        private readonly McpServerInfo _mergedServerInfo;

        // varargs-style ctor mirrors `JsonMuxer`. `mergedServerInfo` is the composed
        // endpoint's single identity returned by `initialize` (§3.1).
        public AbstractMcpMuxer(McpServerInfo mergedServerInfo, params IBaboonRoutableMcpServer<Ctx>[] servers)
        {
            _mergedServerInfo = mergedServerInfo;
            foreach (var s in servers) Register(s);
        }

        // Folds the server's declaration-ordered `Tools` into the union table;
        // throws DuplicateTool on a tool-name collision across servers (the exact
        // MCP-tier analogue of JsonMuxer throwing DuplicateService).
        public void Register(IBaboonRoutableMcpServer<Ctx> server)
        {
            foreach (var t in server.Tools)
            {
                if (_route.ContainsKey(t.Name))
                {
                    throw new BaboonMcpWiringException(new BaboonMcpWiringError(BaboonMcpWiringErrorTag.DuplicateTool, t.Name));
                }
                _route[t.Name] = server;
                _entries[t.Name] = t;
                _toolOrder.Add(t.Name);
            }
            _servers.Add(server);
        }

        public JsonRpcResponse? Handle(JsonRpcRequest request, McpSession session, Ctx ctx, BaboonCodecContext codecCtx)
        {
            var id = request.Id;
            switch (request.Method)
            {
                case "initialize":
                {
                    var pv = request.Params?["protocolVersion"];
                    if (request.Params == null || pv == null)
                    {
                        return ErrorResponse(id, JsonRpcErrorCodes.InvalidParams, "initialize: missing protocolVersion");
                    }
                    session.Initialized = true;
                    var result = new Newtonsoft.Json.Linq.JObject
                    {
                        ["protocolVersion"] = McpProtocol.Version,
                        ["capabilities"] = new Newtonsoft.Json.Linq.JObject { ["tools"] = new Newtonsoft.Json.Linq.JObject() },
                        ["serverInfo"] = new Newtonsoft.Json.Linq.JObject { ["name"] = _mergedServerInfo.Name, ["version"] = _mergedServerInfo.Version },
                    };
                    return new JsonRpcResponse(id, result);
                }
                case "notifications/initialized":
                    return null;
                case "tools/list":
                {
                    if (!session.Initialized)
                    {
                        return ErrorResponse(id, JsonRpcErrorCodes.InvalidRequest, "tools/list before initialize");
                    }
                    return new JsonRpcResponse(id, new Newtonsoft.Json.Linq.JObject { ["tools"] = ToolsListUnion() });
                }
                case "tools/call":
                {
                    if (!session.Initialized)
                    {
                        return ErrorResponse(id, JsonRpcErrorCodes.InvalidRequest, "tools/call before initialize");
                    }
                    var name = request.Params?["name"];
                    if (name == null || name.Type != Newtonsoft.Json.Linq.JTokenType.String)
                    {
                        return ErrorResponse(id, JsonRpcErrorCodes.InvalidParams, "tools/call: missing tool name");
                    }
                    var toolName = name.Value<string>()!;
                    if (!_route.TryGetValue(toolName, out var server))
                    {
                        // NoMatchingTool: surfaced as the SAME wire response the per-service
                        // base uses for an unknown tool (-32602, "unknown tool '<name>'"),
                        // so the bytes are identical whether one server or the muxer rejects.
                        return ErrorResponse(id, JsonRpcErrorCodes.InvalidParams, $"tools/call: unknown tool '{toolName}'");
                    }
                    var entry = _entries[toolName];
                    var argsToken = request.Params?["arguments"] ?? new Newtonsoft.Json.Linq.JObject();
                    var argsJson = argsToken.ToString(Newtonsoft.Json.Formatting.None);
                    var result = server.RouteToolCall(entry.Method, argsJson, ctx, codecCtx);
                    if (result.IsRight)
                    {
                        var content = new Newtonsoft.Json.Linq.JArray { new Newtonsoft.Json.Linq.JObject { ["type"] = "text", ["text"] = result.GetRight() } };
                        return new JsonRpcResponse(id, new Newtonsoft.Json.Linq.JObject { ["content"] = content, ["isError"] = false });
                    }
                    else
                    {
                        // Channel B: a valid protocol call whose domain payload failed.
                        var content = new Newtonsoft.Json.Linq.JArray { new Newtonsoft.Json.Linq.JObject { ["type"] = "text", ["text"] = DescribeWiringError(result.GetLeft()) } };
                        return new JsonRpcResponse(id, new Newtonsoft.Json.Linq.JObject { ["content"] = content, ["isError"] = true });
                    }
                }
                default:
                    return ErrorResponse(id, JsonRpcErrorCodes.MethodNotFound, $"Method not found: {request.Method}");
            }
        }

        // Backs tools/list (§3.2): the union of all registered servers' tool entries
        // in registration-then-declaration order (insertion order of `_toolOrder`),
        // each in the same shape the per-service base emits.
        private Newtonsoft.Json.Linq.JArray ToolsListUnion()
        {
            var tools = new Newtonsoft.Json.Linq.JArray();
            foreach (var name in _toolOrder)
            {
                var t = _entries[name];
                var entry = new Newtonsoft.Json.Linq.JObject
                {
                    ["name"] = t.Name,
                    ["inputSchema"] = t.InputSchema,
                };
                if (t.Description != null)
                {
                    entry["description"] = t.Description;
                }
                tools.Add(entry);
            }
            return tools;
        }

        protected JsonRpcResponse ErrorResponse(Newtonsoft.Json.Linq.JToken? id, int code, string message)
        {
            return new JsonRpcResponse(id, null, new JsonRpcError(code, message));
        }

        protected virtual string DescribeWiringError(BaboonWiringError e)
        {
            return e.ToString();
        }
    }

    // --- Async cross-service MCP muxer (tasks:T106; contract §7) ---
    //
    // Async sibling of `AbstractMcpMuxer`, for backends generated with
    // `--cs-async-services=true`: it composes `IBaboonRoutableMcpServerAsync<Ctx>`
    // members (whose `RouteToolCall` returns `Task<Either<..>>`), so the single
    // `tools/call` dispatch hop is awaited and `Handle` is itself `async Task<>`.
    // The registration / union-table build (`DuplicateTool` on collision), the
    // merged `initialize`, the ordering rule, and the `NoMatchingTool` wire mapping
    // are identical to the sync muxer; only the `tools/call` hop awaits.
    public class AbstractAsyncMcpMuxer<Ctx> : IBaboonMcpServerAsync<Ctx>
    {
        private readonly List<IBaboonRoutableMcpServerAsync<Ctx>> _servers = new List<IBaboonRoutableMcpServerAsync<Ctx>>();
        private readonly Dictionary<string, IBaboonRoutableMcpServerAsync<Ctx>> _route = new Dictionary<string, IBaboonRoutableMcpServerAsync<Ctx>>();
        private readonly Dictionary<string, McpToolEntry> _entries = new Dictionary<string, McpToolEntry>();
        private readonly List<string> _toolOrder = new List<string>();
        private readonly McpServerInfo _mergedServerInfo;

        public AbstractAsyncMcpMuxer(McpServerInfo mergedServerInfo, params IBaboonRoutableMcpServerAsync<Ctx>[] servers)
        {
            _mergedServerInfo = mergedServerInfo;
            foreach (var s in servers) Register(s);
        }

        public void Register(IBaboonRoutableMcpServerAsync<Ctx> server)
        {
            foreach (var t in server.Tools)
            {
                if (_route.ContainsKey(t.Name))
                {
                    throw new BaboonMcpWiringException(new BaboonMcpWiringError(BaboonMcpWiringErrorTag.DuplicateTool, t.Name));
                }
                _route[t.Name] = server;
                _entries[t.Name] = t;
                _toolOrder.Add(t.Name);
            }
            _servers.Add(server);
        }

        public async Task<JsonRpcResponse?> Handle(JsonRpcRequest request, McpSession session, Ctx ctx, BaboonCodecContext codecCtx)
        {
            var id = request.Id;
            switch (request.Method)
            {
                case "initialize":
                {
                    var pv = request.Params?["protocolVersion"];
                    if (request.Params == null || pv == null)
                    {
                        return ErrorResponse(id, JsonRpcErrorCodes.InvalidParams, "initialize: missing protocolVersion");
                    }
                    session.Initialized = true;
                    var result = new Newtonsoft.Json.Linq.JObject
                    {
                        ["protocolVersion"] = McpProtocol.Version,
                        ["capabilities"] = new Newtonsoft.Json.Linq.JObject { ["tools"] = new Newtonsoft.Json.Linq.JObject() },
                        ["serverInfo"] = new Newtonsoft.Json.Linq.JObject { ["name"] = _mergedServerInfo.Name, ["version"] = _mergedServerInfo.Version },
                    };
                    return new JsonRpcResponse(id, result);
                }
                case "notifications/initialized":
                    return null;
                case "tools/list":
                {
                    if (!session.Initialized)
                    {
                        return ErrorResponse(id, JsonRpcErrorCodes.InvalidRequest, "tools/list before initialize");
                    }
                    return new JsonRpcResponse(id, new Newtonsoft.Json.Linq.JObject { ["tools"] = ToolsListUnion() });
                }
                case "tools/call":
                {
                    if (!session.Initialized)
                    {
                        return ErrorResponse(id, JsonRpcErrorCodes.InvalidRequest, "tools/call before initialize");
                    }
                    var name = request.Params?["name"];
                    if (name == null || name.Type != Newtonsoft.Json.Linq.JTokenType.String)
                    {
                        return ErrorResponse(id, JsonRpcErrorCodes.InvalidParams, "tools/call: missing tool name");
                    }
                    var toolName = name.Value<string>()!;
                    if (!_route.TryGetValue(toolName, out var server))
                    {
                        return ErrorResponse(id, JsonRpcErrorCodes.InvalidParams, $"tools/call: unknown tool '{toolName}'");
                    }
                    var entry = _entries[toolName];
                    var argsToken = request.Params?["arguments"] ?? new Newtonsoft.Json.Linq.JObject();
                    var argsJson = argsToken.ToString(Newtonsoft.Json.Formatting.None);
                    var result = await server.RouteToolCall(entry.Method, argsJson, ctx, codecCtx);
                    if (result.IsRight)
                    {
                        var content = new Newtonsoft.Json.Linq.JArray { new Newtonsoft.Json.Linq.JObject { ["type"] = "text", ["text"] = result.GetRight() } };
                        return new JsonRpcResponse(id, new Newtonsoft.Json.Linq.JObject { ["content"] = content, ["isError"] = false });
                    }
                    else
                    {
                        // Channel B: a valid protocol call whose domain payload failed.
                        var content = new Newtonsoft.Json.Linq.JArray { new Newtonsoft.Json.Linq.JObject { ["type"] = "text", ["text"] = DescribeWiringError(result.GetLeft()) } };
                        return new JsonRpcResponse(id, new Newtonsoft.Json.Linq.JObject { ["content"] = content, ["isError"] = true });
                    }
                }
                default:
                    return ErrorResponse(id, JsonRpcErrorCodes.MethodNotFound, $"Method not found: {request.Method}");
            }
        }

        private Newtonsoft.Json.Linq.JArray ToolsListUnion()
        {
            var tools = new Newtonsoft.Json.Linq.JArray();
            foreach (var name in _toolOrder)
            {
                var t = _entries[name];
                var entry = new Newtonsoft.Json.Linq.JObject
                {
                    ["name"] = t.Name,
                    ["inputSchema"] = t.InputSchema,
                };
                if (t.Description != null)
                {
                    entry["description"] = t.Description;
                }
                tools.Add(entry);
            }
            return tools;
        }

        protected JsonRpcResponse ErrorResponse(Newtonsoft.Json.Linq.JToken? id, int code, string message)
        {
            return new JsonRpcResponse(id, null, new JsonRpcError(code, message));
        }

        protected virtual string DescribeWiringError(BaboonWiringError e)
        {
            return e.ToString();
        }
    }

    // --- Async transport-abstract dispatch base ---
    //
    // Async sibling of `AbstractBaboonMcpServer`. Selected when the backend is
    // generated with `--cs-async-services=true`: the `tools/call` dispatch awaits
    // the async `InvokeJson` (whose result is the async wiring's
    // `Task<Either<..>>`), so `Handle` itself is `async Task<JsonRpcResponse?>`.
    // The synchronous method state machine (initialize / tools/list, the error
    // mapping, the wire-string constants) is identical to the sync base; only the
    // single `tools/call` hop awaits.
    public abstract class AbstractBaboonMcpServerAsync<Ctx> : IBaboonMcpServerAsync<Ctx>, IBaboonRoutableMcpServerAsync<Ctx>
    {
        // PUBLIC routable-server surface (tasks:T114), async flavour: RouteToolCall
        // returns Task<Either<..>>; the muxer awaits it before Channel-A/B.
        public abstract McpServerInfo ServerInfo { get; }
        public abstract IReadOnlyList<McpToolEntry> Tools { get; }
        protected abstract Task<Either<BaboonWiringError, string>> InvokeJson(BaboonMethodId method, string data, Ctx ctx, BaboonCodecContext codecCtx);

        public Task<Either<BaboonWiringError, string>> RouteToolCall(BaboonMethodId method, string data, Ctx ctx, BaboonCodecContext codecCtx)
        {
            return InvokeJson(method, data, ctx, codecCtx);
        }

        private Dictionary<string, McpToolEntry> ByName()
        {
            var m = new Dictionary<string, McpToolEntry>();
            foreach (var t in Tools)
            {
                m[t.Name] = t;
            }
            return m;
        }

        public async Task<JsonRpcResponse?> Handle(JsonRpcRequest request, McpSession session, Ctx ctx, BaboonCodecContext codecCtx)
        {
            var id = request.Id;
            switch (request.Method)
            {
                case "initialize":
                {
                    var pv = request.Params?["protocolVersion"];
                    if (request.Params == null || pv == null)
                    {
                        return ErrorResponse(id, JsonRpcErrorCodes.InvalidParams, "initialize: missing protocolVersion");
                    }
                    session.Initialized = true;
                    var result = new JObject
                    {
                        ["protocolVersion"] = McpProtocol.Version,
                        ["capabilities"] = new JObject { ["tools"] = new JObject() },
                        ["serverInfo"] = new JObject { ["name"] = ServerInfo.Name, ["version"] = ServerInfo.Version },
                    };
                    return new JsonRpcResponse(id, result);
                }
                case "notifications/initialized":
                    return null;
                case "tools/list":
                {
                    if (!session.Initialized)
                    {
                        return ErrorResponse(id, JsonRpcErrorCodes.InvalidRequest, "tools/list before initialize");
                    }
                    var tools = new JArray();
                    foreach (var t in Tools)
                    {
                        var entry = new JObject
                        {
                            ["name"] = t.Name,
                            ["inputSchema"] = t.InputSchema,
                        };
                        if (t.Description != null)
                        {
                            entry["description"] = t.Description;
                        }
                        tools.Add(entry);
                    }
                    return new JsonRpcResponse(id, new JObject { ["tools"] = tools });
                }
                case "tools/call":
                {
                    if (!session.Initialized)
                    {
                        return ErrorResponse(id, JsonRpcErrorCodes.InvalidRequest, "tools/call before initialize");
                    }
                    var name = request.Params?["name"];
                    if (name == null || name.Type != JTokenType.String)
                    {
                        return ErrorResponse(id, JsonRpcErrorCodes.InvalidParams, "tools/call: missing tool name");
                    }
                    var toolName = name.Value<string>()!;
                    if (!ByName().TryGetValue(toolName, out var entry))
                    {
                        return ErrorResponse(id, JsonRpcErrorCodes.InvalidParams, $"tools/call: unknown tool '{toolName}'");
                    }
                    var argsToken = request.Params?["arguments"] ?? new JObject();
                    var argsJson = argsToken.ToString(Newtonsoft.Json.Formatting.None);
                    var result = await InvokeJson(entry.Method, argsJson, ctx, codecCtx);
                    if (result.IsRight)
                    {
                        var content = new JArray { new JObject { ["type"] = "text", ["text"] = result.GetRight() } };
                        return new JsonRpcResponse(id, new JObject { ["content"] = content, ["isError"] = false });
                    }
                    else
                    {
                        // Channel B: a valid protocol call whose domain payload failed.
                        var content = new JArray { new JObject { ["type"] = "text", ["text"] = DescribeWiringError(result.GetLeft()) } };
                        return new JsonRpcResponse(id, new JObject { ["content"] = content, ["isError"] = true });
                    }
                }
                default:
                    return ErrorResponse(id, JsonRpcErrorCodes.MethodNotFound, $"Method not found: {request.Method}");
            }
        }

        protected JsonRpcResponse ErrorResponse(JToken? id, int code, string message)
        {
            return new JsonRpcResponse(id, null, new JsonRpcError(code, message));
        }

        protected virtual string DescribeWiringError(BaboonWiringError e)
        {
            return e.ToString();
        }
    }
}
