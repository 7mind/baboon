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
    public abstract class AbstractBaboonMcpServer<Ctx> : IBaboonMcpServer<Ctx>
    {
        protected abstract McpServerInfo ServerInfo { get; }
        protected abstract IReadOnlyList<McpToolEntry> Tools { get; }
        protected abstract Either<BaboonWiringError, string> InvokeJson(BaboonMethodId method, string data, Ctx ctx, BaboonCodecContext codecCtx);

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

    // --- Async transport-abstract dispatch base ---
    //
    // Async sibling of `AbstractBaboonMcpServer`. Selected when the backend is
    // generated with `--cs-async-services=true`: the `tools/call` dispatch awaits
    // the async `InvokeJson` (whose result is the async wiring's
    // `Task<Either<..>>`), so `Handle` itself is `async Task<JsonRpcResponse?>`.
    // The synchronous method state machine (initialize / tools/list, the error
    // mapping, the wire-string constants) is identical to the sync base; only the
    // single `tools/call` hop awaits.
    public abstract class AbstractBaboonMcpServerAsync<Ctx> : IBaboonMcpServerAsync<Ctx>
    {
        protected abstract McpServerInfo ServerInfo { get; }
        protected abstract IReadOnlyList<McpToolEntry> Tools { get; }
        protected abstract Task<Either<BaboonWiringError, string>> InvokeJson(BaboonMethodId method, string data, Ctx ctx, BaboonCodecContext codecCtx);

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
