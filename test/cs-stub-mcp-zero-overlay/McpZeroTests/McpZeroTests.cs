#nullable enable

// T178 / D40 — C# zero-service MCP overlay test (RED baseline).
//
// Purpose: prove the ZERO-SERVICE MCP runtime contract. The zero-service model
// (mcp-stub-zero-services-ok) declares @root types but NO RPC block, so
// servicesOf(domain) is empty and NO per-service `<Service>McpServer` class is
// generated. With zero services the ONLY source of the MCP runtime types
// (AbstractMcpMuxer / IBaboonRoutableMcpServer / McpServerInfo / McpSession /
// JsonRpcRequest / JsonRpcResponse / JsonRpcErrorCodes) is the STATIC runtime
// file `BaboonMcpRuntime.cs`.
//
// RED (pre-fix): the current generator emits NO `BaboonMcpRuntime.cs` for a
// zero-service model, so `Baboon.Runtime.Shared.AbstractMcpMuxer` and its
// siblings are unresolved and THIS FILE FAILS TO COMPILE (CS0246 / CS0234 —
// missing type / missing runtime). That compile failure IS the D40 reproduction.
//
// GREEN (post-fix, later task): once the generator emits the static MCP runtime
// even when servicesOf(domain) is empty, this file compiles and the runtime
// assertions below pass — an empty muxer lists zero tools and rejects any
// tools/call with JSON-RPC -32602.
//
// Assertion discipline: NUnit Assert.* / explicit throw — unconditional, never
// Debug.Assert (elided in Release).

using System;
using Baboon.Runtime.Shared;
using Newtonsoft.Json.Linq;
using NUnit.Framework;

namespace McpZeroTest
{
    [TestFixture]
    public class McpZeroTests
    {
        private readonly BaboonCodecContext _codecCtx = BaboonCodecContext.Default;

        // COMPILE-TIME contract: constructing AbstractMcpMuxer<Ctx> with ZERO
        // registered servers requires the static runtime file to exist. With zero
        // services there is no generated <Service>McpServer to import — these types
        // resolve ONLY from BaboonMcpRuntime.cs.
        private AbstractMcpMuxer<object?> MakeEmptyMuxer()
            => new AbstractMcpMuxer<object?>(new McpServerInfo("ZeroEndpoint", "1.0.0"));

        private McpSession InitedSession(AbstractMcpMuxer<object?> mux)
        {
            var session = new McpSession();
            mux.Handle(new JsonRpcRequest(
                new JValue(0), "initialize",
                JToken.Parse("{\"protocolVersion\":\"2025-06-18\",\"capabilities\":{},\"clientInfo\":{\"name\":\"t\",\"version\":\"0\"}}")
            ), session, null, _codecCtx);
            mux.Handle(new JsonRpcRequest(null, "notifications/initialized", null),
                session, null, _codecCtx);
            return session;
        }

        // RUNTIME contract (a): tools/list on an empty muxer returns an empty array.
        [Test]
        public void ZeroServices_ToolsList_IsEmpty()
        {
            var mux = MakeEmptyMuxer();
            var session = InitedSession(mux);

            var resp = mux.Handle(new JsonRpcRequest(
                new JValue(1), "tools/list", null
            ), session, null, _codecCtx);

            if (resp == null)
                throw new InvalidOperationException("tools/list must return a response");
            Assert.IsNull(resp.Error, "tools/list must not return an error");

            var tools = (JArray)((JObject)resp.Result!)["tools"]!;
            Assert.AreEqual(0, tools.Count, "empty muxer MUST list zero tools");
        }

        // RUNTIME contract (b): an unknown tool call yields JSON-RPC -32602.
        [Test]
        public void ZeroServices_UnknownToolCall_Code32602()
        {
            var mux = MakeEmptyMuxer();
            var session = InitedSession(mux);

            var resp = mux.Handle(new JsonRpcRequest(
                new JValue(2), "tools/call",
                new JObject { ["name"] = "anything_at_all", ["arguments"] = new JObject() }
            ), session, null, _codecCtx);

            if (resp == null)
                throw new InvalidOperationException("tools/call must return a response");
            Assert.IsNotNull(resp.Error, "unknown tool on empty muxer MUST produce a Channel-A error");
            Assert.IsNull(resp.Result, "no result expected for unknown tool");
            Assert.AreEqual(-32602, resp.Error!.Code,
                "unknown-tool error code MUST be -32602 (InvalidParams)");
            Assert.IsNotEmpty(resp.Error.Message, "error.message must be non-empty");
        }
    }
}
