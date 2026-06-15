#nullable enable

// T59 (D24/G11) — C# ASYNC-MCP round-trip overlay (GREEN after the fix).
//
// This overlay is the async sibling of test/cs-stub-mcp-overlay/McpTests/. It is
// generated with BOTH --cs-generate-mcp-server=true AND --cs-async-services=true.
//
// Under --cs-async-services=true the generated errors-mode wiring entry
//   `McpToolsWiring.InvokeJson(...)` returns
//       System.Threading.Tasks.Task<Either<BaboonWiringError, string>>
//   (CSServiceWiringTranslator.scala:786 — `async Task<Either<..>>`), and the
//   generated service interface `IMcpTools` is async-typed (each method returns
//   a `Task<..>`).
//
// T59 threads `asyncServices` into the C# MCP server generator: with the flag on,
// the generated `McpToolsMcpServer<Ctx>` extends the async runtime base
//   `Baboon.Runtime.Shared.AbstractBaboonMcpServerAsync<Ctx>`, takes the async
//   delegate `McpJsonInvokeAsync<Ctx>` (returns `Task<Either<..>>`), `await`s it
//   in `InvokeJson`, and exposes `Task<JsonRpcResponse?> Handle(...)`.
//
//   The async wiring `InvokeJson` (Task<Either<..>>) therefore binds directly to
//   the async `McpJsonInvokeAsync<Ctx>` delegate, and `tools/call` round-trips
//   through awaited dispatch. With the flag OFF the generated MCP server is
//   byte-identical to the pre-change sync baseline.

using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using Baboon.Runtime.Shared;
using Mcp.Stub;
using Newtonsoft.Json;
using Newtonsoft.Json.Linq;
using NJsonSchema;
using NJsonSchema.Validation;
using NUnit.Framework;

namespace McpTest
{
    // ---------------------------------------------------------------------------
    // Passthrough codec for FFancyStr (foreign type → System.String identity).
    // ---------------------------------------------------------------------------
    public sealed class FFancyStrPassthroughJsonCodec : FFancyStr_JsonCodec
    {
        public override JToken Encode(BaboonCodecContext ctx, string value)
            => new JValue(value);

        public override string Decode(BaboonCodecContext ctx, JToken wire)
            => wire.Value<string>()!;
    }

    // ---------------------------------------------------------------------------
    // Stub IMcpTools service. Under --cs-async-services=true the generated
    // IMcpTools is async-typed: every method returns Task<...>. The stub returns
    // a completed Task wrapping ok=true.
    // ---------------------------------------------------------------------------
    public sealed class StubMcpTools : IMcpTools
    {
        public Task<McpTools.ListCollections.Out> ListCollections(McpTools.ListCollections.In arg)
            => Task.FromResult(new McpTools.ListCollections.Out(true));

        public Task<McpTools.SubmitComposite.Out> SubmitComposite(McpTools.SubmitComposite.In arg)
            => Task.FromResult(new McpTools.SubmitComposite.Out(true));

        public Task<McpTools.ProcessShape.Out> ProcessShape(McpTools.ProcessShape.In arg)
            => Task.FromResult(new McpTools.ProcessShape.Out(true));

        public Task<McpTools.ProcessTagged.Out> ProcessTagged(McpTools.ProcessTagged.In arg)
            => Task.FromResult(new McpTools.ProcessTagged.Out(true));

        public Task<McpTools.PagePoints.Out> PagePoints(McpTools.PagePoints.In arg)
            => Task.FromResult(new McpTools.PagePoints.Out(true));

        public Task<McpTools.Ping.Out> Ping(McpTools.Ping.In arg)
            => Task.FromResult(new McpTools.Ping.Out(true));
    }

    [TestFixture]
    public class McpTests
    {
        private readonly BaboonCodecContext _codecCtx = BaboonCodecContext.Default;
        private readonly IBaboonServiceRt _rt = BaboonServiceRtDefault.Instance;

        [OneTimeSetUp]
        public void RegisterPassthroughCodec()
        {
            FFancyStr_JsonCodec.Instance = new FFancyStrPassthroughJsonCodec();
        }

        // ---------------------------------------------------------------------------
        // THE ROUND-TRIP POINT.
        //
        // Under async services McpToolsWiring.InvokeJson(...) returns
        // Task<Either<BaboonWiringError, string>>, which binds directly to the async
        // McpJsonInvokeAsync<Ctx> delegate the generated McpToolsMcpServer<Ctx>
        // requires (T59). The body returns the async wiring Task verbatim.
        // ---------------------------------------------------------------------------
        private McpJsonInvokeAsync<object?> MakeFakeInvoke()
        {
            var stub = new StubMcpTools();
            var rt = _rt;
            return (method, data, _, codecContext) =>
                McpToolsWiring.InvokeJson(method, data, stub, rt, codecContext);
        }

        private McpToolsMcpServer<object?> MakeServer()
            => new McpToolsMcpServer<object?>(MakeFakeInvoke());

        private static async Task<JsonRpcResponse> Send(
            McpToolsMcpServer<object?> server,
            McpSession session,
            JsonRpcRequest req,
            BaboonCodecContext codecCtx)
        {
            var resp = await server.Handle(req, session, null, codecCtx);
            if (resp == null)
                throw new InvalidOperationException(
                    $"Expected a response for \"{req.Method}\" but got null (notification not expected here)");
            return resp;
        }

        private static string NormalizeForNJsonSchema(string schemaJson)
        {
            return schemaJson
                .Replace("\"$defs\":", "\"definitions\":")
                .Replace("#/$defs/", "#/definitions/");
        }

        // ---------------------------------------------------------------------------
        // §1 — initialize
        // ---------------------------------------------------------------------------

        [Test]
        public async Task Sec1_Initialize_ResponseIsCorrect()
        {
            var server = MakeServer();
            var session = new McpSession();

            var resp = await Send(server, session, new JsonRpcRequest(
                new JValue(1),
                "initialize",
                JToken.Parse("{\"protocolVersion\":\"2025-06-18\",\"capabilities\":{},\"clientInfo\":{\"name\":\"test-client\",\"version\":\"0.0.1\"}}")
            ), _codecCtx);

            Assert.AreEqual(1, resp.Id!.Value<int>());
            Assert.IsNull(resp.Error, "Expected no error for initialize");

            var result = (JObject)resp.Result!;
            Assert.AreEqual("2025-06-18", result["protocolVersion"]!.Value<string>());

            var caps = (JObject)result["capabilities"]!;
            Assert.AreEqual(1, caps.Count, "capabilities must have exactly one key");
            Assert.IsNotNull(caps["tools"], "capabilities.tools must be present");
            Assert.AreEqual(0, ((JObject)caps["tools"]!).Count, "capabilities.tools must be {}");

            var info = (JObject)result["serverInfo"]!;
            Assert.IsNotEmpty(info["name"]!.Value<string>()!, "serverInfo.name must be non-empty");
            Assert.IsNotEmpty(info["version"]!.Value<string>()!, "serverInfo.version must be non-empty");
        }

        // ---------------------------------------------------------------------------
        // §2 — tools/list
        // ---------------------------------------------------------------------------

        private async Task<(JArray tools, JsonRpcResponse resp)> InitAndList()
        {
            var server = MakeServer();
            var session = new McpSession();

            await server.Handle(new JsonRpcRequest(
                new JValue(0), "initialize",
                JToken.Parse("{\"protocolVersion\":\"2025-06-18\",\"capabilities\":{},\"clientInfo\":{\"name\":\"test-client\",\"version\":\"0.0.1\"}}")
            ), session, null, _codecCtx);
            await server.Handle(new JsonRpcRequest(null, "notifications/initialized", null),
                session, null, _codecCtx);

            var resp = await Send(server, session, new JsonRpcRequest(
                new JValue(2), "tools/list", null
            ), _codecCtx);

            var tools = (JArray)((JObject)resp.Result!)["tools"]!;
            return (tools, resp);
        }

        [Test]
        public async Task Sec2_ToolsList_ExactlySixToolsInDeclarationOrder()
        {
            var (tools, resp) = await InitAndList();

            Assert.AreEqual(2, resp.Id!.Value<int>());
            Assert.IsNull(resp.Error);
            Assert.AreEqual(6, tools.Count, "MUST be exactly 6 tools");

            Assert.AreEqual("McpTools_listCollections", tools[0]["name"]!.Value<string>());
            Assert.AreEqual("McpTools_submitComposite",  tools[1]["name"]!.Value<string>());
            Assert.AreEqual("McpTools_processShape",     tools[2]["name"]!.Value<string>());
            Assert.AreEqual("McpTools_processTagged",    tools[3]["name"]!.Value<string>());
            Assert.AreEqual("McpTools_pagePoints",       tools[4]["name"]!.Value<string>());
            Assert.AreEqual("McpTools_ping",             tools[5]["name"]!.Value<string>());
        }

        [Test]
        public async Task Sec2_NJsonSchema_AllInputSchemasAreWellFormed()
        {
            var (tools, _) = await InitAndList();
            foreach (var t in tools)
            {
                var normalised = NormalizeForNJsonSchema(t["inputSchema"]!.ToString(Formatting.None));
                var schema = await JsonSchema.FromJsonAsync(normalised);
                Assert.IsNotNull(schema, $"Tool {t["name"]}: schema must not be null after parse");
            }
        }

        // ---------------------------------------------------------------------------
        // §3 — tools/call (success path)
        // ---------------------------------------------------------------------------

        [Test]
        public async Task Sec3_Ping_ReturnsOkTrue()
        {
            var server = MakeServer();
            var session = new McpSession();
            await InitializeSession(server, session);

            var resp = await Send(server, session, new JsonRpcRequest(
                new JValue(3), "tools/call",
                JToken.Parse("{\"name\":\"McpTools_ping\",\"arguments\":{\"seqno\":42,\"label\":\"hello\"}}")
            ), _codecCtx);

            Assert.AreEqual(3, resp.Id!.Value<int>());
            Assert.IsNull(resp.Error, "Unexpected error on ping call");

            var result = (JObject)resp.Result!;
            var content = (JArray)result["content"]!;
            Assert.AreEqual(1, content.Count, "content must have exactly one element");
            Assert.AreEqual("text", content[0]["type"]!.Value<string>());

            var payload = JToken.Parse(content[0]["text"]!.Value<string>()!);
            Assert.AreEqual(true, payload["ok"]!.Value<bool>(), "ok must be true");

            var isError = result["isError"];
            Assert.IsTrue(isError == null || isError.Value<bool>() == false,
                "isError must be false or absent");
        }

        // ---------------------------------------------------------------------------
        // §4 — tools/call (error path) — negative control
        // ---------------------------------------------------------------------------

        [Test]
        public async Task Sec4_UnknownTool_ChannelAError_Code32602()
        {
            var server = MakeServer();
            var session = new McpSession();
            await InitializeSession(server, session);

            var resp = await Send(server, session, new JsonRpcRequest(
                new JValue(5), "tools/call",
                JToken.Parse("{\"name\":\"McpTools_nonexistent\",\"arguments\":{}}")
            ), _codecCtx);

            Assert.AreEqual(5, resp.Id!.Value<int>());
            Assert.IsNotNull(resp.Error, "Unknown tool must produce a Channel-A error");
            Assert.IsNull(resp.Result, "No result expected for unknown tool");
            Assert.AreEqual(-32602, resp.Error!.Code, "Unknown tool error code MUST be -32602");
            Assert.IsNotEmpty(resp.Error.Message, "error.message must be non-empty");
        }

        // ---------------------------------------------------------------------------
        // Helpers
        // ---------------------------------------------------------------------------

        private async Task InitializeSession(McpToolsMcpServer<object?> server, McpSession session)
        {
            await server.Handle(new JsonRpcRequest(
                new JValue(0), "initialize",
                JToken.Parse("{\"protocolVersion\":\"2025-06-18\",\"capabilities\":{},\"clientInfo\":{\"name\":\"test-client\",\"version\":\"0.0.1\"}}")
            ), session, null, _codecCtx);
            await server.Handle(new JsonRpcRequest(null, "notifications/initialized", null),
                session, null, _codecCtx);
        }
    }
}
