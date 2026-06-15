#nullable enable

// T58 (D24/G11) — C# ASYNC-MCP reproduction overlay (EXPECTED RED).
//
// This overlay is the async sibling of test/cs-stub-mcp-overlay/McpTests/. It is
// generated with BOTH --cs-generate-mcp-server=true AND --cs-async-services=true.
//
// PURPOSE — reproduce the sync-delegate vs async-wiring mismatch:
//   Under --cs-async-services=true the generated errors-mode wiring entry
//   `McpToolsWiring.InvokeJson(...)` returns
//       System.Threading.Tasks.Task<Either<BaboonWiringError, string>>
//   (CSServiceWiringTranslator.scala:786 — `async Task<Either<..>>`), and the
//   generated service interface `IMcpTools` is async-typed (each method returns
//   `Task<Either<.., Out>>`).
//
//   But the generated `McpToolsMcpServer<Ctx>` (CsMcpServerGenerator.scala:126-137)
//   is STILL sync: its constructor takes the runtime delegate
//       Baboon.Runtime.Shared.McpJsonInvoke<Ctx>
//   (BaboonMcpRuntime.cs:102) whose return type is the SYNCHRONOUS
//       Either<BaboonWiringError, string>
//   and its `protected override InvokeJson(...)` (BaboonMcpRuntime.cs:120) is
//   declared to return that same synchronous `Either<..>` directly.
//
//   Therefore the async wiring `InvokeJson` (Task<Either<..>>) CANNOT bind to the
//   sync `McpJsonInvoke<Ctx>` delegate that the generated MCP server requires.
//   `MakeFakeInvoke()` below attempts exactly that bind and the C# compiler MUST
//   reject it with a return-type mismatch (CS0029 / CS4016) between
//   `Task<Either<BaboonWiringError, string>>` and `Either<BaboonWiringError, string>`.
//
// EXPECTED FAILURE (right reason): `dotnet build` of this overlay fails at the
//   `MakeFakeInvoke` body — the async wiring result `Task<Either<..>>` is not
//   convertible to the sync `McpJsonInvoke<object?>` delegate's `Either<..>`
//   return type. This is NOT a missing-file / harness error: every type
//   referenced here (McpToolsWiring, McpToolsMcpServer<Ctx>, IMcpTools,
//   McpJsonInvoke<Ctx>) is emitted by the generator under the async flags; only
//   the SYNC-vs-ASYNC return-type contract is incompatible.
//
// This lane GATES the C# MCP async fix (T59). It must stay RED until the
// generator emits an async-capable MCP server (async McpJsonInvoke delegate +
// async InvokeJson override). Do NOT wire it green here.

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
        // THE REPRODUCTION POINT.
        //
        // The generated McpToolsMcpServer<Ctx> requires a SYNC McpJsonInvoke<Ctx>
        // delegate (returns Either<BaboonWiringError, string>). But under async
        // services McpToolsWiring.InvokeJson(...) returns
        // Task<Either<BaboonWiringError, string>>. Returning the async wiring result
        // from a delegate whose return type is the sync Either<..> is a return-type
        // mismatch the C# compiler rejects (CS0029 / CS4016).
        //
        // We deliberately do NOT `await` here: there is no async-capable seam in the
        // sync McpJsonInvoke<Ctx> contract to await into — that absence IS the defect
        // T59 must fix. The bind below is the minimal expression of it.
        // ---------------------------------------------------------------------------
        private McpJsonInvoke<object?> MakeFakeInvoke()
        {
            var stub = new StubMcpTools();
            var rt = _rt;
            // RED: McpToolsWiring.InvokeJson returns Task<Either<..>> under async,
            // but McpJsonInvoke<object?> requires the body to yield Either<..>.
            return (method, data, _, codecContext) =>
                McpToolsWiring.InvokeJson(method, data, stub, rt, codecContext);
        }

        private McpToolsMcpServer<object?> MakeServer()
            => new McpToolsMcpServer<object?>(MakeFakeInvoke());

        private static JsonRpcResponse Send(
            McpToolsMcpServer<object?> server,
            McpSession session,
            JsonRpcRequest req,
            BaboonCodecContext codecCtx)
        {
            var resp = server.Handle(req, session, null, codecCtx);
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
        public void Sec1_Initialize_ResponseIsCorrect()
        {
            var server = MakeServer();
            var session = new McpSession();

            var resp = Send(server, session, new JsonRpcRequest(
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

        private (JArray tools, JsonRpcResponse resp) InitAndList()
        {
            var server = MakeServer();
            var session = new McpSession();

            server.Handle(new JsonRpcRequest(
                new JValue(0), "initialize",
                JToken.Parse("{\"protocolVersion\":\"2025-06-18\",\"capabilities\":{},\"clientInfo\":{\"name\":\"test-client\",\"version\":\"0.0.1\"}}")
            ), session, null, _codecCtx);
            server.Handle(new JsonRpcRequest(null, "notifications/initialized", null),
                session, null, _codecCtx);

            var resp = Send(server, session, new JsonRpcRequest(
                new JValue(2), "tools/list", null
            ), _codecCtx);

            var tools = (JArray)((JObject)resp.Result!)["tools"]!;
            return (tools, resp);
        }

        [Test]
        public void Sec2_ToolsList_ExactlySixToolsInDeclarationOrder()
        {
            var (tools, resp) = InitAndList();

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
            var (tools, _) = InitAndList();
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
        public void Sec3_Ping_ReturnsOkTrue()
        {
            var server = MakeServer();
            var session = new McpSession();
            InitializeSession(server, session);

            var resp = Send(server, session, new JsonRpcRequest(
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
        public void Sec4_UnknownTool_ChannelAError_Code32602()
        {
            var server = MakeServer();
            var session = new McpSession();
            InitializeSession(server, session);

            var resp = Send(server, session, new JsonRpcRequest(
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

        private void InitializeSession(McpToolsMcpServer<object?> server, McpSession session)
        {
            server.Handle(new JsonRpcRequest(
                new JValue(0), "initialize",
                JToken.Parse("{\"protocolVersion\":\"2025-06-18\",\"capabilities\":{},\"clientInfo\":{\"name\":\"test-client\",\"version\":\"0.0.1\"}}")
            ), session, null, _codecCtx);
            server.Handle(new JsonRpcRequest(null, "notifications/initialized", null),
                session, null, _codecCtx);
        }
    }
}
