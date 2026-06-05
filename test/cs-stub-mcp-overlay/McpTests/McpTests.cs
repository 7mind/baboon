#nullable enable

// T11 — C# MCP round-trip overlay test.
//
// Drives the generated McpToolsMcpServer<Ctx> through the canonical T7 scenario
// (docs/research/mcp-roundtrip-scenario.md) using an entirely in-process fake
// transport. No stdio or HTTP is involved.
//
// Assertion discipline (T7 §5.1):
//   - All assertions are NUnit Assert.* methods that throw unconditionally on
//     failure. Debug.Assert is NOT used (it is elided in Release builds).
//
// NJsonSchema validation tier (T7 §5.3 — T11 is the full-validator tier):
//   - Each returned inputSchema is parsed with NJsonSchema.JsonSchema.FromJsonAsync.
//     Parse errors throw immediately, proving the schema is well-formed.
//     NJsonSchema uses Draft 4-7 `definitions` keyword internally; the Baboon
//     emitter produces Draft 2020-12 `$defs`. We normalise `$defs` →
//     `definitions` and `#/$defs/` → `#/definitions/` before parsing — this is
//     a structural rename with no semantic change for the subset of Draft 2020-12
//     features used by the Baboon emitter (all standard keywords, no dialect
//     extensions beyond `$defs`).
//
// Negative controls (T7 §5.2):
//   - §4.1 (unknown tool → -32602): if the server returned success for
//     McpTools_nonexistent the assertions below would fail.
//   - §4.2 (malformed decode → Channel-B isError=true): if isError were false
//     this test would fail.
//   - NJsonSchema conforming/non-conforming: live — see "enum negative control"
//     and "required-field negative control" tests.
//   - DELIBERATE-NEGATIVE-CONTROL (documented, not left active):
//     Replacing `Assert.AreEqual("McpTools_ping", tools[5]["name"]!)` with
//     `Assert.AreEqual("McpTools_WRONG", tools[5]["name"]!)` makes the test fail,
//     proving the position-5 name assertion is live.
//
// Channel-B trigger (§4.2): send `McpTools_ping` with missing required field
//   `seqno`. The generated `Ping.In_JsonCodec.Decode` accesses
//   `asObject["seqno"]!.Value<Int32>()` where the key is absent (JObject
//   indexer returns null), causing NullReferenceException caught by the wiring
//   as `BaboonWiringError.DecoderFailed`.

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
    //
    // FFancyStr is a foreign type with no built-in codec: the generated
    // FFancyStr_JsonCodec.Encode/Decode throws ArgumentException by design,
    // requiring the integrator to provide a real codec via the lazy-instance
    // override. In this test a passthrough (JSON string → string) is sufficient.
    // ---------------------------------------------------------------------------
    public sealed class FFancyStrPassthroughJsonCodec : FFancyStr_JsonCodec
    {
        public override JToken Encode(BaboonCodecContext ctx, string value)
            => new JValue(value);

        public override string Decode(BaboonCodecContext ctx, JToken wire)
            => wire.Value<string>()!;
    }

    // ---------------------------------------------------------------------------
    // Stub IMcpTools service: every method returns ok=true (T7 §3 convention).
    // ---------------------------------------------------------------------------
    public sealed class StubMcpTools : IMcpTools
    {
        public McpTools.ListCollections.Out ListCollections(McpTools.ListCollections.In arg)
            => new McpTools.ListCollections.Out(true);

        public McpTools.SubmitComposite.Out SubmitComposite(McpTools.SubmitComposite.In arg)
            => new McpTools.SubmitComposite.Out(true);

        public McpTools.ProcessShape.Out ProcessShape(McpTools.ProcessShape.In arg)
            => new McpTools.ProcessShape.Out(true);

        public McpTools.ProcessTagged.Out ProcessTagged(McpTools.ProcessTagged.In arg)
            => new McpTools.ProcessTagged.Out(true);

        public McpTools.PagePoints.Out PagePoints(McpTools.PagePoints.In arg)
            => new McpTools.PagePoints.Out(true);

        public McpTools.Ping.Out Ping(McpTools.Ping.In arg)
            => new McpTools.Ping.Out(true);
    }

    // ---------------------------------------------------------------------------
    // MCP test fixtures (T7 scenario: §1 initialize, §2 tools/list, §3 tools/call,
    // §4 error paths).
    // ---------------------------------------------------------------------------
    [TestFixture]
    public class McpTests
    {
        private readonly BaboonCodecContext _codecCtx = BaboonCodecContext.Default;
        private readonly IBaboonServiceRt _rt = BaboonServiceRtDefault.Instance;

        // Register the passthrough codec before any test in this fixture runs.
        [OneTimeSetUp]
        public void RegisterPassthroughCodec()
        {
            FFancyStr_JsonCodec.Instance = new FFancyStrPassthroughJsonCodec();
        }

        // Build a fake McpJsonInvoke<object?> that routes through the generated
        // McpToolsWiring.InvokeJson with a StubMcpTools implementation.
        private McpJsonInvoke<object?> MakeFakeInvoke()
        {
            var stub = new StubMcpTools();
            var rt = _rt;
            return (method, data, _, codecContext) =>
                McpToolsWiring.InvokeJson(method, data, stub, rt, codecContext);
        }

        private McpToolsMcpServer<object?> MakeServer()
            => new McpToolsMcpServer<object?>(MakeFakeInvoke());

        // Helper: send one JSON-RPC request and assert a response was returned.
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

        // ---------------------------------------------------------------------------
        // NJsonSchema normalisation.
        //
        // NJsonSchema uses `definitions` (Draft 4-7). Baboon emits `$defs`
        // (Draft 2020-12). Rename before parsing; the ref paths must also be updated.
        // This normalisation is purely structural: no keyword semantics differ between
        // `$defs` and `definitions` for the subset Baboon emits.
        // ---------------------------------------------------------------------------
        private static string NormalizeForNJsonSchema(string schemaJson)
        {
            // Replace the key "$defs" with "definitions" and update $ref paths.
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

            // protocolVersion MUST be "2025-06-18"
            Assert.AreEqual("2025-06-18", result["protocolVersion"]!.Value<string>());

            // capabilities MUST contain exactly one key "tools" with value {}
            var caps = (JObject)result["capabilities"]!;
            Assert.AreEqual(1, caps.Count, "capabilities must have exactly one key");
            Assert.IsNotNull(caps["tools"], "capabilities.tools must be present");
            Assert.AreEqual(0, ((JObject)caps["tools"]!).Count, "capabilities.tools must be {}");

            // serverInfo: name and version must be non-empty strings
            var info = (JObject)result["serverInfo"]!;
            var srvName = info["name"]!.Value<string>()!;
            var srvVer = info["version"]!.Value<string>()!;
            Assert.IsNotEmpty(srvName, "serverInfo.name must be non-empty");
            Assert.IsNotEmpty(srvVer, "serverInfo.version must be non-empty");
        }

        [Test]
        public void Sec1_InitializedNotification_ProducesNoResponse()
        {
            var server = MakeServer();
            var session = new McpSession();

            // Initialize first.
            server.Handle(new JsonRpcRequest(
                new JValue(0), "initialize",
                JToken.Parse("{\"protocolVersion\":\"2025-06-18\",\"capabilities\":{},\"clientInfo\":{\"name\":\"t\",\"version\":\"0\"}}")
            ), session, null, _codecCtx);

            // notifications/initialized has no id — must return null.
            var notifResp = server.Handle(
                new JsonRpcRequest(null, "notifications/initialized", null),
                session, null, _codecCtx);

            Assert.IsNull(notifResp, "notifications/initialized MUST produce no response");
        }

        // ---------------------------------------------------------------------------
        // §2 — tools/list + NJsonSchema inputSchema validation
        // ---------------------------------------------------------------------------

        // Helper: initialize + tools/list, return tools array.
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

            // Exact position assertions (model declaration order, T7 §0).
            // processTagged is declared between processShape and pagePoints (T26/D11),
            // so it occupies index 3 and shifts pagePoints→4, ping→5.
            // DELIBERATE-NEGATIVE-CONTROL: replacing "McpTools_ping" with "McpTools_WRONG"
            // on the next line makes this test fail, proving position[5] check is live.
            Assert.AreEqual("McpTools_listCollections", tools[0]["name"]!.Value<string>());
            Assert.AreEqual("McpTools_submitComposite",  tools[1]["name"]!.Value<string>());
            Assert.AreEqual("McpTools_processShape",     tools[2]["name"]!.Value<string>());
            Assert.AreEqual("McpTools_processTagged",    tools[3]["name"]!.Value<string>());
            Assert.AreEqual("McpTools_pagePoints",       tools[4]["name"]!.Value<string>());
            Assert.AreEqual("McpTools_ping",             tools[5]["name"]!.Value<string>());

            // No "nextCursor" key (§2.2)
            Assert.IsNull(((JObject)resp.Result!)["nextCursor"],
                "nextCursor must not be present");

            // No "description" key for any tool (stub model has no doc comments)
            foreach (var t in tools)
            {
                Assert.IsNull(t["description"],
                    $"Tool {t["name"]} must have no description");
            }
        }

        [Test]
        public void Sec2_EachInputSchema_HasDraft202012SchemaUri()
        {
            var (tools, _) = InitAndList();
            foreach (var t in tools)
            {
                var schema = (JObject)t["inputSchema"]!;
                Assert.AreEqual(
                    "https://json-schema.org/draft/2020-12/schema",
                    schema["$schema"]!.Value<string>(),
                    $"Tool {t["name"]}: $schema must be the Draft 2020-12 URI");
            }
        }

        [Test]
        public async Task Sec2_NJsonSchema_AllInputSchemasAreWellFormed()
        {
            // NJsonSchema.JsonSchema.FromJsonAsync throws on malformed schema.
            // This is the K1 full-validator check required for T11.
            var (tools, _) = InitAndList();
            foreach (var t in tools)
            {
                var normalised = NormalizeForNJsonSchema(t["inputSchema"]!.ToString(Formatting.None));
                // Must not throw — proves the schema is well-formed.
                var schema = await JsonSchema.FromJsonAsync(normalised);
                Assert.IsNotNull(schema, $"Tool {t["name"]}: schema must not be null after parse");
            }
        }

        [Test]
        public async Task Sec2_NJsonSchema_Tool0_ListCollections_ConformingInstance()
        {
            var (tools, _) = InitAndList();
            var schemaJson = NormalizeForNJsonSchema(tools[0]["inputSchema"]!.ToString(Formatting.None));
            var schema = await JsonSchema.FromJsonAsync(schemaJson);
            // D6/T30: byColor is map[Color,str] — a string-keyed object with enum wire-name
            // keys (matches the wire the codecs emit and the reconciled inputSchema).
            var errors = schema.Validate("{\"tags\":[\"a\",\"b\"],\"uniqueIds\":[1,2],\"labels\":{\"k\":\"v\"},\"byColor\":{\"Green\":\"ok\",\"Red\":\"stop\"}}");
            Assert.AreEqual(0, errors.Count,
                $"McpTools_listCollections conforming instance must be valid; errors: {FormatErrors(errors)}");
        }

        [Test]
        public async Task Sec2_NJsonSchema_Tool1_SubmitComposite_ConformingInstance()
        {
            var (tools, _) = InitAndList();
            var schemaJson = NormalizeForNJsonSchema(tools[1]["inputSchema"]!.ToString(Formatting.None));
            var schema = await JsonSchema.FromJsonAsync(schemaJson);
            var errors = schema.Validate("{\"nested\":{\"point\":{\"x\":1,\"y\":2},\"color\":\"Red\"},\"maybePoint\":null,\"color\":\"Blue\",\"fancy\":\"anything\"}");
            Assert.AreEqual(0, errors.Count,
                $"McpTools_submitComposite conforming instance must be valid; errors: {FormatErrors(errors)}");
        }

        [Test]
        public async Task Sec2_NJsonSchema_Tool1_SubmitComposite_EnumNegativeControl_PurpleIsRejected()
        {
            // NEGATIVE CONTROL (K1): proves the enum constraint ["Red","Green","Blue"] is live.
            // If "Purple" were accepted, K1 would fail silently.
            var (tools, _) = InitAndList();
            var schemaJson = NormalizeForNJsonSchema(tools[1]["inputSchema"]!.ToString(Formatting.None));
            var schema = await JsonSchema.FromJsonAsync(schemaJson);
            var errors = schema.Validate("{\"nested\":{\"point\":{\"x\":1,\"y\":2},\"color\":\"Red\"},\"maybePoint\":null,\"color\":\"Purple\",\"fancy\":\"anything\"}");
            Assert.Greater(errors.Count, 0, "color='Purple' MUST be rejected (not in enum)");
        }

        [Test]
        public async Task Sec2_NJsonSchema_Tool2_ProcessShape_ConformingInstance()
        {
            var (tools, _) = InitAndList();
            var schemaJson = NormalizeForNJsonSchema(tools[2]["inputSchema"]!.ToString(Formatting.None));
            var schema = await JsonSchema.FromJsonAsync(schemaJson);
            var errors = schema.Validate("{\"shape\":{\"radius\":1.5},\"tree\":{\"value\":0,\"children\":[{\"value\":1,\"children\":[]}]}}");
            Assert.AreEqual(0, errors.Count,
                $"McpTools_processShape conforming instance must be valid; errors: {FormatErrors(errors)}");
        }

        [Test]
        public async Task Sec2_NJsonSchema_Tool3_ProcessTagged_ConformingInstance()
        {
            // T26/D11: contract-bearing ADT. The inputSchema for Tagged is a oneOf of
            // flat branch objects (each branch carries the contract field `id` plus its
            // own field). A conforming instance matches one branch's flat shape — this
            // validates against the SCHEMA (the codec wire form is separate).
            var (tools, _) = InitAndList();
            var schemaJson = NormalizeForNJsonSchema(tools[3]["inputSchema"]!.ToString(Formatting.None));
            var schema = await JsonSchema.FromJsonAsync(schemaJson);
            // TagA branch: { id, tag }
            var errorsA = schema.Validate("{\"tagged\":{\"id\":\"abc\",\"tag\":\"hello\"}}");
            Assert.AreEqual(0, errorsA.Count,
                $"McpTools_processTagged TagA conforming instance must be valid; errors: {FormatErrors(errorsA)}");
            // TagB branch: { id, weight }
            var errorsB = schema.Validate("{\"tagged\":{\"id\":\"def\",\"weight\":42}}");
            Assert.AreEqual(0, errorsB.Count,
                $"McpTools_processTagged TagB conforming instance must be valid; errors: {FormatErrors(errorsB)}");
        }

        [Test]
        public async Task Sec2_NJsonSchema_Tool4_PagePoints_ConformingInstance()
        {
            var (tools, _) = InitAndList();
            var schemaJson = NormalizeForNJsonSchema(tools[4]["inputSchema"]!.ToString(Formatting.None));
            var schema = await JsonSchema.FromJsonAsync(schemaJson);
            var errors = schema.Validate("{\"page\":{\"items\":[{\"x\":1,\"y\":2}],\"total\":1}}");
            Assert.AreEqual(0, errors.Count,
                $"McpTools_pagePoints conforming instance must be valid; errors: {FormatErrors(errors)}");
        }

        [Test]
        public async Task Sec2_NJsonSchema_Tool5_Ping_ConformingInstance()
        {
            var (tools, _) = InitAndList();
            var schemaJson = NormalizeForNJsonSchema(tools[5]["inputSchema"]!.ToString(Formatting.None));
            var schema = await JsonSchema.FromJsonAsync(schemaJson);
            var errors = schema.Validate("{\"seqno\":7,\"label\":\"hi\"}");
            Assert.AreEqual(0, errors.Count,
                $"McpTools_ping conforming instance must be valid; errors: {FormatErrors(errors)}");
        }

        [Test]
        public async Task Sec2_NJsonSchema_Tool5_Ping_RequiredFieldNegativeControl_MissingLabelRejected()
        {
            // NEGATIVE CONTROL: proves the required: ["seqno","label"] constraint is live.
            var (tools, _) = InitAndList();
            var schemaJson = NormalizeForNJsonSchema(tools[5]["inputSchema"]!.ToString(Formatting.None));
            var schema = await JsonSchema.FromJsonAsync(schemaJson);
            var errors = schema.Validate("{\"seqno\":7}");  // missing required "label"
            Assert.Greater(errors.Count, 0, "Missing 'label' MUST be rejected");
        }

        // ---------------------------------------------------------------------------
        // §3 — tools/call (success paths)
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

            // isError MUST be false or absent (K4 §2.4 permits omission when false).
            var isError = result["isError"];
            Assert.IsTrue(isError == null || isError.Value<bool>() == false,
                "isError must be false or absent");
        }

        [Test]
        public void Sec3_SubmitComposite_ReturnsOkTrue()
        {
            var server = MakeServer();
            var session = new McpSession();
            InitializeSession(server, session);

            var resp = Send(server, session, new JsonRpcRequest(
                new JValue(4), "tools/call",
                JToken.Parse("{\"name\":\"McpTools_submitComposite\",\"arguments\":{\"nested\":{\"point\":{\"x\":10,\"y\":20},\"color\":\"Red\"},\"maybePoint\":null,\"color\":\"Blue\",\"fancy\":\"test-value\"}}")
            ), _codecCtx);

            Assert.AreEqual(4, resp.Id!.Value<int>());
            Assert.IsNull(resp.Error, "Unexpected error on submitComposite call");

            var result = (JObject)resp.Result!;
            var content = (JArray)result["content"]!;
            Assert.AreEqual(1, content.Count);
            Assert.AreEqual("text", content[0]["type"]!.Value<string>());

            var payload = JToken.Parse(content[0]["text"]!.Value<string>()!);
            Assert.AreEqual(true, payload["ok"]!.Value<bool>(), "ok must be true");

            var isError = result["isError"];
            Assert.IsTrue(isError == null || isError.Value<bool>() == false,
                "isError must be false or absent");
        }

        [Test]
        public void Sec3_ProcessTagged_ReturnsOkTrue()
        {
            // T26/D11: processTagged dispatch with a Tagged TagA value.
            // ADT wire format under --cs-wrapped-adt-branch-codecs=false is the
            // branch-discriminated object {"TagA":{...}} (the codec wraps the branch;
            // the inputSchema oneOf is a separate structural view). Tagged carries no
            // foreign type, so no FFancyStr codec registration is needed.
            var server = MakeServer();
            var session = new McpSession();
            InitializeSession(server, session);

            var resp = Send(server, session, new JsonRpcRequest(
                new JValue(7), "tools/call",
                JToken.Parse("{\"name\":\"McpTools_processTagged\",\"arguments\":{\"tagged\":{\"TagA\":{\"id\":\"abc\",\"tag\":\"hello\"}}}}")
            ), _codecCtx);

            Assert.AreEqual(7, resp.Id!.Value<int>());
            Assert.IsNull(resp.Error, "Unexpected error on processTagged call");

            var result = (JObject)resp.Result!;
            var content = (JArray)result["content"]!;
            Assert.AreEqual(1, content.Count);
            Assert.AreEqual("text", content[0]["type"]!.Value<string>());

            var payload = JToken.Parse(content[0]["text"]!.Value<string>()!);
            Assert.AreEqual(true, payload["ok"]!.Value<bool>(), "ok must be true");

            var isError = result["isError"];
            Assert.IsTrue(isError == null || isError.Value<bool>() == false,
                "isError must be false or absent");
        }

        // ---------------------------------------------------------------------------
        // §4 — tools/call (error paths) — primary negative controls
        // ---------------------------------------------------------------------------

        [Test]
        public void Sec4_UnknownTool_ChannelAError_Code32602()
        {
            // NEGATIVE CONTROL (T7 §5.2): if the server returned a success result for
            // McpTools_nonexistent, Assert.IsNotNull(resp.Error) would fail.
            var server = MakeServer();
            var session = new McpSession();
            InitializeSession(server, session);

            var resp = Send(server, session, new JsonRpcRequest(
                new JValue(5), "tools/call",
                JToken.Parse("{\"name\":\"McpTools_nonexistent\",\"arguments\":{}}")
            ), _codecCtx);

            Assert.AreEqual(5, resp.Id!.Value<int>());
            // MUST be a Channel-A error, not a result.
            Assert.IsNotNull(resp.Error, "Unknown tool must produce a Channel-A error");
            Assert.IsNull(resp.Result, "No result expected for unknown tool");
            // §4.1: code MUST be -32602 (InvalidParams — unknown tool)
            Assert.AreEqual(-32602, resp.Error!.Code,
                "Unknown tool error code MUST be -32602");
            Assert.IsNotEmpty(resp.Error.Message,
                "error.message must be non-empty");
        }

        [Test]
        public void Sec4_DecodeFailure_ChannelB_IsErrorTrue()
        {
            // NEGATIVE CONTROL: if isError were false this test would fail.
            //
            // Channel-B trigger: send ping with missing required "seqno".
            // The generated Ping.In_JsonCodec.Decode accesses asObject["seqno"]!.Value<int>()
            // where the key is absent (JObject indexer returns null for missing keys),
            // causing NullReferenceException. The wiring catch-block wraps it as
            // BaboonWiringError.DecoderFailed and McpToolsWiring.InvokeJson returns Left.
            // The MCP server produces Channel-B: result with isError=true.
            var server = MakeServer();
            var session = new McpSession();
            InitializeSession(server, session);

            var resp = Send(server, session, new JsonRpcRequest(
                new JValue(6), "tools/call",
                JToken.Parse("{\"name\":\"McpTools_ping\",\"arguments\":{\"label\":\"missing-seqno\"}}")
            ), _codecCtx);

            Assert.AreEqual(6, resp.Id!.Value<int>());
            // Channel B: MUST be a result (not error) with isError=true.
            Assert.IsNotNull(resp.Result, "Channel-B: result must be present");
            Assert.IsNull(resp.Error, "Channel-B: must not be a JSON-RPC error");

            var result = (JObject)resp.Result!;
            Assert.AreEqual(true, result["isError"]!.Value<bool>(),
                "isError MUST be true for Channel-B decode failure");

            var content = (JArray)result["content"]!;
            Assert.Greater(content.Count, 0, "content must have at least one element");
            Assert.AreEqual("text", content[0]["type"]!.Value<string>());
            Assert.IsNotEmpty(content[0]["text"]!.Value<string>(),
                "content[0].text must be non-empty");
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

        private static string FormatErrors(ICollection<ValidationError> errors)
        {
            var parts = new List<string>();
            foreach (var e in errors)
                parts.Add($"{e.Path}: {e.Kind}");
            return string.Join("; ", parts);
        }
    }
}
