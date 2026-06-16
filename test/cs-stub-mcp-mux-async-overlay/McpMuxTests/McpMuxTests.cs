#nullable enable

// T106 (async) — C# ASYNC MCP muxer round-trip overlay test.
//
// Async sibling of test/cs-stub-mcp-mux-overlay/. Exercises
// `AbstractAsyncMcpMuxer<Ctx>` by composing two FRESHLY GENERATED async
// `<Service>McpServer` instances produced from the T102 `mcp-mux-stub-ok/`
// model (UserService + OrderService) compiled with `--cs-async-services=true`.
//
// Under `--cs-async-services=true`:
//   - service interface methods return Task<...Out>,
//   - the generated wiring `<Service>Wiring.InvokeJson` is async and returns
//     Task<Either<BaboonWiringError, string>>,
//   - the generated `<Service>McpServer<Ctx>` extends AbstractBaboonMcpServerAsync
//     and implements IBaboonRoutableMcpServerAsync<Ctx>,
//   - `AbstractAsyncMcpMuxer.Handle` is async and awaits each RouteToolCall.
//
// Four asserted muxer behaviours (T106 acceptance, async axis):
//   1. tools/list  -> UNION in registration-then-declaration order (awaited);
//   2. tools/call  -> routes to the correct owning service, awaited;
//   3. colliding server -> throws BaboonMcpWiringException{Tag:DuplicateTool};
//   4. unknown tool name -> -32602 response (NoMatchingTool), awaited.
//
// Assertion discipline: all assertions use NUnit Assert.* which throw
// unconditionally on failure.

using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using Baboon.Runtime.Shared;
using Mcp.Mux.Stub;
using Newtonsoft.Json;
using Newtonsoft.Json.Linq;
using NUnit.Framework;

namespace McpMuxTest
{
    // ---------------------------------------------------------------------------
    // Stub implementations: async, return completed Tasks with fixed values.
    // ---------------------------------------------------------------------------

    public sealed class StubUserService : IUserService
    {
        public Task<UserService.CreateUser.Out> CreateUser(UserService.CreateUser.In arg)
            => Task.FromResult(new UserService.CreateUser.Out(new UserProfile("u1", "a@b.c", UserStatus.Active)));

        public Task<UserService.GetUser.Out> GetUser(UserService.GetUser.In arg)
            => Task.FromResult(new UserService.GetUser.Out(new UserProfile("u1", "a@b.c", UserStatus.Active)));
    }

    public sealed class StubOrderService : IOrderService
    {
        public Task<OrderService.PlaceOrder.Out> PlaceOrder(OrderService.PlaceOrder.In arg)
            => Task.FromResult(new OrderService.PlaceOrder.Out(new OrderSummary("o1", OrderStatus.Confirmed, 10.0)));

        public Task<OrderService.CancelOrder.Out> CancelOrder(OrderService.CancelOrder.In arg)
            => Task.FromResult(new OrderService.CancelOrder.Out(true));
    }

    // ---------------------------------------------------------------------------
    // MCP async muxer tests
    // ---------------------------------------------------------------------------
    [TestFixture]
    public class McpMuxAsyncTests
    {
        private readonly BaboonCodecContext _codecCtx = BaboonCodecContext.Default;
        private readonly IBaboonServiceRt _rt = BaboonServiceRtDefault.Instance;

        private static readonly string[] ExpectedUnion =
        {
            "UserService_createUser",
            "UserService_getUser",
            "OrderService_placeOrder",
            "OrderService_cancelOrder",
        };

        private McpJsonInvokeAsync<object?> MakeUserInvoke()
        {
            var stub = new StubUserService();
            var rt = _rt;
            return (method, data, _, codecContext) =>
                UserServiceWiring.InvokeJson(method, data, stub, rt, codecContext);
        }

        private McpJsonInvokeAsync<object?> MakeOrderInvoke()
        {
            var stub = new StubOrderService();
            var rt = _rt;
            return (method, data, _, codecContext) =>
                OrderServiceWiring.InvokeJson(method, data, stub, rt, codecContext);
        }

        private IBaboonRoutableMcpServerAsync<object?> MakeUserServer()
            => new UserServiceMcpServer<object?>(MakeUserInvoke());

        private IBaboonRoutableMcpServerAsync<object?> MakeOrderServer()
            => new OrderServiceMcpServer<object?>(MakeOrderInvoke());

        private AbstractAsyncMcpMuxer<object?> MakeMuxer()
            => new AbstractAsyncMcpMuxer<object?>(
                new McpServerInfo("MergedAsyncEndpoint", "1.0.0"),
                MakeUserServer(), MakeOrderServer());

        // ---------------------------------------------------------------------------
        // Helper: async initialize + notification + tools/list.
        // ---------------------------------------------------------------------------
        private async Task<(JArray tools, JsonRpcResponse resp)> InitAndList(AbstractAsyncMcpMuxer<object?> mux)
        {
            var session = new McpSession();
            await mux.Handle(new JsonRpcRequest(
                new JValue(0), "initialize",
                JToken.Parse("{\"protocolVersion\":\"2025-06-18\",\"capabilities\":{},\"clientInfo\":{\"name\":\"t\",\"version\":\"0\"}}")
            ), session, null, _codecCtx);
            await mux.Handle(new JsonRpcRequest(null, "notifications/initialized", null),
                session, null, _codecCtx);

            var resp = await mux.Handle(new JsonRpcRequest(
                new JValue(1), "tools/list", null
            ), session, null, _codecCtx);

            Assert.IsNotNull(resp, "tools/list must return a response");
            var tools = (JArray)((JObject)resp!.Result!)["tools"]!;
            return (tools, resp);
        }

        private async Task<McpSession> InitedSession(AbstractAsyncMcpMuxer<object?> mux)
        {
            var session = new McpSession();
            await mux.Handle(new JsonRpcRequest(
                new JValue(0), "initialize",
                JToken.Parse("{\"protocolVersion\":\"2025-06-18\",\"capabilities\":{},\"clientInfo\":{\"name\":\"t\",\"version\":\"0\"}}")
            ), session, null, _codecCtx);
            await mux.Handle(new JsonRpcRequest(null, "notifications/initialized", null),
                session, null, _codecCtx);
            return session;
        }

        private async Task<JsonRpcResponse> Call(AbstractAsyncMcpMuxer<object?> mux, McpSession session, string toolName, JToken args)
        {
            var resp = await mux.Handle(new JsonRpcRequest(
                new JValue(99), "tools/call",
                new JObject { ["name"] = toolName, ["arguments"] = args }
            ), session, null, _codecCtx);
            Assert.IsNotNull(resp, $"tools/call '{toolName}' must return a response");
            return resp!;
        }

        // ---------------------------------------------------------------------------
        // Test 1: tools/list returns the UNION in registration-then-declaration order
        // ---------------------------------------------------------------------------

        [Test]
        public async Task Test1_ToolsList_UnionInRegistrationOrder()
        {
            var (tools, resp) = await InitAndList(MakeMuxer());

            Assert.IsNull(resp.Error, "tools/list must not return an error");
            Assert.AreEqual(4, tools.Count, "MUST be exactly 4 tools (2 per service)");

            for (int i = 0; i < ExpectedUnion.Length; i++)
            {
                Assert.AreEqual(ExpectedUnion[i], tools[i]["name"]!.Value<string>(),
                    $"Position {i} tool name mismatch");
            }
        }

        [Test]
        public async Task Test1_NegativeControl_UnionIsNotInterleaved()
        {
            var (tools, _) = await InitAndList(MakeMuxer());
            var names = new List<string>();
            foreach (var t in tools) names.Add(t["name"]!.Value<string>()!);

            var interleaved = new[] {
                "UserService_createUser",
                "OrderService_placeOrder",
                "UserService_getUser",
                "OrderService_cancelOrder",
            };
            bool matches = true;
            for (int i = 0; i < interleaved.Length; i++)
                if (names[i] != interleaved[i]) { matches = false; break; }
            Assert.IsFalse(matches, "Interleaved ordering must NOT match the actual union");
        }

        // ---------------------------------------------------------------------------
        // Test 2: tools/call routes to the correct owning service (awaited)
        // ---------------------------------------------------------------------------

        [Test]
        public async Task Test2_Routing_UserService_GetUser_ChannelA_IsErrorFalse()
        {
            var mux = MakeMuxer();
            var session = await InitedSession(mux);

            var resp = await Call(mux, session, "UserService_getUser",
                new JObject { ["userId"] = "u1" });

            Assert.AreEqual(99, resp.Id!.Value<int>());
            Assert.IsNull(resp.Error, "Unexpected error on getUser call");

            var result = (JObject)resp.Result!;
            var content = (JArray)result["content"]!;
            Assert.AreEqual(1, content.Count, "content must have exactly one element");
            Assert.AreEqual("text", content[0]["type"]!.Value<string>());

            var isError = result["isError"];
            Assert.IsTrue(isError == null || isError.Value<bool>() == false,
                "isError must be false or absent for Channel-A success");

            var payload = JToken.Parse(content[0]["text"]!.Value<string>()!);
            Assert.AreEqual("u1", payload["profile"]?["userId"]?.Value<string>(),
                "profile.userId must be 'u1'");
        }

        [Test]
        public async Task Test2_Routing_OrderService_CancelOrder_ChannelA_IsErrorFalse()
        {
            var mux = MakeMuxer();
            var session = await InitedSession(mux);

            var resp = await Call(mux, session, "OrderService_cancelOrder",
                new JObject { ["orderId"] = "o1", ["reason"] = JValue.CreateNull() });

            Assert.IsNull(resp.Error, "Unexpected error on cancelOrder call");

            var result = (JObject)resp.Result!;
            var content = (JArray)result["content"]!;
            Assert.AreEqual(1, content.Count);
            Assert.AreEqual("text", content[0]["type"]!.Value<string>());

            var isError = result["isError"];
            Assert.IsTrue(isError == null || isError.Value<bool>() == false,
                "isError must be false or absent");

            var payload = JToken.Parse(content[0]["text"]!.Value<string>()!);
            Assert.AreEqual(true, payload["ok"]?.Value<bool>(), "ok must be true");
        }

        [Test]
        public async Task Test2_Routing_OrderService_PlaceOrder_DecoderFailure_ChannelB_IsErrorTrue()
        {
            var mux = MakeMuxer();
            var session = await InitedSession(mux);

            var resp = await Call(mux, session, "OrderService_placeOrder",
                new JObject { ["userId"] = "u1", ["items"] = JValue.CreateNull() });

            Assert.IsNull(resp.Error, "Channel-B must produce a result, not a JSON-RPC error");
            var result = (JObject)resp.Result!;
            Assert.AreEqual(true, result["isError"]?.Value<bool>(),
                "isError MUST be true for Channel-B decode failure");
            var content = (JArray)result["content"]!;
            Assert.Greater(content.Count, 0, "content must have at least one element");
            Assert.IsNotEmpty(content[0]["text"]?.Value<string>(),
                "content[0].text must be non-empty");
        }

        // ---------------------------------------------------------------------------
        // Test 3: DuplicateTool on collision
        // ---------------------------------------------------------------------------

        [Test]
        public void Test3_DuplicateTool_CollisionOnCtor_ThrowsBaboonMcpWiringException()
        {
            BaboonMcpWiringException? thrown = null;
            try
            {
                _ = new AbstractAsyncMcpMuxer<object?>(
                    new McpServerInfo("Merged", "1.0.0"),
                    MakeUserServer(), MakeUserServer());
            }
            catch (BaboonMcpWiringException ex)
            {
                thrown = ex;
            }
            Assert.IsNotNull(thrown, "Ctor with colliding servers MUST throw BaboonMcpWiringException");
            Assert.AreEqual(BaboonMcpWiringErrorTag.DuplicateTool, thrown!.Error.Tag);
            Assert.AreEqual("UserService_createUser", thrown.Error.ToolName,
                "First colliding tool MUST be UserService_createUser");
        }

        [Test]
        public void Test3_DuplicateTool_CollisionOnRegister_ThrowsBaboonMcpWiringException()
        {
            var mux = new AbstractAsyncMcpMuxer<object?>(
                new McpServerInfo("Merged", "1.0.0"),
                MakeUserServer());

            BaboonMcpWiringException? thrown = null;
            try
            {
                mux.Register(MakeUserServer());
            }
            catch (BaboonMcpWiringException ex)
            {
                thrown = ex;
            }
            Assert.IsNotNull(thrown, "Register with colliding server MUST throw BaboonMcpWiringException");
            Assert.AreEqual(BaboonMcpWiringErrorTag.DuplicateTool, thrown!.Error.Tag);
        }

        // ---------------------------------------------------------------------------
        // Test 4: NoMatchingTool -> -32602 response (awaited)
        // ---------------------------------------------------------------------------

        [Test]
        public async Task Test4_NoMatchingTool_UnknownToolName_Code32602()
        {
            var mux = MakeMuxer();
            var session = await InitedSession(mux);

            var resp = await Call(mux, session, "UserService_nope", new JObject());

            Assert.IsNotNull(resp.Error, "Unknown tool must produce a Channel-A error");
            Assert.IsNull(resp.Result, "No result expected for unknown tool");
            Assert.AreEqual(-32602, resp.Error!.Code, "Unknown tool error code MUST be -32602");
            Assert.IsNotEmpty(resp.Error.Message, "error.message must be non-empty");
        }
    }
}
