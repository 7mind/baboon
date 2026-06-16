#nullable enable

// T106 — C# MCP muxer round-trip overlay test (sync).
//
// Exercises `AbstractMcpMuxer<Ctx>` by composing two FRESHLY GENERATED
// `<Service>McpServer` instances produced from the T102 `mcp-mux-stub-ok/`
// model (UserService + OrderService). Composition is done strictly through
// the public T114 routable surface:
//   - the muxer ctor takes `IBaboonRoutableMcpServer<Ctx>` members,
//   - the test NEVER subclasses a `<Service>McpServer`,
//   - the test NEVER calls a member server's own `Handle`.
//
// Generated code lands in `BaboonDefinitions/Generated/` (the isolated dir
// set by the `test-gen-cs-mcp-mux` mdl action). No committed generated fixtures.
//
// Four asserted muxer behaviours (T106 acceptance):
//   1. tools/list  -> UNION of both services' tools in registration-then-
//                    declaration order;
//   2. tools/call  -> routes the flat tool name to the correct owning
//                    service — proven for a tool of EACH service;
//   3. register a server with a colliding tool name -> throws
//      BaboonMcpWiringException{Tag:DuplicateTool};
//   4. unknown tool name -> -32602 "unknown tool" response (NoMatchingTool).
//
// Assertion discipline: all assertions use NUnit Assert.* which throw
// unconditionally on failure. Debug.Assert is NOT used (elided in Release).

using System;
using System.Collections.Generic;
using Baboon.Runtime.Shared;
using Mcp.Mux.Stub;
using Newtonsoft.Json;
using Newtonsoft.Json.Linq;
using NUnit.Framework;

namespace McpMuxTest
{
    // ---------------------------------------------------------------------------
    // Stub implementations: synchronous, return fixed values.
    // ---------------------------------------------------------------------------

    public sealed class StubUserService : IUserService
    {
        public UserService.CreateUser.Out CreateUser(UserService.CreateUser.In arg)
            => new UserService.CreateUser.Out(new UserProfile("u1", "a@b.c", UserStatus.Active));

        public UserService.GetUser.Out GetUser(UserService.GetUser.In arg)
            => new UserService.GetUser.Out(new UserProfile("u1", "a@b.c", UserStatus.Active));
    }

    public sealed class StubOrderService : IOrderService
    {
        public OrderService.PlaceOrder.Out PlaceOrder(OrderService.PlaceOrder.In arg)
            => new OrderService.PlaceOrder.Out(new OrderSummary("o1", OrderStatus.Confirmed, 10.0));

        public OrderService.CancelOrder.Out CancelOrder(OrderService.CancelOrder.In arg)
            => new OrderService.CancelOrder.Out(true);
    }

    // ---------------------------------------------------------------------------
    // MCP muxer tests (T106: AbstractMcpMuxer over two freshly-generated servers)
    // ---------------------------------------------------------------------------
    [TestFixture]
    public class McpMuxTests
    {
        private readonly BaboonCodecContext _codecCtx = BaboonCodecContext.Default;
        private readonly IBaboonServiceRt _rt = BaboonServiceRtDefault.Instance;

        // The union of both services' tools in registration-then-declaration order:
        // UserService registered first (createUser, getUser),
        // OrderService second (placeOrder, cancelOrder).
        private static readonly string[] ExpectedUnion =
        {
            "UserService_createUser",
            "UserService_getUser",
            "OrderService_placeOrder",
            "OrderService_cancelOrder",
        };

        private McpJsonInvoke<object?> MakeUserInvoke()
        {
            var stub = new StubUserService();
            var rt = _rt;
            return (method, data, _, codecContext) =>
                UserServiceWiring.InvokeJson(method, data, stub, rt, codecContext);
        }

        private McpJsonInvoke<object?> MakeOrderInvoke()
        {
            var stub = new StubOrderService();
            var rt = _rt;
            return (method, data, _, codecContext) =>
                OrderServiceWiring.InvokeJson(method, data, stub, rt, codecContext);
        }

        private IBaboonRoutableMcpServer<object?> MakeUserServer()
            => new UserServiceMcpServer<object?>(MakeUserInvoke());

        private IBaboonRoutableMcpServer<object?> MakeOrderServer()
            => new OrderServiceMcpServer<object?>(MakeOrderInvoke());

        private AbstractMcpMuxer<object?> MakeMuxer()
            => new AbstractMcpMuxer<object?>(
                new McpServerInfo("MergedEndpoint", "1.0.0"),
                MakeUserServer(), MakeOrderServer());

        // ---------------------------------------------------------------------------
        // Helper: initialize + notification + tools/list, return tools array.
        // ---------------------------------------------------------------------------
        private (JArray tools, JsonRpcResponse resp) InitAndList(AbstractMcpMuxer<object?> mux)
        {
            var session = new McpSession();
            mux.Handle(new JsonRpcRequest(
                new JValue(0), "initialize",
                JToken.Parse("{\"protocolVersion\":\"2025-06-18\",\"capabilities\":{},\"clientInfo\":{\"name\":\"t\",\"version\":\"0\"}}")
            ), session, null, _codecCtx);
            mux.Handle(new JsonRpcRequest(null, "notifications/initialized", null),
                session, null, _codecCtx);

            var resp = mux.Handle(new JsonRpcRequest(
                new JValue(1), "tools/list", null
            ), session, null, _codecCtx);

            Assert.IsNotNull(resp, "tools/list must return a response");
            var tools = (JArray)((JObject)resp!.Result!)["tools"]!;
            return (tools, resp);
        }

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

        private JsonRpcResponse Call(AbstractMcpMuxer<object?> mux, McpSession session, string toolName, JToken args)
        {
            var resp = mux.Handle(new JsonRpcRequest(
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
        public void Test1_ToolsList_UnionInRegistrationOrder()
        {
            var (tools, resp) = InitAndList(MakeMuxer());

            Assert.IsNull(resp.Error, "tools/list must not return an error");
            Assert.AreEqual(4, tools.Count, "MUST be exactly 4 tools (2 per service)");

            for (int i = 0; i < ExpectedUnion.Length; i++)
            {
                Assert.AreEqual(ExpectedUnion[i], tools[i]["name"]!.Value<string>(),
                    $"Position {i} tool name mismatch");
            }
        }

        [Test]
        public void Test1_NegativeControl_UnionIsNotInterleaved()
        {
            // Proves the ordering assertion is live: a wrong order would fail.
            var (tools, _) = InitAndList(MakeMuxer());
            var names = new List<string>();
            foreach (var t in tools) names.Add(t["name"]!.Value<string>()!);

            // Interleaved order must NOT match.
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
        // Test 2: tools/call routes to the correct owning service
        // ---------------------------------------------------------------------------

        [Test]
        public void Test2_Routing_UserService_GetUser_ChannelA_IsErrorFalse()
        {
            var mux = MakeMuxer();
            var session = InitedSession(mux);

            var resp = Call(mux, session, "UserService_getUser",
                new JObject { ["userId"] = "u1" });

            Assert.AreEqual(99, resp.Id!.Value<int>());
            Assert.IsNull(resp.Error, "Unexpected error on getUser call");

            var result = (JObject)resp.Result!;
            var content = (JArray)result["content"]!;
            Assert.AreEqual(1, content.Count, "content must have exactly one element");
            Assert.AreEqual("text", content[0]["type"]!.Value<string>());

            // isError MUST be false or absent.
            var isError = result["isError"];
            Assert.IsTrue(isError == null || isError.Value<bool>() == false,
                "isError must be false or absent for Channel-A success");

            // The JSON payload proves UserService handled it (profile.userId present).
            var payload = JToken.Parse(content[0]["text"]!.Value<string>()!);
            Assert.AreEqual("u1", payload["profile"]?["userId"]?.Value<string>(),
                "profile.userId must be 'u1'");
        }

        [Test]
        public void Test2_Routing_OrderService_CancelOrder_ChannelA_IsErrorFalse()
        {
            var mux = MakeMuxer();
            var session = InitedSession(mux);

            var resp = Call(mux, session, "OrderService_cancelOrder",
                new JObject { ["orderId"] = "o1", ["reason"] = JValue.CreateNull() });

            Assert.IsNull(resp.Error, "Unexpected error on cancelOrder call");

            var result = (JObject)resp.Result!;
            var content = (JArray)result["content"]!;
            Assert.AreEqual(1, content.Count, "content must have exactly one element");
            Assert.AreEqual("text", content[0]["type"]!.Value<string>());

            var isError = result["isError"];
            Assert.IsTrue(isError == null || isError.Value<bool>() == false,
                "isError must be false or absent for Channel-A success");

            // The JSON payload proves OrderService handled it (ok field present).
            var payload = JToken.Parse(content[0]["text"]!.Value<string>()!);
            Assert.AreEqual(true, payload["ok"]?.Value<bool>(), "ok must be true");
        }

        [Test]
        public void Test2_Routing_OrderService_PlaceOrder_DecoderFailure_ChannelB_IsErrorTrue()
        {
            // items:null triggers a NullReferenceException in the decoder (accessing
            // array elements on null), caught by the wiring as DecoderFailed -> Left
            // -> Channel-B (isError:true). Proves per-service Channel-B works through the muxer.
            var mux = MakeMuxer();
            var session = InitedSession(mux);

            var resp = Call(mux, session, "OrderService_placeOrder",
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
            // Second UserServiceMcpServer re-declares UserService_createUser / _getUser.
            BaboonMcpWiringException? thrown = null;
            try
            {
                _ = new AbstractMcpMuxer<object?>(
                    new McpServerInfo("Merged", "1.0.0"),
                    MakeUserServer(), MakeUserServer());
            }
            catch (BaboonMcpWiringException ex)
            {
                thrown = ex;
            }
            Assert.IsNotNull(thrown, "Ctor with colliding servers MUST throw BaboonMcpWiringException");
            Assert.AreEqual(BaboonMcpWiringErrorTag.DuplicateTool, thrown!.Error.Tag,
                "Error tag MUST be DuplicateTool");
            Assert.AreEqual("UserService_createUser", thrown.Error.ToolName,
                "First colliding tool name MUST be UserService_createUser");
        }

        [Test]
        public void Test3_DuplicateTool_CollisionOnRegister_ThrowsBaboonMcpWiringException()
        {
            var mux = new AbstractMcpMuxer<object?>(
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
        // Test 4: NoMatchingTool -> -32602 response
        // ---------------------------------------------------------------------------

        [Test]
        public void Test4_NoMatchingTool_UnknownToolName_Code32602()
        {
            var mux = MakeMuxer();
            var session = InitedSession(mux);

            var resp = Call(mux, session, "UserService_nope", new JObject());

            // -32602 surfaces as a JSON-RPC error (NOT a result).
            Assert.IsNotNull(resp.Error, "Unknown tool must produce a Channel-A error");
            Assert.IsNull(resp.Result, "No result expected for unknown tool");
            Assert.AreEqual(-32602, resp.Error!.Code,
                "Unknown tool error code MUST be -32602");
            Assert.IsNotEmpty(resp.Error.Message, "error.message must be non-empty");
        }
    }
}
