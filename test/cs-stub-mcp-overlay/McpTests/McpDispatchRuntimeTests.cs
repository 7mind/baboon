#nullable enable
using System.Collections.Generic;
using System.Threading.Tasks;
using Baboon.Runtime.Shared;
using Newtonsoft.Json.Linq;
using NUnit.Framework;

namespace McpTest
{
    [TestFixture]
    public class McpDispatchRuntimeTests
    {
        private sealed class SyncServer : AbstractBaboonMcpServer<int>
        {
            public override McpServerInfo ServerInfo => new("test", "1.0.0");
            public readonly List<McpToolEntry> Entries = new();
            public override IReadOnlyList<McpToolEntry> Tools => Entries;
            protected override Either<BaboonWiringError, string> InvokeJson(BaboonMethodId method, string data, int ctx, BaboonCodecContext codecCtx)
                => Either.Right<BaboonWiringError, string>($"{method.MethodName}:{data}:{ctx}");
        }

        private sealed class AsyncServer : AbstractBaboonMcpServerAsync<int>
        {
            public override McpServerInfo ServerInfo => new("test", "1.0.0");
            public readonly List<McpToolEntry> Entries = new();
            public override IReadOnlyList<McpToolEntry> Tools => Entries;
            protected override Task<Either<BaboonWiringError, string>> InvokeJson(BaboonMethodId method, string data, int ctx, BaboonCodecContext codecCtx)
                => Task.FromResult(Either.Right<BaboonWiringError, string>($"{method.MethodName}:{data}:{ctx}"));
        }

        private static McpToolEntry Tool(string name, string method)
            => new(name, new BaboonMethodId("svc", method), new JObject(), "description");

        [Test]
        public void OwnedRegistryCopiesMutableSchemasAndRetainsDeclarationOrder()
        {
            var input = new List<McpToolEntry> { Tool("first", "one"), Tool("second", "two") };
            var registry = new McpToolRegistry(input);
            input[0].InputSchema["changed"] = true;
            input.Clear();
            var snapshot = registry.Snapshot();
            snapshot[0].InputSchema["changed"] = true;
            Assert.That(registry.Snapshot()[0].InputSchema["changed"], Is.Null);
            Assert.That(registry.Snapshot()[1].Name, Is.EqualTo("second"));
            Assert.That(registry.TryFind("first", out var method), Is.True);
            Assert.That(method.MethodName, Is.EqualTo("one"));
            Assert.That(registry.TryFind("absent", out _), Is.False);
        }

        [Test]
        public async Task SyncAsyncServersAndMuxersHaveEquivalentProtocolResponses()
        {
            var sync = new SyncServer();
            var asyncServer = new AsyncServer();
            sync.Entries.Add(Tool("echo", "echo"));
            asyncServer.Entries.Add(Tool("echo", "echo"));
            var mux = new AbstractMcpMuxer<int>(sync.ServerInfo, sync);
            var asyncMux = new AbstractAsyncMcpMuxer<int>(sync.ServerInfo, asyncServer);
            var sessions = new[] { new McpSession(), new McpSession(), new McpSession(), new McpSession() };
            var requests = new[]
            {
                new JsonRpcRequest(new JValue(1), "tools/list", null),
                new JsonRpcRequest(new JValue(2), "initialize", new JObject()),
                new JsonRpcRequest(new JValue(3), "initialize", JObject.Parse("{\"protocolVersion\":\"old\"}")),
                new JsonRpcRequest(null, "notifications/initialized", null),
                new JsonRpcRequest(new JValue(4), "tools/list", null),
                new JsonRpcRequest(new JValue(5), "tools/call", new JObject()),
                new JsonRpcRequest(new JValue(6), "tools/call", JObject.Parse("{\"name\":\"absent\"}")),
                new JsonRpcRequest(new JValue(7), "tools/call", JObject.Parse("{\"name\":\"echo\",\"arguments\":{\"value\":42}}")),
                new JsonRpcRequest(new JValue(8), "unknown", null),
            };
            foreach (var request in requests)
            {
                var responses = new[]
                {
                    sync.Handle(request, sessions[0], 17, BaboonCodecContext.Compact),
                    await asyncServer.Handle(request, sessions[1], 17, BaboonCodecContext.Compact),
                    mux.Handle(request, sessions[2], 17, BaboonCodecContext.Compact),
                    await asyncMux.Handle(request, sessions[3], 17, BaboonCodecContext.Compact),
                };
                var expected = JToken.FromObject(new { Response = responses[0] });
                foreach (var response in responses)
                    Assert.That(JToken.DeepEquals(expected, JToken.FromObject(new { Response = response })), Is.True, request.Method);
                if (request.Id?.Value<int>() == 7)
                    Assert.That(responses[0]!.Result!["content"]![0]!["text"]!.Value<string>(), Is.EqualTo("echo:{\"value\":42}:17"));
            }
        }

        [Test]
        public void ExternalServerToolRegistryRemainsDynamicAndLastDuplicateWins()
        {
            var server = new SyncServer();
            server.Entries.Add(Tool("echo", "first"));
            var session = new McpSession { Initialized = true };
            var request = new JsonRpcRequest(new JValue(1), "tools/call", JObject.Parse("{\"name\":\"echo\"}"));
            Assert.That(server.Handle(request, session, 0, BaboonCodecContext.Compact)!.Result!.ToString(), Does.Contain("first:{}:0"));
            server.Entries.Add(Tool("echo", "last"));
            Assert.That(server.Handle(request, session, 0, BaboonCodecContext.Compact)!.Result!.ToString(), Does.Contain("last:{}:0"));
        }
    }
}
