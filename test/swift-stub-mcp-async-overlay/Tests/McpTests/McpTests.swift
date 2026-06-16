// T66/T67 — Swift ASYNC-MCP round-trip overlay test (D24 / G11).
//
// Async sibling of `test/swift-stub-mcp-overlay/Tests/McpTests/McpTests.swift`.
//
// This lane is generated with BOTH `--sw-generate-mcp-server=true` AND
// `--sw-async-services=true`. T66 added it RED (the generated MCP server bound a
// SYNC `throws -> String` delegate, incompatible with the `async throws`
// `McpToolsWiring.invokeJson`); T67 turns it GREEN by threading `asyncServices`
// into the generator + runtime:
//
//   - Under `--sw-async-services=true`, the generated no-errors service-wiring
//     dispatcher `McpToolsWiring.invokeJson(...)` is `async throws -> String`
//     (SwServiceWiringTranslator.scala:43 `dispatcherEffects`, :508-513).
//
//   - The generated MCP server now binds an `async throws -> String` delegate
//     and conforms to `IBaboonAsyncMcpServer`, whose `handle` is GENUINELY
//     `async` and `await`s the delegate DIRECTLY in the caller's task (no
//     synchronous `DispatchSemaphore` bridge). The adapter calls
//     `await server.handle(...)`.
//
// Two round-trips below exercise the green path: one from a non-isolated caller
// (§1) and one from an actor-isolated `@MainActor` caller (§2). §2 is the
// regression for the round-1 deadlock: the prior synchronous bridge parked the
// calling thread on a semaphore while a context-inheriting `Task` waited on the
// same actor's executor — a permanent deadlock from `@MainActor`. The
// genuinely-async `handle` suspends cooperatively instead of blocking a thread,
// so an actor-isolated caller completes. The sync `test-swift-mcp` lane is
// untouched and still passes; its output is byte-identical to baseline.

import XCTest
import Foundation
import BaboonRuntime
import McpStub

final class McpTests: XCTestCase {

    // -----------------------------------------------------------------------
    // Stub McpTools service. Under `--sw-async-services=true` the generated
    // `McpTools` protocol declares each method as `async throws`
    // (SwDefnTranslator.scala:966/1029), so the stub implements them as
    // `async throws`; every method returns ok=true (T7 §3 convention).
    // -----------------------------------------------------------------------
    final class StubMcpTools: McpTools {
        func listCollections(arg: mcptools.listcollections.`in`) async throws -> mcptools.listcollections.out {
            return mcptools.listcollections.out(ok: true)
        }
        func submitComposite(arg: mcptools.submitcomposite.`in`) async throws -> mcptools.submitcomposite.out {
            return mcptools.submitcomposite.out(ok: true)
        }
        func processShape(arg: mcptools.processshape.`in`) async throws -> mcptools.processshape.out {
            return mcptools.processshape.out(ok: true)
        }
        func processTagged(arg: mcptools.processtagged.`in`) async throws -> mcptools.processtagged.out {
            return mcptools.processtagged.out(ok: true)
        }
        func pagePoints(arg: mcptools.pagepoints.`in`) async throws -> mcptools.pagepoints.out {
            return mcptools.pagepoints.out(ok: true)
        }
        func ping(arg: mcptools.ping.`in`) async throws -> mcptools.ping.out {
            return mcptools.ping.out(ok: true)
        }
    }

    // Server factory: Ctx = McpTools (the service impl), so the no-errors
    // McpToolsWiring.invokeJson is used directly as the delegate — exactly as in
    // the sync overlay.
    //
    // Under `--sw-async-services=true` (T67 fix) `McpToolsWiring.invokeJson` is
    // `async throws -> String` and `McpToolsMcpServer.init` accepts an
    // `async throws -> String` delegate closure, so the `try await` body below
    // type-checks. (The sync overlay's identical binding compiles because there
    // both sides are sync `throws`.)
    private func makeServer(_ impl: McpTools) -> McpToolsMcpServer<McpTools> {
        return McpToolsMcpServer<McpTools> { method, data, stub, codecCtx in
            return try await McpToolsWiring.invokeJson(method, data, stub, codecCtx)
        }
    }

    private func parseAny(_ json: String) -> Any {
        let data = json.data(using: .utf8)!
        return try! JSONSerialization.jsonObject(with: data, options: [.fragmentsAllowed])
    }

    private func initSession(_ server: McpToolsMcpServer<McpTools>, _ session: McpSession, _ impl: McpTools) async {
        _ = await server.handle(
            JsonRpcRequest(1, "initialize", parseAny(#"{"protocolVersion":"2025-06-18","capabilities":{},"clientInfo":{"name":"test-client","version":"0.0.1"}}"#)),
            session, impl, BaboonCodecContext.defaultCtx
        )
        _ = await server.handle(
            JsonRpcRequest(nil, "notifications/initialized", nil),
            session, impl, BaboonCodecContext.defaultCtx
        )
    }

    private func send(_ server: McpToolsMcpServer<McpTools>, _ session: McpSession, _ impl: McpTools, _ req: JsonRpcRequest) async -> JsonRpcResponse {
        guard let resp = await server.handle(req, session, impl, BaboonCodecContext.defaultCtx) else {
            XCTFail("Expected a response for \"\(req.method)\" but got nil")
            return JsonRpcResponse(nil, error: JsonRpcError(-1, "no response"))
        }
        return resp
    }

    // Shared assertion over a `tools/call` ping response (reused by the
    // non-isolated and the @MainActor-isolated round-trips).
    private func assertPingOk(_ resp: JsonRpcResponse) {
        XCTAssertNil(resp.error, "Unexpected error on ping call")
        let result = resp.result as! [String: Any]
        let content = result["content"] as! [[String: Any]]
        XCTAssertEqual(content.count, 1, "content must have exactly one element")
        XCTAssertEqual(content[0]["type"] as? String, "text")
        let payload = parseAny(content[0]["text"] as! String) as! [String: Any]
        XCTAssertEqual(payload["ok"] as? Bool, true, "ok must be true")
        let isError = result["isError"]
        XCTAssertTrue(isError == nil || (isError as? Bool) == false, "isError must be false or absent")
    }

    // =======================================================================
    // §1 — initialize + tools/call round-trip (non-isolated caller).
    //
    // With the async-MCP fix (T67) landed, `handle` is genuinely `async`: the
    // async delegate is awaited directly in the caller's task (no semaphore
    // bridge), so this drives the canonical round-trip.
    // =======================================================================
    func test_async_mcp_initialize_and_ping_roundtrip() async {
        let stub = StubMcpTools()
        let server = makeServer(stub)
        let session = McpSession()
        await initSession(server, session, stub)

        let resp = await send(server, session, stub, JsonRpcRequest(3, "tools/call",
            parseAny(#"{"name":"McpTools_ping","arguments":{"seqno":42,"label":"hello"}}"#)))

        assertPingOk(resp)
    }

    // =======================================================================
    // §2 — actor-isolated (@MainActor) caller round-trip (D24/T67 regression).
    //
    // This test body runs ON the main actor and `await`s the SAME async
    // `handle`. The prior round's synchronous bridge (a context-inheriting
    // `Task` parked behind a `DispatchSemaphore`) DEADLOCKED here: the inherited
    // task was scheduled on the main actor's executor, which the blocked thread
    // held, so `signal()` never ran. The genuinely-async `handle` suspends the
    // caller's task cooperatively instead of blocking a thread, so an
    // actor-isolated caller completes. A deadlock would manifest as the whole
    // test process hanging until the XCTest timeout — the assertion below would
    // never be reached.
    // =======================================================================
    @MainActor
    func test_async_mcp_ping_roundtrip_from_main_actor() async {
        let stub = StubMcpTools()
        let server = makeServer(stub)
        let session = McpSession()
        await initSession(server, session, stub)

        let resp = await send(server, session, stub, JsonRpcRequest(3, "tools/call",
            parseAny(#"{"name":"McpTools_ping","arguments":{"seqno":7,"label":"main-actor"}}"#)))

        assertPingOk(resp)
    }
}
