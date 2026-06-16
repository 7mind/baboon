// T66 — Swift ASYNC-MCP round-trip overlay test (D24 / G11).
//
// Async sibling of `test/swift-stub-mcp-overlay/Tests/McpTests/McpTests.swift`.
//
// This lane is generated with BOTH `--sw-generate-mcp-server=true` AND
// `--sw-async-services=true`. It is a DELIBERATE RED reproduction of the
// async-MCP wiring mismatch (D24), gating the Swift async-MCP backend fix (T67):
//
//   - Under `--sw-async-services=true`, the generated no-errors service-wiring
//     dispatcher `McpToolsWiring.invokeJson(...)` is emitted as `async throws`
//     (SwServiceWiringTranslator.scala:43 `dispatcherEffects`, applied at the
//     `invokeJson` declaration, :508-513). Its body `try await impl.<method>(...)`
//     only type-checks inside an async context, and a call to it evaluates to an
//     async expression that requires `await`.
//
//   - The generated MCP server, however, still binds its `invokeJson` delegate
//     to the SYNC closure type `(BaboonMethodId, String, Ctx, BaboonCodecContext)
//     throws -> String` (SwMcpServerGenerator.scala:125 stored property /
//     :127 initializer parameter). That is a SYNCHRONOUS, non-async closure: its
//     body must evaluate to a `String` without `await`.
//
// Therefore the `makeServer()` binding below — the async analogue of the sync
// overlay's binding, which there compiles cleanly because `McpToolsWiring
// .invokeJson` is sync `throws` — cannot type-check: a `try await` async call is
// supplied where a non-async `throws -> String` closure is required. swiftc
// rejects it with an async/sync mismatch ("'async' call in a function that does
// not support concurrency" / "invalid conversion from 'async' function ... to
// synchronous function type" / "cannot pass function of type ... 'async' ... to
// parameter expecting synchronous function type").
//
// EXPECTED RED: `swift build` / `swift test` of this overlay MUST FAIL at the
// makeServer binding with that async/sync mismatch — NOT an unrelated error.
// The sync `test-swift-mcp` lane is untouched and still passes.
//
// The async-MCP backend fix (T67) must make the generated MCP server able to
// hold the async wiring dispatcher (an async-capable delegate type whose
// `handle` drives the async to completion, mirroring the Rust block_on path);
// only then does this binding compile and this lane go green.

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
    // DELIBERATE RED (D24): `McpToolsWiring.invokeJson` is `async throws -> String`
    // under `--sw-async-services=true`, so the closure body needs `try await`.
    // But `McpToolsMcpServer.init` requires a SYNC `throws -> String` closure
    // (SwMcpServerGenerator.scala:127). The async call cannot inhabit the
    // non-async closure type — this binding does NOT compile. (The sync overlay's
    // identical binding compiles because there `invokeJson` is sync `throws`.)
    private func makeServer(_ impl: McpTools) -> McpToolsMcpServer<McpTools> {
        return McpToolsMcpServer<McpTools> { method, data, stub, codecCtx in
            return try await McpToolsWiring.invokeJson(method, data, stub, codecCtx)
        }
    }

    private func parseAny(_ json: String) -> Any {
        let data = json.data(using: .utf8)!
        return try! JSONSerialization.jsonObject(with: data, options: [.fragmentsAllowed])
    }

    private func initSession(_ server: McpToolsMcpServer<McpTools>, _ session: McpSession, _ impl: McpTools) {
        _ = server.handle(
            JsonRpcRequest(1, "initialize", parseAny(#"{"protocolVersion":"2025-06-18","capabilities":{},"clientInfo":{"name":"test-client","version":"0.0.1"}}"#)),
            session, impl, BaboonCodecContext.defaultCtx
        )
        _ = server.handle(
            JsonRpcRequest(nil, "notifications/initialized", nil),
            session, impl, BaboonCodecContext.defaultCtx
        )
    }

    private func send(_ server: McpToolsMcpServer<McpTools>, _ session: McpSession, _ impl: McpTools, _ req: JsonRpcRequest) -> JsonRpcResponse {
        guard let resp = server.handle(req, session, impl, BaboonCodecContext.defaultCtx) else {
            XCTFail("Expected a response for \"\(req.method)\" but got nil")
            return JsonRpcResponse(nil, error: JsonRpcError(-1, "no response"))
        }
        return resp
    }

    // =======================================================================
    // §1 — initialize + tools/call round-trip.
    //
    // If the async-MCP fix (T67) lands, this drives the canonical round-trip;
    // until then the overlay does not compile (the makeServer binding above is
    // rejected) and this test never runs. The lane's RED state is asserted at
    // COMPILE time, not in this body.
    // =======================================================================
    func test_async_mcp_initialize_and_ping_roundtrip() {
        let stub = StubMcpTools()
        let server = makeServer(stub)
        let session = McpSession()
        initSession(server, session, stub)

        let resp = send(server, session, stub, JsonRpcRequest(3, "tools/call",
            parseAny(#"{"name":"McpTools_ping","arguments":{"seqno":42,"label":"hello"}}"#)))

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
}
