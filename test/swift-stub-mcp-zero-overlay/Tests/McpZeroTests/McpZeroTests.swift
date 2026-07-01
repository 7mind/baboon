// T178 / D40 — Swift zero-service MCP overlay test (RED baseline).
//
// Purpose: prove the ZERO-SERVICE MCP runtime contract. The zero-service model
// (mcp-stub-zero-services-ok) declares @root types but NO RPC block, so
// servicesOf(domain) is empty and NO per-service `<Service>McpServer` is
// generated. With zero services the ONLY source of the MCP runtime types
// (AbstractMcpMuxer / AnyRoutableMcpServer / McpServerInfo / McpSession /
// JsonRpcRequest / JsonRpcResponse / jsonRpcErrorInvalidParams) is the STATIC
// runtime file `baboon_mcp_runtime.swift` (inside the BaboonRuntime module).
//
// RED (pre-fix): the current generator emits NO `baboon_mcp_runtime.swift` for a
// zero-service model, so the references below are unresolved and `swift test`
// FAILS to compile ("cannot find 'AbstractMcpMuxer' in scope"). That failure IS
// the D40 reproduction.
//
// GREEN (post-fix, later task): once the generator emits the static MCP runtime
// even when servicesOf(domain) is empty, this file compiles and the runtime
// assertions below pass — an empty muxer lists zero tools and rejects any
// tools/call with JSON-RPC -32602.
//
// Assertion discipline: explicit `XCTFail` + `return` / `throw` on failure —
// unconditional (Swift `assert` traps only in debug builds; XCTest assertions
// always evaluate).

import XCTest
import Foundation
import BaboonRuntime

final class McpZeroTests: XCTestCase {

    // COMPILE-TIME contract: constructing AbstractMcpMuxer<Ctx> with ZERO
    // registered servers requires the static runtime file to exist. With zero
    // services there is no generated <Service>McpServer — these types resolve
    // ONLY from baboon_mcp_runtime.swift.
    private func makeEmptyMuxer() throws -> AbstractMcpMuxer<Int> {
        return try AbstractMcpMuxer<Int>(McpServerInfo("ZeroEndpoint", "1.0.0"))
    }

    private func initedSession(_ mux: AbstractMcpMuxer<Int>) -> McpSession {
        let session = McpSession()
        _ = mux.handle(
            JsonRpcRequest(1, "initialize", [
                "protocolVersion": "2025-06-18",
                "capabilities": [String: Any](),
                "clientInfo": ["name": "t", "version": "0"],
            ] as [String: Any]),
            session, 0, BaboonCodecContext.defaultCtx
        )
        _ = mux.handle(
            JsonRpcRequest(nil, "notifications/initialized", nil),
            session, 0, BaboonCodecContext.defaultCtx
        )
        return session
    }

    // RUNTIME contract (a): tools/list on an empty muxer returns an empty array.
    func testToolsListIsEmpty() throws {
        let mux = try makeEmptyMuxer()
        let session = initedSession(mux)

        guard let resp = mux.handle(
            JsonRpcRequest(1, "tools/list", nil),
            session, 0, BaboonCodecContext.defaultCtx
        ) else {
            XCTFail("tools/list must return a response")
            return
        }
        XCTAssertNil(resp.error, "tools/list must not return an error")
        guard let result = resp.result as? [String: Any],
              let tools = result["tools"] as? [Any] else {
            XCTFail("tools/list result must carry a tools array")
            return
        }
        XCTAssertEqual(tools.count, 0, "empty muxer MUST list zero tools")
    }

    // RUNTIME contract (b): an unknown tool call yields JSON-RPC -32602.
    func testUnknownToolCallCode32602() throws {
        let mux = try makeEmptyMuxer()
        let session = initedSession(mux)

        guard let resp = mux.handle(
            JsonRpcRequest(2, "tools/call", [
                "name": "anything_at_all",
                "arguments": [String: Any](),
            ] as [String: Any]),
            session, 0, BaboonCodecContext.defaultCtx
        ) else {
            XCTFail("tools/call must return a response")
            return
        }
        XCTAssertNil(resp.result, "no result expected for unknown tool")
        guard let error = resp.error else {
            XCTFail("unknown tool on empty muxer MUST produce a Channel-A error")
            return
        }
        XCTAssertEqual(error.code, jsonRpcErrorInvalidParams,
                       "unknown-tool error code MUST be -32602 (InvalidParams)")
        XCTAssertFalse(error.message.isEmpty, "error.message must be non-empty")
    }
}
