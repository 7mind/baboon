// T113 — Swift ASYNC MCP muxer round-trip overlay test.
//
// Async sibling of `test/swift-stub-mcp-mux-overlay/Tests/McpMuxTests/McpMuxTests.swift`.
//
// Generated with BOTH `--sw-generate-mcp-server=true` AND
// `--sw-async-services=true`. Under the async axis the generated
// `<Svc>Wiring.invokeJson` is `async throws -> String` and the generated MCP
// servers conform to `IBaboonAsyncMcpServer` (whose `routeToolCall` is
// `async throws` and `handle` is GENUINELY `async`, awaiting the delegate
// directly in the caller's task — no synchronous bridge). The muxer therefore
// composes `AnyAsyncRoutableMcpServer<Ctx>` members behind
// `AbstractAsyncMcpMuxer<Ctx>` (`async handle`); the single `tools/call` hop
// awaits the owning server's `routeToolCall` before applying Channel-A/B.
//
// The registration / union-table build (duplicateTool on collision) is identical
// to the sync muxer and stays synchronous (a registration-time programmer error,
// not a per-request await).
//
// Same four asserted behaviours as the sync lane, plus an @MainActor-isolated
// round-trip (the genuinely-async handle suspends cooperatively instead of
// blocking a thread, so an actor-isolated caller completes — the round-1 D24
// deadlock regression for the async MCP surface).
//
// Ctx is `Void`: each member's delegate closes over its own stub impl. Assertion
// discipline: ONLY XCTest assertions (record failures unconditionally), never the
// vacuous `assert`.

import XCTest
import Foundation
import BaboonRuntime
import McpMuxStub

final class McpMuxTests: XCTestCase {

    // Under `--sw-async-services=true` the generated service protocols declare
    // each method as `async throws`, so the stub impls match; every method
    // returns a fixed value (no I/O).
    final class StubUserService: UserService {
        func createUser(arg: userservice.createuser.`in`) async throws -> userservice.createuser.out {
            return userservice.createuser.out(profile: UserProfile(userId: "u1", email: "a@b.c", status: UserStatus.Active))
        }
        func getUser(arg: userservice.getuser.`in`) async throws -> userservice.getuser.out {
            return userservice.getuser.out(profile: UserProfile(userId: "u1", email: "a@b.c", status: UserStatus.Active))
        }
    }

    final class StubOrderService: OrderService {
        func placeOrder(arg: orderservice.placeorder.`in`) async throws -> orderservice.placeorder.out {
            return orderservice.placeorder.out(summary: OrderSummary(orderId: "o1", status: OrderStatus.Confirmed, total: 10.0))
        }
        func cancelOrder(arg: orderservice.cancelorder.`in`) async throws -> orderservice.cancelorder.out {
            return orderservice.cancelorder.out(ok: true)
        }
    }

    private let expectedUnion = [
        "UserService_createUser",
        "UserService_getUser",
        "OrderService_placeOrder",
        "OrderService_cancelOrder",
    ]

    private let codecCtx = BaboonCodecContext.defaultCtx

    private func parseAny(_ json: String) -> Any {
        let data = json.data(using: .utf8)!
        return try! JSONSerialization.jsonObject(with: data, options: [.fragmentsAllowed])
    }

    // Members compose ONLY via the public async routable surface: the delegate is
    // `async throws` and closes over the stub impl (Ctx = Void). The eraser
    // captures serverInfo / tools / routeToolCall — never handle().
    private func makeUserServer() -> AnyAsyncRoutableMcpServer<Void> {
        let stub = StubUserService()
        let server = UserServiceMcpServer<Void> { method, data, _, ctx in
            return try await UserServiceWiring.invokeJson(method, data, stub, ctx)
        }
        return AnyAsyncRoutableMcpServer<Void>(server)
    }

    private func makeOrderServer() -> AnyAsyncRoutableMcpServer<Void> {
        let stub = StubOrderService()
        let server = OrderServiceMcpServer<Void> { method, data, _, ctx in
            return try await OrderServiceWiring.invokeJson(method, data, stub, ctx)
        }
        return AnyAsyncRoutableMcpServer<Void>(server)
    }

    private func makeMuxer() -> AbstractAsyncMcpMuxer<Void> {
        return try! AbstractAsyncMcpMuxer<Void>(
            McpServerInfo("MergedEndpoint", "1.0.0"),
            makeUserServer(), makeOrderServer())
    }

    private func initedSession(_ mux: AbstractAsyncMcpMuxer<Void>) async -> McpSession {
        let session = McpSession()
        _ = await mux.handle(
            JsonRpcRequest(0, "initialize", parseAny(#"{"protocolVersion":"2025-06-18","capabilities":{},"clientInfo":{"name":"t","version":"0"}}"#)),
            session, (), codecCtx)
        _ = await mux.handle(JsonRpcRequest(nil, "notifications/initialized", nil), session, (), codecCtx)
        return session
    }

    private func listedTools(_ mux: AbstractAsyncMcpMuxer<Void>) async -> (tools: [[String: Any]], resp: JsonRpcResponse) {
        let session = await initedSession(mux)
        guard let resp = await mux.handle(JsonRpcRequest(1, "tools/list", nil), session, (), codecCtx) else {
            XCTFail("tools/list must return a response")
            return ([], JsonRpcResponse(nil, error: JsonRpcError(-1, "no response")))
        }
        let result = resp.result as! [String: Any]
        return ((result["tools"] as! [[String: Any]]), resp)
    }

    private func call(_ mux: AbstractAsyncMcpMuxer<Void>, _ session: McpSession, _ toolName: String, _ argsJson: String) async -> JsonRpcResponse {
        guard let resp = await mux.handle(
            JsonRpcRequest(99, "tools/call", parseAny("{\"name\":\"\(toolName)\",\"arguments\":\(argsJson)}")),
            session, (), codecCtx) else {
            XCTFail("tools/call '\(toolName)' must return a response")
            return JsonRpcResponse(nil, error: JsonRpcError(-1, "no response"))
        }
        return resp
    }

    // =======================================================================
    // Test 1 — tools/list returns the UNION in registration-then-declaration order
    // =======================================================================
    func test1_toolsList_unionInRegistrationOrder() async {
        let (tools, resp) = await listedTools(makeMuxer())
        XCTAssertNil(resp.error, "tools/list must not return an error")
        XCTAssertEqual(tools.count, 4, "MUST be exactly 4 tools (2 per service)")
        for (i, name) in expectedUnion.enumerated() {
            XCTAssertEqual(tools[i]["name"] as? String, name, "position \(i) tool name mismatch")
        }
        for t in tools {
            XCTAssertNil(t["description"], "tool \(t["name"] ?? "?") must have no description")
        }
    }

    func test1_negativeControl_unionIsNotInterleaved() async {
        let (tools, _) = await listedTools(makeMuxer())
        let names = tools.map { $0["name"] as! String }
        let interleaved = [
            "UserService_createUser",
            "OrderService_placeOrder",
            "UserService_getUser",
            "OrderService_cancelOrder",
        ]
        XCTAssertNotEqual(names, interleaved, "interleaved ordering must NOT match the actual union")
    }

    // =======================================================================
    // Test 2 — tools/call routes to the correct owning service
    // =======================================================================
    func test2_routing_userService_getUser_channelA_isErrorFalse() async {
        let mux = makeMuxer()
        let session = await initedSession(mux)
        let resp = await call(mux, session, "UserService_getUser", #"{"userId":"u1"}"#)

        XCTAssertNil(resp.error, "unexpected error on getUser call")
        let result = resp.result as! [String: Any]
        let content = result["content"] as! [[String: Any]]
        XCTAssertEqual(content.count, 1, "content must have exactly one element")
        let isError = result["isError"]
        XCTAssertTrue(isError == nil || (isError as? Bool) == false, "isError must be false or absent")
        let payload = parseAny(content[0]["text"] as! String) as! [String: Any]
        let profile = payload["profile"] as! [String: Any]
        XCTAssertEqual(profile["userId"] as? String, "u1", "profile.userId must be 'u1'")
    }

    func test2_routing_orderService_cancelOrder_channelA_isErrorFalse() async {
        let mux = makeMuxer()
        let session = await initedSession(mux)
        let resp = await call(mux, session, "OrderService_cancelOrder", #"{"orderId":"o1","reason":null}"#)

        XCTAssertNil(resp.error, "unexpected error on cancelOrder call")
        let result = resp.result as! [String: Any]
        let content = result["content"] as! [[String: Any]]
        XCTAssertEqual(content.count, 1, "content must have exactly one element")
        let isError = result["isError"]
        XCTAssertTrue(isError == nil || (isError as? Bool) == false, "isError must be false or absent")
        let payload = parseAny(content[0]["text"] as! String) as! [String: Any]
        XCTAssertEqual(payload["ok"] as? Bool, true, "ok must be true")
    }

    func test2_routing_orderService_placeOrder_decodeFailure_channelB_isErrorTrue() async {
        // Channel-B trigger (sw-specific): a non-object element in `items`
        // (`[123]`) reaches OrderItem_JsonCodec.decode(ctx, 123), whose guard
        // THROWS BaboonCodecError.invalidInput (catchable). Proves per-service
        // Channel-B works through the async muxer (its generic catch maps any
        // non-wiring error to isError:true content after the single await hop).
        let mux = makeMuxer()
        let session = await initedSession(mux)
        let resp = await call(mux, session, "OrderService_placeOrder", #"{"userId":"u1","items":[123]}"#)

        XCTAssertNil(resp.error, "Channel-B must produce a result, not a JSON-RPC error")
        let result = resp.result as! [String: Any]
        XCTAssertEqual(result["isError"] as? Bool, true, "isError MUST be true for Channel-B decode failure")
        let content = result["content"] as! [[String: Any]]
        XCTAssertTrue(content.count > 0, "content must have at least one element")
        XCTAssertFalse((content[0]["text"] as! String).isEmpty, "content[0].text must be non-empty")
    }

    // =======================================================================
    // Test 2b — @MainActor-isolated round-trip.
    //
    // The genuinely-async `handle` suspends cooperatively rather than blocking a
    // thread, so an actor-isolated caller completes (the D24 deadlock regression
    // for the async MCP surface, here at the muxer tier).
    // =======================================================================
    @MainActor
    func test2c_routing_fromMainActor_completes() async {
        let mux = makeMuxer()
        let session = await initedSession(mux)
        let resp = await call(mux, session, "UserService_getUser", #"{"userId":"u1"}"#)
        XCTAssertNil(resp.error, "unexpected error on getUser call from @MainActor")
        let result = resp.result as! [String: Any]
        let content = result["content"] as! [[String: Any]]
        let payload = parseAny(content[0]["text"] as! String) as! [String: Any]
        let profile = payload["profile"] as! [String: Any]
        XCTAssertEqual(profile["userId"] as? String, "u1", "profile.userId must be 'u1'")
    }

    // =======================================================================
    // Test 3 — DuplicateTool on collision (synchronous registration-time error)
    // =======================================================================
    func test3_duplicateTool_collisionOnCtor_throws() {
        var thrown: BaboonMcpWiringError? = nil
        do {
            _ = try AbstractAsyncMcpMuxer<Void>(
                McpServerInfo("Merged", "1.0.0"),
                makeUserServer(), makeUserServer())
        } catch let e as BaboonMcpWiringException {
            thrown = e.error
        } catch {
            XCTFail("unexpected error type: \(error)")
        }
        guard let err = thrown else {
            XCTFail("ctor with colliding servers MUST throw BaboonMcpWiringException")
            return
        }
        guard case .duplicateTool(let name) = err else {
            XCTFail("error MUST be .duplicateTool, got \(err)")
            return
        }
        XCTAssertEqual(name, "UserService_createUser", "first colliding tool name MUST be UserService_createUser")
    }

    func test3_duplicateTool_collisionOnRegister_throws() {
        let mux = try! AbstractAsyncMcpMuxer<Void>(McpServerInfo("Merged", "1.0.0"), makeUserServer())
        var thrown: BaboonMcpWiringError? = nil
        do {
            try mux.register(makeUserServer())
        } catch let e as BaboonMcpWiringException {
            thrown = e.error
        } catch {
            XCTFail("unexpected error type: \(error)")
        }
        guard let err = thrown else {
            XCTFail("register with colliding server MUST throw BaboonMcpWiringException")
            return
        }
        guard case .duplicateTool = err else {
            XCTFail("error MUST be .duplicateTool, got \(err)")
            return
        }
    }

    // =======================================================================
    // Test 4 — NoMatchingTool -> -32602 response (unknown flat tool name)
    // =======================================================================
    func test4_noMatchingTool_unknownToolName_code32602() async {
        let mux = makeMuxer()
        let session = await initedSession(mux)
        let resp = await call(mux, session, "UserService_nope", "{}")

        XCTAssertNotNil(resp.error, "unknown tool must produce a Channel-A error")
        XCTAssertNil(resp.result, "no result expected for unknown tool")
        XCTAssertEqual(resp.error?.code, -32602, "unknown tool error code MUST be -32602")
        XCTAssertFalse((resp.error?.message ?? "").isEmpty, "error.message must be non-empty")
    }
}
