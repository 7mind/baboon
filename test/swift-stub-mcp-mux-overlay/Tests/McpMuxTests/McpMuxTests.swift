// T113 — Swift MCP muxer round-trip overlay test (sync).
//
// Exercises `AbstractMcpMuxer<Ctx>` by composing two FRESHLY GENERATED
// `<Service>McpServer` instances produced from the T102 `mcp-mux-stub-ok/`
// model (UserService + OrderService). Composition is done strictly through the
// public T114 routable surface:
//   - the muxer ctor / register take `AnyRoutableMcpServer<Ctx>` (the Swift
//     type-eraser over the public serverInfo / tools / routeToolCall surface),
//   - the test NEVER subclasses a `<Service>McpServer`,
//   - the test NEVER calls a member server's own `handle(...)`.
//
// Ctx is `Void`: each member's `invokeJson` delegate closes over its own stub
// service impl, so the muxer's single shared `Ctx` carries nothing (the C#
// `object?` / Rust unit precedent). Generated code lands in Sources/McpMuxStub
// (the isolated dir set by the `test-gen-swift-mcp-mux` mdl action). No
// committed generated fixtures.
//
// Four asserted muxer behaviours (T113 acceptance):
//   1. tools/list -> UNION of both services' tools in registration-then-
//      declaration order;
//   2. tools/call -> routes the flat tool name to the correct owning service —
//      proven for a tool of EACH service (UserService_getUser,
//      OrderService_cancelOrder), Channel-A isError:false, plus a per-service
//      Channel-B (isError:true) through the muxer;
//   3. registering a server with a colliding tool name -> throws
//      BaboonMcpWiringException(.duplicateTool);
//   4. unknown tool name -> -32602 "unknown tool" response (NoMatchingTool).
//
// Assertion discipline (sw NO-ERRORS mode): Swift `assert(...)` is ELIDED in
// release/-Ounchecked builds and therefore VACUOUS. This suite uses ONLY XCTest
// assertions (`XCTAssert*`/`XCTFail`), which record failures UNCONDITIONALLY,
// plus `do/catch` for the duplicate-tool throw.

import XCTest
import Foundation
import BaboonRuntime
import McpMuxStub

final class McpMuxTests: XCTestCase {

    // -----------------------------------------------------------------------
    // Stub service impls: synchronous, return fixed values (no I/O).
    // -----------------------------------------------------------------------
    final class StubUserService: UserService {
        func createUser(arg: userservice.createuser.`in`) -> userservice.createuser.out {
            return userservice.createuser.out(profile: UserProfile(userId: "u1", email: "a@b.c", status: UserStatus.Active))
        }
        func getUser(arg: userservice.getuser.`in`) -> userservice.getuser.out {
            return userservice.getuser.out(profile: UserProfile(userId: "u1", email: "a@b.c", status: UserStatus.Active))
        }
    }

    final class StubOrderService: OrderService {
        func placeOrder(arg: orderservice.placeorder.`in`) -> orderservice.placeorder.out {
            return orderservice.placeorder.out(summary: OrderSummary(orderId: "o1", status: OrderStatus.Confirmed, total: 10.0))
        }
        func cancelOrder(arg: orderservice.cancelorder.`in`) -> orderservice.cancelorder.out {
            return orderservice.cancelorder.out(ok: true)
        }
    }

    // The union of both services' tools in registration-then-declaration order:
    // UserService registered first (createUser, getUser declared in that order),
    // OrderService second (placeOrder, cancelOrder).
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

    // The member servers compose ONLY via the public routable surface: the
    // delegate closes over the stub impl, so Ctx is Void. The eraser captures
    // serverInfo / tools / routeToolCall — never handle().
    private func makeUserServer() -> AnyRoutableMcpServer<Void> {
        let stub = StubUserService()
        let server = UserServiceMcpServer<Void> { method, data, _, ctx in
            return try UserServiceWiring.invokeJson(method, data, stub, ctx)
        }
        return AnyRoutableMcpServer<Void>(server)
    }

    private func makeOrderServer() -> AnyRoutableMcpServer<Void> {
        let stub = StubOrderService()
        let server = OrderServiceMcpServer<Void> { method, data, _, ctx in
            return try OrderServiceWiring.invokeJson(method, data, stub, ctx)
        }
        return AnyRoutableMcpServer<Void>(server)
    }

    private func makeMuxer() -> AbstractMcpMuxer<Void> {
        return try! AbstractMcpMuxer<Void>(
            McpServerInfo("MergedEndpoint", "1.0.0"),
            makeUserServer(), makeOrderServer())
    }

    private func initedSession(_ mux: AbstractMcpMuxer<Void>) -> McpSession {
        let session = McpSession()
        _ = mux.handle(
            JsonRpcRequest(0, "initialize", parseAny(#"{"protocolVersion":"2025-06-18","capabilities":{},"clientInfo":{"name":"t","version":"0"}}"#)),
            session, (), codecCtx)
        _ = mux.handle(JsonRpcRequest(nil, "notifications/initialized", nil), session, (), codecCtx)
        return session
    }

    private func listedTools(_ mux: AbstractMcpMuxer<Void>) -> (tools: [[String: Any]], resp: JsonRpcResponse) {
        let session = initedSession(mux)
        guard let resp = mux.handle(JsonRpcRequest(1, "tools/list", nil), session, (), codecCtx) else {
            XCTFail("tools/list must return a response")
            return ([], JsonRpcResponse(nil, error: JsonRpcError(-1, "no response")))
        }
        let result = resp.result as! [String: Any]
        return ((result["tools"] as! [[String: Any]]), resp)
    }

    private func call(_ mux: AbstractMcpMuxer<Void>, _ session: McpSession, _ toolName: String, _ argsJson: String) -> JsonRpcResponse {
        guard let resp = mux.handle(
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
    func test1_toolsList_unionInRegistrationOrder() {
        let (tools, resp) = listedTools(makeMuxer())
        XCTAssertNil(resp.error, "tools/list must not return an error")
        XCTAssertEqual(tools.count, 4, "MUST be exactly 4 tools (2 per service)")
        for (i, name) in expectedUnion.enumerated() {
            XCTAssertEqual(tools[i]["name"] as? String, name, "position \(i) tool name mismatch")
        }
        // No description key for any tool (the stub model has no doc comments).
        for t in tools {
            XCTAssertNil(t["description"], "tool \(t["name"] ?? "?") must have no description")
        }
    }

    func test1_negativeControl_unionIsNotInterleaved() {
        // Proves the ordering assertion is live: an interleaved order must NOT match.
        let (tools, _) = listedTools(makeMuxer())
        let names = tools.map { $0["name"] as! String }
        let interleaved = [
            "UserService_createUser",
            "OrderService_placeOrder",
            "UserService_getUser",
            "OrderService_cancelOrder",
        ]
        XCTAssertNotEqual(names, interleaved, "interleaved ordering must NOT match the actual union")
    }

    func test1_eachInputSchema_isWellFormedObjectWithDraftUri() {
        let (tools, _) = listedTools(makeMuxer())
        for t in tools {
            let schema = t["inputSchema"] as! [String: Any]
            XCTAssertEqual(schema["$schema"] as? String, "https://json-schema.org/draft/2020-12/schema",
                "tool \(t["name"] ?? "?"): $schema must be the Draft 2020-12 URI")
            XCTAssertTrue(JSONSerialization.isValidJSONObject(schema), "inputSchema must be a valid JSON object")
        }
    }

    // =======================================================================
    // Test 2 — tools/call routes to the correct owning service
    // =======================================================================
    func test2_routing_userService_getUser_channelA_isErrorFalse() {
        let mux = makeMuxer()
        let session = initedSession(mux)
        let resp = call(mux, session, "UserService_getUser", #"{"userId":"u1"}"#)

        XCTAssertEqual(resp.id as? Int, 99)
        XCTAssertNil(resp.error, "unexpected error on getUser call")
        let result = resp.result as! [String: Any]
        let content = result["content"] as! [[String: Any]]
        XCTAssertEqual(content.count, 1, "content must have exactly one element")
        XCTAssertEqual(content[0]["type"] as? String, "text")
        let isError = result["isError"]
        XCTAssertTrue(isError == nil || (isError as? Bool) == false, "isError must be false or absent")
        // The payload proves UserService handled it (profile.userId present).
        let payload = parseAny(content[0]["text"] as! String) as! [String: Any]
        let profile = payload["profile"] as! [String: Any]
        XCTAssertEqual(profile["userId"] as? String, "u1", "profile.userId must be 'u1'")
    }

    func test2_routing_orderService_cancelOrder_channelA_isErrorFalse() {
        let mux = makeMuxer()
        let session = initedSession(mux)
        let resp = call(mux, session, "OrderService_cancelOrder", #"{"orderId":"o1","reason":null}"#)

        XCTAssertNil(resp.error, "unexpected error on cancelOrder call")
        let result = resp.result as! [String: Any]
        let content = result["content"] as! [[String: Any]]
        XCTAssertEqual(content.count, 1, "content must have exactly one element")
        XCTAssertEqual(content[0]["type"] as? String, "text")
        let isError = result["isError"]
        XCTAssertTrue(isError == nil || (isError as? Bool) == false, "isError must be false or absent")
        // The payload proves OrderService handled it (ok field present).
        let payload = parseAny(content[0]["text"] as! String) as! [String: Any]
        XCTAssertEqual(payload["ok"] as? Bool, true, "ok must be true")
    }

    func test2_routing_orderService_placeOrder_decodeFailure_channelB_isErrorTrue() {
        // Channel-B trigger (sw-specific): a non-object element in `items`
        // (`[123]`) reaches OrderItem_JsonCodec.decode(ctx, 123), whose top-level
        // `wire as? [String: Any]` guard THROWS BaboonCodecError.invalidInput — a
        // catchable Swift Error. A missing-key trigger would TRAP (force-unwrap),
        // so it is deliberately avoided. This proves per-service Channel-B works
        // through the muxer (the muxer's generic catch maps any non-wiring error to
        // isError:true content).
        let mux = makeMuxer()
        let session = initedSession(mux)
        let resp = call(mux, session, "OrderService_placeOrder", #"{"userId":"u1","items":[123]}"#)

        XCTAssertNil(resp.error, "Channel-B must produce a result, not a JSON-RPC error")
        let result = resp.result as! [String: Any]
        XCTAssertEqual(result["isError"] as? Bool, true, "isError MUST be true for Channel-B decode failure")
        let content = result["content"] as! [[String: Any]]
        XCTAssertTrue(content.count > 0, "content must have at least one element")
        XCTAssertEqual(content[0]["type"] as? String, "text")
        XCTAssertFalse((content[0]["text"] as! String).isEmpty, "content[0].text must be non-empty")
    }

    // =======================================================================
    // Test 3 — DuplicateTool on collision (registration-time programmer error)
    // =======================================================================
    func test3_duplicateTool_collisionOnCtor_throws() {
        // Two UserService servers re-declare UserService_createUser / _getUser.
        var thrown: BaboonMcpWiringError? = nil
        do {
            _ = try AbstractMcpMuxer<Void>(
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
        let mux = try! AbstractMcpMuxer<Void>(McpServerInfo("Merged", "1.0.0"), makeUserServer())
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
    func test4_noMatchingTool_unknownToolName_code32602() {
        let mux = makeMuxer()
        let session = initedSession(mux)
        let resp = call(mux, session, "UserService_nope", "{}")

        XCTAssertNotNil(resp.error, "unknown tool must produce a Channel-A error")
        XCTAssertNil(resp.result, "no result expected for unknown tool")
        XCTAssertEqual(resp.error?.code, -32602, "unknown tool error code MUST be -32602")
        XCTAssertFalse((resp.error?.message ?? "").isEmpty, "error.message must be non-empty")
    }
}
