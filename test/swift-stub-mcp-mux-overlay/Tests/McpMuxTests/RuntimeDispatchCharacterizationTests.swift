import Foundation
import XCTest
import BaboonRuntime

private final class SyncProtocolProbe: IBaboonMcpServer {
    typealias Ctx = Int
    let serverInfo = McpServerInfo("probe", "1")
    var tools: [McpToolEntry]
    init(_ names: [String]) { tools = names.map { McpToolEntry($0, BaboonMethodId(serviceId: "probe", methodName: $0), [:], "description") } }
    func invokeJson(_ method: BaboonMethodId, _ data: String, _ ctx: Int, _ codecCtx: BaboonCodecContext) throws -> String {
        if method.methodName == "fail" { throw BaboonWiringException(.noMatchingMethod(method)) }
        return data
    }
}

private struct AsyncProtocolProbe: IBaboonAsyncMcpServer {
    typealias Ctx = Int
    let delegate: SyncProtocolProbe
    var serverInfo: McpServerInfo { delegate.serverInfo }
    var tools: [McpToolEntry] { delegate.tools }
    func invokeJson(_ method: BaboonMethodId, _ data: String, _ ctx: Int, _ codecCtx: BaboonCodecContext) async throws -> String {
        await Task.yield()
        return try delegate.invokeJson(method, data, ctx, codecCtx)
    }
}

final class RuntimeDispatchCharacterizationTests: XCTestCase {
    private func wire(_ response: JsonRpcResponse?) throws -> Data {
        guard let response else { return Data() }
        var value: [String: Any] = ["id": response.id ?? NSNull()]
        if let result = response.result { value["result"] = result }
        if let error = response.error { value["error"] = ["code": error.code, "message": error.message] }
        return try JSONSerialization.data(withJSONObject: value, options: [.sortedKeys])
    }

    @MainActor func testFourSurfacesHaveIdenticalProtocolResults() async throws {
        let sync = SyncProtocolProbe(["echo", "fail"])
        let async = AsyncProtocolProbe(delegate: sync)
        let mux = try AbstractMcpMuxer(sync.serverInfo, AnyRoutableMcpServer(sync))
        let asyncMux = try AbstractAsyncMcpMuxer(sync.serverInfo, AnyAsyncRoutableMcpServer(async))
        let sessions = (0..<4).map { _ in McpSession() }
        let requests = [
            JsonRpcRequest(1, "tools/list", nil), JsonRpcRequest(2, "tools/call", nil),
            JsonRpcRequest(3, "initialize", nil), JsonRpcRequest(4, "initialize", [:]),
            JsonRpcRequest(5, "initialize", ["protocolVersion": "probe"]),
            JsonRpcRequest(nil, "notifications/initialized", nil), JsonRpcRequest(6, "tools/list", nil),
            JsonRpcRequest(7, "tools/call", [:]), JsonRpcRequest(8, "tools/call", ["name": "missing"]),
            JsonRpcRequest(9, "tools/call", ["name": "echo", "arguments": ["b": 2, "a": 1]]),
            JsonRpcRequest(10, "tools/call", ["name": "fail"]), JsonRpcRequest(11, "unknown", nil),
        ]
        let expectedCodes: [Int?] = [-32600, -32600, -32602, -32602, nil, nil, nil, -32602, -32602, nil, nil, -32601]
        for (index, request) in requests.enumerated() {
            let a = sync.handle(request, sessions[0], 0, .compact)
            let b = await async.handle(request, sessions[1], 0, .compact)
            let c = mux.handle(request, sessions[2], 0, .compact)
            let d = await asyncMux.handle(request, sessions[3], 0, .compact)
            XCTAssertEqual(a?.error?.code, expectedCodes[index])
            XCTAssertEqual(try wire(a), try wire(b))
            XCTAssertEqual(try wire(a), try wire(c))
            XCTAssertEqual(try wire(a), try wire(d))
            XCTAssertTrue(sessions.allSatisfy { $0.initialized == sessions[0].initialized })
        }
    }

    func testDynamicConformerToolChangesAreVisible() {
        let server = SyncProtocolProbe(["old"])
        let session = McpSession()
        session.initialized = true
        server.tools = SyncProtocolProbe(["new"]).tools
        XCTAssertNil(server.handle(JsonRpcRequest(1, "tools/call", ["name": "new"]), session, 0, .compact)?.error)
        XCTAssertEqual(server.handle(JsonRpcRequest(2, "tools/call", ["name": "old"]), session, 0, .compact)?.error?.code, -32602)
    }

    func testRejectedRegistrationDoesNotPublishPartialTools() throws {
        let first = SyncProtocolProbe(["existing"])
        let mux = try AbstractMcpMuxer(first.serverInfo, AnyRoutableMcpServer(first))
        XCTAssertThrowsError(try mux.register(AnyRoutableMcpServer(SyncProtocolProbe(["new", "existing"]))))
        let session = McpSession()
        session.initialized = true
        XCTAssertEqual(mux.handle(JsonRpcRequest(1, "tools/call", ["name": "new"]), session, 0, .compact)?.error?.code, -32602)
    }

    func testAsyncRejectedRegistrationDoesNotPublishPartialTools() async throws {
        let first = AsyncProtocolProbe(delegate: SyncProtocolProbe(["existing"]))
        let mux = try AbstractAsyncMcpMuxer(first.serverInfo, AnyAsyncRoutableMcpServer(first))
        XCTAssertThrowsError(try mux.register(AnyAsyncRoutableMcpServer(AsyncProtocolProbe(delegate: SyncProtocolProbe(["new", "existing"])))))
        let session = McpSession()
        session.initialized = true
        let response = await mux.handle(JsonRpcRequest(1, "tools/call", ["name": "new"]), session, 0, .compact)
        XCTAssertEqual(response?.error?.code, -32602)
    }
}
