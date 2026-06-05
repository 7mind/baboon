// T17 — Swift MCP round-trip overlay test.
//
// Drives the generated McpToolsMcpServer<McpTools> through the canonical T7
// scenario (docs/research/mcp-roundtrip-scenario.md) using an entirely
// in-process delegate. No stdio or HTTP is involved — the MCP dispatch surface
// is transport-abstract (K4 §Q2 / K5).
//
// Assertion discipline (T7 §5.1):
//   - Swift `assert(...)`/`precondition` — `assert` is ELIDED in release/-Ounchecked
//     builds and therefore VACUOUS. This suite uses ONLY XCTest assertions
//     (`XCTAssert*`/`XCTFail`, which throw/record unconditionally) plus a
//     hand-rolled `requireTrue(...)` built on `XCTFail` for the negative control.
//
// JSONSerialization JSON Schema validation tier (K1 — T17 tier):
//   Part (a) — well-formedness: each returned inputSchema is re-serialized and
//     re-parsed via `JSONSerialization` (well-formedness gate).
//   Part (b) — structural equality: each returned inputSchema is asserted
//     STRUCTURALLY EQUAL to the corresponding T7 §2.3 reference literal embedded
//     below (NOT a self-round-trip). `required` arrays are compared as SETS per
//     §5.4; objects are compared key-order-insensitively via a recursive deep
//     comparator.
//
// Negative controls (T7 §5.2):
//   - §4.1 (unknown tool → -32602): if the server returned success for
//     McpTools_nonexistent the assertions would fail.
//   - §4.2 (malformed decode → Channel-B isError=true): if isError were false
//     this test would fail.
//   - Schema structural-equality negative control (k1_negativeControl): a
//     deliberately-wrong reference MUST make the comparator return false; the
//     test FAILS if the comparator erroneously returns true, then re-verifies
//     the correct reference still returns true (restore).
//
// Channel-B trigger (§4.2 — Swift-specific): Swift `!` force-unwrap of a missing
//   dictionary key TRAPS (non-catchable) rather than throwing, so the
//   "missing required field" trigger used by the GC-language replicas would
//   crash the process here. Instead we send `McpTools_submitComposite` with a
//   non-object `nested` value (`"nested": 123`): the generated
//   submitcomposite.in decode reaches `Nested_JsonCodec.decode(ctx, 123)`, whose
//   top-level `wire as? [String: Any]` guard fails and THROWS
//   `BaboonCodecError.invalidInput` — a catchable Swift `Error`. That is exactly
//   the K4 Channel-B "arguments is an object but won't decode into the DTO" case.

import XCTest
import Foundation
import BaboonRuntime
import McpStub

final class McpTests: XCTestCase {

    // -----------------------------------------------------------------------
    // T7 §2.3 reference inputSchema values (authoritative: McpInputSchemaEmitter
    // golden test McpInputSchemaEmissionTest.scala + mcp-roundtrip-scenario.md).
    // Embedded as JSON-text literals and parsed via JSONSerialization to the same
    // [String: Any] shape the server returns, then deep-compared.
    // -----------------------------------------------------------------------

    private let refListCollections = ##"{"$schema":"https://json-schema.org/draft/2020-12/schema","type":"object","properties":{"tags":{"type":"array","items":{"type":"string"}},"uniqueIds":{"type":"array","items":{"type":"integer","format":"int64"},"uniqueItems":true},"labels":{"type":"object","additionalProperties":{"type":"string"}},"byColor":{"type":"array","items":{"type":"object","required":["key","value"],"properties":{"key":{"$ref":"#/$defs/mcp_stub_Color"},"value":{"type":"string"}}}}},"required":["tags","uniqueIds","labels","byColor"],"$defs":{"mcp_stub_Color":{"type":"string","enum":["Red","Green","Blue"]}}}"##

    private let refSubmitComposite = ##"{"$schema":"https://json-schema.org/draft/2020-12/schema","type":"object","properties":{"nested":{"$ref":"#/$defs/mcp_stub_Nested"},"maybePoint":{"oneOf":[{"$ref":"#/$defs/mcp_stub_Point"},{"type":"null"}]},"color":{"$ref":"#/$defs/mcp_stub_Color"},"fancy":{"type":"string"}},"required":["nested","color","fancy"],"$defs":{"mcp_stub_Color":{"type":"string","enum":["Red","Green","Blue"]},"mcp_stub_Nested":{"type":"object","properties":{"point":{"$ref":"#/$defs/mcp_stub_Point"},"color":{"$ref":"#/$defs/mcp_stub_Color"},"label":{"oneOf":[{"type":"string"},{"type":"null"}]}},"required":["point","color"]},"mcp_stub_Point":{"type":"object","properties":{"x":{"type":"integer","format":"int32"},"y":{"type":"integer","format":"int32"}},"required":["x","y"]}}}"##

    private let refProcessShape = ##"{"$schema":"https://json-schema.org/draft/2020-12/schema","type":"object","properties":{"shape":{"$ref":"#/$defs/mcp_stub_Shape"},"tree":{"$ref":"#/$defs/mcp_stub_Tree"}},"required":["shape","tree"],"$defs":{"mcp_stub_Shape":{"oneOf":[{"$ref":"#/$defs/mcp_stub_Shape_Circle"},{"$ref":"#/$defs/mcp_stub_Shape_Rect"}]},"mcp_stub_Tree":{"type":"object","properties":{"value":{"type":"integer","format":"int32"},"left":{"oneOf":[{"$ref":"#/$defs/mcp_stub_Tree"},{"type":"null"}]},"children":{"type":"array","items":{"$ref":"#/$defs/mcp_stub_Tree"}}},"required":["value","children"]},"mcp_stub_Shape_Circle":{"type":"object","properties":{"radius":{"type":"number","format":"double"}},"required":["radius"]},"mcp_stub_Shape_Rect":{"type":"object","properties":{"w":{"type":"number","format":"double"},"h":{"type":"number","format":"double"}},"required":["w","h"]}}}"##

    private let refPagePoints = ##"{"$schema":"https://json-schema.org/draft/2020-12/schema","type":"object","properties":{"page":{"$ref":"#/$defs/mcp_stub_PointPage"}},"required":["page"],"$defs":{"mcp_stub_Point":{"type":"object","properties":{"x":{"type":"integer","format":"int32"},"y":{"type":"integer","format":"int32"}},"required":["x","y"]},"mcp_stub_PointPage":{"type":"object","properties":{"items":{"type":"array","items":{"$ref":"#/$defs/mcp_stub_Point"}},"total":{"type":"integer","format":"int32","minimum":0}},"required":["items","total"]}}}"##

    private let refPing = #"{"$schema":"https://json-schema.org/draft/2020-12/schema","type":"object","properties":{"seqno":{"type":"integer","format":"int32"},"label":{"type":"string"}},"required":["seqno","label"]}"#

    // -----------------------------------------------------------------------
    // Recursive structural-equality helper (T7 §5.4):
    //   - Dictionary: compare key-by-key (key-order-insensitive); equal key sets.
    //   - Array under key "required": compare as a SET of strings.
    //   - Any other array: element-by-element (ordered).
    //   - Scalars: compare via NSNumber/NSString equality (JSONSerialization
    //     bridges JSON scalars to NSNumber/NSString).
    // -----------------------------------------------------------------------
    private func schemasStructurallyEqual(_ actual: Any, _ expected: Any) -> Bool {
        return schemasEqualImpl(actual, expected, inRequiredKey: false)
    }

    private func schemasEqualImpl(_ actual: Any, _ expected: Any, inRequiredKey: Bool) -> Bool {
        if inRequiredKey {
            guard let a = actual as? [Any], let e = expected as? [Any] else { return false }
            let aSet = Set(a.map { "\($0)" })
            let eSet = Set(e.map { "\($0)" })
            return aSet == eSet
        }
        if let a = actual as? [String: Any], let e = expected as? [String: Any] {
            if a.count != e.count { return false }
            for (key, eVal) in e {
                guard let aVal = a[key] else { return false }
                if !schemasEqualImpl(aVal, eVal, inRequiredKey: key == "required") {
                    return false
                }
            }
            return true
        }
        if let a = actual as? [Any], let e = expected as? [Any] {
            if a.count != e.count { return false }
            for i in 0..<a.count {
                if !schemasEqualImpl(a[i], e[i], inRequiredKey: false) {
                    return false
                }
            }
            return true
        }
        // Scalars: JSONSerialization yields NSNumber for numbers/bools and
        // NSString for strings; both conform to NSObject equality.
        if let a = actual as? NSObject, let e = expected as? NSObject {
            return a.isEqual(e)
        }
        return false
    }

    private func parseSchema(_ json: String) -> [String: Any] {
        let data = json.data(using: .utf8)!
        return (try! JSONSerialization.jsonObject(with: data, options: [])) as! [String: Any]
    }

    private func parseAny(_ json: String) -> Any {
        let data = json.data(using: .utf8)!
        return try! JSONSerialization.jsonObject(with: data, options: [.fragmentsAllowed])
    }

    // -----------------------------------------------------------------------
    // Stub McpTools service: every method returns ok=true (T7 §3 convention).
    // -----------------------------------------------------------------------
    final class StubMcpTools: McpTools {
        func listCollections(arg: mcptools.listcollections.`in`) -> mcptools.listcollections.out {
            return mcptools.listcollections.out(ok: true)
        }
        func submitComposite(arg: mcptools.submitcomposite.`in`) -> mcptools.submitcomposite.out {
            return mcptools.submitcomposite.out(ok: true)
        }
        func processShape(arg: mcptools.processshape.`in`) -> mcptools.processshape.out {
            return mcptools.processshape.out(ok: true)
        }
        func pagePoints(arg: mcptools.pagepoints.`in`) -> mcptools.pagepoints.out {
            return mcptools.pagepoints.out(ok: true)
        }
        func ping(arg: mcptools.ping.`in`) -> mcptools.ping.out {
            return mcptools.ping.out(ok: true)
        }
    }

    // Server factory: Ctx = McpTools (the service impl), so the no-errors
    // McpToolsWiring.invokeJson is used directly as the delegate.
    private func makeServer(_ impl: McpTools) -> McpToolsMcpServer<McpTools> {
        return McpToolsMcpServer<McpTools> { method, data, stub, codecCtx in
            return try McpToolsWiring.invokeJson(method, data, stub, codecCtx)
        }
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

    private let expectedToolNames = [
        "McpTools_listCollections",
        "McpTools_submitComposite",
        "McpTools_processShape",
        "McpTools_pagePoints",
        "McpTools_ping",
    ]

    // =======================================================================
    // §1 — initialize
    // =======================================================================
    func test_sec1_initialize_responseIsCorrect() {
        let stub = StubMcpTools()
        let server = makeServer(stub)
        let session = McpSession()
        let resp = send(server, session, stub,
            JsonRpcRequest(1, "initialize", parseAny(#"{"protocolVersion":"2025-06-18","capabilities":{},"clientInfo":{"name":"test-client","version":"0.0.1"}}"#)))

        XCTAssertNil(resp.error, "Expected no error for initialize")
        let result = resp.result as! [String: Any]
        XCTAssertEqual(result["protocolVersion"] as? String, "2025-06-18", "protocolVersion must be 2025-06-18")
        let caps = result["capabilities"] as! [String: Any]
        XCTAssertEqual(caps.count, 1, "capabilities must have exactly one key")
        XCTAssertNotNil(caps["tools"], "capabilities.tools must be present")
        XCTAssertTrue((caps["tools"] as! [String: Any]).isEmpty, "capabilities.tools must be {}")
        let info = result["serverInfo"] as! [String: Any]
        XCTAssertFalse((info["name"] as! String).isEmpty, "serverInfo.name must be non-empty")
        XCTAssertFalse((info["version"] as! String).isEmpty, "serverInfo.version must be non-empty")
    }

    func test_sec1_initializedNotification_producesNoResponse() {
        let stub = StubMcpTools()
        let server = makeServer(stub)
        let session = McpSession()
        _ = server.handle(
            JsonRpcRequest(0, "initialize", parseAny(#"{"protocolVersion":"2025-06-18","capabilities":{},"clientInfo":{"name":"t","version":"0"}}"#)),
            session, stub, BaboonCodecContext.defaultCtx)
        let notifResp = server.handle(
            JsonRpcRequest(nil, "notifications/initialized", nil),
            session, stub, BaboonCodecContext.defaultCtx)
        XCTAssertNil(notifResp, "notifications/initialized MUST produce no response")
    }

    func test_sec1_toolsListBeforeInitialize_isInvalidRequest() {
        let stub = StubMcpTools()
        let server = makeServer(stub)
        let session = McpSession()
        let resp = send(server, session, stub, JsonRpcRequest(9, "tools/list", nil))
        XCTAssertNotNil(resp.error, "tools/list before initialize must be a Channel-A error")
        XCTAssertEqual(resp.error?.code, -32600, "must be InvalidRequest (-32600)")
    }

    // =======================================================================
    // §2 — tools/list + inputSchema validation (K1 tier)
    // =======================================================================
    private func listedTools() -> [[String: Any]] {
        let stub = StubMcpTools()
        let server = makeServer(stub)
        let session = McpSession()
        initSession(server, session, stub)
        let resp = send(server, session, stub, JsonRpcRequest(2, "tools/list", nil))
        let result = resp.result as! [String: Any]
        return (result["tools"] as! [[String: Any]])
    }

    func test_sec2_exactlyFiveToolsInDeclarationOrder() {
        let tools = listedTools()
        XCTAssertEqual(tools.count, 5, "MUST be exactly 5 tools")
        for (i, name) in expectedToolNames.enumerated() {
            XCTAssertEqual(tools[i]["name"] as? String, name, "tool position \(i) must be \(name)")
        }
        // No "description" key for any tool (stub model has no doc comments).
        for t in tools {
            XCTAssertNil(t["description"], "tool \(t["name"] ?? "?") must have no description")
        }
    }

    func test_sec2_eachInputSchema_hasDraft202012SchemaUri() {
        let tools = listedTools()
        for t in tools {
            let schema = t["inputSchema"] as! [String: Any]
            XCTAssertEqual(schema["$schema"] as? String, "https://json-schema.org/draft/2020-12/schema",
                "tool \(t["name"] ?? "?"): $schema must be the Draft 2020-12 URI")
        }
    }

    func test_sec2_k1_allInputSchemas_areWellFormedJson_viaJSONSerialization() {
        // K1 part (a) — well-formedness gate: each inputSchema must survive a
        // full JSONSerialization serialize/re-parse cycle without throwing.
        let tools = listedTools()
        for t in tools {
            let schema = t["inputSchema"]
            XCTAssertTrue(schema is [String: Any], "tool \(t["name"] ?? "?"): inputSchema must be an object")
            XCTAssertTrue(JSONSerialization.isValidJSONObject(schema!), "tool \(t["name"] ?? "?"): inputSchema must be a valid JSON object")
            let data = try! JSONSerialization.data(withJSONObject: schema!, options: [.sortedKeys])
            let reparsed = try! JSONSerialization.jsonObject(with: data, options: [])
            XCTAssertTrue(reparsed is [String: Any], "tool \(t["name"] ?? "?"): schema must re-parse to an object")
        }
    }

    func test_sec2_k1_allFiveTools_structuralEqualityToT7Reference() {
        // K1 part (b) — structural equality to T7 §2.3 reference literals.
        let tools = listedTools()
        let refs = [refListCollections, refSubmitComposite, refProcessShape, refPagePoints, refPing]
        for i in 0..<5 {
            let toolName = tools[i]["name"] as! String
            // Re-serialize and re-parse via JSONSerialization to exercise the
            // full codec round-trip (codec-rendering divergence coverage, K1 §3).
            let actualData = try! JSONSerialization.data(withJSONObject: tools[i]["inputSchema"]!, options: [])
            let actual = try! JSONSerialization.jsonObject(with: actualData, options: [])
            let expected = parseSchema(refs[i])
            if !schemasStructurallyEqual(actual, expected) {
                XCTFail("Tool \(toolName) (index \(i)): inputSchema not structurally equal to T7 §2.3 reference.\n  actual: \(String(data: actualData, encoding: .utf8)!)\n  expected: \(refs[i])")
            }
        }
    }

    func test_sec2_k1_negativeControl_wrongReferenceDetectedByComparator() {
        // NEGATIVE CONTROL (T7 §5.2): the comparator MUST DETECT a wrong reference.
        // Wrong reference: ping schema with an extra top-level field "extra":"bad".
        let tools = listedTools()
        let pingSchema = tools[4]["inputSchema"] as! [String: Any]

        let wrongRef = parseSchema(#"{"$schema":"https://json-schema.org/draft/2020-12/schema","type":"object","properties":{"seqno":{"type":"integer","format":"int32"},"label":{"type":"string"}},"required":["seqno","label"],"extra":"bad"}"#)

        // MUST return false for the wrong reference (the comparator is live).
        if schemasStructurallyEqual(pingSchema, wrongRef) {
            XCTFail("NEGATIVE CONTROL FAILED: schemasStructurallyEqual returned true for a deliberately-wrong reference (extra field). The comparator is not functioning correctly.")
        }
        // And MUST return true for the correct reference (restore).
        if !schemasStructurallyEqual(pingSchema, parseSchema(refPing)) {
            XCTFail("Positive case failed after negative control: ping schema must equal refPing.")
        }
    }

    // =======================================================================
    // §3 — tools/call (success paths)
    // =======================================================================
    func test_sec3_ping_returnsOkTrue() {
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

    func test_sec3_listCollections_returnsOkTrue() {
        let stub = StubMcpTools()
        let server = makeServer(stub)
        let session = McpSession()
        initSession(server, session, stub)

        // byColor is map[Color,str]; an empty object {} satisfies the decoder.
        let resp = send(server, session, stub, JsonRpcRequest(4, "tools/call",
            parseAny(#"{"name":"McpTools_listCollections","arguments":{"tags":["a","b"],"uniqueIds":[1,2],"labels":{"k":"v"},"byColor":{}}}"#)))

        XCTAssertNil(resp.error, "Unexpected error on listCollections call")
        let result = resp.result as! [String: Any]
        let content = result["content"] as! [[String: Any]]
        XCTAssertEqual(content.count, 1)
        let payload = parseAny(content[0]["text"] as! String) as! [String: Any]
        XCTAssertEqual(payload["ok"] as? Bool, true, "ok must be true")
        let isError = result["isError"]
        XCTAssertTrue(isError == nil || (isError as? Bool) == false, "isError must be false or absent")
    }

    // =======================================================================
    // §4 — tools/call (error paths) — primary negative controls
    // =======================================================================
    func test_sec4_unknownTool_channelAError_code32602() {
        // NEGATIVE CONTROL (T7 §5.2): success for McpTools_nonexistent would fail.
        let stub = StubMcpTools()
        let server = makeServer(stub)
        let session = McpSession()
        initSession(server, session, stub)

        let resp = send(server, session, stub, JsonRpcRequest(5, "tools/call",
            parseAny(#"{"name":"McpTools_nonexistent","arguments":{}}"#)))

        XCTAssertNotNil(resp.error, "Unknown tool must produce a Channel-A error")
        XCTAssertNil(resp.result, "No result expected for unknown tool")
        XCTAssertEqual(resp.error?.code, -32602, "Unknown tool error code MUST be -32602")
        XCTAssertFalse((resp.error?.message ?? "").isEmpty, "error.message must be non-empty")
    }

    func test_sec4_decodeFailure_channelB_isErrorTrue() {
        // NEGATIVE CONTROL: if isError were false this test would fail.
        //
        // Swift-specific Channel-B trigger (see file header): submitComposite with
        // a non-object `nested` value (123). The submitcomposite.in decode reaches
        // Nested_JsonCodec.decode(ctx, 123), whose dict guard THROWS
        // BaboonCodecError.invalidInput (catchable) — the K4 Channel-B
        // "object but won't decode into the DTO" case. A missing-key trigger would
        // TRAP in Swift (force-unwrap), not throw, so it is deliberately avoided.
        let stub = StubMcpTools()
        let server = makeServer(stub)
        let session = McpSession()
        initSession(server, session, stub)

        let resp = send(server, session, stub, JsonRpcRequest(6, "tools/call",
            parseAny(#"{"name":"McpTools_submitComposite","arguments":{"nested":123,"color":"Red","fancy":"x"}}"#)))

        // Channel B: MUST be a result (not error) with isError=true.
        XCTAssertNotNil(resp.result, "Channel-B: result must be present")
        XCTAssertNil(resp.error, "Channel-B: must not be a JSON-RPC error")
        let result = resp.result as! [String: Any]
        XCTAssertEqual(result["isError"] as? Bool, true, "isError MUST be true for Channel-B decode failure")
        let content = result["content"] as! [[String: Any]]
        XCTAssertTrue(content.count > 0, "content must have at least one element")
        XCTAssertEqual(content[0]["type"] as? String, "text")
        XCTAssertFalse((content[0]["text"] as! String).isEmpty, "content[0].text must be non-empty")
    }
}
