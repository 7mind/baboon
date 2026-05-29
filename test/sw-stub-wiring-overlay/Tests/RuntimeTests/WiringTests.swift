// Service-wiring + cross-domain muxer tests (no-errors service-result mode).
// Generated into this stub only by the `test-gen-sw-wiring` mdl action
// (rsync + codegen with --service-result-no-errors=true). Exercises the
// per-domain dispatchers (I1Wiring/I2Wiring) and the cross-domain
// JsonMuxer/UebaMuxer that routes by method.serviceId.
import XCTest
import Foundation
import BaboonRuntime
import TestpkgPkg0

private final class MockI1: I1 {
    func testCall(arg: i1.testcall.`in`) -> i1.testcall.out { i1.testcall.out(i00: 42) }
    func testCall2(arg: T7_Empty) -> T7_Empty { T7_Empty() }
}

private final class MockI2: I2 {
    func noErrCall(arg: i2.noerrcall.`in`) -> i2.noerrcall.out {
        i2.noerrcall.out(result: "result_\(arg.value)")
    }
}

final class WiringTests: XCTestCase {
    private let ctx = BaboonCodecContext.compact

    private func toJsonString(_ obj: Any) throws -> String {
        let data = try JSONSerialization.data(withJSONObject: obj, options: [.fragmentsAllowed])
        return String(data: data, encoding: .utf8)!
    }

    private func encodeI1In() throws -> String {
        try toJsonString(i1.testcall.in_JsonCodec.instance.encode(ctx, i1.testcall.`in`()))
    }

    private func encodeI2In() throws -> String {
        try toJsonString(i2.noerrcall.in_JsonCodec.instance.encode(ctx, i2.noerrcall.`in`(value: 123)))
    }

    private func decodeI1Out(_ s: String) throws -> i1.testcall.out {
        let wire = try JSONSerialization.jsonObject(with: s.data(using: .utf8)!, options: [.fragmentsAllowed])
        return try i1.testcall.out_JsonCodec.instance.decode(ctx, wire)
    }

    private func decodeI2Out(_ s: String) throws -> i2.noerrcall.out {
        let wire = try JSONSerialization.jsonObject(with: s.data(using: .utf8)!, options: [.fragmentsAllowed])
        return try i2.noerrcall.out_JsonCodec.instance.decode(ctx, wire)
    }

    // ==================== Per-domain dispatch ====================

    func testI1JsonDispatchSuccess() throws {
        let method = BaboonMethodId(serviceId: "I1", methodName: "testCall")
        let result = try I1Wiring.invokeJson(method, try encodeI1In(), MockI1(), ctx)
        XCTAssertEqual(try decodeI1Out(result).i00, 42)
    }

    func testI1JsonDispatchNoMatchingMethod() {
        let method = BaboonMethodId(serviceId: "I1", methodName: "nonexistent")
        XCTAssertThrowsError(try I1Wiring.invokeJson(method, "{}", MockI1(), ctx)) { error in
            guard let we = error as? BaboonWiringException, case .noMatchingMethod = we.error else {
                return XCTFail("expected NoMatchingMethod, got \(error)")
            }
        }
    }

    // ==================== Cross-domain Muxer ====================
    // A single muxer composes I1 and I2 and routes by method.serviceId.

    private func newJsonMuxer() throws -> JsonMuxer<String> {
        try JsonMuxer<String>(AnyJsonService(I1JsonService(MockI1())), AnyJsonService(I2JsonService(MockI2())))
    }

    private func newUebaMuxer() throws -> UebaMuxer<Data> {
        try UebaMuxer<Data>(AnyUebaService(I1UebaService(MockI1())), AnyUebaService(I2UebaService(MockI2())))
    }

    func testJsonMuxerRoutesToI1() throws {
        let method = BaboonMethodId(serviceId: "I1", methodName: "testCall")
        let result = try newJsonMuxer().invoke(method, try encodeI1In(), ctx)
        XCTAssertEqual(try decodeI1Out(result).i00, 42)
    }

    func testJsonMuxerRoutesToI2() throws {
        let method = BaboonMethodId(serviceId: "I2", methodName: "noErrCall")
        let result = try newJsonMuxer().invoke(method, try encodeI2In(), ctx)
        XCTAssertEqual(try decodeI2Out(result).result, "result_123")
    }

    func testJsonMuxerNoMatchingService() throws {
        let muxer = try newJsonMuxer()
        let method = BaboonMethodId(serviceId: "Nonexistent", methodName: "x")
        XCTAssertThrowsError(try muxer.invoke(method, "{}", ctx)) { error in
            guard let we = error as? BaboonWiringException, case .noMatchingService = we.error else {
                return XCTFail("expected NoMatchingService, got \(error)")
            }
        }
    }

    func testJsonMuxerDuplicateService() {
        XCTAssertThrowsError(
            try JsonMuxer<String>(AnyJsonService(I1JsonService(MockI1())), AnyJsonService(I1JsonService(MockI1())))
        ) { error in
            guard let we = error as? BaboonWiringException, case .duplicateService = we.error else {
                return XCTFail("expected DuplicateService, got \(error)")
            }
        }
    }

    func testUebaMuxerRoutesToI1() throws {
        let method = BaboonMethodId(serviceId: "I1", methodName: "testCall")
        let writer = BaboonBinTools.createWriter()
        i1.testcall.in_UebaCodec.instance.encode(ctx, writer, i1.testcall.`in`())
        let result = try newUebaMuxer().invoke(method, writer.toData(), ctx)
        let decoded = try i1.testcall.out_UebaCodec.instance.decode(ctx, BaboonBinTools.createReader(result))
        XCTAssertEqual(decoded.i00, 42)
    }

    func testUebaMuxerRoutesToI2() throws {
        let method = BaboonMethodId(serviceId: "I2", methodName: "noErrCall")
        let writer = BaboonBinTools.createWriter()
        i2.noerrcall.in_UebaCodec.instance.encode(ctx, writer, i2.noerrcall.`in`(value: 456))
        let result = try newUebaMuxer().invoke(method, writer.toData(), ctx)
        let decoded = try i2.noerrcall.out_UebaCodec.instance.decode(ctx, BaboonBinTools.createReader(result))
        XCTAssertEqual(decoded.result, "result_456")
    }

    func testUebaMuxerNoMatchingService() throws {
        let muxer = try newUebaMuxer()
        let method = BaboonMethodId(serviceId: "Nonexistent", methodName: "x")
        XCTAssertThrowsError(try muxer.invoke(method, Data(), ctx)) { error in
            guard let we = error as? BaboonWiringException, case .noMatchingService = we.error else {
                return XCTFail("expected NoMatchingService, got \(error)")
            }
        }
    }
}
