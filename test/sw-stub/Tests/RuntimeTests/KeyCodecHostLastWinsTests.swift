// PR-26.2 (M26) — KeyCodec Host last-wins concurrency contract regression test.
//
// Asserts FStr_KeyCodecHost.register(impl) overwrites the previously registered
// impl (last-wins). Swift used a `nonisolated(unsafe) static var` mutable
// singleton pre-PR-26.2; this test pins that behavior across future refactors.
//
// Generated symbols are produced by mdl :test-gen-regular-adt under
// target/test-regular/sw-stub/.

import XCTest
import Foundation
@testable import BaboonRuntime
@testable import MyOkM19Foreign

final class KeyCodecHostLastWinsTests: XCTestCase {

    private let ctx = BaboonCodecContext.compact

    private struct PrefixCodec: FStr_KeyCodec {
        let tag: String
        func encodeKey(_ value: FStr) -> String { return "\(tag):\(value)" }
        func decodeKey(_ s: String) throws -> FStr {
            let pfx = "\(tag):"
            return s.hasPrefix(pfx) ? String(s.dropFirst(pfx.count)) : s
        }
    }

    private struct IdentityCodec: FStr_KeyCodec {
        func encodeKey(_ value: FStr) -> String { return value }
        func decodeKey(_ s: String) throws -> FStr { return s }
    }

    // PR-26.2-D01: restore identity impl after each test so the global
    // FStr_KeyCodecHost singleton does not leak a PrefixCodec into sibling
    // tests sharing the process. Runs even on assertion failure.
    override func tearDown() {
        FStr_KeyCodecHost.register(IdentityCodec())
        super.tearDown()
    }

    func testRegisterBAfterRegisterAObservesB() throws {
        let original = Holder(m: [ItemKey(v: "k"): "v"])

        FStr_KeyCodecHost.register(PrefixCodec(tag: "A"))
        let encodedA = Holder_JsonCodec.instance.encode(ctx, original)
        let bytesA = try JSONSerialization.data(withJSONObject: encodedA, options: [.sortedKeys])
        let stringA = String(data: bytesA, encoding: .utf8)!
        XCTAssertTrue(stringA.contains("A:k"),
                      "expected A: prefix in encoded wire form, got \(stringA)")

        FStr_KeyCodecHost.register(PrefixCodec(tag: "B"))
        let encodedB = Holder_JsonCodec.instance.encode(ctx, original)
        let bytesB = try JSONSerialization.data(withJSONObject: encodedB, options: [.sortedKeys])
        let stringB = String(data: bytesB, encoding: .utf8)!
        XCTAssertTrue(stringB.contains("B:k"),
                      "PR-26.2 last-wins regression: expected B: prefix after re-register, got \(stringB)")
        XCTAssertFalse(stringB.contains("A:k"),
                       "PR-26.2 last-wins regression: A: prefix still present after B re-register, got \(stringB)")
    }
}
