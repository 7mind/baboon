import XCTest
import Foundation
import BaboonRuntime
@testable import ConvtestM24foreign

// ----------------------------------------------------------------------------
// PR-I.2 (M24 Phase 3.2) — Custom-foreign `<Foreign>_KeyCodec` extension hook
// (Swift mirror).
//
// Mirrors PR-I.1a (Scala) / PR-I.1b (Java + Kotlin) / PR-I.1c (C#) /
// PR-I.1d (Dart + TS) cross-language tests: round-trip the Swift-emitted
// m24-foreign-keycodec.json through ForeignKeyHolder_JsonCodec and assert
// byte-identity (PR-I-D02 pattern guidance) of the encoded JSON string
// against the canonical wire form `{"m":{"alpha":"v1","beta":"v2"}}`.
// ----------------------------------------------------------------------------

final class M24ForeignKeyCodecTests: XCTestCase {
    let baseDir = URL(fileURLWithPath: "../../target/compat-test").standardizedFileURL.path
    let ctx = BaboonCodecContext.defaultCtx

    func testM24ForeignKeyCodecRoundTrip() throws {
        let filePath = "\(baseDir)/swift-json/m24-foreign-keycodec.json"
        guard FileManager.default.fileExists(atPath: filePath) else {
            XCTFail("Swift m24-foreign-keycodec fixture not found: \(filePath)")
            return
        }
        let raw = try Data(contentsOf: URL(fileURLWithPath: filePath))
        let json = try JSONSerialization.jsonObject(with: raw)
        let decoded = try ForeignKeyHolder_JsonCodec.instance.decode(ctx, json)
        let expected = ForeignKeyHolder(m: [
            ItemKey(v: "alpha"): "v1",
            ItemKey(v: "beta"): "v2",
        ])
        XCTAssertEqual(decoded, expected, "round-trip diverged")
    }

    func testM24ForeignKeyCodecCanonicalWireForm() throws {
        let sample = ForeignKeyHolder(m: [
            ItemKey(v: "alpha"): "v1",
            ItemKey(v: "beta"): "v2",
        ])
        let encoded = ForeignKeyHolder_JsonCodec.instance.encode(ctx, sample)
        // Match the on-wire compact form (no .prettyPrinted) with sorted keys for
        // deterministic ordering of map keys (alpha before beta) and outer object.
        let bytes = try JSONSerialization.data(withJSONObject: encoded as Any, options: [.sortedKeys])
        let actual = String(data: bytes, encoding: .utf8)!
        let expected = "{\"m\":{\"alpha\":\"v1\",\"beta\":\"v2\"}}"
        XCTAssertEqual(actual, expected, "FStr_KeyCodec wire form diverged")
    }
}
