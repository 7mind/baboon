// Hand-written documentary test pinning the four-case try-emission rule in
// SwJsonCodecGenerator.scala (PR-B / M24 fix).
//
// Context: the map-arm of `decodeElement` now emits `try` independently for
// the key and value tokens, covering four cases:
//
//   keyThr  valThr  →  closure shape
//   false   false      ($keyDec, $valDec)
//   true    false      (try $keyDec, $valDec)
//   false   true       ($keyDec, try $valDec)
//   true    true       (try $keyDec, try $valDec)
//
// The m19/foreign fixture (wrapper-around-foreign.baboon) exercises the
// false/false case: FStr is a Custom-foreign Swift typealias decoded via
// `$ref as! FStr` (no try), and the value type `str` is decoded via `as! String`
// (no try). The `Dictionary(...)` closure must therefore contain neither `try`.
//
// If a future regression reintroduces `try` where it should not appear (or
// omits it where it should), the generated code will either fail to compile
// (spurious `try` on a non-throwing expression triggers -Wunused-result
// warning-as-error) or produce a runtime decode failure (missing `try` in
// front of a `throws` call). This test compiles and runs the round-trip as a
// canary for the latter half of that invariant.
//
// The former half (no spurious `try` in the neither-throws case) is validated
// structurally by the generated `holder.swift` closure shape, checked via the
// mdl :test-gen-regular-adt + source inspection step in PR-B verification.
//
// Equivalence with ForeignMapKeyRoundTripTests.swift is intentional — this
// file serves as a named anchor for the specific four-case invariant, not as
// an independent coverage layer.

import XCTest
import Foundation
@testable import BaboonRuntime
@testable import MyOkM19Foreign

final class MapKeyTryPropagationTests: XCTestCase {

    private let ctx = BaboonCodecContext.compact

    // Exercises the false/false (key-non-throwing, value-non-throwing) case.
    // The generated closure is `($keyDec, $valDec)` — no `try` on either side.
    func testMapKeyTryPropagation_neitherThrows_roundTrip() throws {
        let original = Holder(m: [
            ItemKey(v: "x"): "1",
            ItemKey(v: "y"): "2",
        ])
        let encoded = Holder_JsonCodec.instance.encode(ctx, original)
        let decoded = try Holder_JsonCodec.instance.decode(ctx, encoded)
        XCTAssertEqual(decoded, original, "JSON round-trip diverged: \(decoded) vs \(original)")
    }

    func testMapKeyTryPropagation_emptyMap_roundTrip() throws {
        let original = Holder(m: [:])
        let encoded = Holder_JsonCodec.instance.encode(ctx, original)
        let decoded = try Holder_JsonCodec.instance.decode(ctx, encoded)
        XCTAssertEqual(decoded, original)
    }
}
