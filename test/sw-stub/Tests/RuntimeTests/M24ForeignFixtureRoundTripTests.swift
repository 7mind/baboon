// PR-26.4 (M26) regression: hasForeignType filter lift in SwCodecFixtureTranslator
// + SwCodecTestsTranslator. Closes PR-I.2-D02 + PR-68-D02.
//
// Pre-fix: foreign-bearing DTOs (e.g. `Holder { m: map[ItemKey, str] }` from
// `my.ok.m19.foreign`, where `ItemKey { v: FStr }` and `FStr` is Custom-foreign
// `Swift.String`) were excluded from fixture / test emission. As a result,
// `Holder_Fixture` did not exist — referencing it from this file would have been
// a compile error.
//
// Post-fix: `genScalar` synthesizes Custom-foreign fixture values via the Swift-decl
// allowlist (`Swift.String` → `rnd.nextString()` here). The two assertions below
// verify (a) the fixture symbol exists, and (b) JSON round-trip succeeds against
// the existing Custom-foreign KeyCodec hook (PR-I.2 default identity for stringy
// `Swift.String` foreigns).

import XCTest
import Foundation
@testable import BaboonRuntime
@testable import MyOkM19Foreign

final class M24ForeignFixtureRoundTripTests: XCTestCase {

    private let ctx = BaboonCodecContext.compact

    func testForeignBearingFixtureSymbolExists_jsonRoundTrip() throws {
        // Pre-fix this line is a compile error: Holder_Fixture suppressed by the
        // hasForeignType filter at SwCodecFixtureTranslator.scala:38. The fact that
        // this file builds at all is half the regression check; the round-trip
        // below exercises the end-to-end codec path.
        let rnd = BaboonRandomFactory.create()
        let original = Holder_Fixture.randomJson(rnd)
        let encoded = Holder_JsonCodec.instance.encode(ctx, original)
        let decoded = try Holder_JsonCodec.instance.decode(ctx, encoded)
        XCTAssertEqual(decoded, original, "JSON round-trip diverged: \(decoded) vs \(original)")
    }

    // NOTE: a UEBA round-trip variant is intentionally omitted — the auto-generated
    // `ItemKey_UebaCodec` encodes its `v: FStr` field via `FStr_UebaCodec`, which is a
    // `fatalError` placeholder for host-supplied implementations. Round-tripping `Holder`
    // through UEBA would trap. The hand-written JSON test above is sufficient to verify
    // the fixture-emitter regression: pre-fix `Holder_Fixture` did not exist; post-fix it
    // does, and the JSON path completes via the PR-I.2 `FStr_KeyCodecHost` default identity.
}
