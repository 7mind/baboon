// Every runtime fails fast when a value's `baboonMinReaderVersions` lacks the tier the envelope
// needs (docs/forward-compat.md, "Envelope integration"). Generated types always carry all four
// tiers; this guards hand-written conformances.
import XCTest
@testable import BaboonRuntime

/// A hand-written value whose min-reader table is chosen per test.
private struct NoTiers: BaboonMetaProvider {
    let tiers: [String: String]
    var baboonDomainVersion: String { "1.0.0" }
    var baboonDomainIdentifier: String { "t.d" }
    var baboonTypeIdentifier: String { "t.d/:#T" }
    var baboonSameInVersions: [String] { ["1.0.0"] }
    var baboonMinReaderVersions: [String: String] { tiers }
}

final class MinReaderTiersFailFastTests: XCTestCase {
    func testJsonBoundMissingThrows() throws {
        XCTAssertThrowsError(try BaboonTypeMeta.from(NoTiers(tiers: [:]))) { error in
            XCTAssertTrue("\(error)".contains("json-additive"), "\(error)")
        }
        // with the JSON bound present the meta is built normally
        XCTAssertEqual(try BaboonTypeMeta.from(NoTiers(tiers: ["json-additive": "1.0.0"])).domainVersionReadableMin, "1.0.0")
    }

    func testPrefixBoundMissingThrowsForV2() throws {
        let jsonOnly = NoTiers(tiers: ["json-additive": "1.0.0"])
        let v2 = BaboonCodecContext.custom(false, .strict, .v2, nil)
        XCTAssertThrowsError(try BaboonTypeMeta.forBin(jsonOnly, v2)) { error in
            XCTAssertTrue("\(error)".contains("prefix-compact"), "\(error)")
        }
        // the default v1/Strict context needs no prefix tier
        XCTAssertEqual(try BaboonTypeMeta.forBin(jsonOnly, BaboonCodecContext.compact).metaVersion, 1)
    }
}
