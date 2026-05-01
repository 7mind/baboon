// PR-F (M24) — cross-language malformed map-key error consistency.
//
// Verifies that decoding a JSON object whose map-key cannot be parsed back into the
// id type throws BaboonCodecException.decoderFailure with message containing
// "malformed key". Replaces the prior fatalError-on-Left and forced cast.
//
// Uses the my.ok.m19.singleid fixture; generated symbols are produced by
// mdl :test-gen-regular-adt under target/test-regular/sw-stub/.

import XCTest
import Foundation
@testable import BaboonRuntime
@testable import MyOkM19Singleid

final class MapKeyMalformedTests: XCTestCase {

    private let ctx = BaboonCodecContext.compact

    func testHolderJsonDecodeThrowsDecoderFailureForMalformedMapKey() {
        let badJson: [String: Any] = ["m": ["not_a_valid_id": "v"]]
        do {
            _ = try Holder_JsonCodec.instance.decode(ctx, badJson)
            XCTFail("expected throw, got success")
        } catch let err as BaboonCodecException {
            XCTAssertTrue(err.description.contains("malformed key"),
                "expected 'malformed key' in error description; got: \(err.description)")
        } catch {
            XCTFail("expected BaboonCodecException; got \(type(of: error))")
        }
    }
}
