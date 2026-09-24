// 64-bit integers are written as decimal strings (docs/json-codecs.md, "64-bit integers").
// Readers stay lenient about the JSON-number form an older compiler produced, and that
// leniency is what keeps documents written before the change readable.
//
// Nothing writes numbers any more, so without this test the number arm of every decoder is
// dead as far as the suite is concerned.
//
// Uses the identifier.ok fixture (id LongId { x: i64 }; id UInts { a: u08, b: u16, c: u32,
// d: u64 }); generated symbols are produced by mdl :test-gen-regular-adt.

import XCTest
import Foundation
@testable import BaboonRuntime
@testable import IdentifierOk

final class LegacyInt64WireTests: XCTestCase {

    private let ctx = BaboonCodecContext.compact

    private func parse(_ s: String) throws -> Any {
        try JSONSerialization.jsonObject(with: Data(s.utf8), options: [])
    }

    func testI64DecodesFromTheLegacyNumericForm() throws {
        let decoded = try LongId_JsonCodec.instance.decode(ctx, parse(#"{"x":-9007199254740991}"#))
        XCTAssertEqual(decoded.x, -9007199254740991)
    }

    func testI64DecodesFromTheStringForm() throws {
        let decoded = try LongId_JsonCodec.instance.decode(ctx, parse(#"{"x":"-9223372036854775808"}"#))
        XCTAssertEqual(decoded.x, Int64.min)
    }

    func testU64DecodesFromTheLegacyNumericForm() throws {
        let decoded = try UInts_JsonCodec.instance.decode(ctx, parse(#"{"a":1,"b":2,"c":3,"d":42}"#))
        XCTAssertEqual(decoded.d, 42)
    }

    func testU64DecodesFromTheStringForm() throws {
        let decoded = try UInts_JsonCodec.instance.decode(ctx, parse(#"{"a":1,"b":2,"c":3,"d":"42"}"#))
        XCTAssertEqual(decoded.d, 42)
    }
}
