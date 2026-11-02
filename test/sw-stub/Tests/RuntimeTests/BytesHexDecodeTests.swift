// `bytes` JSON wire-form decoding (GitHub 7mind/baboon#102, #103): hex digits, with ASCII
// whitespace, `:` and `-` ignored as separators; anything else, or an odd digit count, throws
// BaboonCodecException.decoderFailure — never wrong bytes, truncation or a process abort.
//
// The codec-level tests use the identifier.ok fixture (id Mixed { ..., payload: bytes, ... });
// generated symbols are produced by mdl :test-gen-regular-adt.

import XCTest
import Foundation
@testable import BaboonRuntime
@testable import IdentifierOk

final class BytesHexDecodeTests: XCTestCase {

    private let ctx = BaboonCodecContext.compact

    private let validVectors: [(String, [UInt8])] = [
        ("", []),
        ("ABCD", [0xAB, 0xCD]),
        ("abcd", [0xAB, 0xCD]),
        ("aBcD", [0xAB, 0xCD]),
        ("AB CD", [0xAB, 0xCD]),
        ("AB:CD", [0xAB, 0xCD]),
        ("AB-CD", [0xAB, 0xCD]),
        ("A B C D", [0xAB, 0xCD]),
        ("\tAB\r\nCD ", [0xAB, 0xCD]),
        ("A:B-C D", [0xAB, 0xCD]),
        ("\u{0B}AB\u{0C}CD", [0xAB, 0xCD]),
        ("00FF7f", [0x00, 0xFF, 0x7F]),
        (" - : ", []),
    ]

    private let invalidVectors: [String] = [
        "ZZ",
        "AQ",
        "+1",
        "0x01",
        "q80=",
        "ABC",
        "A",
        "-1",
        " 1",
        "\u{0663}\u{0663}",
        "\u{FF21}\u{FF21}",
        "a\u{00E9}b",
        "AB\u{00A0}CD",
    ]

    private func describe(_ s: String) -> String {
        return s.unicodeScalars.map { String($0.value, radix: 16) }.joined(separator: " ")
    }

    private func assertDecoderFailure(_ input: String, _ body: () throws -> Any) {
        do {
            let got = try body()
            XCTFail("expected decoderFailure for [\(describe(input))], got \(got)")
        } catch let err as BaboonCodecException {
            guard case .decoderFailure = err else {
                XCTFail("expected decoderFailure for [\(describe(input))], got \(err)")
                return
            }
        } catch {
            XCTFail("expected BaboonCodecException for [\(describe(input))], got \(type(of: error))")
        }
    }

    func testFromHexStringAcceptsValidVectors() throws {
        for (input, expected) in validVectors {
            XCTAssertEqual(Array(try BaboonByteStringTools.fromHexString(input)), expected, "input [\(describe(input))]")
        }
    }

    func testFromHexStringRejectsInvalidVectors() {
        for input in invalidVectors {
            assertDecoderFailure(input) { try BaboonByteStringTools.fromHexString(input) }
        }
    }

    func testFromHexStringNamesTheOffendingCharacterAndOddLength() {
        XCTAssertThrowsError(try BaboonByteStringTools.fromHexString("AQ")) { err in
            XCTAssertTrue((err as? BaboonCodecException)?.message.contains("'Q'") == true, "\(err)")
        }
        XCTAssertThrowsError(try BaboonByteStringTools.fromHexString("ABC")) { err in
            XCTAssertTrue((err as? BaboonCodecException)?.message.contains("(3)") == true, "\(err)")
        }
    }

    private func wireWithPayload(_ payload: String) -> [String: Any] {
        let src = Mixed(
            active: true,
            id: UUID(uuidString: "DE7B9E1E-5C93-45FE-BEEC-DA99994F629A")!,
            payload: Data(),
            created: Date(timeIntervalSince1970: 1_777_466_096.789),
            scheduled: BaboonDateTimeOffset(epochMillis: 1_777_458_896_789, offsetMillis: 2 * 3600 * 1000, kind: "offset")
        )
        var wire = Mixed_JsonCodec.instance.encode(ctx, src) as! [String: Any]
        wire["payload"] = payload
        return wire
    }

    func testGeneratedJsonCodecDecodesValidPayloads() throws {
        for (input, expected) in validVectors {
            let decoded = try Mixed_JsonCodec.instance.decode(ctx, wireWithPayload(input))
            XCTAssertEqual(Array(decoded.payload), expected, "input [\(describe(input))]")
        }
    }

    func testGeneratedJsonCodecRejectsInvalidPayloads() {
        for input in invalidVectors {
            assertDecoderFailure(input) { try Mixed_JsonCodec.instance.decode(self.ctx, self.wireWithPayload(input)) }
        }
    }
}
