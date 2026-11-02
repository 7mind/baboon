// GitHub 7mind/baboon#106 — generated JSON decoders must throw
// BaboonCodecException.decoderFailure on a wrong-typed, missing or malformed value instead of
// aborting the process through a force cast / force unwrap / assert.
//
// Uses the testpkg.pkg0 fixtures (v3 = TestpkgPkg0, v1 = TestpkgPkg0_v1_0_0 for the
// map[uid, i32] field); generated symbols are produced by mdl :test-gen-regular-adt.

import XCTest
import Foundation
@testable import BaboonRuntime
@testable import TestpkgPkg0
import TestpkgPkg0_v1_0_0

final class JsonDecodeMalformedInputTests: XCTestCase {

    private let ctx = BaboonCodecContext.compact

    private let validT6D2: [String: Any] = [
        "f00": "s", "f01": 1, "f02": 2, "f03": 3, "f04": "4",
        "f05": 5, "f06": 6, "f07": 7, "f08": "8",
        "f09": "2024-01-02T03:04:05.006+01:00", "f10": "2024-01-02T03:04:05.006Z",
        "f11": 1.5, "f12": 2.5, "f13": "3.5", "f14": true,
    ]

    // Round-trips through JSONSerialization so values carry the Foundation types a real
    // decoder sees (NSNumber, NSString, NSNull, NSArray, NSDictionary).
    private func wire(_ value: Any) throws -> Any {
        let data = try JSONSerialization.data(withJSONObject: value, options: [.fragmentsAllowed])
        return try JSONSerialization.jsonObject(with: data, options: [.fragmentsAllowed])
    }

    private func with(_ base: [String: Any], _ key: String, _ value: Any?) -> [String: Any] {
        var copy = base
        copy[key] = value
        return copy
    }

    private func validT6D1() throws -> [String: Any] {
        let value = TestpkgPkg0.T6_D1_Fixture.randomJson(BaboonRandomFactory.create())
        return try wire(TestpkgPkg0.T6_D1_JsonCodec.instance.encode(ctx, value)) as! [String: Any]
    }

    private func assertDecoderFailure<T>(
        _ label: String,
        _ messageFragment: String? = nil,
        _ body: () throws -> T,
        file: StaticString = #filePath,
        line: UInt = #line
    ) {
        do {
            let got = try body()
            XCTFail("\(label): expected decoderFailure, got \(got)", file: file, line: line)
        } catch let err as BaboonCodecException {
            guard case .decoderFailure = err else {
                XCTFail("\(label): expected decoderFailure, got \(err)", file: file, line: line)
                return
            }
            if let fragment = messageFragment {
                XCTAssertTrue(err.message.contains(fragment),
                              "\(label): expected '\(fragment)' in '\(err.message)'", file: file, line: line)
            }
        } catch {
            XCTFail("\(label): expected BaboonCodecException, got \(type(of: error)): \(error)", file: file, line: line)
        }
    }

    func testValidInputStillDecodes() throws {
        let d2 = try TestpkgPkg0.T6_D2_JsonCodec.instance.decode(ctx, try wire(validT6D2))
        XCTAssertEqual(d2.f00, "s")
        XCTAssertEqual(d2.f04, 4)
        XCTAssertEqual(d2.f08, 8)
        XCTAssertEqual(d2.f14, true)

        let original = TestpkgPkg0.T6_D1_Fixture.randomJson(BaboonRandomFactory.create())
        let decoded = try TestpkgPkg0.T6_D1_JsonCodec.instance.decode(ctx, try wire(TestpkgPkg0.T6_D1_JsonCodec.instance.encode(ctx, original)))
        // Float fields may not survive a JSON text round-trip bit-exactly; compare the rest.
        XCTAssertEqual(decoded.fNewLst, original.fNewLst)
        XCTAssertEqual(decoded.fPrecex4, original.fPrecex4)
        XCTAssertEqual(decoded.fNewOpt, original.fNewOpt)
        XCTAssertEqual(decoded.fSameCustom.f00, original.fSameCustom.f00)
        XCTAssertEqual(decoded.fSameCustom.f04, original.fSameCustom.f04)
        XCTAssertEqual(decoded.fSameCustom.f10, original.fSameCustom.f10)

        let maps = try TestpkgPkg0.Maps_JsonCodec.instance.decode(ctx, try wire(["f1": ["1": "a", "-2": "b"]]))
        XCTAssertEqual(maps.f1, [1: "a", -2: "b"])

        let uid = UUID()
        let t13 = try TestpkgPkg0_v1_0_0.T13_2_JsonCodec.instance.decode(
            ctx, try wire(["u": uid.uuidString, "f": [String: Any](), "f1": [uid.uuidString: 3]]))
        XCTAssertEqual(t13.f1, [uid: 3])
    }

    func testOptionalFieldAbsentOrNullDecodesToNil() throws {
        var base = try validT6D1()
        base["fNewOpt"] = NSNull()
        XCTAssertNil(try TestpkgPkg0.T6_D1_JsonCodec.instance.decode(ctx, try wire(base)).fNewOpt)
        base.removeValue(forKey: "fNewOpt")
        XCTAssertNil(try TestpkgPkg0.T6_D1_JsonCodec.instance.decode(ctx, try wire(base)).fNewOpt)
    }

    func testWrongTypedScalarFieldThrows() throws {
        let cases: [(String, Any)] = [
            ("f00", 1), ("f00", NSNull()), ("f07", "x"), ("f14", "yes"), ("f12", [1]),
            ("f04", [String: Any]()), ("f13", [1]), ("f10", 5),
        ]
        for (key, value) in cases {
            let input = try wire(with(validT6D2, key, value))
            assertDecoderFailure("T6_D2.\(key) = \(value)", "T6_D2.\(key)") {
                try TestpkgPkg0.T6_D2_JsonCodec.instance.decode(ctx, input)
            }
        }
    }

    func testMissingRequiredFieldThrows() throws {
        for key in ["f00", "f07", "f10", "f14"] {
            let input = try wire(with(validT6D2, key, nil))
            assertDecoderFailure("T6_D2 without \(key)", "'\(key)'") {
                try TestpkgPkg0.T6_D2_JsonCodec.instance.decode(ctx, input)
            }
        }
        for key in ["fSameCustom", "fNewLst", "fNewMap"] {
            let input = try wire(with(try validT6D1(), key, nil))
            assertDecoderFailure("T6_D1 without \(key)", "'\(key)'") {
                try TestpkgPkg0.T6_D1_JsonCodec.instance.decode(ctx, input)
            }
        }
    }

    func testWrongTypedCollectionThrows() throws {
        let cases: [(String, Any)] = [
            ("fNewLst", "x"), ("fNewLst", ["a"]), ("fWrapSet", ["k": 1]),
            ("fNewMap", [1, 2]), ("fNewMap", ["k": "v"]), ("fPrecex5", ["x"]),
        ]
        for (key, value) in cases {
            let input = try wire(with(try validT6D1(), key, value))
            assertDecoderFailure("T6_D1.\(key) = \(value)", "T6_D1.\(key)") {
                try TestpkgPkg0.T6_D1_JsonCodec.instance.decode(ctx, input)
            }
        }
        // A non-object element reaches the nested DTO codec's own top-level guard, which throws
        // BaboonCodecError.invalidInput (pre-existing error type for that guard).
        let nested = try wire(with(try validT6D1(), "fSameCustomLst", [1]))
        XCTAssertThrowsError(try TestpkgPkg0.T6_D1_JsonCodec.instance.decode(ctx, nested))
    }

    func testMalformedMapKeyThrows() throws {
        for key in ["x", "1.5", "", "300"] {
            let input = try wire(["f1": [key: "a"]])
            assertDecoderFailure("Maps.f1 key '\(key)'", "malformed key") {
                try TestpkgPkg0.Maps_JsonCodec.instance.decode(ctx, input)
            }
        }
        let u32Input = try wire(with(try validT6D1(), "fPrecex4", ["-1": 1]))
        assertDecoderFailure("T6_D1.fPrecex4 key '-1'", "malformed key") {
            try TestpkgPkg0.T6_D1_JsonCodec.instance.decode(ctx, u32Input)
        }
        let uuidInput = try wire(["u": UUID().uuidString, "f": [String: Any](), "f1": ["not-a-uuid": 1]])
        assertDecoderFailure("T13_2.f1 key 'not-a-uuid'", "not-a-uuid") {
            try TestpkgPkg0_v1_0_0.T13_2_JsonCodec.instance.decode(ctx, uuidInput)
        }
    }

    func testDuplicateDecodedMapKeyThrows() throws {
        let input = try wire(["f1": ["1": "a", "01": "b"]])
        assertDecoderFailure("Maps.f1 keys '1' and '01'", "duplicate map key") {
            try TestpkgPkg0.Maps_JsonCodec.instance.decode(ctx, input)
        }
    }

    func testMalformedScalarStringThrows() throws {
        let cases: [(String, Any)] = [
            ("f04", "abc"), ("f04", "-1"), ("f08", "1.5"),
            ("f09", "2024-01-02"), ("f09", "2024-01-02T03:04:05.006+0100"),
            ("f10", "garbage-garbage-garbage"), ("f10", "2024-01-02T03:04:05.Z"),
        ]
        for (key, value) in cases {
            let input = try wire(with(validT6D2, key, value))
            assertDecoderFailure("T6_D2.\(key) = \(value)") {
                try TestpkgPkg0.T6_D2_JsonCodec.instance.decode(ctx, input)
            }
        }
        let uuidInput = try wire(["f1": 1, "f2": "not-a-uuid"])
        assertDecoderFailure("T7_D2.f2 = not-a-uuid", "T7_D2.f2") {
            try TestpkgPkg0.T7_D2_JsonCodec.instance.decode(ctx, uuidInput)
        }
    }
}
