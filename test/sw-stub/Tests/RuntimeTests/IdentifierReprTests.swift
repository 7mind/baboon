// NOTE: This test references generated symbols emitted by the Swift codegen
// path (BaboonIdentifierRepr, BaboonEither, identifier.ok.* types). It runs
// only from the rsync'd codegen copy under target/test-regular/sw-stub/.
// PR-57c mirror of the Kotlin/Java IdentifierReprTest suites.

import XCTest
import Foundation
import BaboonRuntime
import IdentifierOk

final class IdentifierReprTests: XCTestCase {

    // Spec §6.2: each of the 5 metacharacters escaped.
    func testEscapeStrAllMetacharacters() {
        let src = "\\#:{}"
        XCTAssertEqual("\\\\\\#\\:\\{\\}", BaboonIdentifierRepr.escapeStr(src))
    }

    // Spec §6.3: trailing single backslash escapes to `\\`.
    func testEscapeStrTrailingBackslash() {
        XCTAssertEqual("foo\\\\", BaboonIdentifierRepr.escapeStr("foo\\"))
    }

    // Spec §6.4: 4 backslashes round-trip.
    func testEscapeStrAllBackslashes() {
        XCTAssertEqual(
            "\\\\\\\\\\\\\\\\",
            BaboonIdentifierRepr.escapeStr("\\\\\\\\")
        )
    }

    // Spec §6.7: i64.min renders as plain signed decimal.
    func testLongIdRoundtripInt64Min() {
        let src = LongId(x: Int64.min)
        let s = src.description
        XCTAssertEqual("LongId:1.0.0#x:-9223372036854775808", s)

        let parsed = LongIdCodec.parseRepr(s)
        guard case .right(let got) = parsed else {
            XCTFail("Expected right; got: \(parsed)")
            return
        }
        XCTAssertEqual(src, got)
    }

    func testLongIdRoundtripInt64Max() {
        let src = LongId(x: Int64.max)
        let s = src.description
        XCTAssertEqual("LongId:1.0.0#x:9223372036854775807", s)

        let parsed = LongIdCodec.parseRepr(s)
        guard case .right(let got) = parsed else {
            XCTFail("Expected right; got: \(parsed)")
            return
        }
        XCTAssertEqual(src, got)
    }

    // Spec §6.8: u64.MAX_VALUE renders as unsigned decimal via emitted helper.
    func testU64ToStringMaxValue() {
        XCTAssertEqual(
            "18446744073709551615",
            BaboonIdentifierRepr.u64ToString(UInt64.max)
        )
    }

    func testBytesToHexEmpty() {
        XCTAssertEqual("", BaboonIdentifierRepr.bytesToHex(Data()))
    }

    func testBytesToHexHighBytes() {
        let bs = Data([0xff, 0xfe, 0x00])
        XCTAssertEqual("fffe00", BaboonIdentifierRepr.bytesToHex(bs))
    }

    func testBitToStringLowercase() {
        XCTAssertEqual("true", BaboonIdentifierRepr.bitToString(true))
        XCTAssertEqual("false", BaboonIdentifierRepr.bitToString(false))
    }

    func testTsuToString24Chars() {
        var comps = DateComponents()
        comps.year = 2026; comps.month = 4; comps.day = 29
        comps.hour = 12; comps.minute = 34; comps.second = 56
        comps.nanosecond = 789_000_000
        var cal = Calendar(identifier: .gregorian)
        cal.timeZone = TimeZone(secondsFromGMT: 0)!
        let dt = cal.date(from: comps)!
        let s = BaboonIdentifierRepr.tsuToString(dt)
        XCTAssertEqual(24, s.count, "tsu format must be 24 chars; got: \(s)")
        XCTAssertEqual("2026-04-29T12:34:56.789Z", s)
    }

    func testTsoToString29CharsNeverZ() {
        // BaboonDateTimeOffset stores epoch+offset; 12:34:56 local at +02:00 = epoch ms
        // = 12:34:56 + (-2h) = 10:34:56 UTC = (epoch since 1970).
        var comps = DateComponents()
        comps.year = 2026; comps.month = 4; comps.day = 29
        comps.hour = 10; comps.minute = 34; comps.second = 56
        comps.nanosecond = 789_000_000
        var cal = Calendar(identifier: .gregorian)
        cal.timeZone = TimeZone(secondsFromGMT: 0)!
        let utcDate = cal.date(from: comps)!
        let epochMs = Int64(utcDate.timeIntervalSince1970 * 1000.0)
        let dto = BaboonDateTimeOffset(
            epochMillis: epochMs,
            offsetMillis: 2 * 3600 * 1000,
            kind: "offset"
        )
        let s = BaboonIdentifierRepr.tsoToString(dto)
        XCTAssertEqual(29, s.count)
        XCTAssertEqual("2026-04-29T12:34:56.789+02:00", s)

        let utcDto = BaboonDateTimeOffset(
            epochMillis: epochMs,
            offsetMillis: 0,
            kind: "utc"
        )
        let sUtc = BaboonIdentifierRepr.tsoToString(utcDto)
        XCTAssertFalse(sUtc.contains("Z"), "tso must never use Z shorthand: \(sUtc)")
    }

    // Spec §6.9: multi-field flat round-trip.
    func testPointIdRoundtripFlatMultifield() {
        let src = PointId(x: 1, label: "hello")
        let s = src.description
        XCTAssertEqual("PointId:1.0.0#x:1:label:hello", s)

        let parsed = PointIdCodec.parseRepr(s)
        guard case .right(let got) = parsed else {
            XCTFail("Expected right; got: \(parsed)")
            return
        }
        XCTAssertEqual(src, got)
    }

    // Spec §6.2: str with all 5 metacharacters round-trips.
    func testPointIdRoundtripStrAllMetacharacters() {
        let src = PointId(x: 0, label: "\\#:{}")
        let s = src.description
        XCTAssertEqual("PointId:1.0.0#x:0:label:\\\\\\#\\:\\{\\}", s)

        let parsed = PointIdCodec.parseRepr(s)
        guard case .right(let got) = parsed else {
            XCTFail("Expected right; got: \(parsed)")
            return
        }
        XCTAssertEqual(src, got)
    }

    // Spec §6.1: empty str field round-trips.
    func testPointIdRoundtripEmptyStr() {
        let src = PointId(x: 42, label: "")
        let s = src.description
        XCTAssertEqual("PointId:1.0.0#x:42:label:", s)

        let parsed = PointIdCodec.parseRepr(s)
        guard case .right(let got) = parsed else {
            XCTFail("Expected right; got: \(parsed)")
            return
        }
        XCTAssertEqual(src, got)
    }

    func testPointIdParseReprRejectsOutOfRangeI32() {
        let bad = "PointId:1.0.0#x:2147483648:label:foo"
        let parsed = PointIdCodec.parseRepr(bad)
        guard case .left(let err) = parsed else {
            XCTFail("Expected left")
            return
        }
        XCTAssertTrue(err.contains("i32 out of range"), "got: \(err)")
    }

    // Spec §6.8: UInts with u64.MAX_VALUE round-trips.
    func testUIntsRoundtripU64Max() {
        let src = UInts(a: 0, b: 0, c: 0, d: UInt64.max)
        let s = src.description
        XCTAssertTrue(
            s.contains("d:18446744073709551615"),
            "expected u64 MAX as 18446744073709551615; got: \(s)"
        )

        let parsed = UIntsCodec.parseRepr(s)
        guard case .right(let got) = parsed else {
            XCTFail("Expected right")
            return
        }
        XCTAssertEqual(src, got)
    }

    func testMixedRoundtripEmptyBytesAndUtcTimes() {
        var comps = DateComponents()
        comps.year = 2026; comps.month = 4; comps.day = 29
        comps.hour = 12; comps.minute = 34; comps.second = 56
        comps.nanosecond = 789_000_000
        var cal = Calendar(identifier: .gregorian)
        cal.timeZone = TimeZone(secondsFromGMT: 0)!
        let utcCreated = cal.date(from: comps)!

        // Same wall-clock at +02:00 → UTC is 10:34:56.789
        var schComps = DateComponents()
        schComps.year = 2026; schComps.month = 4; schComps.day = 29
        schComps.hour = 10; schComps.minute = 34; schComps.second = 56
        schComps.nanosecond = 789_000_000
        let schUtc = cal.date(from: schComps)!
        let scheduledMs = Int64(schUtc.timeIntervalSince1970 * 1000.0)
        let scheduled = BaboonDateTimeOffset(
            epochMillis: scheduledMs,
            offsetMillis: 2 * 3600 * 1000,
            kind: "offset"
        )

        let src = Mixed(
            active: true,
            id: UUID(uuidString: "DE7B9E1E-5C93-45FE-BEEC-DA99994F629A")!,
            payload: Data(),
            created: utcCreated,
            scheduled: scheduled
        )
        let s = src.description
        XCTAssertTrue(
            s.contains("Mixed:1.0.0#active:true:id:de7b9e1e-5c93-45fe-beec-da99994f629a:"),
            "source: \(s)"
        )
        XCTAssertTrue(
            s.contains(":payload::created:2026-04-29T12:34:56.789Z:scheduled:2026-04-29T12:34:56.789+02:00"),
            "source: \(s)"
        )

        let parsed = MixedCodec.parseRepr(s)
        guard case .right(let got) = parsed else {
            XCTFail("Expected right")
            return
        }
        XCTAssertEqual(src.active, got.active)
        XCTAssertEqual(src.id, got.id)
        XCTAssertEqual(0, got.payload.count)
        XCTAssertEqual(src.created, got.created)
        XCTAssertEqual(src.scheduled, got.scheduled)
    }

    func testOuterRoundtripNestedId() {
        let src = Outer(ref: PointId(x: 7, label: "k"), tag: "t")
        let s = src.description
        XCTAssertEqual("Outer:1.0.0#ref:{PointId:1.0.0#x:7:label:k}:tag:t", s)

        let parsed = OuterCodec.parseRepr(s)
        guard case .right(let got) = parsed else {
            XCTFail("Expected right")
            return
        }
        XCTAssertEqual(src, got)
    }

    // Spec §6.12: empty-fields id renders as `<Name>:<version>#`.
    func testMarkerEmptyFields() {
        let src = Marker()
        XCTAssertEqual("Marker:1.0.0#", src.description)

        let parsed = MarkerCodec.parseRepr("Marker:1.0.0#")
        guard case .right = parsed else {
            XCTFail("Expected right")
            return
        }
    }

    func testMixedParseReprRejectsUidMixedCase() {
        let bad = "Mixed:1.0.0#active:true:id:DE7B9E1E-5C93-45FE-BEEC-DA99994F629A:payload::created:2026-04-29T12:34:56.789Z:scheduled:2026-04-29T12:34:56.789+02:00"
        let parsed = MixedCodec.parseRepr(bad)
        guard case .left(let err) = parsed else {
            XCTFail("Expected left")
            return
        }
        XCTAssertTrue(err.contains("uid not in canonical lowercase form"), "got: \(err)")
    }

    // Spec §5.5: bare `\` followed by non-metachar is a parse error.
    func testPointIdParseReprRejectsInvalidEscape() {
        let bad = "PointId:1.0.0#x:0:label:foo\\zbar"
        let parsed = PointIdCodec.parseRepr(bad)
        guard case .left(let err) = parsed else {
            XCTFail("Expected left")
            return
        }
        XCTAssertTrue(err.contains("invalid escape"), "got: \(err)")
    }

    // Spec §5.5: trailing bare `\` is a parse error.
    func testPointIdParseReprRejectsTrailingBackslash() {
        let bad = "PointId:1.0.0#x:0:label:foo\\"
        let parsed = PointIdCodec.parseRepr(bad)
        guard case .left(let err) = parsed else {
            XCTFail("Expected left")
            return
        }
        XCTAssertTrue(err.contains("trailing backslash"), "got: \(err)")
    }

    // Spec §6.10: 4-level deep nested-id round-trip — A → B → C → D.
    func testDeepNestedRoundtripFourLevels() {
        let src = A(b: B(c: C(d: D(x: 42))))
        let s = src.description
        XCTAssertEqual(
            "A:1.0.0#b:{B:1.0.0#c:{C:1.0.0#d:{D:1.0.0#x:42}}}",
            s
        )

        let parsed = ACodec.parseRepr(s)
        guard case .right(let got) = parsed else {
            XCTFail("Expected right")
            return
        }
        XCTAssertEqual(src, got)
    }

    // Spec §5.4 (D06): unsigned values MUST NOT have a leading `+`.
    func testUIntsParseReprRejectsLeadingPlus() {
        let bad = "UInts:1.0.0#a:+1:b:2:c:3:d:4"
        let parsed = UIntsCodec.parseRepr(bad)
        guard case .left(let err) = parsed else {
            XCTFail("Expected left")
            return
        }
        XCTAssertTrue(err.contains("leading sign"), "got: \(err)")
    }

    // Spec §5.4 (PR-C): signed integer wire forms MUST NOT have a leading `+`.
    func testPointIdParseReprRejectsLeadingPlusOnSignedInt() {
        let bad = "PointId:1.0.0#x:+42:label:hello"
        let parsed = PointIdCodec.parseRepr(bad)
        guard case .left(let err) = parsed else {
            XCTFail("Expected left")
            return
        }
        XCTAssertTrue(err.contains("leading '+'"), "got: \(err)")
    }

    // Spec §5.4 (PR-C): i64 signed field must also reject leading `+`.
    func testLongIdParseReprRejectsLeadingPlusOnI64() {
        let bad = "LongId:1.0.0#x:+1"
        let parsed = LongIdCodec.parseRepr(bad)
        guard case .left(let err) = parsed else {
            XCTFail("Expected left")
            return
        }
        XCTAssertTrue(err.contains("leading '+'"), "got: \(err)")
    }

    func testParseBytesHexRejectsUppercase() {
        let parsed = BaboonIdentifierRepr.parseBytesHex("AABB")
        guard case .left(let err) = parsed else {
            XCTFail("Expected left")
            return
        }
        XCTAssertTrue(err.contains("non-lowercase or non-hex"), "got: \(err)")
    }

    func testParseBytesHexRejectsOddLength() {
        let parsed = BaboonIdentifierRepr.parseBytesHex("aab")
        guard case .left(let err) = parsed else {
            XCTFail("Expected left")
            return
        }
        XCTAssertTrue(err.contains("odd-length"), "got: \(err)")
    }

    func testIsCanonicalUidAcceptsLowercase() {
        XCTAssertTrue(
            BaboonIdentifierRepr.isCanonicalUid("de7b9e1e-5c93-45fe-beec-da99994f629a")
        )
    }

    func testIsCanonicalUidRejectsUppercase() {
        XCTAssertFalse(
            BaboonIdentifierRepr.isCanonicalUid("DE7B9E1E-5C93-45FE-BEEC-DA99994F629A")
        )
    }
}
