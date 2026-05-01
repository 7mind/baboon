// NOTE: This test references generated runtime symbols emitted by the Kotlin
// codegen path (BaboonIdentifierRepr, ByteString, Either, plus identifier.ok.*
// types). It runs only from the rsync'd codegen copy under
// target/test-regular/kt-stub/. PR-57b mirror of Java's IdentifierReprTest.
package runtime

import baboon.runtime.shared.BaboonIdentifierRepr
import baboon.runtime.shared.ByteString
import baboon.runtime.shared.Either
import identifier.ok.A
import identifier.ok.ACodec
import identifier.ok.B
import identifier.ok.C
import identifier.ok.D
import identifier.ok.LongId
import identifier.ok.LongIdCodec
import identifier.ok.Marker
import identifier.ok.MarkerCodec
import identifier.ok.Mixed
import identifier.ok.MixedCodec
import identifier.ok.Outer
import identifier.ok.OuterCodec
import identifier.ok.PointId
import identifier.ok.PointIdCodec
import identifier.ok.UInts
import identifier.ok.UIntsCodec
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertFalse
import org.junit.jupiter.api.Assertions.assertNotNull
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.Test
import java.time.OffsetDateTime
import java.time.ZoneOffset
import java.util.UUID

class IdentifierReprTest {

    // Spec §6.2: each of the 5 metacharacters escaped.
    @Test
    fun escapeStr_AllMetacharacters() {
        val src = "\\#:{}"
        assertEquals("\\\\\\#\\:\\{\\}", BaboonIdentifierRepr.escapeStr(src))
    }

    // Spec §6.3: trailing single backslash escapes to `\\`.
    @Test
    fun escapeStr_TrailingBackslash() {
        assertEquals("foo\\\\", BaboonIdentifierRepr.escapeStr("foo\\"))
    }

    // Spec §6.4: 4 backslashes round-trip.
    @Test
    fun escapeStr_AllBackslashes() {
        assertEquals("\\\\\\\\\\\\\\\\", BaboonIdentifierRepr.escapeStr("\\\\\\\\"))
    }

    // Spec §6.7: i64.MIN_VALUE renders as plain signed decimal.
    @Test
    fun longId_Roundtrip_Int64MinValue() {
        val src = LongId(Long.MIN_VALUE)
        val s = src.toString()
        assertEquals("LongId:1.0.0#x:-9223372036854775808", s)

        val parsed = LongIdCodec.parseRepr(s)
        assertTrue(parsed is Either.Right, "Expected Right; got: $parsed")
        assertEquals(src, (parsed as Either.Right).value)
    }

    @Test
    fun longId_Roundtrip_Int64MaxValue() {
        val src = LongId(Long.MAX_VALUE)
        val s = src.toString()
        assertEquals("LongId:1.0.0#x:9223372036854775807", s)

        val parsed = LongIdCodec.parseRepr(s)
        assertTrue(parsed is Either.Right)
        assertEquals(src, (parsed as Either.Right).value)
    }

    // Spec §6.8: u64.MAX_VALUE renders as unsigned decimal via emitted helper.
    @Test
    fun u64ToString_MaxValue() {
        // Kotlin u64 stored as ULong; max value is ULong.MAX_VALUE.
        assertEquals("18446744073709551615", BaboonIdentifierRepr.u64ToString(ULong.MAX_VALUE))
    }

    @Test
    fun bytesToHex_Empty() {
        assertEquals("", BaboonIdentifierRepr.bytesToHex(ByteString.of(ByteArray(0))))
    }

    @Test
    fun bytesToHex_HighBytes() {
        val bs = ByteString.of(byteArrayOf(0xff.toByte(), 0xfe.toByte(), 0x00))
        assertEquals("fffe00", BaboonIdentifierRepr.bytesToHex(bs))
    }

    @Test
    fun bitToString_Lowercase() {
        assertEquals("true", BaboonIdentifierRepr.bitToString(true))
        assertEquals("false", BaboonIdentifierRepr.bitToString(false))
    }

    @Test
    fun tsuToString_24Chars() {
        val dt = OffsetDateTime.of(2026, 4, 29, 12, 34, 56, 789_000_000, ZoneOffset.UTC)
        val s = BaboonIdentifierRepr.tsuToString(dt)
        assertEquals(24, s.length, "tsu format must be 24 chars")
        assertEquals("2026-04-29T12:34:56.789Z", s)
    }

    @Test
    fun tsoToString_29Chars_NeverZ() {
        val dt = OffsetDateTime.of(2026, 4, 29, 12, 34, 56, 789_000_000, ZoneOffset.ofHours(2))
        val s = BaboonIdentifierRepr.tsoToString(dt)
        assertEquals(29, s.length)
        assertEquals("2026-04-29T12:34:56.789+02:00", s)

        val utc = OffsetDateTime.of(2026, 4, 29, 12, 34, 56, 789_000_000, ZoneOffset.UTC)
        val sUtc = BaboonIdentifierRepr.tsoToString(utc)
        assertEquals("2026-04-29T12:34:56.789+00:00", sUtc)
        assertFalse(sUtc.contains("Z"), "tso must never use Z shorthand: $sUtc")
    }

    // Spec §6.9: multi-field flat round-trip.
    @Test
    fun pointId_Roundtrip_FlatMultiField() {
        val src = PointId(1, "hello")
        val s = src.toString()
        assertEquals("PointId:1.0.0#x:1:label:hello", s)

        val parsed = PointIdCodec.parseRepr(s)
        assertTrue(parsed is Either.Right)
        assertEquals(src, (parsed as Either.Right).value)
    }

    // Spec §6.2: str with all 5 metacharacters round-trips.
    @Test
    fun pointId_Roundtrip_StrWithAllMetacharacters() {
        val src = PointId(0, "\\#:{}")
        val s = src.toString()
        assertEquals("PointId:1.0.0#x:0:label:\\\\\\#\\:\\{\\}", s)

        val parsed = PointIdCodec.parseRepr(s)
        assertTrue(parsed is Either.Right)
        assertEquals(src, (parsed as Either.Right).value)
    }

    // Spec §6.1: empty str field round-trips.
    @Test
    fun pointId_Roundtrip_EmptyStr() {
        val src = PointId(42, "")
        val s = src.toString()
        assertEquals("PointId:1.0.0#x:42:label:", s)

        val parsed = PointIdCodec.parseRepr(s)
        assertTrue(parsed is Either.Right)
        assertEquals(src, (parsed as Either.Right).value)
    }

    @Test
    fun pointId_ParseRepr_RejectsOutOfRange_I32() {
        val bad = "PointId:1.0.0#x:2147483648:label:foo"
        val parsed = PointIdCodec.parseRepr(bad)
        assertTrue(parsed is Either.Left)
        assertTrue(((parsed as Either.Left).value).contains("i32 out of range"),
            "Expected 'i32 out of range' but got: ${parsed.value}")
    }

    // Spec §6.8: UInts with u64.MAX_VALUE round-trips.
    @Test
    fun uInts_Roundtrip_U64MaxValue() {
        val src = UInts(0u, 0u, 0u, ULong.MAX_VALUE)
        val s = src.toString()
        assertTrue(s.contains("d:18446744073709551615"),
            "Expected u64 MAX as 18446744073709551615; got: $s")

        val parsed = UIntsCodec.parseRepr(s)
        assertTrue(parsed is Either.Right)
        assertEquals(src, (parsed as Either.Right).value)
    }

    @Test
    fun mixed_Roundtrip_EmptyBytes_AndUtcTimes() {
        val src = Mixed(
            true,
            UUID.fromString("de7b9e1e-5c93-45fe-beec-da99994f629a"),
            ByteString.of(ByteArray(0)),
            OffsetDateTime.of(2026, 4, 29, 12, 34, 56, 789_000_000, ZoneOffset.UTC),
            OffsetDateTime.of(2026, 4, 29, 12, 34, 56, 789_000_000, ZoneOffset.ofHours(2))
        )
        val s = src.toString()
        assertTrue(s.contains("Mixed:1.0.0#active:true:id:de7b9e1e-5c93-45fe-beec-da99994f629a:"),
            "Source: $s")
        assertTrue(s.contains(":payload::created:2026-04-29T12:34:56.789Z:scheduled:2026-04-29T12:34:56.789+02:00"),
            "Source: $s")

        val parsed = MixedCodec.parseRepr(s)
        assertTrue(parsed is Either.Right)
        val got = (parsed as Either.Right).value
        assertEquals(src.active, got.active)
        assertEquals(src.id, got.id)
        assertEquals(0, got.payload.underlyingUnsafe().size)
        assertEquals(src.created, got.created)
        assertEquals(src.scheduled, got.scheduled)
    }

    // Nested id: 1-level via Outer→PointId.
    @Test
    fun outer_Roundtrip_NestedId() {
        val src = Outer(PointId(7, "k"), "t")
        val s = src.toString()
        assertEquals("Outer:1.0.0#ref:{PointId:1.0.0#x:7:label:k}:tag:t", s)

        val parsed = OuterCodec.parseRepr(s)
        assertTrue(parsed is Either.Right)
        assertEquals(src, (parsed as Either.Right).value)
    }

    // Spec §6.12: empty-fields id renders as `<Name>:<version>#`.
    @Test
    fun marker_EmptyFields() {
        val src = Marker()
        assertEquals("Marker:1.0.0#", src.toString())

        val parsed = MarkerCodec.parseRepr("Marker:1.0.0#")
        assertTrue(parsed is Either.Right)
        assertNotNull((parsed as Either.Right).value)
    }

    @Test
    fun mixed_ParseRepr_RejectsUidMixedCase() {
        val bad = "Mixed:1.0.0#active:true:id:DE7B9E1E-5C93-45FE-BEEC-DA99994F629A:payload::created:2026-04-29T12:34:56.789Z:scheduled:2026-04-29T12:34:56.789+02:00"
        val parsed = MixedCodec.parseRepr(bad)
        assertTrue(parsed is Either.Left)
        assertTrue(((parsed as Either.Left).value).contains("uid not in canonical lowercase form"),
            "Expected lowercase enforcement; got: ${parsed.value}")
    }

    // Spec §5.5: bare `\` followed by non-metachar is a parse error.
    @Test
    fun pointId_ParseRepr_RejectsInvalidEscape() {
        val bad = "PointId:1.0.0#x:0:label:foo\\zbar"
        val parsed = PointIdCodec.parseRepr(bad)
        assertTrue(parsed is Either.Left)
        assertTrue(((parsed as Either.Left).value).contains("invalid escape"),
            "Expected 'invalid escape' but got: ${parsed.value}")
    }

    // Spec §5.5: trailing bare `\` is a parse error.
    @Test
    fun pointId_ParseRepr_RejectsTrailingBackslash() {
        val bad = "PointId:1.0.0#x:0:label:foo\\"
        val parsed = PointIdCodec.parseRepr(bad)
        assertTrue(parsed is Either.Left)
        assertTrue(((parsed as Either.Left).value).contains("trailing backslash"),
            "Expected 'trailing backslash' but got: ${parsed.value}")
    }

    // Spec §6.10: 4-level deep nested-id round-trip — A → B → C → D.
    @Test
    fun deepNested_Roundtrip_FourLevels() {
        val src = A(B(C(D(42))))
        val s = src.toString()
        assertEquals("A:1.0.0#b:{B:1.0.0#c:{C:1.0.0#d:{D:1.0.0#x:42}}}", s)

        val parsed = ACodec.parseRepr(s)
        assertTrue(parsed is Either.Right)
        assertEquals(src, (parsed as Either.Right).value)
    }

    // Spec §5.4 (D06): unsigned values MUST NOT have a leading `+`.
    @Test
    fun uInts_ParseRepr_RejectsLeadingPlus() {
        val bad = "UInts:1.0.0#a:+1:b:2:c:3:d:4"
        val parsed = UIntsCodec.parseRepr(bad)
        assertTrue(parsed is Either.Left)
        assertTrue(((parsed as Either.Left).value).contains("leading sign"),
            "Expected 'leading sign' but got: ${parsed.value}")
    }

    // Spec §5.4 (PR-C): signed integer wire forms MUST NOT have a leading `+`.
    @Test
    fun pointId_ParseRepr_RejectsLeadingPlusOnSignedInt() {
        val bad = "PointId:1.0.0#x:+42:label:hello"
        val parsed = PointIdCodec.parseRepr(bad)
        assertTrue(parsed is Either.Left)
        assertTrue(((parsed as Either.Left).value).contains("leading '+'"),
            "Expected leading '+' rejection but got: ${(parsed as Either.Left).value}")
    }

    // Spec §5.4 (PR-C): i64 signed field must also reject leading `+`.
    @Test
    fun longId_ParseRepr_RejectsLeadingPlusOnI64() {
        val bad = "LongId:1.0.0#x:+1"
        val parsed = LongIdCodec.parseRepr(bad)
        assertTrue(parsed is Either.Left)
        assertTrue(((parsed as Either.Left).value).contains("leading '+'"),
            "Expected leading '+' rejection but got: ${(parsed as Either.Left).value}")
    }
}
