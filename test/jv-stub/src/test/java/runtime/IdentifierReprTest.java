package runtime;

import baboon.runtime.shared.BaboonEither;
import baboon.runtime.shared.BaboonIdentifierRepr;
import baboon.runtime.shared.ByteString;
import identifier.ok.A;
import identifier.ok.ACodec;
import identifier.ok.B;
import identifier.ok.C;
import identifier.ok.D;
import identifier.ok.LongId;
import identifier.ok.LongIdCodec;
import identifier.ok.Marker;
import identifier.ok.MarkerCodec;
import identifier.ok.Mixed;
import identifier.ok.MixedCodec;
import identifier.ok.Outer;
import identifier.ok.OuterCodec;
import identifier.ok.PointId;
import identifier.ok.PointIdCodec;
import identifier.ok.UInts;
import identifier.ok.UIntsCodec;

import org.junit.jupiter.api.Test;

import java.time.OffsetDateTime;
import java.time.ZoneOffset;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertNotNull;

/** PR-57a hand-crafted edge-case tests for `id` toString + parseRepr per
 *  docs/spec/identifier-repr.md. Exercises the runtime helper directly plus
 *  emitted PointId/Mixed/UInts/Outer/Marker codecs end-to-end.
 *
 *  NOTE: requires generated code under identifier.ok package; runs only from
 *  the rsync'd codegen copy under target/test-regular/jv-stub/. */
public class IdentifierReprTest {

    // Spec §6.2: each of the 5 metacharacters escaped.
    @Test
    void escapeStr_AllMetacharacters() {
        // 5 chars: backslash, hash, colon, open brace, close brace.
        var src = "\\#:{}";
        var got = BaboonIdentifierRepr.escapeStr(src);
        assertEquals("\\\\\\#\\:\\{\\}", got);
    }

    // Spec §6.3: trailing single backslash escapes to `\\`.
    @Test
    void escapeStr_TrailingBackslash() {
        assertEquals("foo\\\\", BaboonIdentifierRepr.escapeStr("foo\\"));
    }

    // Spec §6.4: 4 backslashes round-trip.
    @Test
    void escapeStr_AllBackslashes() {
        assertEquals("\\\\\\\\\\\\\\\\", BaboonIdentifierRepr.escapeStr("\\\\\\\\"));
    }

    // Spec §6.7: i64.MIN_VALUE renders as plain signed decimal via emitted codec round-trip.
    @Test
    void longId_Roundtrip_Int64MinValue() {
        var src = new LongId(Long.MIN_VALUE);
        var s = src.toString();
        assertEquals("LongId:1.0.0#x:-9223372036854775808", s);

        var parsed = LongIdCodec.parseRepr(s);
        var right = assertInstanceOf(BaboonEither.Right.class, parsed);
        assertEquals(src, right.value());
    }

    // Spec §6.7 (paired): i64.MAX_VALUE round-trips through emitted codec.
    @Test
    void longId_Roundtrip_Int64MaxValue() {
        var src = new LongId(Long.MAX_VALUE);
        var s = src.toString();
        assertEquals("LongId:1.0.0#x:9223372036854775807", s);

        var parsed = LongIdCodec.parseRepr(s);
        var right = assertInstanceOf(BaboonEither.Right.class, parsed);
        assertEquals(src, right.value());
    }

    // Spec §6.8: u64.MAX_VALUE renders as unsigned decimal.
    @Test
    void u64ToString_MaxValue() {
        // u64 stored as signed long → -1L
        assertEquals("18446744073709551615", BaboonIdentifierRepr.u64ToString(-1L));
    }

    // Spec §6.5: bytes empty → empty string.
    @Test
    void bytesToHex_Empty() {
        assertEquals("", BaboonIdentifierRepr.bytesToHex(ByteString.of(new byte[0])));
    }

    // Spec §6.6: bytes lowercase hex no separators.
    @Test
    void bytesToHex_HighBytes() {
        assertEquals("fffe00", BaboonIdentifierRepr.bytesToHex(
            ByteString.of(new byte[] { (byte) 0xff, (byte) 0xfe, (byte) 0x00 })));
    }

    // Spec §3 row bit: lowercase ASCII.
    @Test
    void bitToString_Lowercase() {
        assertEquals("true", BaboonIdentifierRepr.bitToString(true));
        assertEquals("false", BaboonIdentifierRepr.bitToString(false));
    }

    // Spec §3: tsu = 24 chars.
    @Test
    void tsuToString_24Chars() {
        var dt = OffsetDateTime.of(2026, 4, 29, 12, 34, 56, 789_000_000, ZoneOffset.UTC);
        var s = BaboonIdentifierRepr.tsuToString(dt);
        assertEquals(24, s.length(), "tsu format must be 24 chars");
        assertEquals("2026-04-29T12:34:56.789Z", s);
    }

    // Spec §3: tso = 29 chars; never collapses to Z.
    @Test
    void tsoToString_29Chars_NeverZ() {
        var dt = OffsetDateTime.of(2026, 4, 29, 12, 34, 56, 789_000_000, ZoneOffset.ofHours(2));
        var s = BaboonIdentifierRepr.tsoToString(dt);
        assertEquals(29, s.length());
        assertEquals("2026-04-29T12:34:56.789+02:00", s);

        // Verify UTC tso also yields ±HH:MM, not Z.
        var utc = OffsetDateTime.of(2026, 4, 29, 12, 34, 56, 789_000_000, ZoneOffset.UTC);
        var sUtc = BaboonIdentifierRepr.tsoToString(utc);
        assertEquals("2026-04-29T12:34:56.789+00:00", sUtc);
        assertTrue(!sUtc.contains("Z"), "tso must never use Z shorthand: " + sUtc);
    }

    // Spec §6.9: multi-field flat round-trip via emitted codec.
    @Test
    void pointId_Roundtrip_FlatMultiField() {
        var src = new PointId(1, "hello");
        var s = src.toString();
        assertEquals("PointId:1.0.0#x:1:label:hello", s);

        var parsed = PointIdCodec.parseRepr(s);
        var right = assertInstanceOf(BaboonEither.Right.class, parsed);
        assertEquals(src, right.value());
    }

    // Spec §6.2: str with all 5 metacharacters survives round-trip.
    @Test
    void pointId_Roundtrip_StrWithAllMetacharacters() {
        var src = new PointId(0, "\\#:{}");
        var s = src.toString();
        assertEquals("PointId:1.0.0#x:0:label:\\\\\\#\\:\\{\\}", s);

        var parsed = PointIdCodec.parseRepr(s);
        var right = assertInstanceOf(BaboonEither.Right.class, parsed);
        assertEquals(src, right.value());
    }

    // Spec §6.1: empty str field round-trips.
    @Test
    void pointId_Roundtrip_EmptyStr() {
        var src = new PointId(42, "");
        var s = src.toString();
        assertEquals("PointId:1.0.0#x:42:label:", s);

        var parsed = PointIdCodec.parseRepr(s);
        var right = assertInstanceOf(BaboonEither.Right.class, parsed);
        assertEquals(src, right.value());
    }

    // i32 boundary check.
    @Test
    void pointId_ParseRepr_RejectsOutOfRange_I32() {
        var bad = "PointId:1.0.0#x:2147483648:label:foo";
        var parsed = PointIdCodec.parseRepr(bad);
        var left = assertInstanceOf(BaboonEither.Left.class, parsed);
        assertTrue(((String) left.value()).contains("i32 out of range"),
            "Expected 'i32 out of range' but got: " + left.value());
    }

    // Spec §6.8: UInts with u64.MAX_VALUE round-trips.
    @Test
    void uInts_Roundtrip_U64MaxValue() {
        var src = new UInts((short) 0, 0, 0L, -1L);  // u64 MAX as signed -1
        var s = src.toString();
        assertTrue(s.contains("d:18446744073709551615"),
            "Expected u64 MAX as 18446744073709551615; got: " + s);

        var parsed = UIntsCodec.parseRepr(s);
        var right = assertInstanceOf(BaboonEither.Right.class, parsed);
        assertEquals(src, right.value());
    }

    // Mixed round-trip with empty bytes.
    @Test
    void mixed_Roundtrip_EmptyBytes_AndUtcTimes() {
        var src = new Mixed(
            true,
            UUID.fromString("de7b9e1e-5c93-45fe-beec-da99994f629a"),
            ByteString.of(new byte[0]),
            OffsetDateTime.of(2026, 4, 29, 12, 34, 56, 789_000_000, ZoneOffset.UTC),
            OffsetDateTime.of(2026, 4, 29, 12, 34, 56, 789_000_000, ZoneOffset.ofHours(2))
        );
        var s = src.toString();
        assertTrue(s.contains("Mixed:1.0.0#active:true:id:de7b9e1e-5c93-45fe-beec-da99994f629a:"),
            "Source: " + s);
        assertTrue(s.contains(":payload::created:2026-04-29T12:34:56.789Z:scheduled:2026-04-29T12:34:56.789+02:00"),
            "Source: " + s);

        var parsed = MixedCodec.parseRepr(s);
        var right = assertInstanceOf(BaboonEither.Right.class, parsed);
        var got = (Mixed) right.value();
        assertEquals(src.active(), got.active());
        assertEquals(src.id(), got.id());
        assertEquals(0, got.payload().underlyingUnsafe().length);
    }

    // Spec §6.10 nested-id (only 1 level via Outer→PointId for fixture).
    @Test
    void outer_Roundtrip_NestedId() {
        var src = new Outer(new PointId(7, "k"), "t");
        var s = src.toString();
        assertEquals("Outer:1.0.0#ref:{PointId:1.0.0#x:7:label:k}:tag:t", s);

        var parsed = OuterCodec.parseRepr(s);
        var right = assertInstanceOf(BaboonEither.Right.class, parsed);
        assertEquals(src, right.value());
    }

    // Spec §6.12: empty-fields id renders as `<Name>:<version>#`.
    @Test
    void marker_EmptyFields() {
        var src = new Marker();
        assertEquals("Marker:1.0.0#", src.toString());

        var parsed = MarkerCodec.parseRepr("Marker:1.0.0#");
        var right = assertInstanceOf(BaboonEither.Right.class, parsed);
        assertNotNull(right.value());
    }

    // Uid: parser rejects mixed-case form (spec §5.4 strict lowercase).
    @Test
    void mixed_ParseRepr_RejectsUidMixedCase() {
        var bad = "Mixed:1.0.0#active:true:id:DE7B9E1E-5C93-45FE-BEEC-DA99994F629A:payload::created:2026-04-29T12:34:56.789Z:scheduled:2026-04-29T12:34:56.789+02:00";
        var parsed = MixedCodec.parseRepr(bad);
        var left = assertInstanceOf(BaboonEither.Left.class, parsed);
        assertTrue(((String) left.value()).contains("uid not in canonical lowercase form"),
            "Expected lowercase enforcement; got: " + left.value());
    }

    // Spec §5.5: bare `\` followed by non-metachar is a parse error.
    // In Java string literals, `"foo\\zbar"` is the runtime string foo + backslash + z + bar.
    @Test
    void pointId_ParseRepr_RejectsInvalidEscape() {
        var bad = "PointId:1.0.0#x:0:label:foo\\zbar";
        var parsed = PointIdCodec.parseRepr(bad);
        var left = assertInstanceOf(BaboonEither.Left.class, parsed);
        assertTrue(((String) left.value()).contains("invalid escape"),
            "Expected 'invalid escape' but got: " + left.value());
    }

    // Spec §5.5: trailing bare `\` is a parse error.
    // The Java literal `"foo\\"` is foo + one backslash.
    @Test
    void pointId_ParseRepr_RejectsTrailingBackslash() {
        var bad = "PointId:1.0.0#x:0:label:foo\\";
        var parsed = PointIdCodec.parseRepr(bad);
        var left = assertInstanceOf(BaboonEither.Left.class, parsed);
        assertTrue(((String) left.value()).contains("trailing backslash"),
            "Expected 'trailing backslash' but got: " + left.value());
    }

    // Spec §6.10: 4-level deep nested-id round-trip — A → B → C → D.
    // Each nesting level adds one `{...}` wrapper.
    @Test
    void deepNested_Roundtrip_FourLevels() {
        var src = new A(new B(new C(new D(42))));
        var s = src.toString();
        assertEquals("A:1.0.0#b:{B:1.0.0#c:{C:1.0.0#d:{D:1.0.0#x:42}}}", s);

        var parsed = ACodec.parseRepr(s);
        var right = assertInstanceOf(BaboonEither.Right.class, parsed);
        assertEquals(src, right.value());
    }

    // Spec §5.4 (D06): unsigned values MUST NOT have a leading `+`.
    // Cross-language invariant: both C# and Java reject this.
    @Test
    void uInts_ParseRepr_RejectsLeadingPlus() {
        var bad = "UInts:1.0.0#a:+1:b:2:c:3:d:4";
        var parsed = UIntsCodec.parseRepr(bad);
        var left = assertInstanceOf(BaboonEither.Left.class, parsed);
        // Java emitter rejects leading sign explicitly per D06 fix.
        assertTrue(((String) left.value()).contains("leading sign"),
            "Expected 'leading sign' but got: " + left.value());
    }

    // Spec §5.4 (PR-C): signed integer wire forms MUST NOT have a leading `+`.
    @Test
    void pointId_ParseRepr_RejectsLeadingPlusOnSignedInt() {
        var bad = "PointId:1.0.0#x:+42:label:hello";
        var parsed = PointIdCodec.parseRepr(bad);
        var left = assertInstanceOf(BaboonEither.Left.class, parsed);
        assertTrue(((String) left.value()).contains("leading '+'"),
            "Expected leading '+' rejection but got: " + left.value());
    }

    // Spec §5.4 (PR-C): i64 signed field must also reject leading `+`.
    @Test
    void longId_ParseRepr_RejectsLeadingPlusOnI64() {
        var bad = "LongId:1.0.0#x:+1";
        var parsed = LongIdCodec.parseRepr(bad);
        var left = assertInstanceOf(BaboonEither.Left.class, parsed);
        assertTrue(((String) left.value()).contains("leading '+'"),
            "Expected leading '+' rejection but got: " + left.value());
    }
}
