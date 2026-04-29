// PR-57a hand-crafted edge-case tests for `id` toString + ParseRepr per
// docs/spec/identifier-repr.md. Exercises the runtime helper directly plus
// emitted PointId/Mixed/UInts/Outer/Marker codecs end-to-end.
//
// NOTE: requires generated code under Identifier.Ok namespace; runs only from
// the rsync'd codegen copy under target/test-regular/cs-stub/.
#nullable enable

using System;
using System.Globalization;
using System.Text;
using Baboon.Runtime.Shared;
using Baboon.Time;
using Identifier.Ok;
using NUnit.Framework;

namespace ConversionsTest
{
    [TestFixture]
    public class IdentifierReprTests
    {
        // Spec §6.2: each of the 5 metacharacters escaped.
        [Test]
        public void EscapeStr_AllMetacharacters()
        {
            // 5 chars: backslash, hash, colon, open brace, close brace.
            var src = "\\#:{}";
            var got = BaboonIdentifierRepr.EscapeStr(src);
            Assert.That(got, Is.EqualTo("\\\\\\#\\:\\{\\}"));
        }

        // Spec §6.3: trailing single backslash escapes to `\\`.
        [Test]
        public void EscapeStr_TrailingBackslash()
        {
            Assert.That(BaboonIdentifierRepr.EscapeStr("foo\\"), Is.EqualTo("foo\\\\"));
        }

        // Spec §6.4: 4 backslashes round-trip.
        [Test]
        public void EscapeStr_AllBackslashes()
        {
            Assert.That(BaboonIdentifierRepr.EscapeStr("\\\\\\\\"), Is.EqualTo("\\\\\\\\\\\\\\\\"));
        }

        // Spec §6.7: i64.MIN_VALUE renders as plain signed decimal via emitted codec round-trip.
        [Test]
        public void LongId_Roundtrip_Int64MinValue()
        {
            var src = new LongId(X: long.MinValue);
            var s = src.ToString();
            Assert.That(s, Is.EqualTo("LongId:1.0.0#x:-9223372036854775808"));

            var parsed = LongIdCodec.ParseRepr(s);
            var right = parsed as Either<string, LongId>.Right;
            Assert.That(right, Is.Not.Null);
            Assert.That(right!.Value, Is.EqualTo(src));
        }

        // Spec §6.7 (paired): i64.MAX_VALUE round-trips through emitted codec.
        [Test]
        public void LongId_Roundtrip_Int64MaxValue()
        {
            var src = new LongId(X: long.MaxValue);
            var s = src.ToString();
            Assert.That(s, Is.EqualTo("LongId:1.0.0#x:9223372036854775807"));

            var parsed = LongIdCodec.ParseRepr(s);
            var right = parsed as Either<string, LongId>.Right;
            Assert.That(right, Is.Not.Null);
            Assert.That(right!.Value, Is.EqualTo(src));
        }

        // Spec §6.8: u64.MAX_VALUE renders as unsigned decimal.
        [Test]
        public void U64ToString_MaxValue()
        {
            Assert.That(BaboonIdentifierRepr.U64ToString(ulong.MaxValue), Is.EqualTo("18446744073709551615"));
        }

        // Spec §6.5: bytes empty → empty string.
        [Test]
        public void BytesToHex_Empty()
        {
            var bs = new ByteString(Array.Empty<byte>());
            Assert.That(BaboonIdentifierRepr.BytesToHex(bs), Is.EqualTo(""));
        }

        // Spec §6.6: bytes lowercase hex no separators.
        [Test]
        public void BytesToHex_HighBytes()
        {
            var bs = new ByteString(new byte[] { 0xff, 0xfe, 0x00 });
            Assert.That(BaboonIdentifierRepr.BytesToHex(bs), Is.EqualTo("fffe00"));
        }

        // Spec §3 row bit: lowercase ASCII.
        [Test]
        public void BitToString_Lowercase()
        {
            Assert.That(BaboonIdentifierRepr.BitToString(true), Is.EqualTo("true"));
            Assert.That(BaboonIdentifierRepr.BitToString(false), Is.EqualTo("false"));
        }

        // Spec §3: tsu = 24 chars.
        [Test]
        public void TsuToString_24Chars()
        {
            var dto = new DateTimeOffset(2026, 4, 29, 12, 34, 56, 789, TimeSpan.Zero);
            var s = BaboonIdentifierRepr.TsuToString(dto);
            Assert.That(s.Length, Is.EqualTo(24), "tsu format must be 24 chars");
            Assert.That(s, Is.EqualTo("2026-04-29T12:34:56.789Z"));
        }

        // Spec §3: tso = 29 chars; never collapses to Z.
        [Test]
        public void TsoToString_29Chars_NeverZ()
        {
            var dto = new DateTimeOffset(2026, 4, 29, 12, 34, 56, 789, TimeSpan.FromHours(2));
            var s = BaboonIdentifierRepr.TsoToString(dto);
            Assert.That(s.Length, Is.EqualTo(29));
            Assert.That(s, Is.EqualTo("2026-04-29T12:34:56.789+02:00"));

            // Verify UTC tso also yields ±HH:MM, not Z.
            var utc = new DateTimeOffset(2026, 4, 29, 12, 34, 56, 789, TimeSpan.Zero);
            var sUtc = BaboonIdentifierRepr.TsoToString(utc);
            Assert.That(sUtc, Is.EqualTo("2026-04-29T12:34:56.789+00:00"));
            StringAssert.DoesNotContain("Z", sUtc);
        }

        // Spec §6.9: multi-field flat round-trip via emitted codec.
        [Test]
        public void PointId_Roundtrip_FlatMultiField()
        {
            var src = new PointId(X: 1, Label: "hello");
            var s = src.ToString();
            Assert.That(s, Is.EqualTo("PointId:1.0.0#x:1:label:hello"));

            var parsed = PointIdCodec.ParseRepr(s);
            Assert.That(parsed, Is.InstanceOf<Either<string, PointId>.Right>());
            Assert.That(((Either<string, PointId>.Right)parsed).Value, Is.EqualTo(src));
        }

        // Spec §6.2: str with all 5 metacharacters survives round-trip.
        [Test]
        public void PointId_Roundtrip_StrWithAllMetacharacters()
        {
            var src = new PointId(X: 0, Label: "\\#:{}");
            var s = src.ToString();
            Assert.That(s, Is.EqualTo("PointId:1.0.0#x:0:label:\\\\\\#\\:\\{\\}"));

            var parsed = PointIdCodec.ParseRepr(s);
            var right = parsed as Either<string, PointId>.Right;
            Assert.That(right, Is.Not.Null);
            Assert.That(right!.Value, Is.EqualTo(src));
        }

        // Spec §6.1: empty str field round-trips.
        [Test]
        public void PointId_Roundtrip_EmptyStr()
        {
            var src = new PointId(X: 42, Label: "");
            var s = src.ToString();
            Assert.That(s, Is.EqualTo("PointId:1.0.0#x:42:label:"));

            var parsed = PointIdCodec.ParseRepr(s);
            var right = parsed as Either<string, PointId>.Right;
            Assert.That(right, Is.Not.Null);
            Assert.That(right!.Value, Is.EqualTo(src));
        }

        // Spec §6.7: i64 boundaries in PointId-like (we use UInts to exercise unsigned).
        // Here we just verify i32 boundary inside PointId.
        [Test]
        public void PointId_ParseRepr_RejectsOutOfRange_I32()
        {
            // x is i32; 2^31 is out of range.
            var bad = "PointId:1.0.0#x:2147483648:label:foo";
            var parsed = PointIdCodec.ParseRepr(bad);
            Assert.That(parsed, Is.InstanceOf<Either<string, PointId>.Left>());
            var left = (Either<string, PointId>.Left)parsed;
            StringAssert.Contains("i32 out of range", left.Value);
        }

        // Spec §6.8: UInts with u64.MAX_VALUE round-trips.
        [Test]
        public void UInts_Roundtrip_U64MaxValue()
        {
            var src = new UInts(A: 0, B: 0, C: 0, D: ulong.MaxValue);
            var s = src.ToString();
            StringAssert.Contains("d:18446744073709551615", s);

            var parsed = UIntsCodec.ParseRepr(s);
            var right = parsed as Either<string, UInts>.Right;
            Assert.That(right, Is.Not.Null);
            Assert.That(right!.Value, Is.EqualTo(src));
        }

        // Spec §6.5: bytes empty + Mixed round-trip.
        [Test]
        public void Mixed_Roundtrip_EmptyBytes_AndUtcTimes()
        {
            var src = new Mixed(
                Active: true,
                Id: Guid.Parse("de7b9e1e-5c93-45fe-beec-da99994f629a"),
                Payload: new ByteString(Array.Empty<byte>()),
                Created: new RpDateTime(new DateTimeOffset(2026, 4, 29, 12, 34, 56, 789, TimeSpan.Zero)),
                Scheduled: new RpDateTime(new DateTimeOffset(2026, 4, 29, 12, 34, 56, 789, TimeSpan.FromHours(2)))
            );
            var s = src.ToString();
            // Spec §6.11 mixed-field example structure (with empty bytes here).
            StringAssert.Contains("Mixed:1.0.0#active:true:id:de7b9e1e-5c93-45fe-beec-da99994f629a:", s);
            StringAssert.Contains(":payload::created:2026-04-29T12:34:56.789Z:scheduled:2026-04-29T12:34:56.789+02:00", s);

            var parsed = MixedCodec.ParseRepr(s);
            var right = parsed as Either<string, Mixed>.Right;
            Assert.That(right, Is.Not.Null,
                "expected Right but got: " + (parsed is Either<string, Mixed>.Left l ? l.Value : "?"));
            // Verify field-by-field equality (Mixed is a record so structural).
            var got = right!.Value;
            Assert.That(got.Active, Is.EqualTo(src.Active));
            Assert.That(got.Id, Is.EqualTo(src.Id));
            Assert.That(got.Payload.UnderlyingUnsafe().Length, Is.EqualTo(0));
        }

        // Spec §6.10: deep nested via Outer→PointId (only 1 level here, matching fixture shape).
        [Test]
        public void Outer_Roundtrip_NestedId()
        {
            var src = new Outer(Ref: new PointId(X: 7, Label: "k"), Tag: "t");
            var s = src.ToString();
            Assert.That(s, Is.EqualTo("Outer:1.0.0#ref:{PointId:1.0.0#x:7:label:k}:tag:t"));

            var parsed = OuterCodec.ParseRepr(s);
            var right = parsed as Either<string, Outer>.Right;
            Assert.That(right, Is.Not.Null);
            Assert.That(right!.Value, Is.EqualTo(src));
        }

        // Spec §6.12: empty-fields id renders as `<Name>:<version>#`.
        [Test]
        public void Marker_EmptyFields()
        {
            var src = new Marker();
            Assert.That(src.ToString(), Is.EqualTo("Marker:1.0.0#"));

            var parsed = MarkerCodec.ParseRepr("Marker:1.0.0#");
            var right = parsed as Either<string, Marker>.Right;
            Assert.That(right, Is.Not.Null);
        }

        // Uid: parser rejects mixed-case form (spec §5.4 strict lowercase).
        [Test]
        public void Mixed_ParseRepr_RejectsUidMixedCase()
        {
            var bad = "Mixed:1.0.0#active:true:id:DE7B9E1E-5C93-45FE-BEEC-DA99994F629A:payload::created:2026-04-29T12:34:56.789Z:scheduled:2026-04-29T12:34:56.789+02:00";
            var parsed = MixedCodec.ParseRepr(bad);
            var left = parsed as Either<string, Mixed>.Left;
            Assert.That(left, Is.Not.Null, "Expected Left but got Right");
            StringAssert.Contains("uid not in canonical lowercase form", left!.Value);
        }

        // Spec §5.5: bare `\` followed by non-metachar is a parse error.
        // The C# string literal `"foo\\zbar"` contains the runtime characters
        // foo + backslash + z + bar — i.e. a backslash followed by 'z'.
        [Test]
        public void PointId_ParseRepr_RejectsInvalidEscape()
        {
            var bad = "PointId:1.0.0#x:0:label:foo\\zbar";
            var parsed = PointIdCodec.ParseRepr(bad);
            var left = parsed as Either<string, PointId>.Left;
            Assert.That(left, Is.Not.Null, "Expected Left for invalid escape; got Right");
            StringAssert.Contains("invalid escape", left!.Value);
        }

        // Spec §5.5: trailing bare `\` in a str field is a parse error.
        // The C# literal `"foo\\"` is the runtime string foo + one backslash.
        [Test]
        public void PointId_ParseRepr_RejectsTrailingBackslash()
        {
            var bad = "PointId:1.0.0#x:0:label:foo\\";
            var parsed = PointIdCodec.ParseRepr(bad);
            var left = parsed as Either<string, PointId>.Left;
            Assert.That(left, Is.Not.Null, "Expected Left for trailing backslash; got Right");
            StringAssert.Contains("trailing backslash", left!.Value);
        }

        // Spec §6.10: 4-level deep nested-id round-trip — A → B → C → D.
        // Each nesting level adds one `{...}` wrapper.
        [Test]
        public void DeepNested_Roundtrip_FourLevels()
        {
            var src = new A(B: new B(C: new C(D: new D(X: 42))));
            var s = src.ToString();
            Assert.That(s, Is.EqualTo("A:1.0.0#b:{B:1.0.0#c:{C:1.0.0#d:{D:1.0.0#x:42}}}"));

            var parsed = ACodec.ParseRepr(s);
            var right = parsed as Either<string, A>.Right;
            Assert.That(right, Is.Not.Null);
            Assert.That(right!.Value, Is.EqualTo(src));
        }

        // Spec §5.4 (D06): unsigned values MUST NOT have a leading `+`.
        // Cross-language invariant: both C# and Java reject this.
        [Test]
        public void UInts_ParseRepr_RejectsLeadingPlus()
        {
            var bad = "UInts:1.0.0#a:+1:b:2:c:3:d:4";
            var parsed = UIntsCodec.ParseRepr(bad);
            var left = parsed as Either<string, UInts>.Left;
            Assert.That(left, Is.Not.Null, "Expected Left for leading `+`; got Right");
            StringAssert.Contains("could not parse", left!.Value);
        }
    }
}
