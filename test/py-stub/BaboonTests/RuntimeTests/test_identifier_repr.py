# Hand-written runtime tests for identifier __repr__ + parse_repr (PR-57d).
# Mirrors per-language test suites from PR-56 (Scala) / PR-57a-c (C#/Java/
# Kotlin/Rust/Swift). Lives under RuntimeTests/ so codegen does not stomp it
# (codegen writes into BaboonTests/GeneratedTests/).
#
# All carryover lessons exercised: i64 round-trip via emitted code
# (PR-57a-D01), invalid escape rejection (PR-57a-D02), 4-deep nested
# chain A→B→C→D (PR-57a-D03), unsigned leading-+ rejection (PR-57a-D06),
# i64 always-true range check elision (PR-57a-D01), tsu/tso round-trip
# equality (PR-57b-D02).

import datetime
import unittest
import uuid

from BaboonDefinitions.Generated.baboon_identifier_repr import (
    bytes_to_hex,
    escape_str,
    parse_bytes_hex,
    u64_to_string,
)
from BaboonDefinitions.Generated.baboon_service_wiring import BaboonLeft, BaboonRight
from BaboonDefinitions.Generated.identifier.ok.A import A, ACodec
from BaboonDefinitions.Generated.identifier.ok.B import B
from BaboonDefinitions.Generated.identifier.ok.C import C
from BaboonDefinitions.Generated.identifier.ok.D import D
from BaboonDefinitions.Generated.identifier.ok.LongId import LongId, LongIdCodec
from BaboonDefinitions.Generated.identifier.ok.Marker import Marker, MarkerCodec
from BaboonDefinitions.Generated.identifier.ok.Mixed import Mixed, MixedCodec
from BaboonDefinitions.Generated.identifier.ok.Outer import Outer, OuterCodec
from BaboonDefinitions.Generated.identifier.ok.PointId import PointId, PointIdCodec
from BaboonDefinitions.Generated.identifier.ok.UInts import UInts, UIntsCodec


class IdentifierReprRuntimeTest(unittest.TestCase):
    # Spec §6.2: each of the 5 metacharacters escaped.
    def test_escape_all_metacharacters(self):
        self.assertEqual(escape_str("\\#:{}"), "\\\\\\#\\:\\{\\}")

    def test_escape_trailing_backslash(self):
        self.assertEqual(escape_str("foo\\"), "foo\\\\")

    def test_escape_all_backslashes(self):
        self.assertEqual(escape_str("\\\\\\\\"), "\\\\\\\\\\\\\\\\")

    def test_bytes_to_hex_empty(self):
        self.assertEqual(bytes_to_hex(b""), "")

    def test_bytes_to_hex_high_bytes(self):
        self.assertEqual(bytes_to_hex(b"\xff\xfe\x00"), "fffe00")

    # Spec §6.8: u64.MAX_VALUE renders as unsigned decimal.
    def test_u64_to_string_max(self):
        self.assertEqual(u64_to_string(18446744073709551615), "18446744073709551615")

    def test_parse_bytes_hex_empty(self):
        r = parse_bytes_hex("")
        self.assertIsInstance(r, BaboonRight)

    def test_parse_bytes_hex_uppercase_rejected(self):
        r = parse_bytes_hex("AABB")
        self.assertIsInstance(r, BaboonLeft)

    def test_parse_bytes_hex_odd_length_rejected(self):
        r = parse_bytes_hex("aab")
        self.assertIsInstance(r, BaboonLeft)


class PointIdReprTest(unittest.TestCase):
    def test_flat_multifield_round_trip(self):
        # Spec §6.9
        src = PointId(x=1, label="hello")
        s = repr(src)
        self.assertEqual(s, "PointId:1.0.0#x:1:label:hello")
        r = PointIdCodec.parse_repr(s)
        self.assertIsInstance(r, BaboonRight)
        self.assertEqual(r.value.x, 1)
        self.assertEqual(r.value.label, "hello")

    def test_str_all_metacharacters_round_trip(self):
        # Spec §6.2
        src = PointId(x=0, label="\\#:{}")
        s = repr(src)
        self.assertEqual(s, "PointId:1.0.0#x:0:label:\\\\\\#\\:\\{\\}")
        r = PointIdCodec.parse_repr(s)
        self.assertIsInstance(r, BaboonRight)
        self.assertEqual(r.value.label, "\\#:{}")

    def test_empty_str_field_round_trip(self):
        # Spec §6.1
        src = PointId(x=42, label="")
        self.assertEqual(repr(src), "PointId:1.0.0#x:42:label:")
        r = PointIdCodec.parse_repr(repr(src))
        self.assertIsInstance(r, BaboonRight)
        self.assertEqual(r.value.label, "")

    def test_rejects_out_of_range_i32(self):
        r = PointIdCodec.parse_repr("PointId:1.0.0#x:2147483648:label:foo")
        self.assertIsInstance(r, BaboonLeft)
        self.assertIn("i32 out of range", r.value)

    # PR-57a-D02 carryover.
    def test_rejects_invalid_escape(self):
        r = PointIdCodec.parse_repr("PointId:1.0.0#x:0:label:foo\\zbar")
        self.assertIsInstance(r, BaboonLeft)
        self.assertIn("invalid escape", r.value)

    def test_rejects_trailing_backslash(self):
        r = PointIdCodec.parse_repr("PointId:1.0.0#x:0:label:foo\\")
        self.assertIsInstance(r, BaboonLeft)
        self.assertIn("trailing backslash", r.value)

    def test_rejects_unknown_name(self):
        r = PointIdCodec.parse_repr("Wrong:1.0.0#x:1:label:hello")
        self.assertIsInstance(r, BaboonLeft)

    def test_rejects_unknown_version(self):
        r = PointIdCodec.parse_repr("PointId:9.9.9#x:1:label:hello")
        self.assertIsInstance(r, BaboonLeft)

    def test_rejects_trailing_input(self):
        r = PointIdCodec.parse_repr("PointId:1.0.0#x:1:label:hello:extra")
        self.assertIsInstance(r, BaboonLeft)


class LongIdReprTest(unittest.TestCase):
    # PR-57a-D01 carryover: real i64 round-trip via the EMITTED code path.
    def test_i64_min_round_trip(self):
        src = LongId(x=-9223372036854775808)
        s = repr(src)
        self.assertEqual(s, "LongId:1.0.0#x:-9223372036854775808")
        r = LongIdCodec.parse_repr(s)
        self.assertIsInstance(r, BaboonRight)
        self.assertEqual(r.value.x, -9223372036854775808)

    def test_i64_max_round_trip(self):
        src = LongId(x=9223372036854775807)
        s = repr(src)
        self.assertEqual(s, "LongId:1.0.0#x:9223372036854775807")
        r = LongIdCodec.parse_repr(s)
        self.assertIsInstance(r, BaboonRight)
        self.assertEqual(r.value.x, 9223372036854775807)


class UIntsReprTest(unittest.TestCase):
    def test_u64_max_round_trip(self):
        # Spec §6.8
        src = UInts(a=0, b=0, c=0, d=18446744073709551615)
        s = repr(src)
        self.assertIn("d:18446744073709551615", s)
        r = UIntsCodec.parse_repr(s)
        self.assertIsInstance(r, BaboonRight)
        self.assertEqual(r.value.d, 18446744073709551615)

    # PR-57a-D06 carryover.
    def test_rejects_leading_plus_on_unsigned(self):
        r = UIntsCodec.parse_repr("UInts:1.0.0#a:+1:b:2:c:3:d:4")
        self.assertIsInstance(r, BaboonLeft)
        self.assertIn("leading sign", r.value)

    # Spec §5.4 (PR-C): signed integer wire forms MUST NOT have a leading `+`.
    def test_rejects_leading_plus_on_signed_i32(self):
        r = PointIdCodec.parse_repr("PointId:1.0.0#x:+42:label:hello")
        self.assertIsInstance(r, BaboonLeft)
        self.assertIn("leading '+'", r.value)

    # Spec §5.4 (PR-C): i64 signed field must also reject leading `+`.
    def test_rejects_leading_plus_on_signed_i64(self):
        r = LongIdCodec.parse_repr("LongId:1.0.0#x:+1")
        self.assertIsInstance(r, BaboonLeft)
        self.assertIn("leading '+'", r.value)

    def test_rejects_out_of_range_u08(self):
        r = UIntsCodec.parse_repr("UInts:1.0.0#a:256:b:0:c:0:d:0")
        self.assertIsInstance(r, BaboonLeft)
        self.assertIn("u08 out of range", r.value)


class MixedReprTest(unittest.TestCase):
    def test_mixed_round_trip_empty_bytes_and_times(self):
        # PR-57b-D02 carryover.
        created = datetime.datetime(
            2026, 4, 29, 12, 34, 56, 789000, tzinfo=datetime.timezone.utc
        )
        scheduled = datetime.datetime(
            2026,
            4,
            29,
            12,
            34,
            56,
            789000,
            tzinfo=datetime.timezone(datetime.timedelta(hours=2)),
        )
        src = Mixed(
            active=True,
            id=uuid.UUID("de7b9e1e-5c93-45fe-beec-da99994f629a"),
            payload=b"",
            created=created,
            scheduled=scheduled,
        )
        s = repr(src)
        self.assertIn("Mixed:1.0.0#active:true:id:de7b9e1e-5c93-45fe-beec-da99994f629a:", s)
        self.assertIn(
            ":payload::created:2026-04-29T12:34:56.789Z:scheduled:2026-04-29T12:34:56.789+02:00",
            s,
        )

        r = MixedCodec.parse_repr(s)
        self.assertIsInstance(r, BaboonRight)
        self.assertEqual(r.value.active, True)
        self.assertEqual(str(r.value.id), "de7b9e1e-5c93-45fe-beec-da99994f629a")
        self.assertEqual(len(r.value.payload), 0)
        self.assertEqual(r.value.created, created)
        # tso round-trip equality (PR-57b-D02): same instant, same offset.
        self.assertEqual(r.value.scheduled, scheduled)
        self.assertEqual(r.value.scheduled.utcoffset(), scheduled.utcoffset())

    def test_rejects_mixed_case_uid(self):
        bad = (
            "Mixed:1.0.0#active:true:id:DE7B9E1E-5C93-45FE-BEEC-DA99994F629A"
            ":payload::created:2026-04-29T12:34:56.789Z"
            ":scheduled:2026-04-29T12:34:56.789+02:00"
        )
        r = MixedCodec.parse_repr(bad)
        self.assertIsInstance(r, BaboonLeft)
        self.assertIn("uid not in canonical lowercase form", r.value)

    def test_mixed_with_non_empty_bytes(self):
        created = datetime.datetime(2020, 1, 1, tzinfo=datetime.timezone.utc)
        scheduled = datetime.datetime(2020, 1, 1, tzinfo=datetime.timezone.utc)
        src = Mixed(
            active=False,
            id=uuid.UUID("00000000-0000-0000-0000-000000000000"),
            payload=b"\x01\x02",
            created=created,
            scheduled=scheduled,
        )
        s = repr(src)
        self.assertIn(":payload:0102:", s)
        r = MixedCodec.parse_repr(s)
        self.assertIsInstance(r, BaboonRight)
        self.assertEqual(r.value.payload, b"\x01\x02")


class MarkerReprTest(unittest.TestCase):
    # Spec §6.12.
    def test_empty_fields_id(self):
        src = Marker()
        self.assertEqual(repr(src), "Marker:1.0.0#")
        r = MarkerCodec.parse_repr("Marker:1.0.0#")
        self.assertIsInstance(r, BaboonRight)


class OuterReprTest(unittest.TestCase):
    def test_nested_id_round_trip(self):
        src = Outer(ref=PointId(x=7, label="k"), tag="t")
        s = repr(src)
        self.assertEqual(s, "Outer:1.0.0#ref:{PointId:1.0.0#x:7:label:k}:tag:t")
        r = OuterCodec.parse_repr(s)
        self.assertIsInstance(r, BaboonRight)
        self.assertEqual(r.value.ref.x, 7)
        self.assertEqual(r.value.ref.label, "k")
        self.assertEqual(r.value.tag, "t")


class DeepNestedReprTest(unittest.TestCase):
    # PR-57a-D03 carryover, spec §6.10.
    def test_four_level_deep_nested(self):
        src = A(b=B(c=C(d=D(x=42))))
        s = repr(src)
        self.assertEqual(s, "A:1.0.0#b:{B:1.0.0#c:{C:1.0.0#d:{D:1.0.0#x:42}}}")
        r = ACodec.parse_repr(s)
        self.assertIsInstance(r, BaboonRight)
        self.assertEqual(r.value.b.c.d.x, 42)


if __name__ == "__main__":
    unittest.main()
