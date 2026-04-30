// Hand-written runtime tests for identifier toString + parseRepr (PR-57d).
// Mirrors per-language test suites from PR-56 (Scala) / PR-57a-c (C#/Java/
// Kotlin/Rust/Swift). Lives under test/runtime/ so codegen does not stomp it.
//
// All carryover lessons exercised: i64 round-trip via emitted code
// (PR-57a-D01), invalid escape rejection (PR-57a-D02), 4-deep nested
// chain A→B→C→D (PR-57a-D03), unsigned leading-+ rejection (PR-57a-D06),
// i64 always-true range check elision (PR-57a-D01), tsu/tso round-trip
// equality (PR-57b-D02).

import 'dart:typed_data';

import 'package:baboon_runtime/baboon_identifier_repr.dart';
import 'package:baboon_runtime/baboon_runtime.dart';
import 'package:dt_stub/identifier/ok/a.dart';
import 'package:dt_stub/identifier/ok/b.dart';
import 'package:dt_stub/identifier/ok/c.dart';
import 'package:dt_stub/identifier/ok/d.dart';
import 'package:dt_stub/identifier/ok/long_id.dart';
import 'package:dt_stub/identifier/ok/marker.dart';
import 'package:dt_stub/identifier/ok/mixed.dart';
import 'package:dt_stub/identifier/ok/outer.dart';
import 'package:dt_stub/identifier/ok/point_id.dart';
import 'package:dt_stub/identifier/ok/u_ints.dart';
import 'package:test/test.dart';

void main() {
  group('IdentifierRepr — runtime helpers', () {
    // Spec §6.2: each of the 5 metacharacters escaped.
    test('escapeStr — all 5 metacharacters', () {
      expect(BaboonIdRepr.escapeStr(r'\#:{}'), r'\\\#\:\{\}');
    });

    test('escapeStr — trailing backslash', () {
      expect(BaboonIdRepr.escapeStr(r'foo\'), r'foo\\');
    });

    test('escapeStr — all backslashes', () {
      expect(BaboonIdRepr.escapeStr(r'\\\\'), r'\\\\\\\\');
    });

    test('bytesToHex — empty', () {
      expect(BaboonIdRepr.bytesToHex(Uint8List(0)), '');
    });

    test('bytesToHex — high bytes', () {
      expect(BaboonIdRepr.bytesToHex(Uint8List.fromList([0xff, 0xfe, 0x00])),
          'fffe00');
    });

    test('parseBytesHex — empty is OK', () {
      final r = BaboonIdRepr.parseBytesHex('');
      expect(r is BaboonRight, true);
    });

    test('parseBytesHex — uppercase rejected', () {
      final r = BaboonIdRepr.parseBytesHex('AABB');
      expect(r is BaboonLeft, true);
    });

    test('parseBytesHex — odd length rejected', () {
      final r = BaboonIdRepr.parseBytesHex('aab');
      expect(r is BaboonLeft, true);
    });
  });

  group('IdentifierRepr — PointId (i32 + str)', () {
    test('flat multi-field round-trip (spec §6.9)', () {
      final src = PointId(x: 1, label: 'hello');
      final s = src.toString();
      expect(s, 'PointId:1.0.0#x:1:label:hello');
      final r = PointIdCodec.parseRepr(s);
      expect(r is BaboonRight<String, PointId>, true);
      if (r is BaboonRight<String, PointId>) {
        expect(r.value.x, 1);
        expect(r.value.label, 'hello');
      }
    });

    test('str all metacharacters round-trip', () {
      final src = PointId(x: 0, label: r'\#:{}');
      final s = src.toString();
      expect(s, r'PointId:1.0.0#x:0:label:\\\#\:\{\}');
      final r = PointIdCodec.parseRepr(s);
      expect(r is BaboonRight<String, PointId>, true);
      if (r is BaboonRight<String, PointId>) {
        expect(r.value.label, r'\#:{}');
      }
    });

    test('empty str field round-trip', () {
      final src = PointId(x: 42, label: '');
      expect(src.toString(), 'PointId:1.0.0#x:42:label:');
      final r = PointIdCodec.parseRepr(src.toString());
      expect(r is BaboonRight<String, PointId>, true);
      if (r is BaboonRight<String, PointId>) expect(r.value.label, '');
    });

    test('rejects out-of-range i32', () {
      final r = PointIdCodec.parseRepr('PointId:1.0.0#x:2147483648:label:foo');
      expect(r is BaboonLeft, true);
      if (r is BaboonLeft<String, PointId>) {
        expect(r.value.contains('i32 out of range'), true);
      }
    });

    test('rejects invalid escape (\\z)', () {
      final r = PointIdCodec.parseRepr(r'PointId:1.0.0#x:0:label:foo\zbar');
      expect(r is BaboonLeft, true);
      if (r is BaboonLeft<String, PointId>) {
        expect(r.value.contains('invalid escape'), true);
      }
    });

    test('rejects trailing backslash', () {
      final r = PointIdCodec.parseRepr(r'PointId:1.0.0#x:0:label:foo\');
      expect(r is BaboonLeft, true);
      if (r is BaboonLeft<String, PointId>) {
        expect(r.value.contains('trailing backslash'), true);
      }
    });

    test('rejects unknown name', () {
      final r = PointIdCodec.parseRepr('Wrong:1.0.0#x:1:label:hello');
      expect(r is BaboonLeft, true);
    });

    test('rejects unknown version', () {
      final r = PointIdCodec.parseRepr('PointId:9.9.9#x:1:label:hello');
      expect(r is BaboonLeft, true);
    });

    test('rejects trailing input', () {
      final r = PointIdCodec.parseRepr('PointId:1.0.0#x:1:label:hello:extra');
      expect(r is BaboonLeft, true);
    });
  });

  group('IdentifierRepr — LongId (i64)', () {
    // PR-57a-D01 carryover: real i64 round-trip via the EMITTED code path.
    // Dart `int` is signed 64-bit on native; min/max constants below are
    // the i64 boundary values.
    test('i64 MIN round-trip', () {
      final src = LongId(x: -9223372036854775808);
      final s = src.toString();
      expect(s, 'LongId:1.0.0#x:-9223372036854775808');
      final r = LongIdCodec.parseRepr(s);
      expect(r is BaboonRight<String, LongId>, true);
      if (r is BaboonRight<String, LongId>) {
        expect(r.value.x, -9223372036854775808);
      }
    });

    test('i64 MAX round-trip', () {
      final src = LongId(x: 9223372036854775807);
      final s = src.toString();
      expect(s, 'LongId:1.0.0#x:9223372036854775807');
      final r = LongIdCodec.parseRepr(s);
      expect(r is BaboonRight<String, LongId>, true);
      if (r is BaboonRight<String, LongId>) {
        expect(r.value.x, 9223372036854775807);
      }
    });
  });

  group('IdentifierRepr — UInts (u08/u16/u32/u64)', () {
    test('u64 MAX round-trip (spec §6.8)', () {
      // Dart int is signed-64; u64::MAX is rendered via the BigInt-wrapped
      // u64ToString helper. We pass the signed-bit-pattern equivalent.
      final src = UInts(a: 0, b: 0, c: 0, d: -1);
      final s = src.toString();
      expect(s.contains('d:18446744073709551615'), true);
      final r = UIntsCodec.parseRepr(s);
      expect(r is BaboonRight<String, UInts>, true);
      if (r is BaboonRight<String, UInts>) {
        expect(r.value.d, -1);
      }
    });

    // PR-57a-D06 carryover: leading + on unsigned is rejected.
    test('rejects leading + on unsigned', () {
      final r = UIntsCodec.parseRepr('UInts:1.0.0#a:+1:b:2:c:3:d:4');
      expect(r is BaboonLeft, true);
      if (r is BaboonLeft<String, UInts>) {
        expect(r.value.contains('leading sign'), true);
      }
    });

    test('rejects out-of-range u08', () {
      final r = UIntsCodec.parseRepr('UInts:1.0.0#a:256:b:0:c:0:d:0');
      expect(r is BaboonLeft, true);
      if (r is BaboonLeft<String, UInts>) {
        expect(r.value.contains('u08 out of range'), true);
      }
    });
  });

  group('IdentifierRepr — Mixed (uid + bytes + tsu + tso + bit)', () {
    test('Mixed round-trip with empty bytes and times', () {
      // PR-57b-D02 carryover: tsu/tso round-trip equality.
      final created = DateTime.utc(2026, 4, 29, 12, 34, 56, 789);
      final scheduled = BaboonDateTimeOffset(
        epochMillis: DateTime.utc(2026, 4, 29, 10, 34, 56, 789).millisecondsSinceEpoch,
        offsetMillis: 2 * 3600000,
        kind: 'offset',
      );
      final src = Mixed(
        active: true,
        id: 'de7b9e1e-5c93-45fe-beec-da99994f629a',
        payload: Uint8List(0),
        created: created,
        scheduled: scheduled,
      );
      final s = src.toString();
      expect(
          s.contains(
              'Mixed:1.0.0#active:true:id:de7b9e1e-5c93-45fe-beec-da99994f629a:'),
          true);
      expect(
          s.contains(
              ':payload::created:2026-04-29T12:34:56.789Z:scheduled:2026-04-29T12:34:56.789+02:00'),
          true);

      final r = MixedCodec.parseRepr(s);
      expect(r is BaboonRight<String, Mixed>, true);
      if (r is BaboonRight<String, Mixed>) {
        expect(r.value.active, true);
        expect(r.value.id, 'de7b9e1e-5c93-45fe-beec-da99994f629a');
        expect(r.value.payload.length, 0);
        expect(r.value.created, created);
        expect(r.value.scheduled.epochMillis, scheduled.epochMillis);
        expect(r.value.scheduled.offsetMillis, scheduled.offsetMillis);
      }
    });

    test('rejects mixed-case uid', () {
      const bad =
          'Mixed:1.0.0#active:true:id:DE7B9E1E-5C93-45FE-BEEC-DA99994F629A:payload::created:2026-04-29T12:34:56.789Z:scheduled:2026-04-29T12:34:56.789+02:00';
      final r = MixedCodec.parseRepr(bad);
      expect(r is BaboonLeft, true);
      if (r is BaboonLeft<String, Mixed>) {
        expect(r.value.contains('uid not in canonical lowercase form'), true);
      }
    });

    test('Mixed with non-empty bytes', () {
      final created = DateTime.utc(2020, 1, 1, 0, 0, 0, 0);
      final scheduled = BaboonDateTimeOffset(
          epochMillis: DateTime.utc(2020, 1, 1, 0, 0, 0, 0).millisecondsSinceEpoch,
          offsetMillis: 0,
          kind: 'utc');
      final src = Mixed(
        active: false,
        id: '00000000-0000-0000-0000-000000000000',
        payload: Uint8List.fromList([0x01, 0x02]),
        created: created,
        scheduled: scheduled,
      );
      final s = src.toString();
      expect(s.contains(':payload:0102:'), true);
      final r = MixedCodec.parseRepr(s);
      expect(r is BaboonRight<String, Mixed>, true);
      if (r is BaboonRight<String, Mixed>) {
        expect(r.value.payload, [0x01, 0x02]);
      }
    });
  });

  group('IdentifierRepr — Marker (empty fields)', () {
    test('empty-fields id renders as <Name>:<version># (spec §6.12)', () {
      final src = Marker();
      expect(src.toString(), 'Marker:1.0.0#');
      final r = MarkerCodec.parseRepr('Marker:1.0.0#');
      expect(r is BaboonRight<String, Marker>, true);
    });
  });

  group('IdentifierRepr — Outer (nested id)', () {
    test('nested id round-trip', () {
      final inner = PointId(x: 7, label: 'k');
      final src = Outer(ref: inner, tag: 't');
      final s = src.toString();
      expect(s, 'Outer:1.0.0#ref:{PointId:1.0.0#x:7:label:k}:tag:t');
      final r = OuterCodec.parseRepr(s);
      expect(r is BaboonRight<String, Outer>, true);
      if (r is BaboonRight<String, Outer>) {
        expect(r.value.ref.x, 7);
        expect(r.value.ref.label, 'k');
        expect(r.value.tag, 't');
      }
    });
  });

  group('IdentifierRepr — A→B→C→D (4-deep nested, spec §6.10)', () {
    // PR-57a-D03 carryover.
    test('4-level deep nested-id round-trip', () {
      final src = A(b: B(c: C(d: D(x: 42))));
      final s = src.toString();
      expect(s, 'A:1.0.0#b:{B:1.0.0#c:{C:1.0.0#d:{D:1.0.0#x:42}}}');
      final r = ACodec.parseRepr(s);
      expect(r is BaboonRight<String, A>, true);
      if (r is BaboonRight<String, A>) {
        expect(r.value.b.c.d.x, 42);
      }
    });
  });
}
