// Hand-written runtime tests for identifier toString + parseRepr (PR-57d).
// Mirrors per-language test suites from PR-56 (Scala) / PR-57a-c (C#/Java/
// Kotlin/Rust/Swift). Lives under src/runtime-tests/ so codegen does not
// stomp it (codegen writes into src/generated/ and src/baboontests/).
//
// All carryover lessons exercised: i64 round-trip via emitted code
// (PR-57a-D01), invalid escape rejection (PR-57a-D02), 4-deep nested
// chain A→B→C→D (PR-57a-D03), unsigned leading-+ rejection (PR-57a-D06),
// i64 always-true range check elision (PR-57a-D01), tsu/tso round-trip
// equality (PR-57b-D02).

import { describe, expect, test } from "vitest";

import {
    BaboonDateTimeOffset,
    BaboonDateTimeUtc,
} from "../generated/BaboonSharedRuntime";

import {
    bytesToHex,
    escapeStr,
    parseBytesHex,
    u64ToString,
} from "../generated/baboon-identifier-repr";

import { PointId, pointIdCodec } from "../generated/identifier/ok/PointId";
import { LongId, longIdCodec } from "../generated/identifier/ok/LongId";
import { UInts, uIntsCodec } from "../generated/identifier/ok/UInts";
import { Mixed, mixedCodec } from "../generated/identifier/ok/Mixed";
import { Marker, markerCodec } from "../generated/identifier/ok/Marker";
import { Outer, outerCodec } from "../generated/identifier/ok/Outer";
import { A, aCodec } from "../generated/identifier/ok/A";
import { B } from "../generated/identifier/ok/B";
import { C } from "../generated/identifier/ok/C";
import { D } from "../generated/identifier/ok/D";

describe("IdentifierRepr — runtime helpers", () => {
    // Spec §6.2: each of the 5 metacharacters escaped.
    test("escapeStr — all 5 metacharacters", () => {
        expect(escapeStr("\\#:{}")).toBe("\\\\\\#\\:\\{\\}");
    });

    // Spec §6.3: trailing single backslash escapes to \\.
    test("escapeStr — trailing backslash", () => {
        expect(escapeStr("foo\\")).toBe("foo\\\\");
    });

    // Spec §6.4: 4 backslashes round-trip.
    test("escapeStr — all backslashes", () => {
        expect(escapeStr("\\\\\\\\")).toBe("\\\\\\\\\\\\\\\\");
    });

    test("bytesToHex — empty", () => {
        expect(bytesToHex(new Uint8Array(0))).toBe("");
    });

    test("bytesToHex — high bytes", () => {
        expect(bytesToHex(new Uint8Array([0xff, 0xfe, 0x00]))).toBe("fffe00");
    });

    // Spec §6.8: u64.MAX_VALUE renders as unsigned decimal.
    test("u64ToString — max value", () => {
        expect(u64ToString(18446744073709551615n)).toBe("18446744073709551615");
    });

    test("parseBytesHex — empty is OK", () => {
        const r = parseBytesHex("");
        expect(r.tag).toBe("Right");
        if (r.tag === "Right") expect(r.value.length).toBe(0);
    });

    test("parseBytesHex — uppercase rejected", () => {
        const r = parseBytesHex("AABB");
        expect(r.tag).toBe("Left");
    });

    test("parseBytesHex — odd length rejected", () => {
        const r = parseBytesHex("aab");
        expect(r.tag).toBe("Left");
    });
});

describe("IdentifierRepr — PointId (i32 + str)", () => {
    test("flat multi-field round-trip (spec §6.9)", () => {
        const src = new PointId(1, "hello");
        const s = src.toString();
        expect(s).toBe("PointId:1.0.0#x:1:label:hello");
        const r = pointIdCodec.parseRepr(s);
        expect(r.tag).toBe("Right");
        if (r.tag === "Right") {
            expect(r.value.x).toBe(1);
            expect(r.value.label).toBe("hello");
        }
    });

    // Spec §6.2: str with all 5 metacharacters round-trips.
    test("str all metacharacters round-trip", () => {
        const src = new PointId(0, "\\#:{}");
        const s = src.toString();
        expect(s).toBe("PointId:1.0.0#x:0:label:\\\\\\#\\:\\{\\}");
        const r = pointIdCodec.parseRepr(s);
        expect(r.tag).toBe("Right");
        if (r.tag === "Right") expect(r.value.label).toBe("\\#:{}");
    });

    // Spec §6.1: empty str field round-trip.
    test("empty str field round-trip", () => {
        const src = new PointId(42, "");
        const s = src.toString();
        expect(s).toBe("PointId:1.0.0#x:42:label:");
        const r = pointIdCodec.parseRepr(s);
        expect(r.tag).toBe("Right");
        if (r.tag === "Right") expect(r.value.label).toBe("");
    });

    test("rejects out-of-range i32", () => {
        const r = pointIdCodec.parseRepr("PointId:1.0.0#x:2147483648:label:foo");
        expect(r.tag).toBe("Left");
        if (r.tag === "Left") expect(r.value).toContain("i32 out of range");
    });

    // PR-57a-D02 carryover: bare `\X` for X not in metachar set is parse error.
    test("rejects invalid escape (\\z)", () => {
        const r = pointIdCodec.parseRepr("PointId:1.0.0#x:0:label:foo\\zbar");
        expect(r.tag).toBe("Left");
        if (r.tag === "Left") expect(r.value).toContain("invalid escape");
    });

    // PR-57a-D02 carryover: trailing `\` is parse error.
    test("rejects trailing backslash", () => {
        const r = pointIdCodec.parseRepr("PointId:1.0.0#x:0:label:foo\\");
        expect(r.tag).toBe("Left");
        if (r.tag === "Left") expect(r.value).toContain("trailing backslash");
    });

    test("rejects unknown name", () => {
        const r = pointIdCodec.parseRepr("Wrong:1.0.0#x:1:label:hello");
        expect(r.tag).toBe("Left");
    });

    test("rejects unknown version", () => {
        const r = pointIdCodec.parseRepr("PointId:9.9.9#x:1:label:hello");
        expect(r.tag).toBe("Left");
    });

    test("rejects trailing input", () => {
        const r = pointIdCodec.parseRepr("PointId:1.0.0#x:1:label:hello:extra");
        expect(r.tag).toBe("Left");
    });

    // Spec §5.4 (PR-C): signed integer wire forms MUST NOT have a leading `+`.
    // TS regex `/^-?[0-9]+$/` already rejects `+` — this test locks it in.
    test("rejects leading + on signed i32 (Spec §5.4)", () => {
        const r = pointIdCodec.parseRepr("PointId:1.0.0#x:+42:label:hello");
        expect(r.tag).toBe("Left");
        if (r.tag === "Left") expect(r.value).toContain("could not parse signed integer");
    });
});

describe("IdentifierRepr — LongId (i64)", () => {
    // PR-57a-D01 carryover: real i64 round-trip via the EMITTED code path
    // (not the runtime stdlib BigInt parser).
    test("i64 MIN round-trip", () => {
        const src = new LongId(-9223372036854775808n);
        const s = src.toString();
        expect(s).toBe("LongId:1.0.0#x:-9223372036854775808");
        const r = longIdCodec.parseRepr(s);
        expect(r.tag).toBe("Right");
        if (r.tag === "Right") expect(r.value.x).toBe(-9223372036854775808n);
    });

    test("i64 MAX round-trip", () => {
        const src = new LongId(9223372036854775807n);
        const s = src.toString();
        expect(s).toBe("LongId:1.0.0#x:9223372036854775807");
        const r = longIdCodec.parseRepr(s);
        expect(r.tag).toBe("Right");
        if (r.tag === "Right") expect(r.value.x).toBe(9223372036854775807n);
    });

    // Spec §5.4 (PR-C): i64 signed field must also reject leading `+`.
    test("rejects leading + on signed i64 (Spec §5.4)", () => {
        const r = longIdCodec.parseRepr("LongId:1.0.0#x:+1");
        expect(r.tag).toBe("Left");
        if (r.tag === "Left") expect(r.value).toContain("could not parse i64");
    });
});

describe("IdentifierRepr — UInts (u08/u16/u32/u64)", () => {
    test("u64 MAX round-trip (spec §6.8)", () => {
        const src = new UInts(0, 0, 0, 18446744073709551615n);
        const s = src.toString();
        expect(s).toContain("d:18446744073709551615");
        const r = uIntsCodec.parseRepr(s);
        expect(r.tag).toBe("Right");
        if (r.tag === "Right") expect(r.value.d).toBe(18446744073709551615n);
    });

    // PR-57a-D06 carryover: leading + on unsigned is rejected.
    test("rejects leading + on unsigned", () => {
        const r = uIntsCodec.parseRepr("UInts:1.0.0#a:+1:b:2:c:3:d:4");
        expect(r.tag).toBe("Left");
        if (r.tag === "Left") expect(r.value).toContain("leading sign");
    });

    test("rejects out-of-range u08", () => {
        const r = uIntsCodec.parseRepr("UInts:1.0.0#a:256:b:0:c:0:d:0");
        expect(r.tag).toBe("Left");
        if (r.tag === "Left") expect(r.value).toContain("u08 out of range");
    });
});

describe("IdentifierRepr — Mixed (uid + bytes + tsu + tso + bit)", () => {
    test("Mixed round-trip with empty bytes and UTC times", () => {
        // PR-57b-D02 carryover: tsu/tso round-trip equality.
        const created = BaboonDateTimeUtc.fromMillis(Date.UTC(2026, 3, 29, 12, 34, 56, 789));
        const scheduled = BaboonDateTimeOffset.fromDateAndOffset(
            new Date(Date.UTC(2026, 3, 29, 10, 34, 56, 789)),
            2 * 3600 * 1000,
        );
        const src = new Mixed(true, "de7b9e1e-5c93-45fe-beec-da99994f629a", new Uint8Array(0), created, scheduled);
        const s = src.toString();
        expect(s).toContain("Mixed:1.0.0#active:true:id:de7b9e1e-5c93-45fe-beec-da99994f629a:");
        expect(s).toContain(":payload::created:2026-04-29T12:34:56.789Z:scheduled:2026-04-29T12:34:56.789+02:00");

        const r = mixedCodec.parseRepr(s);
        expect(r.tag).toBe("Right");
        if (r.tag === "Right") {
            expect(r.value.active).toBe(true);
            expect(r.value.id).toBe("de7b9e1e-5c93-45fe-beec-da99994f629a");
            expect(r.value.payload.length).toBe(0);
            expect(r.value.created.getTime()).toBe(created.getTime());
            expect(r.value.scheduled.getTime()).toBe(scheduled.getTime());
            expect(r.value.scheduled.offsetMs).toBe(scheduled.offsetMs);
        }
    });

    test("rejects mixed-case uid", () => {
        const bad = "Mixed:1.0.0#active:true:id:DE7B9E1E-5C93-45FE-BEEC-DA99994F629A:payload::created:2026-04-29T12:34:56.789Z:scheduled:2026-04-29T12:34:56.789+02:00";
        const r = mixedCodec.parseRepr(bad);
        expect(r.tag).toBe("Left");
        if (r.tag === "Left") expect(r.value).toContain("uid not in canonical lowercase form");
    });

    test("Mixed with non-empty bytes", () => {
        const created = BaboonDateTimeUtc.fromMillis(Date.UTC(2020, 0, 1, 0, 0, 0, 0));
        const scheduled = BaboonDateTimeOffset.fromDateAndOffset(new Date(Date.UTC(2020, 0, 1, 0, 0, 0, 0)), 0);
        const src = new Mixed(false, "00000000-0000-0000-0000-000000000000", new Uint8Array([0x01, 0x02]), created, scheduled);
        const s = src.toString();
        expect(s).toContain(":payload:0102:");
        const r = mixedCodec.parseRepr(s);
        expect(r.tag).toBe("Right");
        if (r.tag === "Right") {
            expect(Array.from(r.value.payload)).toEqual([0x01, 0x02]);
        }
    });
});

describe("IdentifierRepr — Marker (empty fields)", () => {
    test("empty-fields id renders as <Name>:<version># (spec §6.12)", () => {
        const src = new Marker();
        expect(src.toString()).toBe("Marker:1.0.0#");
        const r = markerCodec.parseRepr("Marker:1.0.0#");
        expect(r.tag).toBe("Right");
    });
});

describe("IdentifierRepr — Outer (nested id)", () => {
    test("nested id round-trip", () => {
        const inner = new PointId(7, "k");
        const src = new Outer(inner, "t");
        const s = src.toString();
        expect(s).toBe("Outer:1.0.0#ref:{PointId:1.0.0#x:7:label:k}:tag:t");
        const r = outerCodec.parseRepr(s);
        expect(r.tag).toBe("Right");
        if (r.tag === "Right") {
            expect(r.value.ref.x).toBe(7);
            expect(r.value.ref.label).toBe("k");
            expect(r.value.tag).toBe("t");
        }
    });
});

describe("IdentifierRepr — A→B→C→D (4-deep nested, spec §6.10)", () => {
    // PR-57a-D03 carryover: 4-deep nested round-trip.
    test("4-level deep nested-id round-trip", () => {
        const src = new A(new B(new C(new D(42))));
        const s = src.toString();
        expect(s).toBe("A:1.0.0#b:{B:1.0.0#c:{C:1.0.0#d:{D:1.0.0#x:42}}}");
        const r = aCodec.parseRepr(s);
        expect(r.tag).toBe("Right");
        if (r.tag === "Right") {
            expect(r.value.b.c.d.x).toBe(42);
        }
    });
});
