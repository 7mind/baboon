// PR-G (M24.2.2) — direct-builtin map-key wire format unification.
//
// Verifies that maps with non-string builtin keys (i32/i64/f32/f64/bool/uid/tsu/tso) emit the
// same string-keyed-object form as Scala/Rust/Java/Kotlin/C#/Dart/Swift/Python — *not* the prior
// tuple-array form. Round-trips through encode/decode and asserts shape on the wire.
//
// Generated symbols come from pkg0/pkg01.baboon under target/test-regular/ts-stub/.
// - T13_2.f1: map[uid, i32]               (uid → string passthrough)
// - T9_test.f8: map[tso, lst[opt[tsu]]]   (BaboonDateTimeOffset key)

import { describe, expect, test } from "vitest";

import { BaboonCodecContext, BaboonDateTimeOffset, BaboonDateTimeUtc, BaboonDecoderFailure } from "../generated/BaboonSharedRuntime";
import { T13_2, T13_2_JsonCodec } from "../generated/testpkg/pkg0/v1_0_0/T13_2";
import { T13_1 } from "../generated/testpkg/pkg0/v1_0_0/T13_1";
import { T9_test, T9_test_JsonCodec } from "../generated/testpkg/pkg0/v1_0_0/T9_test";
import { T6_D1_JsonCodec } from "../generated/testpkg/pkg0/v1_0_0/T6_D1";

// Minimal valid T6_D2 JSON for use in T6_D1 test construction (all numeric keys are strings for i64/u64).
const validT6D2 = {
    f00: "", f01: 0, f02: 0, f03: 0, f04: "0", f05: 0, f06: 0, f07: 0, f08: "0",
    f09: "2024-01-01T00:00:00.000Z", f10: "2024-01-01T00:00:00.000Z",
    f11: 0.0, f12: 0.0, f13: "0", f14: false, f15: "00000000-0000-0000-0000-000000000000",
};

describe("PR-G (M24.2.2) — non-string builtin map keys emit string-keyed-object form", () => {
    test("map[uid, i32] encodes as {uidStr: number}, NOT [[uidStr, number], ...]", () => {
        const ctx = BaboonCodecContext.Compact;
        const value = new T13_2(
            "00000000-0000-0000-0000-0000000000ff",
            new Map<T13_1, number>([[T13_1.A1, 10]]),
            new Map<string, number>([
                ["00000000-0000-0000-0000-000000000001", 1],
                ["00000000-0000-0000-0000-000000000002", 2],
            ]),
        );
        const wire = T13_2_JsonCodec.instance.encode(ctx, value) as Record<string, unknown>;

        // f1 must be a plain object (string-keyed), NOT an array of [k, v] pairs.
        expect(Array.isArray(wire.f1)).toBe(false);
        expect(typeof wire.f1).toBe("object");
        expect(wire.f1).toEqual({
            "00000000-0000-0000-0000-000000000001": 1,
            "00000000-0000-0000-0000-000000000002": 2,
        });

        // Round-trip through decode preserves the Map.
        const decoded = T13_2_JsonCodec.instance.decode(ctx, wire);
        expect(decoded.f1.size).toBe(2);
        expect(decoded.f1.get("00000000-0000-0000-0000-000000000001")).toBe(1);
        expect(decoded.f1.get("00000000-0000-0000-0000-000000000002")).toBe(2);
    });

    test("map[tso, lst[opt[tsu]]] encodes as {ISOStr: [...]}, NOT [[ISOStr, [...]], ...]", () => {
        const ctx = BaboonCodecContext.Compact;
        const k1 = BaboonDateTimeOffset.fromISO("2024-06-15T14:30:45.000+02:00");
        const k2 = BaboonDateTimeOffset.fromISO("2025-01-01T00:00:00.000Z");
        const v1 = [BaboonDateTimeUtc.fromISO("2024-06-15T12:30:45.000Z"), undefined];
        const v2 = [BaboonDateTimeUtc.fromISO("2025-01-01T00:00:00.000Z")];
        const value = new T9_test(
            new Map(),                      // f2: map[str, lst[opt[i32]]]
            undefined,                      // f3
            undefined,                      // f4
            new Map(),                      // f5
            new Map(),                      // f6
            undefined,                      // f7
            new Map([[k1, v1], [k2, v2]]),  // f8: map[tso, lst[opt[tsu]]]
            undefined,                      // f9
            new Map(),                      // f10
        );
        const wire = T9_test_JsonCodec.instance.encode(ctx, value) as Record<string, unknown>;

        // f8 must be a string-keyed object whose keys are ISO-8601 strings.
        expect(Array.isArray(wire.f8)).toBe(false);
        expect(typeof wire.f8).toBe("object");
        const f8 = wire.f8 as Record<string, unknown>;
        const keys = Object.keys(f8).sort();
        expect(keys.length).toBe(2);
        // Each key parses as a Date (ISO-8601).
        for (const k of keys) {
            expect(Number.isNaN(new Date(k).getTime())).toBe(false);
        }

        // Round-trip preserves cardinality and values.
        const decoded = T9_test_JsonCodec.instance.decode(ctx, wire);
        expect(decoded.f8.size).toBe(2);
    });

    test("M3: map[u08, u08] decode throws BaboonDecoderFailure for malformed integer key", () => {
        // fPrecex4 uses parsePrimitiveKey with parseInt guard — "not-a-number" must throw.
        const ctx = BaboonCodecContext.Compact;
        const badJson = {
            fSameBuiltin: 0, fSameCustom: validT6D2, fSameCustomOpt: null, fSameCustomLst: [],
            fSameCustomMap: {}, fWrapOpt: 0, fWrapLst: 0, fWrapSet: 0,
            fSwapOptSet0: null, fSwapOptSet1: null, fSwapOptSet2: null,
            fSwapOptLst0: null, fSwapOptLst1: null, fSwapOptLst2: null,
            fSwapLstSet: [], fSwapSetLst: [],
            fPrecex0: 0, fPrecex1: null, fPrecex2: [], fPrecex3: [],
            fPrecex4: { "not-a-number": 1 },
            fSwapPrecex0: null, fSwapPrecex1: [], fSwapPrecex2: [],
        };
        expect(() => T6_D1_JsonCodec.instance.decode(ctx, badJson))
            .toThrow(BaboonDecoderFailure);
        try {
            T6_D1_JsonCodec.instance.decode(ctx, badJson);
        } catch (e) {
            expect((e as Error).message).toContain("malformed key");
        }
    });
});
