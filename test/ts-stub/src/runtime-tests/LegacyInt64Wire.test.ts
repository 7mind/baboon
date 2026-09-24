// 64-bit integers are written as decimal strings (docs/json-codecs.md, "64-bit integers").
// Readers stay lenient about the JSON-number form an older compiler produced, and that
// leniency is what keeps documents written before the change readable.
//
// Nothing writes numbers any more, so without this test the number arm of every decoder is
// dead as far as the suite is concerned. TypeScript is the one backend where the leniency
// is bounded: a numeric token above Number.MAX_SAFE_INTEGER was already destroyed by
// JSON.parse, so it is refused rather than rounded (see Int64Precision.test.ts).
//
// Uses the identifier.ok fixture (id LongId { x: i64 }; id UInts { a: u08, b: u16, c: u32,
// d: u64 }); generated symbols are produced by mdl :test-gen-regular-adt.

import { describe, expect, test } from "vitest";

import { BaboonCodecContext } from "../generated/BaboonSharedRuntime";
import { LongId_JsonCodec } from "../generated/identifier/ok/LongId";
import { UInts_JsonCodec } from "../generated/identifier/ok/UInts";

describe("64-bit integers decode from both wire forms", () => {
    const ctx = BaboonCodecContext.Compact;

    test("i64 decodes from the legacy numeric form", () => {
        const decoded = LongId_JsonCodec.instance.decode(ctx, JSON.parse('{"x":-9007199254740991}'));
        expect(decoded.x).toBe(-9007199254740991n);
    });

    test("i64 decodes from the string form", () => {
        const decoded = LongId_JsonCodec.instance.decode(ctx, JSON.parse('{"x":"-9223372036854775808"}'));
        expect(decoded.x).toBe(-9223372036854775808n);
    });

    test("u64 decodes from the legacy numeric form", () => {
        const decoded = UInts_JsonCodec.instance.decode(ctx, JSON.parse('{"a":1,"b":2,"c":3,"d":42}'));
        expect(decoded.d).toBe(42n);
    });

    test("u64 decodes from the string form", () => {
        const decoded = UInts_JsonCodec.instance.decode(ctx, JSON.parse('{"a":1,"b":2,"c":3,"d":"42"}'));
        expect(decoded.d).toBe(42n);
    });
});
