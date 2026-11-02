// `bytes` JSON hex-decoding contract (issues #102 / #103): ASCII hex digits of either case,
// ASCII whitespace / ':' / '-' as separators, everything else throws BaboonDecoderFailure —
// never wrong bytes.
//
// The JSON-codec cases use testpkg.pkg0 v3 `T1_D1 { f0: T1_D2, f1: i08, f3: str, f4: bytes, f5: opt[bytes] }`.
// Generated symbols are produced by mdl :test-gen-regular-adt under target/test-regular/ts-stub/.

import { describe, expect, test } from "vitest";

import { BaboonCodecContext, BaboonDecoderFailure, BinTools } from "../generated/BaboonSharedRuntime";
import { T1_D1_JsonCodec } from "../generated/testpkg/pkg0/T1_D1";

const VALID: [string, number[]][] = [
    ["", []],
    ["ABCD", [0xAB, 0xCD]],
    ["abcd", [0xAB, 0xCD]],
    ["aBcD", [0xAB, 0xCD]],
    ["AB CD", [0xAB, 0xCD]],
    ["AB:CD", [0xAB, 0xCD]],
    ["AB-CD", [0xAB, 0xCD]],
    ["A B C D", [0xAB, 0xCD]],
    ["\tAB\r\nCD ", [0xAB, 0xCD]],
    ["A:B-C D", [0xAB, 0xCD]],
    ["00FF7f", [0x00, 0xFF, 0x7F]],
    [" - : ", []],
];

const INVALID: string[] = [
    "ZZ",
    "AQ",
    "+1",
    "0x01",
    "q80=",
    "ABC",
    "A",
    "-1",
    "٣٣",
    "ＡＡ",
    "aéb",
    "AB CD",
];

const t1d1Json = (f4: string, f5: string) => ({ f0: { f: "x" }, f1: 1, f3: "s", f4, f5 });

describe("BinTools.hexDecode", () => {
    test.each(VALID)("decodes %j", (input, expected) => {
        expect(Array.from(BinTools.hexDecode(input))).toEqual(expected);
    });

    test.each(INVALID)("rejects %j with BaboonDecoderFailure", (input) => {
        expect(() => BinTools.hexDecode(input)).toThrow(BaboonDecoderFailure);
    });

    test("rejects a non-string wire value with BaboonDecoderFailure", () => {
        expect(() => BinTools.hexDecode(12 as unknown as string)).toThrow(BaboonDecoderFailure);
    });
});

describe("T1_D1 JSON decode of bytes fields", () => {
    const ctx = BaboonCodecContext.Compact;

    test("accepts separated hex", () => {
        const decoded = T1_D1_JsonCodec.instance.decode(ctx, t1d1Json("ab:cd", "00 FF-7f"));
        expect(Array.from(decoded.f4)).toEqual([0xAB, 0xCD]);
        expect(Array.from(decoded.f5!)).toEqual([0x00, 0xFF, 0x7F]);
    });

    test.each(INVALID)("throws BaboonDecoderFailure for malformed hex %j", (input) => {
        expect(() => T1_D1_JsonCodec.instance.decode(ctx, t1d1Json(input, "00"))).toThrow(BaboonDecoderFailure);
        expect(() => T1_D1_JsonCodec.instance.decode(ctx, t1d1Json("00", input))).toThrow(BaboonDecoderFailure);
    });
});
