import { Buffer } from "node:buffer";
import { describe, expect, test } from "vitest";
import { BaboonBinReader, BaboonBinWriter, BinTools } from "../generated/BaboonSharedRuntime";

describe("binary storage", () => {
    test("fixed-width writes retain little-endian bytes across growth", () => {
        const writer = new BaboonBinWriter(1);
        BinTools.writeI8(writer, -128);
        BinTools.writeU8(writer, 255);
        BinTools.writeI16(writer, -32768);
        BinTools.writeU16(writer, 65535);
        BinTools.writeI32(writer, -2147483648);
        BinTools.writeU32(writer, 4294967295);
        BinTools.writeI64(writer, -9223372036854775808n);
        BinTools.writeU64(writer, 18446744073709551615n);
        BinTools.writeF32(writer, -0);
        BinTools.writeF64(writer, Infinity);
        expect(Buffer.from(writer.toBytes()).toString("hex")).toBe(
            "80ff0080ffff00000080ffffffff0000000000000080ffffffffffffffff00000080000000000000f07f",
        );
        expect(writer.position()).toBe(42);
    });

    test("float special values and integer wrapping retain their values", () => {
        const writer = new BaboonBinWriter(3);
        BinTools.writeI8(writer, 255);
        BinTools.writeU16(writer, -1);
        BinTools.writeF32(writer, NaN);
        BinTools.writeF64(writer, -0);
        BinTools.writeF64(writer, -Infinity);
        const reader = new BaboonBinReader(writer.toBytes());
        expect(reader.readI8()).toBe(-1);
        expect(reader.readU16()).toBe(65535);
        expect(reader.readF32()).toBeNaN();
        expect(Object.is(reader.readF64(), -0)).toBe(true);
        expect(reader.readF64()).toBe(-Infinity);
        expect(reader.remaining()).toBe(0);
    });

    test("exported bytes are independent of writer storage", () => {
        const writer = new BaboonBinWriter(2);
        BinTools.writeU16(writer, 0x1234);
        const first = writer.toBytes();
        first[0] = 0;
        BinTools.writeU32(writer, 0xabcdef01);
        expect([...writer.toBytes()]).toEqual([0x34, 0x12, 1, 0xef, 0xcd, 0xab]);
        expect([...first]).toEqual([0, 0x12]);
    });

    for (const input of [new Uint8Array([9, 1, 2, 3, 8]), Buffer.from([9, 1, 2, 3, 8])]) {
        test(`readBytes owns its result for ${input.constructor.name} offset views`, () => {
            const reader = new BaboonBinReader(input.subarray(1, 4));
            const bytes = reader.readBytes(2);
            expect(bytes.constructor).toBe(Uint8Array);
            expect([...bytes]).toEqual([1, 2]);
            bytes[0] = 7;
            expect(input[1]).toBe(1);
            input[2] = 6;
            expect(bytes[1]).toBe(2);
            expect(reader.position()).toBe(2);
            expect(reader.readByte()).toBe(3);
        });
    }

    test("readBytes preserves truncation and cursor advancement", () => {
        const reader = new BaboonBinReader(new Uint8Array([1, 2]));
        expect([...reader.readBytes(4)]).toEqual([1, 2]);
        expect(reader.position()).toBe(4);
        expect(reader.remaining()).toBe(-2);
    });
});
