import { describe, expect, test } from "vitest";
import { BaboonBinReader, BaboonBinWriter, BaboonCodecContext, BaboonDecoderFailure, BaboonEncoderFailure, BaboonEnvelopeVersion, ForwardWritePolicy, Lazy } from "../generated/BaboonSharedRuntime";
import { T1_E2, T1_E2_UEBACodec } from "../generated/testpkg/pkg0/T1_E2";
import { T1_E2_RET, T1_E2_RET_UEBACodec } from "../generated/testpkg/pkg0/T1_E2_RET";

describe("UEBA layout validation", () => {
    const value = new T1_E2_RET(T1_E2.A, undefined);

    test("rejects invalid index entries before decoding an otherwise valid payload", () => {
        for (const [offset, length] of [[0, 0], [-1, 1], [0, -1]]) {
            const writer = new BaboonBinWriter();
            writer.writeByte(1);
            writer.writeI32(offset);
            writer.writeI32(length);
            writer.writeByte(0);
            writer.writeByte(0);
            expect(() => T1_E2_RET_UEBACodec.instance.decode(BaboonCodecContext.Compact, new BaboonBinReader(writer.toBytes()))).toThrow(BaboonDecoderFailure);
        }
    });

    test("rejects a missing index when the context requires one", () => {
        const writer = new BaboonBinWriter();
        T1_E2_RET_UEBACodec.instance.encode(BaboonCodecContext.Compact, value, writer);
        expect(() => T1_E2_RET_UEBACodec.instance.decode(BaboonCodecContext.Indexed, new BaboonBinReader(writer.toBytes()))).toThrow(BaboonDecoderFailure);
    });

    test("honors the index flag on custom contexts", () => {
        const ctx = BaboonCodecContext.custom(true, ForwardWritePolicy.Strict, BaboonEnvelopeVersion.V1, undefined);
        const writer = new BaboonBinWriter();
        T1_E2_RET_UEBACodec.instance.encode(ctx, value, writer);
        expect(writer.toBytes()[0]).toBe(1);
        expect(T1_E2_RET_UEBACodec.instance.decode(ctx, new BaboonBinReader(writer.toBytes()))).toEqual(value);
    });

    test("rejects a custom codec that violates its fixed wire length", () => {
        class WrongLength extends T1_E2_UEBACodec {
            override encode(_ctx: BaboonCodecContext, _value: T1_E2, writer: BaboonBinWriter): void {
                writer.writeByte(0);
                writer.writeByte(0);
            }
        }
        const saved = T1_E2_UEBACodec.lazyInstance;
        try {
            T1_E2_UEBACodec.lazyInstance = new Lazy(() => new WrongLength());
            expect(() => T1_E2_RET_UEBACodec.instance.encode(BaboonCodecContext.Indexed, value, new BaboonBinWriter())).toThrow(BaboonEncoderFailure);
        } finally {
            T1_E2_UEBACodec.lazyInstance = saved;
        }
    });
});
