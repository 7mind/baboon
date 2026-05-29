import {afterEach, describe, expect, test} from "vitest";
import {ItemKey} from "../src/generated/convtest/m24foreign/ItemKey";
import {FStr_JsonCodec, FStr_UEBACodec} from "../src/generated/convtest/m24foreign/FStr";
import {
    BaboonBinReader,
    BaboonBinWriter,
    BaboonCodecContext,
    BaboonDecoderFailure,
    BaboonEncoderFailure,
    BinTools,
    Lazy,
} from "../src/generated/BaboonSharedRuntime";

// -----------------------------------------------------------------------------
// Custom-foreign VALUE codec (TypeScript mirror of the C# contract).
//
// A Custom-bound foreign (`FStr`) now emits `FStr_JsonCodec` / `FStr_UEBACodec`
// value codec classes whose encode/decode throw by default and are overridable
// via `lazyInstance` — exactly like the C# `IBaboonJsonCodec.Base` foreigns.
// Container types route the foreign FIELD position (here `ItemKey.v`) through
// that codec. This is distinct from the map-KEY path exercised in
// m24-foreign-keycodec.test.ts (which uses `FStr_KeyCodecHost`).
// -----------------------------------------------------------------------------

const ctx = BaboonCodecContext.Default;

// Each test installs its own override; restore the throwing default afterwards
// so static codec state does not leak between tests.
afterEach(() => {
    FStr_JsonCodec.lazyInstance = new Lazy(() => new FStr_JsonCodec());
    FStr_UEBACodec.lazyInstance = new Lazy(() => new FStr_UEBACodec());
});

describe("Custom-foreign value codec (TypeScript)", () => {
    test("JSON value codec throws by default (no impl registered)", () => {
        expect(() => ItemKey.jsonCodec().encode(ctx, new ItemKey("abc"))).toThrow(BaboonEncoderFailure);
        expect(() => ItemKey.jsonCodec().decode(ctx, {v: "abc"})).toThrow(BaboonDecoderFailure);
    });

    test("UEBA value codec throws by default (no impl registered)", () => {
        const writer = new BaboonBinWriter();
        expect(() => ItemKey.binCodec().encode(ctx, new ItemKey("abc"), writer)).toThrow(BaboonEncoderFailure);
    });

    test("JSON value codec uses a host-provided impl via lazyInstance", () => {
        class UpperFStrJsonCodec extends FStr_JsonCodec {
            override encode(_c: BaboonCodecContext, value: string): unknown {
                return value.toUpperCase();
            }
            override decode(_c: BaboonCodecContext, json: unknown): string {
                return (json as string).toLowerCase();
            }
        }
        FStr_JsonCodec.lazyInstance = new Lazy(() => new UpperFStrJsonCodec());

        const encoded = ItemKey.jsonCodec().encode(ctx, new ItemKey("abc"));
        expect(encoded).toEqual({v: "ABC"});
        const decoded = ItemKey.jsonCodec().decode(ctx, {v: "ABC"});
        expect(decoded.v).toBe("abc");
    });

    test("UEBA value codec round-trips through a host-provided impl", () => {
        class StringFStrUEBACodec extends FStr_UEBACodec {
            override encode(_c: BaboonCodecContext, value: string, writer: BaboonBinWriter): void {
                BinTools.writeString(writer, value);
            }
            override decode(_c: BaboonCodecContext, reader: BaboonBinReader): string {
                return BinTools.readString(reader);
            }
        }
        FStr_UEBACodec.lazyInstance = new Lazy(() => new StringFStrUEBACodec());

        const writer = new BaboonBinWriter();
        ItemKey.binCodec().encode(ctx, new ItemKey("hello"), writer);
        const decoded = ItemKey.binCodec().decode(ctx, new BaboonBinReader(writer.toBytes()));
        expect(decoded.v).toBe("hello");
    });
});
