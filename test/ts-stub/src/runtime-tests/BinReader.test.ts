// Regression tests for `BaboonBinReader.readByte()` past-EOF behavior (PR-19-D05) and
// `BaboonTypeMeta.versionMinCompat()` empty-string handling (PR-19-D06).
//
// PR-19-D05: `readByte()` declares `: number`. The bounds-check at `BaboonSharedRuntime.ts`
// (`if (this.pos >= this.buf.length) throw new BaboonDecoderFailure(...)`) is verified here as a
// regression guard.
//
// PR-19-D06: `versionMinCompat()` previously falsy-checked `domainVersionMinCompat`, conflating
// empty-string (an explicit value) with absent. Codegen always emits a real value when set, so
// empty-string must be honored verbatim.

import { describe, expect, test } from "vitest";

import {
    BaboonBinReader,
    BaboonDecoderFailure,
    BaboonTypeMeta,
    BaboonTypeMetaCodec,
} from "../generated/BaboonSharedRuntime";

describe("BaboonBinReader.readByte() bounds (PR-19-D05)", () => {
    test("throws BaboonDecoderFailure past end-of-buffer", () => {
        const reader = new BaboonBinReader(new Uint8Array([0x42]));
        expect(reader.readByte()).toBe(0x42);
        expect(() => reader.readByte()).toThrow(BaboonDecoderFailure);
    });

    test("throws BaboonDecoderFailure on empty buffer", () => {
        const reader = new BaboonBinReader(new Uint8Array([]));
        expect(() => reader.readByte()).toThrow(BaboonDecoderFailure);
    });
});

describe("BaboonTypeMeta.versionMinCompat() empty-string handling (PR-19-D06)", () => {
    test("returns undefined when min-compat equals domain version", () => {
        const meta = new BaboonTypeMeta(
            BaboonTypeMetaCodec.META_VERSION,
            "foo.bar",
            "1.0.0",
            "1.0.0",
            "T",
        );
        expect(meta.versionMinCompat()).toBeUndefined();
    });

    test("honors empty-string as an explicit min-compat value distinct from domainVersion", () => {
        const meta = new BaboonTypeMeta(
            BaboonTypeMetaCodec.META_VERSION,
            "foo.bar",
            "1.0.0",
            "",
            "T",
        );
        const dv = meta.versionMinCompat();
        expect(dv).toBeDefined();
        expect(dv!.domainIdentifier).toBe("foo.bar");
        expect(dv!.domainVersion).toBe("");
    });

    test("returns proper BaboonDomainVersion for distinct min-compat", () => {
        const meta = new BaboonTypeMeta(
            BaboonTypeMetaCodec.META_VERSION,
            "foo.bar",
            "1.2.0",
            "1.0.0",
            "T",
        );
        const dv = meta.versionMinCompat();
        expect(dv).toBeDefined();
        expect(dv!.domainIdentifier).toBe("foo.bar");
        expect(dv!.domainVersion).toBe("1.0.0");
    });
});
