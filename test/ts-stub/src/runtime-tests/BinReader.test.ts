// Regression tests for `BaboonBinReader.readByte()` past-EOF behavior (PR-19-D05) and
// `BaboonTypeMeta.versionMinCompat()` empty-string handling (peer-runtime parity, PR-25.4-R01).
//
// PR-19-D05: `readByte()` declares `: number`. The bounds-check at `BaboonSharedRuntime.ts`
// (`if (this.pos >= this.buf.length) throw new BaboonDecoderFailure(...)`) is verified here as a
// regression guard.
//
// PR-25.4-R01 (closes PR-19-D06 wontfix): `versionMinCompat()` treats empty-string as absent for
// cross-runtime parity. Scala/Kotlin/KMP/Java/Swift/Dart all normalize empty/blank to None|null;
// TS follows suit. PR-25.4 round-1 attempted to honor empty-string as a distinct value and was
// reverted on adversarial review (asymmetric round-trip across runtimes). The empty-string test
// below now guards the parity invariant against future drift.

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

describe("BaboonTypeMeta.versionMinCompat() empty-string handling (peer-runtime parity, PR-25.4-R01)", () => {
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

    test("treats empty-string as absent (peer-runtime parity with Scala/Kotlin/KMP/Java/Swift/Dart)", () => {
        const meta = new BaboonTypeMeta(
            BaboonTypeMetaCodec.META_VERSION,
            "foo.bar",
            "1.0.0",
            "",
            "T",
        );
        expect(meta.versionMinCompat()).toBeUndefined();
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
