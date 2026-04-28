// Hand-written runtime tests for `BaboonAnyOpaque` (`AnyMeta` / `AnyMetaCodec`),
// `BaboonCodecsFacade`, and the `BaboonCodecContext` extension. Mirrors the Java
// `AnyMetaCodecTest.java` baseline (PR 6.1 ~41-test scope).
//
// Lives under `src/runtime-tests/` rather than `src/baboontests/` so codegen does not stomp it
// (codegen writes into `src/generated/` and `src/baboontests/`). Imports resolve against
// `src/generated/BaboonSharedRuntime.ts` etc., which the compiler writes when the runtime stub
// project is rsync'd + codegen'd.

import { describe, expect, test } from "vitest";

import {
    AnyMeta,
    AnyMetaCodec,
    anyMetaEquals,
    anyOpaqueEquals,
    anyOpaqueJson,
    anyOpaqueUeba,
    createAnyMeta,
    uint8ArrayEquals,
} from "../generated/BaboonAnyOpaque";

import {
    BaboonBinReader,
    BaboonBinWriter,
    BaboonCodecContext,
    BaboonCodecException,
    BaboonCodecNotFound,
    BaboonDecoderFailure,
    BaboonDomainVersion,
    BaboonTypeMeta,
    BaboonVersion,
    BinTools,
    Lazy,
    AbstractBaboonJsonCodecs,
    AbstractBaboonUebaCodecs,
    AbstractBaboonConversions,
    BaboonBinCodec,
    BaboonGenerated,
    BaboonJsonCodec,
    BaboonMeta,
} from "../generated/BaboonSharedRuntime";

import { BaboonCodecsFacade } from "../generated/BaboonCodecsFacade";

function writeBin(meta: AnyMeta): Uint8Array {
    const w = new BaboonBinWriter();
    AnyMetaCodec.writeBin(meta, w);
    return w.toBytes();
}

function readBin(bytes: Uint8Array): AnyMeta {
    return AnyMetaCodec.readBin(new BaboonBinReader(bytes));
}

function readBinWithLength(bytes: Uint8Array): { meta: AnyMeta; bytesRead: number } {
    return AnyMetaCodec.readBinWithLength(new BaboonBinReader(bytes));
}

// ===== AnyMeta invariants ======================================================================

describe("AnyMeta invariants", () => {
    test("rejects bit-mismatch domain (kind 0x07, domain null)", () => {
        // PR-04-D01: the validator enforces all four rules at once.
        expect(() => createAnyMeta(0x07, null, "1.0.0", "T")).toThrow();
    });

    test("rejects bit-mismatch version (kind 0x03, version null)", () => {
        expect(() => createAnyMeta(0x03, null, null, "T")).toThrow();
    });

    test("rejects bit-mismatch typeid (kind 0x01, typeid null)", () => {
        expect(() => createAnyMeta(0x01, null, null, null)).toThrow();
    });

    test("rejects extra domain without bit (kind 0x00, domain set)", () => {
        expect(() => createAnyMeta(0x00, "d", null, null)).toThrow();
    });

    test("rejects reserved kind 0x04", () => {
        // PR-04-D01: 0x04 is reserved.
        expect(() => createAnyMeta(0x04, "d", null, null)).toThrow(/reserved/);
    });

    test("rejects reserved kind 0x05", () => {
        expect(() => createAnyMeta(0x05, "d", null, "T")).toThrow(/reserved/);
    });

    test("rejects non-byte kind (PR-12-D01: TS number is double)", () => {
        expect(() => createAnyMeta(-1, null, null, null)).toThrow();
        expect(() => createAnyMeta(0x100, null, null, null)).toThrow();
        expect(() => createAnyMeta(1.5, null, null, null)).toThrow();
    });
});

// ===== Binary round-trip per kind ==============================================================

describe("AnyMetaCodec binary round-trip", () => {
    test("kind A (0x07) full triple", () => {
        const meta = createAnyMeta(0x07, "dom", "1.0.0", "Type");
        expect(anyMetaEquals(readBin(writeBin(meta)), meta)).toBe(true);
    });

    test("kind B (0x03) version+typeid", () => {
        const meta = createAnyMeta(0x03, null, "1.0.0", "Type");
        expect(anyMetaEquals(readBin(writeBin(meta)), meta)).toBe(true);
    });

    test("kind C (0x01) typeid only", () => {
        const meta = createAnyMeta(0x01, null, null, "Type");
        expect(anyMetaEquals(readBin(writeBin(meta)), meta)).toBe(true);
    });

    test("kind D1 (0x06) domain+version", () => {
        const meta = createAnyMeta(0x06, "dom", "1.0.0", null);
        expect(anyMetaEquals(readBin(writeBin(meta)), meta)).toBe(true);
    });

    test("kind D2 (0x02) version only", () => {
        const meta = createAnyMeta(0x02, null, "1.0.0", null);
        expect(anyMetaEquals(readBin(writeBin(meta)), meta)).toBe(true);
    });

    test("kind D3 (0x00) bare", () => {
        const meta = createAnyMeta(0x00, null, null, null);
        expect(anyMetaEquals(readBin(writeBin(meta)), meta)).toBe(true);
    });
});

// ===== Binary round-trip edge cases ============================================================

describe("AnyMetaCodec binary edge cases", () => {
    test("empty strings", () => {
        const meta = createAnyMeta(0x07, "", "", "");
        expect(anyMetaEquals(readBin(writeBin(meta)), meta)).toBe(true);
    });

    test("non-ASCII UTF-8", () => {
        const meta = createAnyMeta(0x07, "δωμάιν", "1.0.0", "τύπος");
        expect(anyMetaEquals(readBin(writeBin(meta)), meta)).toBe(true);
    });

    test("128-byte string triggers multi-byte ULEB128 length", () => {
        const big = "x".repeat(128);
        const meta = createAnyMeta(0x07, big, "1.0.0", "T");
        expect(anyMetaEquals(readBin(writeBin(meta)), meta)).toBe(true);
    });

    test("readBinWithLength reports exact bytes consumed (PR-05-D01)", () => {
        const meta = createAnyMeta(0x07, "dom", "1.0.0", "Type");
        const baseBytes = writeBin(meta);
        const padded = new Uint8Array(baseBytes.length + 5);
        padded.set(baseBytes, 0);
        padded.set([0xAA, 0xBB, 0xCC, 0xDD, 0xEE], baseBytes.length);

        const out = readBinWithLength(padded);
        expect(anyMetaEquals(out.meta, meta)).toBe(true);
        expect(out.bytesRead).toBe(baseBytes.length);
    });
});

// ===== JSON round-trip =========================================================================

describe("AnyMetaCodec JSON round-trip", () => {
    test("kind A round-trip via writeJson/readJson", () => {
        const meta = createAnyMeta(0x07, "dom", "1.0.0", "T");
        const wire = AnyMetaCodec.writeJson(meta);
        const result = AnyMetaCodec.readJson(wire);
        expect(result.tag).toBe("Right");
        if (result.tag === "Right") expect(anyMetaEquals(result.value, meta)).toBe(true);
    });

    test("all kinds round-trip", () => {
        const all: AnyMeta[] = [
            createAnyMeta(0x07, "d", "1.0.0", "T"),
            createAnyMeta(0x03, null, "1.0.0", "T"),
            createAnyMeta(0x01, null, null, "T"),
            createAnyMeta(0x06, "d", "1.0.0", null),
            createAnyMeta(0x02, null, "1.0.0", null),
            createAnyMeta(0x00, null, null, null),
        ];
        for (const meta of all) {
            const wire = AnyMetaCodec.writeJson(meta);
            const result = AnyMetaCodec.readJson(wire);
            expect(result.tag).toBe("Right");
            if (result.tag === "Right") expect(anyMetaEquals(result.value, meta)).toBe(true);
        }
    });

    test("readJson returns Left on non-object (PR-04-D02)", () => {
        const result = AnyMetaCodec.readJson("not-an-obj");
        expect(result.tag).toBe("Left");
    });

    test("readJson returns Left on missing kind", () => {
        const result = AnyMetaCodec.readJson({ [AnyMetaCodec.ANY_DOMAIN_KEY]: "dom" });
        expect(result.tag).toBe("Left");
    });

    test("readJson returns Left on non-numeric kind", () => {
        const result = AnyMetaCodec.readJson({ [AnyMetaCodec.ANY_KIND_KEY]: "seven" });
        expect(result.tag).toBe("Left");
    });

    test("readJson returns Left on incomplete-for-kind (0x07 + only typeid)", () => {
        const result = AnyMetaCodec.readJson({
            [AnyMetaCodec.ANY_KIND_KEY]: 0x07,
            [AnyMetaCodec.ANY_TYPEID_KEY]: "T",
        });
        expect(result.tag).toBe("Left");
    });

    test("readJson returns Left on forbidden-for-kind (0x00 + domain present)", () => {
        const result = AnyMetaCodec.readJson({
            [AnyMetaCodec.ANY_KIND_KEY]: 0x00,
            [AnyMetaCodec.ANY_DOMAIN_KEY]: "dom",
        });
        expect(result.tag).toBe("Left");
    });

    test("writeJson always returns plain object", () => {
        const wire = AnyMetaCodec.writeJson(createAnyMeta(0x00, null, null, null));
        expect(typeof wire).toBe("object");
        expect(Array.isArray(wire)).toBe(false);
        expect(wire).not.toBeNull();
    });
});

// ===== AnyOpaque content equality (PR-05-D08) ==================================================

describe("AnyOpaque content equality", () => {
    test("anyOpaqueUeba equals by content (Uint8Array reference identity is not enough)", () => {
        const meta = createAnyMeta(0x00, null, null, null);
        const x = anyOpaqueUeba(meta, new Uint8Array([1, 2, 3]));
        const y = anyOpaqueUeba(meta, new Uint8Array([1, 2, 3]));
        expect(anyOpaqueEquals(x, y)).toBe(true);

        const z = anyOpaqueUeba(meta, new Uint8Array([1, 2, 4]));
        expect(anyOpaqueEquals(x, z)).toBe(false);
    });

    test("anyOpaqueJson equals by content (deep equal)", () => {
        const meta = createAnyMeta(0x00, null, null, null);
        const x = anyOpaqueJson(meta, { a: 1, b: "z" });
        const y = anyOpaqueJson(meta, { a: 1, b: "z" });
        expect(anyOpaqueEquals(x, y)).toBe(true);

        const z = anyOpaqueJson(meta, { a: 2, b: "z" });
        expect(anyOpaqueEquals(x, z)).toBe(false);
    });

    test("uint8ArrayEquals on identical reference", () => {
        const a = new Uint8Array([1, 2, 3]);
        expect(uint8ArrayEquals(a, a)).toBe(true);
    });

    test("uint8ArrayEquals on different lengths", () => {
        expect(uint8ArrayEquals(new Uint8Array([1, 2]), new Uint8Array([1, 2, 3]))).toBe(false);
    });
});

// ===== BaboonCodecContext extension ============================================================

describe("BaboonCodecContext withFacade", () => {
    test("Compact has undefined facade and useIndices=false", () => {
        expect(BaboonCodecContext.Compact.facade).toBeUndefined();
        expect(BaboonCodecContext.Compact.useIndices).toBe(false);
    });

    test("Indexed has undefined facade and useIndices=true", () => {
        expect(BaboonCodecContext.Indexed.facade).toBeUndefined();
        expect(BaboonCodecContext.Indexed.useIndices).toBe(true);
    });

    test("Default is the Compact singleton", () => {
        expect(BaboonCodecContext.Default).toBe(BaboonCodecContext.Compact);
    });

    test("withFacade exposes the supplied facade", () => {
        const facade = new BaboonCodecsFacade();
        const ctx = BaboonCodecContext.withFacade(true, facade);
        expect(ctx.facade).toBe(facade);
        expect(ctx.useIndices).toBe(true);
    });

    test("singletons are reference-stable for === comparison (existing generated code)", () => {
        // Generated codecs do `ctx === BaboonCodecContext.Indexed` — the class promotion must not
        // break this contract.
        expect(BaboonCodecContext.Indexed === BaboonCodecContext.Indexed).toBe(true);
        expect(BaboonCodecContext.Compact === BaboonCodecContext.Indexed).toBe(false);
    });
});

// ===== Cross-format helpers (no codecs registered) =============================================

describe("BaboonCodecsFacade cross-format helpers (incomplete meta / no codec)", () => {
    test("jsonToUebaBytes returns Left on incomplete meta (PR-06-D01)", () => {
        const facade = new BaboonCodecsFacade();
        const meta = createAnyMeta(0x00, null, null, null);
        const r = facade.jsonToUebaBytes(meta, {});
        expect(r.tag).toBe("Left");
    });

    test("uebaToJson returns Left on incomplete meta", () => {
        const facade = new BaboonCodecsFacade();
        const meta = createAnyMeta(0x00, null, null, null);
        const r = facade.uebaToJson(meta, new Uint8Array([1, 2, 3]));
        expect(r.tag).toBe("Left");
    });

    test("jsonToUebaBytes returns Left on no codec", () => {
        const facade = new BaboonCodecsFacade();
        const meta = createAnyMeta(0x07, "dom", "1.0.0", "T");
        const r = facade.jsonToUebaBytes(meta, {});
        expect(r.tag).toBe("Left");
        if (r.tag === "Left") expect(r.value).toBeInstanceOf(BaboonCodecNotFound);
    });

    test("uebaToJson returns Left on no codec", () => {
        const facade = new BaboonCodecsFacade();
        const meta = createAnyMeta(0x07, "dom", "1.0.0", "T");
        const r = facade.uebaToJson(meta, new Uint8Array([1, 2, 3]));
        expect(r.tag).toBe("Left");
        if (r.tag === "Left") expect(r.value).toBeInstanceOf(BaboonCodecNotFound);
    });

    test("decodeAny returns Left on incomplete meta", () => {
        const facade = new BaboonCodecsFacade();
        const meta = createAnyMeta(0x00, null, null, null);
        const opaque = anyOpaqueUeba(meta, new Uint8Array([0]));
        const r = facade.decodeAny(opaque);
        expect(r.tag).toBe("Left");
    });
});

// ===== Static-fallback semantics (PR-06-D01) ===================================================

describe("Static-fallback semantics", () => {
    test("kind 0x00 with statics resolves past meta-validation", () => {
        // No codec registered, but the failure must be CodecNotFound (synthesised meta is
        // complete) rather than "AnyMeta requires ...".
        const facade = new BaboonCodecsFacade();
        const meta = createAnyMeta(0x00, null, null, null);
        const r = facade.jsonToUebaBytes(meta, {}, "dom", "1.0.0", "T");
        expect(r.tag).toBe("Left");
        if (r.tag === "Left") expect(r.value).toBeInstanceOf(BaboonCodecNotFound);
    });

    test("wire meta domain overrides static (PR-06-D01: wire wins)", () => {
        const facade = new BaboonCodecsFacade();
        const meta = createAnyMeta(0x07, "dom-wire", "1.0.0", "T");
        const r = facade.jsonToUebaBytes(meta, {}, "dom-static", "0.9.0", "Other");
        expect(r.tag).toBe("Left");
        if (r.tag === "Left") expect(r.value.message).toContain("dom-wire");
    });
});

// ===== Single-version-domain regression (PR-07-D02) ============================================

class StubBin extends AbstractBaboonUebaCodecs {
    public addCodec(typeId: string, codec: BaboonBinCodec<unknown>): void {
        this.register(typeId, new Lazy(() => codec));
    }
}

class StubJson extends AbstractBaboonJsonCodecs {
    public addCodec(typeId: string, codec: BaboonJsonCodec<unknown>): void {
        this.register(typeId, new Lazy(() => codec));
    }
}

class StubMeta implements BaboonMeta {
    constructor(private readonly versions: string[]) {}
    public sameInVersions(_typeId: string): string[] { return this.versions; }
}

class StubConv implements AbstractBaboonConversions {
    public versionsFrom(): string[] { return []; }
    public versionTo(): string { return "1.0.0"; }
}

class StubGenerated implements BaboonGenerated {
    public baboonDomainVersion(): string { return "1.0.0"; }
    public baboonDomainIdentifier(): string { return "dom"; }
    public baboonSameInVersions(): string[] { return ["1.0.0"]; }
    public baboonTypeIdentifier(): string { return "T"; }
}

class StubBinCodec implements BaboonBinCodec<BaboonGenerated> {
    public encode(_ctx: BaboonCodecContext, _value: BaboonGenerated, writer: BaboonBinWriter): void {
        BinTools.writeByte(writer, 0x42);
    }
    public decode(_ctx: BaboonCodecContext, reader: BaboonBinReader): BaboonGenerated {
        BinTools.readByte(reader);
        return new StubGenerated();
    }
}

class StubJsonCodec implements BaboonJsonCodec<BaboonGenerated> {
    public encode(_ctx: BaboonCodecContext, _value: BaboonGenerated): unknown {
        return "ok";
    }
    public decode(_ctx: BaboonCodecContext, _wire: unknown): BaboonGenerated {
        return new StubGenerated();
    }
}

describe("getCodec single-version-domain (PR-07-D02)", () => {
    test("non-exact lookup at the latest version routes to exact lookup", () => {
        const facade = new BaboonCodecsFacade();
        const dv = new BaboonDomainVersion("dom", "1.0.0");
        const binCodecs = new StubBin();
        binCodecs.addCodec("T", new StubBinCodec());
        const jsonCodecs = new StubJson();
        jsonCodecs.addCodec("T", new StubJsonCodec());
        const meta = new StubMeta(["1.0.0"]);
        const conv = new StubConv();

        facade.register(dv, () => jsonCodecs, () => binCodecs, () => conv, () => meta);

        const any = createAnyMeta(0x07, "dom", "1.0.0", "T");
        const payload = new Uint8Array([0x42]);
        const r = facade.uebaToJson(any, payload);
        expect(r.tag).toBe("Right");
    });

    test("verify() rejects empty registry (PR-08-D02 fail-fast)", () => {
        const facade = new BaboonCodecsFacade();
        expect(() => facade.verify()).toThrow();
    });
});

// ===== encodeToBin / encodeToJson useAdtIdentifier plumbing (PR-19-D02) =========================

/**
 * Stub that simultaneously satisfies `BaboonGenerated` (concrete-branch identifier) and
 * `BaboonAdtMemberMeta` (ADT identifier). Mirrors the shape generated for an ADT branch.
 */
class StubAdtBranchGenerated implements BaboonGenerated {
    public readonly baboonAdtTypeIdentifier: string = "AdtT";
    public baboonDomainVersion(): string { return "1.0.0"; }
    public baboonDomainIdentifier(): string { return "dom"; }
    public baboonSameInVersions(): string[] { return ["1.0.0"]; }
    public baboonTypeIdentifier(): string { return "BranchT"; }
}

describe("encodeToBin / encodeToJson useAdtIdentifier plumbing (PR-19-D02)", () => {
    function makeFacade(): BaboonCodecsFacade {
        const facade = new BaboonCodecsFacade();
        const dv = new BaboonDomainVersion("dom", "1.0.0");
        const binCodecs = new StubBin();
        binCodecs.addCodec("BranchT", new StubBinCodec());
        binCodecs.addCodec("AdtT", new StubBinCodec());
        const jsonCodecs = new StubJson();
        jsonCodecs.addCodec("BranchT", new StubJsonCodec());
        jsonCodecs.addCodec("AdtT", new StubJsonCodec());
        const meta = new StubMeta(["1.0.0"]);
        const conv = new StubConv();
        facade.register(dv, () => jsonCodecs, () => binCodecs, () => conv, () => meta);
        return facade;
    }

    test("encodeToBin defaults to concrete branch typeid", () => {
        const facade = makeFacade();
        const r = facade.encodeToBin(BaboonCodecContext.Compact, new StubAdtBranchGenerated());
        expect(r.tag).toBe("Right");
        if (r.tag !== "Right") return;
        const meta = BaboonTypeMeta.readMeta(new BaboonBinReader(r.value));
        expect(meta).toBeDefined();
        expect(meta!.typeIdentifier).toBe("BranchT");
    });

    test("encodeToBin with useAdtIdentifier=true uses the ADT typeid", () => {
        const facade = makeFacade();
        const r = facade.encodeToBin(BaboonCodecContext.Compact, new StubAdtBranchGenerated(), undefined, true);
        expect(r.tag).toBe("Right");
        if (r.tag !== "Right") return;
        const meta = BaboonTypeMeta.readMeta(new BaboonBinReader(r.value));
        expect(meta).toBeDefined();
        expect(meta!.typeIdentifier).toBe("AdtT");
    });

    test("encodeToJson defaults to concrete branch typeid", () => {
        const facade = makeFacade();
        const r = facade.encodeToJson(new StubAdtBranchGenerated());
        expect(r.tag).toBe("Right");
        if (r.tag !== "Right") return;
        expect(r.value["$t"]).toBe("BranchT");
    });

    test("encodeToJson with useAdtIdentifier=true uses the ADT typeid", () => {
        const facade = makeFacade();
        const r = facade.encodeToJson(new StubAdtBranchGenerated(), undefined, true);
        expect(r.tag).toBe("Right");
        if (r.tag !== "Right") return;
        expect(r.value["$t"]).toBe("AdtT");
    });
});

// ===== BaboonTypeMeta JSON $mv handling (PR-08-D01) ============================================

describe("BaboonTypeMeta.readMetaJson $mv handling", () => {
    test("accepts absent $mv", () => {
        const meta = BaboonTypeMeta.readMetaJson({ "$d": "dom", "$v": "1.0.0", "$t": "T" });
        expect(meta).toBeDefined();
        expect(meta!.domainIdentifier).toBe("dom");
    });

    test("accepts explicit $mv=1", () => {
        const meta = BaboonTypeMeta.readMetaJson({
            "$mv": "1", "$d": "dom", "$v": "1.0.0", "$t": "T",
        });
        expect(meta).toBeDefined();
    });

    test("rejects $mv=2", () => {
        const meta = BaboonTypeMeta.readMetaJson({
            "$mv": "2", "$d": "dom", "$v": "1.0.0", "$t": "T",
        });
        expect(meta).toBeUndefined();
    });

    test("rejects non-string $mv", () => {
        const meta = BaboonTypeMeta.readMetaJson({
            "$mv": 1, "$d": "dom", "$v": "1.0.0", "$t": "T",
        });
        expect(meta).toBeUndefined();
    });
});

// ===== BaboonVersion sanity ====================================================================

describe("BaboonVersion", () => {
    test("parses x.y.z and compares numerically", () => {
        const a = BaboonVersion.from("1.2.3");
        const b = BaboonVersion.from("1.2.4");
        expect(a.compareTo(b)).toBeLessThan(0);
        expect(b.compareTo(a)).toBeGreaterThan(0);
        expect(a.compareTo(BaboonVersion.from("1.2.3"))).toBe(0);
    });

    test("rejects non-x.y.z input", () => {
        expect(() => BaboonVersion.from("1.2")).toThrow();
        expect(() => BaboonVersion.from("a.b.c")).toThrow();
    });
});

// ===== Typed exception kind discrimination =====================================================

describe("BaboonCodecException kind discrimination", () => {
    test("kind tag matches subclass (no instanceof needed)", () => {
        const e: BaboonCodecException = new BaboonDecoderFailure("x");
        expect(e.kind).toBe("DecoderFailure");
        const e2: BaboonCodecException = new BaboonCodecNotFound("y");
        expect(e2.kind).toBe("CodecNotFound");
    });
});
