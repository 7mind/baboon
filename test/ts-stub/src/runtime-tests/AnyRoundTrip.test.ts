// NOTE: This test references generated runtime symbols (AnyMeta, AnyMetaCodec,
// BaboonCodecsFacade, ...) AND generated DTO/codec symbols (my.ok.Holder, my.ok.Inner, ...)
// which are produced into this stub only when codegen runs against the full model set
// (including `any-ok/`). Running `vitest` directly from the source tree without codegen
// will fail with missing symbols; run from the codegen'd copy under `target/`.
//
// Round-trip and cross-format tests for `any` fields (issue #69 PR 7.4 / M7 close).
// Mirrors Scala's AnyRoundTripSpec (PR 2.4) / C# (PR 3.4) / Rust (PR 4.3) /
// Kotlin (PR 5.4) / Java (PR 6.4). Exercises the `any-ok` fixture's six DSL variants
// (A=any, B=any[domain:this], C=any[domain:current], D1=any[Inner], D2=any[domain:this,Inner],
// D3=any[domain:current,Inner]) plus the three nested positions (opt/lst/map-value).

import { describe, expect, test } from "vitest";

import {
    AnyMeta,
    AnyMetaCodec,
    AnyOpaque,
    anyOpaqueJson,
    anyOpaqueUeba,
    createAnyMeta,
} from "../generated/BaboonAnyOpaque";

import {
    AbstractBaboonConversions,
    AbstractBaboonJsonCodecs,
    AbstractBaboonUebaCodecs,
    BaboonBinCodec,
    BaboonBinReader,
    BaboonBinWriter,
    BaboonCodecContext,
    BaboonDomainVersion,
    BaboonEncoderFailure,
    BaboonJsonCodec,
    BaboonMeta,
    BinTools,
    Lazy,
} from "../generated/BaboonSharedRuntime";

import { BaboonCodecsFacade } from "../generated/BaboonCodecsFacade";

import { Holder, Holder_JsonCodec, Holder_UEBACodec } from "../generated/my/ok/Holder";
import { Inner, Inner_JsonCodec, Inner_UEBACodec } from "../generated/my/ok/Inner";

// ===== Test scaffolding ========================================================================

const DOMAIN_ID = "my.ok";
const VERSION_STR = "1.0.0";
const INNER_TYPE = "my.ok/:#Inner";

// AnyMeta builders for each variant — populating only the bits the kind byte claims.
const metaA = (typeid: string = "opaque.Type") => createAnyMeta(0x07, DOMAIN_ID, VERSION_STR, typeid);
const metaB = (typeid: string = "opaque.Type") => createAnyMeta(0x03, null, VERSION_STR, typeid);
const metaC = (typeid: string = "opaque.Type") => createAnyMeta(0x01, null, null, typeid);
const metaD1 = () => createAnyMeta(0x06, DOMAIN_ID, VERSION_STR, null);
const metaD2 = () => createAnyMeta(0x02, null, VERSION_STR, null);
const metaD3 = () => createAnyMeta(0x00, null, null, null);

const SAMPLE_INNER = new Inner(42);

// Generated codecs use the order `(ctx, value, writer)` for UEBA `encode`, but the registry
// `BaboonBinCodec<T>` interface uses `(ctx, writer, value)`. Wrap so the facade's cross-convert
// can call the codec with the interface order. This adapter pattern matches the StubBinCodec
// shape in AnyMetaCodec.test.ts.
class InnerBinAdapter implements BaboonBinCodec<Inner> {
    public encode(ctx: BaboonCodecContext, writer: BaboonBinWriter, value: Inner): void {
        Inner_UEBACodec.instance.encode(ctx, value, writer);
    }
    public decode(ctx: BaboonCodecContext, reader: BaboonBinReader): Inner {
        return Inner_UEBACodec.instance.decode(ctx, reader);
    }
}

class HolderBinAdapter implements BaboonBinCodec<Holder> {
    public encode(ctx: BaboonCodecContext, writer: BaboonBinWriter, value: Holder): void {
        Holder_UEBACodec.instance.encode(ctx, value, writer);
    }
    public decode(ctx: BaboonCodecContext, reader: BaboonBinReader): Holder {
        return Holder_UEBACodec.instance.decode(ctx, reader);
    }
}

class InnerJsonAdapter implements BaboonJsonCodec<Inner> {
    public encode(ctx: BaboonCodecContext, value: Inner): unknown {
        return Inner_JsonCodec.instance.encode(ctx, value);
    }
    public decode(ctx: BaboonCodecContext, wire: unknown): Inner {
        return Inner_JsonCodec.instance.decode(ctx, wire);
    }
}

class HolderJsonAdapter implements BaboonJsonCodec<Holder> {
    public encode(ctx: BaboonCodecContext, value: Holder): unknown {
        return Holder_JsonCodec.instance.encode(ctx, value);
    }
    public decode(ctx: BaboonCodecContext, wire: unknown): Holder {
        return Holder_JsonCodec.instance.decode(ctx, wire);
    }
}

class MyOkUebaCodecs extends AbstractBaboonUebaCodecs {
    constructor() {
        super();
        this.register(Inner.BaboonTypeIdentifier, new Lazy<BaboonBinCodec<unknown>>(() => new InnerBinAdapter() as BaboonBinCodec<unknown>));
        this.register(Holder.BaboonTypeIdentifier, new Lazy<BaboonBinCodec<unknown>>(() => new HolderBinAdapter() as BaboonBinCodec<unknown>));
    }
}

class MyOkJsonCodecs extends AbstractBaboonJsonCodecs {
    constructor() {
        super();
        this.register(Inner.BaboonTypeIdentifier, new Lazy<BaboonJsonCodec<unknown>>(() => new InnerJsonAdapter() as BaboonJsonCodec<unknown>));
        this.register(Holder.BaboonTypeIdentifier, new Lazy<BaboonJsonCodec<unknown>>(() => new HolderJsonAdapter() as BaboonJsonCodec<unknown>));
    }
}

class MyOkConversions implements AbstractBaboonConversions {
    public versionsFrom(): string[] { return []; }
    public versionTo(): string { return VERSION_STR; }
}

class MyOkMeta implements BaboonMeta {
    public sameInVersions(_typeId: string): string[] { return [VERSION_STR]; }
}

// Fresh per-test facade: registers Holder/Inner codecs from the my.ok domain so the
// cross-format helpers and decodeAny can resolve `(domain, version, typeid)` triples.
function freshFacade(): BaboonCodecsFacade {
    const facade = new BaboonCodecsFacade();
    const dv = new BaboonDomainVersion(DOMAIN_ID, VERSION_STR);
    facade.register(
        dv,
        () => new MyOkJsonCodecs(),
        () => new MyOkUebaCodecs(),
        () => new MyOkConversions(),
        () => new MyOkMeta(),
    );
    return facade;
}

// Encode an Inner via the generated UEBA codec — used when constructing AnyOpaqueUeba payloads
// so cross-convert tests have a real Inner to deserialize.
function innerToUebaBytes(inner: Inner): Uint8Array {
    const writer = new BaboonBinWriter();
    Inner_UEBACodec.instance.encode(BaboonCodecContext.Compact, inner, writer);
    return writer.toBytes();
}

function innerToJson(inner: Inner): unknown {
    return Inner_JsonCodec.instance.encode(BaboonCodecContext.Compact, inner);
}

// Build a complete Holder with one AnyOpaqueUeba per variant. UEBA round-trips natively
// (no facade needed for encode/decode — facade is only consulted for cross-convert).
function buildUebaHolder(): Holder {
    const innerBytes = innerToUebaBytes(SAMPLE_INNER);
    return new Holder(
        anyOpaqueUeba(metaA(), new Uint8Array([1, 2, 3])),
        anyOpaqueUeba(metaB(), new Uint8Array([4, 5])),
        anyOpaqueUeba(metaC(), new Uint8Array([6])),
        anyOpaqueUeba(metaD1(), innerBytes),
        anyOpaqueUeba(metaD2(), innerBytes),
        anyOpaqueUeba(metaD3(), innerBytes),
        anyOpaqueUeba(metaA(), new Uint8Array([7])),
        [anyOpaqueUeba(metaD1(), innerBytes)],
        new Map([["k1", anyOpaqueUeba(metaA(), new Uint8Array([8]))]]),
    );
}

// Build a Holder using AnyOpaqueJson branches everywhere with arbitrary inner JSON content.
// Used as the "all native JSON branch" baseline for JSON round-trip tests.
function buildJsonNativeHolder(): Holder {
    const arbitraryJson = { payload: 42 };
    const innerJson = innerToJson(SAMPLE_INNER);
    return new Holder(
        anyOpaqueJson(metaA(), arbitraryJson),
        anyOpaqueJson(metaB(), arbitraryJson),
        anyOpaqueJson(metaC(), arbitraryJson),
        anyOpaqueJson(metaD1(), innerJson),
        anyOpaqueJson(metaD2(), innerJson),
        anyOpaqueJson(metaD3(), innerJson),
        anyOpaqueJson(metaA(), arbitraryJson),
        [anyOpaqueJson(metaD1(), innerJson)],
        new Map([["k1", anyOpaqueJson(metaA(), arbitraryJson)]]),
    );
}

// Build a Holder using AnyOpaqueJson branches with REAL Inner JSON for D variants and
// typeid=INNER_TYPE for A/B/C so cross-convert can resolve Inner via the registered facade.
function buildJsonHolderForCrossConvert(): Holder {
    const innerJson = innerToJson(SAMPLE_INNER);
    return new Holder(
        anyOpaqueJson(metaA(INNER_TYPE), innerJson),
        anyOpaqueJson(metaB(INNER_TYPE), innerJson),
        anyOpaqueJson(metaC(INNER_TYPE), innerJson),
        anyOpaqueJson(metaD1(), innerJson),
        anyOpaqueJson(metaD2(), innerJson),
        anyOpaqueJson(metaD3(), innerJson),
        anyOpaqueJson(metaA(INNER_TYPE), innerJson),
        [anyOpaqueJson(metaD1(), innerJson)],
        new Map([["k1", anyOpaqueJson(metaA(INNER_TYPE), innerJson)]]),
    );
}

function encodeUebaBytes(value: Holder, ctx: BaboonCodecContext): Uint8Array {
    const writer = new BaboonBinWriter();
    Holder_UEBACodec.instance.encode(ctx, value, writer);
    return writer.toBytes();
}

function decodeUebaBytes(bytes: Uint8Array, ctx: BaboonCodecContext = BaboonCodecContext.Compact): Holder {
    return Holder_UEBACodec.instance.decode(ctx, new BaboonBinReader(bytes));
}

function readI32Le(data: Uint8Array, offset: number): number {
    return (data[offset]! & 0xFF)
        | ((data[offset + 1]! & 0xFF) << 8)
        | ((data[offset + 2]! & 0xFF) << 16)
        | ((data[offset + 3]! & 0xFF) << 24);
}

// ===== 1. Per-variant UEBA round-trip ==========================================================

describe("UEBA round-trip", () => {
    test("compact mode preserves all six variants + nested positions", () => {
        const original = buildUebaHolder();
        const bytes = encodeUebaBytes(original, BaboonCodecContext.Compact);
        const decoded = decodeUebaBytes(bytes, BaboonCodecContext.Compact);
        expect(decoded).toStrictEqual(original);
    });

    test("indexed mode preserves content (TS-13-D03 not applicable: TS Holder emits real indexed blocks)", () => {
        // Mirrors Java/Kotlin/Rust per-variant indexed-mode coverage. TS's Holder_UEBACodec emits a
        // header byte 0x01 then per-field (offset, length) i32 prefixes followed by the contiguous
        // payload buffer; decode reads the prefix block then linearly decodes the data.
        const original = buildUebaHolder();
        const bytes = encodeUebaBytes(original, BaboonCodecContext.Indexed);
        const decoded = decodeUebaBytes(bytes, BaboonCodecContext.Indexed);
        expect(decoded).toStrictEqual(original);
    });

    test("UEBA decode yields AnyOpaqueUeba branch with matching kind bytes", () => {
        const original = buildUebaHolder();
        const bytes = encodeUebaBytes(original, BaboonCodecContext.Compact);
        const decoded = decodeUebaBytes(bytes);
        expect(decoded.fAny.tag).toBe("Ueba");
        expect(decoded.fAny.meta.kind).toBe(0x07);
        expect(decoded.fDomainThis.meta.kind).toBe(0x03);
        expect(decoded.fDomainCurrent.meta.kind).toBe(0x01);
        expect(decoded.fUnderlying.meta.kind).toBe(0x06);
        expect(decoded.fThisUnderlying.meta.kind).toBe(0x02);
        expect(decoded.fCurrentUnderlying.meta.kind).toBe(0x00);
    });
});

// ===== 2. Per-variant JSON round-trip ==========================================================

describe("JSON round-trip", () => {
    test("preserves all six variants + nested positions", () => {
        const original = buildJsonNativeHolder();
        const json = Holder_JsonCodec.instance.encode(BaboonCodecContext.Compact, original);
        const decoded = Holder_JsonCodec.instance.decode(BaboonCodecContext.Compact, json);
        expect(decoded).toStrictEqual(original);
    });

    test("JSON decode yields AnyOpaqueJson branch with matching kind bytes", () => {
        const original = buildJsonNativeHolder();
        const json = Holder_JsonCodec.instance.encode(BaboonCodecContext.Compact, original);
        const decoded = Holder_JsonCodec.instance.decode(BaboonCodecContext.Compact, json);
        expect(decoded.fAny.tag).toBe("Json");
        expect(decoded.fAny.meta.kind).toBe(0x07);
        expect(decoded.fDomainThis.meta.kind).toBe(0x03);
        expect(decoded.fDomainCurrent.meta.kind).toBe(0x01);
        expect(decoded.fUnderlying.meta.kind).toBe(0x06);
        expect(decoded.fThisUnderlying.meta.kind).toBe(0x02);
        expect(decoded.fCurrentUnderlying.meta.kind).toBe(0x00);
    });
});

// ===== 3. Cross-format conversion via facade ===================================================

describe("Cross-format conversion via facade", () => {
    test("JSON-branch Holder encodes to UEBA bytes and round-trips identically", () => {
        // buildJsonHolderForCrossConvert uses AnyOpaqueJson branches for all fields with real Inner
        // JSON; encoding to UEBA forces jsonToUebaBytes per field. After decode the branches are
        // AnyOpaqueUeba. Re-encoding the now-UEBA-branched holder (no facade) must produce identical
        // bytes — proves the cross-converted bytes match a native UEBA encode of the same value.
        const facade = freshFacade();
        const ctxWithFacade = BaboonCodecContext.withFacade(false, facade);
        const original = buildJsonHolderForCrossConvert();
        const bytes = encodeUebaBytes(original, ctxWithFacade);
        const decoded = decodeUebaBytes(bytes);

        const rebytes = encodeUebaBytes(decoded, BaboonCodecContext.Compact);
        expect(Array.from(bytes)).toStrictEqual(Array.from(rebytes));
    });

    test("UEBA-branch Holder encodes to JSON envelope (variant kinds preserved)", () => {
        // buildUebaHolder uses AnyOpaqueUeba branches everywhere; encoding to JSON triggers
        // uebaToJson for each field. For untyped variants A/B/C the wire meta carries typeid;
        // we substitute typeid=INNER_TYPE so the registered Inner codec resolves and the bytes
        // deserialize as Inner. D variants resolve via static fallbacks emitted by codec gen.
        const facade = freshFacade();
        const ctxWithFacade = BaboonCodecContext.withFacade(false, facade);
        const innerBytes = innerToUebaBytes(SAMPLE_INNER);
        const base = buildUebaHolder();
        const crossable = new Holder(
            anyOpaqueUeba(metaA(INNER_TYPE), innerBytes),
            anyOpaqueUeba(metaB(INNER_TYPE), innerBytes),
            anyOpaqueUeba(metaC(INNER_TYPE), innerBytes),
            base.fUnderlying,
            base.fThisUnderlying,
            base.fCurrentUnderlying,
            anyOpaqueUeba(metaA(INNER_TYPE), innerBytes),
            base.fLst,
            new Map([["k1", anyOpaqueUeba(metaA(INNER_TYPE), innerBytes)]]),
        );
        const json = Holder_JsonCodec.instance.encode(ctxWithFacade, crossable);
        // Sanity-decode the JSON to ensure the envelope is well-formed.
        const decoded = Holder_JsonCodec.instance.decode(BaboonCodecContext.Compact, json);
        expect(decoded.fAny.tag).toBe("Json");
        expect(decoded.fAny.meta.kind).toBe(0x07);
        expect(decoded.fUnderlying.meta.kind).toBe(0x06);
        expect(decoded.fCurrentUnderlying.meta.kind).toBe(0x00); // D3: kind=0x00, statics filled
    });

    test("D3 isolated field cross-converts via static fallbacks (PR-06-D01 TS analog)", () => {
        // PR-06-D01 (TS analog) regression: D3 has all-None meta on wire; the codec generator
        // emits (currentDomain, currentVersion, underlyingFqid) as static fallbacks. Without these
        // the facade cannot resolve and cross-convert fails.
        const facade = freshFacade();
        const ctxWithFacade = BaboonCodecContext.withFacade(false, facade);
        const innerJson = innerToJson(SAMPLE_INNER);
        const base = buildUebaHolder();
        const mixed = new Holder(
            base.fAny,
            base.fDomainThis,
            base.fDomainCurrent,
            base.fUnderlying,
            base.fThisUnderlying,
            anyOpaqueJson(metaD3(), innerJson),
            base.fOpt,
            base.fLst,
            base.fMapValue,
        );
        // No throw on encode means jsonToUebaBytes succeeded for the D3 field (statics resolved).
        const bytes = encodeUebaBytes(mixed, ctxWithFacade);
        const decoded = decodeUebaBytes(bytes);
        expect(decoded.fCurrentUnderlying.meta.kind).toBe(0x00);
        expect(decoded.fCurrentUnderlying.tag).toBe("Ueba");
        if (decoded.fCurrentUnderlying.tag !== "Ueba") return;
        const blob = decoded.fCurrentUnderlying.bytes;
        const inner = Inner_UEBACodec.instance.decode(BaboonCodecContext.Compact, new BaboonBinReader(blob));
        expect(inner).toStrictEqual(SAMPLE_INNER);
    });
});

// ===== 4. facade.decodeAny end-to-end ==========================================================

describe("facade.decodeAny", () => {
    test("resolves UEBA Inner payload to typed Inner", () => {
        const facade = freshFacade();
        const meta: AnyMeta = createAnyMeta(0x07, DOMAIN_ID, VERSION_STR, INNER_TYPE);
        const opaque: AnyOpaque = anyOpaqueUeba(meta, innerToUebaBytes(SAMPLE_INNER));
        const result = facade.decodeAny(opaque);
        expect(result.tag).toBe("Right");
        if (result.tag !== "Right") return;
        expect(result.value).toStrictEqual(SAMPLE_INNER);
    });

    test("resolves JSON Inner payload to typed Inner", () => {
        const facade = freshFacade();
        const meta: AnyMeta = createAnyMeta(0x07, DOMAIN_ID, VERSION_STR, INNER_TYPE);
        const opaque: AnyOpaque = anyOpaqueJson(meta, innerToJson(SAMPLE_INNER));
        const result = facade.decodeAny(opaque);
        expect(result.tag).toBe("Right");
        if (result.tag !== "Right") return;
        expect(result.value).toStrictEqual(SAMPLE_INNER);
    });
});

// ===== 5. Forward-compat: trailing meta-extension bytes inside meta-length window ==============

describe("Forward-compat", () => {
    test("extra meta-extension bytes are skipped on UEBA decode", () => {
        // Encode a Holder normally, then surgically patch the FIRST any-field's meta-length to
        // claim 5 extra bytes, and splice 5 bytes into the meta block. The decoder must consume
        // the meta, observe the gap (anyMetaLen - bytesRead), skip them, and continue parsing.
        const original = buildUebaHolder();
        const bytes = encodeUebaBytes(original, BaboonCodecContext.Compact);

        // Layout of the first any-field on the wire (Compact, useIndices=false):
        // [1 byte header][i32 anyLength][i32 anyMetaLen][... metaBytes ...][... blob ...]
        const headerLen = 1;
        const anyLengthOffset = headerLen;
        const anyMetaLenOffset = headerLen + 4;
        const anyMetaStartOffset = headerLen + 4 + 4;
        const origAnyLength = readI32Le(bytes, anyLengthOffset);
        const origAnyMetaLen = readI32Le(bytes, anyMetaLenOffset);

        const extension = new Uint8Array([0x11, 0x22, 0x33, 0x44, 0x55]);
        const newAnyMetaLen = origAnyMetaLen + extension.length;
        const newAnyLength = origAnyLength + extension.length;

        const origMetaSlice = bytes.slice(anyMetaStartOffset, anyMetaStartOffset + origAnyMetaLen);
        const origBlobAndRest = bytes.slice(anyMetaStartOffset + origAnyMetaLen);

        const patched = new BaboonBinWriter();
        BinTools.writeByte(patched, bytes[0]!);
        BinTools.writeI32(patched, newAnyLength);
        BinTools.writeI32(patched, newAnyMetaLen);
        patched.writeBytes(origMetaSlice);
        patched.writeBytes(extension);
        patched.writeBytes(origBlobAndRest);
        const patchedBytes = patched.toBytes();

        const decoded = decodeUebaBytes(patchedBytes);
        expect(decoded).toStrictEqual(original);
    });
});

// ===== 6. Fail-fast: missing-facade cross-convert ==============================================

describe("Fail-fast missing-facade", () => {
    test("encoding JSON-branch any into UEBA without facade throws", () => {
        const base = buildUebaHolder();
        const mixed = new Holder(
            anyOpaqueJson(metaA(), { x: 1 }),
            base.fDomainThis,
            base.fDomainCurrent,
            base.fUnderlying,
            base.fThisUnderlying,
            base.fCurrentUnderlying,
            base.fOpt,
            base.fLst,
            base.fMapValue,
        );
        expect(() => {
            const writer = new BaboonBinWriter();
            Holder_UEBACodec.instance.encode(BaboonCodecContext.Compact, mixed, writer);
        }).toThrow(BaboonEncoderFailure);
    });

    test("encoding UEBA-branch any into JSON without facade throws", () => {
        const base = buildJsonNativeHolder();
        const mixed = new Holder(
            anyOpaqueUeba(metaA(), new Uint8Array([1, 2])),
            base.fDomainThis,
            base.fDomainCurrent,
            base.fUnderlying,
            base.fThisUnderlying,
            base.fCurrentUnderlying,
            base.fOpt,
            base.fLst,
            base.fMapValue,
        );
        expect(() => {
            Holder_JsonCodec.instance.encode(BaboonCodecContext.Compact, mixed);
        }).toThrow(BaboonEncoderFailure);
    });
});

// ===== 7. JSON envelope shape lock-in ==========================================================

describe("JSON envelope shape", () => {
    test("envelope carries $ak (+ optional $ad/$av/$at) and $c content key", () => {
        // Sanity: the JSON envelope produced by the codec embeds the AnyMeta keys ($ak, $ad?,
        // $av?, $at?) alongside the $c content key. Any change to the envelope that drops one of
        // these would break cross-language interop.
        const original = buildJsonNativeHolder();
        const token = Holder_JsonCodec.instance.encode(BaboonCodecContext.Compact, original) as Record<string, unknown>;

        // fAny variant A → all four meta keys + $c present.
        const anyField = token["fAny"] as Record<string, unknown>;
        expect(anyField[AnyMetaCodec.ANY_KIND_KEY]).toBe(0x07);
        expect(anyField[AnyMetaCodec.ANY_DOMAIN_KEY]).toBeDefined();
        expect(anyField[AnyMetaCodec.ANY_VERSION_KEY]).toBeDefined();
        expect(anyField[AnyMetaCodec.ANY_TYPEID_KEY]).toBeDefined();
        expect(anyField[AnyMetaCodec.ANY_CONTENT_KEY]).toBeDefined();

        // fCurrentUnderlying variant D3 → only $ak + $c (kind 0x00, no other meta on wire).
        const d3 = token["fCurrentUnderlying"] as Record<string, unknown>;
        expect(d3[AnyMetaCodec.ANY_KIND_KEY]).toBe(0x00);
        expect(d3[AnyMetaCodec.ANY_CONTENT_KEY]).toBeDefined();
        expect(d3[AnyMetaCodec.ANY_DOMAIN_KEY]).toBeUndefined();
        expect(d3[AnyMetaCodec.ANY_VERSION_KEY]).toBeUndefined();
        expect(d3[AnyMetaCodec.ANY_TYPEID_KEY]).toBeUndefined();

        // Sanity: the envelope keys appear exactly as documented (regression-proof key list).
        const expected = new Set([
            AnyMetaCodec.ANY_KIND_KEY,
            AnyMetaCodec.ANY_DOMAIN_KEY,
            AnyMetaCodec.ANY_VERSION_KEY,
            AnyMetaCodec.ANY_TYPEID_KEY,
            AnyMetaCodec.ANY_CONTENT_KEY,
        ]);
        const present = new Set(Object.keys(anyField));
        expect(present).toStrictEqual(expected);
    });
});
