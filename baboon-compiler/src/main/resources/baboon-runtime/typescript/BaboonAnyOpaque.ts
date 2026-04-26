// `any`-feature surface types for TypeScript. Mirrors C# `AnyOpaque.cs`, Java `BaboonAnyOpaque.java`,
// Kotlin `BaboonAnyOpaque.kt`. Container holds:
//   * `AnyMeta` — locked four-byte/six-kind meta envelope, with construction-time invariant checks
//   * `AnyOpaque` — discriminated union (Ueba / Json) for `any`-typed payloads
//   * `AnyMetaCodec` — static helper for binary + JSON serialisation of the envelope
//
// Defects addressed:
//   PR-04-D01  reject reserved meta-kinds 0x04 / 0x05
//   PR-04-D02  `readJson` returns `BaboonEither` (user-facing) while `readBin` throws (wire-trust)
//   PR-05-D01  `readBinWithLength` reports bytes-consumed for forward-compat skip-trailer
//   PR-05-D08  `Uint8Array` `===`-equality is reference-based; `anyOpaqueEquals` does content compare

import {
    BaboonBinReader,
    BaboonBinWriter,
    BaboonCodecException,
    BaboonDecoderFailure,
    BaboonEither,
    BinTools,
} from "./BaboonSharedRuntime";

// --- AnyMeta -----------------------------------------------------------------------------------

/**
 * Locked four-byte / six-kind meta envelope for `any`-typed payloads.
 *
 * Invariants (enforced by `createAnyMeta`):
 *   - bit 2 (DOMAIN_BIT, 0x04) set <-> domain != null
 *   - bit 1 (VERSION_BIT, 0x02) set <-> version != null
 *   - bit 0 (TYPEID_BIT, 0x01) set <-> typeid != null
 *   - kind in {0x00, 0x01, 0x02, 0x03, 0x06, 0x07} — 0x04 / 0x05 reserved (PR-04-D01)
 *
 * The `AnyMeta` interface is `readonly` and frozen at construction so any value passed around the
 * runtime preserves the validated invariant. All construction must go through `createAnyMeta`.
 */
export interface AnyMeta {
    readonly kind: number;
    readonly domain: string | null;
    readonly version: string | null;
    readonly typeid: string | null;
}

const VALID_KINDS: ReadonlySet<number> = new Set([0x00, 0x01, 0x02, 0x03, 0x06, 0x07]);

const DOMAIN_BIT = 0x04;
const VERSION_BIT = 0x02;
const TYPEID_BIT = 0x01;

/**
 * Validating factory for `AnyMeta`. Throws `Error` (not `BaboonCodecException`) for invariant
 * violations — this is a programmer/wire-bug detector, not a decoding error path.
 */
export function createAnyMeta(
    kind: number,
    domain: string | null,
    version: string | null,
    typeid: string | null,
): AnyMeta {
    // PR-12-D01: TS `number` is a double; bit ops force i32. We range-check `kind` to a u8
    // explicitly so a non-byte value can't slip past the bit-mask checks below (e.g. -1 has all
    // bits set under `&`).
    if (!Number.isInteger(kind) || kind < 0 || kind > 0xFF) {
        throw new Error(`AnyMeta: kind must be a u8 byte 0x00..0xFF, got ${kind}`);
    }

    const domainBitSet = (kind & DOMAIN_BIT) !== 0;
    if (domainBitSet !== (domain !== null)) {
        throw new Error(
            `AnyMeta: domain presence (${domain !== null}) does not match kind 0x${(kind & 0xFF).toString(16)} bit 2`,
        );
    }
    const versionBitSet = (kind & VERSION_BIT) !== 0;
    if (versionBitSet !== (version !== null)) {
        throw new Error(
            `AnyMeta: version presence (${version !== null}) does not match kind 0x${(kind & 0xFF).toString(16)} bit 1`,
        );
    }
    const typeidBitSet = (kind & TYPEID_BIT) !== 0;
    if (typeidBitSet !== (typeid !== null)) {
        throw new Error(
            `AnyMeta: typeid presence (${typeid !== null}) does not match kind 0x${(kind & 0xFF).toString(16)} bit 0`,
        );
    }
    if (!VALID_KINDS.has(kind)) {
        throw new Error(
            `AnyMeta: reserved or invalid meta-kind byte: 0x${(kind & 0xFF).toString(16).padStart(2, "0")}`,
        );
    }
    return Object.freeze({ kind, domain, version, typeid });
}

export function anyMetaEquals(a: AnyMeta, b: AnyMeta): boolean {
    return a.kind === b.kind && a.domain === b.domain && a.version === b.version && a.typeid === b.typeid;
}

// --- AnyOpaque ---------------------------------------------------------------------------------

/**
 * Language-surface ADT for `any`-typed fields. Carries a meta envelope plus a payload in either
 * binary (UEBA) or JSON form. Modelled as a tag-discriminated union — TS idiom for sealed
 * hierarchies — so consumers can `switch (x.tag)` exhaustively.
 */
export type AnyOpaque =
    | { readonly tag: "Ueba"; readonly meta: AnyMeta; readonly bytes: Uint8Array }
    | { readonly tag: "Json"; readonly meta: AnyMeta; readonly json: unknown };

export function anyOpaqueUeba(meta: AnyMeta, bytes: Uint8Array): AnyOpaque {
    return { tag: "Ueba", meta, bytes };
}

export function anyOpaqueJson(meta: AnyMeta, json: unknown): AnyOpaque {
    return { tag: "Json", meta, json };
}

/**
 * Content equality for `AnyOpaque`. PR-05-D08: `Uint8Array` `===` is reference-identity, so a
 * naive structural check fails on bytewise-equal arrays from different sources. JSON content is
 * compared via `jsonContentEquals` (recursive deep equal — JSON has no shared semantics with TS
 * reference identity beyond primitives).
 */
export function anyOpaqueEquals(a: AnyOpaque, b: AnyOpaque): boolean {
    if (a.tag !== b.tag) return false;
    if (!anyMetaEquals(a.meta, b.meta)) return false;
    if (a.tag === "Ueba" && b.tag === "Ueba") {
        return uint8ArrayEquals(a.bytes, b.bytes);
    }
    if (a.tag === "Json" && b.tag === "Json") {
        return jsonContentEquals(a.json, b.json);
    }
    return false;
}

export function uint8ArrayEquals(a: Uint8Array, b: Uint8Array): boolean {
    if (a === b) return true;
    if (a.length !== b.length) return false;
    for (let i = 0; i < a.length; i++) {
        if (a[i] !== b[i]) return false;
    }
    return true;
}

function jsonContentEquals(a: unknown, b: unknown): boolean {
    if (a === b) return true;
    if (a === null || b === null) return a === b;
    if (typeof a !== typeof b) return false;
    if (Array.isArray(a)) {
        if (!Array.isArray(b) || a.length !== b.length) return false;
        for (let i = 0; i < a.length; i++) {
            if (!jsonContentEquals(a[i], b[i])) return false;
        }
        return true;
    }
    if (typeof a === "object") {
        const ao = a as Record<string, unknown>;
        const bo = b as Record<string, unknown>;
        const aKeys = Object.keys(ao);
        const bKeys = Object.keys(bo);
        if (aKeys.length !== bKeys.length) return false;
        for (const k of aKeys) {
            if (!Object.prototype.hasOwnProperty.call(bo, k)) return false;
            if (!jsonContentEquals(ao[k], bo[k])) return false;
        }
        return true;
    }
    return false;
}

// --- AnyMetaCodec ------------------------------------------------------------------------------

/**
 * Static helper for `AnyMeta` binary + JSON serialisation. Members keep PR-locked names: bit
 * masks (`DOMAIN_BIT` / `VERSION_BIT` / `TYPEID_BIT`) and JSON envelope keys (`$ak` / `$ad` /
 * `$av` / `$at` / `$c`) match every other runtime.
 *
 * `readBin` / `writeBin` trust the wire and throw on bad input. `readJson` returns
 * `BaboonEither<BaboonCodecException, AnyMeta>` per PR-04-D02 — JSON decode is user-facing and
 * threads errors instead of throwing.
 */
export const AnyMetaCodec = Object.freeze({
    DOMAIN_BIT,
    VERSION_BIT,
    TYPEID_BIT,

    ANY_KIND_KEY: "$ak",
    ANY_DOMAIN_KEY: "$ad",
    ANY_VERSION_KEY: "$av",
    ANY_TYPEID_KEY: "$at",
    ANY_CONTENT_KEY: "$c",

    VALID_KINDS,

    writeBin(meta: AnyMeta, writer: BaboonBinWriter): void {
        BinTools.writeByte(writer, meta.kind & 0xFF);
        if (meta.domain !== null) BinTools.writeString(writer, meta.domain);
        if (meta.version !== null) BinTools.writeString(writer, meta.version);
        if (meta.typeid !== null) BinTools.writeString(writer, meta.typeid);
    },

    readBin(reader: BaboonBinReader): AnyMeta {
        const kind = BinTools.readByte(reader);
        const domain = (kind & DOMAIN_BIT) !== 0 ? BinTools.readString(reader) : null;
        const version = (kind & VERSION_BIT) !== 0 ? BinTools.readString(reader) : null;
        const typeid = (kind & TYPEID_BIT) !== 0 ? BinTools.readString(reader) : null;
        return createAnyMeta(kind, domain, version, typeid);
    },

    /**
     * PR-05-D01: read meta and report bytes consumed. Callers that know the on-wire `meta-length`
     * window can skip any trailing bytes left in it — that's how the wire format keeps forward-
     * compat with future meta extensions. We compute the byte-count by snapshotting reader
     * positions before/after, mirroring Java's `CountingInputStream` outcome.
     */
    readBinWithLength(reader: BaboonBinReader): { meta: AnyMeta; bytesRead: number } {
        const before = reader.position();
        const meta = AnyMetaCodec.readBin(reader);
        const after = reader.position();
        return { meta, bytesRead: after - before };
    },

    /**
     * Always returns a plain JS object (never `null`/array). The JSON encoder envelope build
     * relies on this invariant — adding `$c` content into a non-object would silently lose the
     * key (PR-08-D06 analog).
     */
    writeJson(meta: AnyMeta): Record<string, unknown> {
        const obj: Record<string, unknown> = {};
        obj[AnyMetaCodec.ANY_KIND_KEY] = meta.kind & 0xFF;
        if (meta.domain !== null) obj[AnyMetaCodec.ANY_DOMAIN_KEY] = meta.domain;
        if (meta.version !== null) obj[AnyMetaCodec.ANY_VERSION_KEY] = meta.version;
        if (meta.typeid !== null) obj[AnyMetaCodec.ANY_TYPEID_KEY] = meta.typeid;
        return obj;
    },

    readJson(json: unknown): BaboonEither<BaboonCodecException, AnyMeta> {
        if (typeof json !== "object" || json === null || Array.isArray(json)) {
            return {
                tag: "Left",
                value: new BaboonDecoderFailure(
                    `AnyMetaCodec.readJson: expected JSON object, got ${json === null ? "null" : typeof json}`,
                ),
            };
        }
        const obj = json as Record<string, unknown>;

        const kindNode = obj[AnyMetaCodec.ANY_KIND_KEY];
        if (typeof kindNode !== "number" || !Number.isInteger(kindNode)) {
            return {
                tag: "Left",
                value: new BaboonDecoderFailure(
                    `AnyMetaCodec.readJson: missing or non-numeric '${AnyMetaCodec.ANY_KIND_KEY}' field`,
                ),
            };
        }
        const kind = kindNode & 0xFF;

        const domainResult = readOptString(obj, AnyMetaCodec.ANY_DOMAIN_KEY, kind, DOMAIN_BIT, "domain");
        if (domainResult.tag === "Left") return domainResult;

        const versionResult = readOptString(obj, AnyMetaCodec.ANY_VERSION_KEY, kind, VERSION_BIT, "version");
        if (versionResult.tag === "Left") return versionResult;

        const typeidResult = readOptString(obj, AnyMetaCodec.ANY_TYPEID_KEY, kind, TYPEID_BIT, "typeid");
        if (typeidResult.tag === "Left") return typeidResult;

        try {
            return {
                tag: "Right",
                value: createAnyMeta(kind, domainResult.value, versionResult.value, typeidResult.value),
            };
        } catch (e) {
            const msg = e instanceof Error ? e.message : String(e);
            return { tag: "Left", value: new BaboonDecoderFailure(`AnyMetaCodec.readJson: invalid meta: ${msg}`) };
        }
    },
});

function readOptString(
    obj: Record<string, unknown>,
    key: string,
    kind: number,
    bit: number,
    name: string,
): BaboonEither<BaboonCodecException, string | null> {
    const present = (kind & bit) !== 0;
    const token = obj[key];
    const value = typeof token === "string" ? token : null;

    if (present && value !== null) return { tag: "Right", value };
    if (!present && value === null) return { tag: "Right", value: null };
    if (present) {
        return {
            tag: "Left",
            value: new BaboonDecoderFailure(
                `AnyMetaCodec.readJson: kind 0x${(kind & 0xFF).toString(16)} requires '${key}' (${name}) but it is missing`,
            ),
        };
    }
    return {
        tag: "Left",
        value: new BaboonDecoderFailure(
            `AnyMetaCodec.readJson: kind 0x${(kind & 0xFF).toString(16)} forbids '${key}' (${name}) but it is present`,
        ),
    };
}
