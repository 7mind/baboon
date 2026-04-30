// Runtime helpers for the `id` toString / parseRepr machinery defined in
// docs/spec/identifier-repr.md. The TypeScript backend (PR-57d) uses these
// helpers from emitted code; conformance to the spec is the contract.
//
// Mirrors the JVM (Java/Kotlin), Rust, and Swift helpers in API and behaviour.
// Result type: `BaboonEither<L, R>` (matching project convention from
// `BaboonSharedRuntime.ts`). NOT `Result` (Rust) / `Either` (Scala). Errors
// are human-readable strings — same content as sibling backends — so test
// assertions can match on substrings across languages.

import { BaboonDateTimeOffset, BaboonDateTimeUtc, BaboonEither } from "./BaboonSharedRuntime";

// Defensive: numeric char-code constants rather than literal escapes so the
// emitted/copied source is robust against any future template-escape pass.
const BS = 0x5c; // '\\'
const HSH = 0x23; // '#'
const COL = 0x3a; // ':'
const OBR = 0x7b; // '{'
const CBR = 0x7d; // '}'

const _BS = String.fromCharCode(BS);
const _HSH = String.fromCharCode(HSH);
const _COL = String.fromCharCode(COL);
const _OBR = String.fromCharCode(OBR);
const _CBR = String.fromCharCode(CBR);

function leftS<T>(s: string): BaboonEither<string, T> {
    return { tag: "Left", value: s };
}

function rightS<T>(t: T): BaboonEither<string, T> {
    return { tag: "Right", value: t };
}

/** Backslash-escape the 5 metacharacters per spec §4.2. */
export function escapeStr(s: string): string {
    let out = "";
    for (let i = 0; i < s.length; i++) {
        const c = s.charCodeAt(i);
        if (c === BS || c === HSH || c === COL || c === OBR || c === CBR) {
            out += _BS;
        }
        out += s.charAt(i);
    }
    return out;
}

/** Lowercase hex, no separators, per spec §3 / §4.4. */
export function bytesToHex(bytes: Uint8Array): string {
    let out = "";
    for (let i = 0; i < bytes.length; i++) {
        const b = bytes[i]!;
        out += (b >> 4).toString(16) + (b & 0xf).toString(16);
    }
    return out;
}

/**
 * Render a `tsu` per spec §3: RFC 3339 UTC, milliseconds, trailing `Z`,
 * exactly 24 characters.
 */
export function tsuToString(dt: BaboonDateTimeUtc): string {
    const utcMs = dt.getTime();
    const d = new Date(utcMs);
    const y = String(d.getUTCFullYear()).padStart(4, "0");
    const M = String(d.getUTCMonth() + 1).padStart(2, "0");
    const dd = String(d.getUTCDate()).padStart(2, "0");
    const h = String(d.getUTCHours()).padStart(2, "0");
    const m = String(d.getUTCMinutes()).padStart(2, "0");
    const s = String(d.getUTCSeconds()).padStart(2, "0");
    const ms = String(d.getUTCMilliseconds()).padStart(3, "0");
    return `${y}-${M}-${dd}T${h}:${m}:${s}.${ms}Z`;
}

/**
 * Render a `tso` per spec §3: RFC 3339 with explicit `±HH:MM` offset,
 * milliseconds, exactly 29 characters. NEVER emits `Z` shorthand.
 */
export function tsoToString(dt: BaboonDateTimeOffset): string {
    const utcMs = dt.getTime();
    const localMs = utcMs + dt.offsetMs;
    const d = new Date(localMs);
    const y = String(d.getUTCFullYear()).padStart(4, "0");
    const M = String(d.getUTCMonth() + 1).padStart(2, "0");
    const dd = String(d.getUTCDate()).padStart(2, "0");
    const h = String(d.getUTCHours()).padStart(2, "0");
    const m = String(d.getUTCMinutes()).padStart(2, "0");
    const s = String(d.getUTCSeconds()).padStart(2, "0");
    const ms = String(d.getUTCMilliseconds()).padStart(3, "0");
    const off = dt.offsetMs;
    const absOff = Math.abs(off);
    const sign = off >= 0 ? "+" : "-";
    const oh = String(Math.floor(absOff / 3600000)).padStart(2, "0");
    const om = String(Math.floor((absOff % 3600000) / 60000)).padStart(2, "0");
    return `${y}-${M}-${dd}T${h}:${m}:${s}.${ms}${sign}${oh}:${om}`;
}

/**
 * Render an unsigned 64-bit value as decimal. The TS UEBA/JSON codecs store
 * u64 as `bigint`; `bigint.toString()` is unsigned-correct for the value range.
 */
export function u64ToString(v: bigint): string {
    return v.toString();
}

/** Render a `bit` per spec §3 — exact lowercase ASCII. */
export function bitToString(b: boolean): string {
    return b ? "true" : "false";
}

export function parseTsuRepr(s: string): BaboonEither<string, BaboonDateTimeUtc> {
    if (s.length !== 24) {
        return leftS(`tsu repr must be 24 chars, got ${s.length}`);
    }
    if (!s.endsWith("Z")) {
        return leftS(`tsu repr must end with 'Z', got: ${s}`);
    }
    // strict 24-char shape: yyyy-MM-ddTHH:mm:ss.SSSZ
    const re = /^(\d{4})-(\d{2})-(\d{2})T(\d{2}):(\d{2}):(\d{2})\.(\d{3})Z$/;
    const m = re.exec(s);
    if (!m) {
        return leftS(`could not parse tsu: ${s}`);
    }
    const ms = Date.UTC(
        parseInt(m[1]!, 10),
        parseInt(m[2]!, 10) - 1,
        parseInt(m[3]!, 10),
        parseInt(m[4]!, 10),
        parseInt(m[5]!, 10),
        parseInt(m[6]!, 10),
        parseInt(m[7]!, 10),
    );
    if (Number.isNaN(ms)) {
        return leftS(`could not parse tsu: ${s}`);
    }
    return rightS(BaboonDateTimeUtc.fromMillis(ms));
}

export function parseTsoRepr(s: string): BaboonEither<string, BaboonDateTimeOffset> {
    if (s.length !== 29) {
        return leftS(`tso repr must be 29 chars, got ${s.length}`);
    }
    const re = /^(\d{4})-(\d{2})-(\d{2})T(\d{2}):(\d{2}):(\d{2})\.(\d{3})([+-])(\d{2}):(\d{2})$/;
    const m = re.exec(s);
    if (!m) {
        return leftS(`could not parse tso: ${s}`);
    }
    const localMs = Date.UTC(
        parseInt(m[1]!, 10),
        parseInt(m[2]!, 10) - 1,
        parseInt(m[3]!, 10),
        parseInt(m[4]!, 10),
        parseInt(m[5]!, 10),
        parseInt(m[6]!, 10),
        parseInt(m[7]!, 10),
    );
    const sign = m[8] === "+" ? 1 : -1;
    const oh = parseInt(m[9]!, 10);
    const om = parseInt(m[10]!, 10);
    const offsetMs = sign * (oh * 3600000 + om * 60000);
    const utcMs = localMs - offsetMs;
    return rightS(BaboonDateTimeOffset.fromDateAndOffset(new Date(utcMs), offsetMs));
}

/** Decode `bytes` from lowercase hex. Empty string is legal (empty bytes). */
export function parseBytesHex(s: string): BaboonEither<string, Uint8Array> {
    if (s.length === 0) {
        return rightS(new Uint8Array(0));
    }
    if ((s.length & 1) !== 0) {
        return leftS(`odd-length hex sequence: ${s}`);
    }
    const out = new Uint8Array(s.length / 2);
    for (let i = 0; i < s.length; i += 2) {
        const hi = hexDigit(s.charCodeAt(i));
        const lo = hexDigit(s.charCodeAt(i + 1));
        if (hi < 0 || lo < 0) {
            return leftS(`non-lowercase or non-hex character in: ${s}`);
        }
        out[i / 2] = (hi << 4) | lo;
    }
    return rightS(out);
}

function hexDigit(b: number): number {
    if (b >= 0x30 && b <= 0x39) return b - 0x30; // 0..9
    if (b >= 0x61 && b <= 0x66) return 10 + (b - 0x61); // a..f (lowercase only)
    return -1;
}

export function parseBit(s: string): BaboonEither<string, boolean> {
    if (s === "true") return rightS(true);
    if (s === "false") return rightS(false);
    return leftS(`expected 'true' or 'false' but found '${s}'`);
}

/**
 * Lowercase canonical-form check for uid strings (spec §3 / §5.4):
 * `[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}`.
 */
export function isCanonicalUid(s: string): boolean {
    return /^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/.test(s);
}

/**
 * Cursor-based parser for parseRepr decoders. Schema-directed; the caller
 * (the emitted `<typeName>Codec.parseRepr`) drives the field sequence per
 * declared type and order.
 */
export class IdReprCursor {
    private readonly source: string;
    private pos: number;

    constructor(source: string) {
        this.source = source;
        this.pos = 0;
    }

    public position(): number {
        return this.pos;
    }

    public atEnd(): boolean {
        return this.pos >= this.source.length;
    }

    public expect(c: string): BaboonEither<string, null> {
        if (this.pos >= this.source.length) {
            return leftS(`expected '${c}' at ${this.pos} but reached end of input`);
        }
        const ch = this.source.charAt(this.pos);
        if (ch !== c) {
            return leftS(`expected '${c}' at ${this.pos} but found '${ch}'`);
        }
        this.pos += 1;
        return rightS(null);
    }

    public expectLiteral(lit: string): BaboonEither<string, null> {
        if (this.pos + lit.length > this.source.length) {
            return leftS(`expected literal '${lit}' at ${this.pos} but reached end of input`);
        }
        for (let i = 0; i < lit.length; i++) {
            if (this.source.charAt(this.pos + i) !== lit.charAt(i)) {
                return leftS(`expected literal '${lit}' at ${this.pos}`);
            }
        }
        this.pos += lit.length;
        return rightS(null);
    }

    /**
     * Read until the next bare metachar in `:#{}`. Backslash escapes are NOT
     * processed here — see readStrField. Used for primitive consumption
     * (numbers, uuids, hex bytes).
     */
    public readUntilStructural(): string {
        const start = this.pos;
        while (this.pos < this.source.length) {
            const c = this.source.charCodeAt(this.pos);
            if (c === COL || c === HSH || c === OBR || c === CBR) {
                break;
            }
            this.pos += 1;
        }
        return this.source.substring(start, this.pos);
    }

    /**
     * Consume exactly n characters (UTF-16 code units, but the lexemes are pure
     * ASCII so that coincides with chars). Used for tsu/tso fixed-width
     * lexemes per spec §5.4.
     */
    public readFixed(n: number): BaboonEither<string, string> {
        if (this.pos + n > this.source.length) {
            return leftS(
                `expected ${n} chars at ${this.pos} but only ${this.source.length - this.pos} remain`,
            );
        }
        const out = this.source.substring(this.pos, this.pos + n);
        this.pos += n;
        return rightS(out);
    }

    /**
     * Read a `str` field value with backslash-unescaping per spec §5.5.
     * Bare metachars are structural boundaries (consumer stops, but does not
     * advance past them). A `\X` for X in the 5 metachars yields literal X. A
     * bare `\` followed by anything else is a parse error. Trailing `\` is a
     * parse error.
     */
    public readStrField(): BaboonEither<string, string> {
        let out = "";
        while (this.pos < this.source.length) {
            const c = this.source.charCodeAt(this.pos);
            if (c === COL || c === HSH || c === OBR || c === CBR) {
                return rightS(out);
            }
            if (c === BS) {
                if (this.pos + 1 >= this.source.length) {
                    return leftS(`trailing backslash at ${this.pos}`);
                }
                const nxt = this.source.charCodeAt(this.pos + 1);
                if (nxt === BS || nxt === HSH || nxt === COL || nxt === OBR || nxt === CBR) {
                    out += this.source.charAt(this.pos + 1);
                    this.pos += 2;
                } else {
                    return leftS(`invalid escape at ${this.pos}`);
                }
            } else {
                out += this.source.charAt(this.pos);
                this.pos += 1;
            }
        }
        return rightS(out);
    }
}

/** Validate header of an identifier-repr: `<simpleName>:<version>#`. */
export function parseHeader(
    cursor: IdReprCursor,
    expectedSimpleName: string,
    expectedVersion: string,
): BaboonEither<string, null> {
    const nameLit = cursor.readUntilStructural();
    if (nameLit !== expectedSimpleName) {
        return leftS(`expected name '${expectedSimpleName}' but found '${nameLit}'`);
    }
    const r1 = cursor.expect(_COL);
    if (r1.tag === "Left") return r1;
    const verLit = cursor.readUntilStructural();
    if (verLit !== expectedVersion) {
        return leftS(`expected version '${expectedVersion}' but found '${verLit}'`);
    }
    return cursor.expect(_HSH);
}

/** Validate field-name segment: `<expectedFieldName>:`. */
export function parseFieldName(
    cursor: IdReprCursor,
    expectedFieldName: string,
): BaboonEither<string, null> {
    const name = cursor.readUntilStructural();
    if (name !== expectedFieldName) {
        return leftS(`expected field name '${expectedFieldName}' but found '${name}'`);
    }
    return cursor.expect(_COL);
}
