

// --- Codec Context ---

// Type-only import keeps the runtime module graph acyclic: `BaboonCodecsFacade.ts` imports the
// `BaboonCodecContext` class from this file, so we cannot pull the runtime symbol back the other
// way. `import type` gets erased after type-checking.
import type { BaboonCodecsFacade } from "./BaboonCodecsFacade";

/**
 * Codec context: pairs a `useIndices` flag with an optional `BaboonCodecsFacade` reference. The
 * facade is threaded through generated codec calls so the `any`-feature cross-format conversion
 * (UEBA <-> JSON) can resolve codecs by `(domain, version, typeid)` from an `AnyMeta` envelope.
 *
 * Was an `enum` in earlier TS runtimes (matching Java pre-promotion). Promoted to a class so a
 * per-instance facade can be carried; the public-static `Default` / `Compact` / `Indexed`
 * singletons preserve call-site compatibility (`BaboonCodecContext.Compact` etc. still resolves
 * the same way; `ctx === BaboonCodecContext.Indexed` reference-equality still works).
 *
 * Mirrors PR 6.1 (Java) plumbing — Q6 option (a) in the design plan.
 */
export class BaboonCodecContext {
    private readonly _useIndices: boolean;
    private readonly _facade: BaboonCodecsFacade | undefined;

    private constructor(useIndices: boolean, facade: BaboonCodecsFacade | undefined) {
        this._useIndices = useIndices;
        this._facade = facade;
    }

    public get useIndices(): boolean {
        return this._useIndices;
    }

    public get facade(): BaboonCodecsFacade | undefined {
        return this._facade;
    }

    public static readonly Indexed: BaboonCodecContext = new BaboonCodecContext(true, undefined);
    public static readonly Compact: BaboonCodecContext = new BaboonCodecContext(false, undefined);
    public static readonly Default: BaboonCodecContext = BaboonCodecContext.Compact;

    public static withFacade(useIndices: boolean, facade: BaboonCodecsFacade): BaboonCodecContext {
        return new BaboonCodecContext(useIndices, facade);
    }
}

// --- Binary Writer ---

export class BaboonBinWriter {
    private buf: Uint8Array;
    private pos: number;

    constructor(initialCapacity: number = 256) {
        this.buf = new Uint8Array(initialCapacity);
        this.pos = 0;
    }

    private ensureCapacity(needed: number): void {
        if (this.pos + needed > this.buf.length) {
            let newCap = this.buf.length * 2;
            while (newCap < this.pos + needed) {
                newCap *= 2;
            }
            const newBuf = new Uint8Array(newCap);
            newBuf.set(this.buf.subarray(0, this.pos));
            this.buf = newBuf;
        }
    }

    writeByte(value: number): void {
        this.ensureCapacity(1);
        this.buf[this.pos++] = value & 0xFF;
    }

    writeBytes(data: Uint8Array): void {
        this.ensureCapacity(data.length);
        this.buf.set(data, this.pos);
        this.pos += data.length;
    }

    writeAll(data: Uint8Array): void {
        this.writeBytes(data);
    }

    position(): number {
        return this.pos;
    }

    toBytes(): Uint8Array {
        return this.buf.slice(0, this.pos);
    }
}

// --- Binary Reader ---

export class BaboonBinReader {
    private buf: Uint8Array;
    private view: DataView;
    private pos: number;

    constructor(data: Uint8Array) {
        this.buf = data;
        this.view = new DataView(data.buffer, data.byteOffset, data.byteLength);
        this.pos = 0;
    }

    readByte(): number {
        if (this.pos >= this.buf.length) {
            throw new BaboonDecoderFailure(`UEBA: unexpected end of input at position ${this.pos}`);
        }
        const v = this.buf[this.pos]!;
        this.pos += 1;
        return v;
    }

    readBytes(length: number): Uint8Array {
        const slice = this.buf.slice(this.pos, this.pos + length);
        this.pos += length;
        return new Uint8Array(slice);
    }

    readI8(): number {
        const v = this.view.getInt8(this.pos);
        this.pos += 1;
        return v;
    }

    readI16(): number {
        const v = this.view.getInt16(this.pos, true);
        this.pos += 2;
        return v;
    }

    readI32(): number {
        const v = this.view.getInt32(this.pos, true);
        this.pos += 4;
        return v;
    }

    readI64(): bigint {
        const v = this.view.getBigInt64(this.pos, true);
        this.pos += 8;
        return v;
    }

    readU8(): number {
        const v = this.view.getUint8(this.pos);
        this.pos += 1;
        return v;
    }

    readU16(): number {
        const v = this.view.getUint16(this.pos, true);
        this.pos += 2;
        return v;
    }

    readU32(): number {
        const v = this.view.getUint32(this.pos, true);
        this.pos += 4;
        return v;
    }

    readU64(): bigint {
        const v = this.view.getBigUint64(this.pos, true);
        this.pos += 8;
        return v;
    }

    readF32(): number {
        const v = this.view.getFloat32(this.pos, true);
        this.pos += 4;
        return v;
    }

    readF64(): number {
        const v = this.view.getFloat64(this.pos, true);
        this.pos += 8;
        return v;
    }

    position(): number {
        return this.pos;
    }

    remaining(): number {
        return this.buf.length - this.pos;
    }
}

// --- BaboonDecimal ---

export class BaboonDecimal {
    private readonly value: string;

    private constructor(value: string) {
        this.value = value;
    }

    static fromNumber(n: number): BaboonDecimal {
        return new BaboonDecimal(String(n));
    }

    static fromString(s: string): BaboonDecimal {
        return new BaboonDecimal(s);
    }

    toNumber(): number {
        return Number(this.value);
    }

    toString(): string {
        return this.value;
    }

    toJSON(): number {
        return this.toNumber();
    }
}

// --- BaboonDateTimeUtc ---

export class BaboonDateTimeUtc {
    readonly date: Date;

    private constructor(date: Date) {
        this.date = date;
    }

    static fromISO(s: string): BaboonDateTimeUtc {
        return new BaboonDateTimeUtc(new Date(s));
    }

    static fromDate(d: Date): BaboonDateTimeUtc {
        return new BaboonDateTimeUtc(d);
    }

    static fromMillis(ms: number): BaboonDateTimeUtc {
        return new BaboonDateTimeUtc(new Date(ms));
    }

    toISOString(): string {
        return this.date.toISOString().replace(/(\.\d{3})\d*Z$/, "$1Z");
    }

    getTime(): number {
        return this.date.getTime();
    }

    toJSON(): string {
        return this.toISOString();
    }
}

// --- BaboonDateTimeOffset ---

export class BaboonDateTimeOffset {
    readonly date: Date;
    readonly offsetMs: number;

    private constructor(date: Date, offsetMs: number) {
        this.date = date;
        this.offsetMs = offsetMs;
    }

    static fromISO(s: string): BaboonDateTimeOffset {
        const d = new Date(s);
        const offsetMs = BaboonDateTimeOffset.parseOffsetMs(s);
        return new BaboonDateTimeOffset(d, offsetMs);
    }

    static fromDateAndOffset(d: Date, offsetMs: number): BaboonDateTimeOffset {
        return new BaboonDateTimeOffset(d, offsetMs);
    }

    private static parseOffsetMs(s: string): number {
        if (s.endsWith("Z") || s.endsWith("z")) {
            return 0;
        }
        const match = s.match(/([+-])(\d{2}):(\d{2})$/);
        if (match) {
            const sign = match[1]! === "+" ? 1 : -1;
            const hours = parseInt(match[2]!, 10);
            const minutes = parseInt(match[3]!, 10);
            return sign * (hours * 3600 + minutes * 60) * 1000;
        }
        return 0;
    }

    toISOString(): string {
        const utcMs = this.date.getTime();
        const localMs = utcMs + this.offsetMs;
        const localDate = new Date(localMs);

        const yyyy = localDate.getUTCFullYear().toString().padStart(4, "0");
        const MM = (localDate.getUTCMonth() + 1).toString().padStart(2, "0");
        const dd = localDate.getUTCDate().toString().padStart(2, "0");
        const HH = localDate.getUTCHours().toString().padStart(2, "0");
        const mm = localDate.getUTCMinutes().toString().padStart(2, "0");
        const ss = localDate.getUTCSeconds().toString().padStart(2, "0");
        const ms = localDate.getUTCMilliseconds().toString().padStart(3, "0");

        // PR-28.3 (M28): tso always renders explicit ±HH:MM (UTC = "+00:00", NOT "Z").
        // tsu owns Z semantics; keeping tso ↔ tsu distinguishable on round-trip.
        const absOffset = Math.abs(this.offsetMs);
        const offsetSign = this.offsetMs >= 0 ? "+" : "-";
        const offsetHours = Math.floor(absOffset / 3600000).toString().padStart(2, "0");
        const offsetMinutes = Math.floor((absOffset % 3600000) / 60000).toString().padStart(2, "0");

        return `${yyyy}-${MM}-${dd}T${HH}:${mm}:${ss}.${ms}${offsetSign}${offsetHours}:${offsetMinutes}`;
    }

    getTime(): number {
        return this.date.getTime();
    }

    toJSON(): string {
        return this.toISOString();
    }
}

// --- BinTools ---

const DOTNET_EPOCH_OFFSET_MS = 62135596800000n;

const textEncoder = new TextEncoder();
const textDecoder = new TextDecoder();

export class BinTools {
    // --- Writers ---

    static writeBool(writer: BaboonBinWriter, value: boolean): void {
        writer.writeByte(value ? 1 : 0);
    }

    static writeByte(writer: BaboonBinWriter, value: number): void {
        writer.writeByte(value & 0xFF);
    }

    static writeI8(writer: BaboonBinWriter, value: number): void {
        const buf = new ArrayBuffer(1);
        new DataView(buf).setInt8(0, value);
        writer.writeBytes(new Uint8Array(buf));
    }

    static writeI16(writer: BaboonBinWriter, value: number): void {
        const buf = new ArrayBuffer(2);
        new DataView(buf).setInt16(0, value, true);
        writer.writeBytes(new Uint8Array(buf));
    }

    static writeI32(writer: BaboonBinWriter, value: number): void {
        const buf = new ArrayBuffer(4);
        new DataView(buf).setInt32(0, value, true);
        writer.writeBytes(new Uint8Array(buf));
    }

    static writeI64(writer: BaboonBinWriter, value: bigint): void {
        const buf = new ArrayBuffer(8);
        new DataView(buf).setBigInt64(0, value, true);
        writer.writeBytes(new Uint8Array(buf));
    }

    static writeU8(writer: BaboonBinWriter, value: number): void {
        writer.writeByte(value & 0xFF);
    }

    static writeU16(writer: BaboonBinWriter, value: number): void {
        const buf = new ArrayBuffer(2);
        new DataView(buf).setUint16(0, value, true);
        writer.writeBytes(new Uint8Array(buf));
    }

    static writeU32(writer: BaboonBinWriter, value: number): void {
        const buf = new ArrayBuffer(4);
        new DataView(buf).setUint32(0, value, true);
        writer.writeBytes(new Uint8Array(buf));
    }

    static writeU64(writer: BaboonBinWriter, value: bigint): void {
        const buf = new ArrayBuffer(8);
        new DataView(buf).setBigUint64(0, value, true);
        writer.writeBytes(new Uint8Array(buf));
    }

    static writeF32(writer: BaboonBinWriter, value: number): void {
        const buf = new ArrayBuffer(4);
        new DataView(buf).setFloat32(0, value, true);
        writer.writeBytes(new Uint8Array(buf));
    }

    static writeF64(writer: BaboonBinWriter, value: number): void {
        const buf = new ArrayBuffer(8);
        new DataView(buf).setFloat64(0, value, true);
        writer.writeBytes(new Uint8Array(buf));
    }

    static writeDecimal(writer: BaboonBinWriter, value: BaboonDecimal): void {
        // .NET decimal format: lo (i32), mid (i32), hi (i32), flags (i32)
        let str = value.toString();
        const isNeg = str.startsWith("-");
        if (isNeg) {
            str = str.substring(1);
        }

        let scale = 0;
        const dotIdx = str.indexOf(".");
        if (dotIdx >= 0) {
            scale = str.length - dotIdx - 1;
        }

        const mantissa = BigInt(str.replace(".", ""));
        const lo = Number(mantissa & 0xFFFFFFFFn);
        const mid = Number((mantissa >> 32n) & 0xFFFFFFFFn);
        const hi = Number((mantissa >> 64n) & 0xFFFFFFFFn);

        const sign = isNeg ? 0x80000000 : 0;
        const flags = (sign | (scale << 16)) >>> 0;

        BinTools.writeI32(writer, lo | 0);
        BinTools.writeI32(writer, mid | 0);
        BinTools.writeI32(writer, hi | 0);
        BinTools.writeI32(writer, flags | 0);
    }

    static writeString(writer: BaboonBinWriter, value: string): void {
        const bytes = textEncoder.encode(value);
        // VLQ (7-bit variable-length) encoding for length
        let len = bytes.length;
        do {
            let currentByte = len & 0x7F;
            len >>>= 7;
            if (len !== 0) {
                currentByte |= 0x80;
            }
            writer.writeByte(currentByte);
        } while (len !== 0);
        writer.writeBytes(bytes);
    }

    static writeBytes(writer: BaboonBinWriter, value: Uint8Array): void {
        BinTools.writeI32(writer, value.length);
        writer.writeBytes(value);
    }

    static writeUuid(writer: BaboonBinWriter, value: string): void {
        // .NET GUID mixed-endian format
        const hex = value.replace(/-/g, "");
        const bytes = new Uint8Array(16);
        for (let i = 0; i < 16; i++) {
            bytes[i] = parseInt(hex.substring(i * 2, i * 2 + 2), 16);
        }
        // Swap for .NET GUID mixed-endian: reverse 0-3, 4-5, 6-7
        const guid = new Uint8Array(16);
        guid[0] = bytes[3]!; guid[1] = bytes[2]!; guid[2] = bytes[1]!; guid[3] = bytes[0]!;
        guid[4] = bytes[5]!; guid[5] = bytes[4]!;
        guid[6] = bytes[7]!; guid[7] = bytes[6]!;
        for (let i = 8; i < 16; i++) guid[i] = bytes[i]!;
        writer.writeBytes(guid);
    }

    static writeTimestampUtc(writer: BaboonBinWriter, value: BaboonDateTimeUtc): void {
        const epochMs = BigInt(value.getTime());
        const dotnetUtcTicksMs = epochMs + DOTNET_EPOCH_OFFSET_MS;
        const offsetMs = 0n;
        const kind = 1; // UTC
        BinTools.writeI64(writer, dotnetUtcTicksMs);
        BinTools.writeI64(writer, offsetMs);
        BinTools.writeByte(writer, kind);
    }

    static writeTimestampOffset(writer: BaboonBinWriter, value: BaboonDateTimeOffset): void {
        const epochMs = BigInt(value.getTime());
        const dotnetUtcTicksMs = epochMs + DOTNET_EPOCH_OFFSET_MS;
        const offsetMsBigInt = BigInt(value.offsetMs);
        const dotnetLocalTicksMs = dotnetUtcTicksMs + offsetMsBigInt;
        const kind = value.offsetMs === 0 ? 1 : 0;
        BinTools.writeI64(writer, dotnetLocalTicksMs);
        BinTools.writeI64(writer, offsetMsBigInt);
        BinTools.writeByte(writer, kind);
    }

    // --- Readers ---

    static readBool(reader: BaboonBinReader): boolean {
        return reader.readByte() !== 0;
    }

    static readByte(reader: BaboonBinReader): number {
        return reader.readByte();
    }

    static readI8(reader: BaboonBinReader): number {
        return reader.readI8();
    }

    static readI16(reader: BaboonBinReader): number {
        return reader.readI16();
    }

    static readI32(reader: BaboonBinReader): number {
        return reader.readI32();
    }

    static readI64(reader: BaboonBinReader): bigint {
        return reader.readI64();
    }

    static readU8(reader: BaboonBinReader): number {
        return reader.readU8();
    }

    static readU16(reader: BaboonBinReader): number {
        return reader.readU16();
    }

    static readU32(reader: BaboonBinReader): number {
        return reader.readU32();
    }

    static readU64(reader: BaboonBinReader): bigint {
        return reader.readU64();
    }

    static readF32(reader: BaboonBinReader): number {
        return reader.readF32();
    }

    static readF64(reader: BaboonBinReader): number {
        return reader.readF64();
    }

    static readDecimal(reader: BaboonBinReader): BaboonDecimal {
        // .NET decimal format: lo (i32), mid (i32), hi (i32), flags (i32)
        const lo = BigInt(reader.readI32()) & 0xFFFFFFFFn;
        const mid = BigInt(reader.readI32()) & 0xFFFFFFFFn;
        const hi = BigInt(reader.readI32()) & 0xFFFFFFFFn;
        const flags = reader.readI32() >>> 0;

        const scale = (flags >> 16) & 0xFF;
        const isNeg = (flags & 0x80000000) !== 0;

        const mantissa = lo | (mid << 32n) | (hi << 64n);
        let str = mantissa.toString();

        if (scale > 0) {
            while (str.length <= scale) {
                str = "0" + str;
            }
            str = str.slice(0, str.length - scale) + "." + str.slice(str.length - scale);
            // Remove trailing zeros after decimal point
            str = str.replace(/\.?0+$/, "") || "0";
        }

        if (isNeg && str !== "0") {
            str = "-" + str;
        }

        return BaboonDecimal.fromString(str);
    }

    static readString(reader: BaboonBinReader): string {
        // VLQ (7-bit variable-length) decoding for length
        let length = 0;
        let shift = 0;
        while (true) {
            const byteRead = reader.readByte();
            length |= (byteRead & 0x7F) << shift;
            shift += 7;
            if ((byteRead & 0x80) === 0) break;
        }
        const bytes = reader.readBytes(length);
        return textDecoder.decode(bytes);
    }

    static readBytes(reader: BaboonBinReader): Uint8Array {
        const len = reader.readI32();
        return reader.readBytes(len);
    }

    static readUuid(reader: BaboonBinReader): string {
        // .NET GUID mixed-endian format
        const raw = reader.readBytes(16);
        const bytes = new Uint8Array(16);
        // Reverse .NET swaps: 0-3, 4-5, 6-7
        bytes[0] = raw[3]!; bytes[1] = raw[2]!; bytes[2] = raw[1]!; bytes[3] = raw[0]!;
        bytes[4] = raw[5]!; bytes[5] = raw[4]!;
        bytes[6] = raw[7]!; bytes[7] = raw[6]!;
        for (let i = 8; i < 16; i++) bytes[i] = raw[i]!;

        const hex = Array.from(bytes).map(b => b.toString(16).padStart(2, "0")).join("");
        return `${hex.slice(0, 8)}-${hex.slice(8, 12)}-${hex.slice(12, 16)}-${hex.slice(16, 20)}-${hex.slice(20)}`;
    }

    static readTimestampUtc(reader: BaboonBinReader): BaboonDateTimeUtc {
        const dotnetLocalTicksMs = reader.readI64();
        const offsetMs = reader.readI64();
        const _kind = reader.readByte();

        const dotnetUtcTicksMs = dotnetLocalTicksMs - offsetMs;
        const epochMs = dotnetUtcTicksMs - DOTNET_EPOCH_OFFSET_MS;

        return BaboonDateTimeUtc.fromMillis(Number(epochMs));
    }

    static readTimestampOffset(reader: BaboonBinReader): BaboonDateTimeOffset {
        const dotnetLocalTicksMs = reader.readI64();
        const offsetMsBigInt = reader.readI64();
        const _kind = reader.readByte();

        const dotnetUtcTicksMs = dotnetLocalTicksMs - offsetMsBigInt;
        const epochMs = dotnetUtcTicksMs - DOTNET_EPOCH_OFFSET_MS;
        const offsetMs = Number(offsetMsBigInt);

        return BaboonDateTimeOffset.fromDateAndOffset(new Date(Number(epochMs)), offsetMs);
    }

    // --- Hex encoding ---

    static hexEncode(bytes: Uint8Array): string {
        return Array.from(bytes).map(b => b.toString(16).padStart(2, "0").toUpperCase()).join("");
    }

    static hexDecode(hex: string): Uint8Array {
        if (hex.length % 2 !== 0) {
            throw new Error("Hex string has odd length: " + hex.length);
        }
        const bytes = new Uint8Array(hex.length / 2);
        for (let i = 0; i < hex.length; i += 2) {
            bytes[i / 2] = parseInt(hex.substring(i, i + 2), 16);
        }
        return bytes;
    }
}

// --- Metadata interfaces ---

export interface BaboonGenerated {
    baboonDomainVersion(): string
    baboonDomainIdentifier(): string
    baboonSameInVersions(): string[]
    baboonTypeIdentifier(): string
}

export interface BaboonGeneratedLatest extends BaboonGenerated {
}

export interface BaboonAdtMemberMeta extends BaboonGenerated {
    // PR-25.8 / PR-22-D02: declared as a method (not a property) to match the generated emission.
    // `TsDomainTreeTools.adtMeta` emits `public baboonAdtTypeIdentifier() { return ... }`. The
    // interface previously declared a `readonly ... : string` property which structurally rejected
    // the method signature, so generated ADT branches did not satisfy the interface and
    // `BaboonTypeMeta.from`'s `isAdtMember` typeguard always failed silently.
    baboonAdtTypeIdentifier(): string;
}

// --- Service wiring types ---

export interface BaboonMethodId {
    readonly serviceName: string;
    readonly methodName: string;
}

export type BaboonWiringError =
    | { readonly tag: 'NoMatchingMethod'; readonly method: BaboonMethodId }
    | { readonly tag: 'NoMatchingService'; readonly method: BaboonMethodId }
    | { readonly tag: 'DuplicateService'; readonly serviceName: string }
    | { readonly tag: 'DecoderFailed'; readonly method: BaboonMethodId; readonly error: unknown }
    | { readonly tag: 'EncoderFailed'; readonly method: BaboonMethodId; readonly error: unknown }
    | { readonly tag: 'CallFailed'; readonly method: BaboonMethodId; readonly domainError: unknown };

export class BaboonWiringException extends Error {
    readonly error: BaboonWiringError;
    constructor(error: BaboonWiringError) {
        super(JSON.stringify(error));
        this.error = error;
    }
}

// --- Service muxers ---
//
// Cross-domain composable dispatch. A muxer holds a set of services from any
// model(s) and routes an `(method, data, ctx)` call to the right one by
// `method.serviceName`. The R type parameter encodes the return shape so the
// same class supports both sync and async generated services — pass
// `JsonMuxer<Promise<string>>` (default) for `--ts-async-services=true`
// generated code, or `JsonMuxer<string>` for sync code. The per-service
// wrapper classes emitted alongside `invokeJson_X` / `invokeUeba_X` carry the
// matching parameterisation.

export interface IBaboonJsonService<R = Promise<string>> {
    readonly serviceName: string;
    invoke(method: BaboonMethodId, data: string, ctx: BaboonCodecContext): R;
}

export interface IBaboonUebaService<R = Promise<Uint8Array>> {
    readonly serviceName: string;
    invoke(method: BaboonMethodId, data: Uint8Array, ctx: BaboonCodecContext): R;
}

export class JsonMuxer<R = Promise<string>> {
    private readonly table = new Map<string, IBaboonJsonService<R>>();
    constructor(...services: IBaboonJsonService<R>[]) {
        for (const s of services) this.register(s);
    }
    register(service: IBaboonJsonService<R>): void {
        if (this.table.has(service.serviceName)) {
            throw new BaboonWiringException({ tag: 'DuplicateService', serviceName: service.serviceName });
        }
        this.table.set(service.serviceName, service);
    }
    invoke(method: BaboonMethodId, data: string, ctx: BaboonCodecContext): R {
        const service = this.table.get(method.serviceName);
        if (service === undefined) {
            throw new BaboonWiringException({ tag: 'NoMatchingService', method });
        }
        return service.invoke(method, data, ctx);
    }
    serviceNames(): readonly string[] {
        return Array.from(this.table.keys());
    }
}

export class UebaMuxer<R = Promise<Uint8Array>> {
    private readonly table = new Map<string, IBaboonUebaService<R>>();
    constructor(...services: IBaboonUebaService<R>[]) {
        for (const s of services) this.register(s);
    }
    register(service: IBaboonUebaService<R>): void {
        if (this.table.has(service.serviceName)) {
            throw new BaboonWiringException({ tag: 'DuplicateService', serviceName: service.serviceName });
        }
        this.table.set(service.serviceName, service);
    }
    invoke(method: BaboonMethodId, data: Uint8Array, ctx: BaboonCodecContext): R {
        const service = this.table.get(method.serviceName);
        if (service === undefined) {
            throw new BaboonWiringException({ tag: 'NoMatchingService', method });
        }
        return service.invoke(method, data, ctx);
    }
    serviceNames(): readonly string[] {
        return Array.from(this.table.keys());
    }
}

// Context-carrying variants, emitted when a service.context mode (`abstract`
// or `type`) is active. The abstract/concrete service context `Ctx` is
// supplied per-invocation (alongside the codec context) rather than baked into
// the wrapper, so callers thread a fresh context through each dispatch. The
// context-free interfaces above are left untouched so `--service-context-mode
// none` output (and the service-acceptance matrix) is byte-identical.
export interface IBaboonJsonServiceCtx<Ctx, R = Promise<string>> {
    readonly serviceName: string;
    invoke(method: BaboonMethodId, data: string, ctx: Ctx, codecCtx: BaboonCodecContext): R;
}

export interface IBaboonUebaServiceCtx<Ctx, R = Promise<Uint8Array>> {
    readonly serviceName: string;
    invoke(method: BaboonMethodId, data: Uint8Array, ctx: Ctx, codecCtx: BaboonCodecContext): R;
}

export class JsonMuxerCtx<Ctx, R = Promise<string>> {
    private readonly table = new Map<string, IBaboonJsonServiceCtx<Ctx, R>>();
    constructor(...services: IBaboonJsonServiceCtx<Ctx, R>[]) {
        for (const s of services) this.register(s);
    }
    register(service: IBaboonJsonServiceCtx<Ctx, R>): void {
        if (this.table.has(service.serviceName)) {
            throw new BaboonWiringException({ tag: 'DuplicateService', serviceName: service.serviceName });
        }
        this.table.set(service.serviceName, service);
    }
    invoke(method: BaboonMethodId, data: string, ctx: Ctx, codecCtx: BaboonCodecContext): R {
        const service = this.table.get(method.serviceName);
        if (service === undefined) {
            throw new BaboonWiringException({ tag: 'NoMatchingService', method });
        }
        return service.invoke(method, data, ctx, codecCtx);
    }
    serviceNames(): readonly string[] {
        return Array.from(this.table.keys());
    }
}

export class UebaMuxerCtx<Ctx, R = Promise<Uint8Array>> {
    private readonly table = new Map<string, IBaboonUebaServiceCtx<Ctx, R>>();
    constructor(...services: IBaboonUebaServiceCtx<Ctx, R>[]) {
        for (const s of services) this.register(s);
    }
    register(service: IBaboonUebaServiceCtx<Ctx, R>): void {
        if (this.table.has(service.serviceName)) {
            throw new BaboonWiringException({ tag: 'DuplicateService', serviceName: service.serviceName });
        }
        this.table.set(service.serviceName, service);
    }
    invoke(method: BaboonMethodId, data: Uint8Array, ctx: Ctx, codecCtx: BaboonCodecContext): R {
        const service = this.table.get(method.serviceName);
        if (service === undefined) {
            throw new BaboonWiringException({ tag: 'NoMatchingService', method });
        }
        return service.invoke(method, data, ctx, codecCtx);
    }
    serviceNames(): readonly string[] {
        return Array.from(this.table.keys());
    }
}

export type BaboonEither<L, R> =
    | { readonly tag: 'Left'; readonly value: L }
    | { readonly tag: 'Right'; readonly value: R };

// --- Conversion interfaces ---

export interface AbstractConversion<From, To> {
    convert(from: From): To;
}

export interface AbstractBaboonConversions {
    versionsFrom(): string[];
    versionTo(): string;
}

export class Lazy<T> {
    private valueRef: T | undefined;
    private initialized = false;

    constructor(private readonly initializer: () => T) {
    }

    get value(): T {
        if (!this.initialized) {
            this.valueRef = this.initializer();
            this.initialized = true;
        }
        return this.valueRef!;
    }

    get isValueCreated(): boolean {
        return this.initialized;
    }
}

// --- BaboonException ---

export class BaboonException extends Error {
    constructor(message: string, options?: { cause?: unknown }) {
        super(message);
        this.name = "BaboonException";
        if (options && options.cause !== undefined) {
            (this as { cause?: unknown }).cause = options.cause;
        }
    }
}

// --- BaboonCodecException ---

/**
 * Typed codec error hierarchy. Mirrors C# / Java sealed exception hierarchies. Subclasses are
 * brand-tagged via `kind` so consumers can `switch` on a discriminated string instead of relying
 * on `instanceof` (which is also fine, both forms work).
 */
export abstract class BaboonCodecException extends BaboonException {
    public abstract readonly kind:
        | "EncoderFailure"
        | "DecoderFailure"
        | "ConverterFailure"
        | "CodecNotFound"
        | "ConversionNotFound";

    constructor(message: string, options?: { cause?: unknown }) {
        super(message, options);
        this.name = "BaboonCodecException";
    }
}

export class BaboonEncoderFailure extends BaboonCodecException {
    public readonly kind = "EncoderFailure" as const;
    constructor(message: string, options?: { cause?: unknown }) {
        super(message, options);
        this.name = "BaboonEncoderFailure";
    }
}

export class BaboonDecoderFailure extends BaboonCodecException {
    public readonly kind = "DecoderFailure" as const;
    constructor(message: string, options?: { cause?: unknown }) {
        super(message, options);
        this.name = "BaboonDecoderFailure";
    }
}

export class BaboonConverterFailure extends BaboonCodecException {
    public readonly kind = "ConverterFailure" as const;
    constructor(message: string, options?: { cause?: unknown }) {
        super(message, options);
        this.name = "BaboonConverterFailure";
    }
}

export class BaboonCodecNotFound extends BaboonCodecException {
    public readonly kind = "CodecNotFound" as const;
    constructor(message: string) {
        super(message);
        this.name = "BaboonCodecNotFound";
    }
}

export class BaboonConversionNotFound extends BaboonCodecException {
    public readonly kind = "ConversionNotFound" as const;
    constructor(message: string) {
        super(message);
        this.name = "BaboonConversionNotFound";
    }
}

// --- BaboonVersion ---

/**
 * Parsed semver-shaped version (`major.minor.patch`). Named `BaboonVersion` (not `Version`) to
 * avoid clashes with global / DOM `Version` ambient types — same defensive renaming as the
 * other runtimes (PR-08-D05 lesson).
 */
export class BaboonVersion {
    public readonly major: number;
    public readonly minor: number;
    public readonly patch: number;

    constructor(major: number, minor: number, patch: number) {
        this.major = major;
        this.minor = minor;
        this.patch = patch;
    }

    public compareTo(other: BaboonVersion): number {
        // PR-12-D01: TS `number` is a double; differences here are integer comparisons of
        // already-validated parsed components. Use explicit `<`/`>` rather than `a - b` to
        // sidestep any float subtraction surprises.
        if (this.major < other.major) return -1;
        if (this.major > other.major) return 1;
        if (this.minor < other.minor) return -1;
        if (this.minor > other.minor) return 1;
        if (this.patch < other.patch) return -1;
        if (this.patch > other.patch) return 1;
        return 0;
    }

    public equals(other: BaboonVersion): boolean {
        return this.compareTo(other) === 0;
    }

    public toString(): string {
        return `${this.major}.${this.minor}.${this.patch}`;
    }

    public static from(version: string): BaboonVersion {
        const chunks = version.split(".");
        if (chunks.length < 3) {
            throw new BaboonException(`Expected to have version in format x.y.z, got ${version}`);
        }
        return new BaboonVersion(
            BaboonVersion.parse(chunks[0]!, "major", version),
            BaboonVersion.parse(chunks[1]!, "minor", version),
            BaboonVersion.parse(chunks[2]!, "patch", version),
        );
    }

    private static parse(s: string, slot: string, version: string): number {
        const trimmed = s.trim();
        if (!/^-?\d+$/.test(trimmed)) {
            throw new BaboonException(
                `Expected to have version in format x.y.z, got ${version}. Invalid ${slot} value.`,
            );
        }
        return parseInt(trimmed, 10);
    }
}

// --- BaboonDomainVersion ---

export class BaboonDomainVersion {
    public readonly domainIdentifier: string;
    public readonly domainVersion: string;

    constructor(domainIdentifier: string, domainVersion: string) {
        this.domainIdentifier = domainIdentifier;
        this.domainVersion = domainVersion;
    }

    public version(): BaboonVersion {
        return BaboonVersion.from(this.domainVersion);
    }

    public equals(other: BaboonDomainVersion): boolean {
        return this.domainIdentifier === other.domainIdentifier && this.domainVersion === other.domainVersion;
    }

    public key(): string {
        return `${this.domainIdentifier}@${this.domainVersion}`;
    }

    public toString(): string {
        return `${this.domainIdentifier}:${this.domainVersion}`;
    }
}

// --- BaboonTypeMeta ---

/**
 * Per-payload type-meta envelope written ahead of UEBA / JSON-encoded values. Mirrors C#/Java
 * `BaboonTypeMeta`. `domainVersionMinCompat` defaults to `domainVersion` when wire-omitted.
 *
 * `readMeta` (binary + JSON) returns `undefined` for unrecognised meta-versions per PR-08-D01:
 * an unknown `$mv` reads as "I cannot decode this" not "this is malformed", so the caller can
 * skip / forward-compat rather than throw.
 */
export class BaboonTypeMeta {
    public readonly metaVersion: number;
    public readonly domainIdentifier: string;
    public readonly domainVersion: string;
    public readonly domainVersionMinCompat: string;
    public readonly typeIdentifier: string;

    constructor(
        metaVersion: number,
        domainIdentifier: string,
        domainVersion: string,
        domainVersionMinCompat: string,
        typeIdentifier: string,
    ) {
        this.metaVersion = metaVersion;
        this.domainIdentifier = domainIdentifier;
        this.domainVersion = domainVersion;
        this.domainVersionMinCompat = domainVersionMinCompat;
        this.typeIdentifier = typeIdentifier;
    }

    public versionRef(): BaboonDomainVersion {
        return new BaboonDomainVersion(this.domainIdentifier, this.domainVersion);
    }

    // `versionMinCompat()` treats empty-string `domainVersionMinCompat` as absent
    // for cross-runtime parity. Scala/Kotlin/KMP/Java/Swift/Dart runtimes all
    // normalize empty/blank to None|null; TypeScript follows suit. Wire decoders
    // substitute `domainVersionMinCompat = domainVersion` when the flag byte is 0,
    // so the only way to observe `""` is from a non-conformant peer that wrote
    // it explicitly — and that peer would not have meant it as a distinct value.
    public versionMinCompat(): BaboonDomainVersion | undefined {
        if (!this.domainVersionMinCompat || this.domainVersionMinCompat === this.domainVersion) {
            return undefined;
        }
        return new BaboonDomainVersion(this.domainIdentifier, this.domainVersionMinCompat);
    }

    public writeBin(writer: BaboonBinWriter): void {
        BaboonTypeMetaCodec.writeBin(this, writer);
    }

    public writeJson(): Record<string, unknown> {
        return BaboonTypeMetaCodec.writeJson(this);
    }

    /**
     * Reflectively pulls the Baboon metadata fields off a generated value. Generated TS classes
     * expose `baboonDomainIdentifier()`, `baboonDomainVersion()`, `baboonTypeIdentifier()`,
     * `baboonSameInVersions()` and (for ADT branches) `baboonAdtTypeIdentifier()`. When the
     * caller's declared static type is the ADT trait — flagged via `useAdtIdentifier=true` — the
     * encoder envelope must use the ADT's type identifier so the decoder can dispatch. The
     * trait/branch decision is the caller's, mirroring Java's `declaredType` reflection check.
     *
     * PR-08-D02: `baboonSameInVersions()` MUST be non-empty for any registered type. We index
     * `[0]` directly so a violation throws rather than silently masquerading.
     */
    public static from(value: BaboonGenerated, useAdtIdentifier: boolean = false): BaboonTypeMeta {
        let typeIdentifier: string;
        if (useAdtIdentifier && BaboonTypeMeta.isAdtMember(value)) {
            typeIdentifier = value.baboonAdtTypeIdentifier();
        } else {
            typeIdentifier = value.baboonTypeIdentifier();
        }
        const sameIn = value.baboonSameInVersions();
        if (sameIn.length === 0) {
            throw new BaboonException(
                `baboonSameInVersions() is empty for type ${value.baboonTypeIdentifier()}`,
            );
        }
        return new BaboonTypeMeta(
            BaboonTypeMetaCodec.META_VERSION,
            value.baboonDomainIdentifier(),
            value.baboonDomainVersion(),
            sameIn[0]!,
            typeIdentifier,
        );
    }

    private static isAdtMember(value: BaboonGenerated): value is BaboonAdtMemberMeta {
        // PR-25.8 / PR-22-D02: typeguard checks for a method (function-typed property), matching
        // codegen — `TsDomainTreeTools.adtMeta` emits `baboonAdtTypeIdentifier()` as an instance
        // method, not a `string`-valued property. The previous `=== "string"` check never matched
        // and silently fell through to the concrete-branch type identifier.
        return typeof (value as { baboonAdtTypeIdentifier?: unknown }).baboonAdtTypeIdentifier === "function";
    }

    public static readMeta(reader: BaboonBinReader): BaboonTypeMeta | undefined {
        return BaboonTypeMetaCodec.readBin(reader);
    }

    public static readMetaJson(json: unknown): BaboonTypeMeta | undefined {
        return BaboonTypeMetaCodec.readJson(json);
    }

    public equals(other: BaboonTypeMeta): boolean {
        return this.metaVersion === other.metaVersion
            && this.domainIdentifier === other.domainIdentifier
            && this.domainVersion === other.domainVersion
            && this.domainVersionMinCompat === other.domainVersionMinCompat
            && this.typeIdentifier === other.typeIdentifier;
    }
}

export class BaboonTypeMetaCodec {
    public static readonly META_VERSION_1: number = 1;
    public static readonly META_VERSION: number = BaboonTypeMetaCodec.META_VERSION_1;

    public static readonly META_VERSION_KEY = "$mv";
    public static readonly DOMAIN_IDENTIFIER_KEY = "$d";
    public static readonly DOMAIN_VERSION_KEY = "$v";
    public static readonly DOMAIN_VERSION_MIN_COMPAT_KEY = "$uv";
    public static readonly TYPE_IDENTIFIER_KEY = "$t";

    public static writeBin(meta: BaboonTypeMeta, writer: BaboonBinWriter): void {
        BinTools.writeByte(writer, BaboonTypeMetaCodec.META_VERSION & 0xFF);
        BinTools.writeString(writer, meta.domainIdentifier);
        BinTools.writeString(writer, meta.domainVersion);
        if (meta.domainVersion === meta.domainVersionMinCompat) {
            BinTools.writeByte(writer, 0);
        } else {
            BinTools.writeByte(writer, 1);
            BinTools.writeString(writer, meta.domainVersionMinCompat);
        }
        BinTools.writeString(writer, meta.typeIdentifier);
    }

    public static readBin(reader: BaboonBinReader): BaboonTypeMeta | undefined {
        const metaVersion = BinTools.readByte(reader);
        if (metaVersion !== BaboonTypeMetaCodec.META_VERSION_1) return undefined;

        const domainIdentifier = BinTools.readString(reader);
        const domainVersion = BinTools.readString(reader);
        const hasMinCompat = BinTools.readByte(reader);
        const domainVersionMinCompat = hasMinCompat === 1 ? BinTools.readString(reader) : domainVersion;
        const typeIdentifier = BinTools.readString(reader);

        return new BaboonTypeMeta(
            BaboonTypeMetaCodec.META_VERSION,
            domainIdentifier,
            domainVersion,
            domainVersionMinCompat,
            typeIdentifier,
        );
    }

    public static writeJson(meta: BaboonTypeMeta): Record<string, unknown> {
        // MFACADE-PR-3: always emit `$mv` as a JSON number so envelopes are
        // self-identifying without out-of-band knowledge (proposal §10.6 (a)).
        const obj: Record<string, unknown> = {};
        obj[BaboonTypeMetaCodec.META_VERSION_KEY] = BaboonTypeMetaCodec.META_VERSION;
        obj[BaboonTypeMetaCodec.DOMAIN_IDENTIFIER_KEY] = meta.domainIdentifier;
        obj[BaboonTypeMetaCodec.DOMAIN_VERSION_KEY] = meta.domainVersion;
        obj[BaboonTypeMetaCodec.TYPE_IDENTIFIER_KEY] = meta.typeIdentifier;
        if (meta.domainVersion !== meta.domainVersionMinCompat) {
            obj[BaboonTypeMetaCodec.DOMAIN_VERSION_MIN_COMPAT_KEY] = meta.domainVersionMinCompat;
        }
        return obj;
    }

    /**
     * Mirrors Scala/C#/Java (PR-08-D01): if `$mv` is present and not "1", reject (return
     * `undefined`); if absent, fall through to v1 read. JSON `$mv` is a string for parity with
     * the existing wire format across languages.
     */
    public static readJson(json: unknown): BaboonTypeMeta | undefined {
        if (typeof json !== "object" || json === null || Array.isArray(json)) return undefined;
        const obj = json as Record<string, unknown>;

        // MFACADE-PR-3: accept $mv as either a JSON number or a string (back-compat
        // with M28-vintage fixtures); both must equal META_VERSION_1.
        const mvNode = obj[BaboonTypeMetaCodec.META_VERSION_KEY];
        if (mvNode !== undefined) {
            let mv: number;
            if (typeof mvNode === "number") {
                if (!Number.isInteger(mvNode)) return undefined;
                mv = mvNode;
            } else if (typeof mvNode === "string") {
                if (!/^-?\d+$/.test(mvNode)) return undefined;
                mv = parseInt(mvNode, 10);
            } else {
                return undefined;
            }
            if (mv !== BaboonTypeMetaCodec.META_VERSION_1) return undefined;
        }

        const d = obj[BaboonTypeMetaCodec.DOMAIN_IDENTIFIER_KEY];
        const v = obj[BaboonTypeMetaCodec.DOMAIN_VERSION_KEY];
        const t = obj[BaboonTypeMetaCodec.TYPE_IDENTIFIER_KEY];
        if (typeof d !== "string" || typeof v !== "string" || typeof t !== "string") return undefined;

        const uvNode = obj[BaboonTypeMetaCodec.DOMAIN_VERSION_MIN_COMPAT_KEY];
        const uv = typeof uvNode === "string" ? uvNode : v;

        return new BaboonTypeMeta(BaboonTypeMetaCodec.META_VERSION, d, v, uv, t);
    }
}

// --- Codec interfaces ---

/**
 * Marker interface implemented by all codec types. Lets the codec registry hold a heterogeneous
 * lazy-codec map keyed by type identifier without losing the ability to dispatch generically.
 * Mirrors Java `BaboonCodecData` / C# `IBaboonCodecData`.
 */
// eslint-disable-next-line @typescript-eslint/no-empty-interface
export interface BaboonCodecData {
}

export interface BaboonBinCodec<T> extends BaboonCodecData {
    // PR 13.2 fix: signature corrected to match `TsUEBACodecGenerator` codegen — codecs emit
    // `(ctx, value, writer)`. Old interface declared `(ctx, writer, value)` which silently
    // diverged from the codegen and broke `BaboonCodecsFacade.jsonToUebaBytes` cross-format
    // conversion (discovered in PR 13.2 / PR-26-D01).
    encode(ctx: BaboonCodecContext, value: T, writer: BaboonBinWriter): void;
    decode(ctx: BaboonCodecContext, reader: BaboonBinReader): T;
}

export interface BaboonJsonCodec<T> extends BaboonCodecData {
    encode(ctx: BaboonCodecContext, value: T): unknown;
    decode(ctx: BaboonCodecContext, wire: unknown): T;
}

// --- BaboonMeta (per-version "same-in-versions" lookup) ---

export interface BaboonMeta {
    sameInVersions(typeId: string): string[];
}

// --- AbstractBaboonCodecs registry base ---

/**
 * Shared codec-registry base. Holds a single map of `typeId -> Lazy<BaboonCodecData>` so the
 * facade's generic `getCodec` can iterate either the JSON or UEBA registry uniformly.
 * Mirrors C# `AbstractBaboonCodecs` / Java `AbstractBaboonCodecs`.
 */
export abstract class AbstractBaboonCodecs {
    private readonly codecs: Map<string, Lazy<BaboonCodecData>> = new Map();

    protected registerData(typeId: string, codec: Lazy<BaboonCodecData>): void {
        this.codecs.set(typeId, codec);
    }

    public find(typeId: string): Lazy<BaboonCodecData> {
        const v = this.codecs.get(typeId);
        if (v === undefined) {
            throw new BaboonException(`No codec registered for ${typeId}`);
        }
        return v;
    }

    public tryFind(typeId: string): Lazy<BaboonCodecData> | undefined {
        return this.codecs.get(typeId);
    }
}

export abstract class AbstractBaboonJsonCodecs extends AbstractBaboonCodecs {
    public register(typeId: string, codec: Lazy<BaboonJsonCodec<unknown>>): void {
        this.registerData(typeId, codec as Lazy<BaboonCodecData>);
    }

    public getCodec<T>(typeId: string): BaboonJsonCodec<T> {
        const codec = this.tryFind(typeId);
        if (codec === undefined) {
            throw new BaboonException(`No JSON codec registered for ${typeId}`);
        }
        return codec.value as BaboonJsonCodec<T>;
    }
}

export abstract class AbstractBaboonUebaCodecs extends AbstractBaboonCodecs {
    public register(typeId: string, codec: Lazy<BaboonBinCodec<unknown>>): void {
        this.registerData(typeId, codec as Lazy<BaboonCodecData>);
    }

    public getCodec<T>(typeId: string): BaboonBinCodec<T> {
        const codec = this.tryFind(typeId);
        if (codec === undefined) {
            throw new BaboonException(`No UEBA codec registered for ${typeId}`);
        }
        return codec.value as BaboonBinCodec<T>;
    }
}

// --- BaboonExt helpers ---
//
// Mirrors C# `BaboonExt` convenience methods. Exported as free functions following the
// existing TypeScript runtime idiom (standalone exports rather than a static utility class).

/**
 * Returns a `BaboonDomainVersion` combining the domain identifier and version from a generated
 * value. Mirrors C# `BaboonExt.DomainVersion(g)`.
 */
export function domainVersion(g: BaboonGenerated): BaboonDomainVersion {
    return new BaboonDomainVersion(g.baboonDomainIdentifier(), g.baboonDomainVersion());
}

/**
 * Returns the first entry of `g.baboonSameInVersions()` — the oldest version in which this
 * type's schema has been unmodified. Mirrors C# `BaboonExt.BaboonUnmodifiedSinceVersion(g)`.
 *
 * Throws if `baboonSameInVersions()` is empty (codegen invariant violation).
 */
export function baboonUnmodifiedSinceVersion(g: BaboonGenerated): string {
    const versions = g.baboonSameInVersions();
    if (versions.length === 0) {
        throw new BaboonException(
            `baboonSameInVersions() is empty for type ${g.baboonTypeIdentifier()}`,
        );
    }
    return versions[0]!;
}

/**
 * Returns the first entry of `meta.sameInVersions(typeId)` — the oldest version in which
 * the type identified by `typeId` has been unmodified. Mirrors C# `BaboonExt.UnmodifiedSinceVersion(meta, tpe)`.
 *
 * Throws if `sameInVersions()` returns an empty array (codegen invariant violation).
 */
export function unmodifiedSinceVersion(meta: BaboonMeta, typeId: string): string {
    const versions = meta.sameInVersions(typeId);
    if (versions.length === 0) {
        throw new BaboonException(
            `sameInVersions() is empty for typeId ${typeId}`,
        );
    }
    return versions[0]!;
}