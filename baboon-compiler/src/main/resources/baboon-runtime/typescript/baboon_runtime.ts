// --- Codec Context ---

export enum BaboonCodecContext {
    Default = "Default",
    Indexed = "Indexed",
    Compact = "Compact",
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
        const v = this.buf[this.pos];
        this.pos += 1;
        return v;
    }

    readBytes(length: number): Uint8Array {
        const slice = this.buf.slice(this.pos, this.pos + length);
        this.pos += length;
        return slice;
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
        return this.date.toISOString().replace(/(\\.\\d{3})\\d*Z$/, "$1Z");
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
        const match = s.match(/([+-])(\\d{2}):(\\d{2})$/);
        if (match) {
            const sign = match[1] === "+" ? 1 : -1;
            const hours = parseInt(match[2], 10);
            const minutes = parseInt(match[3], 10);
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

        if (this.offsetMs === 0) {
            return `${yyyy}-${MM}-${dd}T${HH}:${mm}:${ss}.${ms}Z`;
        }

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
        const num = value.toNumber();
        const isNeg = num < 0;
        const abs = Math.abs(num);
        const str = abs.toString();

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
        guid[0] = bytes[3]; guid[1] = bytes[2]; guid[2] = bytes[1]; guid[3] = bytes[0];
        guid[4] = bytes[5]; guid[5] = bytes[4];
        guid[6] = bytes[7]; guid[7] = bytes[6];
        for (let i = 8; i < 16; i++) guid[i] = bytes[i];
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
            str = str.replace(/\\.?0+$/, "") || "0";
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
        bytes[0] = raw[3]; bytes[1] = raw[2]; bytes[2] = raw[1]; bytes[3] = raw[0];
        bytes[4] = raw[5]; bytes[5] = raw[4];
        bytes[6] = raw[7]; bytes[7] = raw[6];
        for (let i = 8; i < 16; i++) bytes[i] = raw[i];

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
    readonly baboon_domain_version: string;
    readonly baboon_domain_identifier: string;
    readonly baboon_type_identifier: string;
}

export interface BaboonGeneratedLatest extends BaboonGenerated {}

export interface BaboonAdtMemberMeta extends BaboonGenerated {
    readonly baboon_adt_type_identifier: string;
}

// --- Conversion interfaces ---

export interface AbstractConversion<From, To> {
    convert(from: From): To;
}

export interface AbstractBaboonConversions {
    versionsFrom(): string[];
    versionTo(): string;
}
