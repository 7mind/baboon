import { BaboonDecimal, BaboonDateTimeUtc, BaboonDateTimeOffset } from "./baboon_runtime";

export class BaboonRandom {
    nextI08(): number {
        return Math.floor(Math.random() * 256) - 128;
    }

    nextI16(): number {
        return Math.floor(Math.random() * 65536) - 32768;
    }

    nextI32(): number {
        return Math.floor(Math.random() * 4294967296) - 2147483648;
    }

    nextI64(): bigint {
        const hi = BigInt(Math.floor(Math.random() * 4294967296));
        const lo = BigInt(Math.floor(Math.random() * 4294967296));
        const raw = (hi << 32n) | lo;
        return raw >= 9223372036854775808n ? raw - 18446744073709551616n : raw;
    }

    nextU08(): number {
        return Math.floor(Math.random() * 256);
    }

    nextU16(): number {
        return Math.floor(Math.random() * 65536);
    }

    nextU32(): number {
        return Math.floor(Math.random() * 4294967296);
    }

    nextU64(): bigint {
        const hi = BigInt(Math.floor(Math.random() * 4294967296));
        const lo = BigInt(Math.floor(Math.random() * 4294967296));
        return (hi << 32n) | lo;
    }

    nextF32(): number {
        const val = (Math.random() * 2000) - 1000;
        const buf = new Float32Array(1);
        buf[0] = val;
        return buf[0];
    }

    nextF64(): number {
        return (Math.random() * 2000) - 1000;
    }

    nextF128(): BaboonDecimal {
        const value = Math.floor(Math.random() * 1999998) - 999999;
        return BaboonDecimal.fromNumber(value / 100);
    }

    nextString(): string {
        const len = Math.floor(Math.random() * 19) + 1;
        let result = "";
        for (let i = 0; i < len; i++) {
            result += String.fromCharCode(97 + Math.floor(Math.random() * 26));
        }
        return result;
    }

    nextBytes(): Uint8Array {
        const len = Math.floor(Math.random() * 19) + 1;
        const bytes = new Uint8Array(len);
        for (let i = 0; i < len; i++) {
            bytes[i] = Math.floor(Math.random() * 256);
        }
        return bytes;
    }

    nextUid(): string {
        const hex = (n: number) => {
            let result = "";
            for (let i = 0; i < n; i++) {
                result += Math.floor(Math.random() * 16).toString(16);
            }
            return result;
        };
        return `${hex(8)}-${hex(4)}-4${hex(3)}-${(8 + Math.floor(Math.random() * 4)).toString(16)}${hex(3)}-${hex(12)}`;
    }

    nextTsu(): BaboonDateTimeUtc {
        const secs = Math.floor(Math.random() * 2000000000);
        const millis = Math.floor(Math.random() * 1000);
        return BaboonDateTimeUtc.fromMillis(secs * 1000 + millis);
    }

    nextTso(): BaboonDateTimeOffset {
        const secs = Math.floor(Math.random() * 2000000000);
        const millis = Math.floor(Math.random() * 1000);
        const offsetHours = Math.floor(Math.random() * 25) - 12;
        const epochMs = secs * 1000 + millis;
        return BaboonDateTimeOffset.fromDateAndOffset(new Date(epochMs), offsetHours * 3600 * 1000);
    }

    nextBit(): boolean {
        return Math.random() < 0.5;
    }

    nextUSize(max: number): number {
        return Math.floor(Math.random() * max);
    }

    mkEnum<T>(values: ReadonlyArray<T>): T {
        const idx = Math.floor(Math.random() * values.length);
        return values[idx];
    }
}
