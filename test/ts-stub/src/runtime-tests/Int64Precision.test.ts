// NOTE: references generated symbols under src/generated/** which are produced into this stub
// only by `mdl :build :test-gen-regular-adt`. Run from the codegen'd copy.
//
// 64-bit integers are the one place where JSON's number type cannot carry Baboon's value range.
// Backends disagree about the wire form — C#/Java/Kotlin/Rust write numbers, TypeScript/Dart/
// Swift write decimal strings (docs/json-codecs.md) — and every other backend's decoder accepts
// both. TypeScript's assumed a string and passed whatever it received to `BigInt(...)`.
//
// The failure that mattered: by the time a codec runs, `JSON.parse` has already rounded a numeric
// token beyond Number.MAX_SAFE_INTEGER, so Int64.MaxValue written by C# as
// `{"appPortalId":9223372036854775807}` arrived as 9223372036854775808n — a different, wrong,
// perfectly valid-looking integer. The original is unrecoverable at this layer, so the decoder
// must either read the exact string form or refuse; it must never round silently.
import { describe, expect, test } from 'vitest';
import { BaboonInt64 } from '../generated/BaboonSharedRuntime';

describe('BaboonInt64.read', () => {
    test('reads the exact value from the decimal-string form', () => {
        expect(BaboonInt64.read('9223372036854775807', 'x')).toBe(9223372036854775807n);
        expect(BaboonInt64.read('-9223372036854775808', 'x')).toBe(-9223372036854775808n);
        expect(BaboonInt64.read('18446744073709551615', 'x')).toBe(18446744073709551615n);
    });

    test('accepts numeric tokens that JSON.parse can carry losslessly', () => {
        expect(BaboonInt64.read(42, 'x')).toBe(42n);
        expect(BaboonInt64.read(-42, 'x')).toBe(-42n);
        expect(BaboonInt64.read(Number.MAX_SAFE_INTEGER, 'x')).toBe(BigInt(Number.MAX_SAFE_INTEGER));
    });

    test('refuses a numeric token that JSON.parse has already rounded', () => {
        // This is what a C#-written Int64.MaxValue looks like after JSON.parse.
        const rounded = JSON.parse('{"v":9223372036854775807}').v;
        expect(rounded).toBe(9223372036854775808); // precision is gone before we are called
        expect(() => BaboonInt64.read(rounded, 'v')).toThrow(/exceeds Number.MAX_SAFE_INTEGER/);
    });

    test('refuses non-integral and non-numeric input', () => {
        expect(() => BaboonInt64.read(1.5, 'x')).toThrow(/not an integer/);
        expect(() => BaboonInt64.read('nope', 'x')).toThrow(/not a 64-bit integer/);
        expect(() => BaboonInt64.read(null, 'x')).toThrow(/expected a decimal string or a number/);
        expect(() => BaboonInt64.read({}, 'x')).toThrow(/expected a decimal string or a number/);
    });

    test('round-trips the string form this backend emits', () => {
        const original = 9223372036854775807n;
        const wire = JSON.parse(JSON.stringify({ v: original.toString() })).v;
        expect(BaboonInt64.read(wire, 'v')).toBe(original);
    });
});
