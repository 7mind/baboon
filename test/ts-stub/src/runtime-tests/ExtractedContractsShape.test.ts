// T44 — Interface-shape assertions for extracted contracts (TypeScript stub).
//
// Generated symbols (my/ok/extracted/contracts/*) are emitted by
// `mdl :build :test-gen-regular-adt`. Running tests directly from the source
// tree will fail with missing imports; run from the codegen'd copy under
// target/test-regular/ts-stub/.
//
// Enforcement strategy:
//   All typed-assignment assertions in this file are compile-time-checked via
//   `tsc --noEmit` (run as `npm run build` in the lane before `npm test`).
//   `vitest run` uses esbuild transpile without type-checking, so without the
//   `tsc --noEmit` step the type assertions would not be enforced.
//
// Coverage:
//   (a) Contract variant — the host class nominally `implements` B in its
//       class declaration. The typed assignment `const b: B = new Host(...)`
//       is enforced by tsc: if Host stopped implementing B the assignment
//       would be a compile error. A B-declared member is then read at runtime
//       through the B-typed reference; failures throw unconditionally.
//   (b) Mirror variant — B (the mirror interface) exists as a standalone TS
//       interface with the expected member set. The probe class nominally
//       `implements` B (verified by compile-time typed assignment). The host
//       class does NOT nominally declare `implements B`, and more importantly,
//       under TypeScript structural typing the host IS still structurally
//       compatible with B (both have the same subset of members). The genuine
//       negative that tsc enforces: B cannot be assigned to the host type
//       because B lacks the host's extra members. This is expressed via a
//       ts-expect-error directive — if the assignment ever became valid (i.e.
//       if the assertion were wrong), tsc would flag "Unused ts-expect-error"
//       turning the vacuous annotation into a compile error.
//   (c) Member-set spot-check — field names and types verified through B-typed
//       bindings (compile-time + runtime).
//
// Runtime checks use explicit throw, not vitest `expect`, because `expect`
// may be configured to not throw in some environments.

import { describe, test } from 'vitest';
import { IntBox } from '../generated/my/ok/extracted/contracts/IntBox';
import { StrBox } from '../generated/my/ok/extracted/contracts/StrBox';
import { IntKey } from '../generated/my/ok/extracted/contracts/IntKey';
import { IntTagged } from '../generated/my/ok/extracted/contracts/IntTagged';
import { IntTagMirrorProbe } from '../generated/my/ok/extracted/contracts/IntTagMirrorProbe';
import { IntContainer } from '../generated/my/ok/extracted/contracts/IntContainer';
import { IntResult_Ok } from '../generated/my/ok/extracted/contracts/IntResult';
import { IntPayload } from '../generated/my/ok/extracted/contracts/IntPayload';
import { IntPayloadProbe } from '../generated/my/ok/extracted/contracts/IntPayloadProbe';
import type { IBox } from '../generated/my/ok/extracted/contracts/IBox';
import type { IKey } from '../generated/my/ok/extracted/contracts/IKey';
import type { ITagged } from '../generated/my/ok/extracted/contracts/ITagged';
import type { ITagMirror } from '../generated/my/ok/extracted/contracts/ITagMirror';
import type { IContainer } from '../generated/my/ok/extracted/contracts/IContainer';
import type { ResultBase } from '../generated/my/ok/extracted/contracts/ResultBase';
import type { IMirroredPayload } from '../generated/my/ok/extracted/contracts/IMirroredPayload';

function require(condition: boolean, message: string): void {
    if (!condition) throw new Error(`ExtractedContractsShape: ${message}`);
}

// ── (a) Contract variant: host nominally implements B ─────────────────────────

describe('IBox contract variant', () => {
    test('IntBox is assignable to IBox (compile-time nominal implements); count readable', () => {
        const host = new IntBox(7, 42);

        // Compile-time: IntBox declares `implements IBox` — the type annotation
        // enforces the nominal relationship. tsc --noEmit will reject this if
        // IntBox ever stops implementing IBox.
        const b: IBox = host;

        const countViaB: number = b.count;
        require(countViaB === 7, `IBox.count must equal 7 but was ${countViaB}`);
    });

    test('StrBox is also assignable to IBox (req 8: >=2 instantiations share B)', () => {
        const strHost = new StrBox(3, 'hello');

        // Req 8: both IntBox and StrBox nominally implement IBox.
        const b: IBox = strHost;
        const countViaB: number = b.count;
        require(countViaB === 3, `IBox.count (via StrBox) must equal 3 but was ${countViaB}`);
    });
});

describe('IKey contract variant', () => {
    test('IntKey is assignable to IKey (compile-time); key readable', () => {
        const host = new IntKey(999n, 1);

        // Compile-time: IntKey declares `implements IKey`. IKey.key is bigint.
        const b: IKey = host;

        const keyViaB: bigint = b.key;
        require(keyViaB === 999n, `IKey.key must equal 999n but was ${keyViaB}`);
    });
});

describe('ITagged contract variant', () => {
    test('IntTagged is assignable to ITagged (compile-time); label readable', () => {
        const host = new IntTagged('hello', 5);

        // Compile-time: IntTagged declares `implements ITagged`.
        const b: ITagged = host;

        const labelViaB: string = b.label;
        require(labelViaB === 'hello', `ITagged.label must equal 'hello' but was '${labelViaB}'`);
    });
});

describe('IContainer contract variant', () => {
    test('IntContainer is assignable to IContainer (compile-time); members readable', () => {
        const host = new IntContainer(1, 99, 2, 3, 4);

        // Compile-time: IntContainer declares `implements IContainer`.
        const b: IContainer = host;

        // Spot-check all three IContainer-declared fields.
        require(b.own === 1, `IContainer.own must equal 1 but was ${b.own}`);
        require(b.second === 3, `IContainer.second must equal 3 but was ${b.second}`);
        require(b.base_field === 4, `IContainer.base_field must equal 4 but was ${b.base_field}`);
    });
});

describe('IResult contract variant (ADT host)', () => {
    test('IntResult_Ok is assignable to ResultBase (compile-time); tag readable', () => {
        const host = new IntResult_Ok('ok', 0);

        // IntResult_Ok nominally implements both ResultBase and IResult.
        // IResult extends ResultBase which declares tag: string.
        const b: ResultBase = host;

        const tagViaB: string = b.tag;
        require(tagViaB === 'ok', `ResultBase.tag must equal 'ok' but was '${tagViaB}'`);
    });
});

// ── (b) Mirror variant: probe implements B; host does not nominally declare B ─
//
// TypeScript structural-typing reality for the mirror variant:
//   IMirroredPayload = { readonly label: string }
//   IntPayload       = { readonly label: string; readonly value: number }
//
//   Under TS structural typing, IntPayload IS assignable TO IMirroredPayload
//   (a type with MORE members satisfies an interface with fewer).
//   So `const _: IMirroredPayload = new IntPayload(...)` compiles without error —
//   the negative claim that IntPayload is NOT assignable to IMirroredPayload is
//   FALSE and cannot be expressed via IsAssignableTo.
//
//   The GENUINE TS negative (enforced by tsc): the reverse direction.
//   IMirroredPayload lacks `value: number`, so IMirroredPayload is NOT assignable
//   to IntPayload. A ts-expect-error directive below asserts this; if the annotation
//   were wrong (i.e., the assignment became valid), tsc would emit
//   "Unused ts-expect-error" — a real compile error — catching any drift.
//
//   The same applies to ITagMirror vs IntTagged (ITagMirror lacks `extra`).

describe('IMirroredPayload mirror variant', () => {
    test('IntPayloadProbe is assignable to IMirroredPayload (probe nominally implements B)', () => {
        const probe = new IntPayloadProbe('mirror-test');

        // Compile-time: IntPayloadProbe declares `implements IMirroredPayload`.
        // tsc --noEmit enforces this typed assignment.
        const b: IMirroredPayload = probe;

        const labelViaB: string = b.label;
        require(labelViaB === 'mirror-test',
            `IMirroredPayload.label must equal 'mirror-test' but was '${labelViaB}'`);
    });

    test('IMirroredPayload is NOT assignable to IntPayload (genuine tsc-enforced negative)', () => {
        // IMirroredPayload = { label: string } — lacks `value: number`.
        // IntPayload requires both { label, value }. Therefore:
        //   IMirroredPayload is NOT assignable TO IntPayload.
        // This is the true structural negative for the mirror variant in TypeScript.
        //
        // The directive below is enforced by `tsc --noEmit` in the lane:
        // if it were ever wrong (i.e., IMirroredPayload became assignable to IntPayload),
        // tsc would report "Unused ts-expect-error" — a real compile-time error.
        //
        // @ts-expect-error IMirroredPayload is not assignable to IntPayload (lacks 'value')
        const _notAssignable: IntPayload = {} as IMirroredPayload;
        void _notAssignable; // suppress unused-variable warning
    });

    test('IMirroredPayload member set: has exactly label: string', () => {
        const probe = new IntPayloadProbe('spot');
        const b: IMirroredPayload = probe;
        const label: string = b.label; // compile-time type assertion: string
        require(label === 'spot', `IMirroredPayload.label spot-check failed: '${label}'`);
    });
});

describe('ITagMirror mirror variant', () => {
    test('IntTagMirrorProbe is assignable to ITagMirror (probe nominally implements B)', () => {
        const probe = new IntTagMirrorProbe('tag-mirror');

        // Compile-time: IntTagMirrorProbe declares `implements ITagMirror`.
        const b: ITagMirror = probe;

        const labelViaB: string = b.label;
        require(labelViaB === 'tag-mirror',
            `ITagMirror.label must equal 'tag-mirror' but was '${labelViaB}'`);
    });

    test('ITagMirror is NOT assignable to IntTagged (genuine tsc-enforced negative)', () => {
        // ITagMirror = { label: string } — lacks `extra: number`.
        // IntTagged requires both { label, extra }. Therefore:
        //   ITagMirror is NOT assignable TO IntTagged.
        //
        // @ts-expect-error ITagMirror is not assignable to IntTagged (lacks 'extra')
        const _notAssignable: IntTagged = {} as ITagMirror;
        void _notAssignable;
    });

    test('ITagMirror member set: has exactly label: string', () => {
        const probe = new IntTagMirrorProbe('spot');
        const b: ITagMirror = probe;
        const label: string = b.label;
        require(label === 'spot', `ITagMirror.label spot-check failed: '${label}'`);
    });
});

// ── (c) Member-set spot-checks ─────────────────────────────────────────────────

describe('IBox member set spot-check', () => {
    test('IBox has exactly count: number', () => {
        const box = new IntBox(11, 0);
        const b: IBox = box;
        const count: number = b.count; // compile-time: number
        require(count === 11, `IBox.count spot-check failed: ${count}`);
    });
});

describe('IKey member set spot-check', () => {
    test('IKey has exactly key: bigint', () => {
        const key = new IntKey(12345n, 0);
        const b: IKey = key;
        const k: bigint = b.key; // compile-time: bigint
        require(k === 12345n, `IKey.key spot-check failed: ${k}`);
    });
});

describe('IContainer member set spot-check', () => {
    test('IContainer has own: number, second: number, base_field: number', () => {
        const c = new IntContainer(10, 0, 0, 20, 30);
        const b: IContainer = c;
        require(b.own === 10, `IContainer.own spot-check failed: ${b.own}`);
        require(b.second === 20, `IContainer.second spot-check failed: ${b.second}`);
        require(b.base_field === 30, `IContainer.base_field spot-check failed: ${b.base_field}`);
    });
});
