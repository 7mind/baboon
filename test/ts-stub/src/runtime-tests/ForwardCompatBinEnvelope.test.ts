// NOTE: This test references generated symbols (src/generated/fwde2e/**) which are
// copied into this stub only by `mdl :build :test-gen-regular-adt` (rsync + codegen
// into target/test-regular/ts-stub/). Run from the codegen'd copy.
//
// End-to-end proof of the UEBA envelope's writer-side forward policy
// (docs/forward-compat.md, "Envelope integration (UEBA)"): the v1 binary envelope has a
// single bound slot, `domainVersionMinCompat`. Under the default Strict policy it carries the
// byte-identical bound (unchanged behaviour); under Tolerant it carries the prefix bound for
// the chosen index mode, and a reader deployed before the writer's version decodes the payload
// with its newest codec.
import { describe, expect, test } from 'vitest';
import {
    AbstractBaboonConversions,
    AbstractBaboonJsonCodecs,
    AbstractBaboonUebaCodecs,
    BaboonBinCodec,
    BaboonBinReader,
    BaboonCodecContext,
    BaboonDomainVersion,
    BaboonEither,
    BaboonEnvelopeVersion,
    BaboonGenerated,
    BaboonJsonCodec,
    BaboonMeta,
    BaboonTypeMeta,
    ForwardWritePolicy,
    Lazy,
} from '../generated/BaboonSharedRuntime';
import { BaboonCodecsFacade } from '../generated/BaboonCodecsFacade';
import { DomainFwde2eFwdFacade } from '../generated/fwde2e/fwd/DomainFwde2eFwdFacade';
import { DomainFwde2eChainFacade } from '../generated/fwde2e/chain/DomainFwde2eChainFacade';

import { FwdAppendVar, FwdEnumHost, FwdMidInsert, FwdStable } from '../generated/fwde2e/fwd/index';
import { FwdEnumGrows } from '../generated/fwde2e/fwd/FwdEnumGrows';
import { FwdAppendVar as FwdAppendVarV1, FwdAppendVar_JsonCodec as FwdAppendVarV1_JsonCodec, FwdAppendVar_UEBACodec as FwdAppendVarV1_UEBACodec } from '../generated/fwde2e/fwd/v1_0_0/FwdAppendVar';
import { FwdMidInsert as FwdMidInsertV1, FwdMidInsert_JsonCodec as FwdMidInsertV1_JsonCodec, FwdMidInsert_UEBACodec as FwdMidInsertV1_UEBACodec } from '../generated/fwde2e/fwd/v1_0_0/FwdMidInsert';
import { FwdStable as FwdStableV1, FwdStable_JsonCodec as FwdStableV1_JsonCodec, FwdStable_UEBACodec as FwdStableV1_UEBACodec } from '../generated/fwde2e/fwd/v1_0_0/FwdStable';
import { FwdEnumHost as FwdEnumHostV1, FwdEnumHost_JsonCodec as FwdEnumHostV1_JsonCodec, FwdEnumHost_UEBACodec as FwdEnumHostV1_UEBACodec } from '../generated/fwde2e/fwd/v1_0_0/FwdEnumHost';

import { ChainAppend } from '../generated/fwde2e/chain/ChainAppend';
import { ChainAppend as ChainAppendV1, ChainAppend_JsonCodec as ChainAppendV1_JsonCodec, ChainAppend_UEBACodec as ChainAppendV1_UEBACodec } from '../generated/fwde2e/chain/v1_0_0/ChainAppend';
import { ChainAppend as ChainAppendV2, ChainAppend_JsonCodec as ChainAppendV2_JsonCodec, ChainAppend_UEBACodec as ChainAppendV2_UEBACodec } from '../generated/fwde2e/chain/v2_0_0/ChainAppend';

type Registration = { typeId: string; json: BaboonJsonCodec<unknown>; ueba: BaboonBinCodec<unknown> };

// The generated per-version codec registries are module-private, so old readers are assembled
// from the exported per-type codecs (mirrors ForwardCompatEnvelope.test.ts).
function versionModules(domain: string, version: string, regs: Registration[]) {
    class JsonCodecs extends AbstractBaboonJsonCodecs {
        constructor() {
            super();
            for (const r of regs) this.register(r.typeId, new Lazy<BaboonJsonCodec<unknown>>(() => r.json));
        }
    }
    class UebaCodecs extends AbstractBaboonUebaCodecs {
        constructor() {
            super();
            for (const r of regs) this.register(r.typeId, new Lazy<BaboonBinCodec<unknown>>(() => r.ueba));
        }
    }
    class Conversions implements AbstractBaboonConversions {
        public versionsFrom(): string[] { return []; }
        public versionTo(): string { return version; }
    }
    class Meta implements BaboonMeta {
        public sameInVersions(_typeId: string): string[] { return [version]; }
        public forwardReadableVersions(_typeId: string): { readonly [version: string]: string } { return { [version]: 'identical' }; }
    }
    return (f: BaboonCodecsFacade) =>
        f.register(new BaboonDomainVersion(domain, version), () => new JsonCodecs(), () => new UebaCodecs(), () => new Conversions(), () => new Meta());
}

const fwdV1 = versionModules('fwde2e.fwd', '1.0.0', [
    { typeId: FwdAppendVarV1.BaboonTypeIdentifier, json: FwdAppendVarV1_JsonCodec.instance as unknown as BaboonJsonCodec<unknown>, ueba: FwdAppendVarV1_UEBACodec.instance as unknown as BaboonBinCodec<unknown> },
    { typeId: FwdMidInsertV1.BaboonTypeIdentifier, json: FwdMidInsertV1_JsonCodec.instance as unknown as BaboonJsonCodec<unknown>, ueba: FwdMidInsertV1_UEBACodec.instance as unknown as BaboonBinCodec<unknown> },
    { typeId: FwdStableV1.BaboonTypeIdentifier, json: FwdStableV1_JsonCodec.instance as unknown as BaboonJsonCodec<unknown>, ueba: FwdStableV1_UEBACodec.instance as unknown as BaboonBinCodec<unknown> },
    { typeId: FwdEnumHostV1.BaboonTypeIdentifier, json: FwdEnumHostV1_JsonCodec.instance as unknown as BaboonJsonCodec<unknown>, ueba: FwdEnumHostV1_UEBACodec.instance as unknown as BaboonBinCodec<unknown> },
]);
const chainV1 = versionModules('fwde2e.chain', '1.0.0', [
    { typeId: ChainAppendV1.BaboonTypeIdentifier, json: ChainAppendV1_JsonCodec.instance as unknown as BaboonJsonCodec<unknown>, ueba: ChainAppendV1_UEBACodec.instance as unknown as BaboonBinCodec<unknown> },
]);
const chainV2 = versionModules('fwde2e.chain', '2.0.0', [
    { typeId: ChainAppendV2.BaboonTypeIdentifier, json: ChainAppendV2_JsonCodec.instance as unknown as BaboonJsonCodec<unknown>, ueba: ChainAppendV2_UEBACodec.instance as unknown as BaboonBinCodec<unknown> },
]);

// a reader deployed before fwde2e.fwd 2.0.0 existed: knows 1.0.0 only
function oldFwdReader(): BaboonCodecsFacade { const f = new BaboonCodecsFacade(); fwdV1(f); return f; }
// a reader deployed before fwde2e.chain 3.0.0 existed: knows 1.0.0 and 2.0.0
function midChainReader(): BaboonCodecsFacade { const f = new BaboonCodecsFacade(); chainV1(f); chainV2(f); return f; }
function v1ChainReader(): BaboonCodecsFacade { const f = new BaboonCodecsFacade(); chainV1(f); return f; }

const fwdWriter = new DomainFwde2eFwdFacade();
const chainWriter = new DomainFwde2eChainFacade();

const tolerantCompact = BaboonCodecContext.custom(false, ForwardWritePolicy.Tolerant, BaboonEnvelopeVersion.V1, undefined);
const tolerantIndexed = BaboonCodecContext.custom(true, ForwardWritePolicy.Tolerant, BaboonEnvelopeVersion.V1, undefined);

function unwrap<L, R>(e: BaboonEither<L, R>): R {
    if (e.tag !== 'Right') throw new Error(`expected Right, got Left: ${String((e as { value: unknown }).value)}`);
    return e.value;
}

function bin(writer: BaboonCodecsFacade, ctx: BaboonCodecContext, value: BaboonGenerated): Uint8Array {
    return unwrap(writer.encodeToBin(ctx, value));
}

function minCompatOf(bytes: Uint8Array): string {
    return BaboonTypeMeta.readMeta(new BaboonBinReader(bytes))!.domainVersionMinCompat;
}

describe('UEBA envelope writer-side forward policy (fwde2e.fwd, fwde2e.chain)', () => {
    test('Strict (default) writer publishes the byte-identical bound: envelopes are unchanged and the old reader refuses', () => {
        expect(BaboonCodecContext.Compact.forwardWritePolicy).toBe(ForwardWritePolicy.Strict);
        expect(BaboonCodecContext.Indexed.forwardWritePolicy).toBe(ForwardWritePolicy.Strict);

        const appended = bin(fwdWriter, BaboonCodecContext.Compact, new FwdAppendVar(42, 'hello', 'appended'));
        expect(minCompatOf(appended)).toBe('2.0.0');
        expect(oldFwdReader().decodeFromBin(appended).tag).toBe('Left');

        const stable = bin(fwdWriter, BaboonCodecContext.Compact, new FwdStable('same'));
        expect(minCompatOf(stable)).toBe('1.0.0');
        expect((unwrap(oldFwdReader().decodeFromBin(stable)) as FwdStableV1).s).toBe('same');
    });

    test('Tolerant writer publishes the compact prefix bound and the old reader decodes with its own codec', () => {
        const appended = bin(fwdWriter, tolerantCompact, new FwdAppendVar(42, 'hello', 'appended'));
        expect(minCompatOf(appended)).toBe('1.0.0');
        const decoded = unwrap(oldFwdReader().decodeFromBin(appended)) as FwdAppendVarV1;
        expect(decoded).toBeInstanceOf(FwdAppendVarV1);
        expect(decoded.a).toBe(42);
        expect(decoded.b).toBe('hello');

        // json-additive only (mid-position insert): no UEBA prefix bound — byte-identical to Strict, refused
        const inserted = new FwdMidInsert(7, 99, 'zed');
        expect(bin(fwdWriter, tolerantCompact, inserted)).toEqual(bin(fwdWriter, BaboonCodecContext.Compact, inserted));
        expect(oldFwdReader().decodeFromBin(bin(fwdWriter, tolerantCompact, inserted)).tag).toBe('Left');

        // byte-identical type: bound is 1.0.0 under both policies
        const stable = new FwdStable('same');
        expect(bin(fwdWriter, tolerantCompact, stable)).toEqual(bin(fwdWriter, BaboonCodecContext.Compact, stable));

        // grown enum: not forward-readable at all — unchanged envelope, refused
        const enumHost = new FwdEnumHost(FwdEnumGrows.C);
        expect(bin(fwdWriter, tolerantCompact, enumHost)).toEqual(bin(fwdWriter, BaboonCodecContext.Compact, enumHost));
        expect(oldFwdReader().decodeFromBin(bin(fwdWriter, tolerantCompact, enumHost)).tag).toBe('Left');
    });

    test('indexed payload: a variable-length appended field only earns prefix-compact, so no bound is lowered', () => {
        const appended = bin(fwdWriter, tolerantIndexed, new FwdAppendVar(42, 'hello', 'appended'));
        expect(minCompatOf(appended)).toBe('2.0.0');
        expect(oldFwdReader().decodeFromBin(appended).tag).toBe('Left');
        expect(FwdAppendVar.BaboonMinReaderVersions[BaboonTypeMeta.UEBA_PREFIX_COMPACT_TIER]).toBe('1.0.0');
        expect(FwdAppendVar.BaboonMinReaderVersions[BaboonTypeMeta.UEBA_PREFIX_ANY_MODE_TIER]).toBe('2.0.0');
    });

    test('three-version chain: the Tolerant bound is the chain minimum and each older reader decodes with its newest codec', () => {
        const bytes = bin(chainWriter, tolerantCompact, new ChainAppend(1, 'b', 'c'));
        expect(minCompatOf(bytes)).toBe('1.0.0');

        const mid = unwrap(midChainReader().decodeFromBin(bytes)) as ChainAppendV2;
        expect(mid).toBeInstanceOf(ChainAppendV2);
        expect(mid.a).toBe(1);
        expect(mid.b).toBe('b');

        const oldest = unwrap(v1ChainReader().decodeFromBin(bytes)) as ChainAppendV1;
        expect(oldest).toBeInstanceOf(ChainAppendV1);
        expect(oldest.a).toBe(1);

        // a Strict 3.0.0 envelope is refused by both
        const strict = bin(chainWriter, BaboonCodecContext.Compact, new ChainAppend(1, 'b', 'c'));
        expect(minCompatOf(strict)).toBe('3.0.0');
        expect(midChainReader().decodeFromBin(strict).tag).toBe('Left');
    });
});
