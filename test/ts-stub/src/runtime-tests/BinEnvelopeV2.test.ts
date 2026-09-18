// NOTE: This test references generated symbols (src/generated/fwde2e/**) which are copied into
// this stub only by `mdl :build :test-gen-regular-adt` (rsync + codegen into
// target/test-regular/ts-stub/). Run from the codegen'd copy.
//
// Binary envelope metaVersion 2 (docs/spec/codec-envelope.md §2.1.3): the JSON-equivalent layout
// carrying BOTH bounds — `domainVersionMinCompat` (byte-identical) and `domainVersionReadableMin`
// (prefix bound for the payload's index mode) — behind a flags byte. Writers stay on v1 unless the
// context selects v2; readers accept both.
import { describe, expect, test } from 'vitest';
import {
    AbstractBaboonConversions,
    AbstractBaboonJsonCodecs,
    AbstractBaboonUebaCodecs,
    BaboonBinCodec,
    BaboonBinReader,
    BaboonBinWriter,
    BaboonCodecContext,
    BaboonDomainVersion,
    BaboonEither,
    BaboonEnvelopeVersion,
    BaboonGenerated,
    BaboonJsonCodec,
    BaboonMeta,
    BaboonTypeMeta,
    BinTools,
    ForwardWritePolicy,
    Lazy,
} from '../generated/BaboonSharedRuntime';
import { BaboonCodecsFacade, ForwardReadPolicy } from '../generated/BaboonCodecsFacade';
import { DomainFwde2eFwdFacade } from '../generated/fwde2e/fwd/DomainFwde2eFwdFacade';
import { DomainFwde2eChainFacade } from '../generated/fwde2e/chain/DomainFwde2eChainFacade';

import { FwdAppendVar, FwdEnumHost, FwdMidInsert, FwdStable } from '../generated/fwde2e/fwd/index';
import { FwdAppendVar_UEBACodec } from '../generated/fwde2e/fwd/FwdAppendVar';
import { FwdEnumGrows } from '../generated/fwde2e/fwd/FwdEnumGrows';
import { FwdAppendVar as FwdAppendVarV1, FwdAppendVar_JsonCodec as FwdAppendVarV1_JsonCodec, FwdAppendVar_UEBACodec as FwdAppendVarV1_UEBACodec } from '../generated/fwde2e/fwd/v1_0_0/FwdAppendVar';
import { FwdMidInsert as FwdMidInsertV1, FwdMidInsert_JsonCodec as FwdMidInsertV1_JsonCodec, FwdMidInsert_UEBACodec as FwdMidInsertV1_UEBACodec } from '../generated/fwde2e/fwd/v1_0_0/FwdMidInsert';
import { FwdStable as FwdStableV1, FwdStable_JsonCodec as FwdStableV1_JsonCodec, FwdStable_UEBACodec as FwdStableV1_UEBACodec } from '../generated/fwde2e/fwd/v1_0_0/FwdStable';
import { FwdEnumHost as FwdEnumHostV1, FwdEnumHost_JsonCodec as FwdEnumHostV1_JsonCodec, FwdEnumHost_UEBACodec as FwdEnumHostV1_UEBACodec } from '../generated/fwde2e/fwd/v1_0_0/FwdEnumHost';
import { ChainAppend } from '../generated/fwde2e/chain/ChainAppend';
import { ChainAppend as ChainAppendV1, ChainAppend_JsonCodec as ChainAppendV1_JsonCodec, ChainAppend_UEBACodec as ChainAppendV1_UEBACodec } from '../generated/fwde2e/chain/v1_0_0/ChainAppend';
import { ChainAppend as ChainAppendV2, ChainAppend_JsonCodec as ChainAppendV2_JsonCodec, ChainAppend_UEBACodec as ChainAppendV2_UEBACodec } from '../generated/fwde2e/chain/v2_0_0/ChainAppend';

type Registration = { typeId: string; json: BaboonJsonCodec<unknown>; ueba: BaboonBinCodec<unknown> };
function versionModules(domain: string, version: string, regs: Registration[]) {
    class JsonCodecs extends AbstractBaboonJsonCodecs {
        constructor() { super(); for (const r of regs) this.register(r.typeId, new Lazy<BaboonJsonCodec<unknown>>(() => r.json)); }
    }
    class UebaCodecs extends AbstractBaboonUebaCodecs {
        constructor() { super(); for (const r of regs) this.register(r.typeId, new Lazy<BaboonBinCodec<unknown>>(() => r.ueba)); }
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
const reg = (typeId: string, json: unknown, ueba: unknown): Registration =>
    ({ typeId, json: json as BaboonJsonCodec<unknown>, ueba: ueba as BaboonBinCodec<unknown> });
const fwdV1 = versionModules('fwde2e.fwd', '1.0.0', [
    reg(FwdAppendVarV1.BaboonTypeIdentifier, FwdAppendVarV1_JsonCodec.instance, FwdAppendVarV1_UEBACodec.instance),
    reg(FwdMidInsertV1.BaboonTypeIdentifier, FwdMidInsertV1_JsonCodec.instance, FwdMidInsertV1_UEBACodec.instance),
    reg(FwdStableV1.BaboonTypeIdentifier, FwdStableV1_JsonCodec.instance, FwdStableV1_UEBACodec.instance),
    reg(FwdEnumHostV1.BaboonTypeIdentifier, FwdEnumHostV1_JsonCodec.instance, FwdEnumHostV1_UEBACodec.instance),
]);
const chainV1 = versionModules('fwde2e.chain', '1.0.0', [reg(ChainAppendV1.BaboonTypeIdentifier, ChainAppendV1_JsonCodec.instance, ChainAppendV1_UEBACodec.instance)]);
const chainV2 = versionModules('fwde2e.chain', '2.0.0', [reg(ChainAppendV2.BaboonTypeIdentifier, ChainAppendV2_JsonCodec.instance, ChainAppendV2_UEBACodec.instance)]);

function oldFwdReader(policy: ForwardReadPolicy): BaboonCodecsFacade { const f = new BaboonCodecsFacade(); fwdV1(f); f.forwardReadPolicy = policy; return f; }
function midChainReader(policy: ForwardReadPolicy): BaboonCodecsFacade { const f = new BaboonCodecsFacade(); chainV1(f); chainV2(f); f.forwardReadPolicy = policy; return f; }

const fwdWriter = new DomainFwde2eFwdFacade();
const chainWriter = new DomainFwde2eChainFacade();
const v2Compact = BaboonCodecContext.custom(false, ForwardWritePolicy.Strict, BaboonEnvelopeVersion.V2, undefined);
const v2Indexed = BaboonCodecContext.custom(true, ForwardWritePolicy.Strict, BaboonEnvelopeVersion.V2, undefined);

function unwrap<L, R>(e: BaboonEither<L, R>): R {
    if (e.tag !== 'Right') throw new Error(`expected Right, got Left: ${String((e as { value: unknown }).value)}`);
    return e.value;
}
function bin(writer: BaboonCodecsFacade, ctx: BaboonCodecContext, value: BaboonGenerated): Uint8Array { return unwrap(writer.encodeToBin(ctx, value)); }
function readMeta(bytes: Uint8Array): BaboonTypeMeta | undefined { return BaboonTypeMeta.readMeta(new BaboonBinReader(bytes)); }
function concat(...parts: Uint8Array[]): Uint8Array {
    const out = new Uint8Array(parts.reduce((n, p) => n + p.length, 0));
    let o = 0; for (const p of parts) { out.set(p, o); o += p.length; }
    return out;
}
function str(s: string): Uint8Array { const w = new BaboonBinWriter(); BinTools.writeString(w, s); return w.toBytes(); }
function fwdHead(metaVersion: number): Uint8Array {
    const w = new BaboonBinWriter(); BinTools.writeByte(w, metaVersion); BinTools.writeString(w, 'fwde2e.fwd'); BinTools.writeString(w, '2.0.0'); return w.toBytes();
}
const appended = new FwdAppendVar(42, 'hi', 't');
const appendedPayload = (() => { const w = new BaboonBinWriter(); FwdAppendVar_UEBACodec.instance.encode(BaboonCodecContext.Compact, appended, w); return w.toBytes(); })();

// Hand-assembled v2 envelope: 02 | domainId | domainVersion | flags | [minCompat] | [readableMin] | typeId | payload
function v2Envelope(flags: number, minCompat: string | undefined, readableMin: string | undefined, typeId: string, payload: Uint8Array): Uint8Array {
    const w = new BaboonBinWriter();
    BinTools.writeByte(w, 2); BinTools.writeString(w, 'fwde2e.fwd'); BinTools.writeString(w, '2.0.0'); BinTools.writeByte(w, flags);
    if (minCompat !== undefined) BinTools.writeString(w, minCompat);
    if (readableMin !== undefined) BinTools.writeString(w, readableMin);
    BinTools.writeString(w, typeId);
    return concat(w.toBytes(), payload);
}

describe('binary envelope metaVersion 2', () => {
    test('reader accepts a hand-assembled v2 envelope: readable-min lets a Tolerant old reader decode, Lossless refuses', () => {
        const env = v2Envelope(0x02, undefined, '1.0.0', FwdAppendVar.BaboonTypeIdentifier, appendedPayload);
        const meta = readMeta(env)!;
        expect(meta.metaVersion).toBe(2);
        expect(meta.domainVersionMinCompat).toBe('2.0.0');
        expect(meta.domainVersionReadableMin).toBe('1.0.0');
        const decoded = unwrap(oldFwdReader(ForwardReadPolicy.Tolerant).decodeFromBin(env)) as FwdAppendVarV1;
        expect(decoded).toBeInstanceOf(FwdAppendVarV1);
        expect(decoded.a).toBe(42);
        expect(oldFwdReader(ForwardReadPolicy.Lossless).decodeFromBin(env).tag).toBe('Left');
    });

    test('the default context still writes v1: envelopes are byte-identical to before', () => {
        expect(BaboonCodecContext.Compact.envelopeVersion).toBe(BaboonEnvelopeVersion.V1);
        expect(BaboonCodecContext.Indexed.envelopeVersion).toBe(BaboonEnvelopeVersion.V1);
        const env = bin(fwdWriter, BaboonCodecContext.Compact, appended);
        expect(env).toEqual(concat(fwdHead(1), Uint8Array.of(0), str(FwdAppendVar.BaboonTypeIdentifier), appendedPayload));
    });

    test('v2 writer publishes both bounds with JSON elision rules, per type', () => {
        const app = bin(fwdWriter, v2Compact, appended);
        expect(app).toEqual(concat(fwdHead(2), Uint8Array.of(0x02), str('1.0.0'), str(FwdAppendVar.BaboonTypeIdentifier), appendedPayload));
        const appMeta = readMeta(app)!;
        expect([appMeta.metaVersion, appMeta.domainVersionMinCompat, appMeta.domainVersionReadableMin]).toEqual([2, '2.0.0', '1.0.0']);

        const appIdx = bin(fwdWriter, v2Indexed, appended);
        const idxHead = concat(fwdHead(2), Uint8Array.of(0x00), str(FwdAppendVar.BaboonTypeIdentifier));
        expect(Array.from(appIdx.slice(0, idxHead.length))).toEqual(Array.from(idxHead));

        const stable = bin(fwdWriter, v2Compact, new FwdStable('s'));
        const stableHead = concat(fwdHead(2), Uint8Array.of(0x01), str('1.0.0'), str(FwdStable.BaboonTypeIdentifier));
        expect(Array.from(stable.slice(0, stableHead.length))).toEqual(Array.from(stableHead));

        for (const [value, typeId] of [[new FwdMidInsert(7, 99, 'z'), FwdMidInsert.BaboonTypeIdentifier], [new FwdEnumHost(FwdEnumGrows.C), FwdEnumHost.BaboonTypeIdentifier]] as const) {
            const env = bin(fwdWriter, v2Compact, value as BaboonGenerated);
            const head = concat(fwdHead(2), Uint8Array.of(0x00), str(typeId));
            expect(Array.from(env.slice(0, head.length))).toEqual(Array.from(head));
        }

        const v2Tolerant = BaboonCodecContext.custom(false, ForwardWritePolicy.Tolerant, BaboonEnvelopeVersion.V2, undefined);
        expect(bin(fwdWriter, v2Tolerant, appended)).toEqual(app);
    });

    test('v2 round-trips through the writer and through old readers under each policy', () => {
        const app = bin(fwdWriter, v2Compact, appended);
        expect((unwrap(fwdWriter.decodeFromBin(app)) as FwdAppendVar).t).toBe('t');
        expect((unwrap(oldFwdReader(ForwardReadPolicy.Tolerant).decodeFromBin(app)) as FwdAppendVarV1).b).toBe('hi');
        expect(oldFwdReader(ForwardReadPolicy.Lossless).decodeFromBin(app).tag).toBe('Left');

        const stable = bin(fwdWriter, v2Compact, new FwdStable('s'));
        expect((unwrap(oldFwdReader(ForwardReadPolicy.Lossless).decodeFromBin(stable)) as FwdStableV1).s).toBe('s');
        expect(oldFwdReader(ForwardReadPolicy.Tolerant).decodeFromBin(bin(fwdWriter, v2Compact, new FwdEnumHost(FwdEnumGrows.C))).tag).toBe('Left');
    });

    test('three-version chain in v2: Lossless is enforceable for binary, Tolerant decodes with the newest codec', () => {
        const env = bin(chainWriter, v2Compact, new ChainAppend(1, 'b', 'c'));
        const meta = readMeta(env)!;
        expect([meta.domainVersionMinCompat, meta.domainVersionReadableMin]).toEqual(['3.0.0', '1.0.0']);
        const mid = unwrap(midChainReader(ForwardReadPolicy.Tolerant).decodeFromBin(env)) as ChainAppendV2;
        expect(mid).toBeInstanceOf(ChainAppendV2);
        expect(mid.b).toBe('b');
        expect(midChainReader(ForwardReadPolicy.Lossless).decodeFromBin(env).tag).toBe('Left');
    });

    test('v2 readMeta rejects unknown flag bits; unknown metaVersions stay rejected', () => {
        expect(readMeta(v2Envelope(0x04, undefined, undefined, 'T', new Uint8Array()))).toBeUndefined();
        const both = readMeta(v2Envelope(0x03, '1.5.0', '1.0.0', 'T', new Uint8Array()))!;
        expect([both.domainVersionMinCompat, both.domainVersionReadableMin]).toEqual(['1.5.0', '1.0.0']);
        const w = new BaboonBinWriter(); BinTools.writeByte(w, 3); BinTools.writeString(w, 'd'); BinTools.writeString(w, '1.0.0');
        expect(readMeta(w.toBytes())).toBeUndefined();
    });
});
