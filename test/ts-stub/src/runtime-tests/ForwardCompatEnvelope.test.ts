// NOTE: This test references generated symbols (src/generated/fwde2e/**) which are
// copied into this stub only by `mdl :build :test-gen-regular-adt` (rsync + codegen
// into target/test-regular/ts-stub/). Run from the codegen'd copy.
//
// End-to-end proof of the JSON envelope's `$rv` (readable-min) bound
// (docs/forward-compat.md, "Envelope integration"): a facade that registers ONLY the
// old 1.0.0 domain version decodes envelopes produced by the 2.0.0 facade exactly where
// `$rv` says it can, under the Tolerant policy — and refuses under Lossless or when no
// `$rv` was published.
import { describe, expect, test } from 'vitest';
import {
    AbstractBaboonConversions,
    AbstractBaboonJsonCodecs,
    AbstractBaboonUebaCodecs,
    BaboonBinCodec,
    BaboonDomainVersion,
    BaboonEither,
    BaboonJsonCodec,
    BaboonMeta,
    BaboonTypeMeta,
    Lazy,
} from '../generated/BaboonSharedRuntime';
import { BaboonCodecsFacade, ForwardReadPolicy } from '../generated/BaboonCodecsFacade';
import { DomainFwde2eFwdFacade } from '../generated/fwde2e/fwd/DomainFwde2eFwdFacade';

import { FwdAppendVar, FwdEnumHost, FwdMidInsert, FwdStable } from '../generated/fwde2e/fwd/index';
import { FwdEnumGrows } from '../generated/fwde2e/fwd/FwdEnumGrows';

import { FwdAppendVar as FwdAppendVarV1, FwdAppendVar_JsonCodec as FwdAppendVarV1_JsonCodec, FwdAppendVar_UEBACodec as FwdAppendVarV1_UEBACodec } from '../generated/fwde2e/fwd/v1_0_0/FwdAppendVar';
import { FwdMidInsert as FwdMidInsertV1, FwdMidInsert_JsonCodec as FwdMidInsertV1_JsonCodec, FwdMidInsert_UEBACodec as FwdMidInsertV1_UEBACodec } from '../generated/fwde2e/fwd/v1_0_0/FwdMidInsert';
import { FwdStable as FwdStableV1, FwdStable_JsonCodec as FwdStableV1_JsonCodec, FwdStable_UEBACodec as FwdStableV1_UEBACodec } from '../generated/fwde2e/fwd/v1_0_0/FwdStable';
import { FwdEnumHost as FwdEnumHostV1, FwdEnumHost_JsonCodec as FwdEnumHostV1_JsonCodec, FwdEnumHost_UEBACodec as FwdEnumHostV1_UEBACodec } from '../generated/fwde2e/fwd/v1_0_0/FwdEnumHost';

const DOMAIN = 'fwde2e.fwd';
const V1 = '1.0.0';

// The generated per-version codec registries are module-private, so the old reader is
// assembled from the exported per-type 1.0.0 codecs (mirrors AnyRoundTrip.test.ts).
class V1JsonCodecs extends AbstractBaboonJsonCodecs {
    constructor() {
        super();
        this.register(FwdAppendVarV1.BaboonTypeIdentifier, new Lazy<BaboonJsonCodec<unknown>>(() => FwdAppendVarV1_JsonCodec.instance as unknown as BaboonJsonCodec<unknown>));
        this.register(FwdMidInsertV1.BaboonTypeIdentifier, new Lazy<BaboonJsonCodec<unknown>>(() => FwdMidInsertV1_JsonCodec.instance as unknown as BaboonJsonCodec<unknown>));
        this.register(FwdStableV1.BaboonTypeIdentifier, new Lazy<BaboonJsonCodec<unknown>>(() => FwdStableV1_JsonCodec.instance as unknown as BaboonJsonCodec<unknown>));
        this.register(FwdEnumHostV1.BaboonTypeIdentifier, new Lazy<BaboonJsonCodec<unknown>>(() => FwdEnumHostV1_JsonCodec.instance as unknown as BaboonJsonCodec<unknown>));
    }
}

class V1UebaCodecs extends AbstractBaboonUebaCodecs {
    constructor() {
        super();
        this.register(FwdAppendVarV1.BaboonTypeIdentifier, new Lazy<BaboonBinCodec<unknown>>(() => FwdAppendVarV1_UEBACodec.instance as unknown as BaboonBinCodec<unknown>));
        this.register(FwdMidInsertV1.BaboonTypeIdentifier, new Lazy<BaboonBinCodec<unknown>>(() => FwdMidInsertV1_UEBACodec.instance as unknown as BaboonBinCodec<unknown>));
        this.register(FwdStableV1.BaboonTypeIdentifier, new Lazy<BaboonBinCodec<unknown>>(() => FwdStableV1_UEBACodec.instance as unknown as BaboonBinCodec<unknown>));
        this.register(FwdEnumHostV1.BaboonTypeIdentifier, new Lazy<BaboonBinCodec<unknown>>(() => FwdEnumHostV1_UEBACodec.instance as unknown as BaboonBinCodec<unknown>));
    }
}

class V1Conversions implements AbstractBaboonConversions {
    public versionsFrom(): string[] { return []; }
    public versionTo(): string { return V1; }
}

class V1Meta implements BaboonMeta {
    public sameInVersions(_typeId: string): string[] { return [V1]; }
    public forwardReadableVersions(_typeId: string): { readonly [version: string]: string } { return { [V1]: 'identical' }; }
}

// a reader deployed before 2.0.0 existed: knows fwde2e.fwd 1.0.0 only
function oldReader(policy: ForwardReadPolicy): BaboonCodecsFacade {
    const f = new BaboonCodecsFacade();
    f.register(new BaboonDomainVersion(DOMAIN, V1), () => new V1JsonCodecs(), () => new V1UebaCodecs(), () => new V1Conversions(), () => new V1Meta());
    f.forwardReadPolicy = policy;
    return f;
}

const writer = new DomainFwde2eFwdFacade();

function unwrap<L, R>(e: BaboonEither<L, R>): R {
    if (e.tag !== 'Right') throw new Error(`expected Right, got Left: ${String((e as { value: unknown }).value)}`);
    return e.value;
}

function envelope(value: FwdAppendVar | FwdMidInsert | FwdStable | FwdEnumHost): Record<string, unknown> {
    return unwrap(writer.encodeToJson(value)) as Record<string, unknown>;
}

describe('JSON envelope $rv (fwde2e.fwd)', () => {
    test('writer publishes $rv only where the json-additive bound is older than the identical bound', () => {
        const appended = envelope(new FwdAppendVar(42, 'hello', 'appended'));
        expect(appended['$v']).toBe('2.0.0');
        expect(appended['$uv']).toBeUndefined();
        expect(appended['$rv']).toBe('1.0.0');

        expect(envelope(new FwdMidInsert(7, 99, 'zed'))['$rv']).toBe('1.0.0');

        const stable = envelope(new FwdStable('same'));
        expect(stable['$uv']).toBe('1.0.0');
        expect(stable['$rv']).toBeUndefined();

        const enumHost = envelope(new FwdEnumHost(FwdEnumGrows.C));
        expect(enumHost['$uv']).toBeUndefined();
        expect(enumHost['$rv']).toBeUndefined();

        expect(FwdAppendVar.BaboonMinReaderVersions['identical']).toBe(FwdAppendVar.BaboonSameInVersions[0]);
        expect(FwdAppendVar.BaboonMinReaderVersions['json-additive']).toBe('1.0.0');
    });

    test('Tolerant old reader decodes newer json-additive payloads with its own codec, dropping unknown fields', () => {
        const reader = oldReader(ForwardReadPolicy.Tolerant);

        const appended = unwrap(reader.decodeFromJson(envelope(new FwdAppendVar(42, 'hello', 'appended')))) as FwdAppendVarV1;
        expect(appended).toBeInstanceOf(FwdAppendVarV1);
        expect(appended.a).toBe(42);
        expect(appended.b).toBe('hello');

        const inserted = unwrap(reader.decodeFromJson(envelope(new FwdMidInsert(7, 99, 'zed')))) as FwdMidInsertV1;
        expect(inserted.a).toBe(7);
        expect(inserted.z).toBe('zed');

        const stable = unwrap(reader.decodeFromJson(envelope(new FwdStable('same')))) as FwdStableV1;
        expect(stable.s).toBe('same');
    });

    test('Lossless old reader refuses newer payloads unless byte-identical', () => {
        const reader = oldReader(ForwardReadPolicy.Lossless);
        expect(reader.decodeFromJson(envelope(new FwdAppendVar(42, 'hello', 'appended'))).tag).toBe('Left');
        const stable = unwrap(reader.decodeFromJson(envelope(new FwdStable('same')))) as FwdStableV1;
        expect(stable.s).toBe('same');
    });

    test('no published bound: the old reader refuses under either policy', () => {
        const enumHost = envelope(new FwdEnumHost(FwdEnumGrows.C));
        expect(oldReader(ForwardReadPolicy.Tolerant).decodeFromJson(enumHost).tag).toBe('Left');
        expect(oldReader(ForwardReadPolicy.Lossless).decodeFromJson(enumHost).tag).toBe('Left');
    });

    test('envelope round-trips $rv through readMetaJson', () => {
        const meta = BaboonTypeMeta.readMetaJson(envelope(new FwdAppendVar(1, 'b', 't')))!;
        expect(meta.domainVersion).toBe('2.0.0');
        expect(meta.domainVersionMinCompat).toBe('2.0.0');
        expect(meta.domainVersionReadableMin).toBe('1.0.0');
        expect(meta.versionReadableMin()?.domainVersion).toBe('1.0.0');
        expect(meta.versionMinCompat()).toBeUndefined();
        // five-argument construction keeps readable-min = minCompat (pre-$rv envelopes)
        const legacy = new BaboonTypeMeta(1, DOMAIN, '2.0.0', '1.5.0', 'T');
        expect(legacy.domainVersionReadableMin).toBe('1.5.0');
    });
});
