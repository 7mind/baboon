// NOTE: This test references generated symbols (src/generated/fwde2e/**) which are
// copied into this stub only by `mdl :build :test-gen-regular-adt` (rsync + codegen
// into target/test-regular/ts-stub/). Run from the codegen'd copy.
//
// End-to-end proof of the forward-compatibility metadata
// (docs/drafts/20260911-0937-forward-compat-metadata.md): a codec generated for an
// OLDER version decodes blobs produced by a NEWER version's codec, exactly where the
// emitted `BaboonForwardReadable` metadata promises it.
import { describe, expect, test } from 'vitest';
import { BaboonBinReader, BaboonBinWriter, BaboonCodecContext, BaboonGenerated } from '../generated/BaboonSharedRuntime';

import { FwdAppendVar, FwdAppendVar_UEBACodec, FwdAppendVar_JsonCodec } from '../generated/fwde2e/fwd/FwdAppendVar';
import { FwdMidInsert, FwdMidInsert_UEBACodec, FwdMidInsert_JsonCodec } from '../generated/fwde2e/fwd/FwdMidInsert';
import { FwdStable, FwdStable_UEBACodec, FwdStable_JsonCodec } from '../generated/fwde2e/fwd/FwdStable';
import { FwdEnumHost, FwdEnumHost_UEBACodec, FwdEnumHost_JsonCodec } from '../generated/fwde2e/fwd/FwdEnumHost';
import { FwdEnumGrows } from '../generated/fwde2e/fwd/FwdEnumGrows';

import { FwdAppendVar as FwdAppendVarV1, FwdAppendVar_UEBACodec as FwdAppendVarV1_UEBACodec, FwdAppendVar_JsonCodec as FwdAppendVarV1_JsonCodec } from '../generated/fwde2e/fwd/v1_0_0/FwdAppendVar';
import { FwdMidInsert as FwdMidInsertV1, FwdMidInsert_UEBACodec as FwdMidInsertV1_UEBACodec, FwdMidInsert_JsonCodec as FwdMidInsertV1_JsonCodec } from '../generated/fwde2e/fwd/v1_0_0/FwdMidInsert';
import { FwdStable as FwdStableV1, FwdStable_UEBACodec as FwdStableV1_UEBACodec, FwdStable_JsonCodec as FwdStableV1_JsonCodec } from '../generated/fwde2e/fwd/v1_0_0/FwdStable';
import { FwdEnumHost as FwdEnumHostV1, FwdEnumHost_UEBACodec as FwdEnumHostV1_UEBACodec, FwdEnumHost_JsonCodec as FwdEnumHostV1_JsonCodec } from '../generated/fwde2e/fwd/v1_0_0/FwdEnumHost';

const ctx = BaboonCodecContext.Compact;

function uebaBytes<T>(codec: { encode(ctx: BaboonCodecContext, value: T, writer: BaboonBinWriter): void }, value: T): Uint8Array {
    const writer = new BaboonBinWriter();
    codec.encode(ctx, value, writer);
    return writer.toBytes();
}

describe('forward-compat metadata (fwde2e.fwd)', () => {
    test('emitted forward metadata carries the expected tiers', () => {
        expect(FwdAppendVarV1.BaboonForwardReadable).toStrictEqual({ '1.0.0': 'identical', '2.0.0': 'prefix-compact' });
        expect(FwdMidInsertV1.BaboonForwardReadable).toStrictEqual({ '1.0.0': 'identical', '2.0.0': 'json-additive' });
        expect(FwdStableV1.BaboonForwardReadable).toStrictEqual({ '1.0.0': 'identical', '2.0.0': 'identical' });
        expect(FwdEnumHostV1.BaboonForwardReadable).toStrictEqual({ '1.0.0': 'identical' });
        // latest version: no successors yet
        expect(FwdAppendVar.BaboonForwardReadable).toStrictEqual({ '2.0.0': 'identical' });
        // instance accessor mirrors the static
        const v1: BaboonGenerated = new FwdAppendVarV1(1, 'x');
        expect(v1.baboonForwardReadable()).toStrictEqual(FwdAppendVarV1.BaboonForwardReadable);
    });

    test('PREFIX_COMPACT: old UEBA codec prefix-reads a compact blob with appended fields', () => {
        const value = new FwdAppendVar(42, 'hello', 'appended');
        const reader = new BaboonBinReader(uebaBytes(FwdAppendVar_UEBACodec.instance, value));
        const decoded = FwdAppendVarV1_UEBACodec.instance.decode(ctx, reader);
        expect(decoded.a).toBe(42);
        expect(decoded.b).toBe('hello');
        // prefix semantics: the appended field's bytes are left unconsumed
        expect(() => reader.readByte()).not.toThrow();
    });

    test('JSON_ADDITIVE: old JSON codec reads mid-inserted and appended fields at any position', () => {
        const inserted = new FwdMidInsert(7, 99, 'zed');
        const decMid = FwdMidInsertV1_JsonCodec.instance.decode(ctx, FwdMidInsert_JsonCodec.instance.encode(ctx, inserted));
        expect(decMid.a).toBe(7);
        expect(decMid.z).toBe('zed');

        const appended = new FwdAppendVar(1, 'b', 't');
        const decApp = FwdAppendVarV1_JsonCodec.instance.decode(ctx, FwdAppendVar_JsonCodec.instance.encode(ctx, appended));
        expect(decApp.a).toBe(1);
        expect(decApp.b).toBe('b');
    });

    test('JSON_ADDITIVE does NOT extend to UEBA: mid-inserted field desyncs the positional read', () => {
        const value = new FwdMidInsert(7, 99, 'zed');
        const reader = new BaboonBinReader(uebaBytes(FwdMidInsert_UEBACodec.instance, value));
        let desynced = false;
        try {
            const decoded = FwdMidInsertV1_UEBACodec.instance.decode(ctx, reader);
            // the old codec reads the inserted opt bytes as the `z` string: garbage or failure
            desynced = decoded.z !== 'zed';
        } catch {
            desynced = true;
        }
        expect(desynced).toBe(true);
    });

    test('IDENTICAL: unchanged type reads exactly through the old codec', () => {
        const value = new FwdStable('same');
        const reader = new BaboonBinReader(uebaBytes(FwdStable_UEBACodec.instance, value));
        const decoded = FwdStableV1_UEBACodec.instance.decode(ctx, reader);
        expect(decoded.s).toBe('same');
        // identical type must consume the blob exactly
        expect(() => reader.readByte()).toThrow();

        const json = FwdStable_JsonCodec.instance.encode(ctx, value);
        expect(FwdStableV1_JsonCodec.instance.decode(ctx, json).s).toBe('same');
    });

    test('negative control: a grown enum value is NOT readable by the old codec', () => {
        const value = new FwdEnumHost(FwdEnumGrows.C);

        expect(() => FwdEnumHostV1_JsonCodec.instance.decode(ctx, FwdEnumHost_JsonCodec.instance.encode(ctx, value))).toThrow();
        expect(() => FwdEnumHostV1_UEBACodec.instance.decode(ctx, new BaboonBinReader(uebaBytes(FwdEnumHost_UEBACodec.instance, value)))).toThrow();
    });
});
