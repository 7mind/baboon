// codec-envelope.md §2.1: the binary envelope's hasMinCompat flag is 0x00 (elided) or 0x01
// (min-compat string follows); "other flag values are illegal; readers reject them". A lenient
// reader would treat 0x02 as "absent" and then misparse the min-compat string as the type id.
import { describe, expect, test } from 'vitest';
import { BaboonBinReader, BaboonBinWriter, BaboonTypeMeta, BaboonTypeMetaCodec, BinTools } from '../generated/BaboonSharedRuntime';

function binEnvelope(flag: number, minCompat: string | undefined): Uint8Array {
    const w = new BaboonBinWriter();
    BinTools.writeByte(w, BaboonTypeMetaCodec.META_VERSION);
    BinTools.writeString(w, 'com.example.dom');
    BinTools.writeString(w, '2.0.0');
    BinTools.writeByte(w, flag);
    if (minCompat !== undefined) BinTools.writeString(w, minCompat);
    BinTools.writeString(w, 'MyType');
    return w.toBytes();
}

describe('binary envelope hasMinCompat flag byte', () => {
    test('flag 0 (elided) and 1 (present) are honoured', () => {
        expect(BaboonTypeMeta.readMeta(new BaboonBinReader(binEnvelope(0, undefined)))?.domainVersionMinCompat).toBe('2.0.0');
        expect(BaboonTypeMeta.readMeta(new BaboonBinReader(binEnvelope(1, '1.0.0')))?.domainVersionMinCompat).toBe('1.0.0');
    });

    test('unknown flag values are rejected instead of misparsed', () => {
        expect(BaboonTypeMeta.readMeta(new BaboonBinReader(binEnvelope(2, '1.0.0')))).toBeUndefined();
        expect(BaboonTypeMeta.readMeta(new BaboonBinReader(binEnvelope(0xff, undefined)))).toBeUndefined();
    });
});
