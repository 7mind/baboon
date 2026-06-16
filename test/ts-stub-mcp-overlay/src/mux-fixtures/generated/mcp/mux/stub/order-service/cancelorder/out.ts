import {type BaboonGeneratedLatest} from '../../../../../BaboonSharedRuntime'
import {BaboonBinReader, BaboonBinWriter, BaboonCodecContext, BinTools, Lazy} from '../../../../../BaboonSharedRuntime'

export class Out implements BaboonGeneratedLatest {
    private readonly _ok: boolean;

    constructor(ok: boolean) {
        this._ok = ok
    }

    public get ok(): boolean {
        return this._ok;
    }

    public toJSON(): Record<string, unknown> {
        return {
            ok: this._ok
        };
    }

    public with(overrides: {ok?: boolean}): Out {
        return new Out(
            'ok' in overrides ? overrides.ok! : this._ok
        );
    }

    public static fromPlain(obj: {ok: boolean}): Out {
        return new Out(
            obj.ok
        );
    }

    public static readonly BaboonDomainVersion = '1.0.0'
    public baboonDomainVersion() {
        return Out.BaboonDomainVersion
    }
    public static readonly BaboonDomainIdentifier = 'mcp.mux.stub'
    public baboonDomainIdentifier() {
        return Out.BaboonDomainIdentifier
    }
    public static readonly BaboonTypeIdentifier = 'mcp.mux.stub/OrderService/cancelOrder#out'
    public baboonTypeIdentifier() {
        return Out.BaboonTypeIdentifier
    }
    public static readonly BaboonSameInVersions = ["1.0.0"]
    public baboonSameInVersions() {
        return Out.BaboonSameInVersions
    }
    public static jsonCodec(): Out_JsonCodec {
        return Out_JsonCodec.instance
    }
    public static binCodec(): Out_UEBACodec {
        return Out_UEBACodec.instance
    }
}

export class Out_JsonCodec {
    public encode(ctx: BaboonCodecContext, value: Out): unknown {
        if (this !== Out_JsonCodec.lazyInstance.value) {
          return Out_JsonCodec.lazyInstance.value.encode(ctx, value)
        }
    
        return {
            "ok": value.ok,
        }
    }
    public decode(ctx: BaboonCodecContext, json: unknown): Out {
        if (this !== Out_JsonCodec .lazyInstance.value) {
            return Out_JsonCodec.lazyInstance.value.decode(ctx, json)
        }
    
        const obj = json as Record<string, unknown>;
        return new Out (
            obj["ok"] as boolean,
        )
    }

    public static readonly BaboonDomainVersion = '1.0.0'
    public baboonDomainVersion() {
        return Out_JsonCodec.BaboonDomainVersion
    }
    public static readonly BaboonDomainIdentifier = 'mcp.mux.stub'
    public baboonDomainIdentifier() {
        return Out_JsonCodec.BaboonDomainIdentifier
    }
    public static readonly BaboonTypeIdentifier = 'mcp.mux.stub/OrderService/cancelOrder#out'
    public baboonTypeIdentifier() {
        return Out_JsonCodec.BaboonTypeIdentifier
    }

    public static lazyInstance = new Lazy(() => new Out_JsonCodec())
    public static get instance(): Out_JsonCodec {
        return Out_JsonCodec.lazyInstance.value
    }
}

export class Out_UEBACodec {
    public encode(ctx: BaboonCodecContext, value: Out, writer: BaboonBinWriter): void {
        if (this !== Out_UEBACodec.lazyInstance.value) {
            Out_UEBACodec.lazyInstance.value.encode(ctx, value, writer); return;
        }
    
        if (ctx === BaboonCodecContext.Indexed) {
            BinTools.writeByte(writer, 0x01);
            const buffer = new BaboonBinWriter();
            BinTools.writeBool(buffer, value.ok);
            writer.writeAll(buffer.toBytes());
        } else {
            BinTools.writeByte(writer, 0x00)
            BinTools.writeBool(writer, value.ok);
        }
    }
    
    public decode(ctx: BaboonCodecContext, reader: BaboonBinReader): Out {
        if (this !== Out_UEBACodec .lazyInstance.value) {
            return Out_UEBACodec.lazyInstance.value.decode(ctx, reader)
        }
    
        const header = BinTools.readByte(reader);
        const useIndices = header === 0x01;
        if (useIndices) {
            for (let i = 0; i < 0; i++) {
                BinTools.readI32(reader);
                BinTools.readI32(reader);
            }
        }
        const ok = BinTools.readBool(reader);
        return new Out(
            ok,
        );
    }

    public static readonly BaboonDomainVersion = '1.0.0'
    public baboonDomainVersion() {
        return Out_UEBACodec.BaboonDomainVersion
    }
    public static readonly BaboonDomainIdentifier = 'mcp.mux.stub'
    public baboonDomainIdentifier() {
        return Out_UEBACodec.BaboonDomainIdentifier
    }
    public static readonly BaboonTypeIdentifier = 'mcp.mux.stub/OrderService/cancelOrder#out'
    public baboonTypeIdentifier() {
        return Out_UEBACodec.BaboonTypeIdentifier
    }

    public static lazyInstance = new Lazy(() => new Out_UEBACodec())
    public static get instance(): Out_UEBACodec {
        return Out_UEBACodec.lazyInstance.value
    }
}