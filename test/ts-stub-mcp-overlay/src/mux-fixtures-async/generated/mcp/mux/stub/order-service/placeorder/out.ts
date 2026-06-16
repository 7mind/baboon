import {type BaboonGeneratedLatest} from '../../../../../BaboonSharedRuntime'
import {BaboonBinReader, BaboonBinWriter, BaboonCodecContext, BinTools, Lazy} from '../../../../../BaboonSharedRuntime'
import {OrderSummary, OrderSummary_JsonCodec, OrderSummary_UEBACodec} from '../../OrderSummary'

export class Out implements BaboonGeneratedLatest {
    private readonly _summary: OrderSummary;

    constructor(summary: OrderSummary) {
        this._summary = summary
    }

    public get summary(): OrderSummary {
        return this._summary;
    }

    public toJSON(): Record<string, unknown> {
        return {
            summary: this._summary
        };
    }

    public with(overrides: {summary?: OrderSummary}): Out {
        return new Out(
            'summary' in overrides ? overrides.summary! : this._summary
        );
    }

    public static fromPlain(obj: {summary: OrderSummary}): Out {
        return new Out(
            obj.summary
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
    public static readonly BaboonTypeIdentifier = 'mcp.mux.stub/OrderService/placeOrder#out'
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
            "summary": OrderSummary_JsonCodec.instance.encode(BaboonCodecContext.Default, value.summary),
        }
    }
    public decode(ctx: BaboonCodecContext, json: unknown): Out {
        if (this !== Out_JsonCodec .lazyInstance.value) {
            return Out_JsonCodec.lazyInstance.value.decode(ctx, json)
        }
    
        const obj = json as Record<string, unknown>;
        return new Out (
            OrderSummary_JsonCodec.instance.decode(BaboonCodecContext.Default, obj["summary"]),
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
    public static readonly BaboonTypeIdentifier = 'mcp.mux.stub/OrderService/placeOrder#out'
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
            {
                const before = buffer.position();
                BinTools.writeI32(writer, before);
                OrderSummary_UEBACodec.instance.encode(ctx, value.summary, buffer);
                const after = buffer.position();
                BinTools.writeI32(writer, after - before);
            }
            writer.writeAll(buffer.toBytes());
        } else {
            BinTools.writeByte(writer, 0x00)
            OrderSummary_UEBACodec.instance.encode(ctx, value.summary, writer);
        }
    }
    
    public decode(ctx: BaboonCodecContext, reader: BaboonBinReader): Out {
        if (this !== Out_UEBACodec .lazyInstance.value) {
            return Out_UEBACodec.lazyInstance.value.decode(ctx, reader)
        }
    
        const header = BinTools.readByte(reader);
        const useIndices = header === 0x01;
        if (useIndices) {
            for (let i = 0; i < 1; i++) {
                BinTools.readI32(reader);
                BinTools.readI32(reader);
            }
        }
        const summary = OrderSummary_UEBACodec.instance.decode(ctx, reader);
        return new Out(
            summary,
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
    public static readonly BaboonTypeIdentifier = 'mcp.mux.stub/OrderService/placeOrder#out'
    public baboonTypeIdentifier() {
        return Out_UEBACodec.BaboonTypeIdentifier
    }

    public static lazyInstance = new Lazy(() => new Out_UEBACodec())
    public static get instance(): Out_UEBACodec {
        return Out_UEBACodec.lazyInstance.value
    }
}