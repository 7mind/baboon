import {type BaboonGeneratedLatest} from '../../../../../BaboonSharedRuntime'
import {BaboonBinReader, BaboonBinWriter, BaboonCodecContext, BinTools, Lazy} from '../../../../../BaboonSharedRuntime'

export class In implements BaboonGeneratedLatest {
    private readonly _orderId: string;
    private readonly _reason: string | undefined;

    constructor(orderId: string, reason: string | undefined) {
        this._orderId = orderId
        this._reason = reason
    }

    public get orderId(): string {
        return this._orderId;
    }
    public get reason(): string | undefined {
        return this._reason;
    }

    public toJSON(): Record<string, unknown> {
        return {
            orderId: this._orderId,
            reason: this._reason !== undefined ? this._reason : undefined
        };
    }

    public with(overrides: {orderId?: string; reason?: string | undefined}): In {
        return new In(
            'orderId' in overrides ? overrides.orderId! : this._orderId,
            'reason' in overrides ? overrides.reason! : this._reason
        );
    }

    public static fromPlain(obj: {orderId: string; reason: string | undefined}): In {
        return new In(
            obj.orderId,
            obj.reason
        );
    }

    public static readonly BaboonDomainVersion = '1.0.0'
    public baboonDomainVersion() {
        return In.BaboonDomainVersion
    }
    public static readonly BaboonDomainIdentifier = 'mcp.mux.stub'
    public baboonDomainIdentifier() {
        return In.BaboonDomainIdentifier
    }
    public static readonly BaboonTypeIdentifier = 'mcp.mux.stub/OrderService/cancelOrder#in'
    public baboonTypeIdentifier() {
        return In.BaboonTypeIdentifier
    }
    public static readonly BaboonSameInVersions = ["1.0.0"]
    public baboonSameInVersions() {
        return In.BaboonSameInVersions
    }
    public static jsonCodec(): In_JsonCodec {
        return In_JsonCodec.instance
    }
    public static binCodec(): In_UEBACodec {
        return In_UEBACodec.instance
    }
}

export class In_JsonCodec {
    public encode(ctx: BaboonCodecContext, value: In): unknown {
        if (this !== In_JsonCodec.lazyInstance.value) {
          return In_JsonCodec.lazyInstance.value.encode(ctx, value)
        }
    
        return {
            "orderId": value.orderId,
            "reason": value.reason === undefined ? null : value.reason,
        }
    }
    public decode(ctx: BaboonCodecContext, json: unknown): In {
        if (this !== In_JsonCodec .lazyInstance.value) {
            return In_JsonCodec.lazyInstance.value.decode(ctx, json)
        }
    
        const obj = json as Record<string, unknown>;
        return new In (
            obj["orderId"] as string,
            obj["reason"] === undefined || obj["reason"] === null ? undefined : obj["reason"] === undefined || obj["reason"] === null ? undefined : obj["reason"] as string,
        )
    }

    public static readonly BaboonDomainVersion = '1.0.0'
    public baboonDomainVersion() {
        return In_JsonCodec.BaboonDomainVersion
    }
    public static readonly BaboonDomainIdentifier = 'mcp.mux.stub'
    public baboonDomainIdentifier() {
        return In_JsonCodec.BaboonDomainIdentifier
    }
    public static readonly BaboonTypeIdentifier = 'mcp.mux.stub/OrderService/cancelOrder#in'
    public baboonTypeIdentifier() {
        return In_JsonCodec.BaboonTypeIdentifier
    }

    public static lazyInstance = new Lazy(() => new In_JsonCodec())
    public static get instance(): In_JsonCodec {
        return In_JsonCodec.lazyInstance.value
    }
}

export class In_UEBACodec {
    public encode(ctx: BaboonCodecContext, value: In, writer: BaboonBinWriter): void {
        if (this !== In_UEBACodec.lazyInstance.value) {
            In_UEBACodec.lazyInstance.value.encode(ctx, value, writer); return;
        }
    
        if (ctx === BaboonCodecContext.Indexed) {
            BinTools.writeByte(writer, 0x01);
            const buffer = new BaboonBinWriter();
            {
                const before = buffer.position();
                BinTools.writeI32(writer, before);
                BinTools.writeString(buffer, value.orderId);
                const after = buffer.position();
                BinTools.writeI32(writer, after - before);
            }
            {
                const before = buffer.position();
                BinTools.writeI32(writer, before);
                if (value.reason === undefined) {
                BinTools.writeByte(buffer, 0);
            } else {
                BinTools.writeByte(buffer, 1);
                BinTools.writeString(buffer, value.reason);
            }
                const after = buffer.position();
                BinTools.writeI32(writer, after - before);
            }
            writer.writeAll(buffer.toBytes());
        } else {
            BinTools.writeByte(writer, 0x00)
            BinTools.writeString(writer, value.orderId);
            if (value.reason === undefined) {
                BinTools.writeByte(writer, 0);
            } else {
                BinTools.writeByte(writer, 1);
                BinTools.writeString(writer, value.reason);
            }
        }
    }
    
    public decode(ctx: BaboonCodecContext, reader: BaboonBinReader): In {
        if (this !== In_UEBACodec .lazyInstance.value) {
            return In_UEBACodec.lazyInstance.value.decode(ctx, reader)
        }
    
        const header = BinTools.readByte(reader);
        const useIndices = header === 0x01;
        if (useIndices) {
            for (let i = 0; i < 2; i++) {
                BinTools.readI32(reader);
                BinTools.readI32(reader);
            }
        }
        const orderId = BinTools.readString(reader);
        const reason = (BinTools.readByte(reader) === 0 ? undefined : BinTools.readString(reader));
        return new In(
            orderId,
            reason,
        );
    }

    public static readonly BaboonDomainVersion = '1.0.0'
    public baboonDomainVersion() {
        return In_UEBACodec.BaboonDomainVersion
    }
    public static readonly BaboonDomainIdentifier = 'mcp.mux.stub'
    public baboonDomainIdentifier() {
        return In_UEBACodec.BaboonDomainIdentifier
    }
    public static readonly BaboonTypeIdentifier = 'mcp.mux.stub/OrderService/cancelOrder#in'
    public baboonTypeIdentifier() {
        return In_UEBACodec.BaboonTypeIdentifier
    }

    public static lazyInstance = new Lazy(() => new In_UEBACodec())
    public static get instance(): In_UEBACodec {
        return In_UEBACodec.lazyInstance.value
    }
}