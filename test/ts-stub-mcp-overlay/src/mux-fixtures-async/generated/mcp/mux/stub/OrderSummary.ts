import {type BaboonGeneratedLatest} from '../../../BaboonSharedRuntime'
import {BaboonBinReader, BaboonBinWriter, BaboonCodecContext, BinTools, Lazy} from '../../../BaboonSharedRuntime'
import {OrderStatus, OrderStatus_JsonCodec, OrderStatus_UEBACodec} from './OrderStatus'

export class OrderSummary implements BaboonGeneratedLatest {
    private readonly _orderId: string;
    private readonly _status: OrderStatus;
    private readonly _total: number;

    constructor(orderId: string, status: OrderStatus, total: number) {
        this._orderId = orderId
        this._status = status
        this._total = total
    }

    public get orderId(): string {
        return this._orderId;
    }
    public get status(): OrderStatus {
        return this._status;
    }
    public get total(): number {
        return this._total;
    }

    public toJSON(): Record<string, unknown> {
        return {
            orderId: this._orderId,
            status: this._status,
            total: this._total
        };
    }

    public with(overrides: {orderId?: string; status?: OrderStatus; total?: number}): OrderSummary {
        return new OrderSummary(
            'orderId' in overrides ? overrides.orderId! : this._orderId,
            'status' in overrides ? overrides.status! : this._status,
            'total' in overrides ? overrides.total! : this._total
        );
    }

    public static fromPlain(obj: {orderId: string; status: OrderStatus; total: number}): OrderSummary {
        return new OrderSummary(
            obj.orderId,
            obj.status,
            obj.total
        );
    }

    public static readonly BaboonDomainVersion = '1.0.0'
    public baboonDomainVersion() {
        return OrderSummary.BaboonDomainVersion
    }
    public static readonly BaboonDomainIdentifier = 'mcp.mux.stub'
    public baboonDomainIdentifier() {
        return OrderSummary.BaboonDomainIdentifier
    }
    public static readonly BaboonTypeIdentifier = 'mcp.mux.stub/:#OrderSummary'
    public baboonTypeIdentifier() {
        return OrderSummary.BaboonTypeIdentifier
    }
    public static readonly BaboonSameInVersions = ["1.0.0"]
    public baboonSameInVersions() {
        return OrderSummary.BaboonSameInVersions
    }
    public static jsonCodec(): OrderSummary_JsonCodec {
        return OrderSummary_JsonCodec.instance
    }
    public static binCodec(): OrderSummary_UEBACodec {
        return OrderSummary_UEBACodec.instance
    }
}

export class OrderSummary_JsonCodec {
    public encode(ctx: BaboonCodecContext, value: OrderSummary): unknown {
        if (this !== OrderSummary_JsonCodec.lazyInstance.value) {
          return OrderSummary_JsonCodec.lazyInstance.value.encode(ctx, value)
        }
    
        return {
            "orderId": value.orderId,
            "status": OrderStatus_JsonCodec.instance.encode(BaboonCodecContext.Default, value.status),
            "total": value.total,
        }
    }
    public decode(ctx: BaboonCodecContext, json: unknown): OrderSummary {
        if (this !== OrderSummary_JsonCodec .lazyInstance.value) {
            return OrderSummary_JsonCodec.lazyInstance.value.decode(ctx, json)
        }
    
        const obj = json as Record<string, unknown>;
        return new OrderSummary (
            obj["orderId"] as string,
            OrderStatus_JsonCodec.instance.decode(BaboonCodecContext.Default, obj["status"]),
            obj["total"] as number,
        )
    }

    public static readonly BaboonDomainVersion = '1.0.0'
    public baboonDomainVersion() {
        return OrderSummary_JsonCodec.BaboonDomainVersion
    }
    public static readonly BaboonDomainIdentifier = 'mcp.mux.stub'
    public baboonDomainIdentifier() {
        return OrderSummary_JsonCodec.BaboonDomainIdentifier
    }
    public static readonly BaboonTypeIdentifier = 'mcp.mux.stub/:#OrderSummary'
    public baboonTypeIdentifier() {
        return OrderSummary_JsonCodec.BaboonTypeIdentifier
    }

    public static lazyInstance = new Lazy(() => new OrderSummary_JsonCodec())
    public static get instance(): OrderSummary_JsonCodec {
        return OrderSummary_JsonCodec.lazyInstance.value
    }
}

export class OrderSummary_UEBACodec {
    public encode(ctx: BaboonCodecContext, value: OrderSummary, writer: BaboonBinWriter): void {
        if (this !== OrderSummary_UEBACodec.lazyInstance.value) {
            OrderSummary_UEBACodec.lazyInstance.value.encode(ctx, value, writer); return;
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
            OrderStatus_UEBACodec.instance.encode(ctx, value.status, buffer);
            BinTools.writeF64(buffer, value.total);
            writer.writeAll(buffer.toBytes());
        } else {
            BinTools.writeByte(writer, 0x00)
            BinTools.writeString(writer, value.orderId);
            OrderStatus_UEBACodec.instance.encode(ctx, value.status, writer);
            BinTools.writeF64(writer, value.total);
        }
    }
    
    public decode(ctx: BaboonCodecContext, reader: BaboonBinReader): OrderSummary {
        if (this !== OrderSummary_UEBACodec .lazyInstance.value) {
            return OrderSummary_UEBACodec.lazyInstance.value.decode(ctx, reader)
        }
    
        const header = BinTools.readByte(reader);
        const useIndices = header === 0x01;
        if (useIndices) {
            for (let i = 0; i < 1; i++) {
                BinTools.readI32(reader);
                BinTools.readI32(reader);
            }
        }
        const orderId = BinTools.readString(reader);
        const status = OrderStatus_UEBACodec.instance.decode(ctx, reader);
        const total = BinTools.readF64(reader);
        return new OrderSummary(
            orderId,
            status,
            total,
        );
    }

    public static readonly BaboonDomainVersion = '1.0.0'
    public baboonDomainVersion() {
        return OrderSummary_UEBACodec.BaboonDomainVersion
    }
    public static readonly BaboonDomainIdentifier = 'mcp.mux.stub'
    public baboonDomainIdentifier() {
        return OrderSummary_UEBACodec.BaboonDomainIdentifier
    }
    public static readonly BaboonTypeIdentifier = 'mcp.mux.stub/:#OrderSummary'
    public baboonTypeIdentifier() {
        return OrderSummary_UEBACodec.BaboonTypeIdentifier
    }

    public static lazyInstance = new Lazy(() => new OrderSummary_UEBACodec())
    public static get instance(): OrderSummary_UEBACodec {
        return OrderSummary_UEBACodec.lazyInstance.value
    }
}