import {BaboonBinReader, BaboonBinWriter, BaboonCodecContext, BaboonDecoderFailure, BinTools, Lazy} from '../../../BaboonSharedRuntime'

export enum OrderStatus {
    Pending = "Pending",
    Confirmed = "Confirmed",
    Shipped = "Shipped",
    Cancelled = "Cancelled"
}

export const OrderStatus_values: ReadonlyArray<OrderStatus> = [
    OrderStatus.Pending,
    OrderStatus.Confirmed,
    OrderStatus.Shipped,
    OrderStatus.Cancelled
] as const;

export function OrderStatus_parse(s: string): OrderStatus {
    const found = OrderStatus_values.find(v => v === s);
    if (found === undefined) {
        throw new BaboonDecoderFailure("Unknown OrderStatus variant: " + s);
    }
    return found;
}

export class OrderStatus_JsonCodec {
    public encode(ctx: BaboonCodecContext, value: OrderStatus): unknown {
        if (this !== OrderStatus_JsonCodec.lazyInstance.value) {
          return OrderStatus_JsonCodec.lazyInstance.value.encode(ctx, value)
        }
    
        return value
    }
    public decode(ctx: BaboonCodecContext, json: unknown): OrderStatus {
        if (this !== OrderStatus_JsonCodec .lazyInstance.value) {
            return OrderStatus_JsonCodec.lazyInstance.value.decode(ctx, json)
        }
    
        return OrderStatus_parse(json as string)
    }

    public static readonly BaboonDomainVersion = '1.0.0'
    public baboonDomainVersion() {
        return OrderStatus_JsonCodec.BaboonDomainVersion
    }
    public static readonly BaboonDomainIdentifier = 'mcp.mux.stub'
    public baboonDomainIdentifier() {
        return OrderStatus_JsonCodec.BaboonDomainIdentifier
    }
    public static readonly BaboonTypeIdentifier = 'mcp.mux.stub/:#OrderStatus'
    public baboonTypeIdentifier() {
        return OrderStatus_JsonCodec.BaboonTypeIdentifier
    }

    public static lazyInstance = new Lazy(() => new OrderStatus_JsonCodec())
    public static get instance(): OrderStatus_JsonCodec {
        return OrderStatus_JsonCodec.lazyInstance.value
    }
}

export class OrderStatus_UEBACodec {
    public encode(ctx: BaboonCodecContext, value: OrderStatus, writer: BaboonBinWriter): void {
        if (this !== OrderStatus_UEBACodec.lazyInstance.value) {
            OrderStatus_UEBACodec.lazyInstance.value.encode(ctx, value, writer); return;
        }
    
        switch (value) {
            case "Pending": BinTools.writeByte(writer, 0); break;
                case "Confirmed": BinTools.writeByte(writer, 1); break;
                case "Shipped": BinTools.writeByte(writer, 2); break;
                case "Cancelled": BinTools.writeByte(writer, 3); break;
            default: throw new Error("Unknown enum variant: " + value);
        }
    }
    
    public decode(ctx: BaboonCodecContext, reader: BaboonBinReader): OrderStatus {
        if (this !== OrderStatus_UEBACodec .lazyInstance.value) {
            return OrderStatus_UEBACodec.lazyInstance.value.decode(ctx, reader)
        }
    
        const tag = BinTools.readByte(reader);
        switch (tag) {
            case 0: return "Pending" as OrderStatus;
                case 1: return "Confirmed" as OrderStatus;
                case 2: return "Shipped" as OrderStatus;
                case 3: return "Cancelled" as OrderStatus;
            default: throw new Error("Unknown enum variant tag: " + tag);
        }
    }

    public static readonly BaboonDomainVersion = '1.0.0'
    public baboonDomainVersion() {
        return OrderStatus_UEBACodec.BaboonDomainVersion
    }
    public static readonly BaboonDomainIdentifier = 'mcp.mux.stub'
    public baboonDomainIdentifier() {
        return OrderStatus_UEBACodec.BaboonDomainIdentifier
    }
    public static readonly BaboonTypeIdentifier = 'mcp.mux.stub/:#OrderStatus'
    public baboonTypeIdentifier() {
        return OrderStatus_UEBACodec.BaboonTypeIdentifier
    }

    public static lazyInstance = new Lazy(() => new OrderStatus_UEBACodec())
    public static get instance(): OrderStatus_UEBACodec {
        return OrderStatus_UEBACodec.lazyInstance.value
    }
}