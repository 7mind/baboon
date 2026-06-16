import {type BaboonGeneratedLatest} from '../../../BaboonSharedRuntime'
import {BaboonBinReader, BaboonBinWriter, BaboonCodecContext, BinTools, Lazy} from '../../../BaboonSharedRuntime'

export class OrderItem implements BaboonGeneratedLatest {
    private readonly _productId: string;
    private readonly _quantity: number;
    private readonly _unitPrice: number;

    constructor(productId: string, quantity: number, unitPrice: number) {
        this._productId = productId
        this._quantity = quantity
        this._unitPrice = unitPrice
    }

    public get productId(): string {
        return this._productId;
    }
    public get quantity(): number {
        return this._quantity;
    }
    public get unitPrice(): number {
        return this._unitPrice;
    }

    public toJSON(): Record<string, unknown> {
        return {
            productId: this._productId,
            quantity: this._quantity,
            unitPrice: this._unitPrice
        };
    }

    public with(overrides: {productId?: string; quantity?: number; unitPrice?: number}): OrderItem {
        return new OrderItem(
            'productId' in overrides ? overrides.productId! : this._productId,
            'quantity' in overrides ? overrides.quantity! : this._quantity,
            'unitPrice' in overrides ? overrides.unitPrice! : this._unitPrice
        );
    }

    public static fromPlain(obj: {productId: string; quantity: number; unitPrice: number}): OrderItem {
        return new OrderItem(
            obj.productId,
            obj.quantity,
            obj.unitPrice
        );
    }

    public static readonly BaboonDomainVersion = '1.0.0'
    public baboonDomainVersion() {
        return OrderItem.BaboonDomainVersion
    }
    public static readonly BaboonDomainIdentifier = 'mcp.mux.stub'
    public baboonDomainIdentifier() {
        return OrderItem.BaboonDomainIdentifier
    }
    public static readonly BaboonTypeIdentifier = 'mcp.mux.stub/:#OrderItem'
    public baboonTypeIdentifier() {
        return OrderItem.BaboonTypeIdentifier
    }
    public static readonly BaboonSameInVersions = ["1.0.0"]
    public baboonSameInVersions() {
        return OrderItem.BaboonSameInVersions
    }
    public static jsonCodec(): OrderItem_JsonCodec {
        return OrderItem_JsonCodec.instance
    }
    public static binCodec(): OrderItem_UEBACodec {
        return OrderItem_UEBACodec.instance
    }
}

export class OrderItem_JsonCodec {
    public encode(ctx: BaboonCodecContext, value: OrderItem): unknown {
        if (this !== OrderItem_JsonCodec.lazyInstance.value) {
          return OrderItem_JsonCodec.lazyInstance.value.encode(ctx, value)
        }
    
        return {
            "productId": value.productId,
            "quantity": value.quantity,
            "unitPrice": value.unitPrice,
        }
    }
    public decode(ctx: BaboonCodecContext, json: unknown): OrderItem {
        if (this !== OrderItem_JsonCodec .lazyInstance.value) {
            return OrderItem_JsonCodec.lazyInstance.value.decode(ctx, json)
        }
    
        const obj = json as Record<string, unknown>;
        return new OrderItem (
            obj["productId"] as string,
            obj["quantity"] as number,
            obj["unitPrice"] as number,
        )
    }

    public static readonly BaboonDomainVersion = '1.0.0'
    public baboonDomainVersion() {
        return OrderItem_JsonCodec.BaboonDomainVersion
    }
    public static readonly BaboonDomainIdentifier = 'mcp.mux.stub'
    public baboonDomainIdentifier() {
        return OrderItem_JsonCodec.BaboonDomainIdentifier
    }
    public static readonly BaboonTypeIdentifier = 'mcp.mux.stub/:#OrderItem'
    public baboonTypeIdentifier() {
        return OrderItem_JsonCodec.BaboonTypeIdentifier
    }

    public static lazyInstance = new Lazy(() => new OrderItem_JsonCodec())
    public static get instance(): OrderItem_JsonCodec {
        return OrderItem_JsonCodec.lazyInstance.value
    }
}

export class OrderItem_UEBACodec {
    public encode(ctx: BaboonCodecContext, value: OrderItem, writer: BaboonBinWriter): void {
        if (this !== OrderItem_UEBACodec.lazyInstance.value) {
            OrderItem_UEBACodec.lazyInstance.value.encode(ctx, value, writer); return;
        }
    
        if (ctx === BaboonCodecContext.Indexed) {
            BinTools.writeByte(writer, 0x01);
            const buffer = new BaboonBinWriter();
            {
                const before = buffer.position();
                BinTools.writeI32(writer, before);
                BinTools.writeString(buffer, value.productId);
                const after = buffer.position();
                BinTools.writeI32(writer, after - before);
            }
            BinTools.writeI32(buffer, value.quantity);
            BinTools.writeF64(buffer, value.unitPrice);
            writer.writeAll(buffer.toBytes());
        } else {
            BinTools.writeByte(writer, 0x00)
            BinTools.writeString(writer, value.productId);
            BinTools.writeI32(writer, value.quantity);
            BinTools.writeF64(writer, value.unitPrice);
        }
    }
    
    public decode(ctx: BaboonCodecContext, reader: BaboonBinReader): OrderItem {
        if (this !== OrderItem_UEBACodec .lazyInstance.value) {
            return OrderItem_UEBACodec.lazyInstance.value.decode(ctx, reader)
        }
    
        const header = BinTools.readByte(reader);
        const useIndices = header === 0x01;
        if (useIndices) {
            for (let i = 0; i < 1; i++) {
                BinTools.readI32(reader);
                BinTools.readI32(reader);
            }
        }
        const productId = BinTools.readString(reader);
        const quantity = BinTools.readI32(reader);
        const unitPrice = BinTools.readF64(reader);
        return new OrderItem(
            productId,
            quantity,
            unitPrice,
        );
    }

    public static readonly BaboonDomainVersion = '1.0.0'
    public baboonDomainVersion() {
        return OrderItem_UEBACodec.BaboonDomainVersion
    }
    public static readonly BaboonDomainIdentifier = 'mcp.mux.stub'
    public baboonDomainIdentifier() {
        return OrderItem_UEBACodec.BaboonDomainIdentifier
    }
    public static readonly BaboonTypeIdentifier = 'mcp.mux.stub/:#OrderItem'
    public baboonTypeIdentifier() {
        return OrderItem_UEBACodec.BaboonTypeIdentifier
    }

    public static lazyInstance = new Lazy(() => new OrderItem_UEBACodec())
    public static get instance(): OrderItem_UEBACodec {
        return OrderItem_UEBACodec.lazyInstance.value
    }
}