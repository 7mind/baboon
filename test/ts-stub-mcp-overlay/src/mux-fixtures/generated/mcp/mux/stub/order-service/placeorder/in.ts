import {type BaboonGeneratedLatest} from '../../../../../BaboonSharedRuntime'
import {BaboonBinReader, BaboonBinWriter, BaboonCodecContext, BinTools, Lazy} from '../../../../../BaboonSharedRuntime'
import {OrderItem, OrderItem_JsonCodec, OrderItem_UEBACodec} from '../../OrderItem'

export class In implements BaboonGeneratedLatest {
    private readonly _userId: string;
    private readonly _items: Array<OrderItem>;

    constructor(userId: string, items: Array<OrderItem>) {
        this._userId = userId
        this._items = items
    }

    public get userId(): string {
        return this._userId;
    }
    public get items(): Array<OrderItem> {
        return this._items;
    }

    public toJSON(): Record<string, unknown> {
        return {
            userId: this._userId,
            items: this._items
        };
    }

    public with(overrides: {userId?: string; items?: Array<OrderItem>}): In {
        return new In(
            'userId' in overrides ? overrides.userId! : this._userId,
            'items' in overrides ? overrides.items! : this._items
        );
    }

    public static fromPlain(obj: {userId: string; items: Array<OrderItem>}): In {
        return new In(
            obj.userId,
            obj.items
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
    public static readonly BaboonTypeIdentifier = 'mcp.mux.stub/OrderService/placeOrder#in'
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
            "userId": value.userId,
            "items": Array.from(value.items).map(item => OrderItem_JsonCodec.instance.encode(BaboonCodecContext.Default, item)),
        }
    }
    public decode(ctx: BaboonCodecContext, json: unknown): In {
        if (this !== In_JsonCodec .lazyInstance.value) {
            return In_JsonCodec.lazyInstance.value.decode(ctx, json)
        }
    
        const obj = json as Record<string, unknown>;
        return new In (
            obj["userId"] as string,
            (obj["items"] as unknown[]).map(item => OrderItem_JsonCodec.instance.decode(BaboonCodecContext.Default, item)),
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
    public static readonly BaboonTypeIdentifier = 'mcp.mux.stub/OrderService/placeOrder#in'
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
                BinTools.writeString(buffer, value.userId);
                const after = buffer.position();
                BinTools.writeI32(writer, after - before);
            }
            {
                const before = buffer.position();
                BinTools.writeI32(writer, before);
                BinTools.writeI32(buffer, Array.from(value.items).length);
            for (const item of value.items) {
                OrderItem_UEBACodec.instance.encode(ctx, item, buffer);
            }
                const after = buffer.position();
                BinTools.writeI32(writer, after - before);
            }
            writer.writeAll(buffer.toBytes());
        } else {
            BinTools.writeByte(writer, 0x00)
            BinTools.writeString(writer, value.userId);
            BinTools.writeI32(writer, Array.from(value.items).length);
            for (const item of value.items) {
                OrderItem_UEBACodec.instance.encode(ctx, item, writer);
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
        const userId = BinTools.readString(reader);
        const items = Array.from({ length: BinTools.readI32(reader) }, () => OrderItem_UEBACodec.instance.decode(ctx, reader));
        return new In(
            userId,
            items,
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
    public static readonly BaboonTypeIdentifier = 'mcp.mux.stub/OrderService/placeOrder#in'
    public baboonTypeIdentifier() {
        return In_UEBACodec.BaboonTypeIdentifier
    }

    public static lazyInstance = new Lazy(() => new In_UEBACodec())
    public static get instance(): In_UEBACodec {
        return In_UEBACodec.lazyInstance.value
    }
}