import {type BaboonGeneratedLatest} from '../../../../../BaboonSharedRuntime'
import {BaboonBinReader, BaboonBinWriter, BaboonCodecContext, BinTools, Lazy} from '../../../../../BaboonSharedRuntime'

export class In implements BaboonGeneratedLatest {
    private readonly _email: string;
    private readonly _name: string;

    constructor(email: string, name: string) {
        this._email = email
        this._name = name
    }

    public get email(): string {
        return this._email;
    }
    public get name(): string {
        return this._name;
    }

    public toJSON(): Record<string, unknown> {
        return {
            email: this._email,
            name: this._name
        };
    }

    public with(overrides: {email?: string; name?: string}): In {
        return new In(
            'email' in overrides ? overrides.email! : this._email,
            'name' in overrides ? overrides.name! : this._name
        );
    }

    public static fromPlain(obj: {email: string; name: string}): In {
        return new In(
            obj.email,
            obj.name
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
    public static readonly BaboonTypeIdentifier = 'mcp.mux.stub/UserService/createUser#in'
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
            "email": value.email,
            "name": value.name,
        }
    }
    public decode(ctx: BaboonCodecContext, json: unknown): In {
        if (this !== In_JsonCodec .lazyInstance.value) {
            return In_JsonCodec.lazyInstance.value.decode(ctx, json)
        }
    
        const obj = json as Record<string, unknown>;
        return new In (
            obj["email"] as string,
            obj["name"] as string,
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
    public static readonly BaboonTypeIdentifier = 'mcp.mux.stub/UserService/createUser#in'
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
                BinTools.writeString(buffer, value.email);
                const after = buffer.position();
                BinTools.writeI32(writer, after - before);
            }
            {
                const before = buffer.position();
                BinTools.writeI32(writer, before);
                BinTools.writeString(buffer, value.name);
                const after = buffer.position();
                BinTools.writeI32(writer, after - before);
            }
            writer.writeAll(buffer.toBytes());
        } else {
            BinTools.writeByte(writer, 0x00)
            BinTools.writeString(writer, value.email);
            BinTools.writeString(writer, value.name);
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
        const email = BinTools.readString(reader);
        const name = BinTools.readString(reader);
        return new In(
            email,
            name,
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
    public static readonly BaboonTypeIdentifier = 'mcp.mux.stub/UserService/createUser#in'
    public baboonTypeIdentifier() {
        return In_UEBACodec.BaboonTypeIdentifier
    }

    public static lazyInstance = new Lazy(() => new In_UEBACodec())
    public static get instance(): In_UEBACodec {
        return In_UEBACodec.lazyInstance.value
    }
}