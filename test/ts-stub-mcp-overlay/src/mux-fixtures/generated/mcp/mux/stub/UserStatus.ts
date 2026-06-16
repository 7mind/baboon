import {BaboonBinReader, BaboonBinWriter, BaboonCodecContext, BaboonDecoderFailure, BinTools, Lazy} from '../../../BaboonSharedRuntime'

export enum UserStatus {
    Active = "Active",
    Suspended = "Suspended",
    Deleted = "Deleted"
}

export const UserStatus_values: ReadonlyArray<UserStatus> = [
    UserStatus.Active,
    UserStatus.Suspended,
    UserStatus.Deleted
] as const;

export function UserStatus_parse(s: string): UserStatus {
    const found = UserStatus_values.find(v => v === s);
    if (found === undefined) {
        throw new BaboonDecoderFailure("Unknown UserStatus variant: " + s);
    }
    return found;
}

export class UserStatus_JsonCodec {
    public encode(ctx: BaboonCodecContext, value: UserStatus): unknown {
        if (this !== UserStatus_JsonCodec.lazyInstance.value) {
          return UserStatus_JsonCodec.lazyInstance.value.encode(ctx, value)
        }
    
        return value
    }
    public decode(ctx: BaboonCodecContext, json: unknown): UserStatus {
        if (this !== UserStatus_JsonCodec .lazyInstance.value) {
            return UserStatus_JsonCodec.lazyInstance.value.decode(ctx, json)
        }
    
        return UserStatus_parse(json as string)
    }

    public static readonly BaboonDomainVersion = '1.0.0'
    public baboonDomainVersion() {
        return UserStatus_JsonCodec.BaboonDomainVersion
    }
    public static readonly BaboonDomainIdentifier = 'mcp.mux.stub'
    public baboonDomainIdentifier() {
        return UserStatus_JsonCodec.BaboonDomainIdentifier
    }
    public static readonly BaboonTypeIdentifier = 'mcp.mux.stub/:#UserStatus'
    public baboonTypeIdentifier() {
        return UserStatus_JsonCodec.BaboonTypeIdentifier
    }

    public static lazyInstance = new Lazy(() => new UserStatus_JsonCodec())
    public static get instance(): UserStatus_JsonCodec {
        return UserStatus_JsonCodec.lazyInstance.value
    }
}

export class UserStatus_UEBACodec {
    public encode(ctx: BaboonCodecContext, value: UserStatus, writer: BaboonBinWriter): void {
        if (this !== UserStatus_UEBACodec.lazyInstance.value) {
            UserStatus_UEBACodec.lazyInstance.value.encode(ctx, value, writer); return;
        }
    
        switch (value) {
            case "Active": BinTools.writeByte(writer, 0); break;
                case "Suspended": BinTools.writeByte(writer, 1); break;
                case "Deleted": BinTools.writeByte(writer, 2); break;
            default: throw new Error("Unknown enum variant: " + value);
        }
    }
    
    public decode(ctx: BaboonCodecContext, reader: BaboonBinReader): UserStatus {
        if (this !== UserStatus_UEBACodec .lazyInstance.value) {
            return UserStatus_UEBACodec.lazyInstance.value.decode(ctx, reader)
        }
    
        const tag = BinTools.readByte(reader);
        switch (tag) {
            case 0: return "Active" as UserStatus;
                case 1: return "Suspended" as UserStatus;
                case 2: return "Deleted" as UserStatus;
            default: throw new Error("Unknown enum variant tag: " + tag);
        }
    }

    public static readonly BaboonDomainVersion = '1.0.0'
    public baboonDomainVersion() {
        return UserStatus_UEBACodec.BaboonDomainVersion
    }
    public static readonly BaboonDomainIdentifier = 'mcp.mux.stub'
    public baboonDomainIdentifier() {
        return UserStatus_UEBACodec.BaboonDomainIdentifier
    }
    public static readonly BaboonTypeIdentifier = 'mcp.mux.stub/:#UserStatus'
    public baboonTypeIdentifier() {
        return UserStatus_UEBACodec.BaboonTypeIdentifier
    }

    public static lazyInstance = new Lazy(() => new UserStatus_UEBACodec())
    public static get instance(): UserStatus_UEBACodec {
        return UserStatus_UEBACodec.lazyInstance.value
    }
}