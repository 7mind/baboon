import {type BaboonGeneratedLatest} from '../../../BaboonSharedRuntime'
import {BaboonBinReader, BaboonBinWriter, BaboonCodecContext, BinTools, Lazy} from '../../../BaboonSharedRuntime'
import {UserStatus, UserStatus_JsonCodec, UserStatus_UEBACodec} from './UserStatus'

export class UserProfile implements BaboonGeneratedLatest {
    private readonly _userId: string;
    private readonly _email: string;
    private readonly _status: UserStatus;

    constructor(userId: string, email: string, status: UserStatus) {
        this._userId = userId
        this._email = email
        this._status = status
    }

    public get userId(): string {
        return this._userId;
    }
    public get email(): string {
        return this._email;
    }
    public get status(): UserStatus {
        return this._status;
    }

    public toJSON(): Record<string, unknown> {
        return {
            userId: this._userId,
            email: this._email,
            status: this._status
        };
    }

    public with(overrides: {userId?: string; email?: string; status?: UserStatus}): UserProfile {
        return new UserProfile(
            'userId' in overrides ? overrides.userId! : this._userId,
            'email' in overrides ? overrides.email! : this._email,
            'status' in overrides ? overrides.status! : this._status
        );
    }

    public static fromPlain(obj: {userId: string; email: string; status: UserStatus}): UserProfile {
        return new UserProfile(
            obj.userId,
            obj.email,
            obj.status
        );
    }

    public static readonly BaboonDomainVersion = '1.0.0'
    public baboonDomainVersion() {
        return UserProfile.BaboonDomainVersion
    }
    public static readonly BaboonDomainIdentifier = 'mcp.mux.stub'
    public baboonDomainIdentifier() {
        return UserProfile.BaboonDomainIdentifier
    }
    public static readonly BaboonTypeIdentifier = 'mcp.mux.stub/:#UserProfile'
    public baboonTypeIdentifier() {
        return UserProfile.BaboonTypeIdentifier
    }
    public static readonly BaboonSameInVersions = ["1.0.0"]
    public baboonSameInVersions() {
        return UserProfile.BaboonSameInVersions
    }
    public static jsonCodec(): UserProfile_JsonCodec {
        return UserProfile_JsonCodec.instance
    }
    public static binCodec(): UserProfile_UEBACodec {
        return UserProfile_UEBACodec.instance
    }
}

export class UserProfile_JsonCodec {
    public encode(ctx: BaboonCodecContext, value: UserProfile): unknown {
        if (this !== UserProfile_JsonCodec.lazyInstance.value) {
          return UserProfile_JsonCodec.lazyInstance.value.encode(ctx, value)
        }
    
        return {
            "userId": value.userId,
            "email": value.email,
            "status": UserStatus_JsonCodec.instance.encode(BaboonCodecContext.Default, value.status),
        }
    }
    public decode(ctx: BaboonCodecContext, json: unknown): UserProfile {
        if (this !== UserProfile_JsonCodec .lazyInstance.value) {
            return UserProfile_JsonCodec.lazyInstance.value.decode(ctx, json)
        }
    
        const obj = json as Record<string, unknown>;
        return new UserProfile (
            obj["userId"] as string,
            obj["email"] as string,
            UserStatus_JsonCodec.instance.decode(BaboonCodecContext.Default, obj["status"]),
        )
    }

    public static readonly BaboonDomainVersion = '1.0.0'
    public baboonDomainVersion() {
        return UserProfile_JsonCodec.BaboonDomainVersion
    }
    public static readonly BaboonDomainIdentifier = 'mcp.mux.stub'
    public baboonDomainIdentifier() {
        return UserProfile_JsonCodec.BaboonDomainIdentifier
    }
    public static readonly BaboonTypeIdentifier = 'mcp.mux.stub/:#UserProfile'
    public baboonTypeIdentifier() {
        return UserProfile_JsonCodec.BaboonTypeIdentifier
    }

    public static lazyInstance = new Lazy(() => new UserProfile_JsonCodec())
    public static get instance(): UserProfile_JsonCodec {
        return UserProfile_JsonCodec.lazyInstance.value
    }
}

export class UserProfile_UEBACodec {
    public encode(ctx: BaboonCodecContext, value: UserProfile, writer: BaboonBinWriter): void {
        if (this !== UserProfile_UEBACodec.lazyInstance.value) {
            UserProfile_UEBACodec.lazyInstance.value.encode(ctx, value, writer); return;
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
                BinTools.writeString(buffer, value.email);
                const after = buffer.position();
                BinTools.writeI32(writer, after - before);
            }
            UserStatus_UEBACodec.instance.encode(ctx, value.status, buffer);
            writer.writeAll(buffer.toBytes());
        } else {
            BinTools.writeByte(writer, 0x00)
            BinTools.writeString(writer, value.userId);
            BinTools.writeString(writer, value.email);
            UserStatus_UEBACodec.instance.encode(ctx, value.status, writer);
        }
    }
    
    public decode(ctx: BaboonCodecContext, reader: BaboonBinReader): UserProfile {
        if (this !== UserProfile_UEBACodec .lazyInstance.value) {
            return UserProfile_UEBACodec.lazyInstance.value.decode(ctx, reader)
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
        const email = BinTools.readString(reader);
        const status = UserStatus_UEBACodec.instance.decode(ctx, reader);
        return new UserProfile(
            userId,
            email,
            status,
        );
    }

    public static readonly BaboonDomainVersion = '1.0.0'
    public baboonDomainVersion() {
        return UserProfile_UEBACodec.BaboonDomainVersion
    }
    public static readonly BaboonDomainIdentifier = 'mcp.mux.stub'
    public baboonDomainIdentifier() {
        return UserProfile_UEBACodec.BaboonDomainIdentifier
    }
    public static readonly BaboonTypeIdentifier = 'mcp.mux.stub/:#UserProfile'
    public baboonTypeIdentifier() {
        return UserProfile_UEBACodec.BaboonTypeIdentifier
    }

    public static lazyInstance = new Lazy(() => new UserProfile_UEBACodec())
    public static get instance(): UserProfile_UEBACodec {
        return UserProfile_UEBACodec.lazyInstance.value
    }
}