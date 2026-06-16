import {type BaboonMethodId, type BaboonWiringError, type IBaboonJsonService, type IBaboonUebaService} from '../../../../BaboonSharedRuntime'
import {BaboonBinReader, BaboonBinWriter, BaboonCodecContext, BaboonEither} from '../../../../BaboonSharedRuntime'
import {IBaboonServiceRt} from '../baboon-service-rt'
import {In as createuser_In, In_JsonCodec as createuser_In_JsonCodec, In_UEBACodec as createuser_In_UEBACodec} from './createuser/in'
import {Out as createuser_Out, Out_JsonCodec as createuser_Out_JsonCodec, Out_UEBACodec as createuser_Out_UEBACodec} from './createuser/out'
import {In as getuser_In, In_JsonCodec as getuser_In_JsonCodec, In_UEBACodec as getuser_In_UEBACodec} from './getuser/in'
import {Out as getuser_Out, Out_JsonCodec as getuser_Out_JsonCodec, Out_UEBACodec as getuser_Out_UEBACodec} from './getuser/out'
import {type UserService} from './service'

type _ContainerImport<L, R> = BaboonEither<L, R>;

export async function invokeJson_UserService(
    method: BaboonMethodId,
    data: string,
    impl: UserService,
    rt: IBaboonServiceRt,
    ctx: BaboonCodecContext
): Promise<BaboonEither<BaboonWiringError, string>> {
    switch (method.methodName) {
        case "createUser": {
            let decoded: createuser_In;
            try {
                decoded = createuser_In_JsonCodec.instance.decode(BaboonCodecContext.Default, JSON.parse(data));
            } catch (ex: unknown) {
                return rt.fail<BaboonWiringError, string>({ tag: 'DecoderFailed', method, error: ex });
            }
            let output: createuser_Out;
            try {
                output = await impl.createUser(decoded);
            } catch (ex: unknown) {
                return rt.fail<BaboonWiringError, string>({ tag: 'CallFailed', method, domainError: ex });
            }
            try {
                return rt.pure<BaboonWiringError, string>(JSON.stringify(createuser_Out_JsonCodec.instance.encode(BaboonCodecContext.Default, output)));
            } catch (ex: unknown) {
                return rt.fail<BaboonWiringError, string>({ tag: 'EncoderFailed', method, error: ex });
            }
        }
        case "getUser": {
            let decoded: getuser_In;
            try {
                decoded = getuser_In_JsonCodec.instance.decode(BaboonCodecContext.Default, JSON.parse(data));
            } catch (ex: unknown) {
                return rt.fail<BaboonWiringError, string>({ tag: 'DecoderFailed', method, error: ex });
            }
            let output: getuser_Out;
            try {
                output = await impl.getUser(decoded);
            } catch (ex: unknown) {
                return rt.fail<BaboonWiringError, string>({ tag: 'CallFailed', method, domainError: ex });
            }
            try {
                return rt.pure<BaboonWiringError, string>(JSON.stringify(getuser_Out_JsonCodec.instance.encode(BaboonCodecContext.Default, output)));
            } catch (ex: unknown) {
                return rt.fail<BaboonWiringError, string>({ tag: 'EncoderFailed', method, error: ex });
            }
        }
        default:
            return rt.fail<BaboonWiringError, string>({ tag: 'NoMatchingMethod', method });
    }
}

export async function invokeUeba_UserService(
    method: BaboonMethodId,
    data: Uint8Array,
    impl: UserService,
    rt: IBaboonServiceRt,
    ctx: BaboonCodecContext
): Promise<BaboonEither<BaboonWiringError, Uint8Array>> {
    switch (method.methodName) {
        case "createUser": {
            let decoded: createuser_In;
            try {
                decoded = (() => { const reader = new BaboonBinReader(data); return createuser_In_UEBACodec.instance.decode(ctx, reader); })();
            } catch (ex: unknown) {
                return rt.fail<BaboonWiringError, Uint8Array>({ tag: 'DecoderFailed', method, error: ex });
            }
            let output: createuser_Out;
            try {
                output = await impl.createUser(decoded);
            } catch (ex: unknown) {
                return rt.fail<BaboonWiringError, Uint8Array>({ tag: 'CallFailed', method, domainError: ex });
            }
            try {
                return rt.pure<BaboonWiringError, Uint8Array>((() => { const writer = new BaboonBinWriter(); createuser_Out_UEBACodec.instance.encode(ctx, output, writer); return writer.toBytes(); })());
            } catch (ex: unknown) {
                return rt.fail<BaboonWiringError, Uint8Array>({ tag: 'EncoderFailed', method, error: ex });
            }
        }
        case "getUser": {
            let decoded: getuser_In;
            try {
                decoded = (() => { const reader = new BaboonBinReader(data); return getuser_In_UEBACodec.instance.decode(ctx, reader); })();
            } catch (ex: unknown) {
                return rt.fail<BaboonWiringError, Uint8Array>({ tag: 'DecoderFailed', method, error: ex });
            }
            let output: getuser_Out;
            try {
                output = await impl.getUser(decoded);
            } catch (ex: unknown) {
                return rt.fail<BaboonWiringError, Uint8Array>({ tag: 'CallFailed', method, domainError: ex });
            }
            try {
                return rt.pure<BaboonWiringError, Uint8Array>((() => { const writer = new BaboonBinWriter(); getuser_Out_UEBACodec.instance.encode(ctx, output, writer); return writer.toBytes(); })());
            } catch (ex: unknown) {
                return rt.fail<BaboonWiringError, Uint8Array>({ tag: 'EncoderFailed', method, error: ex });
            }
        }
        default:
            return rt.fail<BaboonWiringError, Uint8Array>({ tag: 'NoMatchingMethod', method });
    }
}

export class UserServiceJsonService implements IBaboonJsonService<Promise<BaboonEither<BaboonWiringError, string>>> {
    readonly serviceName = "UserService";
    private readonly impl: UserService;
    private readonly rt: IBaboonServiceRt;

    constructor(impl: UserService, rt: IBaboonServiceRt) {
        this.impl = impl;
        this.rt = rt;
    }

    invoke(method: BaboonMethodId, data: string, ctx: BaboonCodecContext): Promise<BaboonEither<BaboonWiringError, string>> {
        return invokeJson_UserService(method, data, this.impl, this.rt, ctx);
    }
}

export class UserServiceUebaService implements IBaboonUebaService<Promise<BaboonEither<BaboonWiringError, Uint8Array>>> {
    readonly serviceName = "UserService";
    private readonly impl: UserService;
    private readonly rt: IBaboonServiceRt;

    constructor(impl: UserService, rt: IBaboonServiceRt) {
        this.impl = impl;
        this.rt = rt;
    }

    invoke(method: BaboonMethodId, data: Uint8Array, ctx: BaboonCodecContext): Promise<BaboonEither<BaboonWiringError, Uint8Array>> {
        return invokeUeba_UserService(method, data, this.impl, this.rt, ctx);
    }
}