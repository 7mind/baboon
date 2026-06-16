import {type BaboonMethodId, type BaboonWiringError, type IBaboonJsonService, type IBaboonUebaService} from '../../../../BaboonSharedRuntime'
import {BaboonBinReader, BaboonBinWriter, BaboonCodecContext, BaboonEither} from '../../../../BaboonSharedRuntime'
import {IBaboonServiceRt} from '../baboon-service-rt'
import {In as createuser_In, In_JsonCodec as createuser_In_JsonCodec, In_UEBACodec as createuser_In_UEBACodec} from './createuser/in'
import {Out as createuser_Out, Out_JsonCodec as createuser_Out_JsonCodec, Out_UEBACodec as createuser_Out_UEBACodec} from './createuser/out'
import {In as getuser_In, In_JsonCodec as getuser_In_JsonCodec, In_UEBACodec as getuser_In_UEBACodec} from './getuser/in'
import {Out as getuser_Out, Out_JsonCodec as getuser_Out_JsonCodec, Out_UEBACodec as getuser_Out_UEBACodec} from './getuser/out'
import {type UserService} from './service'

type _ContainerImport<L, R> = BaboonEither<L, R>;

export function invokeJson_UserService(
    method: BaboonMethodId,
    data: string,
    impl: UserService,
    rt: IBaboonServiceRt,
    ctx: BaboonCodecContext
): BaboonEither<BaboonWiringError, string> {
    switch (method.methodName) {
        case "createUser": {
            let input: BaboonEither<BaboonWiringError, In>;
            try {
                input = rt.pure<BaboonWiringError, createuser_In>(createuser_In_JsonCodec.instance.decode(BaboonCodecContext.Default, JSON.parse(data)));
            } catch (ex: unknown) {
                input = rt.fail<BaboonWiringError, createuser_In>({ tag: 'DecoderFailed', method, error: ex });
            }
            const output = rt.flatMap(input, (v: createuser_In) => {
                try {
                    return rt.pure<BaboonWiringError, createuser_Out>(impl.createUser(v));
                } catch (ex: unknown) {
                    return rt.fail<BaboonWiringError, createuser_Out>({ tag: 'CallFailed', method, domainError: ex });
                }
            });
            return rt.flatMap(output, (v: createuser_Out) => {
                try {
                    return rt.pure<BaboonWiringError, string>(JSON.stringify(createuser_Out_JsonCodec.instance.encode(BaboonCodecContext.Default, v)));
                } catch (ex: unknown) {
                    return rt.fail<BaboonWiringError, string>({ tag: 'EncoderFailed', method, error: ex });
                }
            });
        }
        case "getUser": {
            let input: BaboonEither<BaboonWiringError, In>;
            try {
                input = rt.pure<BaboonWiringError, getuser_In>(getuser_In_JsonCodec.instance.decode(BaboonCodecContext.Default, JSON.parse(data)));
            } catch (ex: unknown) {
                input = rt.fail<BaboonWiringError, getuser_In>({ tag: 'DecoderFailed', method, error: ex });
            }
            const output = rt.flatMap(input, (v: getuser_In) => {
                try {
                    return rt.pure<BaboonWiringError, getuser_Out>(impl.getUser(v));
                } catch (ex: unknown) {
                    return rt.fail<BaboonWiringError, getuser_Out>({ tag: 'CallFailed', method, domainError: ex });
                }
            });
            return rt.flatMap(output, (v: getuser_Out) => {
                try {
                    return rt.pure<BaboonWiringError, string>(JSON.stringify(getuser_Out_JsonCodec.instance.encode(BaboonCodecContext.Default, v)));
                } catch (ex: unknown) {
                    return rt.fail<BaboonWiringError, string>({ tag: 'EncoderFailed', method, error: ex });
                }
            });
        }
        default:
            return rt.fail<BaboonWiringError, string>({ tag: 'NoMatchingMethod', method });
    }
}

export function invokeUeba_UserService(
    method: BaboonMethodId,
    data: Uint8Array,
    impl: UserService,
    rt: IBaboonServiceRt,
    ctx: BaboonCodecContext
): BaboonEither<BaboonWiringError, Uint8Array> {
    switch (method.methodName) {
        case "createUser": {
            let input: BaboonEither<BaboonWiringError, In>;
            try {
                input = rt.pure<BaboonWiringError, createuser_In>((() => { const reader = new BaboonBinReader(data); return createuser_In_UEBACodec.instance.decode(ctx, reader); })());
            } catch (ex: unknown) {
                input = rt.fail<BaboonWiringError, createuser_In>({ tag: 'DecoderFailed', method, error: ex });
            }
            const output = rt.flatMap(input, (v: createuser_In) => {
                try {
                    return rt.pure<BaboonWiringError, createuser_Out>(impl.createUser(v));
                } catch (ex: unknown) {
                    return rt.fail<BaboonWiringError, createuser_Out>({ tag: 'CallFailed', method, domainError: ex });
                }
            });
            return rt.flatMap(output, (v: createuser_Out) => {
                try {
                    return rt.pure<BaboonWiringError, Uint8Array>((() => { const writer = new BaboonBinWriter(); createuser_Out_UEBACodec.instance.encode(ctx, v, writer); return writer.toBytes(); })());
                } catch (ex: unknown) {
                    return rt.fail<BaboonWiringError, Uint8Array>({ tag: 'EncoderFailed', method, error: ex });
                }
            });
        }
        case "getUser": {
            let input: BaboonEither<BaboonWiringError, In>;
            try {
                input = rt.pure<BaboonWiringError, getuser_In>((() => { const reader = new BaboonBinReader(data); return getuser_In_UEBACodec.instance.decode(ctx, reader); })());
            } catch (ex: unknown) {
                input = rt.fail<BaboonWiringError, getuser_In>({ tag: 'DecoderFailed', method, error: ex });
            }
            const output = rt.flatMap(input, (v: getuser_In) => {
                try {
                    return rt.pure<BaboonWiringError, getuser_Out>(impl.getUser(v));
                } catch (ex: unknown) {
                    return rt.fail<BaboonWiringError, getuser_Out>({ tag: 'CallFailed', method, domainError: ex });
                }
            });
            return rt.flatMap(output, (v: getuser_Out) => {
                try {
                    return rt.pure<BaboonWiringError, Uint8Array>((() => { const writer = new BaboonBinWriter(); getuser_Out_UEBACodec.instance.encode(ctx, v, writer); return writer.toBytes(); })());
                } catch (ex: unknown) {
                    return rt.fail<BaboonWiringError, Uint8Array>({ tag: 'EncoderFailed', method, error: ex });
                }
            });
        }
        default:
            return rt.fail<BaboonWiringError, Uint8Array>({ tag: 'NoMatchingMethod', method });
    }
}

export class UserServiceJsonService implements IBaboonJsonService<BaboonEither<BaboonWiringError, string>> {
    readonly serviceName = "UserService";
    private readonly impl: UserService;
    private readonly rt: IBaboonServiceRt;

    constructor(impl: UserService, rt: IBaboonServiceRt) {
        this.impl = impl;
        this.rt = rt;
    }

    invoke(method: BaboonMethodId, data: string, ctx: BaboonCodecContext): BaboonEither<BaboonWiringError, string> {
        return invokeJson_UserService(method, data, this.impl, this.rt, ctx);
    }
}

export class UserServiceUebaService implements IBaboonUebaService<BaboonEither<BaboonWiringError, Uint8Array>> {
    readonly serviceName = "UserService";
    private readonly impl: UserService;
    private readonly rt: IBaboonServiceRt;

    constructor(impl: UserService, rt: IBaboonServiceRt) {
        this.impl = impl;
        this.rt = rt;
    }

    invoke(method: BaboonMethodId, data: Uint8Array, ctx: BaboonCodecContext): BaboonEither<BaboonWiringError, Uint8Array> {
        return invokeUeba_UserService(method, data, this.impl, this.rt, ctx);
    }
}