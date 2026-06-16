import {type BaboonMethodId, type BaboonWiringError, type IBaboonJsonService, type IBaboonUebaService} from '../../../../BaboonSharedRuntime'
import {BaboonBinReader, BaboonBinWriter, BaboonCodecContext, BaboonEither} from '../../../../BaboonSharedRuntime'
import {IBaboonServiceRt} from '../baboon-service-rt'
import {In as cancelorder_In, In_JsonCodec as cancelorder_In_JsonCodec, In_UEBACodec as cancelorder_In_UEBACodec} from './cancelorder/in'
import {Out as cancelorder_Out, Out_JsonCodec as cancelorder_Out_JsonCodec, Out_UEBACodec as cancelorder_Out_UEBACodec} from './cancelorder/out'
import {In as placeorder_In, In_JsonCodec as placeorder_In_JsonCodec, In_UEBACodec as placeorder_In_UEBACodec} from './placeorder/in'
import {Out as placeorder_Out, Out_JsonCodec as placeorder_Out_JsonCodec, Out_UEBACodec as placeorder_Out_UEBACodec} from './placeorder/out'
import {type OrderService} from './service'

type _ContainerImport<L, R> = BaboonEither<L, R>;

export function invokeJson_OrderService(
    method: BaboonMethodId,
    data: string,
    impl: OrderService,
    rt: IBaboonServiceRt,
    ctx: BaboonCodecContext
): BaboonEither<BaboonWiringError, string> {
    switch (method.methodName) {
        case "placeOrder": {
            let input: BaboonEither<BaboonWiringError, In>;
            try {
                input = rt.pure<BaboonWiringError, placeorder_In>(placeorder_In_JsonCodec.instance.decode(BaboonCodecContext.Default, JSON.parse(data)));
            } catch (ex: unknown) {
                input = rt.fail<BaboonWiringError, placeorder_In>({ tag: 'DecoderFailed', method, error: ex });
            }
            const output = rt.flatMap(input, (v: placeorder_In) => {
                try {
                    return rt.pure<BaboonWiringError, placeorder_Out>(impl.placeOrder(v));
                } catch (ex: unknown) {
                    return rt.fail<BaboonWiringError, placeorder_Out>({ tag: 'CallFailed', method, domainError: ex });
                }
            });
            return rt.flatMap(output, (v: placeorder_Out) => {
                try {
                    return rt.pure<BaboonWiringError, string>(JSON.stringify(placeorder_Out_JsonCodec.instance.encode(BaboonCodecContext.Default, v)));
                } catch (ex: unknown) {
                    return rt.fail<BaboonWiringError, string>({ tag: 'EncoderFailed', method, error: ex });
                }
            });
        }
        case "cancelOrder": {
            let input: BaboonEither<BaboonWiringError, In>;
            try {
                input = rt.pure<BaboonWiringError, cancelorder_In>(cancelorder_In_JsonCodec.instance.decode(BaboonCodecContext.Default, JSON.parse(data)));
            } catch (ex: unknown) {
                input = rt.fail<BaboonWiringError, cancelorder_In>({ tag: 'DecoderFailed', method, error: ex });
            }
            const output = rt.flatMap(input, (v: cancelorder_In) => {
                try {
                    return rt.pure<BaboonWiringError, cancelorder_Out>(impl.cancelOrder(v));
                } catch (ex: unknown) {
                    return rt.fail<BaboonWiringError, cancelorder_Out>({ tag: 'CallFailed', method, domainError: ex });
                }
            });
            return rt.flatMap(output, (v: cancelorder_Out) => {
                try {
                    return rt.pure<BaboonWiringError, string>(JSON.stringify(cancelorder_Out_JsonCodec.instance.encode(BaboonCodecContext.Default, v)));
                } catch (ex: unknown) {
                    return rt.fail<BaboonWiringError, string>({ tag: 'EncoderFailed', method, error: ex });
                }
            });
        }
        default:
            return rt.fail<BaboonWiringError, string>({ tag: 'NoMatchingMethod', method });
    }
}

export function invokeUeba_OrderService(
    method: BaboonMethodId,
    data: Uint8Array,
    impl: OrderService,
    rt: IBaboonServiceRt,
    ctx: BaboonCodecContext
): BaboonEither<BaboonWiringError, Uint8Array> {
    switch (method.methodName) {
        case "placeOrder": {
            let input: BaboonEither<BaboonWiringError, In>;
            try {
                input = rt.pure<BaboonWiringError, placeorder_In>((() => { const reader = new BaboonBinReader(data); return placeorder_In_UEBACodec.instance.decode(ctx, reader); })());
            } catch (ex: unknown) {
                input = rt.fail<BaboonWiringError, placeorder_In>({ tag: 'DecoderFailed', method, error: ex });
            }
            const output = rt.flatMap(input, (v: placeorder_In) => {
                try {
                    return rt.pure<BaboonWiringError, placeorder_Out>(impl.placeOrder(v));
                } catch (ex: unknown) {
                    return rt.fail<BaboonWiringError, placeorder_Out>({ tag: 'CallFailed', method, domainError: ex });
                }
            });
            return rt.flatMap(output, (v: placeorder_Out) => {
                try {
                    return rt.pure<BaboonWiringError, Uint8Array>((() => { const writer = new BaboonBinWriter(); placeorder_Out_UEBACodec.instance.encode(ctx, v, writer); return writer.toBytes(); })());
                } catch (ex: unknown) {
                    return rt.fail<BaboonWiringError, Uint8Array>({ tag: 'EncoderFailed', method, error: ex });
                }
            });
        }
        case "cancelOrder": {
            let input: BaboonEither<BaboonWiringError, In>;
            try {
                input = rt.pure<BaboonWiringError, cancelorder_In>((() => { const reader = new BaboonBinReader(data); return cancelorder_In_UEBACodec.instance.decode(ctx, reader); })());
            } catch (ex: unknown) {
                input = rt.fail<BaboonWiringError, cancelorder_In>({ tag: 'DecoderFailed', method, error: ex });
            }
            const output = rt.flatMap(input, (v: cancelorder_In) => {
                try {
                    return rt.pure<BaboonWiringError, cancelorder_Out>(impl.cancelOrder(v));
                } catch (ex: unknown) {
                    return rt.fail<BaboonWiringError, cancelorder_Out>({ tag: 'CallFailed', method, domainError: ex });
                }
            });
            return rt.flatMap(output, (v: cancelorder_Out) => {
                try {
                    return rt.pure<BaboonWiringError, Uint8Array>((() => { const writer = new BaboonBinWriter(); cancelorder_Out_UEBACodec.instance.encode(ctx, v, writer); return writer.toBytes(); })());
                } catch (ex: unknown) {
                    return rt.fail<BaboonWiringError, Uint8Array>({ tag: 'EncoderFailed', method, error: ex });
                }
            });
        }
        default:
            return rt.fail<BaboonWiringError, Uint8Array>({ tag: 'NoMatchingMethod', method });
    }
}

export class OrderServiceJsonService implements IBaboonJsonService<BaboonEither<BaboonWiringError, string>> {
    readonly serviceName = "OrderService";
    private readonly impl: OrderService;
    private readonly rt: IBaboonServiceRt;

    constructor(impl: OrderService, rt: IBaboonServiceRt) {
        this.impl = impl;
        this.rt = rt;
    }

    invoke(method: BaboonMethodId, data: string, ctx: BaboonCodecContext): BaboonEither<BaboonWiringError, string> {
        return invokeJson_OrderService(method, data, this.impl, this.rt, ctx);
    }
}

export class OrderServiceUebaService implements IBaboonUebaService<BaboonEither<BaboonWiringError, Uint8Array>> {
    readonly serviceName = "OrderService";
    private readonly impl: OrderService;
    private readonly rt: IBaboonServiceRt;

    constructor(impl: OrderService, rt: IBaboonServiceRt) {
        this.impl = impl;
        this.rt = rt;
    }

    invoke(method: BaboonMethodId, data: Uint8Array, ctx: BaboonCodecContext): BaboonEither<BaboonWiringError, Uint8Array> {
        return invokeUeba_OrderService(method, data, this.impl, this.rt, ctx);
    }
}