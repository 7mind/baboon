import {type BaboonWiringError} from '../../../BaboonSharedRuntime'
import {BaboonCodecContext, BaboonEither, BaboonWiringException} from '../../../BaboonSharedRuntime'
import {IBaboonServiceRt} from './baboon-service-rt'
import {type OrderService} from './order-service/service'
import {invokeJson_OrderService, invokeUeba_OrderService} from './order-service/wiring'
import {type UserService} from './user-service/service'
import {invokeJson_UserService, invokeUeba_UserService} from './user-service/wiring'

type _DispatcherContainerImport<L, R> = BaboonEither<L, R>;

type _DispatcherWiringErrorImport = BaboonWiringError;

export async function dispatchUeba(
    serviceName: string,
    methodName: string,
    data: Uint8Array,
    impls: {OrderService: OrderService; UserService: UserService},
    rt: IBaboonServiceRt, ctx: BaboonCodecContext
): Promise<BaboonEither<BaboonWiringError, Uint8Array>> {
    switch (serviceName) {
        case "OrderService":
            return await invokeUeba_OrderService({ serviceName, methodName }, data, impls.OrderService, rt, ctx);
        case "UserService":
            return await invokeUeba_UserService({ serviceName, methodName }, data, impls.UserService, rt, ctx);
        default:
            throw new BaboonWiringException({ tag: 'NoMatchingMethod', method: { serviceName, methodName } });
    }
}

export async function dispatchJson(
    serviceName: string,
    methodName: string,
    data: string,
    impls: {OrderService: OrderService; UserService: UserService},
    rt: IBaboonServiceRt, ctx: BaboonCodecContext
): Promise<BaboonEither<BaboonWiringError, string>> {
    switch (serviceName) {
        case "OrderService":
            return await invokeJson_OrderService({ serviceName, methodName }, data, impls.OrderService, rt, ctx);
        case "UserService":
            return await invokeJson_UserService({ serviceName, methodName }, data, impls.UserService, rt, ctx);
        default:
            throw new BaboonWiringException({ tag: 'NoMatchingMethod', method: { serviceName, methodName } });
    }
}