import {BaboonBinReader, BaboonBinWriter, BaboonCodecContext} from '../../../../BaboonSharedRuntime'
import {In as cancelorder_In, In_JsonCodec as cancelorder_In_JsonCodec, In_UEBACodec as cancelorder_In_UEBACodec} from './cancelorder/in'
import {Out as cancelorder_Out, Out_JsonCodec as cancelorder_Out_JsonCodec, Out_UEBACodec as cancelorder_Out_UEBACodec} from './cancelorder/out'
import {In as placeorder_In, In_JsonCodec as placeorder_In_JsonCodec, In_UEBACodec as placeorder_In_UEBACodec} from './placeorder/in'
import {Out as placeorder_Out, Out_JsonCodec as placeorder_Out_JsonCodec, Out_UEBACodec as placeorder_Out_UEBACodec} from './placeorder/out'

export class OrderServiceClient {
    private readonly transportUeba: (service: string, method: string, data: Uint8Array) => Promise<Uint8Array>
    private readonly transportJson: (service: string, method: string, data: string) => Promise<string>

    constructor(transportUeba: (service: string, method: string, data: Uint8Array) => Promise<Uint8Array>, transportJson: (service: string, method: string, data: string) => Promise<string>) {
        this.transportUeba = transportUeba;
        this.transportJson = transportJson;
    }

    public async placeOrder(arg: placeorder_In, ctx: BaboonCodecContext = BaboonCodecContext.Default): Promise<placeorder_Out> {
        const writer = new BaboonBinWriter();
        placeorder_In_UEBACodec.instance.encode(ctx, arg, writer);;
        const resp = await this.transportUeba("OrderService", "placeOrder", writer.toBytes());
        return placeorder_Out_UEBACodec.instance.decode(ctx, new BaboonBinReader(resp));
    }
    
    public async placeOrderJson(arg: placeorder_In): Promise<placeorder_Out> {
        const encoded = JSON.stringify(placeorder_In_JsonCodec.instance.encode(BaboonCodecContext.Default, arg));
        const resp = await this.transportJson("OrderService", "placeOrder", encoded);
        return placeorder_Out_JsonCodec.instance.decode(BaboonCodecContext.Default, JSON.parse(resp)) as placeorder_Out;
    }
    
    public async cancelOrder(arg: cancelorder_In, ctx: BaboonCodecContext = BaboonCodecContext.Default): Promise<cancelorder_Out> {
        const writer = new BaboonBinWriter();
        cancelorder_In_UEBACodec.instance.encode(ctx, arg, writer);;
        const resp = await this.transportUeba("OrderService", "cancelOrder", writer.toBytes());
        return cancelorder_Out_UEBACodec.instance.decode(ctx, new BaboonBinReader(resp));
    }
    
    public async cancelOrderJson(arg: cancelorder_In): Promise<cancelorder_Out> {
        const encoded = JSON.stringify(cancelorder_In_JsonCodec.instance.encode(BaboonCodecContext.Default, arg));
        const resp = await this.transportJson("OrderService", "cancelOrder", encoded);
        return cancelorder_Out_JsonCodec.instance.decode(BaboonCodecContext.Default, JSON.parse(resp)) as cancelorder_Out;
    }
}