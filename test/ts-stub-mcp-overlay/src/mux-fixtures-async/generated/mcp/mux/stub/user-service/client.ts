import {BaboonBinReader, BaboonBinWriter, BaboonCodecContext} from '../../../../BaboonSharedRuntime'
import {In as createuser_In, In_JsonCodec as createuser_In_JsonCodec, In_UEBACodec as createuser_In_UEBACodec} from './createuser/in'
import {Out as createuser_Out, Out_JsonCodec as createuser_Out_JsonCodec, Out_UEBACodec as createuser_Out_UEBACodec} from './createuser/out'
import {In as getuser_In, In_JsonCodec as getuser_In_JsonCodec, In_UEBACodec as getuser_In_UEBACodec} from './getuser/in'
import {Out as getuser_Out, Out_JsonCodec as getuser_Out_JsonCodec, Out_UEBACodec as getuser_Out_UEBACodec} from './getuser/out'

export class UserServiceClient {
    private readonly transportUeba: (service: string, method: string, data: Uint8Array) => Promise<Uint8Array>
    private readonly transportJson: (service: string, method: string, data: string) => Promise<string>

    constructor(transportUeba: (service: string, method: string, data: Uint8Array) => Promise<Uint8Array>, transportJson: (service: string, method: string, data: string) => Promise<string>) {
        this.transportUeba = transportUeba;
        this.transportJson = transportJson;
    }

    public async createUser(arg: createuser_In, ctx: BaboonCodecContext = BaboonCodecContext.Default): Promise<createuser_Out> {
        const writer = new BaboonBinWriter();
        createuser_In_UEBACodec.instance.encode(ctx, arg, writer);;
        const resp = await this.transportUeba("UserService", "createUser", writer.toBytes());
        return createuser_Out_UEBACodec.instance.decode(ctx, new BaboonBinReader(resp));
    }
    
    public async createUserJson(arg: createuser_In): Promise<createuser_Out> {
        const encoded = JSON.stringify(createuser_In_JsonCodec.instance.encode(BaboonCodecContext.Default, arg));
        const resp = await this.transportJson("UserService", "createUser", encoded);
        return createuser_Out_JsonCodec.instance.decode(BaboonCodecContext.Default, JSON.parse(resp)) as createuser_Out;
    }
    
    public async getUser(arg: getuser_In, ctx: BaboonCodecContext = BaboonCodecContext.Default): Promise<getuser_Out> {
        const writer = new BaboonBinWriter();
        getuser_In_UEBACodec.instance.encode(ctx, arg, writer);;
        const resp = await this.transportUeba("UserService", "getUser", writer.toBytes());
        return getuser_Out_UEBACodec.instance.decode(ctx, new BaboonBinReader(resp));
    }
    
    public async getUserJson(arg: getuser_In): Promise<getuser_Out> {
        const encoded = JSON.stringify(getuser_In_JsonCodec.instance.encode(BaboonCodecContext.Default, arg));
        const resp = await this.transportJson("UserService", "getUser", encoded);
        return getuser_Out_JsonCodec.instance.decode(BaboonCodecContext.Default, JSON.parse(resp)) as getuser_Out;
    }
}