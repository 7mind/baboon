

import { AbstractBaboonJsonCodecs, AbstractBaboonUebaCodecs, BaboonBinCodec, BaboonJsonCodec, BaboonDomainVersion, BaboonMeta, Lazy } from '../../../BaboonSharedRuntime';
import type { AbstractBaboonConversions } from '../../../BaboonSharedRuntime';
import { BaboonCodecsFacade } from '../../../BaboonCodecsFacade';
import { OrderItem, OrderItem_JsonCodec, OrderItem_UEBACodec } from './OrderItem';
import { OrderStatus, OrderStatus_JsonCodec, OrderStatus_UEBACodec } from './OrderStatus';
import { OrderSummary, OrderSummary_JsonCodec, OrderSummary_UEBACodec } from './OrderSummary';
import { UserProfile, UserProfile_JsonCodec, UserProfile_UEBACodec } from './UserProfile';
import { UserStatus, UserStatus_JsonCodec, UserStatus_UEBACodec } from './UserStatus';
import { In as OrderserviceCancelorderIn, In_JsonCodec as OrderserviceCancelorderIn_JsonCodec, In_UEBACodec as OrderserviceCancelorderIn_UEBACodec } from './order-service/cancelorder/in';
import { Out as OrderserviceCancelorderOut, Out_JsonCodec as OrderserviceCancelorderOut_JsonCodec, Out_UEBACodec as OrderserviceCancelorderOut_UEBACodec } from './order-service/cancelorder/out';
import { In as OrderservicePlaceorderIn, In_JsonCodec as OrderservicePlaceorderIn_JsonCodec, In_UEBACodec as OrderservicePlaceorderIn_UEBACodec } from './order-service/placeorder/in';
import { Out as OrderservicePlaceorderOut, Out_JsonCodec as OrderservicePlaceorderOut_JsonCodec, Out_UEBACodec as OrderservicePlaceorderOut_UEBACodec } from './order-service/placeorder/out';
import { In as UserserviceCreateuserIn, In_JsonCodec as UserserviceCreateuserIn_JsonCodec, In_UEBACodec as UserserviceCreateuserIn_UEBACodec } from './user-service/createuser/in';
import { Out as UserserviceCreateuserOut, Out_JsonCodec as UserserviceCreateuserOut_JsonCodec, Out_UEBACodec as UserserviceCreateuserOut_UEBACodec } from './user-service/createuser/out';
import { In as UserserviceGetuserIn, In_JsonCodec as UserserviceGetuserIn_JsonCodec, In_UEBACodec as UserserviceGetuserIn_UEBACodec } from './user-service/getuser/in';
import { Out as UserserviceGetuserOut, Out_JsonCodec as UserserviceGetuserOut_JsonCodec, Out_UEBACodec as UserserviceGetuserOut_UEBACodec } from './user-service/getuser/out';

class DomainMcpMuxStubV1_0_0JsonCodecs extends AbstractBaboonJsonCodecs {
    constructor() {
        super();
        this.register(OrderItem_JsonCodec.BaboonTypeIdentifier, new Lazy<BaboonJsonCodec<unknown>>(() => OrderItem_JsonCodec.instance as unknown as BaboonJsonCodec<unknown>));
        this.register(OrderStatus_JsonCodec.BaboonTypeIdentifier, new Lazy<BaboonJsonCodec<unknown>>(() => OrderStatus_JsonCodec.instance as unknown as BaboonJsonCodec<unknown>));
        this.register(OrderSummary_JsonCodec.BaboonTypeIdentifier, new Lazy<BaboonJsonCodec<unknown>>(() => OrderSummary_JsonCodec.instance as unknown as BaboonJsonCodec<unknown>));
        this.register(UserProfile_JsonCodec.BaboonTypeIdentifier, new Lazy<BaboonJsonCodec<unknown>>(() => UserProfile_JsonCodec.instance as unknown as BaboonJsonCodec<unknown>));
        this.register(UserStatus_JsonCodec.BaboonTypeIdentifier, new Lazy<BaboonJsonCodec<unknown>>(() => UserStatus_JsonCodec.instance as unknown as BaboonJsonCodec<unknown>));
        this.register(OrderserviceCancelorderIn_JsonCodec.BaboonTypeIdentifier, new Lazy<BaboonJsonCodec<unknown>>(() => OrderserviceCancelorderIn_JsonCodec.instance as unknown as BaboonJsonCodec<unknown>));
        this.register(OrderserviceCancelorderOut_JsonCodec.BaboonTypeIdentifier, new Lazy<BaboonJsonCodec<unknown>>(() => OrderserviceCancelorderOut_JsonCodec.instance as unknown as BaboonJsonCodec<unknown>));
        this.register(OrderservicePlaceorderIn_JsonCodec.BaboonTypeIdentifier, new Lazy<BaboonJsonCodec<unknown>>(() => OrderservicePlaceorderIn_JsonCodec.instance as unknown as BaboonJsonCodec<unknown>));
        this.register(OrderservicePlaceorderOut_JsonCodec.BaboonTypeIdentifier, new Lazy<BaboonJsonCodec<unknown>>(() => OrderservicePlaceorderOut_JsonCodec.instance as unknown as BaboonJsonCodec<unknown>));
        this.register(UserserviceCreateuserIn_JsonCodec.BaboonTypeIdentifier, new Lazy<BaboonJsonCodec<unknown>>(() => UserserviceCreateuserIn_JsonCodec.instance as unknown as BaboonJsonCodec<unknown>));
        this.register(UserserviceCreateuserOut_JsonCodec.BaboonTypeIdentifier, new Lazy<BaboonJsonCodec<unknown>>(() => UserserviceCreateuserOut_JsonCodec.instance as unknown as BaboonJsonCodec<unknown>));
        this.register(UserserviceGetuserIn_JsonCodec.BaboonTypeIdentifier, new Lazy<BaboonJsonCodec<unknown>>(() => UserserviceGetuserIn_JsonCodec.instance as unknown as BaboonJsonCodec<unknown>));
        this.register(UserserviceGetuserOut_JsonCodec.BaboonTypeIdentifier, new Lazy<BaboonJsonCodec<unknown>>(() => UserserviceGetuserOut_JsonCodec.instance as unknown as BaboonJsonCodec<unknown>));
    }
}

class DomainMcpMuxStubV1_0_0UebaCodecs extends AbstractBaboonUebaCodecs {
    constructor() {
        super();
        this.register(OrderItem_UEBACodec.BaboonTypeIdentifier, new Lazy<BaboonBinCodec<unknown>>(() => OrderItem_UEBACodec.instance as unknown as BaboonBinCodec<unknown>));
        this.register(OrderStatus_UEBACodec.BaboonTypeIdentifier, new Lazy<BaboonBinCodec<unknown>>(() => OrderStatus_UEBACodec.instance as unknown as BaboonBinCodec<unknown>));
        this.register(OrderSummary_UEBACodec.BaboonTypeIdentifier, new Lazy<BaboonBinCodec<unknown>>(() => OrderSummary_UEBACodec.instance as unknown as BaboonBinCodec<unknown>));
        this.register(UserProfile_UEBACodec.BaboonTypeIdentifier, new Lazy<BaboonBinCodec<unknown>>(() => UserProfile_UEBACodec.instance as unknown as BaboonBinCodec<unknown>));
        this.register(UserStatus_UEBACodec.BaboonTypeIdentifier, new Lazy<BaboonBinCodec<unknown>>(() => UserStatus_UEBACodec.instance as unknown as BaboonBinCodec<unknown>));
        this.register(OrderserviceCancelorderIn_UEBACodec.BaboonTypeIdentifier, new Lazy<BaboonBinCodec<unknown>>(() => OrderserviceCancelorderIn_UEBACodec.instance as unknown as BaboonBinCodec<unknown>));
        this.register(OrderserviceCancelorderOut_UEBACodec.BaboonTypeIdentifier, new Lazy<BaboonBinCodec<unknown>>(() => OrderserviceCancelorderOut_UEBACodec.instance as unknown as BaboonBinCodec<unknown>));
        this.register(OrderservicePlaceorderIn_UEBACodec.BaboonTypeIdentifier, new Lazy<BaboonBinCodec<unknown>>(() => OrderservicePlaceorderIn_UEBACodec.instance as unknown as BaboonBinCodec<unknown>));
        this.register(OrderservicePlaceorderOut_UEBACodec.BaboonTypeIdentifier, new Lazy<BaboonBinCodec<unknown>>(() => OrderservicePlaceorderOut_UEBACodec.instance as unknown as BaboonBinCodec<unknown>));
        this.register(UserserviceCreateuserIn_UEBACodec.BaboonTypeIdentifier, new Lazy<BaboonBinCodec<unknown>>(() => UserserviceCreateuserIn_UEBACodec.instance as unknown as BaboonBinCodec<unknown>));
        this.register(UserserviceCreateuserOut_UEBACodec.BaboonTypeIdentifier, new Lazy<BaboonBinCodec<unknown>>(() => UserserviceCreateuserOut_UEBACodec.instance as unknown as BaboonBinCodec<unknown>));
        this.register(UserserviceGetuserIn_UEBACodec.BaboonTypeIdentifier, new Lazy<BaboonBinCodec<unknown>>(() => UserserviceGetuserIn_UEBACodec.instance as unknown as BaboonBinCodec<unknown>));
        this.register(UserserviceGetuserOut_UEBACodec.BaboonTypeIdentifier, new Lazy<BaboonBinCodec<unknown>>(() => UserserviceGetuserOut_UEBACodec.instance as unknown as BaboonBinCodec<unknown>));
    }
}

class DomainMcpMuxStubV1_0_0Conversions implements AbstractBaboonConversions {
    public versionsFrom(): string[] { return []; }
    public versionTo(): string { return '1.0.0'; }
}

class DomainMcpMuxStubV1_0_0Meta implements BaboonMeta {
    public sameInVersions(_typeId: string): string[] { return ['1.0.0']; }
}

export class DomainMcpMuxStubFacade extends BaboonCodecsFacade {
    constructor() {
        super();
        this.register(
            new BaboonDomainVersion('mcp.mux.stub', '1.0.0'),
            () => new DomainMcpMuxStubV1_0_0JsonCodecs(),
            () => new DomainMcpMuxStubV1_0_0UebaCodecs(),
            () => new DomainMcpMuxStubV1_0_0Conversions(),
            () => new DomainMcpMuxStubV1_0_0Meta(),
        );
    }
}
