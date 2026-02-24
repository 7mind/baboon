import {BaboonBinReader, BaboonBinWriter, BaboonMethodId} from "./baboondefinitions/generated/BaboonSharedRuntime";
import {Outcome, OutcomeServiceRt} from "./custom-containers";
import {IBaboonServiceRt} from "./baboondefinitions/generated/testpkg/pkg0/baboon-service-rt";
import {I1} from "./baboondefinitions/generated/testpkg/pkg0/I1";
import {In} from "./baboondefinitions/generated/testpkg/pkg0/i1/testcall/in";
import {In as In_No_Err} from "./baboondefinitions/generated/testpkg/pkg0/i2/noerrcall/in";
import {Out as Out_No_Err} from "./baboondefinitions/generated/testpkg/pkg0/i2/noerrcall/out";
import {Out} from "./baboondefinitions/generated/testpkg/pkg0/i1/testcall/out";
import {T7_Empty} from "./baboondefinitions/generated/testpkg/pkg0/T7_Empty";
import {I2} from "./baboondefinitions/generated/testpkg/pkg0/I2";
import {invokeJson_I1, invokeUeba_I1} from "./baboondefinitions/generated/testpkg/pkg0/i1-wiring";
import {invokeJson_I2, invokeUeba_I2} from "./baboondefinitions/generated/testpkg/pkg0/i2-wiring";
import {BaboonCodecContext} from "./baboondefinitions/generated/BaboonSharedRuntime";

const ctx = BaboonCodecContext.Default;
const rt: IBaboonServiceRt = OutcomeServiceRt;

const mockI1: I1 = {
    testCall(arg: In): Outcome<Out> {
        return { tag: "Success", value: new Out( 42 ) };
    },
    testCall2(arg: T7_Empty): Outcome<T7_Empty> {
        return { tag: "Success", value: new T7_Empty() };
    },
};

const failingI1: I1 = {
    testCall(arg: In): Outcome<Out> {
        return { tag: "Failure", error: { msg: "domain error" } };
    },
    testCall2(arg: T7_Empty): Outcome<T7_Empty> {
        return { tag: "Failure", error: {} };
    },
};

const throwingI1: I1 = {
    testCall(_arg: In): Outcome<Out> {
        throw new Error("service error");
    },
    testCall2(_arg: T7_Empty): Outcome<T7_Empty> {
        throw new Error("service error");
    },
};

const mockI2: I2 = {
    noErrCall(arg: In_No_Err): Out_No_Err {
        return new Out_No_Err(arg.value.toString());
    },
};

describe("I1 JSON wiring (Outcome)", () => {
    test("testCall returns Success", () => {
        const method: BaboonMethodId = { serviceName: "I1", methodName: "testCall" };
        const inputJson = JSON.stringify(In.jsonCodec().encode(BaboonCodecContext.Default, new In()));
        const result = invokeJson_I1(method, inputJson, mockI1, rt, ctx) as Outcome<string>;
        expect(result.tag).toBe("Success");
        if (result.tag === "Success") {
            const decoded = Out.jsonCodec().decode(BaboonCodecContext.Default, JSON.parse(result.value));
            expect(decoded.i00).toBe(42);
        }
    });

    test("testCall2 returns Success", () => {
        const method: BaboonMethodId = { serviceName: "I1", methodName: "testCall2" };
        const inputJson = JSON.stringify(T7_Empty.jsonCodec().encode(BaboonCodecContext.Default, new In()));
        const result = invokeJson_I1(method, inputJson, mockI1, rt, ctx) as Outcome<string>;
        expect(result.tag).toBe("Success");
    });

    test("returns Failure for domain error", () => {
        const method: BaboonMethodId = { serviceName: "I1", methodName: "testCall" };
        const inputJson = JSON.stringify(In.jsonCodec().encode(BaboonCodecContext.Default, new In()));
        const result = invokeJson_I1(method, inputJson, failingI1, rt, ctx) as Outcome<string>;
        expect(result.tag).toBe("Failure");
    });

    test("returns Failure for unknown method", () => {
        const method: BaboonMethodId = { serviceName: "I1", methodName: "nonexistent" };
        const result = invokeJson_I1(method, "{}", mockI1, rt, ctx) as Outcome<string>;
        expect(result.tag).toBe("Failure");
    });

    test("returns Failure for bad input", () => {
        const method: BaboonMethodId = { serviceName: "I1", methodName: "testCall" };
        const result = invokeJson_I1(method, "not valid json!!", mockI1, rt, ctx) as Outcome<string>;
        expect(result.tag).toBe("Failure");
    });

    test("returns Failure when service throws", () => {
        const method: BaboonMethodId = { serviceName: "I1", methodName: "testCall" };
        const inputJson = JSON.stringify(In.jsonCodec().encode(BaboonCodecContext.Default, new In()));
        const result = invokeJson_I1(method, inputJson, throwingI1, rt, ctx) as Outcome<string>;
        expect(result.tag).toBe("Failure");
    });
});

describe("I1 UEBA wiring (Outcome)", () => {
    test("testCall returns Success", () => {
        const method: BaboonMethodId = { serviceName: "I1", methodName: "testCall" };
        const writer = new BaboonBinWriter();
        In.binCodec().encode(ctx, new In(), writer);
        const result = invokeUeba_I1(method, writer.toBytes(), mockI1, rt, ctx) as Outcome<Uint8Array>;
        expect(result.tag).toBe("Success");
        if (result.tag === "Success") {
            const reader = new BaboonBinReader(result.value);
            const decoded = Out.binCodec().decode(ctx, reader);
            expect(decoded.i00).toBe(42);
        }
    });

    test("returns Failure for unknown method", () => {
        const method: BaboonMethodId = { serviceName: "I1", methodName: "nonexistent" };
        const result = invokeUeba_I1(method, new Uint8Array(0), mockI1, rt, ctx) as Outcome<Uint8Array>;
        expect(result.tag).toBe("Failure");
    });

    test("returns Failure when service throws", () => {
        const method: BaboonMethodId = { serviceName: "I1", methodName: "testCall" };
        const writer = new BaboonBinWriter();
        In.binCodec().encode(ctx, new In(), writer);
        const result = invokeUeba_I1(method, writer.toBytes(), throwingI1, rt, ctx) as Outcome<Uint8Array>;
        expect(result.tag).toBe("Failure");
    });
});

describe("I2 JSON wiring (Outcome)", () => {
    test("noErrCall returns Success", () => {
        const method: BaboonMethodId = { serviceName: "I2", methodName: "noErrCall" };
        const inputJson = JSON.stringify(In_No_Err.jsonCodec().encode(BaboonCodecContext.Default, new In_No_Err(123)));
        const result = invokeJson_I2(method, inputJson, mockI2, rt, ctx) as Outcome<string>;
        expect(result.tag).toBe("Success");
        if (result.tag === "Success") {
            const decoded = Out_No_Err.jsonCodec().decode(BaboonCodecContext.Default, JSON.parse(result.value));
            expect(decoded.result).toBe("123");
        }
    });
});

describe("I2 UEBA wiring (Outcome)", () => {
    test("noErrCall returns Success", () => {
        const method: BaboonMethodId = { serviceName: "I2", methodName: "noErrCall" };
        const writer = new BaboonBinWriter();
        In_No_Err.binCodec().encode(ctx, new In_No_Err(456), writer);
        const result = invokeUeba_I2(method, writer.toBytes(), mockI2, rt, ctx) as Outcome<Uint8Array>;
        expect(result.tag).toBe("Success");
        if (result.tag === "Success") {
            const reader = new BaboonBinReader(result.value);
            const decoded = Out_No_Err.binCodec().decode(ctx, reader);
            expect(decoded.result).toBe("456");
        }
    });
});