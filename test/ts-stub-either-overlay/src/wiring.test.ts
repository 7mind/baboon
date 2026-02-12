import {
    BaboonBinReader,
    BaboonBinWriter,
    BaboonCodecContext,
    BaboonEither,
    BaboonMethodId
} from "./generated/BaboonSharedRuntime";
import {BaboonServiceRtDefault, IBaboonServiceRt} from "./generated/testpkg/pkg0/baboon-service-rt";
import {I1} from "./generated/testpkg/pkg0/I1";
import {In} from "./generated/testpkg/pkg0/i1/testcall/in";
import {Err} from "./generated/testpkg/pkg0/i1/testcall/err";
import {Out} from "./generated/testpkg/pkg0/i1/testcall/out";
import {T7_Empty} from "./generated/testpkg/pkg0/T7_Empty";
import {I2} from "./generated/testpkg/pkg0/I2";
import {In as In_No_Err} from "./generated/testpkg/pkg0/i2/noerrcall/in";
import {Out as Out_No_Err} from "./generated/testpkg/pkg0/i2/noerrcall/out";
import {invokeJson_I1, invokeUeba_I1} from "./generated/testpkg/pkg0/i1-wiring";
import {invokeJson_I2, invokeUeba_I2} from "./generated/testpkg/pkg0/i2-wiring";

const ctx = BaboonCodecContext.Default;
const rt: IBaboonServiceRt = BaboonServiceRtDefault;

// ==================== Mock implementations ====================

const mockI1: I1 = {
    testCall(arg: In): BaboonEither<Err, Out> {
        return { tag: "Right", value: new Out(42) };
    },
    testCall2(arg: T7_Empty): BaboonEither<T7_Empty, T7_Empty> {
        return { tag: "Right", value: new T7_Empty() };
    },
};

const failingI1: I1 = {
    testCall(arg: In): BaboonEither<Err, Out> {
        return { tag: "Left", value: new Err("domain error" ) };
    },
    testCall2(arg: T7_Empty): BaboonEither<T7_Empty, T7_Empty> {
        return { tag: "Left", value: new T7_Empty() };
    },
};

const throwingI1: I1 = {
    testCall(_arg: In): BaboonEither<Err, Out> {
        throw new Error("service error");
    },
    testCall2(_arg: T7_Empty): BaboonEither<T7_Empty, T7_Empty> {
        throw new Error("service error");
    },
};

const mockI2: I2 = {
    noErrCall(arg: In_No_Err): Out_No_Err {
        return new Out_No_Err(arg.value.toString());
    },
};

// ==================== I1 JSON Tests ====================

describe("I1 JSON wiring", () => {
    test("testCall returns Right for success", () => {
        const method: BaboonMethodId = { serviceName: "I1", methodName: "testCall" };
        const inputJson = JSON.stringify(In.jsonCodec().encode(BaboonCodecContext.Default, new In()));

        const result = invokeJson_I1(method, inputJson, mockI1, rt, ctx);

        expect(result.tag).toBe("Right");
        if (result.tag === "Right") {
            const decoded = Out.jsonCodec().decode(BaboonCodecContext.Default, JSON.parse(result.value));
            expect(decoded.i00).toBe(42);
        }
    });

    test("testCall2 returns Right for success", () => {
        const method: BaboonMethodId = { serviceName: "I1", methodName: "testCall2" };
        const inputJson = JSON.stringify(T7_Empty.jsonCodec().encode(BaboonCodecContext.Default, new T7_Empty()));

        const result = invokeJson_I1(method, inputJson, mockI1, rt, ctx);

        expect(result.tag).toBe("Right");
    });

    test("returns Left(CallFailed) for domain error", () => {
        const method: BaboonMethodId = { serviceName: "I1", methodName: "testCall" };
        const inputJson = JSON.stringify(In.jsonCodec().encode(BaboonCodecContext.Default, new In()));

        const result = invokeJson_I1(method, inputJson, failingI1, rt, ctx);

        expect(result.tag).toBe("Left");
        if (result.tag === "Left") {
            expect(result.value.tag).toBe("CallFailed");
        }
    });

    test("returns Left(NoMatchingMethod) for unknown method", () => {
        const method: BaboonMethodId = { serviceName: "I1", methodName: "nonexistent" };

        const result = invokeJson_I1(method, "{}", mockI1, rt, ctx);

        expect(result.tag).toBe("Left");
        if (result.tag === "Left") {
            expect(result.value.tag).toBe("NoMatchingMethod");
        }
    });

    test("returns Left(DecoderFailed) for bad input", () => {
        const method: BaboonMethodId = { serviceName: "I1", methodName: "testCall" };

        const result = invokeJson_I1(method, "not valid json!!", mockI1, rt, ctx);

        expect(result.tag).toBe("Left");
        if (result.tag === "Left") {
            expect(result.value.tag).toBe("DecoderFailed");
        }
    });

    test("returns Left(CallFailed) when service throws", () => {
        const method: BaboonMethodId = { serviceName: "I1", methodName: "testCall" };
        const inputJson = JSON.stringify(In.jsonCodec().encode(BaboonCodecContext.Default, new In()));

        const result = invokeJson_I1(method, inputJson, throwingI1, rt, ctx);

        expect(result.tag).toBe("Left");
        if (result.tag === "Left") {
            expect(result.value.tag).toBe("CallFailed");
        }
    });
});

// ==================== I1 UEBA Tests ====================

describe("I1 UEBA wiring", () => {
    test("testCall returns Right for success", () => {
        const method: BaboonMethodId = { serviceName: "I1", methodName: "testCall" };
        const writer = new BaboonBinWriter();
        In.binCodec().encode(ctx, new In(), writer);
        const inputBytes = writer.toBytes();

        const result = invokeUeba_I1(method, inputBytes, mockI1, rt, ctx);

        expect(result.tag).toBe("Right");
        if (result.tag === "Right") {
            const reader = new BaboonBinReader(result.value);
            const decoded = Out.binCodec().decode(ctx, reader);
            expect(decoded.i00).toBe(42);
        }
    });

    test("returns Left(NoMatchingMethod) for unknown method", () => {
        const method: BaboonMethodId = { serviceName: "I1", methodName: "nonexistent" };

        const result = invokeUeba_I1(method, new Uint8Array(0), mockI1, rt, ctx);

        expect(result.tag).toBe("Left");
        if (result.tag === "Left") {
            expect(result.value.tag).toBe("NoMatchingMethod");
        }
    });

    test("returns Left(CallFailed) when service throws", () => {
        const method: BaboonMethodId = { serviceName: "I1", methodName: "testCall" };
        const writer = new BaboonBinWriter();
        In.binCodec().encode(ctx, new In(), writer);
        const inputBytes = writer.toBytes();

        const result = invokeUeba_I1(method, inputBytes, throwingI1, rt, ctx);

        expect(result.tag).toBe("Left");
        if (result.tag === "Left") {
            expect(result.value.tag).toBe("CallFailed");
        }
    });
});

// ==================== I2 Tests (no err type) ====================

describe("I2 JSON wiring", () => {
    test("noErrCall returns Right for success", () => {
        const method: BaboonMethodId = { serviceName: "I2", methodName: "noErrCall" };
        const inputJson = JSON.stringify(In_No_Err.jsonCodec().encode(BaboonCodecContext.Default, new In_No_Err(123)));

        const result = invokeJson_I2(method, inputJson, mockI2, rt, ctx);

        expect(result.tag).toBe("Right");
        if (result.tag === "Right") {
            const decoded = Out_No_Err.jsonCodec().decode(BaboonCodecContext.Default, JSON.parse(result.value));
            expect(decoded.result).toBe("123");
        }
    });
});

describe("I2 UEBA wiring", () => {
    test("noErrCall returns Right for success", () => {
        const method: BaboonMethodId = { serviceName: "I2", methodName: "noErrCall" };
        const writer = new BaboonBinWriter();
        In_No_Err.binCodec().encode(ctx, new In_No_Err(456), writer);
        const inputBytes = writer.toBytes();

        const result = invokeUeba_I2(method, inputBytes, mockI2, rt, ctx);

        expect(result.tag).toBe("Right");
        if (result.tag === "Right") {
            const reader = new BaboonBinReader(result.value);
            const decoded = Out_No_Err.binCodec().decode(ctx, reader);
            expect(decoded.result).toBe("456");
        }
    });
});
