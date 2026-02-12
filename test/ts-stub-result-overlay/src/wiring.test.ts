import { BaboonCodecContext, BaboonMethodId, BaboonBinWriter, BaboonBinReader } from "./baboon_runtime";
import { IBaboonServiceRt } from "./testpkg/pkg0/baboon-service-rt";
import { Result, ResultServiceRt } from "./custom-containers";
import { I1 } from "./testpkg/pkg0/i1";
import { I2 } from "./testpkg/pkg0/i2";
import { invokeJson_I1, invokeUeba_I1 } from "./testpkg/pkg0/i1-wiring";
import { invokeJson_I2, invokeUeba_I2 } from "./testpkg/pkg0/i2-wiring";
import { in_ as I1_testCall_in } from "./testpkg/pkg0/i1/testcall/in";
import { out as I1_testCall_out } from "./testpkg/pkg0/i1/testcall/out";
import { err as I1_testCall_err } from "./testpkg/pkg0/i1/testcall/err";
import { encode_in__json as encode_I1_testCall_in_json } from "./testpkg/pkg0/i1/testcall/in";
import { decode_out_json as decode_I1_testCall_out_json } from "./testpkg/pkg0/i1/testcall/out";
import { encode_in__ueba as encode_I1_testCall_in_ueba } from "./testpkg/pkg0/i1/testcall/in";
import { decode_out_ueba as decode_I1_testCall_out_ueba } from "./testpkg/pkg0/i1/testcall/out";
import { T7_Empty } from "./testpkg/pkg0/t7_empty";
import { encode_T7_Empty_json } from "./testpkg/pkg0/t7_empty";
import { in_ as I2_noErrCall_in } from "./testpkg/pkg0/i2/noerrcall/in";
import { out as I2_noErrCall_out } from "./testpkg/pkg0/i2/noerrcall/out";
import { encode_in__json as encode_I2_noErrCall_in_json } from "./testpkg/pkg0/i2/noerrcall/in";
import { decode_out_json as decode_I2_noErrCall_out_json } from "./testpkg/pkg0/i2/noerrcall/out";
import { encode_in__ueba as encode_I2_noErrCall_in_ueba } from "./testpkg/pkg0/i2/noerrcall/in";
import { decode_out_ueba as decode_I2_noErrCall_out_ueba } from "./testpkg/pkg0/i2/noerrcall/out";

const ctx = BaboonCodecContext.Unindexed;
const rt: IBaboonServiceRt = ResultServiceRt;

const mockI1: I1 = {
    testCall(arg: I1_testCall_in): Result<I1_testCall_out, I1_testCall_err> {
        return { tag: "Success", value: { i00: 42 } };
    },
    testCall2(arg: T7_Empty): Result<T7_Empty, T7_Empty> {
        return { tag: "Success", value: {} };
    },
};

const failingI1: I1 = {
    testCall(arg: I1_testCall_in): Result<I1_testCall_out, I1_testCall_err> {
        return { tag: "Failure", error: { msg: "domain error" } };
    },
    testCall2(arg: T7_Empty): Result<T7_Empty, T7_Empty> {
        return { tag: "Failure", error: {} };
    },
};

const throwingI1: I1 = {
    testCall(_arg: I1_testCall_in): Result<I1_testCall_out, I1_testCall_err> {
        throw new Error("service error");
    },
    testCall2(_arg: T7_Empty): Result<T7_Empty, T7_Empty> {
        throw new Error("service error");
    },
};

const mockI2: I2 = {
    noErrCall(arg: I2_noErrCall_in): I2_noErrCall_out {
        return { result: "result_" + arg.value.toString() };
    },
};

describe("I1 JSON wiring (Result)", () => {
    test("testCall returns Success", () => {
        const method: BaboonMethodId = { serviceName: "I1", methodName: "testCall" };
        const inputJson = JSON.stringify(encode_I1_testCall_in_json({}));
        const result = invokeJson_I1(method, inputJson, mockI1, rt, ctx) as Result<string, unknown>;
        expect(result.tag).toBe("Success");
        if (result.tag === "Success") {
            const decoded = decode_I1_testCall_out_json(JSON.parse(result.value));
            expect(decoded.i00).toBe(42);
        }
    });

    test("testCall2 returns Success", () => {
        const method: BaboonMethodId = { serviceName: "I1", methodName: "testCall2" };
        const inputJson = JSON.stringify(encode_T7_Empty_json({}));
        const result = invokeJson_I1(method, inputJson, mockI1, rt, ctx) as Result<string, unknown>;
        expect(result.tag).toBe("Success");
    });

    test("returns Failure for domain error", () => {
        const method: BaboonMethodId = { serviceName: "I1", methodName: "testCall" };
        const inputJson = JSON.stringify(encode_I1_testCall_in_json({}));
        const result = invokeJson_I1(method, inputJson, failingI1, rt, ctx) as Result<string, unknown>;
        expect(result.tag).toBe("Failure");
    });

    test("returns Failure for unknown method", () => {
        const method: BaboonMethodId = { serviceName: "I1", methodName: "nonexistent" };
        const result = invokeJson_I1(method, "{}", mockI1, rt, ctx) as Result<string, unknown>;
        expect(result.tag).toBe("Failure");
    });

    test("returns Failure for bad input", () => {
        const method: BaboonMethodId = { serviceName: "I1", methodName: "testCall" };
        const result = invokeJson_I1(method, "not valid json!!", mockI1, rt, ctx) as Result<string, unknown>;
        expect(result.tag).toBe("Failure");
    });

    test("returns Failure when service throws", () => {
        const method: BaboonMethodId = { serviceName: "I1", methodName: "testCall" };
        const inputJson = JSON.stringify(encode_I1_testCall_in_json({}));
        const result = invokeJson_I1(method, inputJson, throwingI1, rt, ctx) as Result<string, unknown>;
        expect(result.tag).toBe("Failure");
    });
});

describe("I1 UEBA wiring (Result)", () => {
    test("testCall returns Success", () => {
        const method: BaboonMethodId = { serviceName: "I1", methodName: "testCall" };
        const writer = new BaboonBinWriter();
        encode_I1_testCall_in_ueba({}, ctx, writer);
        const result = invokeUeba_I1(method, writer.toBytes(), mockI1, rt, ctx) as Result<Uint8Array, unknown>;
        expect(result.tag).toBe("Success");
        if (result.tag === "Success") {
            const reader = new BaboonBinReader(result.value);
            const decoded = decode_I1_testCall_out_ueba(ctx, reader);
            expect(decoded.i00).toBe(42);
        }
    });

    test("returns Failure for unknown method", () => {
        const method: BaboonMethodId = { serviceName: "I1", methodName: "nonexistent" };
        const result = invokeUeba_I1(method, new Uint8Array(0), mockI1, rt, ctx) as Result<Uint8Array, unknown>;
        expect(result.tag).toBe("Failure");
    });

    test("returns Failure when service throws", () => {
        const method: BaboonMethodId = { serviceName: "I1", methodName: "testCall" };
        const writer = new BaboonBinWriter();
        encode_I1_testCall_in_ueba({}, ctx, writer);
        const result = invokeUeba_I1(method, writer.toBytes(), throwingI1, rt, ctx) as Result<Uint8Array, unknown>;
        expect(result.tag).toBe("Failure");
    });
});

describe("I2 JSON wiring (Result)", () => {
    test("noErrCall returns Success", () => {
        const method: BaboonMethodId = { serviceName: "I2", methodName: "noErrCall" };
        const inputJson = JSON.stringify(encode_I2_noErrCall_in_json({ value: 123 }));
        const result = invokeJson_I2(method, inputJson, mockI2, rt, ctx) as Result<string, unknown>;
        expect(result.tag).toBe("Success");
        if (result.tag === "Success") {
            const decoded = decode_I2_noErrCall_out_json(JSON.parse(result.value));
            expect(decoded.result).toBe("result_123");
        }
    });
});

describe("I2 UEBA wiring (Result)", () => {
    test("noErrCall returns Success", () => {
        const method: BaboonMethodId = { serviceName: "I2", methodName: "noErrCall" };
        const writer = new BaboonBinWriter();
        encode_I2_noErrCall_in_ueba({ value: 456 }, ctx, writer);
        const result = invokeUeba_I2(method, writer.toBytes(), mockI2, rt, ctx) as Result<Uint8Array, unknown>;
        expect(result.tag).toBe("Success");
        if (result.tag === "Success") {
            const reader = new BaboonBinReader(result.value);
            const decoded = decode_I2_noErrCall_out_ueba(ctx, reader);
            expect(decoded.result).toBe("result_456");
        }
    });
});
