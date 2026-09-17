import { expect, test } from "vitest";
import { BaboonCodecContext, BaboonWiringException, JsonMuxer, JsonMuxerCtx, UebaMuxer, UebaMuxerCtx } from "../generated/BaboonSharedRuntime";

test("JSON service registries preserve order, duplicate errors and context forwarding", () => {
    const ctx = BaboonCodecContext.Indexed;
    const service = { serviceName: "one", invoke: (_method: unknown, data: string, codecCtx: BaboonCodecContext) => {
        expect(codecCtx).toBe(ctx); return data;
    } };
    const mux = new JsonMuxer<string>(service);
    mux.register({ ...service, serviceName: "two" });
    expect(mux.serviceNames()).toEqual(["one", "two"]);
    expect(() => mux.register(service)).toThrow(BaboonWiringException);
    expect(() => mux.invoke({ serviceName: "missing", methodName: "x" }, "{}", ctx)).toThrow(BaboonWiringException);
    expect(mux.invoke({ serviceName: "one", methodName: "x" }, "{}", ctx)).toBe("{}");
    const contextual = new JsonMuxerCtx<number, string>({ serviceName: "one", invoke: (_method, data, state, codecCtx) => {
        expect(state).toBe(42); expect(codecCtx).toBe(ctx); return data;
    } });
    expect(contextual.invoke({ serviceName: "one", methodName: "x" }, "{}", 42, ctx)).toBe("{}");
    expect(contextual.serviceNames()).toEqual(["one"]);
});

test("UEBA service registries retain the supplied bytes and context", () => {
    const data = new Uint8Array([1, 2]);
    const ctx = BaboonCodecContext.Compact;
    const mux = new UebaMuxer<Uint8Array>({ serviceName: "one", invoke: (_method, bytes, codecCtx) => {
        expect(codecCtx).toBe(ctx); return bytes;
    } });
    expect(mux.invoke({ serviceName: "one", methodName: "x" }, data, ctx)).toBe(data);
    const contextual = new UebaMuxerCtx<number, Uint8Array>({ serviceName: "one", invoke: (_method, bytes, state, codecCtx) => {
        expect(state).toBe(42); expect(codecCtx).toBe(ctx); return bytes;
    } });
    expect(contextual.invoke({ serviceName: "one", methodName: "x" }, data, 42, ctx)).toBe(data);
    expect(contextual.serviceNames()).toEqual(["one"]);
});
