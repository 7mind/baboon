import { expect, test } from "vitest";
import { BaboonCodecContext } from "../generated-review/BaboonSharedRuntime";
import { ScalarCollections, ScalarCollections_JsonCodec } from "../generated-review/review/evolution/ScalarCollections";

test("identity collection encoding materializes sparse values and preserves set order", () => {
    const values = new Array<number>(3); values[1] = 42;
    const encoded = ScalarCollections_JsonCodec.instance.encode(BaboonCodecContext.Default,
        new ScalarCollections(values, new Set(["b", "a", "b"])));
    expect(encoded).toStrictEqual({ values: [undefined, 42, undefined], uniques: ["b", "a"] });
    expect(JSON.stringify(encoded)).toBe('{"values":[null,42,null],"uniques":["b","a"]}');
});

test("identity collection encoding retains custom iterator and getter order", () => {
    const events: string[] = [];
    const values = [1, 2];
    Object.defineProperty(values, "0", { get: () => { events.push("get"); return 1; } });
    values[Symbol.iterator] = function* (): Generator<number, undefined, unknown> {
        events.push("start"); yield this[0]; events.push("middle"); yield this[1]; events.push("end");
        return undefined;
    };
    expect(ScalarCollections_JsonCodec.instance.encode(BaboonCodecContext.Default, new ScalarCollections(values, new Set())))
        .toStrictEqual({ values: [1, 2], uniques: [] });
    expect(events).toEqual(["start", "get", "middle", "end"]);
});
