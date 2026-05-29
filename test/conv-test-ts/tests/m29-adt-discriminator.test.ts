import {describe, expect, test} from "vitest";
import {BaboonCodecContext} from "../src/generated/BaboonSharedRuntime";
import {
    IntStrEnvelope,
    IntStrEnvelope_Ok,
    IntStrEnvelope_Err,
} from "../src/generated/convtest/m29ok/IntStrEnvelope";

// The `$type` discriminant turns the ADT union into a TS discriminated union: narrowing on
// `value.$type === '<branch id>'` selects the concrete branch type (this file type-checks under
// `tsc` only if narrowing works) and resolves at runtime. The discriminant value is the branch's
// own type identifier, and it must NOT appear on the JSON wire.

const OK_ID = "convtest.m29ok/[convtest.m29ok/:#IntStrEnvelope]#Ok";
const ERR_ID = "convtest.m29ok/[convtest.m29ok/:#IntStrEnvelope]#Err";

describe("ADT $type discriminator (TypeScript)", () => {
    test("narrows the union to the concrete branch", () => {
        const items: IntStrEnvelope[] = [new IntStrEnvelope_Ok(42), new IntStrEnvelope_Err("oops")];
        const seen: string[] = [];
        for (const it of items) {
            if (it.$type === OK_ID) {
                // narrowed to IntStrEnvelope_Ok — `.value` is branch-specific
                seen.push(`ok:${it.value}`);
            } else {
                // narrowed to IntStrEnvelope_Err — `.error` is branch-specific
                seen.push(`err:${it.error}`);
            }
        }
        expect(seen).toEqual(["ok:42", "err:oops"]);
    });

    test("discriminant equals the branch type identifier", () => {
        expect(new IntStrEnvelope_Ok(1).$type).toBe(OK_ID);
        expect(new IntStrEnvelope_Err("x").$type).toBe(ERR_ID);
        expect(OK_ID).toBe(IntStrEnvelope_Ok.BaboonTypeIdentifier);
        expect(ERR_ID).toBe(IntStrEnvelope_Err.BaboonTypeIdentifier);
    });

    test("$type does not leak onto the JSON wire", () => {
        const encoded = IntStrEnvelope_Ok.jsonCodec().encode(BaboonCodecContext.Default, new IntStrEnvelope_Ok(7));
        expect(encoded).toEqual({value: 7});
        expect(JSON.stringify(encoded)).not.toContain("$type");
    });
});
