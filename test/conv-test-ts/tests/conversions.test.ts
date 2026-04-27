import {describe, test, expect} from "vitest";
import * as fs from "fs";
import * as path from "path";
import {fileURLToPath} from "url";
import {AllBasicTypes} from "../src/generated/convtest/testpkg/AllBasicTypes";
import {AnyShowcase} from "../src/generated/convtest/testpkg/AnyShowcase";
import {InnerPayload} from "../src/generated/convtest/testpkg/InnerPayload";
import {BaboonBinReader, BaboonCodecContext} from "../src/generated/BaboonSharedRuntime";
import {AnyOpaque} from "../src/generated/BaboonAnyOpaque";


const __dirname = path.dirname(fileURLToPath(import.meta.url));
const compatDir = path.resolve(__dirname, "../../../target/compat-test");

const languages = ["scala", "cs", "rust", "python", "typescript", "kotlin", "java", "dart", "swift"];

describe("Cross-language JSON compatibility", () => {
    for (const lang of languages) {
        const jsonPath = path.join(compatDir, `${lang}-json`, "all-basic-types.json");

        test(`decode ${lang} JSON`, () => {
            if (!fs.existsSync(jsonPath)) {
                console.warn(`Skipping ${lang}: ${jsonPath} not found`);
                return;
            }

            const data = fs.readFileSync(jsonPath, "utf-8");
            const json = JSON.parse(data);
            const decoded = AllBasicTypes.jsonCodec().decode(BaboonCodecContext.Default, json);
            const reEncoded = AllBasicTypes.jsonCodec().encode(BaboonCodecContext.Default, decoded);
            const reDecoded = AllBasicTypes.jsonCodec().decode(BaboonCodecContext.Default, reEncoded);
            expect(reDecoded).toStrictEqual(decoded);
        });
    }
});

// -----------------------------------------------------------------------------
// AnyShowcase cross-language tests (M13 / PR 13.2)
// -----------------------------------------------------------------------------

const expectedAnyPayloads: { label: string; count: number }[] = [
    {label: "variant-A", count: 1},
    {label: "variant-B", count: 2},
    {label: "variant-C", count: 3},
    {label: "variant-D1", count: 4},
    {label: "variant-D2", count: 5},
    {label: "variant-D3", count: 6},
    {label: "opt-any", count: 7},
    {label: "lst-any-0", count: 8},
];

function readAnyShowcaseJson(source: string): AnyShowcase {
    const file = path.join(compatDir, `${source}-json`, "any-showcase.json");
    const jsonStr = fs.readFileSync(file, "utf-8");
    const json = JSON.parse(jsonStr);
    return AnyShowcase.jsonCodec().decode(BaboonCodecContext.Default, json);
}

function readAnyShowcaseUeba(source: string): AnyShowcase {
    const file = path.join(compatDir, `${source}-ueba`, "any-showcase.ueba");
    const bytes = new Uint8Array(fs.readFileSync(file));
    return AnyShowcase.binCodec().decode(BaboonCodecContext.Default, new BaboonBinReader(bytes));
}

function decodeInner(o: AnyOpaque): InnerPayload {
    if (o.tag === "Ueba") {
        return InnerPayload.binCodec().decode(BaboonCodecContext.Compact, new BaboonBinReader(o.bytes));
    }
    return InnerPayload.jsonCodec().decode(BaboonCodecContext.Compact, o.json);
}

function decodeAllPayloads(v: AnyShowcase): InnerPayload[] {
    if (v.optAny === undefined || v.optAny === null) throw new Error("optAny was empty");
    if (v.lstAny.length === 0) throw new Error("lstAny was empty");
    return [
        decodeInner(v.vAnyA),
        decodeInner(v.vAnyB),
        decodeInner(v.vAnyC),
        decodeInner(v.vAnyD1),
        decodeInner(v.vAnyD2),
        decodeInner(v.vAnyD3),
        decodeInner(v.optAny),
        decodeInner(v.lstAny[0]),
    ];
}

function assertAnyShowcase(source: string, fmt: string, v: AnyShowcase): void {
    const decoded = decodeAllPayloads(v);
    expect(decoded.length).toBe(expectedAnyPayloads.length);
    for (let i = 0; i < expectedAnyPayloads.length; i++) {
        expect(decoded[i].label, `${source} ${fmt} payload ${i} label mismatch`).toBe(expectedAnyPayloads[i].label);
        expect(decoded[i].count, `${source} ${fmt} payload ${i} count mismatch`).toBe(expectedAnyPayloads[i].count);
    }
}

describe("AnyShowcase cross-language compatibility", () => {
    for (const lang of languages) {
        const jsonFile = path.join(compatDir, `${lang}-json`, "any-showcase.json");
        const uebaFile = path.join(compatDir, `${lang}-ueba`, "any-showcase.ueba");

        test(`decode ${lang} any-showcase JSON`, () => {
            if (!fs.existsSync(jsonFile)) {
                console.warn(`Skipping ${lang} any-showcase JSON: file not found`);
                return;
            }
            assertAnyShowcase(lang, "JSON", readAnyShowcaseJson(lang));
        });

        test(`decode ${lang} any-showcase UEBA`, () => {
            if (!fs.existsSync(uebaFile)) {
                console.warn(`Skipping ${lang} any-showcase UEBA: file not found`);
                return;
            }
            assertAnyShowcase(lang, "UEBA", readAnyShowcaseUeba(lang));
        });
    }

    test("any-showcase UEBA bytes byte-identical: typescript vs scala", () => {
        const ts = fs.readFileSync(path.join(compatDir, "typescript-ueba", "any-showcase.ueba"));
        const scala = fs.readFileSync(path.join(compatDir, "scala-ueba", "any-showcase.ueba"));
        expect(new Uint8Array(ts)).toEqual(new Uint8Array(scala));
    });
});
