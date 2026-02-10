import * as fs from "fs";
import * as path from "path";
import { fileURLToPath } from "url";

import { AllBasicTypes } from "./generated/convtest/testpkg/all-basic-types.js";
import { encode_AllBasicTypes_json } from "./generated/convtest/testpkg/all-basic-types.js";
import { encode_AllBasicTypes_ueba } from "./generated/convtest/testpkg/all-basic-types.js";
import { BaboonDecimal, BaboonDateTimeUtc, BaboonDateTimeOffset, BaboonCodecContext, BaboonBinWriter } from "./generated/baboon_runtime.js";

const __dirname = path.dirname(fileURLToPath(import.meta.url));

function createSampleData(): AllBasicTypes {
    return {
        vi8: 42,
        vi16: 1234,
        vi32: 123456,
        vi64: 123456789n,
        vu8: 200,
        vu16: 50000,
        vu32: 3000000000,
        vu64: 10000000000n,
        vf32: 3.14159,
        vf64: 2.718281828,
        vf128: BaboonDecimal.fromString("123456789.987654321"),
        vstr: "Hello, Baboon!",
        vbstr: new Uint8Array([0x48, 0x65, 0x6C, 0x6C, 0x6F, 0x20, 0x42, 0x79, 0x74, 0x65, 0x73]),
        vuid: "12345678-1234-5678-1234-567812345678",
        vbit: true,
        vtsu: BaboonDateTimeUtc.fromISO("2024-06-15T12:30:45.123Z"),
        vtso: BaboonDateTimeOffset.fromISO("2024-06-15T14:30:45.987+02:00"),
        voptStr: "optional value",
        vlstI32: [1, 2, 3, 4, 5],
        vsetStr: new Set(["apple", "banana", "cherry"]),
        vmapStrI32: new Map([["one", 1], ["two", 2], ["three", 3]]),
        voptLst: ["nested", "list", "values"],
        vlstOpt: [10, undefined, 20, 30],
        vmapLst: new Map([
            ["numbers", [1n, 2n, 3n]],
            ["more", [4n, 5n, 6n]],
        ]),
    };
}

function main(): void {
    const sampleData = createSampleData();
    const ctx = BaboonCodecContext.Default;

    const baseDir = path.resolve(__dirname, "../../target/compat-test");
    const jsonDir = path.join(baseDir, "typescript-json");
    const uebaDir = path.join(baseDir, "typescript-ueba");

    fs.mkdirSync(jsonDir, { recursive: true });
    fs.mkdirSync(uebaDir, { recursive: true });

    const json = JSON.stringify(encode_AllBasicTypes_json(sampleData), null, 2);
    const jsonPath = path.join(jsonDir, "all-basic-types.json");
    fs.writeFileSync(jsonPath, json);
    console.log(`Written JSON to ${jsonPath}`);

    const writer = new BaboonBinWriter();
    encode_AllBasicTypes_ueba(sampleData, ctx, writer);
    const uebaPath = path.join(uebaDir, "all-basic-types.ueba");
    fs.writeFileSync(uebaPath, writer.toBytes());
    console.log(`Written UEBA to ${uebaPath}`);

    console.log("TypeScript serialization complete!");
}

main();
