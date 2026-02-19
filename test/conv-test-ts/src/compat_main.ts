import * as fs from "fs";
import * as path from "path";
import { fileURLToPath } from "url";

import { AllBasicTypes } from "./generated/convtest/testpkg/all-basic-types.js";
import { encode_AllBasicTypes_json, decode_AllBasicTypes_json } from "./generated/convtest/testpkg/all-basic-types.js";
import { encode_AllBasicTypes_ueba, decode_AllBasicTypes_ueba } from "./generated/convtest/testpkg/all-basic-types.js";
import { BaboonDecimal, BaboonDateTimeUtc, BaboonDateTimeOffset, BaboonCodecContext, BaboonBinWriter, BaboonBinReader } from "./generated/baboon_runtime.js";

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

function writeJson(data: AllBasicTypes, outputDir: string): void {
    fs.mkdirSync(outputDir, { recursive: true });
    const json = JSON.stringify(encode_AllBasicTypes_json(data), null, 2);
    const jsonPath = path.join(outputDir, "all-basic-types.json");
    fs.writeFileSync(jsonPath, json);
    console.log(`Written JSON to ${jsonPath}`);
}

function writeUeba(data: AllBasicTypes, outputDir: string): void {
    fs.mkdirSync(outputDir, { recursive: true });
    const ctx = BaboonCodecContext.Default;
    const writer = new BaboonBinWriter();
    encode_AllBasicTypes_ueba(data, ctx, writer);
    const uebaPath = path.join(outputDir, "all-basic-types.ueba");
    fs.writeFileSync(uebaPath, writer.toBytes());
    console.log(`Written UEBA to ${uebaPath}`);
}

function readAndVerify(filePath: string): void {
    const ctx = BaboonCodecContext.Default;
    let data: AllBasicTypes;

    try {
        if (filePath.endsWith(".json")) {
            const jsonStr = fs.readFileSync(filePath, "utf-8");
            const json = JSON.parse(jsonStr);
            data = decode_AllBasicTypes_json(json);
        } else if (filePath.endsWith(".ueba")) {
            const bytes = fs.readFileSync(filePath);
            const reader = new BaboonBinReader(new Uint8Array(bytes));
            data = decode_AllBasicTypes_ueba(ctx, reader);
        } else {
            console.error(`Unknown file extension: ${filePath}`);
            process.exit(1);
        }
    } catch (e) {
        console.error(`Deserialization failed: ${e}`);
        process.exit(1);
    }

    if (data.vstr !== "Hello, Baboon!") {
        console.error(`vstr mismatch: expected 'Hello, Baboon!', got '${data.vstr}'`);
        process.exit(1);
    }
    if (data.vi32 !== 123456) {
        console.error(`vi32 mismatch: expected 123456, got ${data.vi32}`);
        process.exit(1);
    }
    if (!data.vbit) {
        console.error(`vbit mismatch: expected true, got ${data.vbit}`);
        process.exit(1);
    }

    // Roundtrip
    try {
        if (filePath.endsWith(".json")) {
            const reEncoded = encode_AllBasicTypes_json(data);
            const reDecoded = decode_AllBasicTypes_json(reEncoded);
            if (reDecoded.vstr !== data.vstr || reDecoded.vi32 !== data.vi32 || reDecoded.vbit !== data.vbit) {
                console.error("JSON roundtrip mismatch");
                process.exit(1);
            }
        } else {
            const writer = new BaboonBinWriter();
            encode_AllBasicTypes_ueba(data, ctx, writer);
            const reReader = new BaboonBinReader(writer.toBytes());
            const reDecoded = decode_AllBasicTypes_ueba(ctx, reReader);
            if (reDecoded.vstr !== data.vstr || reDecoded.vi32 !== data.vi32 || reDecoded.vbit !== data.vbit) {
                console.error("UEBA roundtrip mismatch");
                process.exit(1);
            }
        }
    } catch (e) {
        console.error(`Roundtrip failed: ${e}`);
        process.exit(1);
    }

    console.log("OK");
}

function runLegacy(): void {
    const sampleData = createSampleData();

    const baseDir = path.resolve(__dirname, "../../../target/compat-test");
    writeJson(sampleData, path.join(baseDir, "typescript-json"));
    writeUeba(sampleData, path.join(baseDir, "typescript-ueba"));

    console.log("TypeScript serialization complete!");
}

const args = process.argv.slice(2);
if (args[0] === "write") {
    const outputDir = args[1];
    const format = args[2];
    const sampleData = createSampleData();
    if (format === "json") {
        writeJson(sampleData, outputDir);
    } else if (format === "ueba") {
        writeUeba(sampleData, outputDir);
    } else {
        console.error(`Unknown format: ${format}`);
        process.exit(1);
    }
} else if (args[0] === "read") {
    readAndVerify(args[1]);
} else {
    runLegacy();
}
