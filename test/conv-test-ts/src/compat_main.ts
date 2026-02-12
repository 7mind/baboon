import * as fs from "fs";
import * as path from "path";
import {fileURLToPath} from "url";
import {AllBasicTypes} from "./generated/convtest/testpkg/AllBasicTypes";
import {
    BaboonBinReader,
    BaboonBinWriter,
    BaboonCodecContext,
    BaboonDateTimeOffset,
    BaboonDateTimeUtc,
    BaboonDecimal
} from "./generated/BaboonSharedRuntime";

const __dirname = path.dirname(fileURLToPath(import.meta.url));

function createSampleData(): AllBasicTypes {
    return new AllBasicTypes(
        42,
        1234,
        123456,
        123456789n,
        200,
        50000,
        3000000000,
        10000000000n,
        3.14159,
        2.718281828,
        BaboonDecimal.fromString("123456789.987654321"),
        "Hello, Baboon!",
        new Uint8Array([0x48, 0x65, 0x6C, 0x6C, 0x6F, 0x20, 0x42, 0x79, 0x74, 0x65, 0x73]),
        "12345678-1234-5678-1234-567812345678",
        true,
        BaboonDateTimeUtc.fromISO("2024-06-15T12:30:45.123Z"),
        BaboonDateTimeOffset.fromISO("2024-06-15T14:30:45.987+02:00"),
        "optional value",
        [1, 2, 3, 4, 5],
        new Set(["apple", "banana", "cherry"]),
        new Map([["one", 1], ["two", 2], ["three", 3]]),
        ["nested", "list", "values"],
        [10, undefined, 20, 30],
        new Map([
            ["numbers", [1n, 2n, 3n]],
            ["more", [4n, 5n, 6n]],
        ]),
    );
}

function writeJson(data: AllBasicTypes, outputDir: string): void {
    fs.mkdirSync(outputDir, {recursive: true});
    const json = JSON.stringify(AllBasicTypes.jsonCodec().encode(BaboonCodecContext.Default, data), null, 2);
    const jsonPath = path.join(outputDir, "all-basic-types.json");
    fs.writeFileSync(jsonPath, json);
    console.log(`Written JSON to ${jsonPath}`);
}

function writeUeba(data: AllBasicTypes, outputDir: string): void {
    fs.mkdirSync(outputDir, {recursive: true});
    const ctx = BaboonCodecContext.Default;
    const writer = new BaboonBinWriter();
    AllBasicTypes.binCodec().encode(ctx, data, writer);
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
            data = AllBasicTypes.jsonCodec().decode(BaboonCodecContext.Default, json);
        } else if (filePath.endsWith(".ueba")) {
            const bytes = fs.readFileSync(filePath);
            const reader = new BaboonBinReader(new Uint8Array(bytes));
            data = AllBasicTypes.binCodec().decode(ctx, reader);
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
            const reEncoded = AllBasicTypes.jsonCodec().encode(BaboonCodecContext.Default, data);
            const reDecoded = AllBasicTypes.jsonCodec().decode(BaboonCodecContext.Default, reEncoded);
            if (reDecoded.vstr !== data.vstr || reDecoded.vi32 !== data.vi32 || reDecoded.vbit !== data.vbit) {
                console.error("JSON roundtrip mismatch");
                process.exit(1);
            }
        } else {
            const writer = new BaboonBinWriter();
            AllBasicTypes.binCodec().encode(ctx, data, writer);
            const reReader = new BaboonBinReader(writer.toBytes());
            const reDecoded = AllBasicTypes.binCodec().decode(ctx, reReader);
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
