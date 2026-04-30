import * as fs from "fs";
import * as path from "path";
import {fileURLToPath} from "url";
import {AllBasicTypes} from "./generated/convtest/testpkg/AllBasicTypes";
import {AnyShowcase} from "./generated/convtest/testpkg/AnyShowcase";
import {InnerPayload} from "./generated/convtest/testpkg/InnerPayload";
import {WireEnum} from "./generated/convtest/testpkg/WireEnum";
import {PointId} from "./generated/convtest/testpkg/PointId";
import {
    AbstractBaboonJsonCodecs,
    AbstractBaboonUebaCodecs,
    BaboonBinReader,
    BaboonBinWriter,
    BaboonCodecContext,
    BaboonDateTimeOffset,
    BaboonDateTimeUtc,
    BaboonDecimal,
    BaboonDomainVersion,
    Lazy,
} from "./generated/BaboonSharedRuntime";
import {BaboonCodecsFacade} from "./generated/BaboonCodecsFacade";
import {AnyMeta, AnyOpaque, anyOpaqueJson, anyOpaqueUeba, createAnyMeta} from "./generated/BaboonAnyOpaque";

const __dirname = path.dirname(fileURLToPath(import.meta.url));

const DOMAIN_ID = "convtest.testpkg";
const DOMAIN_VER = "2.0.0";
const INNER_TYPE_ID = "convtest.testpkg/:#InnerPayload";

class BaboonCodecsJson extends AbstractBaboonJsonCodecs {
    constructor() {
        super();
        this.register("convtest.testpkg/:#AllBasicTypes", new Lazy(() => AllBasicTypes.jsonCodec()));
        this.register("convtest.testpkg/:#InnerPayload", new Lazy(() => InnerPayload.jsonCodec()));
        this.register("convtest.testpkg/:#AnyShowcase", new Lazy(() => AnyShowcase.jsonCodec()));
    }
}

class BaboonCodecsUeba extends AbstractBaboonUebaCodecs {
    constructor() {
        super();
        this.register("convtest.testpkg/:#AllBasicTypes", new Lazy(() => AllBasicTypes.binCodec()));
        this.register("convtest.testpkg/:#InnerPayload", new Lazy(() => InnerPayload.binCodec()));
        this.register("convtest.testpkg/:#AnyShowcase", new Lazy(() => AnyShowcase.binCodec()));
    }
}

function freshFacade(): BaboonCodecsFacade {
    const f = new BaboonCodecsFacade();
    f.registerCodecs(
        new BaboonDomainVersion(DOMAIN_ID, DOMAIN_VER),
        () => new BaboonCodecsJson(),
        () => new BaboonCodecsUeba(),
    );
    return f;
}

function expectedInnerPayloads(): InnerPayload[] {
    return [
        new InnerPayload("variant-A", 1),
        new InnerPayload("variant-B", 2),
        new InnerPayload("variant-C", 3),
        new InnerPayload("variant-D1", 4),
        new InnerPayload("variant-D2", 5),
        new InnerPayload("variant-D3", 6),
        new InnerPayload("opt-any", 7),
        new InnerPayload("lst-any-0", 8),
    ];
}

function uebaBytes(p: InnerPayload): Uint8Array {
    const w = new BaboonBinWriter();
    InnerPayload.binCodec().encode(BaboonCodecContext.Compact, p, w);
    return w.toBytes();
}

function asJson(p: InnerPayload): unknown {
    return InnerPayload.jsonCodec().encode(BaboonCodecContext.Compact, p);
}

function createSampleAnyShowcase(): AnyShowcase {
    const payloads = expectedInnerPayloads();
    const [a, b, c, d1, d2, d3, optP, lstP] = payloads;

    const metaA: AnyMeta  = createAnyMeta(0x07, DOMAIN_ID, DOMAIN_VER, INNER_TYPE_ID);
    const metaB: AnyMeta  = createAnyMeta(0x03, null, DOMAIN_VER, INNER_TYPE_ID);
    const metaC: AnyMeta  = createAnyMeta(0x01, null, null, INNER_TYPE_ID);
    const metaD1: AnyMeta = createAnyMeta(0x06, DOMAIN_ID, DOMAIN_VER, null);
    const metaD2: AnyMeta = createAnyMeta(0x02, null, DOMAIN_VER, null);
    const metaD3: AnyMeta = createAnyMeta(0x00, null, null, null);
    const metaOpt: AnyMeta = createAnyMeta(0x07, DOMAIN_ID, DOMAIN_VER, INNER_TYPE_ID);
    const metaLst: AnyMeta = createAnyMeta(0x06, DOMAIN_ID, DOMAIN_VER, null);

    return new AnyShowcase(
        anyOpaqueJson(metaA, asJson(a)),
        anyOpaqueJson(metaB, asJson(b)),
        anyOpaqueJson(metaC, asJson(c)),
        anyOpaqueUeba(metaD1, uebaBytes(d1)),
        anyOpaqueUeba(metaD2, uebaBytes(d2)),
        anyOpaqueUeba(metaD3, uebaBytes(d3)),
        anyOpaqueJson(metaOpt, asJson(optP)),
        [anyOpaqueUeba(metaLst, uebaBytes(lstP))],
    );
}

function writeJsonAny(ctx: BaboonCodecContext, data: AnyShowcase, outputDir: string): void {
    fs.mkdirSync(outputDir, {recursive: true});
    const json = JSON.stringify(AnyShowcase.jsonCodec().encode(ctx, data), null, 2);
    const p = path.join(outputDir, "any-showcase.json");
    fs.writeFileSync(p, json);
    console.log(`Written JSON to ${p}`);
}

function writeUebaAny(ctx: BaboonCodecContext, data: AnyShowcase, outputDir: string): void {
    fs.mkdirSync(outputDir, {recursive: true});
    const w = new BaboonBinWriter();
    AnyShowcase.binCodec().encode(ctx, data, w);
    const p = path.join(outputDir, "any-showcase.ueba");
    fs.writeFileSync(p, w.toBytes());
    console.log(`Written UEBA to ${p}`);
}

function decodeInner(o: AnyOpaque): InnerPayload {
    if (o.tag === "Ueba") {
        return InnerPayload.binCodec().decode(BaboonCodecContext.Compact, new BaboonBinReader(o.bytes));
    }
    return InnerPayload.jsonCodec().decode(BaboonCodecContext.Compact, o.json);
}

function decodeAllPayloads(v: AnyShowcase): InnerPayload[] {
    if (v.optAny === undefined || v.optAny === null) throw new Error("optAny was empty; expected non-null");
    if (v.lstAny.length === 0) throw new Error("lstAny was empty; expected one element");
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

function readAndVerifyAnyShowcase(filePath: string): void {
    const ctx = BaboonCodecContext.Default;
    let data: AnyShowcase;
    try {
        if (filePath.endsWith(".json")) {
            const jsonStr = fs.readFileSync(filePath, "utf-8");
            const json = JSON.parse(jsonStr);
            data = AnyShowcase.jsonCodec().decode(ctx, json);
        } else {
            const bytes = fs.readFileSync(filePath);
            const r = new BaboonBinReader(new Uint8Array(bytes));
            data = AnyShowcase.binCodec().decode(ctx, r);
        }
    } catch (e) {
        console.error(`AnyShowcase deserialization failed: ${e}`);
        process.exit(1);
    }
    try {
        const expected = expectedInnerPayloads();
        const decoded = decodeAllPayloads(data!);
        for (let i = 0; i < expected.length; i++) {
            if (expected[i].label !== decoded[i].label || expected[i].count !== decoded[i].count) {
                console.error(`AnyShowcase payload ${i} mismatch: expected (${expected[i].label}, ${expected[i].count}), got (${decoded[i].label}, ${decoded[i].count})`);
                process.exit(1);
            }
        }
    } catch (e) {
        console.error(`AnyShowcase decode failed: ${e}`);
        process.exit(1);
    }
    console.log("OK");
}

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
        // Non-Pascal-case enum member; canonical JSON wire form is "Cafe" (PR-35-D06 regression guard).
        WireEnum.Cafe,
        // Identifier (PR-57e). Wire form is `{"x": 42, "y": -7}` on JSON and two
        // i32 LE values on UEBA — byte-identical to a `data` of the same shape
        // per docs/spec/identifier-repr.md §1.3 / §7.
        new PointId(42, -7),
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
    if (filePath.endsWith("any-showcase.json") || filePath.endsWith("any-showcase.ueba")) {
        readAndVerifyAnyShowcase(filePath);
        return;
    }
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

// PR-57e (M18.4e) — cross-language identifier repr (toString) byte-identity.
// Per spec §7 the repr/toString form is a separate invariant from the JSON/UEBA wire bytes;
// we write it as a per-language artifact so the Scala-side test can assert all 10 backends
// produce byte-identical output for the same canonical PointId value.
function writePointIdRepr(pid: PointId, outputDir: string): void {
    fs.mkdirSync(outputDir, {recursive: true});
    const reprPath = path.join(outputDir, "point-id.txt");
    // No trailing newline — exact byte match across all languages.
    fs.writeFileSync(reprPath, pid.toString(), {encoding: "utf-8"});
    console.log(`Written repr to ${reprPath}`);
}

function runLegacy(): void {
    const sampleData = createSampleData();
    const sampleAny = createSampleAnyShowcase();
    const facadeCtx = BaboonCodecContext.withFacade(false, freshFacade());

    const baseDir = path.resolve(__dirname, "../../../target/compat-test");
    const tsJsonDir = path.join(baseDir, "typescript-json");
    const tsUebaDir = path.join(baseDir, "typescript-ueba");
    const tsReprDir = path.join(baseDir, "typescript-repr");
    writeJson(sampleData, tsJsonDir);
    writeUeba(sampleData, tsUebaDir);
    writeJsonAny(facadeCtx, sampleAny, tsJsonDir);
    writeUebaAny(facadeCtx, sampleAny, tsUebaDir);
    writePointIdRepr(sampleData.vPointId, tsReprDir);

    console.log("TypeScript serialization complete!");
}

const args = process.argv.slice(2);
if (args[0] === "write") {
    const outputDir = args[1];
    const format = args[2];
    const sampleData = createSampleData();
    const sampleAny = createSampleAnyShowcase();
    const facadeCtx = BaboonCodecContext.withFacade(false, freshFacade());
    if (format === "json") {
        writeJson(sampleData, outputDir);
        writeJsonAny(facadeCtx, sampleAny, outputDir);
    } else if (format === "ueba") {
        writeUeba(sampleData, outputDir);
        writeUebaAny(facadeCtx, sampleAny, outputDir);
    } else {
        console.error(`Unknown format: ${format}`);
        process.exit(1);
    }
} else if (args[0] === "read") {
    readAndVerify(args[1]);
} else {
    runLegacy();
}
