import * as fs from "fs";
import * as path from "path";
import {fileURLToPath} from "url";
import {AllBasicTypes} from "./generated/convtest/testpkg/AllBasicTypes";
import {AnyShowcase} from "./generated/convtest/testpkg/AnyShowcase";
import {InnerPayload} from "./generated/convtest/testpkg/InnerPayload";
import {WireEnum} from "./generated/convtest/testpkg/WireEnum";
import {PointId} from "./generated/convtest/testpkg/PointId";
import {ItemId} from "./generated/convtest/testpkg/ItemId";
import {CompositeId} from "./generated/convtest/testpkg/CompositeId";
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
// PR-I.1d (M24 Phase 3.1) — Custom-foreign KeyCodec hook fixture. Stringy
// FStr foreign + ItemKey wrapper + ForeignKeyHolder round-trip exercises the
// generated FStr_KeyCodecHost identity default impl.
import {ForeignKeyHolder} from "./generated/convtest/m24foreign/ForeignKeyHolder";
import {ItemKey} from "./generated/convtest/m24foreign/ItemKey";
// PR-26.5 (M26) — non-string builtin map-key cross-language fixture.
import {BuiltinMapKeyHolder} from "./generated/convtest/m26builtinkeys/BuiltinMapKeyHolder";
// PR-29.10 (M29) — monomorphised template cross-language acceptance fixture.
import {M29OkHolder} from "./generated/convtest/m29ok/M29OkHolder";
import {IntPage} from "./generated/convtest/m29ok/IntPage";
import {StrPage} from "./generated/convtest/m29ok/StrPage";
import {Item} from "./generated/convtest/m29ok/Item";
import {ItemPage} from "./generated/convtest/m29ok/ItemPage";
import {Ok as EnvelopeOk, Err as EnvelopeErr} from "./generated/convtest/m29ok/IntStrEnvelope";
// PR-33.5 (M33) — structural-inheritance-via-template cross-language acceptance fixture.
import {M33OkHolder} from "./generated/convtest/m33ok/M33OkHolder";
import {IntPageWithStats} from "./generated/convtest/m33ok/IntPageWithStats";
import {PageMinusStats} from "./generated/convtest/m33ok/PageMinusStats";
import {PageOnly} from "./generated/convtest/m33ok/PageOnly";

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
        // PR-61 (M19.3) — id types as JSON map keys. Per PR-60 (M19.2) all id
        // types — single- or multi-field — use canonical repr toString as the
        // key form: e.g. `ItemId:2.0.0#v:00000000-0000-0000-0000-000000000001`.
        // Canonical deterministic uuids ensure cross-language byte-identity.
        new Map<ItemId, number>([
            [new ItemId("00000000-0000-0000-0000-000000000001"), 1],
            [new ItemId("00000000-0000-0000-0000-000000000002"), 2],
        ]),
        new Map<CompositeId, number>([
            [new CompositeId(
                "00000000-0000-0000-0000-0000000000aa",
                "00000000-0000-0000-0000-0000000000bb",
            ), 100],
            [new CompositeId(
                "00000000-0000-0000-0000-0000000000cc",
                "00000000-0000-0000-0000-0000000000dd",
            ), 200],
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
    if (filePath.endsWith("any-showcase.json") || filePath.endsWith("any-showcase.ueba")) {
        readAndVerifyAnyShowcase(filePath);
        return;
    }
    if (filePath.endsWith("m33-ok.json") || filePath.endsWith("m33-ok.ueba")) {
        readAndVerifyM33Ok(filePath);
        return;
    }
    if (filePath.endsWith("m29-ok.json") || filePath.endsWith("m29-ok.ueba")) {
        readAndVerifyM29Ok(filePath);
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

// PR-I.1d (M24 Phase 3.1) — Custom-foreign KeyCodec hook canonical fixture.
// Map keys go through FStr_KeyCodecHost (default identity impl for the stringy
// foreign), so the wire form is `{"m":{"alpha":"v1","beta":"v2"}}`.
function createForeignKeyHolderSample(): ForeignKeyHolder {
    return new ForeignKeyHolder(new Map<ItemKey, string>([
        [new ItemKey("alpha"), "v1"],
        [new ItemKey("beta"), "v2"],
    ]));
}

function writeForeignKeyHolderJson(ctx: BaboonCodecContext, data: ForeignKeyHolder, outputDir: string): void {
    fs.mkdirSync(outputDir, {recursive: true});
    const json = ForeignKeyHolder.jsonCodec().encode(ctx, data);
    // Compact (no indent) so byte-identity matches the canonical wire form
    // `{"m":{"alpha":"v1","beta":"v2"}}` across backends.
    const jsonStr = JSON.stringify(json);
    const p = path.join(outputDir, "m24-foreign-keycodec.json");
    fs.writeFileSync(p, jsonStr);
    console.log(`Written JSON to ${p}`);
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
    writeForeignKeyHolderJson(BaboonCodecContext.Default, createForeignKeyHolderSample(), tsJsonDir);
    writeBuiltinMapKeyHolderJson(BaboonCodecContext.Default, createBuiltinMapKeyHolderSample(), tsJsonDir);
    writeBuiltinMapKeyHolderUeba(BaboonCodecContext.Default, createBuiltinMapKeyHolderSample(), tsUebaDir);

    console.log("TypeScript serialization complete!");
}

// PR-26.5 (M26) — non-string builtin map-key cross-language fixture.
// TS u32 = number; i64 = bigint (per ts-stub generator). Per PR-G the wire
// form for non-string builtin map keys is `{<String(k)>: <v>}`.
function createBuiltinMapKeyHolderSample(): BuiltinMapKeyHolder {
    return new BuiltinMapKeyHolder(
        new Map<number, string>([[42, "v32"]]),
        new Map<bigint, string>([[9223372036854775807n, "vmax"]]),
        new Map<number, string>([[7, "vu32"]]),
        // PR-28.4 (M28): u64 max exercises canonical unsigned wire form.
        new Map<bigint, string>([[18446744073709551615n, "vu64max"]]),
        new Map<boolean, string>([[true, "vt"]]),
        new Map<string, string>([["00000000-0000-0000-0000-000000000001", "vid"]]),
        // PR-28.4 (M28): non-UTC tso offset (PR-28.3 ±HH:MM).
        new Map<BaboonDateTimeOffset, string>([
            [BaboonDateTimeOffset.fromISO("2026-05-02T12:00:00.123+05:30"), "vtso_ist"],
        ]),
    );
}

function writeBuiltinMapKeyHolderJson(ctx: BaboonCodecContext, data: BuiltinMapKeyHolder, outputDir: string): void {
    fs.mkdirSync(outputDir, {recursive: true});
    const json = BuiltinMapKeyHolder.jsonCodec().encode(ctx, data);
    const jsonStr = JSON.stringify(json);
    const p = path.join(outputDir, "m26-builtin-map-keys.json");
    fs.writeFileSync(p, jsonStr);
    console.log(`Written JSON to ${p}`);
}

function writeBuiltinMapKeyHolderUeba(ctx: BaboonCodecContext, data: BuiltinMapKeyHolder, outputDir: string): void {
    fs.mkdirSync(outputDir, {recursive: true});
    const writer = new BaboonBinWriter();
    BuiltinMapKeyHolder.binCodec().encode(ctx, data, writer);
    const p = path.join(outputDir, "m26-builtin-map-keys.ueba");
    fs.writeFileSync(p, writer.toBytes());
    console.log(`Written UEBA to ${p}`);
}

// PR-29.10 (M29) — monomorphised template acceptance fixture helpers.
function createM29OkSample(): M29OkHolder {
    return new M29OkHolder(
        new IntPage([1, 2, 3], 3),
        new StrPage(["hello", "world"], 2),
        new ItemPage([new Item("Widget", 9.99)], 1),
        new EnvelopeOk(42),
        new EnvelopeErr("oops"),
    );
}

function writeM29OkJson(ctx: BaboonCodecContext, data: M29OkHolder, outputDir: string): void {
    fs.mkdirSync(outputDir, {recursive: true});
    const json = JSON.stringify(M29OkHolder.jsonCodec().encode(ctx, data));
    const p = path.join(outputDir, "m29-ok.json");
    fs.writeFileSync(p, json);
    console.log(`Written JSON to ${p}`);
}

function writeM29OkUeba(ctx: BaboonCodecContext, data: M29OkHolder, outputDir: string): void {
    fs.mkdirSync(outputDir, {recursive: true});
    const w = new BaboonBinWriter();
    M29OkHolder.binCodec().encode(ctx, data, w);
    const p = path.join(outputDir, "m29-ok.ueba");
    fs.writeFileSync(p, w.toBytes());
    console.log(`Written UEBA to ${p}`);
}

function readAndVerifyM29Ok(filePath: string): void {
    const ctx = BaboonCodecContext.Default;
    let data: M29OkHolder;
    try {
        if (filePath.endsWith(".json")) {
            const jsonStr = fs.readFileSync(filePath, "utf-8");
            const json = JSON.parse(jsonStr);
            data = M29OkHolder.jsonCodec().decode(ctx, json);
        } else {
            const bytes = fs.readFileSync(filePath);
            const r = new BaboonBinReader(new Uint8Array(bytes));
            data = M29OkHolder.binCodec().decode(ctx, r);
        }
    } catch (e) {
        console.error(`M29OkHolder deserialization failed: ${e}`);
        process.exit(1);
    }
    // Roundtrip
    try {
        if (filePath.endsWith(".json")) {
            const reEncoded = M29OkHolder.jsonCodec().encode(ctx, data!);
            const reDecoded = M29OkHolder.jsonCodec().decode(ctx, reEncoded);
            // Structural equality check via JSON serialization
            if (JSON.stringify(reEncoded) !== JSON.stringify(M29OkHolder.jsonCodec().encode(ctx, reDecoded))) {
                console.error("M29OkHolder JSON roundtrip mismatch");
                process.exit(1);
            }
        } else {
            const w = new BaboonBinWriter();
            M29OkHolder.binCodec().encode(ctx, data!, w);
            const r = new BaboonBinReader(w.toBytes());
            const reDecoded = M29OkHolder.binCodec().decode(ctx, r);
            const w2 = new BaboonBinWriter();
            M29OkHolder.binCodec().encode(ctx, reDecoded, w2);
            if (w.toBytes().join(",") !== w2.toBytes().join(",")) {
                console.error("M29OkHolder UEBA roundtrip mismatch");
                process.exit(1);
            }
        }
    } catch (e) {
        console.error(`M29OkHolder roundtrip failed: ${e}`);
        process.exit(1);
    }
    console.log("OK");
}

// PR-33.5 (M33) — structural-inheritance-via-template acceptance fixture helpers.
function createM33OkSample(): M33OkHolder {
    // PR-33.5-D02 — pairwise-distinct values: total=42 (was 3),
    // nObservations=7 (was 3), so a swapped-field defect surfaces.
    // PR-33.5-D01 — `-` operator coverage (PageMinusStats: items + total
    // survive after Stats subtraction); `^` operator coverage (PageOnly:
    // items + total survive after intersection with Page[i32]).
    return new M33OkHolder(
        new IntPageWithStats([10, 20, 30], 42, 60, 7),
        new PageMinusStats([100, 200], 99),
        new PageOnly([1, 2, 3, 4, 5], 5),
    );
}

function writeM33OkJson(ctx: BaboonCodecContext, data: M33OkHolder, outputDir: string): void {
    fs.mkdirSync(outputDir, {recursive: true});
    const json = JSON.stringify(M33OkHolder.jsonCodec().encode(ctx, data));
    const p = path.join(outputDir, "m33-ok.json");
    fs.writeFileSync(p, json);
    console.log(`Written JSON to ${p}`);
}

function writeM33OkUeba(ctx: BaboonCodecContext, data: M33OkHolder, outputDir: string): void {
    fs.mkdirSync(outputDir, {recursive: true});
    const w = new BaboonBinWriter();
    M33OkHolder.binCodec().encode(ctx, data, w);
    const p = path.join(outputDir, "m33-ok.ueba");
    fs.writeFileSync(p, w.toBytes());
    console.log(`Written UEBA to ${p}`);
}

function readAndVerifyM33Ok(filePath: string): void {
    const ctx = BaboonCodecContext.Default;
    let data: M33OkHolder;
    try {
        if (filePath.endsWith(".json")) {
            const jsonStr = fs.readFileSync(filePath, "utf-8");
            const json = JSON.parse(jsonStr);
            data = M33OkHolder.jsonCodec().decode(ctx, json);
        } else {
            const bytes = fs.readFileSync(filePath);
            const r = new BaboonBinReader(new Uint8Array(bytes));
            data = M33OkHolder.binCodec().decode(ctx, r);
        }
    } catch (e) {
        console.error(`M33OkHolder deserialization failed: ${e}`);
        process.exit(1);
    }
    // Roundtrip
    try {
        if (filePath.endsWith(".json")) {
            const reEncoded = M33OkHolder.jsonCodec().encode(ctx, data!);
            const reDecoded = M33OkHolder.jsonCodec().decode(ctx, reEncoded);
            if (JSON.stringify(reEncoded) !== JSON.stringify(M33OkHolder.jsonCodec().encode(ctx, reDecoded))) {
                console.error("M33OkHolder JSON roundtrip mismatch");
                process.exit(1);
            }
        } else {
            const w = new BaboonBinWriter();
            M33OkHolder.binCodec().encode(ctx, data!, w);
            const r = new BaboonBinReader(w.toBytes());
            const reDecoded = M33OkHolder.binCodec().decode(ctx, r);
            const w2 = new BaboonBinWriter();
            M33OkHolder.binCodec().encode(ctx, reDecoded, w2);
            if (w.toBytes().join(",") !== w2.toBytes().join(",")) {
                console.error("M33OkHolder UEBA roundtrip mismatch");
                process.exit(1);
            }
        }
    } catch (e) {
        console.error(`M33OkHolder roundtrip failed: ${e}`);
        process.exit(1);
    }
    console.log("OK");
}

const args = process.argv.slice(2);
if (args[0] === "write") {
    const outputDir = args[1];
    const format = args[2];
    const sampleData = createSampleData();
    const sampleAny = createSampleAnyShowcase();
    const facadeCtx = BaboonCodecContext.withFacade(false, freshFacade());
    const m29Sample = createM29OkSample();
    const m33Sample = createM33OkSample();
    if (format === "json") {
        writeJson(sampleData, outputDir);
        writeJsonAny(facadeCtx, sampleAny, outputDir);
        writeForeignKeyHolderJson(BaboonCodecContext.Default, createForeignKeyHolderSample(), outputDir);
        writeM29OkJson(BaboonCodecContext.Default, m29Sample, outputDir);
        writeM33OkJson(BaboonCodecContext.Default, m33Sample, outputDir);
    } else if (format === "ueba") {
        writeUeba(sampleData, outputDir);
        writeUebaAny(facadeCtx, sampleAny, outputDir);
        writeM29OkUeba(BaboonCodecContext.Default, m29Sample, outputDir);
        writeM33OkUeba(BaboonCodecContext.Default, m33Sample, outputDir);
    } else {
        console.error(`Unknown format: ${format}`);
        process.exit(1);
    }
} else if (args[0] === "read") {
    readAndVerify(args[1]);
} else {
    runLegacy();
}
