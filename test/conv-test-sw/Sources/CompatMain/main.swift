import Foundation
import BaboonRuntime
@testable import ConvtestTestpkg
// PR-I.2 (M24 Phase 3.2) — Custom-foreign KeyCodec hook fixture. Stringy
// FStr foreign + ItemKey wrapper + ForeignKeyHolder round-trip exercises the
// generated FStr_KeyCodecHost identity default impl.
import ConvtestM24foreign
// PR-26.5 (M26) — non-string builtin map-key cross-language fixture.
import ConvtestM26builtinkeys
// PR-29.10 (M29) — monomorphised-template cross-language wire-format fixture.
import ConvtestM29ok

func fail(_ message: String) -> Never {
    fputs(message + "\n", stderr)
    exit(1)
}

func createSampleData() -> AllBasicTypes {
    var calendar = Calendar(identifier: .gregorian)
    calendar.timeZone = TimeZone(secondsFromGMT: 0)!

    var tsuComps = DateComponents()
    tsuComps.year = 2024
    tsuComps.month = 6
    tsuComps.day = 15
    tsuComps.hour = 12
    tsuComps.minute = 30
    tsuComps.second = 45
    tsuComps.nanosecond = 123 * 1_000_000
    let vtsu = calendar.date(from: tsuComps)!

    var tsoComps = DateComponents()
    tsoComps.year = 2024
    tsoComps.month = 6
    tsoComps.day = 15
    tsoComps.hour = 12
    tsoComps.minute = 30
    tsoComps.second = 45
    tsoComps.nanosecond = 987 * 1_000_000
    let tsoUtcDate = calendar.date(from: tsoComps)!

    return AllBasicTypes(
        vi8: 42,
        vi16: 1234,
        vi32: 123456,
        vi64: 123456789,
        vu8: 200,
        vu16: 50000,
        vu32: 3000000000,
        vu64: 10000000000,
        vf32: 3.14159,
        vf64: 2.718281828,
        vf128: BaboonDecimal("123456789.987654321"),
        vstr: "Hello, Baboon!",
        vbstr: Data([0x48, 0x65, 0x6C, 0x6C, 0x6F, 0x20, 0x42, 0x79, 0x74, 0x65, 0x73]),
        vuid: UUID(uuidString: "12345678-1234-5678-1234-567812345678")!,
        vbit: true,
        vtsu: vtsu,
        vtso: BaboonDateTimeOffset(
            epochMillis: Int64(tsoUtcDate.timeIntervalSince1970 * 1000),
            offsetMillis: 7200000,
            kind: "offset"
        ),
        voptStr: "optional value",
        vlstI32: [1, 2, 3, 4, 5],
        vsetStr: Set(["apple", "banana", "cherry"]),
        vmapStrI32: ["one": 1, "two": 2, "three": 3],
        voptLst: ["nested", "list", "values"],
        vlstOpt: [10, nil, 20, 30],
        vmapLst: ["numbers": [1, 2, 3], "more": [4, 5, 6]],
        // Non-Pascal-case enum member; canonical JSON wire form is "Cafe" (PR-35-D06 regression guard).
        vWireEnum: WireEnum.Cafe,
        // Identifier (PR-57e). Wire form is `{"x": 42, "y": -7}` on JSON and two
        // i32 LE values on UEBA — byte-identical to a `data` of the same shape
        // per docs/spec/identifier-repr.md §1.3 / §7.
        vPointId: PointId(x: 42, y: -7),
        // PR-61 (M19.3) — id types as JSON map keys. Per PR-60 (M19.2) all id
        // types — single- or multi-field — use canonical repr description as the
        // key form: e.g. `ItemId:2.0.0#v:00000000-0000-0000-0000-000000000001`.
        // Canonical deterministic uuids ensure cross-language byte-identity.
        vmapItemIdU32: [
            ItemId(v: UUID(uuidString: "00000000-0000-0000-0000-000000000001")!): 1,
            ItemId(v: UUID(uuidString: "00000000-0000-0000-0000-000000000002")!): 2,
        ],
        vmapCompositeIdU32: [
            CompositeId(
                tenant: UUID(uuidString: "00000000-0000-0000-0000-0000000000aa")!,
                user:   UUID(uuidString: "00000000-0000-0000-0000-0000000000bb")!
            ): 100,
            CompositeId(
                tenant: UUID(uuidString: "00000000-0000-0000-0000-0000000000cc")!,
                user:   UUID(uuidString: "00000000-0000-0000-0000-0000000000dd")!
            ): 200,
        ]
    )
}

// PR 13.2 — domain constants for AnyShowcase.
let domainId = "convtest.testpkg"
let domainVer = "2.0.0"
let innerTypeId = "convtest.testpkg/:#InnerPayload"

func freshFacade() -> BaboonCodecsFacade {
    let f = BaboonCodecsFacade()
    f.registerCodecs(
        BaboonDomainVersion(domainId, domainVer),
        codecsJson: { BaboonCodecsJson_Convtest_Testpkg() },
        codecsBin: { BaboonCodecsUeba_Convtest_Testpkg() }
    )
    return f
}

func expectedInnerPayloads() -> [InnerPayload] {
    return [
        InnerPayload(label: "variant-A", count: 1),
        InnerPayload(label: "variant-B", count: 2),
        InnerPayload(label: "variant-C", count: 3),
        InnerPayload(label: "variant-D1", count: 4),
        InnerPayload(label: "variant-D2", count: 5),
        InnerPayload(label: "variant-D3", count: 6),
        InnerPayload(label: "opt-any", count: 7),
        InnerPayload(label: "lst-any-0", count: 8),
    ]
}

func uebaBytes(_ p: InnerPayload) -> Data {
    let w = BaboonBinWriter()
    InnerPayload_UebaCodec.instance.encode(.compact, w, p)
    return w.toData()
}

func asJson(_ p: InnerPayload) -> Any? {
    return InnerPayload_JsonCodec.instance.encode(.compact, p)
}

func createSampleAnyShowcase() throws -> AnyShowcase {
    let payloads = expectedInnerPayloads()
    let metaA  = try AnyMeta(kind: 0x07, domain: domainId, version: domainVer, typeid: innerTypeId)
    let metaB  = try AnyMeta(kind: 0x03, domain: nil, version: domainVer, typeid: innerTypeId)
    let metaC  = try AnyMeta(kind: 0x01, domain: nil, version: nil, typeid: innerTypeId)
    let metaD1 = try AnyMeta(kind: 0x06, domain: domainId, version: domainVer, typeid: nil)
    let metaD2 = try AnyMeta(kind: 0x02, domain: nil, version: domainVer, typeid: nil)
    let metaD3 = try AnyMeta(kind: 0x00, domain: nil, version: nil, typeid: nil)
    let metaOpt = try AnyMeta(kind: 0x07, domain: domainId, version: domainVer, typeid: innerTypeId)
    let metaLst = try AnyMeta(kind: 0x06, domain: domainId, version: domainVer, typeid: nil)

    return AnyShowcase(
        vAnyA: .json(meta: metaA, json: asJson(payloads[0])),
        vAnyB: .json(meta: metaB, json: asJson(payloads[1])),
        vAnyC: .json(meta: metaC, json: asJson(payloads[2])),
        vAnyD1: .ueba(meta: metaD1, bytes: uebaBytes(payloads[3])),
        vAnyD2: .ueba(meta: metaD2, bytes: uebaBytes(payloads[4])),
        vAnyD3: .ueba(meta: metaD3, bytes: uebaBytes(payloads[5])),
        optAny: .json(meta: metaOpt, json: asJson(payloads[6])),
        lstAny: [.ueba(meta: metaLst, bytes: uebaBytes(payloads[7]))]
    )
}

func writeJsonAny(_ ctx: BaboonCodecContext, _ data: AnyShowcase, _ outputDir: String) throws {
    try FileManager.default.createDirectory(atPath: outputDir, withIntermediateDirectories: true)
    let jsonObj = AnyShowcase_JsonCodec.instance.encode(ctx, data)
    let jsonData = try JSONSerialization.data(withJSONObject: jsonObj as Any, options: [.prettyPrinted, .sortedKeys])
    let path = "\(outputDir)/any-showcase.json"
    try jsonData.write(to: URL(fileURLWithPath: path))
    print("Written JSON to \(path)")
}

func writeUebaAny(_ ctx: BaboonCodecContext, _ data: AnyShowcase, _ outputDir: String) throws {
    try FileManager.default.createDirectory(atPath: outputDir, withIntermediateDirectories: true)
    let w = BaboonBinWriter()
    AnyShowcase_UebaCodec.instance.encode(ctx, w, data)
    let path = "\(outputDir)/any-showcase.ueba"
    try w.toData().write(to: URL(fileURLWithPath: path))
    print("Written UEBA to \(path)")
}

func decodeInner(_ o: AnyOpaque) throws -> InnerPayload {
    switch o {
    case .ueba(_, let bytes):
        let r = BaboonBinReader(bytes)
        return try InnerPayload_UebaCodec.instance.decode(.compact, r)
    case .json(_, let json):
        return try InnerPayload_JsonCodec.instance.decode(.compact, json as Any)
    }
}

func decodeAllPayloads(_ v: AnyShowcase) throws -> [InnerPayload] {
    guard let opt = v.optAny else {
        throw BaboonCodecException.decoderFailure("optAny was nil; expected non-nil", nil)
    }
    guard let lst0 = v.lstAny.first else {
        throw BaboonCodecException.decoderFailure("lstAny was empty; expected one element", nil)
    }
    return [
        try decodeInner(v.vAnyA),
        try decodeInner(v.vAnyB),
        try decodeInner(v.vAnyC),
        try decodeInner(v.vAnyD1),
        try decodeInner(v.vAnyD2),
        try decodeInner(v.vAnyD3),
        try decodeInner(opt),
        try decodeInner(lst0),
    ]
}

func readAndVerifyAnyShowcase(_ filePath: String) throws {
    let data: AnyShowcase
    if filePath.hasSuffix(".json") {
        let raw = try Data(contentsOf: URL(fileURLWithPath: filePath))
        let json = try JSONSerialization.jsonObject(with: raw, options: [.fragmentsAllowed])
        data = try AnyShowcase_JsonCodec.instance.decode(.defaultCtx, json)
    } else {
        let raw = try Data(contentsOf: URL(fileURLWithPath: filePath))
        let r = BaboonBinReader(raw)
        data = try AnyShowcase_UebaCodec.instance.decode(.defaultCtx, r)
    }
    let expected = expectedInnerPayloads()
    let decoded = try decodeAllPayloads(data)
    for i in 0..<expected.count {
        if expected[i] != decoded[i] {
            fail("AnyShowcase payload \(i) mismatch: expected \(expected[i]), got \(decoded[i])")
        }
    }
    if filePath.hasSuffix(".json") {
        let reEncoded = AnyShowcase_JsonCodec.instance.encode(.defaultCtx, data)
        let reDecoded = try AnyShowcase_JsonCodec.instance.decode(.defaultCtx, reEncoded)
        guard reDecoded == data else {
            fail("AnyShowcase JSON roundtrip mismatch (structural)")
        }
    } else {
        let writer = BaboonBinWriter()
        AnyShowcase_UebaCodec.instance.encode(.defaultCtx, writer, data)
        let reReader = BaboonBinReader(writer.toData())
        let reDecoded = try AnyShowcase_UebaCodec.instance.decode(.defaultCtx, reReader)
        guard reDecoded == data else {
            fail("AnyShowcase UEBA roundtrip mismatch (structural)")
        }
    }
}

func writeJson(_ data: AllBasicTypes, _ outputDir: String) throws {
    try FileManager.default.createDirectory(atPath: outputDir, withIntermediateDirectories: true)
    let jsonObj = AllBasicTypes_JsonCodec.instance.encode(.defaultCtx, data)
    let jsonData = try JSONSerialization.data(withJSONObject: jsonObj, options: [.prettyPrinted, .sortedKeys])
    let jsonPath = "\(outputDir)/all-basic-types.json"
    try jsonData.write(to: URL(fileURLWithPath: jsonPath))
    print("Written JSON to \(jsonPath)")
}

func writeUeba(_ data: AllBasicTypes, _ outputDir: String) throws {
    try FileManager.default.createDirectory(atPath: outputDir, withIntermediateDirectories: true)
    let writer = BaboonBinWriter()
    AllBasicTypes_UebaCodec.instance.encode(.defaultCtx, writer, data)
    let uebaPath = "\(outputDir)/all-basic-types.ueba"
    try writer.toData().write(to: URL(fileURLWithPath: uebaPath))
    print("Written UEBA to \(uebaPath)")
}

func readAndVerify(_ filePath: String) throws {
    if filePath.hasSuffix("any-showcase.json") || filePath.hasSuffix("any-showcase.ueba") {
        try readAndVerifyAnyShowcase(filePath)
        return
    }
    if filePath.hasSuffix("m29-ok.json") || filePath.hasSuffix("m29-ok.ueba") {
        try readAndVerifyM29Ok(filePath)
        return
    }
    let data: AllBasicTypes
    if filePath.hasSuffix(".json") {
        let raw = try Data(contentsOf: URL(fileURLWithPath: filePath))
        let json = try JSONSerialization.jsonObject(with: raw, options: [.fragmentsAllowed])
        data = try AllBasicTypes_JsonCodec.instance.decode(.defaultCtx, json)
    } else if filePath.hasSuffix(".ueba") {
        let raw = try Data(contentsOf: URL(fileURLWithPath: filePath))
        let reader = BaboonBinReader(raw)
        data = try AllBasicTypes_UebaCodec.instance.decode(.defaultCtx, reader)
    } else {
        fail("Unknown file extension: \(filePath)")
    }

    guard data.vstr == "Hello, Baboon!" else {
        fail("vstr mismatch: expected 'Hello, Baboon!', got '\(data.vstr)'")
    }
    guard data.vi32 == 123456 else {
        fail("vi32 mismatch: expected 123456, got \(data.vi32)")
    }
    guard data.vbit else {
        fail("vbit mismatch: expected true, got false")
    }

    if filePath.hasSuffix(".json") {
        let reEncoded = AllBasicTypes_JsonCodec.instance.encode(.defaultCtx, data)
        let reDecoded = try AllBasicTypes_JsonCodec.instance.decode(.defaultCtx, reEncoded)
        guard reDecoded == data else {
            fail("AllBasicTypes JSON roundtrip mismatch (structural)")
        }
    } else {
        let writer = BaboonBinWriter()
        AllBasicTypes_UebaCodec.instance.encode(.defaultCtx, writer, data)
        let reReader = BaboonBinReader(writer.toData())
        let reDecoded = try AllBasicTypes_UebaCodec.instance.decode(.defaultCtx, reReader)
        guard reDecoded == data else {
            fail("AllBasicTypes UEBA roundtrip mismatch (structural)")
        }
    }
}

// PR-57e (M18.4e) — cross-language identifier repr (description) byte-identity.
// Per spec §7 the repr form is a separate invariant from the JSON/UEBA wire bytes;
// we write it as a per-language artifact so the Scala-side test can assert all 10 backends
// produce byte-identical output for the same canonical PointId value.
func writePointIdRepr(_ pid: PointId, _ outputDir: String) throws {
    try FileManager.default.createDirectory(atPath: outputDir, withIntermediateDirectories: true)
    let path = "\(outputDir)/point-id.txt"
    // No trailing newline — exact byte match across all languages.
    try pid.description.write(toFile: path, atomically: true, encoding: .utf8)
    print("Written repr to \(path)")
}

// PR-I.2 (M24 Phase 3.2) — Custom-foreign KeyCodec hook canonical fixture.
// Map keys go through FStr_KeyCodecHost (default identity impl for the stringy
// foreign), so the wire form is `{"m":{"alpha":"v1","beta":"v2"}}`.
func createForeignKeyHolderSample() -> ForeignKeyHolder {
    return ForeignKeyHolder(m: [
        ItemKey(v: "alpha"): "v1",
        ItemKey(v: "beta"): "v2",
    ])
}

func writeForeignKeyHolderJson(_ ctx: BaboonCodecContext, _ data: ForeignKeyHolder, _ outputDir: String) throws {
    try FileManager.default.createDirectory(atPath: outputDir, withIntermediateDirectories: true)
    let jsonObj = ForeignKeyHolder_JsonCodec.instance.encode(ctx, data)
    // Compact (no .prettyPrinted) so the byte-identity assertion against the canonical
    // wire form `{"m":{"alpha":"v1","beta":"v2"}}` matches across backends. `.sortedKeys`
    // gives deterministic key ordering on the outer object AND nested maps.
    let jsonData = try JSONSerialization.data(withJSONObject: jsonObj as Any, options: [.sortedKeys])
    let path = "\(outputDir)/m24-foreign-keycodec.json"
    try jsonData.write(to: URL(fileURLWithPath: path))
    print("Written JSON to \(path)")
}

// PR-29.10 (M29) — monomorphised-template cross-language wire-format fixture.
func createM29OkSample() -> M29OkHolder {
    return M29OkHolder(
        intPage: IntPage(items: [Int32(1), Int32(2), Int32(3)], total: UInt32(3)),
        strPage: StrPage(items: ["hello", "world"], total: UInt32(2)),
        itemPage: ItemPage(items: [Item(name: "apple", price: 1.5), Item(name: "banana", price: 0.75)], total: UInt32(2)),
        okEnvelope: .ok(IntStrEnvelope.Ok(value: Int32(42))),
        errEnvelope: .err(IntStrEnvelope.Err(error: "oops"))
    )
}

func writeM29OkJson(_ ctx: BaboonCodecContext, _ data: M29OkHolder, _ outputDir: String) throws {
    try FileManager.default.createDirectory(atPath: outputDir, withIntermediateDirectories: true)
    let jsonObj = M29OkHolder_JsonCodec.instance.encode(ctx, data)
    let jsonData = try JSONSerialization.data(withJSONObject: jsonObj as Any, options: [.sortedKeys])
    let path = "\(outputDir)/m29-ok.json"
    try jsonData.write(to: URL(fileURLWithPath: path))
    print("Written JSON to \(path)")
}

func writeM29OkUeba(_ ctx: BaboonCodecContext, _ data: M29OkHolder, _ outputDir: String) throws {
    try FileManager.default.createDirectory(atPath: outputDir, withIntermediateDirectories: true)
    let writer = BaboonBinWriter()
    M29OkHolder_UebaCodec.instance.encode(ctx, writer, data)
    let path = "\(outputDir)/m29-ok.ueba"
    try writer.toData().write(to: URL(fileURLWithPath: path))
    print("Written UEBA to \(path)")
}

func readAndVerifyM29Ok(_ filePath: String) throws {
    let data: M29OkHolder
    if filePath.hasSuffix(".json") {
        let raw = try Data(contentsOf: URL(fileURLWithPath: filePath))
        let json = try JSONSerialization.jsonObject(with: raw, options: [.fragmentsAllowed])
        data = try M29OkHolder_JsonCodec.instance.decode(.defaultCtx, json)
    } else {
        let raw = try Data(contentsOf: URL(fileURLWithPath: filePath))
        let r = BaboonBinReader(raw)
        data = try M29OkHolder_UebaCodec.instance.decode(.defaultCtx, r)
    }
    guard data.intPage.total == UInt32(3) else {
        fail("M29OkHolder intPage.total mismatch: expected 3, got \(data.intPage.total)")
    }
    guard case .ok = data.okEnvelope else {
        fail("M29OkHolder okEnvelope is not Ok")
    }
    guard case .err = data.errEnvelope else {
        fail("M29OkHolder errEnvelope is not Err")
    }
    // Roundtrip: re-encode the decoded value and decode again; compare structural equality.
    if filePath.hasSuffix(".json") {
        let reEncoded = M29OkHolder_JsonCodec.instance.encode(.defaultCtx, data)
        let reDecoded = try M29OkHolder_JsonCodec.instance.decode(.defaultCtx, reEncoded)
        guard reDecoded == data else {
            fail("M29OkHolder JSON roundtrip mismatch (structural)")
        }
    } else {
        let writer = BaboonBinWriter()
        M29OkHolder_UebaCodec.instance.encode(.defaultCtx, writer, data)
        let reReader = BaboonBinReader(writer.toData())
        let reDecoded = try M29OkHolder_UebaCodec.instance.decode(.defaultCtx, reReader)
        guard reDecoded == data else {
            fail("M29OkHolder UEBA roundtrip mismatch (structural)")
        }
    }
}

func runLegacy() throws {
    let sampleData = createSampleData()
    let sampleAny = try createSampleAnyShowcase()
    let baseDir = URL(fileURLWithPath: "../../target/compat-test").standardizedFileURL.path
    let facadeCtx = BaboonCodecContext.withFacade(false, freshFacade())
    try writeJson(sampleData, "\(baseDir)/swift-json")
    try writeUeba(sampleData, "\(baseDir)/swift-ueba")
    try writeJsonAny(facadeCtx, sampleAny, "\(baseDir)/swift-json")
    try writeUebaAny(facadeCtx, sampleAny, "\(baseDir)/swift-ueba")
    try writePointIdRepr(sampleData.vPointId, "\(baseDir)/swift-repr")
    try writeForeignKeyHolderJson(.defaultCtx, createForeignKeyHolderSample(), "\(baseDir)/swift-json")
    try writeBuiltinMapKeyHolderJson(.defaultCtx, createBuiltinMapKeyHolderSample(), "\(baseDir)/swift-json")
    try writeBuiltinMapKeyHolderUeba(.defaultCtx, createBuiltinMapKeyHolderSample(), "\(baseDir)/swift-ueba")
    print("Swift serialization complete!")
}

// PR-26.5 (M26) — non-string builtin map-key cross-language fixture.
// PR-28.4 (M28) — extended with mu64 + mtso (mf64 deferred).
func createBuiltinMapKeyHolderSample() -> BuiltinMapKeyHolder {
    // PR-28.4: non-UTC tso offset (PR-28.3 ±HH:MM canonicalisation).
    // Wall-clock 2026-05-02T12:00:00.123+05:30 = instant 2026-05-02T06:30:00.123Z.
    var tsoUtcCal = Calendar(identifier: .gregorian)
    tsoUtcCal.timeZone = TimeZone(secondsFromGMT: 0)!
    var tsoIstUtcComps = DateComponents()
    tsoIstUtcComps.year = 2026
    tsoIstUtcComps.month = 5
    tsoIstUtcComps.day = 2
    tsoIstUtcComps.hour = 6
    tsoIstUtcComps.minute = 30
    tsoIstUtcComps.second = 0
    tsoIstUtcComps.nanosecond = 123 * 1_000_000
    let tsoIstUtcDate: Date = tsoUtcCal.date(from: tsoIstUtcComps)!
    let tsoIstEpochSecs: Double = tsoIstUtcDate.timeIntervalSince1970
    let tsoIstEpochMs: Int64 = Int64(tsoIstEpochSecs * 1000.0)
    let tsoIstOffsetSecs: Int64 = Int64(5 * 3600 + 30 * 60)
    let tsoIstOffsetMs: Int64 = tsoIstOffsetSecs * 1000
    let tsoIst = BaboonDateTimeOffset(
        epochMillis: tsoIstEpochMs,
        offsetMillis: tsoIstOffsetMs,
        kind: "offset"
    )
    return BuiltinMapKeyHolder(
        mi32: [Int32(42): "v32"],
        mi64: [Int64(9223372036854775807): "vmax"],
        mu32: [UInt32(7): "vu32"],
        // PR-28.4 (M28): u64 = UInt64.max → canonical "18446744073709551615".
        mu64: [UInt64.max: "vu64max"],
        mbit: [true: "vt"],
        muid: [UUID(uuidString: "00000000-0000-0000-0000-000000000001")!: "vid"],
        mtso: [tsoIst: "vtso_ist"]
    )
}

func writeBuiltinMapKeyHolderJson(_ ctx: BaboonCodecContext, _ data: BuiltinMapKeyHolder, _ outputDir: String) throws {
    try FileManager.default.createDirectory(atPath: outputDir, withIntermediateDirectories: true)
    let jsonObj = BuiltinMapKeyHolder_JsonCodec.instance.encode(ctx, data)
    let jsonData = try JSONSerialization.data(withJSONObject: jsonObj as Any, options: [.sortedKeys])
    let path = "\(outputDir)/m26-builtin-map-keys.json"
    try jsonData.write(to: URL(fileURLWithPath: path))
    print("Written JSON to \(path)")
}

func writeBuiltinMapKeyHolderUeba(_ ctx: BaboonCodecContext, _ data: BuiltinMapKeyHolder, _ outputDir: String) throws {
    try FileManager.default.createDirectory(atPath: outputDir, withIntermediateDirectories: true)
    let writer = BaboonBinWriter()
    BuiltinMapKeyHolder_UebaCodec.instance.encode(ctx, writer, data)
    let path = "\(outputDir)/m26-builtin-map-keys.ueba"
    try writer.toData().write(to: URL(fileURLWithPath: path))
    print("Written UEBA to \(path)")
}

let args = Array(CommandLine.arguments.dropFirst())

do {
    if args.first == "write" {
        guard args.count == 3 else {
            fail("Usage: CompatMain write <outputDir> <json|ueba>")
        }
        let sampleData = createSampleData()
        let sampleAny = try createSampleAnyShowcase()
        let facadeCtx = BaboonCodecContext.withFacade(false, freshFacade())
        let m29Sample = createM29OkSample()
        let outputDir = args[1]
        let format = args[2]
        switch format {
        case "json":
            try writeJson(sampleData, outputDir)
            try writeJsonAny(facadeCtx, sampleAny, outputDir)
            try writeForeignKeyHolderJson(.defaultCtx, createForeignKeyHolderSample(), outputDir)
            try writeM29OkJson(.defaultCtx, m29Sample, outputDir)
        case "ueba":
            try writeUeba(sampleData, outputDir)
            try writeUebaAny(facadeCtx, sampleAny, outputDir)
            try writeM29OkUeba(.defaultCtx, m29Sample, outputDir)
        default:
            fail("Unknown format: \(format)")
        }
    } else if args.first == "read" {
        guard args.count == 2 else {
            fail("Usage: CompatMain read <filePath>")
        }
        try readAndVerify(args[1])
        print("OK")
    } else {
        try runLegacy()
    }
} catch {
    fail("Swift compat command failed: \(error)")
}
