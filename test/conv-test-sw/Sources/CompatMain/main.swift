import Foundation
import BaboonRuntime
@testable import ConvtestTestpkg

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
        vmapLst: ["numbers": [1, 2, 3], "more": [4, 5, 6]]
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
        guard reDecoded.vstr == data.vstr && reDecoded.vi32 == data.vi32 && reDecoded.vbit == data.vbit else {
            fail("JSON roundtrip mismatch")
        }
    } else {
        let writer = BaboonBinWriter()
        AllBasicTypes_UebaCodec.instance.encode(.defaultCtx, writer, data)
        let reReader = BaboonBinReader(writer.toData())
        let reDecoded = try AllBasicTypes_UebaCodec.instance.decode(.defaultCtx, reReader)
        guard reDecoded.vstr == data.vstr && reDecoded.vi32 == data.vi32 && reDecoded.vbit == data.vbit else {
            fail("UEBA roundtrip mismatch")
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
    print("Swift serialization complete!")
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
        let outputDir = args[1]
        let format = args[2]
        switch format {
        case "json":
            try writeJson(sampleData, outputDir)
            try writeJsonAny(facadeCtx, sampleAny, outputDir)
        case "ueba":
            try writeUeba(sampleData, outputDir)
            try writeUebaAny(facadeCtx, sampleAny, outputDir)
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
