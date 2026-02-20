import Foundation
@testable import BaboonGenerated

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
    let baseDir = URL(fileURLWithPath: "../../target/compat-test").standardizedFileURL.path
    try writeJson(sampleData, "\(baseDir)/swift-json")
    try writeUeba(sampleData, "\(baseDir)/swift-ueba")
    print("Swift serialization complete!")
}

let args = Array(CommandLine.arguments.dropFirst())

do {
    if args.first == "write" {
        guard args.count == 3 else {
            fail("Usage: CompatMain write <outputDir> <json|ueba>")
        }
        let sampleData = createSampleData()
        let outputDir = args[1]
        let format = args[2]
        switch format {
        case "json":
            try writeJson(sampleData, outputDir)
        case "ueba":
            try writeUeba(sampleData, outputDir)
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
