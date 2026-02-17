import Foundation
@testable import BaboonGenerated

func createSampleData() -> AllBasicTypes {
    var calendar = Calendar(identifier: .gregorian)
    calendar.timeZone = TimeZone(identifier: "UTC")!

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

let sampleData = createSampleData()
let ctx = BaboonCodecContext.defaultCtx

let baseDir = URL(fileURLWithPath: "../../target/compat-test").standardizedFileURL.path
let swiftJsonDir = "\(baseDir)/swift-json"
let swiftUebaDir = "\(baseDir)/swift-ueba"

try FileManager.default.createDirectory(atPath: swiftJsonDir, withIntermediateDirectories: true)
try FileManager.default.createDirectory(atPath: swiftUebaDir, withIntermediateDirectories: true)

let jsonObj = AllBasicTypes_JsonCodec.instance.encode(ctx, sampleData)
let jsonData = try JSONSerialization.data(withJSONObject: jsonObj, options: [.prettyPrinted, .sortedKeys])
let jsonPath = "\(swiftJsonDir)/all-basic-types.json"
try jsonData.write(to: URL(fileURLWithPath: jsonPath))
print("Written JSON to \(jsonPath)")

let writer = BaboonBinWriter()
AllBasicTypes_UebaCodec.instance.encode(ctx, writer, sampleData)
let uebaBytes = writer.toBytes()
let uebaPath = "\(swiftUebaDir)/all-basic-types.ueba"
try uebaBytes.write(to: URL(fileURLWithPath: uebaPath))
print("Written UEBA to \(uebaPath)")

print("Swift serialization complete!")
