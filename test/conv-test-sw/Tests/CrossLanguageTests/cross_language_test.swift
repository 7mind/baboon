import XCTest
import Foundation
@testable import BaboonGenerated

final class CrossLanguageTests: XCTestCase {
    let baseDir = URL(fileURLWithPath: "../../target/compat-test").standardizedFileURL.path
    let ctx = BaboonCodecContext.defaultCtx

    let sources = ["swift", "scala", "cs", "rust", "python", "typescript", "kotlin", "java", "dart"]

    func readJsonFile(_ source: String) throws -> AllBasicTypes {
        let filePath = "\(baseDir)/\(source)-json/all-basic-types.json"
        let data = try Data(contentsOf: URL(fileURLWithPath: filePath))
        let json = try JSONSerialization.jsonObject(with: data)
        return try AllBasicTypes_JsonCodec.instance.decode(ctx, json)
    }

    func readUebaFile(_ source: String) throws -> AllBasicTypes {
        let filePath = "\(baseDir)/\(source)-ueba/all-basic-types.ueba"
        let data = try Data(contentsOf: URL(fileURLWithPath: filePath))
        let reader = BaboonBinReader(data)
        return try AllBasicTypes_UebaCodec.instance.decode(ctx, reader)
    }

    func testJsonCrossLanguageCompatibility() throws {
        for source in sources {
            let filePath = "\(baseDir)/\(source)-json/all-basic-types.json"
            guard FileManager.default.fileExists(atPath: filePath) else {
                print("Skipping \(source) JSON - file not found")
                continue
            }
            let decoded = try readJsonFile(source)
            let reEncoded = AllBasicTypes_JsonCodec.instance.encode(ctx, decoded)
            let reDecoded = try AllBasicTypes_JsonCodec.instance.decode(ctx, reEncoded)
            XCTAssertEqual(reDecoded, decoded, "JSON round-trip failed for \(source)")
        }
    }

    func testUebaCrossLanguageCompatibility() throws {
        for source in sources {
            let filePath = "\(baseDir)/\(source)-ueba/all-basic-types.ueba"
            guard FileManager.default.fileExists(atPath: filePath) else {
                print("Skipping \(source) UEBA - file not found")
                continue
            }
            let decoded = try readUebaFile(source)
            let writer = BaboonBinWriter()
            AllBasicTypes_UebaCodec.instance.encode(ctx, writer, decoded)
            let reReader = BaboonBinReader(writer.toData())
            let reDecoded = try AllBasicTypes_UebaCodec.instance.decode(ctx, reReader)
            XCTAssertEqual(reDecoded, decoded, "UEBA round-trip failed for \(source)")
        }
    }
}
