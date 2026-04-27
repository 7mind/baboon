import XCTest
import Foundation
import BaboonRuntime
@testable import ConvtestTestpkg

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

    // -------------------------------------------------------------------------
    // AnyShowcase cross-language tests (M13 / PR 13.2)
    // -------------------------------------------------------------------------

    let expectedAnyPayloads: [InnerPayload] = [
        InnerPayload(label: "variant-A", count: 1),
        InnerPayload(label: "variant-B", count: 2),
        InnerPayload(label: "variant-C", count: 3),
        InnerPayload(label: "variant-D1", count: 4),
        InnerPayload(label: "variant-D2", count: 5),
        InnerPayload(label: "variant-D3", count: 6),
        InnerPayload(label: "opt-any", count: 7),
        InnerPayload(label: "lst-any-0", count: 8),
    ]

    func readAnyShowcaseJson(_ source: String) throws -> AnyShowcase {
        let filePath = "\(baseDir)/\(source)-json/any-showcase.json"
        let data = try Data(contentsOf: URL(fileURLWithPath: filePath))
        let json = try JSONSerialization.jsonObject(with: data)
        return try AnyShowcase_JsonCodec.instance.decode(ctx, json)
    }

    func readAnyShowcaseUeba(_ source: String) throws -> AnyShowcase {
        let filePath = "\(baseDir)/\(source)-ueba/any-showcase.ueba"
        let data = try Data(contentsOf: URL(fileURLWithPath: filePath))
        let reader = BaboonBinReader(data)
        return try AnyShowcase_UebaCodec.instance.decode(ctx, reader)
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
        guard let opt = v.optAny else { XCTFail("optAny was nil"); return [] }
        guard let lst0 = v.lstAny.first else { XCTFail("lstAny was empty"); return [] }
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

    func assertAnyShowcase(_ source: String, _ fmt: String, _ v: AnyShowcase) throws {
        let decoded = try decodeAllPayloads(v)
        XCTAssertEqual(decoded.count, expectedAnyPayloads.count, "\(source) \(fmt) count mismatch")
        for i in 0..<expectedAnyPayloads.count {
            XCTAssertEqual(decoded[i], expectedAnyPayloads[i], "\(source) \(fmt) payload \(i) mismatch")
        }
    }

    func testAnyShowcaseJsonCrossLanguage() throws {
        for source in sources {
            let filePath = "\(baseDir)/\(source)-json/any-showcase.json"
            guard FileManager.default.fileExists(atPath: filePath) else {
                print("Skipping \(source) any-showcase JSON - file not found")
                continue
            }
            try assertAnyShowcase(source, "JSON", try readAnyShowcaseJson(source))
        }
    }

    func testAnyShowcaseUebaCrossLanguage() throws {
        for source in sources {
            let filePath = "\(baseDir)/\(source)-ueba/any-showcase.ueba"
            guard FileManager.default.fileExists(atPath: filePath) else {
                print("Skipping \(source) any-showcase UEBA - file not found")
                continue
            }
            try assertAnyShowcase(source, "UEBA", try readAnyShowcaseUeba(source))
        }
    }

    func testAnyShowcaseUebaByteIdenticalSwiftScala() throws {
        let swiftPath = "\(baseDir)/swift-ueba/any-showcase.ueba"
        let scalaPath = "\(baseDir)/scala-ueba/any-showcase.ueba"
        guard FileManager.default.fileExists(atPath: swiftPath),
              FileManager.default.fileExists(atPath: scalaPath) else {
            print("Skipping byte-identical: Swift or Scala fixture missing")
            return
        }
        let swiftBytes = try Data(contentsOf: URL(fileURLWithPath: swiftPath))
        let scalaBytes = try Data(contentsOf: URL(fileURLWithPath: scalaPath))
        XCTAssertEqual(swiftBytes, scalaBytes, "Swift and Scala UEBA bytes diverged")
    }
}
