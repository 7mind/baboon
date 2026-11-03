// Generated UEBA decoders must throw BaboonCodecError on truncated or malformed input instead of
// aborting the process through an out-of-bounds read, an invalid range or a duplicate-key trap
// (the binary counterpart of GitHub 7mind/baboon#106).
//
// Uses the testpkg.pkg0 fixtures; generated symbols are produced by mdl :test-gen-regular-adt.

import XCTest
import Foundation
@testable import BaboonRuntime
@testable import TestpkgPkg0

final class UebaDecodeMalformedInputTests: XCTestCase {

    private func encode<T>(_ codec: BaboonBinCodecBase<T>, _ ctx: BaboonCodecContext, _ value: T) -> Data {
        let writer = BaboonBinWriter()
        codec.encode(ctx, writer, value)
        return writer.toData()
    }

    private func assertCodecError<T>(
        _ label: String,
        _ body: () throws -> T,
        file: StaticString = #filePath,
        line: UInt = #line
    ) {
        do {
            let got = try body()
            XCTFail("\(label): expected BaboonCodecError, got \(got)", file: file, line: line)
        } catch is BaboonCodecError {
        } catch {
            XCTFail("\(label): expected BaboonCodecError, got \(error)", file: file, line: line)
        }
    }

    private func t1d1(_ f6: [Data: String]) -> T1_D1 {
        T1_D1(f0: T1_D2(f: "s", f1: nil), f1: 1, f3: "t", f4: Data([0x01]), f5: nil, f6: f6)
    }

    func testEveryTruncatedPrefixThrows() {
        let rnd = BaboonRandomFactory.create()
        for ctx in [BaboonCodecContext.compact, BaboonCodecContext.indexed] {
            for _ in 0..<5 {
                let data = encode(T6_D1_UebaCodec.instance, ctx, T6_D1_Fixture.random(rnd))
                XCTAssertNoThrow(try T6_D1_UebaCodec.instance.decode(ctx, BaboonBinReader(data)))
                for length in 0..<data.count {
                    assertCodecError("T6_D1 prefix \(length)/\(data.count) indexed=\(ctx.useIndices)") {
                        try T6_D1_UebaCodec.instance.decode(ctx, BaboonBinReader(data.prefix(length)))
                    }
                }
            }
        }
    }

    // f6 is the last field, so a compact encoding with an empty map ends with its i32 count.
    func testNegativeCollectionCountThrows() {
        var data = encode(T1_D1_UebaCodec.instance, .compact, t1d1([:]))
        data.replaceSubrange((data.count - 4)..<data.count, with: [0xFF, 0xFF, 0xFF, 0xFF])
        assertCodecError("T1_D1.f6 count -1") {
            try T1_D1_UebaCodec.instance.decode(.compact, BaboonBinReader(data))
        }
    }

    func testDuplicateMapKeyThrows() {
        let first: [UInt8] = [0xDE, 0xAD, 0xBE, 0xEF, 0x01]
        let second: [UInt8] = [0xDE, 0xAD, 0xBE, 0xEF, 0x02]
        var data = encode(T1_D1_UebaCodec.instance, .compact, t1d1([Data(first): "a", Data(second): "b"]))
        let range = data.range(of: Data(second))!
        data.replaceSubrange(range, with: first)
        assertCodecError("T1_D1.f6 duplicate key") {
            try T1_D1_UebaCodec.instance.decode(.compact, BaboonBinReader(data))
        }
    }

    func testTimestampKindOutOfRangeThrows() {
        let writer = BaboonBinWriter()
        writer.writeI64(0)
        writer.writeI64(0)
        writer.writeU8(3)
        assertCodecError("tsu kind 3") { try BaboonBinReader(writer.toData()).readTsu() }
        assertCodecError("tso kind 3") { try BaboonBinReader(writer.toData()).readTso() }
    }
}
