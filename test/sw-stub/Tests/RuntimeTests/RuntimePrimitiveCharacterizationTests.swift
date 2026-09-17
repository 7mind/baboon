import Foundation
import XCTest
import BaboonRuntime

final class RuntimePrimitiveCharacterizationTests: XCTestCase {
    private struct Indexed: BaboonBinCodecIndexed { let indexElementsCount = 2 }

    func testIndexConsumptionFollowsHeaderRatherThanContext() throws {
        for context in [BaboonCodecContext.compact, BaboonCodecContext.indexed] {
            for header: UInt8 in [0, 1, 2, 3] {
                let writer = BaboonBinWriter()
                writer.writeU8(header)
                if header & 1 != 0 {
                    for value: Int32 in [12, 13, 25, 7] { writer.writeI32(value) }
                }
                writer.writeU8(99)
                let reader = BaboonBinReader(writer.toData())
                let entries = try Indexed().readIndex(context, reader)
                XCTAssertEqual(entries.count, header & 1 == 0 ? 0 : 2)
                let consumed = BaboonBinReader(writer.toData())
                XCTAssertEqual(try Indexed().consumeIndex(context, consumed), entries.count)
                XCTAssertEqual(consumed.position, reader.position)
                XCTAssertEqual(consumed.readU8(), 99)
                XCTAssertEqual(reader.readU8(), 99)
            }
        }
    }

    func testUuidMixedEndianAndSlices() throws {
        let uuid = UUID(uuidString: "00112233-4455-6677-8899-aabbccddeeff")!
        let golden: [UInt8] = [0x33, 0x22, 0x11, 0, 0x55, 0x44, 0x77, 0x66, 0x88, 0x99, 0xaa, 0xbb, 0xcc, 0xdd, 0xee, 0xff]
        let writer = BaboonBinWriter()
        writer.writeUuid(uuid)
        XCTAssertEqual(writer.toData(), Data(golden))
        let prefixed = Data([99, 98] + golden)
        let reader = BaboonBinReader(prefixed.dropFirst(2))
        XCTAssertEqual(try reader.readUuid(), uuid)
        XCTAssertEqual(reader.position, 16)
        for value in [UUID(uuid: (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)), UUID(uuid: (255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255))] + (0..<100).map({ _ in UUID() }) {
            let buffer = BaboonBinWriter()
            buffer.writeUuid(value)
            XCTAssertEqual(try BaboonBinReader(buffer.toData()).readUuid(), value)
        }
        let short = BaboonBinReader(Data(golden.dropLast()))
        XCTAssertThrowsError(try short.readUuid()) { error in
            guard case BaboonCodecError.truncated(let message) = error else { return XCTFail("Unexpected error: \(error)") }
            XCTAssertEqual(message, "readUuid: need 16 bytes at pos 0, only 15 available")
        }
        XCTAssertEqual(short.position, 0)
    }

    func testFixedReadCountsUnicodeScalarsAndPreservesFailurePosition() {
        let cursor = BaboonIdentifierRepr.Cursor("Aé😀e\u{301}:suffix")
        func read(_ count: Int, _ expected: String) {
            guard case .right(let actual) = cursor.readFixed(count) else { return XCTFail("Read failed") }
            XCTAssertEqual(actual, expected)
        }
        read(0, "")
        read(3, "Aé😀")
        XCTAssertEqual(cursor.position(), 7)
        read(2, "e\u{301}")
        XCTAssertEqual(cursor.position(), 10)
        guard case .left(let message) = cursor.readFixed(99) else { return XCTFail("Expected short read") }
        XCTAssertEqual(message, "expected 99 chars at 10 but only 7 remain")
        XCTAssertEqual(cursor.position(), 10)
        read(7, ":suffix")
        XCTAssertTrue(cursor.atEnd())
    }
}
