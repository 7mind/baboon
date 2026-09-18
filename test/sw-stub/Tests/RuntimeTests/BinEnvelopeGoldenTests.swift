// Cross-language golden bytes for the top-level binary envelope (docs/spec/codec-envelope.md §2.1, §2.1.2, §2.1.3; docs/forward-compat.md, "Worked examples"). The same values must produce these exact bytes in every backend; the Scala and TypeScript stubs assert the same sequences structurally.
import XCTest
@testable import BaboonRuntime
@testable import Fwde2eChain
@testable import Fwde2eFwd

final class BinEnvelopeGoldenTests: XCTestCase {
    private func hx(_ d: Data) -> String { d.map { String(format: "%02X", $0) }.joined(separator: " ") }

    private func enc(_ f: BaboonCodecsFacade, _ ctx: BaboonCodecContext, _ v: Any) throws -> Data {
        switch f.encodeToBin(ctx, v) {
        case .success(let bytes): return bytes
        case .failure(let e): XCTFail("encodeToBin must succeed: \(e)"); throw e
        }
    }

    private let v1Tolerant = BaboonCodecContext.custom(false, .tolerant, .v1, nil)
    private let v2Compact = BaboonCodecContext.custom(false, .strict, .v2, nil)
    private let v2Indexed = BaboonCodecContext.custom(true, .strict, .v2, nil)

    func testDefaultContextsWriteV1Strict() {
        XCTAssertEqual(BaboonCodecContext.compact.envelopeVersion, .v1)
        XCTAssertEqual(BaboonCodecContext.compact.forwardWritePolicy, .strict)
        XCTAssertEqual(BaboonCodecContext.indexed.envelopeVersion, .v1)
    }

    func testEnvelopesMatchTheCrossLanguageGoldenBytes() throws {
        let fwd = DomainFwde2eFwdFacade()
        let chain = DomainFwde2eChainFacade()
        let app = FwdAppendVar(a: 42, b: "hi", t: "t")
        // FwdAppendVar, v1 Strict (default) compact: identical bound elided
        XCTAssertEqual(hx(try enc(fwd, BaboonCodecContext.compact, app)), "01 0A 66 77 64 65 32 65 2E 66 77 64 05 32 2E 30 2E 30 00 19 66 77 64 65 32 65 2E 66 77 64 2F 3A 23 46 77 64 41 70 70 65 6E 64 56 61 72 00 2A 00 00 00 02 68 69 01 01 74")
        // FwdAppendVar, v1 Tolerant compact: prefix-compact bound 1.0.0 in the single slot
        XCTAssertEqual(hx(try enc(fwd, v1Tolerant, app)), "01 0A 66 77 64 65 32 65 2E 66 77 64 05 32 2E 30 2E 30 01 05 31 2E 30 2E 30 19 66 77 64 65 32 65 2E 66 77 64 2F 3A 23 46 77 64 41 70 70 65 6E 64 56 61 72 00 2A 00 00 00 02 68 69 01 01 74")
        // FwdAppendVar, v2 compact: flags 0b10, readableMin 1.0.0
        XCTAssertEqual(hx(try enc(fwd, v2Compact, app)), "02 0A 66 77 64 65 32 65 2E 66 77 64 05 32 2E 30 2E 30 02 05 31 2E 30 2E 30 19 66 77 64 65 32 65 2E 66 77 64 2F 3A 23 46 77 64 41 70 70 65 6E 64 56 61 72 00 2A 00 00 00 02 68 69 01 01 74")
        // FwdAppendVar, v2 indexed: flags 0 (prefix-any-mode bound is 2.0.0)
        XCTAssertEqual(hx(try enc(fwd, v2Indexed, app)), "02 0A 66 77 64 65 32 65 2E 66 77 64 05 32 2E 30 2E 30 00 19 66 77 64 65 32 65 2E 66 77 64 2F 3A 23 46 77 64 41 70 70 65 6E 64 56 61 72 01 04 00 00 00 03 00 00 00 07 00 00 00 03 00 00 00 2A 00 00 00 02 68 69 01 01 74")
        // FwdStable, v1 Strict compact: byte-identical since 1.0.0 -> hasMinCompat 1
        XCTAssertEqual(hx(try enc(fwd, BaboonCodecContext.compact, FwdStable(s: "s"))), "01 0A 66 77 64 65 32 65 2E 66 77 64 05 32 2E 30 2E 30 01 05 31 2E 30 2E 30 16 66 77 64 65 32 65 2E 66 77 64 2F 3A 23 46 77 64 53 74 61 62 6C 65 00 01 73")
        // FwdStable, v2 compact: flags 0b01, minCompat 1.0.0, readableMin elided
        XCTAssertEqual(hx(try enc(fwd, v2Compact, FwdStable(s: "s"))), "02 0A 66 77 64 65 32 65 2E 66 77 64 05 32 2E 30 2E 30 01 05 31 2E 30 2E 30 16 66 77 64 65 32 65 2E 66 77 64 2F 3A 23 46 77 64 53 74 61 62 6C 65 00 01 73")
        // FwdEnumHost, v2 compact: flags 0, no bound
        XCTAssertEqual(hx(try enc(fwd, v2Compact, FwdEnumHost(e: FwdEnumGrows.C))), "02 0A 66 77 64 65 32 65 2E 66 77 64 05 32 2E 30 2E 30 00 18 66 77 64 65 32 65 2E 66 77 64 2F 3A 23 46 77 64 45 6E 75 6D 48 6F 73 74 00 02")
        // ChainAppend 3.0.0, v2 compact: flags 0b10, readableMin 1.0.0
        XCTAssertEqual(hx(try enc(chain, v2Compact, ChainAppend(a: 1, b: "b", c: "c"))), "02 0C 66 77 64 65 32 65 2E 63 68 61 69 6E 05 33 2E 30 2E 30 02 05 31 2E 30 2E 30 1A 66 77 64 65 32 65 2E 63 68 61 69 6E 2F 3A 23 43 68 61 69 6E 41 70 70 65 6E 64 00 01 00 00 00 01 01 62 01 01 63")
    }

    func testV2EnvelopeRoundTripsThroughItsOwnFacade() throws {
        let fwd = DomainFwde2eFwdFacade()
        let app = FwdAppendVar(a: 42, b: "hi", t: "t")
        guard case .success(let value) = fwd.decodeFromBinBytes(try enc(fwd, v2Compact, app)) else {
            XCTFail("decodeFromBinBytes must succeed"); return
        }
        XCTAssertEqual(value as? FwdAppendVar, app)
    }
}
