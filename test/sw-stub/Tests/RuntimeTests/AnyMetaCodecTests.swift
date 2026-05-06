// PR 9.1: tests for the new Swift `any`-feature surface — `AnyMeta` invariants, `AnyOpaque`
// equality, `AnyMetaCodec` (binary + JSON), `BaboonTypeMeta`/`BaboonTypeMetaCodec`, the
// `BaboonCodecsFacade` registry/dispatch surface, and the cross-format helpers.
//
// NOTE: this test file lives alongside the codegen-rsync'd Tests/ directory; the surrounding
// `target/test-regular/sw-stub/` Swift package picks it up automatically. The file references
// only runtime symbols (no codegen-specific types), so it builds against the bare `BaboonRuntime`
// target.

import XCTest
import Foundation
@testable import BaboonRuntime

final class AnyMetaCodecTests: XCTestCase {

    // ----- AnyMeta construction (PR-04-D01, PR-12-D01) ---------------------------------------

    func testAnyMeta_acceptsValidKindD3() throws {
        let m = try AnyMeta(kind: 0x00, domain: nil, version: nil, typeid: nil)
        XCTAssertEqual(m.kind, 0x00)
        XCTAssertNil(m.domain)
    }

    func testAnyMeta_acceptsValidKindA() throws {
        let m = try AnyMeta(kind: 0x07, domain: "d", version: "1.0.0", typeid: "T")
        XCTAssertEqual(m.kind, 0x07)
        XCTAssertEqual(m.domain, "d")
        XCTAssertEqual(m.version, "1.0.0")
        XCTAssertEqual(m.typeid, "T")
    }

    func testAnyMeta_rejectsReservedKind04() {
        XCTAssertThrowsError(try AnyMeta(kind: 0x04, domain: "d", version: nil, typeid: nil)) { err in
            guard case BaboonCodecException.encoderFailure(let msg, _) = err else {
                XCTFail("Expected encoderFailure, got \(err)"); return
            }
            XCTAssertTrue(msg.contains("reserved or invalid meta-kind"))
        }
    }

    func testAnyMeta_rejectsReservedKind05() {
        XCTAssertThrowsError(try AnyMeta(kind: 0x05, domain: "d", version: "v", typeid: nil))
    }

    func testAnyMeta_rejectsKindMismatchDomain() {
        // kind 0x07 says all three present, but we pass nil domain.
        XCTAssertThrowsError(try AnyMeta(kind: 0x07, domain: nil, version: "v", typeid: "t")) { err in
            guard case BaboonCodecException.encoderFailure(let msg, _) = err else {
                XCTFail("Expected encoderFailure, got \(err)"); return
            }
            XCTAssertTrue(msg.contains("domain"))
        }
    }

    func testAnyMeta_rejectsKindMismatchVersion() {
        XCTAssertThrowsError(try AnyMeta(kind: 0x07, domain: "d", version: nil, typeid: "t"))
    }

    func testAnyMeta_rejectsKindMismatchTypeid() {
        XCTAssertThrowsError(try AnyMeta(kind: 0x07, domain: "d", version: "v", typeid: nil))
    }

    func testAnyMeta_acceptsAllSixLockedKinds() throws {
        // 0x00 D3, 0x01 D2 (typeid only), 0x02 (version only), 0x03 (version+typeid),
        // 0x06 B (domain+version), 0x07 A (all three).
        _ = try AnyMeta(kind: 0x00, domain: nil, version: nil, typeid: nil)
        _ = try AnyMeta(kind: 0x01, domain: nil, version: nil, typeid: "t")
        _ = try AnyMeta(kind: 0x02, domain: nil, version: "v", typeid: nil)
        _ = try AnyMeta(kind: 0x03, domain: nil, version: "v", typeid: "t")
        _ = try AnyMeta(kind: 0x06, domain: "d", version: "v", typeid: nil)
        _ = try AnyMeta(kind: 0x07, domain: "d", version: "v", typeid: "t")
    }

    // ----- AnyMetaCodec.writeBin / readBin ---------------------------------------------------

    func testAnyMetaCodec_writeBin_readBin_kindA_roundtrip() throws {
        let m = try AnyMeta(kind: 0x07, domain: "my.domain", version: "1.2.3", typeid: "MyT")
        let writer = BaboonBinWriter()
        AnyMetaCodec.writeBin(m, writer)
        let reader = BaboonBinReader(writer.toData())
        let m2 = try AnyMetaCodec.readBin(reader)
        XCTAssertEqual(m, m2)
    }

    func testAnyMetaCodec_writeBin_readBin_kindD3_roundtrip() throws {
        let m = try AnyMeta(kind: 0x00, domain: nil, version: nil, typeid: nil)
        let writer = BaboonBinWriter()
        AnyMetaCodec.writeBin(m, writer)
        let reader = BaboonBinReader(writer.toData())
        let m2 = try AnyMetaCodec.readBin(reader)
        XCTAssertEqual(m, m2)
    }

    func testAnyMetaCodec_writeBin_readBin_nonAsciiUtf8() throws {
        let m = try AnyMeta(kind: 0x07, domain: "domain.unicode.test", version: "1.0.0", typeid: "Тест.类型")
        let writer = BaboonBinWriter()
        AnyMetaCodec.writeBin(m, writer)
        let reader = BaboonBinReader(writer.toData())
        let m2 = try AnyMetaCodec.readBin(reader)
        XCTAssertEqual(m, m2)
    }

    func testAnyMetaCodec_writeBin_readBin_emptyString() throws {
        let m = try AnyMeta(kind: 0x07, domain: "", version: "", typeid: "")
        let writer = BaboonBinWriter()
        AnyMetaCodec.writeBin(m, writer)
        let reader = BaboonBinReader(writer.toData())
        let m2 = try AnyMetaCodec.readBin(reader)
        XCTAssertEqual(m, m2)
    }

    func testAnyMetaCodec_writeBin_readBin_longStringMultibyteVlq() throws {
        // String length 128 forces a multi-byte VLQ prefix (`0x80 0x01`)
        let s = String(repeating: "x", count: 128)
        let m = try AnyMeta(kind: 0x07, domain: s, version: "1.0.0", typeid: "T")
        let writer = BaboonBinWriter()
        AnyMetaCodec.writeBin(m, writer)
        let reader = BaboonBinReader(writer.toData())
        let m2 = try AnyMetaCodec.readBin(reader)
        XCTAssertEqual(m, m2)
    }

    // ----- AnyMetaCodec.readBinWithLength (PR-05-D01) ----------------------------------------

    func testReadBinWithLength_reportsBytesConsumed() throws {
        let m = try AnyMeta(kind: 0x07, domain: "d", version: "v", typeid: "t")
        let writer = BaboonBinWriter()
        AnyMetaCodec.writeBin(m, writer)
        let buffer = writer.toData()
        let reader = BaboonBinReader(buffer)
        let (m2, n) = try AnyMetaCodec.readBinWithLength(reader)
        XCTAssertEqual(m, m2)
        XCTAssertEqual(n, buffer.count)
    }

    func testReadBinWithLength_skipsTrailingExtensionBytes() throws {
        let m = try AnyMeta(kind: 0x00, domain: nil, version: nil, typeid: nil)
        let writer = BaboonBinWriter()
        AnyMetaCodec.writeBin(m, writer)
        // Append 5 extension bytes after the meta block.
        let metaBytes = writer.toData()
        var buf = Data()
        buf.append(metaBytes)
        buf.append(contentsOf: [0xAA, 0xBB, 0xCC, 0xDD, 0xEE])
        let reader = BaboonBinReader(buf)
        let (m2, n) = try AnyMetaCodec.readBinWithLength(reader)
        XCTAssertEqual(m, m2)
        XCTAssertEqual(n, metaBytes.count)
    }

    // ----- AnyMetaCodec.writeJson / readJson (PR-04-D02, PR-06-D08 invariant) ----------------

    func testAnyMetaCodec_writeJson_readJson_kindA_roundtrip() throws {
        let m = try AnyMeta(kind: 0x07, domain: "d", version: "v", typeid: "t")
        let json = AnyMetaCodec.writeJson(m)
        let result = AnyMetaCodec.readJson(json)
        switch result {
        case .success(let m2): XCTAssertEqual(m, m2)
        case .failure(let e): XCTFail("Expected success, got \(e)")
        }
    }

    func testAnyMetaCodec_writeJson_readJson_kindD3_roundtrip() throws {
        let m = try AnyMeta(kind: 0x00, domain: nil, version: nil, typeid: nil)
        let json = AnyMetaCodec.writeJson(m)
        let result = AnyMetaCodec.readJson(json)
        switch result {
        case .success(let m2): XCTAssertEqual(m, m2)
        case .failure(let e): XCTFail("Expected success, got \(e)")
        }
    }

    func testAnyMetaCodec_writeJson_alwaysReturnsObjectAcrossAllSixKinds() throws {
        let kinds: [(UInt8, String?, String?, String?)] = [
            (0x00, nil, nil, nil),
            (0x01, nil, nil, "t"),
            (0x02, nil, "v", nil),
            (0x03, nil, "v", "t"),
            (0x06, "d", "v", nil),
            (0x07, "d", "v", "t"),
        ]
        for (k, d, v, t) in kinds {
            let m = try AnyMeta(kind: k, domain: d, version: v, typeid: t)
            let json = AnyMetaCodec.writeJson(m)
            // [String: Any] is always an object — the assertion is implicit in the type.
            XCTAssertNotNil(json[AnyMetaCodec.ANY_KIND_KEY], "kind 0x\(String(k, radix: 16))")
        }
    }

    func testReadJson_rejectsNonObjectInput() {
        let result = AnyMetaCodec.readJson("not-an-object")
        switch result {
        case .success: XCTFail("Expected failure")
        case .failure(let e): XCTAssertTrue(e.message.contains("expected JSON object"))
        }
    }

    func testReadJson_rejectsMissingKindField() {
        let result = AnyMetaCodec.readJson([String: Any]())
        switch result {
        case .success: XCTFail("Expected failure")
        case .failure(let e): XCTAssertTrue(e.message.contains("$ak"))
        }
    }

    func testReadJson_rejectsKindAsNonNumeric() {
        let result = AnyMetaCodec.readJson(["$ak": "not-a-number"])
        switch result {
        case .success: XCTFail("Expected failure")
        case .failure(let e): XCTAssertTrue(e.message.contains("$ak"))
        }
    }

    func testReadJson_rejectsMissingRequiredDomain() {
        // kind 0x07 requires domain/version/typeid, only typeid present.
        let json: [String: Any] = ["$ak": 0x07, "$at": "t"]
        let result = AnyMetaCodec.readJson(json)
        switch result {
        case .success: XCTFail("Expected failure")
        case .failure(let e):
            XCTAssertTrue(e.message.contains("$ad") || e.message.contains("domain"))
        }
    }

    func testReadJson_rejectsForbiddenDomain() {
        // kind 0x00 forbids domain/version/typeid.
        let json: [String: Any] = ["$ak": 0x00, "$ad": "leaked"]
        let result = AnyMetaCodec.readJson(json)
        switch result {
        case .success: XCTFail("Expected failure")
        case .failure(let e):
            XCTAssertTrue(e.message.contains("forbids"))
        }
    }

    // ----- AnyOpaque equality (PR-05-D08) ----------------------------------------------------

    func testAnyOpaqueUeba_contentEqualBytes_areEqual() throws {
        let m = try AnyMeta(kind: 0x07, domain: "d", version: "v", typeid: "t")
        let a = AnyOpaque.ueba(meta: m, bytes: Data([0x01, 0x02, 0x03]))
        let b = AnyOpaque.ueba(meta: m, bytes: Data([0x01, 0x02, 0x03]))
        XCTAssertEqual(a, b)
    }

    func testAnyOpaqueUeba_differentBytes_areNotEqual() throws {
        let m = try AnyMeta(kind: 0x07, domain: "d", version: "v", typeid: "t")
        let a = AnyOpaque.ueba(meta: m, bytes: Data([0x01, 0x02]))
        let b = AnyOpaque.ueba(meta: m, bytes: Data([0x01, 0x02, 0x03]))
        XCTAssertNotEqual(a, b)
    }

    func testAnyOpaqueUeba_emptyBytes_areEqual() throws {
        let m = try AnyMeta(kind: 0x00, domain: nil, version: nil, typeid: nil)
        let a = AnyOpaque.ueba(meta: m, bytes: Data())
        let b = AnyOpaque.ueba(meta: m, bytes: Data())
        XCTAssertEqual(a, b)
    }

    func testAnyOpaqueJson_contentEqualMaps_areEqual() throws {
        let m = try AnyMeta(kind: 0x07, domain: "d", version: "v", typeid: "t")
        let j1: [String: Any] = ["a": 1, "b": "hello"]
        let j2: [String: Any] = ["a": 1, "b": "hello"]
        let a = AnyOpaque.json(meta: m, json: j1)
        let b = AnyOpaque.json(meta: m, json: j2)
        XCTAssertEqual(a, b)
    }

    func testAnyOpaqueJson_differentValues_areNotEqual() throws {
        let m = try AnyMeta(kind: 0x07, domain: "d", version: "v", typeid: "t")
        let a = AnyOpaque.json(meta: m, json: ["a": 1])
        let b = AnyOpaque.json(meta: m, json: ["a": 2])
        XCTAssertNotEqual(a, b)
    }

    func testAnyOpaque_uebaAndJson_areNotEqual() throws {
        let m = try AnyMeta(kind: 0x07, domain: "d", version: "v", typeid: "t")
        let u = AnyOpaque.ueba(meta: m, bytes: Data())
        let j = AnyOpaque.json(meta: m, json: [String: Any]())
        XCTAssertNotEqual(u, j)
    }

    func testAnyOpaque_metaProperty_returnsMeta() throws {
        let m = try AnyMeta(kind: 0x07, domain: "d", version: "v", typeid: "t")
        let u = AnyOpaque.ueba(meta: m, bytes: Data())
        let j = AnyOpaque.json(meta: m, json: [String: Any]())
        XCTAssertEqual(u.meta, m)
        XCTAssertEqual(j.meta, m)
    }

    // ----- BaboonTypeMeta + BaboonTypeMetaCodec (PR-08-D01 / PR-22-D01) ----------------------

    func testBaboonTypeMeta_writeJson_readMetaJson_roundtrip() {
        let m = BaboonTypeMeta(BaboonTypeMetaCodec.metaVersion, "my.dom", "1.0.0", "1.0.0", "MyT")
        let json = m.writeJson()
        let parsed = BaboonTypeMeta.readMetaJson(json)
        XCTAssertEqual(parsed, m)
    }

    func testReadMetaJson_acceptsMissingMv_asV1() {
        let json: [String: Any] = ["$d": "d", "$v": "1.0.0", "$t": "T"]
        let parsed = BaboonTypeMeta.readMetaJson(json)
        XCTAssertEqual(parsed?.metaVersion, 1)
    }

    func testReadMetaJson_acceptsExplicitMvOne() {
        let json: [String: Any] = ["$mv": "1", "$d": "d", "$v": "1.0.0", "$t": "T"]
        XCTAssertNotNil(BaboonTypeMeta.readMetaJson(json))
    }

    func testReadMetaJson_rejectsMvTwo() {
        let json: [String: Any] = ["$mv": "2", "$d": "d", "$v": "1.0.0", "$t": "T"]
        XCTAssertNil(BaboonTypeMeta.readMetaJson(json))
    }

    func testReadMetaJson_acceptsNumericMv() {
        // MFACADE-PR-3: numeric `$mv` is accepted (canonical form); string form retained
        // for back-compat with M28-vintage fixtures.
        let json: [String: Any] = ["$mv": 1, "$d": "d", "$v": "1.0.0", "$t": "T"]
        XCTAssertNotNil(BaboonTypeMeta.readMetaJson(json))
    }

    func testReadMetaJson_rejectsMissingD() {
        let json: [String: Any] = ["$mv": "1", "$v": "1.0.0", "$t": "T"]
        XCTAssertNil(BaboonTypeMeta.readMetaJson(json))
    }

    func testBaboonTypeMeta_writeBin_readMetaBin_roundtrip() throws {
        let m = BaboonTypeMeta(1, "my.dom", "2.0.0", "1.0.0", "Foo")
        let writer = BaboonBinWriter()
        m.writeBin(writer)
        let reader = BaboonBinReader(writer.toData())
        let parsed = try BaboonTypeMeta.readMetaBin(reader)
        XCTAssertEqual(parsed, m)
    }

    func testBaboonTypeMeta_writeJson_includesUv_whenMinCompatDiffers() {
        let m = BaboonTypeMeta(1, "d", "2.0.0", "1.0.0", "T")
        let json = m.writeJson()
        XCTAssertEqual(json["$uv"] as? String, "1.0.0")
    }

    func testBaboonTypeMeta_writeJson_omitsUv_whenMinCompatEqualsVersion() {
        let m = BaboonTypeMeta(1, "d", "1.0.0", "1.0.0", "T")
        let json = m.writeJson()
        XCTAssertNil(json["$uv"])
    }

    // ----- MFACADE-PR-3-D04: writeJson emits $mv as JSON number ------------------------------

    func testBaboonTypeMeta_writeJson_emitsMvAsNumber() {
        let m = BaboonTypeMeta(BaboonTypeMetaCodec.metaVersion, "d", "1.0.0", "1.0.0", "T")
        let json = m.writeJson()
        let mv = json["$mv"]
        XCTAssertNotNil(mv, "$mv must be present in writeJson output")
        XCTAssertTrue(mv is Int, "$mv must be a Swift Int (JSON number), got \(type(of: mv as Any))")
        XCTAssertFalse(mv is String, "$mv must NOT be a String")
        XCTAssertEqual(mv as? Int, BaboonTypeMetaCodec.metaVersion)
    }

    // ----- MFACADE-PR-3-D06: readMetaJson edge-case rejection matrix -------------------------

    func testReadMetaJson_rejectsFractionalMv() {
        let json: [String: Any] = ["$mv": 1.5, "$d": "d", "$v": "1.0.0", "$t": "T"]
        XCTAssertNil(BaboonTypeMeta.readMetaJson(json))
    }

    func testReadMetaJson_rejectsBooleanMv() {
        let json: [String: Any] = ["$mv": true, "$d": "d", "$v": "1.0.0", "$t": "T"]
        XCTAssertNil(BaboonTypeMeta.readMetaJson(json))
    }

    func testReadMetaJson_rejectsMvOutOfByteRange() {
        let json: [String: Any] = ["$mv": 300, "$d": "d", "$v": "1.0.0", "$t": "T"]
        XCTAssertNil(BaboonTypeMeta.readMetaJson(json))
    }

    func testReadMetaJson_rejectsNegativeMv() {
        let json: [String: Any] = ["$mv": -1, "$d": "d", "$v": "1.0.0", "$t": "T"]
        XCTAssertNil(BaboonTypeMeta.readMetaJson(json))
    }

    func testReadMetaJson_rejectsArrayMv() {
        let json: [String: Any] = ["$mv": [Any](), "$d": "d", "$v": "1.0.0", "$t": "T"]
        XCTAssertNil(BaboonTypeMeta.readMetaJson(json))
    }

    func testReadMetaJson_rejectsDictMv() {
        let json: [String: Any] = ["$mv": [String: Any](), "$d": "d", "$v": "1.0.0", "$t": "T"]
        XCTAssertNil(BaboonTypeMeta.readMetaJson(json))
    }

    // ----- BaboonVersion ---------------------------------------------------------------------

    func testBaboonVersion_parsesValid() throws {
        let v = try BaboonVersion.from("1.2.3")
        XCTAssertEqual(v.major, 1)
        XCTAssertEqual(v.minor, 2)
        XCTAssertEqual(v.patch, 3)
    }

    func testBaboonVersion_compares() throws {
        let a = try BaboonVersion.from("1.0.0")
        let b = try BaboonVersion.from("1.0.1")
        XCTAssertTrue(a < b)
        XCTAssertFalse(b < a)
    }

    func testBaboonVersion_rejectsInvalid() {
        XCTAssertThrowsError(try BaboonVersion.from("not.a.version"))
        XCTAssertThrowsError(try BaboonVersion.from("1.2"))
    }

    // ----- BaboonCodecsFacade — verify(), basic registration ---------------------------------

    func testFacade_verify_failsOnEmptyRegistry() {
        let f = BaboonCodecsFacade()
        XCTAssertThrowsError(try f.verify())
    }

    // ----- BaboonCodecContext.withFacade -----------------------------------------------------

    func testCodecContext_default_hasNoFacade() {
        XCTAssertNil(BaboonCodecContext.default.facade)
        XCTAssertNil(BaboonCodecContext.compact.facade)
        XCTAssertNil(BaboonCodecContext.indexed.facade)
    }

    func testCodecContext_withFacade_carriesFacade() {
        let f = BaboonCodecsFacade()
        let ctx = BaboonCodecContext.withFacade(false, f)
        XCTAssertNotNil(ctx.facade)
        XCTAssertFalse(ctx.useIndices)
    }

    func testCodecContext_withFacade_useIndicesTrue() {
        let f = BaboonCodecsFacade()
        let ctx = BaboonCodecContext.withFacade(true, f)
        XCTAssertTrue(ctx.useIndices)
    }

    // ----- decodeAny / cross-format helpers — Left paths (no codec registered) ---------------

    func testDecodeAny_leftPathOnIncompleteMeta() throws {
        // kind 0x01 (typeid only) — facade resolution requires all three components.
        let m = try AnyMeta(kind: 0x01, domain: nil, version: nil, typeid: "T")
        let opaque = AnyOpaque.ueba(meta: m, bytes: Data())
        let f = BaboonCodecsFacade()
        let result = f.decodeAny(opaque)
        switch result {
        case .success: XCTFail("Expected failure")
        case .failure(let e):
            XCTAssertTrue(e.message.contains("domain"))
            XCTAssertTrue(e.message.contains("version"))
        }
    }

    func testJsonToUebaBytes_acceptsStaticFallback() throws {
        // Variant D3 wire (kind=0x00) — facade must fail because no codec is registered, but the
        // synthetic-meta build must SUCCEED with all three statics supplied.
        let m = try AnyMeta(kind: 0x00, domain: nil, version: nil, typeid: nil)
        let f = BaboonCodecsFacade()
        let result = f.jsonToUebaBytes(
            m, [String: Any](),
            staticDomain: "d", staticVersion: "1.0.0", staticTypeid: "T"
        )
        switch result {
        case .success: XCTFail("Expected codec-not-found failure")
        case .failure(let e):
            // Should fail at codec lookup, not at synthetic-meta build.
            XCTAssertFalse(e.message.contains("requires domain"), "Unexpected meta-build failure: \(e.message)")
        }
    }

    func testJsonToUebaBytes_failsWhenStaticsMissing() throws {
        let m = try AnyMeta(kind: 0x00, domain: nil, version: nil, typeid: nil)
        let f = BaboonCodecsFacade()
        let result = f.jsonToUebaBytes(m, [String: Any]())
        switch result {
        case .success: XCTFail("Expected failure")
        case .failure(let e):
            XCTAssertTrue(e.message.contains("requires domain"))
        }
    }

    func testJsonToUebaBytes_wireOverridesStatic() throws {
        // wire `domain = "wire"` overrides `staticDomain = "static"`. Codec lookup will fail
        // because no codec is registered; we just verify that the meta we threaded into the
        // failure references the wire value.
        let m = try AnyMeta(kind: 0x07, domain: "wire", version: "1.0.0", typeid: "T")
        let f = BaboonCodecsFacade()
        let result = f.jsonToUebaBytes(
            m, [String: Any](),
            staticDomain: "static", staticVersion: "1.0.0", staticTypeid: "T"
        )
        switch result {
        case .success: XCTFail("Expected codec-not-found failure")
        case .failure(let e):
            XCTAssertTrue(e.message.contains("wire"), "Wire domain should win: \(e.message)")
            XCTAssertFalse(e.message.contains("static"), "Static domain must not appear: \(e.message)")
        }
    }

    func testUebaToJson_acceptsStaticFallback() throws {
        let m = try AnyMeta(kind: 0x00, domain: nil, version: nil, typeid: nil)
        let f = BaboonCodecsFacade()
        let result = f.uebaToJson(
            m, Data(),
            staticDomain: "d", staticVersion: "1.0.0", staticTypeid: "T"
        )
        switch result {
        case .success: XCTFail("Expected codec-not-found failure")
        case .failure(let e):
            XCTAssertFalse(e.message.contains("requires domain"))
        }
    }

    // ----- Deep equals / Lazy ----------------------------------------------------------------

    func testBaboonDeepEquals_handlesNullAndPrimitives() {
        XCTAssertTrue(baboonDeepEquals(nil, nil))
        XCTAssertFalse(baboonDeepEquals(nil, 0))
        XCTAssertTrue(baboonDeepEquals("a", "a"))
        XCTAssertTrue(baboonDeepEquals(1, 1))
    }

    func testBaboonDeepEquals_handlesNestedStructure() {
        let a: [String: Any] = ["k": ["v1", "v2"], "n": 42]
        let b: [String: Any] = ["k": ["v1", "v2"], "n": 42]
        XCTAssertTrue(baboonDeepEquals(a, b))
    }

    func testBaboonLazy_evaluatesOnce() {
        var calls = 0
        let lazy = BaboonLazy { () -> Int in calls += 1; return 42 }
        XCTAssertFalse(lazy.isValueCreated)
        XCTAssertEqual(lazy.value, 42)
        XCTAssertEqual(lazy.value, 42)
        XCTAssertTrue(lazy.isValueCreated)
        XCTAssertEqual(calls, 1)
    }

    // ----- MFACADE-PR-4: preload / decodeFromBinLatest / decodeFromJsonLatest ----------------

    func testPreload_doesNotThrowOnEmptyFacade() {
        // preload() is fire-and-forget; on an empty facade the lazy maps are empty and the
        // async block completes immediately without any observable side-effect.
        let f = BaboonCodecsFacade()
        f.preload()   // must not throw, trap, or otherwise crash
    }

    func testDecodeFromBinLatest_roundTrip() throws {
        final class StubLatestValue: BaboonGeneratedLatest, BaboonMetaProvider {
            var baboonDomainVersion: String { "1.0.0" }
            var baboonDomainIdentifier: String { "stub.dom" }
            var baboonTypeIdentifier: String { "StubT" }
            var baboonSameInVersions: [String] { ["1.0.0"] }
        }

        final class StubBinCodec: BaboonBinCodecBase<StubLatestValue> {
            override func decode(_ ctx: BaboonCodecContext, _ reader: BaboonBinReader) throws -> StubLatestValue {
                _ = reader.readU8()
                return StubLatestValue()
            }
            override func encode(_ ctx: BaboonCodecContext, _ writer: BaboonBinWriter, _ value: StubLatestValue) {
                writer.writeU8(0x55)
            }
        }

        final class StubJsonCodec: BaboonJsonCodecBase<StubLatestValue> {
            override func decode(_ ctx: BaboonCodecContext, _ wire: Any) throws -> StubLatestValue {
                return StubLatestValue()
            }
            override func encode(_ ctx: BaboonCodecContext, _ value: StubLatestValue) -> Any { return "stub" }
        }

        final class StubMeta: BaboonMeta {
            static var baboonDomainVersion: String { "1.0.0" }
            static var baboonDomainIdentifier: String { "stub.dom" }
            static var baboonTypeIdentifier: String { "StubT" }
            func sameInVersions(_ typeId: String) -> [String] { ["1.0.0"] }
        }

        let dv = BaboonDomainVersion("stub.dom", "1.0.0")
        let binCodecs = AbstractBaboonUebaCodecs()
        binCodecs.register("StubT") { StubBinCodec() }
        let jsonCodecs = AbstractBaboonJsonCodecs()
        jsonCodecs.register("StubT") { StubJsonCodec() }
        let facade = BaboonCodecsFacade()
        _ = facade.register(dv, codecsJson: { jsonCodecs }, codecsBin: { binCodecs }, conversions: { AbstractBaboonConversions() }, meta: { StubMeta() })

        // Encode a value through encodeToBin, then decode via decodeFromBinLatest.
        let original = StubLatestValue()
        let encodeResult = facade.encodeToBin(BaboonCodecContext.compact, original)
        guard case .success(let bytes) = encodeResult else {
            XCTFail("encodeToBin failed: \(encodeResult)")
            return
        }
        let reader = BaboonBinReader(bytes)
        let decodeResult: Result<StubLatestValue, BaboonCodecException> = facade.decodeFromBinLatest(reader)
        switch decodeResult {
        case .success:
            break   // decoded successfully — type is correct by the generic constraint
        case .failure(let e):
            XCTFail("decodeFromBinLatest failed: \(e)")
        }
    }

    // Concrete BaboonGeneratedLatest used to witness the generic parameter in absent-envelope
    // and nil-input tests. Swift cannot bind `any BaboonGeneratedLatest` to a generic
    // `T: BaboonGeneratedLatest`; a concrete type is required at the call site.
    private final class _StubLatestForNilTests: BaboonGeneratedLatest, BaboonMetaProvider {
        var baboonDomainVersion: String { "stub.dom" }
        var baboonDomainIdentifier: String { "stub.dom" }
        var baboonTypeIdentifier: String { "StubT" }
        var baboonSameInVersions: [String] { ["1.0.0"] }
    }

    func testDecodeFromJsonLatest_absentEnvelope_returnsNil() throws {
        let facade = BaboonCodecsFacade()
        // A plain dictionary with no `$d`/`$v`/`$t` keys has no valid BaboonTypeMeta envelope.
        let result: _StubLatestForNilTests? = try facade.decodeFromJsonLatest([String: Any]())
        XCTAssertNil(result)
    }

    func testDecodeFromJsonLatest_nilInput_returnsNil() throws {
        let facade = BaboonCodecsFacade()
        let result: _StubLatestForNilTests? = try facade.decodeFromJsonLatest(Optional<Any>.none as Any)
        XCTAssertNil(result)
    }
}
