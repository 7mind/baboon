// PR 9.4 (issue #69 phase 9.4 / closes M9): hand-written round-trip and cross-format tests for
// `any` fields. Mirrors Scala's AnyRoundTripSpec (PR 2.4) / C# (PR 3.4) / Rust (PR 4.3) /
// Kotlin (PR 5.4) / Java (PR 6.4) / TypeScript (PR 7.4) / Dart (PR 8.4). Exercises the `any-ok`
// fixture's six DSL variants:
//   A=any                        → kind 0x07
//   B=any[domain:this]           → kind 0x03
//   C=any[domain:current]        → kind 0x01
//   D1=any[Inner]                → kind 0x06
//   D2=any[domain:this, Inner]   → kind 0x02
//   D3=any[domain:current, Inner]→ kind 0x00
// plus the three nested positions (opt/lst/map-value).
//
// NOTE: This test references generated MyOk symbols (`Holder`, `Inner`, `Holder_UebaCodec`, ...)
// produced into this stub only when codegen runs against the full model set including `any-ok/`.
// To run this test against codegen output, the consumer must:
//   1. Run baboon codegen with `any-ok/` present in the model dir, populating `Sources/MyOk/`.
//   2. Add a `MyOk` target to Package.swift (path `Sources/MyOk`, deps `BaboonRuntime`) and add
//      `MyOk` to the BaboonTests test target deps. The source-tree Package.swift omits `MyOk`
//      because baboon's codegen `cleanupTargetPaths` wipes Sources/ before each run, so an empty
//      `MyOk/` placeholder cannot survive the standard `:test-gen-regular-adt` flow when `any-ok/`
//      is moved aside (the PR 9.2/9.3 baseline workflow per ledger).
//   3. XCTest must be available (works on macOS Xcode and Swift Linux with corelibs-xctest).
// Sandbox baseline (PR 9.1/9.2/9.3): XCTest module is missing under nix bwrap on Linux Swift
// 5.10.1, so end-to-end `swift test` cannot run here — the file is shape-checked by the file
// editor and the codegen-only workflow is verified to emit the matching fixture branches.
//
// Coverage gap vs Java/Kotlin/TS/Dart: Swift's generated codec helpers use `preconditionFailure`
// (a runtime trap) for missing-facade / kind-mismatch / facade-returned-failure rather than a
// catchable `BaboonEncoderFailure`. This is a deliberate Swift-codegen design choice (PR 9.2/9.3
// ledger entries: `BaboonBinCodecBase.encode` and `BaboonJsonCodecBase.encode` are non-throwing
// per PR 9.1's locked contract, so the helpers cannot `throw`). Consequently the "fail-fast
// missing-facade" tests that other languages run as `assertThrows`/`expect(...).throwsA(...)`
// have no XCTest equivalent here — Swift `preconditionFailure` cannot be caught by an XCTest.
// Documented in PR 9.4 ledger; the remaining 12 round-trip + cross-format tests cover the
// meaningful behaviors.

import XCTest
import Foundation
@testable import BaboonRuntime
@testable import MyOk

// ===== Test scaffolding ========================================================================

private let DOMAIN_ID: String = "my.ok"
private let VERSION_STR: String = "1.0.0"
private let INNER_TYPE: String = "my.ok/:#Inner"

// Per-variant `AnyMeta` builders. Each returns a meta envelope populating only the bits the
// kind byte claims. `try!` is safe because the constructor only throws on invariant violations
// (kind/bit-mask mismatch, reserved kind 0x04/0x05); the literals below are spec-locked.
private func metaA(_ typeid: String = "opaque.Type") -> AnyMeta {
    return try! AnyMeta(kind: 0x07, domain: DOMAIN_ID, version: VERSION_STR, typeid: typeid)
}
private func metaB(_ typeid: String = "opaque.Type") -> AnyMeta {
    return try! AnyMeta(kind: 0x03, domain: nil, version: VERSION_STR, typeid: typeid)
}
private func metaC(_ typeid: String = "opaque.Type") -> AnyMeta {
    return try! AnyMeta(kind: 0x01, domain: nil, version: nil, typeid: typeid)
}
private func metaD1() -> AnyMeta {
    return try! AnyMeta(kind: 0x06, domain: DOMAIN_ID, version: VERSION_STR, typeid: nil)
}
private func metaD2() -> AnyMeta {
    return try! AnyMeta(kind: 0x02, domain: nil, version: VERSION_STR, typeid: nil)
}
private func metaD3() -> AnyMeta {
    return try! AnyMeta(kind: 0x00, domain: nil, version: nil, typeid: nil)
}

private let SAMPLE_INNER: Inner = Inner(x: 42)

// Encode an Inner via the generated UEBA codec — used when constructing `AnyOpaque.ueba` payloads
// so cross-convert tests have a real Inner to deserialize.
private func innerToUebaBytes(_ inner: Inner) -> Data {
    let writer = BaboonBinWriter()
    Inner_UebaCodec.instance.encode(BaboonCodecContext.compact, writer, inner)
    return writer.toData()
}

// JSON-encode an Inner via the generated codec — produces `Any` (concretely `[String: Any]`).
private func innerToJson(_ inner: Inner) -> Any {
    return Inner_JsonCodec.instance.encode(BaboonCodecContext.compact, inner)
}

// Per-test BaboonMeta minimal stub. Conforms to `BaboonMeta` so the facade's max-compat lookup
// can resolve `sameInVersions(typeId)`. Generated `BaboonMetadata_My_Ok` does NOT conform to
// the `BaboonMeta` protocol (PR 9.1 staged rollout — the protocol's static-side `baboonDomainVersion`
// /`baboonDomainIdentifier`/`baboonTypeIdentifier` make it awkward as a registry-style class), so
// we wrap with a compact local stub.
private final class MyOkMeta: BaboonMeta {
    static let baboonDomainVersion: String = VERSION_STR
    static let baboonDomainIdentifier: String = DOMAIN_ID
    static let baboonTypeIdentifier: String = ""
    func sameInVersions(_ typeId: String) -> [String] { return [VERSION_STR] }
}

// Fresh per-test facade: registers Holder/Inner UEBA + JSON codecs from the my.ok domain so the
// cross-format helpers and `decodeAny` can resolve `(domain, version, typeid)` triples.
// Uses the generated `BaboonCodecsUeba_My_Ok` / `BaboonCodecsJson_My_Ok` registry classes
// (which already register `Inner_UebaCodec.instance` / `Inner_JsonCodec.instance` /
// `Holder_UebaCodec.instance` / `Holder_JsonCodec.instance` under the matching type identifiers).
private func freshFacade() -> BaboonCodecsFacade {
    let facade = BaboonCodecsFacade()
    let dv = BaboonDomainVersion(DOMAIN_ID, VERSION_STR)
    facade.register(
        dv,
        codecsJson: { BaboonCodecsJson_My_Ok() },
        codecsBin: { BaboonCodecsUeba_My_Ok() },
        conversions: { BaboonConversions_My_Ok() },
        meta: { MyOkMeta() }
    )
    return facade
}

// Build a complete Holder with one `AnyOpaque.ueba` per variant. UEBA round-trips natively
// (no facade needed for encode/decode — facade is only consulted for cross-convert).
private func buildUebaHolder() -> Holder {
    let innerBytes = innerToUebaBytes(SAMPLE_INNER)
    return Holder(
        fAny: AnyOpaque.ueba(meta: metaA(), bytes: Data([0x01, 0x02, 0x03])),
        fDomainThis: AnyOpaque.ueba(meta: metaB(), bytes: Data([0x04, 0x05])),
        fDomainCurrent: AnyOpaque.ueba(meta: metaC(), bytes: Data([0x06])),
        fUnderlying: AnyOpaque.ueba(meta: metaD1(), bytes: innerBytes),
        fThisUnderlying: AnyOpaque.ueba(meta: metaD2(), bytes: innerBytes),
        fCurrentUnderlying: AnyOpaque.ueba(meta: metaD3(), bytes: innerBytes),
        fOpt: AnyOpaque.ueba(meta: metaA(), bytes: Data([0x07])),
        fLst: [AnyOpaque.ueba(meta: metaD1(), bytes: innerBytes)],
        fMapValue: ["k1": AnyOpaque.ueba(meta: metaA(), bytes: Data([0x08]))]
    )
}

// Build a Holder using `AnyOpaque.json` branches everywhere with arbitrary inner JSON content.
// Used as the "all native JSON branch" baseline for JSON round-trip tests.
private func buildJsonNativeHolder() -> Holder {
    let arbitrary: [String: Any] = ["payload": 42]
    let innerJson = innerToJson(SAMPLE_INNER)
    return Holder(
        fAny: AnyOpaque.json(meta: metaA(), json: arbitrary),
        fDomainThis: AnyOpaque.json(meta: metaB(), json: arbitrary),
        fDomainCurrent: AnyOpaque.json(meta: metaC(), json: arbitrary),
        fUnderlying: AnyOpaque.json(meta: metaD1(), json: innerJson),
        fThisUnderlying: AnyOpaque.json(meta: metaD2(), json: innerJson),
        fCurrentUnderlying: AnyOpaque.json(meta: metaD3(), json: innerJson),
        fOpt: AnyOpaque.json(meta: metaA(), json: arbitrary),
        fLst: [AnyOpaque.json(meta: metaD1(), json: innerJson)],
        fMapValue: ["k1": AnyOpaque.json(meta: metaA(), json: arbitrary)]
    )
}

// Build a Holder using `AnyOpaque.json` branches with REAL Inner JSON for D variants and
// typeid=INNER_TYPE for A/B/C so cross-convert can resolve Inner via the registered facade.
private func buildJsonHolderForCrossConvert() -> Holder {
    let innerJson = innerToJson(SAMPLE_INNER)
    return Holder(
        fAny: AnyOpaque.json(meta: metaA(INNER_TYPE), json: innerJson),
        fDomainThis: AnyOpaque.json(meta: metaB(INNER_TYPE), json: innerJson),
        fDomainCurrent: AnyOpaque.json(meta: metaC(INNER_TYPE), json: innerJson),
        fUnderlying: AnyOpaque.json(meta: metaD1(), json: innerJson),
        fThisUnderlying: AnyOpaque.json(meta: metaD2(), json: innerJson),
        fCurrentUnderlying: AnyOpaque.json(meta: metaD3(), json: innerJson),
        fOpt: AnyOpaque.json(meta: metaA(INNER_TYPE), json: innerJson),
        fLst: [AnyOpaque.json(meta: metaD1(), json: innerJson)],
        fMapValue: ["k1": AnyOpaque.json(meta: metaA(INNER_TYPE), json: innerJson)]
    )
}

private func encodeUebaBytes(_ value: Holder, _ ctx: BaboonCodecContext) -> Data {
    let writer = BaboonBinWriter()
    Holder_UebaCodec.instance.encode(ctx, writer, value)
    return writer.toData()
}

private func decodeUebaBytes(_ bytes: Data, _ ctx: BaboonCodecContext = BaboonCodecContext.compact) throws -> Holder {
    return try Holder_UebaCodec.instance.decode(ctx, BaboonBinReader(bytes))
}

private func readI32Le(_ data: Data, _ offset: Int) -> Int32 {
    let lo = Int32(data[offset]) & 0xFF
    let b1 = (Int32(data[offset + 1]) & 0xFF) << 8
    let b2 = (Int32(data[offset + 2]) & 0xFF) << 16
    let b3 = (Int32(data[offset + 3]) & 0xFF) << 24
    return lo | b1 | b2 | b3
}

// ===== Tests ===================================================================================

final class AnyRoundTripTests: XCTestCase {

    // ----- 1. Per-variant UEBA round-trip ----------------------------------------------------

    func testUebaCompactRoundTrip_preservesAllSixVariantsPlusNestedPositions() throws {
        let original = buildUebaHolder()
        let bytes = encodeUebaBytes(original, BaboonCodecContext.compact)
        let decoded = try decodeUebaBytes(bytes, BaboonCodecContext.compact)
        XCTAssertEqual(decoded, original)
    }

    func testUebaIndexedRoundTrip_preservesContent() throws {
        // Mirrors Java/Kotlin/Rust/TS/Dart per-variant indexed-mode coverage. Swift's
        // `Holder_UebaCodec` emits a header byte 0x01 then per-field (offset, length) i32 prefixes
        // followed by the contiguous payload buffer; decode reads the prefix block then linearly
        // decodes the data. PR-15-D01 (Kotlin trap) does NOT affect Swift.
        let original = buildUebaHolder()
        let bytes = encodeUebaBytes(original, BaboonCodecContext.indexed)
        let decoded = try decodeUebaBytes(bytes, BaboonCodecContext.indexed)
        XCTAssertEqual(decoded, original)
    }

    func testUebaDecode_yieldsUebaBranchWithMatchingKindBytes() throws {
        let original = buildUebaHolder()
        let bytes = encodeUebaBytes(original, BaboonCodecContext.compact)
        let decoded = try decodeUebaBytes(bytes)
        if case .ueba = decoded.fAny {} else { XCTFail("Expected fAny to be .ueba") }
        XCTAssertEqual(decoded.fAny.meta.kind, 0x07)
        XCTAssertEqual(decoded.fDomainThis.meta.kind, 0x03)
        XCTAssertEqual(decoded.fDomainCurrent.meta.kind, 0x01)
        XCTAssertEqual(decoded.fUnderlying.meta.kind, 0x06)
        XCTAssertEqual(decoded.fThisUnderlying.meta.kind, 0x02)
        XCTAssertEqual(decoded.fCurrentUnderlying.meta.kind, 0x00)
    }

    // ----- 2. Per-variant JSON round-trip ----------------------------------------------------

    func testJsonRoundTrip_preservesAllSixVariantsPlusNestedPositions() throws {
        let original = buildJsonNativeHolder()
        let json = Holder_JsonCodec.instance.encode(BaboonCodecContext.compact, original)
        let decoded = try Holder_JsonCodec.instance.decode(BaboonCodecContext.compact, json)
        XCTAssertEqual(decoded, original)
    }

    func testJsonDecode_yieldsJsonBranchWithMatchingKindBytes() throws {
        let original = buildJsonNativeHolder()
        let json = Holder_JsonCodec.instance.encode(BaboonCodecContext.compact, original)
        let decoded = try Holder_JsonCodec.instance.decode(BaboonCodecContext.compact, json)
        if case .json = decoded.fAny {} else { XCTFail("Expected fAny to be .json") }
        XCTAssertEqual(decoded.fAny.meta.kind, 0x07)
        XCTAssertEqual(decoded.fDomainThis.meta.kind, 0x03)
        XCTAssertEqual(decoded.fDomainCurrent.meta.kind, 0x01)
        XCTAssertEqual(decoded.fUnderlying.meta.kind, 0x06)
        XCTAssertEqual(decoded.fThisUnderlying.meta.kind, 0x02)
        XCTAssertEqual(decoded.fCurrentUnderlying.meta.kind, 0x00)
    }

    // ----- 3. Cross-format conversion via facade ---------------------------------------------

    func testCrossFormat_jsonHolderEncodesToUebaBytewiseCanonical() throws {
        // `buildJsonHolderForCrossConvert` uses `AnyOpaque.json` branches for all fields with real
        // Inner JSON; encoding to UEBA forces `jsonToUebaBytes` per field. After decode the
        // branches are `AnyOpaque.ueba`. Re-encoding the now-UEBA-branched holder (no facade) must
        // produce identical bytes — proves the cross-converted bytes match a native UEBA encode of
        // the same value.
        let facade = freshFacade()
        let ctxWithFacade = BaboonCodecContext.withFacade(false, facade)
        let original = buildJsonHolderForCrossConvert()
        let bytes = encodeUebaBytes(original, ctxWithFacade)
        let decoded = try decodeUebaBytes(bytes)

        let rebytes = encodeUebaBytes(decoded, BaboonCodecContext.compact)
        XCTAssertEqual(bytes, rebytes)
    }

    func testCrossFormat_uebaHolderEncodesToJsonEnvelope_variantKindsPreserved() throws {
        // `buildUebaHolder` uses `AnyOpaque.ueba` branches everywhere; encoding to JSON triggers
        // `uebaToJson` for each field. For untyped variants A/B/C the wire meta carries typeid;
        // we substitute typeid=INNER_TYPE so the registered Inner codec resolves and the bytes
        // deserialize as Inner. D variants resolve via static fallbacks emitted by codec gen.
        let facade = freshFacade()
        let ctxWithFacade = BaboonCodecContext.withFacade(false, facade)
        let innerBytes = innerToUebaBytes(SAMPLE_INNER)
        let base = buildUebaHolder()
        let crossable = Holder(
            fAny: AnyOpaque.ueba(meta: metaA(INNER_TYPE), bytes: innerBytes),
            fDomainThis: AnyOpaque.ueba(meta: metaB(INNER_TYPE), bytes: innerBytes),
            fDomainCurrent: AnyOpaque.ueba(meta: metaC(INNER_TYPE), bytes: innerBytes),
            fUnderlying: base.fUnderlying,
            fThisUnderlying: base.fThisUnderlying,
            fCurrentUnderlying: base.fCurrentUnderlying,
            fOpt: AnyOpaque.ueba(meta: metaA(INNER_TYPE), bytes: innerBytes),
            fLst: base.fLst,
            fMapValue: ["k1": AnyOpaque.ueba(meta: metaA(INNER_TYPE), bytes: innerBytes)]
        )
        let json = Holder_JsonCodec.instance.encode(ctxWithFacade, crossable)
        // Sanity-decode the JSON to ensure the envelope is well-formed.
        let decoded = try Holder_JsonCodec.instance.decode(BaboonCodecContext.compact, json)
        if case .json = decoded.fAny {} else { XCTFail("Expected fAny to be .json after uebaToJson cross-encode") }
        XCTAssertEqual(decoded.fAny.meta.kind, 0x07)
        XCTAssertEqual(decoded.fUnderlying.meta.kind, 0x06)
        // D3: kind=0x00, statics filled
        XCTAssertEqual(decoded.fCurrentUnderlying.meta.kind, 0x00)
    }

    func testCrossFormat_d3IsolatedField_resolvesViaStaticFallbacks() throws {
        // PR-06-D01 (Swift analog) regression: D3 has all-nil meta on wire; the codec generator
        // emits (currentDomain, currentVersion, underlyingFqid) as static fallbacks. Without these
        // the facade cannot resolve and cross-convert fails. PR 9.2/9.3 emit the fallback table
        // for all 6 variants — this test exercises the D3 path end-to-end.
        let facade = freshFacade()
        let ctxWithFacade = BaboonCodecContext.withFacade(false, facade)
        let innerJson = innerToJson(SAMPLE_INNER)
        let base = buildUebaHolder()
        let mixed = Holder(
            fAny: base.fAny,
            fDomainThis: base.fDomainThis,
            fDomainCurrent: base.fDomainCurrent,
            fUnderlying: base.fUnderlying,
            fThisUnderlying: base.fThisUnderlying,
            fCurrentUnderlying: AnyOpaque.json(meta: metaD3(), json: innerJson),
            fOpt: base.fOpt,
            fLst: base.fLst,
            fMapValue: base.fMapValue
        )
        // No throw on encode means jsonToUebaBytes succeeded for the D3 field (statics resolved).
        let bytes = encodeUebaBytes(mixed, ctxWithFacade)
        let decoded = try decodeUebaBytes(bytes)
        XCTAssertEqual(decoded.fCurrentUnderlying.meta.kind, 0x00)
        if case .ueba(_, let blob) = decoded.fCurrentUnderlying {
            let inner = try Inner_UebaCodec.instance.decode(BaboonCodecContext.compact, BaboonBinReader(blob))
            XCTAssertEqual(inner, SAMPLE_INNER)
        } else {
            XCTFail("Expected .ueba branch after JSON→UEBA cross-convert")
        }
    }

    // ----- 4. facade.decodeAny end-to-end ----------------------------------------------------

    func testFacadeDecodeAny_resolvesUebaInnerPayloadToTypedInner() throws {
        let facade = freshFacade()
        let meta = try AnyMeta(kind: 0x07, domain: DOMAIN_ID, version: VERSION_STR, typeid: INNER_TYPE)
        let opaque = AnyOpaque.ueba(meta: meta, bytes: innerToUebaBytes(SAMPLE_INNER))
        let result = facade.decodeAny(opaque)
        switch result {
        case .success(let v):
            guard let typed = v as? Inner else { XCTFail("Expected Inner, got \(type(of: v))"); return }
            XCTAssertEqual(typed, SAMPLE_INNER)
        case .failure(let e):
            XCTFail("Expected success, got \(e)")
        }
    }

    func testFacadeDecodeAny_resolvesJsonInnerPayloadToTypedInner() throws {
        let facade = freshFacade()
        let meta = try AnyMeta(kind: 0x07, domain: DOMAIN_ID, version: VERSION_STR, typeid: INNER_TYPE)
        let opaque = AnyOpaque.json(meta: meta, json: innerToJson(SAMPLE_INNER))
        let result = facade.decodeAny(opaque)
        switch result {
        case .success(let v):
            guard let typed = v as? Inner else { XCTFail("Expected Inner, got \(type(of: v))"); return }
            XCTAssertEqual(typed, SAMPLE_INNER)
        case .failure(let e):
            XCTFail("Expected success, got \(e)")
        }
    }

    // ----- 5. Forward-compat: trailing meta-extension bytes inside meta-length window --------

    func testForwardCompat_extraMetaExtensionBytesAreSkippedOnUebaDecode() throws {
        // Encode a Holder normally, then surgically patch the FIRST any-field's meta-length to
        // claim 5 extra bytes, and splice 5 bytes into the meta block. The decoder must consume
        // the meta, observe the gap (anyMetaLen - bytesRead), skip them, and continue parsing.
        // Layout of the first any-field on the wire (Compact, useIndices=false):
        //   [1 byte header][i32 anyLength][i32 anyMetaLen][... metaBytes ...][... blob ...]
        let original = buildUebaHolder()
        let bytes = encodeUebaBytes(original, BaboonCodecContext.compact)

        let headerLen = 1
        let anyLengthOffset = headerLen
        let anyMetaLenOffset = headerLen + 4
        let anyMetaStartOffset = headerLen + 4 + 4
        let origAnyLength = readI32Le(bytes, anyLengthOffset)
        let origAnyMetaLen = readI32Le(bytes, anyMetaLenOffset)

        let extension_: [UInt8] = [0x11, 0x22, 0x33, 0x44, 0x55]
        let newAnyMetaLen = origAnyMetaLen + Int32(extension_.count)
        let newAnyLength = origAnyLength + Int32(extension_.count)

        let origMetaSlice = bytes.subdata(in: anyMetaStartOffset..<(anyMetaStartOffset + Int(origAnyMetaLen)))
        let origBlobAndRest = bytes.subdata(in: (anyMetaStartOffset + Int(origAnyMetaLen))..<bytes.count)

        let patched = BaboonBinWriter()
        patched.writeU8(bytes[0])
        patched.writeI32(newAnyLength)
        patched.writeI32(newAnyMetaLen)
        patched.writeAll(origMetaSlice)
        patched.writeAll(Data(extension_))
        patched.writeAll(origBlobAndRest)

        let decoded = try decodeUebaBytes(patched.toData())
        XCTAssertEqual(decoded, original)
    }

    // ----- 6. JSON envelope shape lock-in ----------------------------------------------------

    func testJsonEnvelopeShape_carriesAkAdAvAtAndCcKeys() throws {
        // Sanity: the JSON envelope produced by the codec embeds the AnyMeta keys ($ak, $ad?,
        // $av?, $at?) alongside the $c content key. Any change to the envelope that drops one
        // of these would break cross-language interop.
        let original = buildJsonNativeHolder()
        let token = Holder_JsonCodec.instance.encode(BaboonCodecContext.compact, original) as! [String: Any]

        // fAny variant A → all four meta keys + $c present.
        let anyField = token["fAny"] as! [String: Any]
        XCTAssertEqual((anyField[AnyMetaCodec.ANY_KIND_KEY] as? Int) ?? -1, 0x07)
        XCTAssertTrue(anyField.keys.contains(AnyMetaCodec.ANY_DOMAIN_KEY))
        XCTAssertTrue(anyField.keys.contains(AnyMetaCodec.ANY_VERSION_KEY))
        XCTAssertTrue(anyField.keys.contains(AnyMetaCodec.ANY_TYPEID_KEY))
        XCTAssertTrue(anyField.keys.contains(AnyMetaCodec.ANY_CONTENT_KEY))

        // fCurrentUnderlying variant D3 → only $ak + $c (kind 0x00, no other meta on wire).
        let d3 = token["fCurrentUnderlying"] as! [String: Any]
        XCTAssertEqual((d3[AnyMetaCodec.ANY_KIND_KEY] as? Int) ?? -1, 0x00)
        XCTAssertTrue(d3.keys.contains(AnyMetaCodec.ANY_CONTENT_KEY))
        XCTAssertFalse(d3.keys.contains(AnyMetaCodec.ANY_DOMAIN_KEY))
        XCTAssertFalse(d3.keys.contains(AnyMetaCodec.ANY_VERSION_KEY))
        XCTAssertFalse(d3.keys.contains(AnyMetaCodec.ANY_TYPEID_KEY))

        // Sanity: the variant-A envelope keys appear exactly as documented (regression-proof key
        // list). The $c content carries the JSON-branch payload (a `[String: Any]` here).
        let expected: Set<String> = [
            AnyMetaCodec.ANY_KIND_KEY,
            AnyMetaCodec.ANY_DOMAIN_KEY,
            AnyMetaCodec.ANY_VERSION_KEY,
            AnyMetaCodec.ANY_TYPEID_KEY,
            AnyMetaCodec.ANY_CONTENT_KEY,
        ]
        XCTAssertEqual(Set(anyField.keys), expected)
    }
}
