// `any`-feature surface types for Swift. Mirrors C# `AnyOpaque.cs`, Java `BaboonAnyOpaque.java`,
// Kotlin `BaboonAnyOpaque.kt`, TypeScript `BaboonAnyOpaque.ts`, Dart `baboon_any_opaque.dart`.
// Container holds:
//   * `AnyMeta`        — locked four-byte/six-kind meta envelope, with construction-time invariant checks
//   * `AnyOpaque`      — Swift `enum` with associated values for `any`-typed payloads
//   * `AnyMetaCodec`   — static helper for binary + JSON serialisation of the envelope
//
// Defects addressed:
//   PR-04-D01  reject reserved meta-kinds 0x04 / 0x05
//   PR-04-D02  `readJson` returns `Result<AnyMeta, BaboonCodecException>` (user-facing) while
//              `readBin` is throws-on-bad-wire (binary trusts the wire)
//   PR-05-D01  `readBinWithLength` reports bytes-consumed for forward-compat skip-trailer
//   PR-05-D08  `Data` `==` is content-wise in Swift; manual deep equality only needed for the
//              JSON variant — `[String: Any]` uses default Equatable
//   PR-12-D01  explicit u8 range check on kind so non-byte values can't slip through bit masks
//   PR-19-D01/PR-20-D01 escape gotcha: this file is emitted via `TextTree.verbatim(...)` (post-PR-9.1
//              switch, mirroring the Dart/TS resolution). `verbatim` skips `StringContext.processEscapes`
//              so file bytes pass through unchanged — Swift `\(...)` interpolation is single-backslash
//              in source. JSON keys (`$ak` etc.) are unaffected — Swift treats `$` as literal in strings.

import Foundation

// --- AnyMeta ---

// Locked four-byte / six-kind meta envelope for `any`-typed payloads.
//
// Invariants (enforced by the constructor):
//   - bit 2 (DOMAIN_BIT, 0x04) set <-> domain != nil
//   - bit 1 (VERSION_BIT, 0x02) set <-> version != nil
//   - bit 0 (TYPEID_BIT, 0x01) set <-> typeid != nil
//   - kind in {0x00, 0x01, 0x02, 0x03, 0x06, 0x07} — 0x04 / 0x05 reserved (PR-04-D01)
//
// Construction with a reserved or mismatched kind throws `BaboonCodecException.encoderFailure`.
public struct AnyMeta: Equatable, Hashable, CustomStringConvertible {
    public let kind: UInt8
    public let domain: String?
    public let version: String?
    public let typeid: String?

    public init(kind: UInt8, domain: String?, version: String?, typeid: String?) throws {
        // PR-12-D01 analog: Swift's UInt8 type already constrains range; kind cannot be < 0 or > 0xFF.
        // Bit-mask invariants below only require the four locked correspondences.
        let domainBitSet = (kind & AnyMetaCodec.DOMAIN_BIT) != 0
        if domainBitSet != (domain != nil) {
            throw BaboonCodecException.encoderFailure(
                "AnyMeta: domain presence (\(domain != nil)) does not match kind 0x\(String(kind, radix: 16)) bit 2",
                nil
            )
        }
        let versionBitSet = (kind & AnyMetaCodec.VERSION_BIT) != 0
        if versionBitSet != (version != nil) {
            throw BaboonCodecException.encoderFailure(
                "AnyMeta: version presence (\(version != nil)) does not match kind 0x\(String(kind, radix: 16)) bit 1",
                nil
            )
        }
        let typeidBitSet = (kind & AnyMetaCodec.TYPEID_BIT) != 0
        if typeidBitSet != (typeid != nil) {
            throw BaboonCodecException.encoderFailure(
                "AnyMeta: typeid presence (\(typeid != nil)) does not match kind 0x\(String(kind, radix: 16)) bit 0",
                nil
            )
        }
        if !AnyMetaCodec.VALID_KINDS.contains(kind) {
            throw BaboonCodecException.encoderFailure(
                "AnyMeta: reserved or invalid meta-kind byte: 0x\(String(format: "%02x", kind))",
                nil
            )
        }
        self.kind = kind
        self.domain = domain
        self.version = version
        self.typeid = typeid
    }

    public var description: String {
        return "AnyMeta(kind=0x\(String(kind, radix: 16)), domain=\(domain ?? "nil"), version=\(version ?? "nil"), typeid=\(typeid ?? "nil"))"
    }
}

// --- AnyOpaque ---

// Language-surface ADT for `any`-typed fields. Carries a meta envelope plus a payload in either
// binary (UEBA) or JSON form. Modelled as a Swift `enum` with associated values; `switch (x)`
// is exhaustive over `.ueba` / `.json` cases.
//
// PR-05-D08: `Data` content equality is the default in Swift (`Data` `==` is byte-wise). For the
// JSON variant, `[String: Any]` does not have a synthesised `Equatable` (heterogeneous values),
// so we override `==` to use `baboonDeepEquals`. Marked `indirect` because `AnyOpaqueJson`'s
// payload may itself contain nested `AnyOpaque` references in cross-language scenarios.
public indirect enum AnyOpaque: Equatable, Hashable, CustomStringConvertible {
    case ueba(meta: AnyMeta, bytes: Data)
    case json(meta: AnyMeta, json: Any?)

    public var meta: AnyMeta {
        switch self {
        case .ueba(let m, _): return m
        case .json(let m, _): return m
        }
    }

    public static func == (lhs: AnyOpaque, rhs: AnyOpaque) -> Bool {
        switch (lhs, rhs) {
        case (.ueba(let m1, let b1), .ueba(let m2, let b2)):
            return m1 == m2 && b1 == b2
        case (.json(let m1, let j1), .json(let m2, let j2)):
            return m1 == m2 && baboonDeepEquals(j1, j2)
        default:
            return false
        }
    }

    // Hand-rolled `hash(into:)` because the `.json` case carries `Any?` which has no synthesised
    // Hashable. Generated DTOs require `Hashable` conformance — adding it here lets DTOs that
    // include `any` fields still synthesise their own conformance. Mirrors Java's hand-rolled
    // hashCode in `BaboonAnyOpaque.java` and Dart's custom hashCode in `baboon_any_opaque.dart`.
    public func hash(into hasher: inout Hasher) {
        switch self {
        case .ueba(let m, let b):
            hasher.combine(0)
            hasher.combine(m)
            hasher.combine(b)
        case .json(let m, let j):
            hasher.combine(1)
            hasher.combine(m)
            hasher.combine(baboonDeepHashCode(j))
        }
    }

    public var description: String {
        switch self {
        case .ueba(let m, let b):
            return "AnyOpaqueUeba(meta=\(m), bytes=Data(\(b.count)))"
        case .json(let m, let j):
            return "AnyOpaqueJson(meta=\(m), json=\(String(describing: j)))"
        }
    }
}

// --- AnyMetaCodec ---

// Static helper for `AnyMeta` binary + JSON serialisation. Members keep PR-locked names: bit
// masks (`DOMAIN_BIT` / `VERSION_BIT` / `TYPEID_BIT`) and JSON envelope keys (`$ak` / `$ad` /
// `$av` / `$at` / `$c`) match every other runtime.
//
// `readBin` / `writeBin` trust the wire and throw on bad input. `readJson` returns
// `Result<AnyMeta, BaboonCodecException>` per PR-04-D02 — JSON decode is user-facing
// and threads errors instead of throwing.
public enum AnyMetaCodec {
    public static let DOMAIN_BIT: UInt8 = 0x04
    public static let VERSION_BIT: UInt8 = 0x02
    public static let TYPEID_BIT: UInt8 = 0x01

    public static let ANY_KIND_KEY: String = "$ak"
    public static let ANY_DOMAIN_KEY: String = "$ad"
    public static let ANY_VERSION_KEY: String = "$av"
    public static let ANY_TYPEID_KEY: String = "$at"
    public static let ANY_CONTENT_KEY: String = "$c"

    public static let VALID_KINDS: Set<UInt8> = [0x00, 0x01, 0x02, 0x03, 0x06, 0x07]

    public static func writeBin(_ meta: AnyMeta, _ writer: BaboonBinWriter) {
        writer.writeU8(meta.kind)
        if let d = meta.domain { writer.writeString(d) }
        if let v = meta.version { writer.writeString(v) }
        if let t = meta.typeid { writer.writeString(t) }
    }

    public static func readBin(_ reader: BaboonBinReader) throws -> AnyMeta {
        let kind = reader.readU8()
        let domain = (kind & DOMAIN_BIT) != 0 ? try reader.readString() : nil
        let version = (kind & VERSION_BIT) != 0 ? try reader.readString() : nil
        let typeid = (kind & TYPEID_BIT) != 0 ? try reader.readString() : nil
        return try AnyMeta(kind: kind, domain: domain, version: version, typeid: typeid)
    }

    // PR-05-D01: read meta and report bytes consumed. Callers that know the on-wire `meta-length`
    // window can skip any trailing bytes left in it — that's how the wire format keeps forward-
    // compat with future meta extensions. We compute the byte-count via reader-position snapshot.
    public static func readBinWithLength(_ reader: BaboonBinReader) throws -> (AnyMeta, Int) {
        let before = reader.position
        let meta = try readBin(reader)
        let after = reader.position
        return (meta, after - before)
    }

    // Always returns a plain `[String: Any]` (never null/list). The JSON encoder envelope build
    // relies on this invariant — adding `$c` content into a non-object would silently lose
    // the key (PR-08-D06 analog).
    public static func writeJson(_ meta: AnyMeta) -> [String: Any] {
        var obj: [String: Any] = [:]
        obj[ANY_KIND_KEY] = Int(meta.kind & 0xFF)
        if let d = meta.domain { obj[ANY_DOMAIN_KEY] = d }
        if let v = meta.version { obj[ANY_VERSION_KEY] = v }
        if let t = meta.typeid { obj[ANY_TYPEID_KEY] = t }
        return obj
    }

    public static func readJson(_ json: Any?) -> Result<AnyMeta, BaboonCodecException> {
        guard let obj = json as? [String: Any] else {
            return .failure(.decoderFailure(
                "AnyMetaCodec.readJson: expected JSON object, got \(json.map { String(describing: type(of: $0)) } ?? "nil")",
                nil
            ))
        }

        guard let kindNode = obj[ANY_KIND_KEY] else {
            return .failure(.decoderFailure(
                "AnyMetaCodec.readJson: missing or non-numeric '\(ANY_KIND_KEY)' field",
                nil
            ))
        }
        let kindInt: Int
        if let n = kindNode as? Int {
            kindInt = n
        } else if let n = kindNode as? NSNumber {
            // JSONSerialization decodes numbers to NSNumber — accept as Int when integral.
            kindInt = n.intValue
        } else {
            return .failure(.decoderFailure(
                "AnyMetaCodec.readJson: missing or non-numeric '\(ANY_KIND_KEY)' field",
                nil
            ))
        }
        let kind = UInt8(kindInt & 0xFF)

        let domainResult = readOptString(obj, ANY_DOMAIN_KEY, kind, DOMAIN_BIT, "domain")
        let domain: String?
        switch domainResult {
        case .failure(let e): return .failure(e)
        case .success(let v): domain = v
        }

        let versionResult = readOptString(obj, ANY_VERSION_KEY, kind, VERSION_BIT, "version")
        let version: String?
        switch versionResult {
        case .failure(let e): return .failure(e)
        case .success(let v): version = v
        }

        let typeidResult = readOptString(obj, ANY_TYPEID_KEY, kind, TYPEID_BIT, "typeid")
        let typeid: String?
        switch typeidResult {
        case .failure(let e): return .failure(e)
        case .success(let v): typeid = v
        }

        do {
            let meta = try AnyMeta(kind: kind, domain: domain, version: version, typeid: typeid)
            return .success(meta)
        } catch let e as BaboonCodecException {
            return .failure(.decoderFailure("AnyMetaCodec.readJson: invalid meta: \(e.message)", e))
        } catch {
            return .failure(.decoderFailure("AnyMetaCodec.readJson: invalid meta: \(error)", error))
        }
    }

    private static func readOptString(
        _ obj: [String: Any],
        _ key: String,
        _ kind: UInt8,
        _ bit: UInt8,
        _ name: String
    ) -> Result<String?, BaboonCodecException> {
        let present = (kind & bit) != 0
        let value = obj[key] as? String

        if present && value != nil { return .success(value) }
        if !present && value == nil { return .success(nil) }
        if present {
            return .failure(.decoderFailure(
                "AnyMetaCodec.readJson: kind 0x\(String(kind, radix: 16)) requires '\(key)' (\(name)) but it is missing",
                nil
            ))
        }
        return .failure(.decoderFailure(
            "AnyMetaCodec.readJson: kind 0x\(String(kind, radix: 16)) forbids '\(key)' (\(name)) but it is present",
            nil
        ))
    }
}
