// BaboonVersion / BaboonDomainVersion / BaboonTypeMeta — the top-level codec envelope
// (docs/spec/codec-envelope.md) and its wire codec. Split out of `baboon_runtime.swift` to keep
// each embedded runtime file under the JVM's 64KB string-constant limit; same Swift module.

import Foundation

// --- Version / DomainVersion / TypeMeta ---
//
// PR-19-D01 lesson: regex literals in template files are read verbatim — but Swift runtime files
// in this project go through `processEscapes` (see PR-20-D01 sister-bug). We use only manual
// numeric parsing, no regex; safe regardless.

public struct BaboonVersion: Comparable, Hashable, CustomStringConvertible {
    public let major: Int
    public let minor: Int
    public let patch: Int

    public init(major: Int, minor: Int, patch: Int) {
        self.major = major
        self.minor = minor
        self.patch = patch
    }

    public static func from(_ version: String) throws -> BaboonVersion {
        let chunks = version.split(separator: ".", omittingEmptySubsequences: false)
        if chunks.count != 3 {
            throw BaboonException("Expected to have version in format x.y.z, got \(version)")
        }
        guard let major = Int(chunks[0].trimmingCharacters(in: .whitespaces)) else {
            throw BaboonException("Expected to have version in format x.y.z, got \(version). Invalid major value.")
        }
        guard let minor = Int(chunks[1].trimmingCharacters(in: .whitespaces)) else {
            throw BaboonException("Expected to have version in format x.y.z, got \(version). Invalid minor value.")
        }
        guard let patch = Int(chunks[2].trimmingCharacters(in: .whitespaces)) else {
            throw BaboonException("Expected to have version in format x.y.z, got \(version). Invalid patch value.")
        }
        return BaboonVersion(major: major, minor: minor, patch: patch)
    }

    public static func < (lhs: BaboonVersion, rhs: BaboonVersion) -> Bool {
        if lhs.major != rhs.major { return lhs.major < rhs.major }
        if lhs.minor != rhs.minor { return lhs.minor < rhs.minor }
        return lhs.patch < rhs.patch
    }

    public var description: String { return "\(major).\(minor).\(patch)" }
}

public struct BaboonException: Error, CustomStringConvertible {
    public let message: String
    public let cause: Error?
    public init(_ message: String, _ cause: Error? = nil) {
        self.message = message
        self.cause = cause
    }
    public var description: String { return "BaboonException: \(message)" }
}

public struct BaboonDomainVersion: Hashable, CustomStringConvertible {
    public let domainIdentifier: String
    public let domainVersion: String

    public init(_ domainIdentifier: String, _ domainVersion: String) {
        self.domainIdentifier = domainIdentifier
        self.domainVersion = domainVersion
    }

    public func version() throws -> BaboonVersion {
        return try BaboonVersion.from(domainVersion)
    }

    public var description: String { return "\(domainIdentifier):\(domainVersion)" }
}

// On-wire type meta envelope. Mirrors Dart/Kotlin `BaboonTypeMeta`.
public struct BaboonTypeMeta: Hashable, CustomStringConvertible {
    public let metaVersion: Int
    public let domainIdentifier: String
    public let domainVersion: String
    public let domainVersionMinCompat: String
    public let typeIdentifier: String
    /// Oldest domain version whose JSON codec can decode the payload under the json-additive
    /// contract (tolerant key lookup; fields unknown to that version are dropped). Always
    /// <= domainVersionMinCompat. Published as `$rv` when it differs from the (effective)
    /// minCompat; the binary v1 envelope does not carry it. Defaults to minCompat.
    public let domainVersionReadableMin: String

    /// Tier key of the JSON envelope's readable-min bound in `baboonMinReaderVersions`.
    public static let jsonReadableTier = "json-additive"
    /// Tier keys of the UEBA prefix bounds in `baboonMinReaderVersions`, per index mode.
    public static let uebaPrefixCompactTier = "prefix-compact"
    public static let uebaPrefixAnyModeTier = "prefix-any-mode"

    public init(
        _ metaVersion: Int,
        _ domainIdentifier: String,
        _ domainVersion: String,
        _ domainVersionMinCompat: String,
        _ typeIdentifier: String,
        _ domainVersionReadableMin: String? = nil
    ) {
        self.metaVersion = metaVersion
        self.domainIdentifier = domainIdentifier
        self.domainVersion = domainVersion
        self.domainVersionMinCompat = domainVersionMinCompat
        self.typeIdentifier = typeIdentifier
        self.domainVersionReadableMin = domainVersionReadableMin ?? domainVersionMinCompat
    }

    public func versionRef() -> BaboonDomainVersion {
        return BaboonDomainVersion(domainIdentifier, domainVersion)
    }

    public func versionReadableMin() -> BaboonDomainVersion? {
        if domainVersionReadableMin.isEmpty { return versionMinCompat() }
        if domainVersionReadableMin == domainVersion { return nil }
        return BaboonDomainVersion(domainIdentifier, domainVersionReadableMin)
    }

    public func versionMinCompat() -> BaboonDomainVersion? {
        if domainVersionMinCompat.isEmpty { return nil }
        if domainVersionMinCompat == domainVersion { return nil }
        return BaboonDomainVersion(domainIdentifier, domainVersionMinCompat)
    }

    public func writeBin(_ writer: BaboonBinWriter) {
        BaboonTypeMetaCodec.writeBin(self, writer)
    }

    public func writeJson() -> [String: Any] {
        return BaboonTypeMetaCodec.writeJson(self)
    }

    // MFACADE-PR-3: accept `$mv` as either a JSON number or a string (back-compat
    // with M28-vintage fixtures); both must equal `metaVersion`. Absent falls through.
    public static func readMetaJson(_ json: Any?) -> BaboonTypeMeta? {
        guard let obj = json as? [String: Any] else { return nil }
        if let mv = obj["$mv"] {
            // MFACADE-PR-3-D02: reject Bool explicitly — in Foundation Bool bridges to NSNumber,
            // so `true`/`false` would otherwise slip through the NSNumber branch with intValue 1/0.
            if mv is Bool { return nil }
            var mvInt: Int? = nil
            if let n = mv as? Int {
                mvInt = n
            } else if let n = mv as? NSNumber {
                // MFACADE-PR-7-D12: reject Float/Double-typed NSNumber. JSONSerialization
                // bridges JSON numbers as NSNumber whose `objCType` reflects the source literal:
                // `d` = Double, `f` = Float; integer types ('i'/'l'/'q'/'s'/'c'/etc.) otherwise.
                // Even whole-valued doubles like `1.0` are rejected because the source token
                // wasn't integer-typed. Decided per-PR-7 to be strict-everywhere about
                // numeric-type discrimination where parse-time preservation allows.
                let oct = String(cString: n.objCType)
                if oct == "d" || oct == "f" { return nil }
                mvInt = n.intValue
            } else if let s = mv as? String {
                mvInt = Int(s)
            }
            guard let n = mvInt, n == BaboonTypeMetaCodec.metaVersion else { return nil }
        }
        guard let d = obj["$d"] as? String else { return nil }
        guard let v = obj["$v"] as? String else { return nil }
        guard let t = obj["$t"] as? String else { return nil }
        let minCompat = (obj["$uv"] as? String) ?? v
        let readableMin = (obj["$rv"] as? String) ?? minCompat
        return BaboonTypeMeta(BaboonTypeMetaCodec.metaVersion, d, v, minCompat, t, readableMin)
    }

    public static func readMetaBin(_ reader: BaboonBinReader) throws -> BaboonTypeMeta? {
        return try BaboonTypeMetaCodec.readMeta(reader)
    }

    // Build a meta from a generated value. Optionally use the ADT type identifier when encoding
    // through an ADT-typed reference (PR-19-D02). Throws when the value does not conform to
    // [BaboonMetaProvider] — generated DTOs gain this conformance via the codegen's
    // automatic `: BaboonMetaProvider` clause (MFACADE-PR-E).
    public static func from(_ value: Any, useAdtIdentifier: Bool = false) throws -> BaboonTypeMeta {
        guard let meta = value as? BaboonMetaProvider else {
            throw BaboonException(
                "BaboonTypeMeta.from: value of type \(type(of: value)) does not conform to BaboonMetaProvider."
            )
        }
        let typeId: String
        if useAdtIdentifier, let adt = value as? BaboonAdtMember {
            typeId = adt.baboonAdtTypeIdentifier
        } else {
            typeId = meta.baboonTypeIdentifier
        }
        let sameIn = meta.baboonSameInVersions
        // PR-08-D02 fail-fast: a generator emitting an empty `sameInVersions` is a bug.
        if sameIn.isEmpty {
            throw BaboonException(
                "BaboonTypeMeta.from: empty baboonSameInVersions for type [\(meta.baboonDomainIdentifier).\(typeId)]"
            )
        }
        guard let readableMin = meta.baboonMinReaderVersions[BaboonTypeMeta.jsonReadableTier] else {
            throw BaboonException(
                "BaboonTypeMeta.from: baboonMinReaderVersions lacks \"\(BaboonTypeMeta.jsonReadableTier)\" for type [\(meta.baboonDomainIdentifier).\(typeId)]"
            )
        }
        return BaboonTypeMeta(
            BaboonTypeMetaCodec.metaVersion,
            meta.baboonDomainIdentifier,
            meta.baboonDomainVersion,
            sameIn[0],
            typeId,
            readableMin
        )
    }

    // Envelope for a UEBA payload written under `ctx`: `from` with `domainVersionMinCompat` lowered
    // to the prefix bound of the context's index mode when the writer policy is `.tolerant`, or with
    // both bounds under v2. Fails fast when the value lacks the tier.
    public static func forBin(_ value: Any, _ ctx: BaboonCodecContext, useAdtIdentifier: Bool = false) throws -> BaboonTypeMeta {
        let meta = try from(value, useAdtIdentifier: useAdtIdentifier)
        let v2 = ctx.envelopeVersion == .v2
        guard v2 || ctx.forwardWritePolicy == .tolerant, let provider = value as? BaboonMetaProvider else {
            return meta
        }
        let tier = ctx.useIndices ? BaboonTypeMeta.uebaPrefixAnyModeTier : BaboonTypeMeta.uebaPrefixCompactTier
        guard let bound = provider.baboonMinReaderVersions[tier] else {
            throw BaboonException(
                "BaboonTypeMeta.forBin: baboonMinReaderVersions lacks \"\(tier)\" for type [\(meta.domainIdentifier).\(meta.typeIdentifier)]"
            )
        }
        // v2 carries both bounds (the writer policy is irrelevant); v1 tolerant puts the prefix bound in its single slot
        return v2
            ? BaboonTypeMeta(BaboonTypeMetaCodec.metaVersion2, meta.domainIdentifier, meta.domainVersion, meta.domainVersionMinCompat, meta.typeIdentifier, bound)
            : BaboonTypeMeta(meta.metaVersion, meta.domainIdentifier, meta.domainVersion, bound, meta.typeIdentifier, meta.domainVersionReadableMin)
    }

    public var description: String {
        return "BaboonTypeMeta(\(domainIdentifier).\(typeIdentifier)@\(domainVersion))"
    }
}

public enum BaboonTypeMetaCodec {
    /// Layout written by default (binary) and always (JSON `$mv`).
    public static let metaVersion: Int = 1
    public static let metaVersion2: Int = 2

    // v2 flags byte (codec-envelope.md §2.1.3): bit 0 — minCompat follows; bit 1 — readableMin follows.
    private static let v2FlagMinCompat: UInt8 = 0x01
    private static let v2FlagReadableMin: UInt8 = 0x02
    private static let v2FlagsMask: UInt8 = v2FlagMinCompat | v2FlagReadableMin

    public static func writeBin(_ meta: BaboonTypeMeta, _ writer: BaboonBinWriter) {
        switch meta.metaVersion {
        case metaVersion: writeBinV1(meta, writer)
        case metaVersion2: writeBinV2(meta, writer)
        default: preconditionFailure("Unsupported binary envelope metaVersion \(meta.metaVersion)")
        }
    }

    // v2: `02 | domainId | domainVersion | flags | [minCompat] | [readableMin] | typeId`; each bound is
    // elided exactly as in JSON (minCompat when == domainVersion, readableMin when == effective minCompat)
    private static func writeBinV2(_ meta: BaboonTypeMeta, _ writer: BaboonBinWriter) {
        let minCompat = meta.domainVersionMinCompat.isEmpty ? meta.domainVersion : meta.domainVersionMinCompat
        let readableMin = meta.domainVersionReadableMin.isEmpty ? minCompat : meta.domainVersionReadableMin
        let hasMinCompat = minCompat != meta.domainVersion
        let hasReadableMin = readableMin != minCompat
        writer.writeU8(UInt8(metaVersion2))
        writer.writeString(meta.domainIdentifier)
        writer.writeString(meta.domainVersion)
        writer.writeU8((hasMinCompat ? v2FlagMinCompat : 0) | (hasReadableMin ? v2FlagReadableMin : 0))
        if hasMinCompat { writer.writeString(minCompat) }
        if hasReadableMin { writer.writeString(readableMin) }
        writer.writeString(meta.typeIdentifier)
    }

    private static func writeBinV1(_ meta: BaboonTypeMeta, _ writer: BaboonBinWriter) {
        writer.writeU8(UInt8(metaVersion))
        writer.writeString(meta.domainIdentifier)
        writer.writeString(meta.domainVersion)
        if meta.domainVersion == meta.domainVersionMinCompat {
            writer.writeU8(0)
        } else {
            writer.writeU8(1)
            writer.writeString(meta.domainVersionMinCompat)
        }
        writer.writeString(meta.typeIdentifier)
    }

    public static func readMeta(_ reader: BaboonBinReader) throws -> BaboonTypeMeta? {
        let v = Int(reader.readU8())
        if v == metaVersion { return try readMetaV1(reader) }
        if v == metaVersion2 { return try readMetaV2(reader) }
        return nil
    }

    private static func readMetaV2(_ reader: BaboonBinReader) throws -> BaboonTypeMeta? {
        let d = try reader.readString()
        let dv = try reader.readString()
        let flags = reader.readU8()
        // unknown flag bits are illegal; a lenient reader would misparse the strings that follow
        if (flags & ~v2FlagsMask) != 0 { return nil }
        let mc = (flags & v2FlagMinCompat) != 0 ? try reader.readString() : dv
        let rm = (flags & v2FlagReadableMin) != 0 ? try reader.readString() : mc
        let t = try reader.readString()
        return BaboonTypeMeta(metaVersion2, d, dv, mc, t, rm)
    }

    private static func readMetaV1(_ reader: BaboonBinReader) throws -> BaboonTypeMeta? {
        let d = try reader.readString()
        let dv = try reader.readString()
        let hasMinCompat = reader.readU8()
        // codec-envelope.md §2.1: only 0x00 (elided) and 0x01 (present) are legal; anything else is rejected
        if hasMinCompat != 0 && hasMinCompat != 1 { return nil }
        let mc = hasMinCompat == 1 ? try reader.readString() : dv
        let t = try reader.readString()
        return BaboonTypeMeta(metaVersion, d, dv, mc, t)
    }

    public static func writeJson(_ meta: BaboonTypeMeta) -> [String: Any] {
        // MFACADE-PR-3: always emit `$mv` as a JSON number so envelopes are
        // self-identifying without out-of-band knowledge (proposal §10.6 (a)).
        var obj: [String: Any] = [
            "$mv": metaVersion,
            "$d": meta.domainIdentifier,
            "$v": meta.domainVersion,
            "$t": meta.typeIdentifier,
        ]
        if meta.domainVersion != meta.domainVersionMinCompat {
            obj["$uv"] = meta.domainVersionMinCompat
        }
        // `$rv` is elided when it equals the effective `$uv`: unchanged types emit no new bytes
        if !meta.domainVersionReadableMin.isEmpty && meta.domainVersionReadableMin != meta.domainVersionMinCompat {
            obj["$rv"] = meta.domainVersionReadableMin
        }
        return obj
    }
}
