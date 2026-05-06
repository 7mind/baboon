import Foundation

private func baboonEpochMillis(_ date: Date) -> Int64 {
    return Int64((date.timeIntervalSince1970 * 1000.0).rounded())
}

// --- Codec Errors ---

public enum BaboonCodecError: Error {
    case invalidInput(String)
    case truncated(String)
    case invalidUtf8
    case invalidUuid
}

// --- Metadata Interfaces ---

public protocol BaboonGenerated {}

public protocol BaboonGeneratedLatest: BaboonGenerated {}

public protocol BaboonAdtMemberMeta {
    static var baboonAdtTypeIdentifier: String { get }
}

public protocol BaboonMeta {
    static var baboonDomainVersion: String { get }
    static var baboonDomainIdentifier: String { get }
    static var baboonTypeIdentifier: String { get }

    // PR 9.1: per-domain-version metadata registries (the generated `BaboonMetadata_*` classes)
    // expose `sameInVersions(typeId)` for the facade's max-compat lookup. Conformance is added
    // by codegen when PR 9.2/9.3 wire the BaboonMeta protocol onto the metadata registry class.
    // Until then, the facade only consumes this method via the Lazy-stored value, so missing
    // conformance only manifests when codecs from older versions are looked up.
    func sameInVersions(_ typeId: String) -> [String]
}

// --- Codec Context ---

// Marker base for `BaboonCodecsFacade` so [BaboonCodecContext] can carry an optional facade
// reference without an import cycle. The real facade lives in `BaboonCodecsFacade.swift` and
// extends this base.
open class BaboonCodecsFacadeBase {
    public init() {}
}

// Codec context. Exposes [useIndices] (UEBA index emission) and an optional [facade] reference
// threaded through generated codec calls so the `any`-feature cross-format conversion
// (UEBA <-> JSON) can resolve codecs by `(domain, version, typeid)` from an `AnyMeta` envelope.
// `nil` for the bare [defaultCtx]/[indexed]/[compact] singletons; [withFacade] is the single
// intended construction path for ctxes that thread a facade. Mirrors Scala/C#/Java/Kotlin/TS/Dart.
//
// PR 9.1: promoted from Swift enum to class so subclassing can carry an optional facade
// reference. Existing static accessors (`.defaultCtx`, `.compact`, `.indexed`, `.default`)
// preserved; `useIndices` preserved as a property; new `facade` getter defaults to `nil`.
open class BaboonCodecContext {
    public init() {}

    open var useIndices: Bool { return false }
    open var facade: BaboonCodecsFacadeBase? { return nil }

    public static let defaultCtx: BaboonCodecContext = BaboonCodecContextCompact()
    public static let `default`: BaboonCodecContext = BaboonCodecContextCompact()
    public static let compact: BaboonCodecContext = BaboonCodecContextCompact()
    public static let indexed: BaboonCodecContext = BaboonCodecContextIndexed()

    public static func withFacade(_ useIndices: Bool, _ facade: BaboonCodecsFacadeBase) -> BaboonCodecContext {
        return BaboonCodecContextWithFacade(useIndices: useIndices, facade: facade)
    }
}

public final class BaboonCodecContextCompact: BaboonCodecContext {
    public override var useIndices: Bool { return false }
}

public final class BaboonCodecContextIndexed: BaboonCodecContext {
    public override var useIndices: Bool { return true }
}

public final class BaboonCodecContextWithFacade: BaboonCodecContext {
    private let _useIndices: Bool
    private let _facade: BaboonCodecsFacadeBase
    public init(useIndices: Bool, facade: BaboonCodecsFacadeBase) {
        self._useIndices = useIndices
        self._facade = facade
        super.init()
    }
    public override var useIndices: Bool { return _useIndices }
    public override var facade: BaboonCodecsFacadeBase? { return _facade }
}

// --- JSON Codecs ---

open class BaboonJsonCodec<T> {
    public init() {}
    open func decode(_ ctx: BaboonCodecContext, _ wire: Any) throws -> T {
        fatalError("Must override")
    }
}

open class BaboonJsonCodecBase<T>: BaboonJsonCodec<T> {
    open func encode(_ ctx: BaboonCodecContext, _ value: T) -> Any {
        fatalError("Must override")
    }

    // BAB-S0x: deterministic JSON-bytes encoding for end-user code.
    //
    // The generated codec's `encode` returns `Any` (a Swift `Dictionary` for object
    // types). Swift `Dictionary` is hash-based; iterating it directly yields
    // hash-ordered keys, and `JSONSerialization.data(withJSONObject:)` re-hashes
    // unless `.sortedKeys` is passed in `options`. Compiler-emitted write sites
    // (service wiring, generated tests) already pass `.sortedKeys`. End-user code
    // that calls `encode(...)` and serialises the returned `Any` itself MUST also
    // pass `.sortedKeys` — or use the helpers below.
    //
    // Always-deterministic helpers; recommend these over manual JSONSerialization.
    public func encodeToJsonData(_ ctx: BaboonCodecContext, _ value: T) throws -> Data {
        let encoded = encode(ctx, value)
        return try JSONSerialization.data(withJSONObject: encoded, options: [.sortedKeys, .fragmentsAllowed])
    }

    public func encodeToJsonString(_ ctx: BaboonCodecContext, _ value: T) throws -> String {
        let data = try encodeToJsonData(ctx, value)
        guard let str = String(data: data, encoding: .utf8) else {
            throw BaboonCodecException.encoderFailure("UTF-8 decode of JSON bytes failed", nil)
        }
        return str
    }
}

open class BaboonJsonCodecBaseGenerated<T>: BaboonJsonCodecBase<T> {}

open class BaboonJsonCodecBaseGeneratedAdt<T>: BaboonJsonCodecBase<T> {}

open class BaboonJsonCodecNoEncoder<T>: BaboonJsonCodec<T> {}

open class BaboonJsonCodecNoEncoderGenerated<T>: BaboonJsonCodecNoEncoder<T> {}

open class BaboonJsonCodecNoEncoderGeneratedAdt<T>: BaboonJsonCodecNoEncoder<T> {}

// --- Binary Codecs ---

open class BaboonBinCodec<T> {
    public init() {}
    open func decode(_ ctx: BaboonCodecContext, _ reader: BaboonBinReader) throws -> T {
        fatalError("Must override")
    }
}

open class BaboonBinCodecBase<T>: BaboonBinCodec<T> {
    open func encode(_ ctx: BaboonCodecContext, _ writer: BaboonBinWriter, _ value: T) {
        fatalError("Must override")
    }
}

open class BaboonBinCodecBaseGenerated<T>: BaboonBinCodecBase<T> {}

open class BaboonBinCodecBaseGeneratedAdt<T>: BaboonBinCodecBase<T> {}

open class BaboonBinCodecNoEncoder<T>: BaboonBinCodec<T> {}

open class BaboonBinCodecNoEncoderGenerated<T>: BaboonBinCodecNoEncoder<T> {}

open class BaboonBinCodecNoEncoderGeneratedAdt<T>: BaboonBinCodecNoEncoder<T> {}

public protocol BaboonBinCodecIndexed {
    var indexElementsCount: Int { get }
}

public struct BaboonIndexEntry {
    public let offset: Int32
    public let length: Int32

    public init(offset: Int32, length: Int32) {
        self.offset = offset
        self.length = length
    }
}

extension BaboonBinCodecIndexed {
    public func readIndex(_ ctx: BaboonCodecContext, _ reader: BaboonBinReader) throws -> [BaboonIndexEntry] {
        let header = reader.readU8()
        let hasIndex = (header & 1) != 0
        if !hasIndex { return [] }
        let count = indexElementsCount
        var entries: [BaboonIndexEntry] = []
        for _ in 0..<count {
            let offset = reader.readI32()
            let length = reader.readI32()
            entries.append(BaboonIndexEntry(offset: offset, length: length))
        }
        return entries
    }
}

// --- Binary Writer ---

public class BaboonBinWriter {
    public static let dotnetEpochOffsetMs: Int64 = 62135596800000

    private var buf: [UInt8]
    public private(set) var position: Int = 0

    public init(initialCapacity: Int = 256) {
        buf = [UInt8](repeating: 0, count: initialCapacity)
    }

    private func ensureCapacity(_ needed: Int) {
        if position + needed > buf.count {
            var newCap = buf.count * 2
            while newCap < position + needed {
                newCap *= 2
            }
            var newBuf = [UInt8](repeating: 0, count: newCap)
            newBuf[0..<position] = buf[0..<position]
            buf = newBuf
        }
    }

    public func writeU8(_ value: UInt8) {
        ensureCapacity(1)
        buf[position] = value
        position += 1
    }

    public func writeI8(_ value: Int8) {
        ensureCapacity(1)
        buf[position] = UInt8(bitPattern: value)
        position += 1
    }

    public func writeU16(_ value: UInt16) {
        ensureCapacity(2)
        var v = value.littleEndian
        withUnsafeBytes(of: &v) { ptr in
            buf[position] = ptr[0]
            buf[position + 1] = ptr[1]
        }
        position += 2
    }

    public func writeI16(_ value: Int16) {
        ensureCapacity(2)
        var v = value.littleEndian
        withUnsafeBytes(of: &v) { ptr in
            buf[position] = ptr[0]
            buf[position + 1] = ptr[1]
        }
        position += 2
    }

    public func writeU32(_ value: UInt32) {
        ensureCapacity(4)
        var v = value.littleEndian
        withUnsafeBytes(of: &v) { ptr in
            for i in 0..<4 { buf[position + i] = ptr[i] }
        }
        position += 4
    }

    public func writeI32(_ value: Int32) {
        ensureCapacity(4)
        var v = value.littleEndian
        withUnsafeBytes(of: &v) { ptr in
            for i in 0..<4 { buf[position + i] = ptr[i] }
        }
        position += 4
    }

    public func writeU64(_ value: UInt64) {
        ensureCapacity(8)
        var v = value.littleEndian
        withUnsafeBytes(of: &v) { ptr in
            for i in 0..<8 { buf[position + i] = ptr[i] }
        }
        position += 8
    }

    public func writeI64(_ value: Int64) {
        ensureCapacity(8)
        var v = value.littleEndian
        withUnsafeBytes(of: &v) { ptr in
            for i in 0..<8 { buf[position + i] = ptr[i] }
        }
        position += 8
    }

    public func writeF32(_ value: Float) {
        ensureCapacity(4)
        var v = value.bitPattern.littleEndian
        withUnsafeBytes(of: &v) { ptr in
            for i in 0..<4 { buf[position + i] = ptr[i] }
        }
        position += 4
    }

    public func writeF64(_ value: Double) {
        ensureCapacity(8)
        var v = value.bitPattern.littleEndian
        withUnsafeBytes(of: &v) { ptr in
            for i in 0..<8 { buf[position + i] = ptr[i] }
        }
        position += 8
    }

    public func writeBool(_ value: Bool) {
        writeU8(value ? 1 : 0)
    }

    public func writeString(_ value: String) {
        let bytes = Array(value.utf8)
        // 7-bit VLQ encoding for string length (compatible with .NET BinaryWriter)
        var len = bytes.count
        repeat {
            var currentByte = UInt8(len & 0x7F)
            len >>= 7
            if len != 0 { currentByte |= 0x80 }
            writeU8(currentByte)
        } while len != 0
        writeAll(Data(bytes))
    }

    public func writeBytes(_ data: Data) {
        writeI32(Int32(data.count))
        writeAll(data)
    }

    public func writeDecimal(_ value: BaboonDecimal) {
        // .NET decimal format: lo (i32), mid (i32), hi (i32), flags (i32) = 16 bytes
        var str = value.stringValue
        let isNeg = str.hasPrefix("-")
        if isNeg { str = String(str.dropFirst()) }

        var scale = 0
        if let dotIdx = str.firstIndex(of: ".") {
            scale = str.distance(from: str.index(after: dotIdx), to: str.endIndex)
        }

        let mantissaStr = str.replacingOccurrences(of: ".", with: "")
        var mantissa: (lo: UInt32, mid: UInt32, hi: UInt32) = (0, 0, 0)

        // Parse mantissa as 96-bit integer
        let digits = Array(mantissaStr.utf8).map { UInt64($0 - 48) }
        // Convert decimal digits to 96-bit binary
        var bigVal: [UInt32] = [0, 0, 0] // lo, mid, hi
        for digit in digits {
            // multiply bigVal by 10 and add digit
            var mulCarry: UInt64 = digit
            for i in 0..<3 {
                let product = UInt64(bigVal[i]) * 10 + mulCarry
                bigVal[i] = UInt32(product & 0xFFFFFFFF)
                mulCarry = product >> 32
            }
        }
        mantissa = (bigVal[0], bigVal[1], bigVal[2])

        let sign: UInt32 = isNeg ? 0x80000000 : 0
        let flags = sign | (UInt32(scale) << 16)

        writeRawU32(mantissa.lo)
        writeRawU32(mantissa.mid)
        writeRawU32(mantissa.hi)
        writeRawU32(flags)
    }

    private func writeRawU32(_ value: UInt32) {
        ensureCapacity(4)
        var v = value.littleEndian
        withUnsafeBytes(of: &v) { ptr in
            for i in 0..<4 { buf[position + i] = ptr[i] }
        }
        position += 4
    }

    public func writeUuid(_ uuid: UUID) {
        let hex = uuid.uuidString.replacingOccurrences(of: "-", with: "").lowercased()
        assert(hex.count == 32)

        let hexBytes = Array(hex.utf8)
        func parseByte(_ offset: Int) -> UInt8 {
            func hexVal(_ c: UInt8) -> UInt8 {
                if c >= 48 && c <= 57 { return c - 48 }
                if c >= 97 && c <= 102 { return c - 87 }
                if c >= 65 && c <= 70 { return c - 55 }
                fatalError("Invalid hex char")
            }
            return hexVal(hexBytes[offset]) << 4 | hexVal(hexBytes[offset + 1])
        }

        // .NET mixed-endian GUID format
        var bytes = [UInt8](repeating: 0, count: 16)
        // First 4 bytes: little-endian
        bytes[0] = parseByte(6)
        bytes[1] = parseByte(4)
        bytes[2] = parseByte(2)
        bytes[3] = parseByte(0)
        // Next 2 bytes: little-endian
        bytes[4] = parseByte(10)
        bytes[5] = parseByte(8)
        // Next 2 bytes: little-endian
        bytes[6] = parseByte(14)
        bytes[7] = parseByte(12)
        // Remaining 8 bytes: big-endian
        for i in 0..<8 {
            bytes[8 + i] = parseByte(16 + i * 2)
        }
        writeAll(Data(bytes))
    }

    public func writeTsu(_ value: Date) {
        let epochMs = baboonEpochMillis(value)
        let dotnetUtcMs = epochMs + BaboonBinWriter.dotnetEpochOffsetMs
        writeI64(dotnetUtcMs)
        writeI64(0)
        writeU8(1) // kind = 1 (UTC)
    }

    public func writeTso(_ value: BaboonDateTimeOffset) {
        let dotnetUtcMs = value.epochMillis + BaboonBinWriter.dotnetEpochOffsetMs
        let dotnetLocalMs = dotnetUtcMs + value.offsetMillis
        writeI64(dotnetLocalMs)
        writeI64(value.offsetMillis)
        writeU8(0) // kind = 0 (offset)
    }

    public func writeAll(_ data: Data) {
        if data.isEmpty { return }
        ensureCapacity(data.count)
        data.copyBytes(to: &buf[position], count: data.count)
        position += data.count
    }

    public func toData() -> Data {
        return Data(buf[0..<position])
    }
}

// --- Binary Reader ---

public class BaboonBinReader {
    private let data: Data
    private var pos: Int = 0

    public init(_ data: Data) {
        self.data = data
    }

    /// PR 9.1: expose the read cursor for `AnyMetaCodec.readBinWithLength` (PR-05-D01).
    public var position: Int { return pos }

    public func readU8() -> UInt8 {
        let v = data[data.startIndex + pos]
        pos += 1
        return v
    }

    public func readI8() -> Int8 {
        let v = Int8(bitPattern: data[data.startIndex + pos])
        pos += 1
        return v
    }

    public func readU16() -> UInt16 {
        var v: UInt16 = 0
        _ = withUnsafeMutableBytes(of: &v) { ptr in
            data.copyBytes(to: ptr, from: (data.startIndex + pos)..<(data.startIndex + pos + 2))
        }
        pos += 2
        return UInt16(littleEndian: v)
    }

    public func readI16() -> Int16 {
        var v: Int16 = 0
        _ = withUnsafeMutableBytes(of: &v) { ptr in
            data.copyBytes(to: ptr, from: (data.startIndex + pos)..<(data.startIndex + pos + 2))
        }
        pos += 2
        return Int16(littleEndian: v)
    }

    public func readU32() -> UInt32 {
        var v: UInt32 = 0
        _ = withUnsafeMutableBytes(of: &v) { ptr in
            data.copyBytes(to: ptr, from: (data.startIndex + pos)..<(data.startIndex + pos + 4))
        }
        pos += 4
        return UInt32(littleEndian: v)
    }

    public func readI32() -> Int32 {
        var v: Int32 = 0
        _ = withUnsafeMutableBytes(of: &v) { ptr in
            data.copyBytes(to: ptr, from: (data.startIndex + pos)..<(data.startIndex + pos + 4))
        }
        pos += 4
        return Int32(littleEndian: v)
    }

    public func readU64() -> UInt64 {
        var v: UInt64 = 0
        _ = withUnsafeMutableBytes(of: &v) { ptr in
            data.copyBytes(to: ptr, from: (data.startIndex + pos)..<(data.startIndex + pos + 8))
        }
        pos += 8
        return UInt64(littleEndian: v)
    }

    public func readI64() -> Int64 {
        var v: Int64 = 0
        _ = withUnsafeMutableBytes(of: &v) { ptr in
            data.copyBytes(to: ptr, from: (data.startIndex + pos)..<(data.startIndex + pos + 8))
        }
        pos += 8
        return Int64(littleEndian: v)
    }

    public func readF32() -> Float {
        let bits = readU32()
        return Float(bitPattern: bits)
    }

    public func readF64() -> Double {
        let bits = readU64()
        return Double(bitPattern: bits)
    }

    public func readBool() -> Bool {
        return readU8() != 0
    }

    public func readString() throws -> String {
        // 7-bit VLQ decoding for string length (compatible with .NET BinaryReader)
        var length = 0
        var shift = 0
        while true {
            let byteRead = Int(readU8())
            length |= (byteRead & 0x7F) << shift
            shift += 7
            if (byteRead & 0x80) == 0 { break }
        }
        guard pos + length <= data.count else {
            throw BaboonCodecError.truncated("readString: need \(length) bytes at pos \(pos), only \(data.count - pos) available")
        }
        let bytes = data.subdata(in: (data.startIndex + pos)..<(data.startIndex + pos + length))
        pos += length
        guard let str = String(data: bytes, encoding: .utf8) else {
            throw BaboonCodecError.invalidUtf8
        }
        return str
    }

    public func readBytes() throws -> Data {
        let length = Int(readI32())
        guard length >= 0 && pos + length <= data.count else {
            throw BaboonCodecError.truncated("readBytes: need \(length) bytes at pos \(pos), only \(data.count - pos) available")
        }
        let bytes = data.subdata(in: (data.startIndex + pos)..<(data.startIndex + pos + length))
        pos += length
        return bytes
    }

    /// PR 9.2: read raw `count` bytes (no length prefix). Used by the `any`-field decoder for the
    /// blob payload — the wire `total-length` already gates the count, so we can copy in a single
    /// `subdata` call instead of looping `readU8()`. Mirrors Dart's PR 8.2 `readNBytes`.
    public func readNBytes(_ count: Int) -> Data {
        let bytes = data.subdata(in: (data.startIndex + pos)..<(data.startIndex + pos + count))
        pos += count
        return bytes
    }

    /// PR 9.2: advance the read cursor past `count` bytes without returning them. Used by the
    /// `any`-field forward-compat skip-trailer (PR-05-D01). Mirrors Dart's PR 8.2 `skipBytes`.
    public func skipBytes(_ count: Int) {
        pos += count
    }

    public func readDecimal() -> BaboonDecimal {
        // .NET decimal format: lo (i32), mid (i32), hi (i32), flags (i32) = 16 bytes
        let lo = UInt64(readRawU32())
        let mid = UInt64(readRawU32())
        let hi = UInt64(readRawU32())
        let flags = readRawU32()

        let scale = Int((flags >> 16) & 0xFF)
        let isNeg = (flags & 0x80000000) != 0

        // Reconstruct 96-bit mantissa
        var mantissa: [UInt32] = [UInt32(lo & 0xFFFFFFFF), UInt32(mid & 0xFFFFFFFF), UInt32(hi & 0xFFFFFFFF)]

        // Convert 96-bit integer to decimal string by repeated division
        var result = ""
        var isZero = false
        while !isZero {
            var remainder: UInt64 = 0
            isZero = true
            for i in stride(from: 2, through: 0, by: -1) {
                let current = remainder << 32 | UInt64(mantissa[i])
                mantissa[i] = UInt32(current / 10)
                remainder = current % 10
                if mantissa[i] != 0 { isZero = false }
            }
            result = String(remainder) + result
        }
        if result.isEmpty { result = "0" }

        if scale > 0 {
            while result.count <= scale {
                result = "0" + result
            }
            let insertIdx = result.index(result.endIndex, offsetBy: -scale)
            result.insert(".", at: insertIdx)
            // Remove trailing zeros after decimal point
            while result.hasSuffix("0") {
                result.removeLast()
            }
            if result.hasSuffix(".") {
                result.removeLast()
            }
            if result.isEmpty { result = "0" }
        }

        if isNeg && result != "0" {
            result = "-" + result
        }

        return BaboonDecimal(result)
    }

    private func readRawU32() -> UInt32 {
        var v: UInt32 = 0
        _ = withUnsafeMutableBytes(of: &v) { ptr in
            data.copyBytes(to: ptr, from: (data.startIndex + pos)..<(data.startIndex + pos + 4))
        }
        pos += 4
        return UInt32(littleEndian: v)
    }

    public func readUuid() throws -> UUID {
        guard pos + 16 <= data.count else {
            throw BaboonCodecError.truncated("readUuid: need 16 bytes at pos \(pos), only \(data.count - pos) available")
        }
        var bytes = [UInt8](repeating: 0, count: 16)
        for i in 0..<16 {
            bytes[i] = data[data.startIndex + pos + i]
        }
        pos += 16

        // .NET mixed-endian GUID format to standard UUID
        func hexByte(_ b: UInt8) -> String {
            return String(format: "%02x", b)
        }

        var hex = ""
        hex += hexByte(bytes[3]) + hexByte(bytes[2]) + hexByte(bytes[1]) + hexByte(bytes[0])
        hex += "-"
        hex += hexByte(bytes[5]) + hexByte(bytes[4])
        hex += "-"
        hex += hexByte(bytes[7]) + hexByte(bytes[6])
        hex += "-"
        hex += hexByte(bytes[8]) + hexByte(bytes[9])
        hex += "-"
        for i in 10..<16 {
            hex += hexByte(bytes[i])
        }
        guard let uuid = UUID(uuidString: hex) else {
            throw BaboonCodecError.invalidUuid
        }
        return uuid
    }

    public func readTsu() -> Date {
        let dotnetLocalMs = readI64()
        let offsetMs = readI64()
        let kind = readU8()
        assert(kind >= 0 && kind <= 2)
        let dotnetUtcMs = dotnetLocalMs - offsetMs
        let epochMs = dotnetUtcMs - BaboonBinWriter.dotnetEpochOffsetMs
        return Date(timeIntervalSince1970: Double(epochMs) / 1000.0)
    }

    public func readTso() -> BaboonDateTimeOffset {
        let dotnetLocalMs = readI64()
        let offsetMs = readI64()
        let kind = readU8()
        assert(kind >= 0 && kind <= 2)
        let dotnetUtcMs = dotnetLocalMs - offsetMs
        let epochMs = dotnetUtcMs - BaboonBinWriter.dotnetEpochOffsetMs
        let normalizedKind: String
        switch kind {
        case 0:
            normalizedKind = "offset"
        case 1:
            normalizedKind = "utc"
        default:
            normalizedKind = "local"
        }
        return BaboonDateTimeOffset(
            epochMillis: epochMs,
            offsetMillis: offsetMs,
            kind: normalizedKind
        )
    }
}

// --- Binary Tools ---

public class BaboonBinTools {
    public static func createWriter(_ initialCapacity: Int = 256) -> BaboonBinWriter {
        return BaboonBinWriter(initialCapacity: initialCapacity)
    }

    public static func createReader(_ data: Data) -> BaboonBinReader {
        return BaboonBinReader(data)
    }
}

// --- Time Formats ---

public class BaboonTimeFormats {
    private static let utcTimeZone = TimeZone(secondsFromGMT: 0)!
    private static let utcCalendar: Calendar = {
        var c = Calendar(identifier: .gregorian)
        c.timeZone = utcTimeZone
        return c
    }()

    public static func formatUtc(_ dt: Date) -> String {
        let epochMillis = baboonEpochMillis(dt)
        let normalizedDate = Date(timeIntervalSince1970: Double(epochMillis) / 1000.0)
        let comps = utcCalendar.dateComponents([.year, .month, .day, .hour, .minute, .second], from: normalizedDate)
        let ms = Int((epochMillis % 1000 + 1000) % 1000)
        return String(
            format: "%04d-%02d-%02dT%02d:%02d:%02d.%03dZ",
            comps.year!,
            comps.month!,
            comps.day!,
            comps.hour!,
            comps.minute!,
            comps.second!,
            ms
        )
    }

    public static func formatOffset(_ dto: BaboonDateTimeOffset) -> String {
        let utcDate = Date(timeIntervalSince1970: Double(dto.epochMillis) / 1000.0)
        let offsetSeconds = Int(dto.offsetMillis / 1000)
        let localDate = utcDate.addingTimeInterval(Double(offsetSeconds))

        let calendar = Calendar(identifier: .gregorian)
        let comps = calendar.dateComponents(in: TimeZone(secondsFromGMT: 0)!, from: localDate)
        let y = String(format: "%04d", comps.year!)
        let m = String(format: "%02d", comps.month!)
        let d = String(format: "%02d", comps.day!)
        let h = String(format: "%02d", comps.hour!)
        let min = String(format: "%02d", comps.minute!)
        let sec = String(format: "%02d", comps.second!)
        let localEpochMillis = baboonEpochMillis(localDate)
        let ms = String(format: "%03d", Int((localEpochMillis % 1000 + 1000) % 1000))
        let base = "\(y)-\(m)-\(d)T\(h):\(min):\(sec).\(ms)"

        let sign = dto.offsetMillis >= 0 ? "+" : "-"
        let absOffset = abs(Int(dto.offsetMillis))
        let hours = String(format: "%02d", absOffset / 3600000)
        let minutes = String(format: "%02d", (absOffset % 3600000) / 60000)
        return "\(base)\(sign)\(hours):\(minutes)"
    }

    public static func parseUtc(_ s: String) -> Date {
        let parsed = parseIso8601(s)
        return Date(timeIntervalSince1970: Double(parsed.epochMillis) / 1000.0)
    }

    public static func parseOffset(_ s: String) -> BaboonDateTimeOffset {
        let parsed = parseIso8601(s)
        if parsed.hasOffset {
            return BaboonDateTimeOffset(
                epochMillis: parsed.epochMillis,
                offsetMillis: parsed.offsetMillis,
                kind: "offset"
            )
        }
        if parsed.hasZulu {
            return BaboonDateTimeOffset(
                epochMillis: parsed.epochMillis,
                offsetMillis: 0,
                kind: "utc"
            )
        }
        return BaboonDateTimeOffset(
            epochMillis: parsed.epochMillis,
            offsetMillis: 0,
            kind: "local"
        )
    }

    private struct ParsedIso8601 {
        let epochMillis: Int64
        let offsetMillis: Int64
        let hasOffset: Bool
        let hasZulu: Bool
    }

    private static func parseIso8601(_ s: String) -> ParsedIso8601 {
        let bytes = Array(s.utf8)
        assert(bytes.count >= 19)
        assert(bytes[4] == 45)  // -
        assert(bytes[7] == 45)  // -
        assert(bytes[10] == 84 || bytes[10] == 116) // T/t
        assert(bytes[13] == 58) // :
        assert(bytes[16] == 58) // :

        let year = parseDecimal(bytes, 0, 4)
        let month = parseDecimal(bytes, 5, 2)
        let day = parseDecimal(bytes, 8, 2)
        let hour = parseDecimal(bytes, 11, 2)
        let minute = parseDecimal(bytes, 14, 2)
        let second = parseDecimal(bytes, 17, 2)

        var idx = 19
        var millis = 0
        if idx < bytes.count && bytes[idx] == 46 { // .
            idx += 1
            let fracStart = idx
            while idx < bytes.count && isDigit(bytes[idx]) {
                idx += 1
            }
            let fracLen = idx - fracStart
            assert(fracLen > 0)
            let used = min(fracLen, 3)
            var i = 0
            while i < used {
                millis = millis * 10 + Int(bytes[fracStart + i] - 48)
                i += 1
            }
            if used == 1 { millis *= 100 }
            if used == 2 { millis *= 10 }
        }

        var offsetMillis: Int64 = 0
        var hasOffset = false
        var hasZulu = false

        if idx < bytes.count {
            let tz = bytes[idx]
            if tz == 90 || tz == 122 { // Z/z
                assert(idx + 1 == bytes.count)
                hasZulu = true
            } else {
                assert(tz == 43 || tz == 45) // +/-
                assert(idx + 6 == bytes.count)
                assert(bytes[idx + 3] == 58) // :
                let sign: Int64 = tz == 43 ? 1 : -1
                let tzHours = Int64(parseDecimal(bytes, idx + 1, 2))
                let tzMinutes = Int64(parseDecimal(bytes, idx + 4, 2))
                offsetMillis = sign * (tzHours * 3600000 + tzMinutes * 60000)
                hasOffset = true
            }
        }

        var comps = DateComponents()
        comps.year = year
        comps.month = month
        comps.day = day
        comps.hour = hour
        comps.minute = minute
        comps.second = second
        comps.nanosecond = millis * 1_000_000

        let localDate = utcCalendar.date(from: comps)!
        let localEpochMillis = baboonEpochMillis(localDate)
        let epochMillis = localEpochMillis - offsetMillis

        return ParsedIso8601(
            epochMillis: epochMillis,
            offsetMillis: offsetMillis,
            hasOffset: hasOffset,
            hasZulu: hasZulu
        )
    }

    private static func parseDecimal(_ bytes: [UInt8], _ start: Int, _ count: Int) -> Int {
        assert(start + count <= bytes.count)
        var value = 0
        var i = 0
        while i < count {
            let b = bytes[start + i]
            assert(isDigit(b))
            value = value * 10 + Int(b - 48)
            i += 1
        }
        return value
    }

    private static func isDigit(_ b: UInt8) -> Bool {
        return b >= 48 && b <= 57
    }
}

// --- Byte String Tools ---

public class BaboonByteStringTools {
    public static func fromHexString(_ hex: String) -> Data {
        let chars = Array(hex.utf8)
        let length = chars.count / 2
        var result = [UInt8](repeating: 0, count: length)
        for i in 0..<length {
            let hi = hexVal(chars[i * 2])
            let lo = hexVal(chars[i * 2 + 1])
            result[i] = (hi << 4) | lo
        }
        return Data(result)
    }

    public static func toHexString(_ data: Data) -> String {
        return data.map { String(format: "%02x", $0) }.joined()
    }

    private static func hexVal(_ c: UInt8) -> UInt8 {
        if c >= 48 && c <= 57 { return c - 48 }       // 0-9
        if c >= 97 && c <= 102 { return c - 87 }       // a-f
        if c >= 65 && c <= 70 { return c - 55 }        // A-F
        fatalError("Invalid hex char")
    }
}

// --- Custom Types ---

public struct BaboonDecimal: Equatable, Hashable {
    public let stringValue: String

    public init(_ value: String) {
        self.stringValue = value
    }

    private static func normalize(_ s: String) -> String {
        if !s.contains(".") { return s }
        var result = s
        while result.hasSuffix("0") {
            result.removeLast()
        }
        if result.hasSuffix(".") { result.removeLast() }
        return result
    }

    public static func == (lhs: BaboonDecimal, rhs: BaboonDecimal) -> Bool {
        return normalize(lhs.stringValue) == normalize(rhs.stringValue)
    }

    public func hash(into hasher: inout Hasher) {
        hasher.combine(BaboonDecimal.normalize(stringValue))
    }
}

public struct BaboonDateTimeOffset: Equatable, Hashable {
    public let epochMillis: Int64
    public let offsetMillis: Int64
    public let kind: String

    public init(epochMillis: Int64, offsetMillis: Int64, kind: String) {
        self.epochMillis = epochMillis
        self.offsetMillis = offsetMillis
        self.kind = kind
    }

    public static func == (lhs: BaboonDateTimeOffset, rhs: BaboonDateTimeOffset) -> Bool {
        return lhs.epochMillis == rhs.epochMillis && lhs.offsetMillis == rhs.offsetMillis
    }

    public func hash(into hasher: inout Hasher) {
        hasher.combine(epochMillis)
        hasher.combine(offsetMillis)
    }

    public func toUtc() -> Date {
        return Date(timeIntervalSince1970: Double(epochMillis) / 1000.0)
    }

    public func toLocal() -> Date {
        return Date(timeIntervalSince1970: Double(epochMillis + offsetMillis) / 1000.0)
    }
}

// --- Conversions ---

open class AbstractConversion<F, T> {
    public init() {}

    open var versionFrom: String { fatalError("Must override") }
    open var versionTo: String { fatalError("Must override") }
    open var typeId: String { fatalError("Must override") }

    open func doConvert(_ context: Any?, _ conversions: AbstractBaboonConversions, _ from: F) -> T {
        fatalError("Must override")
    }

    public func convert(_ from: F) -> T {
        return doConvert(nil, AbstractBaboonConversions(), from)
    }
}

open class AbstractBaboonConversions {
    private var registry: [String: () -> Any] = [:]

    public init() {}

    public func register<F, T>(_ typeId: String, _ factory: @escaping () -> AbstractConversion<F, T>) {
        registry[typeId] = factory
    }

    public func convertWithContext<F, T>(_ context: Any?, _ from: F, _ sourceTypeId: String, _ targetTypeId: String) -> T {
        guard let factory = registry[targetTypeId] else {
            fatalError("No conversion registered for type: \(targetTypeId)")
        }
        let conversion = factory() as! AbstractConversion<F, T>
        return conversion.doConvert(context, self, from)
    }

    open var versionsFrom: [String] { [] }
    open var versionTo: String { "" }
}

// --- Codec type erasure (PR 9.1) ---
//
// Swift generics are invariant: a `BaboonBinCodecBase<ConcreteT>` is NOT a
// `BaboonBinCodecBase<Any>`. The facade dispatches against `Any`-typed codec slots by going
// through these non-generic protocols, which the generic base classes conform to via extensions.
public protocol AnyBaboonBinDecoder: AnyObject {
    func decodeAnyValue(_ ctx: BaboonCodecContext, _ reader: BaboonBinReader) throws -> Any
}

public protocol AnyBaboonBinEncoder: AnyObject {
    func encodeAnyValue(_ ctx: BaboonCodecContext, _ writer: BaboonBinWriter, _ value: Any)
}

public protocol AnyBaboonJsonDecoder: AnyObject {
    func decodeAnyValue(_ ctx: BaboonCodecContext, _ wire: Any) throws -> Any
}

public protocol AnyBaboonJsonEncoder: AnyObject {
    func encodeAnyValue(_ ctx: BaboonCodecContext, _ value: Any) -> Any
}

extension BaboonBinCodec: AnyBaboonBinDecoder {
    public func decodeAnyValue(_ ctx: BaboonCodecContext, _ reader: BaboonBinReader) throws -> Any {
        return try decode(ctx, reader)
    }
}

extension BaboonBinCodecBase: AnyBaboonBinEncoder {
    public func encodeAnyValue(_ ctx: BaboonCodecContext, _ writer: BaboonBinWriter, _ value: Any) {
        guard let typed = value as? T else {
            preconditionFailure("AnyBaboonBinEncoder: value of type \(type(of: value)) is not assignable to expected codec type")
        }
        encode(ctx, writer, typed)
    }
}

extension BaboonJsonCodec: AnyBaboonJsonDecoder {
    public func decodeAnyValue(_ ctx: BaboonCodecContext, _ wire: Any) throws -> Any {
        return try decode(ctx, wire)
    }
}

extension BaboonJsonCodecBase: AnyBaboonJsonEncoder {
    public func encodeAnyValue(_ ctx: BaboonCodecContext, _ value: Any) -> Any {
        guard let typed = value as? T else {
            preconditionFailure("AnyBaboonJsonEncoder: value of type \(type(of: value)) is not assignable to expected codec type")
        }
        return encode(ctx, typed)
    }
}

// PR 9.1: storage and `codecFor` made non-generic (`Any?`) so the facade can dispatch through
// the typed runtime classes without a covariant `BaboonBinCodec<Any>` cast (Swift generics are
// invariant — a `BaboonBinCodec<ConcreteT>` is NOT a `BaboonBinCodec<Any>`). Generated registers
// emit `{ () -> Any in TheCodec.instance }` and the facade casts to the type-erased
// `AnyBaboonBin{Encoder,Decoder}` / `AnyBaboonJson{Encoder,Decoder}` protocols at lookup.
open class AbstractBaboonJsonCodecs {
    private var codecs: [String: Any] = [:]

    public init() {}

    public func register(_ typeId: String, _ factory: () -> Any) {
        codecs[typeId] = factory()
    }

    public func codecFor(_ typeId: String) -> Any? {
        return codecs[typeId]
    }
}

open class AbstractBaboonUebaCodecs {
    private var codecs: [String: Any] = [:]

    public init() {}

    public func register(_ typeId: String, _ factory: () -> Any) {
        codecs[typeId] = factory()
    }

    public func codecFor(_ typeId: String) -> Any? {
        return codecs[typeId]
    }
}

// --- Service Wiring ---

public struct BaboonMethodId: Equatable, Hashable {
    public let serviceId: String
    public let methodName: String

    public init(serviceId: String, methodName: String) {
        self.serviceId = serviceId
        self.methodName = methodName
    }
}

public enum BaboonWiringError: Error {
    case noMatchingMethod(BaboonMethodId)
    case decoderFailed(BaboonMethodId, Error)
    case encoderFailed(BaboonMethodId, Error)
    case callFailed(BaboonMethodId, Any)
}

public struct BaboonWiringException: Error {
    public let error: BaboonWiringError
    public init(_ error: BaboonWiringError) { self.error = error }
}

// --- Either for Service Results ---

public enum BaboonEither<L, R> {
    case left(L)
    case right(R)
}

// --- Codec Exceptions (PR 9.1) ---
//
// Sealed hierarchy mirroring Dart/Java/Kotlin/TS `BaboonCodecException`. Codec/conversion failures
// thread through `Result<T, BaboonCodecException>` per PR-04-D02 — JSON decode is user-facing
// and never throws via this hierarchy; binary decode trusts the wire and may throw.
public enum BaboonCodecException: Error, CustomStringConvertible {
    case encoderFailure(String, Error?)
    case decoderFailure(String, Error?)
    case converterFailure(String, Error?)
    case codecNotFound(String)
    case conversionNotFound(String)

    public var message: String {
        switch self {
        case .encoderFailure(let m, _): return m
        case .decoderFailure(let m, _): return m
        case .converterFailure(let m, _): return m
        case .codecNotFound(let m): return m
        case .conversionNotFound(let m): return m
        }
    }

    public var cause: Error? {
        switch self {
        case .encoderFailure(_, let c): return c
        case .decoderFailure(_, let c): return c
        case .converterFailure(_, let c): return c
        default: return nil
        }
    }

    public var description: String {
        let label: String
        switch self {
        case .encoderFailure: label = "BaboonEncoderFailure"
        case .decoderFailure: label = "BaboonDecoderFailure"
        case .converterFailure: label = "BaboonConverterFailure"
        case .codecNotFound: label = "BaboonCodecNotFound"
        case .conversionNotFound: label = "BaboonConversionNotFound"
        }
        return "\(label): \(message)"
    }
}

// --- Lazy (single-shot) ---
//
// Single-shot lazy initializer. Computes once on first [value] access and caches the result.
// Mirrors Dart `Lazy` semantics. Swift is not single-threaded; we guard with NSLock.
public final class BaboonLazy<T> {
    private var initializer: (() -> T)?
    private var _value: T?
    private let lock = NSLock()

    public init(_ initializer: @escaping () -> T) {
        self.initializer = initializer
    }

    public var value: T {
        lock.lock()
        defer { lock.unlock() }
        if let v = _value { return v }
        guard let init_ = initializer else {
            preconditionFailure("BaboonLazy: missing both init and value")
        }
        let v = init_()
        _value = v
        initializer = nil
        return v
    }

    public var isValueCreated: Bool {
        lock.lock()
        defer { lock.unlock() }
        return _value != nil
    }
}

// --- Codec Data ---
//
// Static metadata exposed by every generated codec — used by [BaboonCodecsFacade] to look up
// codecs by `(domain, version, typeid)`. Mirrors Dart/Kotlin `BaboonCodecData`.
public protocol BaboonCodecData: AnyObject {
    var baboonDomainVersion: String { get }
    var baboonDomainIdentifier: String { get }
    var baboonTypeIdentifier: String { get }
}

// --- Generated meta accessors (instance-side) ---
//
// Shape exposed by generated values. Codegen for Swift currently only emits the static-side
// `BaboonMeta` constants on each DTO; PR 9.2/9.3 will emit the instance-side accessors below.
// Until then, `BaboonTypeMeta.from` requires the value to conform to this protocol — staged
// rollout, mirrors PR-22-D02 (Dart) and PR-19-D02 (TS).
public protocol BaboonMetaProvider {
    var baboonDomainVersion: String { get }
    var baboonDomainIdentifier: String { get }
    var baboonTypeIdentifier: String { get }
    var baboonSameInVersions: [String] { get }
}

// Implemented by generated ADT branches. Mirrors Kotlin/Dart `BaboonAdtMember` for the
// `useAdtIdentifier` path used when encoding through an ADT-typed reference (PR-19-D02).
public protocol BaboonAdtMember {
    var baboonAdtTypeIdentifier: String { get }
}

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

    public init(
        _ metaVersion: Int,
        _ domainIdentifier: String,
        _ domainVersion: String,
        _ domainVersionMinCompat: String,
        _ typeIdentifier: String
    ) {
        self.metaVersion = metaVersion
        self.domainIdentifier = domainIdentifier
        self.domainVersion = domainVersion
        self.domainVersionMinCompat = domainVersionMinCompat
        self.typeIdentifier = typeIdentifier
    }

    public func versionRef() -> BaboonDomainVersion {
        return BaboonDomainVersion(domainIdentifier, domainVersion)
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

    // PR-08-D01 / PR-22-D01: tolerate absent `$mv` (treat as canonical) and reject explicit
    // `$mv != String(metaVersion)`. Spec: `$mv` is a JSON string; numbers are rejected.
    public static func readMetaJson(_ json: Any?) -> BaboonTypeMeta? {
        guard let obj = json as? [String: Any] else { return nil }
        if let mv = obj["$mv"] {
            guard let mvStr = mv as? String, mvStr == String(BaboonTypeMetaCodec.metaVersion) else { return nil }
        }
        guard let d = obj["$d"] as? String else { return nil }
        guard let v = obj["$v"] as? String else { return nil }
        guard let t = obj["$t"] as? String else { return nil }
        let minCompat = (obj["$uv"] as? String) ?? v
        return BaboonTypeMeta(BaboonTypeMetaCodec.metaVersion, d, v, minCompat, t)
    }

    public static func readMetaBin(_ reader: BaboonBinReader) throws -> BaboonTypeMeta? {
        return try BaboonTypeMetaCodec.readMeta(reader)
    }

    // Build a meta from a generated value. Optionally use the ADT type identifier when encoding
    // through an ADT-typed reference (PR-19-D02). Throws when the value does not conform to
    // [BaboonMetaProvider] — staged rollout: PR 9.2/9.3 will conform generated DTOs.
    public static func from(_ value: Any, useAdtIdentifier: Bool = false) throws -> BaboonTypeMeta {
        guard let meta = value as? BaboonMetaProvider else {
            throw BaboonException(
                "BaboonTypeMeta.from: value of type \(type(of: value)) does not conform to BaboonMetaProvider; " +
                "generated DTOs gain this conformance in PR 9.2/9.3."
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
        return BaboonTypeMeta(
            BaboonTypeMetaCodec.metaVersion,
            meta.baboonDomainIdentifier,
            meta.baboonDomainVersion,
            sameIn[0],
            typeId
        )
    }

    public var description: String {
        return "BaboonTypeMeta(\(domainIdentifier).\(typeIdentifier)@\(domainVersion))"
    }
}

public enum BaboonTypeMetaCodec {
    public static let metaVersion: Int = 1

    public static func writeBin(_ meta: BaboonTypeMeta, _ writer: BaboonBinWriter) {
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
        return nil
    }

    private static func readMetaV1(_ reader: BaboonBinReader) throws -> BaboonTypeMeta {
        let d = try reader.readString()
        let dv = try reader.readString()
        let hasMinCompat = reader.readU8()
        let mc = hasMinCompat == 1 ? try reader.readString() : dv
        let t = try reader.readString()
        return BaboonTypeMeta(metaVersion, d, dv, mc, t)
    }

    public static func writeJson(_ meta: BaboonTypeMeta) -> [String: Any] {
        // `$mv` elided for the canonical version (matches cs/java/rust/scala/ts/python writers).
        // Reader treats absent `$mv` as canonical.
        var obj: [String: Any] = [
            "$d": meta.domainIdentifier,
            "$v": meta.domainVersion,
            "$t": meta.typeIdentifier,
        ]
        if meta.domainVersion != meta.domainVersionMinCompat {
            obj["$uv"] = meta.domainVersionMinCompat
        }
        return obj
    }
}

// --- Deep Equality / HashCode for JSON-shaped values ---
//
// Used by `AnyOpaqueJson` content equality. Swift `[String: Any]` and `[Any]` use reference /
// default Equatable rules under `==`; we walk the structure to compare leaves.
public func baboonDeepEquals(_ a: Any?, _ b: Any?) -> Bool {
    if a == nil && b == nil { return true }
    if a == nil || b == nil { return false }
    if let am = a as? [String: Any], let bm = b as? [String: Any] {
        if am.count != bm.count { return false }
        for (k, v) in am {
            guard bm.keys.contains(k) else { return false }
            if !baboonDeepEquals(v, bm[k]!) { return false }
        }
        return true
    }
    if let al = a as? [Any], let bl = b as? [Any] {
        if al.count != bl.count { return false }
        for i in 0..<al.count {
            if !baboonDeepEquals(al[i], bl[i]) { return false }
        }
        return true
    }
    if let an = a as? NSNumber, let bn = b as? NSNumber {
        return an == bn
    }
    if let s1 = a as? String, let s2 = b as? String { return s1 == s2 }
    if let i1 = a as? Int, let i2 = b as? Int { return i1 == i2 }
    if let d1 = a as? Double, let d2 = b as? Double { return d1 == d2 }
    if let b1 = a as? Bool, let b2 = b as? Bool { return b1 == b2 }
    // Fallback: cross-cast to NSObject (covers NSNull and most boxed types)
    if let ao = a as? NSObject, let bo = b as? NSObject { return ao.isEqual(bo) }
    return false
}

public func baboonDeepHashCode(_ value: Any?) -> Int {
    guard let v = value else { return 0 }
    if let m = v as? [String: Any] {
        var h: Int = 0
        // unordered hash
        for (k, val) in m {
            h ^= k.hashValue &+ baboonDeepHashCode(val)
        }
        return h
    }
    if let l = v as? [Any] {
        var h: Int = 17
        for item in l {
            h = h &* 31 &+ baboonDeepHashCode(item)
        }
        return h
    }
    if let s = v as? String { return s.hashValue }
    if let i = v as? Int { return i.hashValue }
    if let d = v as? Double { return d.hashValue }
    if let b = v as? Bool { return b.hashValue }
    if let n = v as? NSNumber { return n.hashValue }
    if let o = v as? NSObject { return o.hash }
    return 0
}
