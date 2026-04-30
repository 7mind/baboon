import Foundation

/// Runtime helpers for the `id` toString / parseRepr machinery defined in
/// docs/spec/identifier-repr.md. The Swift backend (PR-57c) uses these helpers
/// from emitted code; conformance to the spec is the contract.
///
/// Mirrors BaboonIdentifierRepr.{java,kt} in API and behaviour. Result type:
/// existing project convention — emitted decoders return
/// `BaboonEither<String, T>` (the Swift sum already shipped in
/// `baboon_runtime.swift`). Errors are human-readable strings — the same
/// content as the JVM-side helpers — so cross-backend test assertions can
/// match on the same substrings.
public enum BaboonIdentifierRepr {

    // Defensive: numeric Char constants rather than quoted-character literals
    // so the emitted source survives any future template-escape pass.
    private static let BS:  Character = "\u{5C}" // ASCII backslash
    private static let HSH: Character = "#"
    private static let COL: Character = ":"
    private static let OBR: Character = "{"
    private static let CBR: Character = "}"

    private static let bsByte: UInt8  = 0x5C
    private static let hshByte: UInt8 = 0x23
    private static let colByte: UInt8 = 0x3A
    private static let obrByte: UInt8 = 0x7B
    private static let cbrByte: UInt8 = 0x7D

    /// Backslash-escape the 5 metacharacters per spec §4.2.
    public static func escapeStr(_ s: String) -> String {
        var out = ""
        out.reserveCapacity(s.count)
        for c in s {
            if c == BS || c == HSH || c == COL || c == OBR || c == CBR {
                out.append(BS)
            }
            out.append(c)
        }
        return out
    }

    /// Lowercase hex, no separators, per spec §3 / §4.4.
    public static func bytesToHex(_ data: Data) -> String {
        var out = ""
        out.reserveCapacity(data.count * 2)
        for b in data {
            out.append(toLowerHexDigit(Int(b >> 4) & 0xF))
            out.append(toLowerHexDigit(Int(b) & 0xF))
        }
        return out
    }

    private static func toLowerHexDigit(_ v: Int) -> Character {
        let c: UInt8 = v < 10 ? UInt8(v) &+ 0x30 : UInt8(v - 10) &+ 0x61
        return Character(UnicodeScalar(c))
    }

    /// Render a `tsu` per spec §3: RFC 3339 UTC, milliseconds, trailing `Z`,
    /// exactly 24 characters. Uses `Date` like the rest of the Swift runtime.
    public static func tsuToString(_ dt: Date) -> String {
        // Reuse BaboonTimeFormats.formatUtc — already produces the 24-char
        // form `yyyy-MM-ddTHH:mm:ss.SSSZ` per spec §3 (verified by the existing
        // JSON wire helper test suite). Keeps a single canonical formatter.
        return BaboonTimeFormats.formatUtc(dt)
    }

    /// Render a `tso` per spec §3: RFC 3339 with explicit `±HH:MM` offset,
    /// milliseconds, exactly 29 characters. Never emits `Z` shorthand.
    public static func tsoToString(_ dto: BaboonDateTimeOffset) -> String {
        return BaboonTimeFormats.formatOffset(dto)
    }

    /// Render an unsigned-i64 as decimal. Swift's UInt64 is natively unsigned.
    public static func u64ToString(_ v: UInt64) -> String {
        return String(v)
    }

    /// Render a `bit` per spec §3 — exact lowercase ASCII.
    public static func bitToString(_ b: Bool) -> String {
        return b ? "true" : "false"
    }

    public static func parseTsuRepr(_ s: String) -> BaboonEither<String, Date> {
        if s.count != 24 {
            return .left("tsu repr must be 24 chars, got \(s.count)")
        }
        if !s.hasSuffix("Z") {
            return .left("tsu repr must end with 'Z', got: \(s)")
        }
        // BaboonTimeFormats.parseUtc has assertion-based validation for
        // malformed inputs; pre-validate the 24-char fixed structure here so
        // we surface a clean Left<String> instead of crashing on the assert.
        if !isValidTsuShape(s) {
            return .left("could not parse tsu: \(s)")
        }
        let parsed = BaboonTimeFormats.parseUtc(s)
        return .right(parsed)
    }

    public static func parseTsoRepr(_ s: String) -> BaboonEither<String, BaboonDateTimeOffset> {
        if s.count != 29 {
            return .left("tso repr must be 29 chars, got \(s.count)")
        }
        if !isValidTsoShape(s) {
            return .left("could not parse tso: \(s)")
        }
        let parsed = BaboonTimeFormats.parseOffset(s)
        return .right(parsed)
    }

    private static func isValidTsuShape(_ s: String) -> Bool {
        // Layout: yyyy-MM-ddTHH:mm:ss.SSSZ
        let bytes = Array(s.utf8)
        guard bytes.count == 24 else { return false }
        if bytes[4] != 0x2D { return false } // '-'
        if bytes[7] != 0x2D { return false }
        if bytes[10] != 0x54 { return false } // 'T'
        if bytes[13] != 0x3A { return false } // ':'
        if bytes[16] != 0x3A { return false }
        if bytes[19] != 0x2E { return false } // '.'
        if bytes[23] != 0x5A { return false } // 'Z'
        for i in tsuDigitPositions {
            if !isAsciiDigit(bytes[i]) { return false }
        }
        return true
    }

    private static let tsuDigitPositions: [Int] =
        [0, 1, 2, 3, 5, 6, 8, 9, 11, 12, 14, 15, 17, 18, 20, 21, 22]
    private static let tsoDigitPositions: [Int] =
        [0, 1, 2, 3, 5, 6, 8, 9, 11, 12, 14, 15, 17, 18, 20, 21, 22, 24, 25, 27, 28]

    private static func isValidTsoShape(_ s: String) -> Bool {
        // Layout: yyyy-MM-ddTHH:mm:ss.SSS±HH:MM
        let bytes = Array(s.utf8)
        guard bytes.count == 29 else { return false }
        if bytes[4] != 0x2D { return false }
        if bytes[7] != 0x2D { return false }
        if bytes[10] != 0x54 { return false }
        if bytes[13] != 0x3A { return false }
        if bytes[16] != 0x3A { return false }
        if bytes[19] != 0x2E { return false }
        if !(bytes[23] == 0x2B || bytes[23] == 0x2D) { return false } // ± sign
        if bytes[26] != 0x3A { return false }
        for i in tsoDigitPositions {
            if !isAsciiDigit(bytes[i]) { return false }
        }
        return true
    }

    private static func isAsciiDigit(_ b: UInt8) -> Bool {
        return b >= 0x30 && b <= 0x39
    }

    /// Decode `bytes` from lowercase hex. Empty string is legal (empty bytes).
    public static func parseBytesHex(_ s: String) -> BaboonEither<String, Data> {
        if s.isEmpty {
            return .right(Data())
        }
        if (s.count & 1) != 0 {
            return .left("odd-length hex sequence: \(s)")
        }
        let bytes = Array(s.utf8)
        for b in bytes {
            let isLowerHex = (b >= 0x30 && b <= 0x39) || (b >= 0x61 && b <= 0x66)
            if !isLowerHex {
                return .left("non-lowercase or non-hex character in: \(s)")
            }
        }
        var out = [UInt8](repeating: 0, count: bytes.count / 2)
        var i = 0
        while i < bytes.count {
            let hi = hexDigit(bytes[i])
            let lo = hexDigit(bytes[i + 1])
            out[i / 2] = (hi << 4) | lo
            i += 2
        }
        return .right(Data(out))
    }

    private static func hexDigit(_ b: UInt8) -> UInt8 {
        if b >= 0x30 && b <= 0x39 { return b - 0x30 }
        if b >= 0x61 && b <= 0x66 { return 10 + (b - 0x61) }
        return 0
    }

    public static func parseBit(_ s: String) -> BaboonEither<String, Bool> {
        switch s {
        case "true":  return .right(true)
        case "false": return .right(false)
        default:      return .left("expected 'true' or 'false' but found '\(s)'")
        }
    }

    /// Lowercase canonical-form check for uid strings (spec §3 / §5.4):
    /// `[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}`.
    public static func isCanonicalUid(_ s: String) -> Bool {
        let bytes = Array(s.utf8)
        guard bytes.count == 36 else { return false }
        let dashPos: Set<Int> = [8, 13, 18, 23]
        for p in dashPos {
            if bytes[p] != 0x2D { return false }
        }
        for i in 0..<36 {
            if dashPos.contains(i) { continue }
            let b = bytes[i]
            let isLowerHex = (b >= 0x30 && b <= 0x39) || (b >= 0x61 && b <= 0x66)
            if !isLowerHex { return false }
        }
        return true
    }

    /// Cursor-based parser for parseRepr decoders. Schema-directed; the caller
    /// (the emitted `<TypeName>Codec.parseRepr`) drives the field sequence per
    /// declared type and order.
    ///
    /// Reference type so the emitted decoder can mutate `pos` in place across
    /// nested-id recursive calls. Stores source as `[UInt8]` (ASCII metachars
    /// are byte-comparable) but reads back as Swift `String` slices via the
    /// retained `String` for non-ASCII passthrough.
    public final class Cursor {
        private let source: String
        private let bytes: [UInt8]
        private(set) public var pos: Int = 0

        public init(_ source: String) {
            self.source = source
            self.bytes = Array(source.utf8)
        }

        public func position() -> Int { return pos }
        public func atEnd() -> Bool { return pos >= bytes.count }

        public func expect(_ c: Character) -> BaboonEither<String, Void> {
            // All structural metachars are single-byte ASCII; we operate on bytes
            // for speed.
            guard let ascii = c.asciiValue else {
                return .left("Cursor.expect only handles ASCII metachars; got: \(c)")
            }
            if pos >= bytes.count {
                return .left("expected '\(c)' at \(pos) but reached end of input")
            }
            if bytes[pos] != ascii {
                let found = Character(UnicodeScalar(bytes[pos]))
                return .left("expected '\(c)' at \(pos) but found '\(found)'")
            }
            pos += 1
            return .right(())
        }

        public func expectLiteral(_ lit: String) -> BaboonEither<String, Void> {
            let litBytes = Array(lit.utf8)
            if pos + litBytes.count > bytes.count {
                return .left("expected literal '\(lit)' at \(pos) but reached end of input")
            }
            for i in 0..<litBytes.count {
                if bytes[pos + i] != litBytes[i] {
                    return .left("expected literal '\(lit)' at \(pos)")
                }
            }
            pos += litBytes.count
            return .right(())
        }

        /// Read until the next bare metachar in `:#{}`. Backslash escapes are
        /// NOT processed here — see readStrField for that. Used for primitive
        /// consumption (numbers, uuids, hex bytes).
        public func readUntilStructural() -> String {
            let start = pos
            while pos < bytes.count {
                let b = bytes[pos]
                if b == BaboonIdentifierRepr.colByte
                    || b == BaboonIdentifierRepr.hshByte
                    || b == BaboonIdentifierRepr.obrByte
                    || b == BaboonIdentifierRepr.cbrByte {
                    break
                }
                pos += 1
            }
            // ASCII-only stop conditions ⇒ slice is on UTF-8 boundary.
            return String(bytes: bytes[start..<pos], encoding: .utf8) ?? ""
        }

        /// Consume exactly n characters (Unicode scalars) as a fixed-width
        /// lexeme. Used for tsu/tso (which contain `:` characters inside the
        /// lexeme).
        public func readFixed(_ n: Int) -> BaboonEither<String, String> {
            let remaining = String(bytes: bytes[pos..<bytes.count], encoding: .utf8) ?? ""
            var taken = 0
            var endByteOffset = 0
            var iter = remaining.unicodeScalars.makeIterator()
            var byteCursor = 0
            while taken < n {
                guard let sc = iter.next() else { break }
                taken += 1
                byteCursor += sc.utf8.count
                endByteOffset = byteCursor
            }
            if taken < n {
                return .left("expected \(n) chars at \(pos) but only \(taken) remain")
            }
            let result = String(bytes: bytes[pos..<(pos + endByteOffset)], encoding: .utf8) ?? ""
            pos += endByteOffset
            return .right(result)
        }

        /// Read a `str` field value with backslash-unescaping per spec §5.5.
        public func readStrField() -> BaboonEither<String, String> {
            var out: [UInt8] = []
            while pos < bytes.count {
                let b = bytes[pos]
                if b == BaboonIdentifierRepr.colByte
                    || b == BaboonIdentifierRepr.hshByte
                    || b == BaboonIdentifierRepr.obrByte
                    || b == BaboonIdentifierRepr.cbrByte {
                    return .right(String(bytes: out, encoding: .utf8) ?? "")
                }
                if b == BaboonIdentifierRepr.bsByte {
                    if pos + 1 >= bytes.count {
                        return .left("trailing backslash at \(pos)")
                    }
                    let nxt = bytes[pos + 1]
                    if nxt == BaboonIdentifierRepr.bsByte
                        || nxt == BaboonIdentifierRepr.hshByte
                        || nxt == BaboonIdentifierRepr.colByte
                        || nxt == BaboonIdentifierRepr.obrByte
                        || nxt == BaboonIdentifierRepr.cbrByte {
                        out.append(nxt)
                        pos += 2
                    } else {
                        return .left("invalid escape at \(pos)")
                    }
                } else {
                    out.append(b)
                    pos += 1
                }
            }
            return .right(String(bytes: out, encoding: .utf8) ?? "")
        }
    }

    /// Validate header of an identifier-repr: `<simpleName>:<version>#`.
    public static func parseHeader(
        _ cursor: Cursor,
        _ expectedSimpleName: String,
        _ expectedVersion: String
    ) -> BaboonEither<String, Void> {
        let nameLit = cursor.readUntilStructural()
        if nameLit != expectedSimpleName {
            return .left("expected name '\(expectedSimpleName)' but found '\(nameLit)'")
        }
        let afterName = cursor.expect(":")
        if case .left = afterName { return afterName }
        let verLit = cursor.readUntilStructural()
        if verLit != expectedVersion {
            return .left("expected version '\(expectedVersion)' but found '\(verLit)'")
        }
        return cursor.expect("#")
    }

    /// Validate field-name segment: `<expectedFieldName>:`.
    public static func parseFieldName(
        _ cursor: Cursor,
        _ expectedFieldName: String
    ) -> BaboonEither<String, Void> {
        let name = cursor.readUntilStructural()
        if name != expectedFieldName {
            return .left("expected field name '\(expectedFieldName)' but found '\(name)'")
        }
        return cursor.expect(":")
    }
}
