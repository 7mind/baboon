import Foundation

// --- JSON decode helpers (GitHub 7mind/baboon#106) ---
//
// Generated JSON decoders read untrusted input through these helpers. A missing required field,
// a value of the wrong JSON type, an unparsable string (number, uuid, map key) or a duplicate
// decoded map key throws `BaboonCodecException.decoderFailure` — never a force-cast or
// force-unwrap trap. `what` names the decoded location (`Type.field`) for the error message.

public enum BaboonJsonDecode {
    public static func field(_ obj: [String: Any], _ name: String, _ owner: String) throws -> Any {
        guard let value = obj[name] else {
            throw failure("\(owner): missing required field '\(name)'")
        }
        return value
    }

    public static func bool(_ wire: Any, _ what: String) throws -> Bool {
        guard let v = wire as? Bool else { throw mismatch(wire, "bool", what) }
        return v
    }

    public static func number(_ wire: Any, _ what: String) throws -> NSNumber {
        guard let v = wire as? NSNumber else { throw mismatch(wire, "number", what) }
        return v
    }

    public static func string(_ wire: Any, _ what: String) throws -> String {
        guard let v = wire as? String else { throw mismatch(wire, "string", what) }
        return v
    }

    public static func array(_ wire: Any, _ what: String) throws -> [Any] {
        guard let v = wire as? [Any] else { throw mismatch(wire, "array", what) }
        return v
    }

    public static func object(_ wire: Any, _ what: String) throws -> [String: Any] {
        guard let v = wire as? [String: Any] else { throw mismatch(wire, "object", what) }
        return v
    }

    /// `i64` wire form: a decimal string (canonical) or a JSON number (legacy).
    public static func int64(_ wire: Any, _ what: String) throws -> Int64 {
        if let s = wire as? String { return try parse(s, what, Int64.self) }
        return Int64(truncatingIfNeeded: try number(wire, what).int64Value)
    }

    /// `u64` wire form: a decimal string (canonical) or a JSON number (legacy).
    public static func uint64(_ wire: Any, _ what: String) throws -> UInt64 {
        if let s = wire as? String { return try parse(s, what, UInt64.self) }
        return UInt64(truncatingIfNeeded: try number(wire, what).uint64Value)
    }

    /// `f128` wire form: a decimal string (canonical) or a JSON number.
    public static func decimal(_ wire: Any, _ what: String) throws -> BaboonDecimal {
        if let s = wire as? String { return BaboonDecimal(s) }
        return BaboonDecimal(String(describing: try number(wire, what)))
    }

    public static func uuid(_ wire: Any, _ what: String) throws -> UUID {
        let s = try string(wire, what)
        guard let v = UUID(uuidString: s) else { throw failure("\(what): malformed uuid '\(s)'") }
        return v
    }

    /// Parses a builtin scalar from its string form (map keys, string-encoded 64-bit integers).
    public static func parse<T: LosslessStringConvertible>(_ s: String, _ what: String, _ type: T.Type) throws -> T {
        guard let v = T(s) else { throw failure("\(what): malformed key '\(s)' for \(T.self)") }
        return v
    }

    /// Builds a map from decoded entries; two JSON keys that decode to the same key throw.
    public static func dictionary<K: Hashable, V>(_ entries: [(K, V)], _ what: String) throws -> [K: V] {
        var result = [K: V](minimumCapacity: entries.count)
        for (key, value) in entries {
            if result.index(forKey: key) != nil {
                throw failure("\(what): duplicate map key \(key)")
            }
            result[key] = value
        }
        return result
    }

    private static func mismatch(_ wire: Any, _ expected: String, _ what: String) -> BaboonCodecException {
        let actual = wire is NSNull ? "null" : String(describing: type(of: wire))
        return failure("\(what): expected \(expected), got \(actual)")
    }

    private static func failure(_ message: String) -> BaboonCodecException {
        return BaboonCodecException.decoderFailure(message, nil)
    }
}
