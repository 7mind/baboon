import Foundation

class BaboonRandom {
    private var rng = SystemRandomNumberGenerator()

    func nextBool() -> Bool { return Bool.random(using: &rng) }

    func nextI08() -> Int8 { return Int8.random(in: Int8.min...Int8.max, using: &rng) }
    func nextI16() -> Int16 { return Int16.random(in: Int16.min...Int16.max, using: &rng) }
    func nextI32() -> Int32 { return Int32.random(in: Int32.min...Int32.max, using: &rng) }
    func nextI64() -> Int64 { return Int64.random(in: Int64.min...Int64.max, using: &rng) }

    func nextU08() -> UInt8 { return UInt8.random(in: UInt8.min...UInt8.max, using: &rng) }
    func nextU16() -> UInt16 { return UInt16.random(in: UInt16.min...UInt16.max, using: &rng) }
    func nextU32() -> UInt32 { return UInt32.random(in: UInt32.min...UInt32.max, using: &rng) }
    func nextU64() -> UInt64 { return UInt64.random(in: UInt64.min...UInt64.max, using: &rng) }

    func nextF32() -> Float {
        let raw = Float.random(in: -500.0...500.0, using: &rng)
        // Round-trip through bitPattern to ensure UEBA binary precision
        return Float(bitPattern: raw.bitPattern)
    }

    func nextF64() -> Double {
        return Double.random(in: -5e14...5e14, using: &rng)
    }

    func nextDecimal() -> BaboonDecimal {
        let intPart = Int.random(in: -499999...499999, using: &rng)
        let fracPart = Int.random(in: 0...999999, using: &rng)
        let fracStr = String(format: "%06d", fracPart)
        return BaboonDecimal("\\(intPart).\\(fracStr)")
    }

    func nextString() -> String {
        let length = Int.random(in: 1...20, using: &rng)
        let chars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
        let charsArray = Array(chars)
        return String((0..<length).map { _ in charsArray[Int.random(in: 0..<charsArray.count, using: &rng)] })
    }

    func nextBytes() -> Data {
        let length = Int.random(in: 1...32, using: &rng)
        return Data((0..<length).map { _ in UInt8.random(in: 0...255, using: &rng) })
    }

    func nextUuid() -> UUID {
        return UUID()
    }

    func nextTsu() -> Date {
        let year = 2000 + Int.random(in: 0..<30, using: &rng)
        let month = 1 + Int.random(in: 0..<12, using: &rng)
        let day = 1 + Int.random(in: 0..<28, using: &rng)
        let hour = Int.random(in: 0..<24, using: &rng)
        let minute = Int.random(in: 0..<60, using: &rng)
        let second = Int.random(in: 0..<60, using: &rng)
        let millis = Int.random(in: 0..<1000, using: &rng)

        var calendar = Calendar(identifier: .gregorian)
        calendar.timeZone = TimeZone(identifier: "UTC")!
        var comps = DateComponents()
        comps.year = year
        comps.month = month
        comps.day = day
        comps.hour = hour
        comps.minute = minute
        comps.second = second
        comps.nanosecond = millis * 1_000_000
        return calendar.date(from: comps)!
    }

    func nextTso() -> BaboonDateTimeOffset {
        let dt = nextTsu()
        let offsetHours = Int64(Int.random(in: -12...12, using: &rng))
        let offsetMillis = offsetHours * 3600000
        return BaboonDateTimeOffset(
            epochMillis: Int64(dt.timeIntervalSince1970 * 1000),
            offsetMillis: offsetMillis,
            kind: "offset"
        )
    }

    func nextIntRange(_ max: Int) -> Int {
        return Int.random(in: 0..<max, using: &rng)
    }

    func oneOf<T>(_ items: [T]) -> T {
        return items[Int.random(in: 0..<items.count, using: &rng)]
    }

    func mkList<T>(_ gen: () -> T) -> [T] {
        let length = Int.random(in: 1...5, using: &rng)
        return (0..<length).map { _ in gen() }
    }

    func mkSet<T: Hashable>(_ gen: () -> T) -> Set<T> {
        let length = Int.random(in: 1...5, using: &rng)
        var result = Set<T>()
        var attempts = 0
        while result.count < length && attempts < length * 10 {
            result.insert(gen())
            attempts += 1
        }
        return result
    }

    func mkMap<K: Hashable, V>(_ genKey: () -> K, _ genValue: () -> V) -> [K: V] {
        let length = Int.random(in: 1...5, using: &rng)
        var result = [K: V]()
        var attempts = 0
        while result.count < length && attempts < length * 10 {
            result[genKey()] = genValue()
            attempts += 1
        }
        return result
    }

    func mkOptional<T>(_ gen: () -> T) -> T? {
        return Bool.random(using: &rng) ? gen() : nil
    }

    func mkEnum<T>(_ values: [T]) -> T {
        return values[Int.random(in: 0..<values.count, using: &rng)]
    }
}

class BaboonRandomFactory {
    static func create() -> BaboonRandom {
        return BaboonRandom()
    }
}
