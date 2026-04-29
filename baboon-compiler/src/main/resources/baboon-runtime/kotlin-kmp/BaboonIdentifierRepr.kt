package baboon.runtime.shared

import kotlinx.datetime.Instant
import kotlinx.datetime.UtcOffset

/**
 * Runtime helpers for the `id` toString / parseRepr machinery defined in
 * docs/spec/identifier-repr.md. The KMP-Kotlin backend (PR-57b) uses these
 * helpers from emitted code; conformance to the spec is the contract.
 *
 * KMP commonMain constraint: cannot use any `java.*` API. tsu / tso formatting
 * is implemented from primitive epoch arithmetic so the helper is portable
 * across all Kotlin platforms (JVM, JS, Native, Wasm).
 *
 * API mirrors the JVM-Kotlin counterpart in baboon-runtime/kotlin/. Wire-format
 * outputs are byte-identical between the two.
 */
object BaboonIdentifierRepr {

    private const val BS: Char = 92.toChar() // ASCII backslash
    private const val HSH: Char = '#'
    private const val COL: Char = ':'
    private const val OBR: Char = '{'
    private const val CBR: Char = '}'

    private val LOWER_HEX = Regex("^[0-9a-f]*$")
    private val UID_LOWER = Regex("[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}")

    /** Backslash-escape the 5 metacharacters per spec §4.2. */
    fun escapeStr(s: String): String {
        val sb = StringBuilder(s.length)
        for (c in s) {
            if (c == BS || c == HSH || c == COL || c == OBR || c == CBR) {
                sb.append(BS)
            }
            sb.append(c)
        }
        return sb.toString()
    }

    /** Lowercase hex, no separators, per spec §3 / §4.4. */
    fun bytesToHex(bs: ByteString): String {
        val arr = bs.underlyingUnsafe()
        val sb = StringBuilder(arr.size * 2)
        for (b in arr) {
            val v = b.toInt() and 0xFF
            sb.append(toLowerHexDigit((v ushr 4) and 0xF))
            sb.append(toLowerHexDigit(v and 0xF))
        }
        return sb.toString()
    }

    private fun toLowerHexDigit(v: Int): Char =
        (if (v < 10) ('0'.code + v) else ('a'.code + (v - 10))).toChar()

    /** Render a `tsu` per spec §3: RFC 3339 UTC, milliseconds, trailing `Z`,
     * exactly 24 characters. Input is normalised to UTC. */
    fun tsuToString(instant: Instant): String {
        val epochMs = instant.toEpochMilliseconds()
        return formatUtcMillis(epochMs)
    }

    /** Render a `tso` per spec §3: RFC 3339 with explicit `±HH:MM` offset,
     * milliseconds, exactly 29 characters. Never emits `Z` shorthand.
     *
     * BaboonOffsetDateTime.toString already produces this exact form
     * (see BaboonOffsetDateTime.kt). */
    fun tsoToString(dt: BaboonOffsetDateTime): String = dt.toString()

    /** Render an unsigned-i64 (carried as Kotlin ULong) as decimal. */
    fun u64ToString(v: ULong): String = v.toString()

    /** Render a `bit` per spec §3 — exact lowercase ASCII. */
    fun bitToString(b: Boolean): String = if (b) "true" else "false"

    fun parseTsuRepr(s: String): Either<String, Instant> {
        if (s.length != 24) {
            return Either.Left("tsu repr must be 24 chars, got ${s.length}")
        }
        if (s[23] != 'Z') {
            return Either.Left("tsu repr must end with 'Z', got: $s")
        }
        return try {
            // kotlinx.datetime.Instant.parse accepts ISO-8601 with Z.
            Either.Right(Instant.parse(s))
        } catch (e: IllegalArgumentException) {
            Either.Left("could not parse tsu: $s")
        }
    }

    fun parseTsoRepr(s: String): Either<String, BaboonOffsetDateTime> {
        if (s.length != 29) {
            return Either.Left("tso repr must be 29 chars, got ${s.length}")
        }
        return try {
            Either.Right(BaboonOffsetDateTime.parse(s))
        } catch (e: IllegalArgumentException) {
            Either.Left("could not parse tso: $s")
        }
    }

    /** Decode `bytes` from lowercase hex. Empty string is legal (empty bytes). */
    fun parseBytesHex(s: String): Either<String, ByteString> {
        if (s.isEmpty()) return Either.Right(ByteString.of(ByteArray(0)))
        if ((s.length and 1) != 0) return Either.Left("odd-length hex sequence: $s")
        if (!LOWER_HEX.matches(s)) return Either.Left("non-lowercase or non-hex character in: $s")
        val out = ByteArray(s.length / 2)
        var i = 0
        while (i < s.length) {
            val hi = hexDigit(s[i])
            val lo = hexDigit(s[i + 1])
            out[i / 2] = ((hi shl 4) or lo).toByte()
            i += 2
        }
        return Either.Right(ByteString.of(out))
    }

    private fun hexDigit(c: Char): Int = when (c) {
        in '0'..'9' -> c.code - '0'.code
        in 'a'..'f' -> 10 + (c.code - 'a'.code)
        else -> -1
    }

    fun parseBit(s: String): Either<String, Boolean> = when (s) {
        "true" -> Either.Right(true)
        "false" -> Either.Right(false)
        else -> Either.Left("expected 'true' or 'false' but found '$s'")
    }

    /** Lowercase canonical-form check for uid strings (spec §3 / §5.4). */
    fun isCanonicalUid(s: String): Boolean = UID_LOWER.matches(s)

    // ---------- Internal: epoch-millis → `yyyy-MM-ddTHH:mm:ss.SSSZ` ----------

    private fun formatUtcMillis(epochMs: Long): String {
        // Decompose epoch milliseconds into Y/M/D/H/M/S/ms in UTC. Mirrors
        // BaboonOffsetDateTime's gregorian arithmetic but always at UTC offset 0.
        val totalSeconds = floorDiv(epochMs, 1000L)
        val millis = floorMod(epochMs, 1000L).toInt()

        val days = floorDiv(totalSeconds, 86400L).toInt()
        val timeOfDay = floorMod(totalSeconds, 86400L).toInt()
        val hours = timeOfDay / 3600
        val minutes = (timeOfDay % 3600) / 60
        val seconds = timeOfDay % 60

        // Gregorian calendar from days since epoch (1970-01-01).
        val z = days + 719468
        val era = (if (z >= 0) z else z - 146096) / 146097
        val doe = z - era * 146097
        val yoe = (doe - doe / 1460 + doe / 36524 - doe / 146096) / 365
        val y = yoe + era * 400
        val doy = doe - (365 * yoe + yoe / 4 - yoe / 100)
        val mp = (5 * doy + 2) / 153
        val d = doy - (153 * mp + 2) / 5 + 1
        val m = mp + (if (mp < 10) 3 else -9)
        val year = y + (if (m <= 2) 1 else 0)

        return "${pad(year, 4)}-${pad(m, 2)}-${pad(d, 2)}T${pad(hours, 2)}:${pad(minutes, 2)}:${pad(seconds, 2)}.${pad(millis, 3)}Z"
    }

    private fun pad(v: Int, n: Int): String = v.toString().padStart(n, '0')

    private fun floorDiv(x: Long, y: Long): Long {
        val r = x / y
        return if ((x xor y) < 0 && r * y != x) r - 1 else r
    }

    private fun floorMod(x: Long, y: Long): Long = x - floorDiv(x, y) * y

    /** Cursor-based parser for parseRepr decoders. Schema-directed; the
     * caller (the emitted &lt;TypeName&gt;Codec.parseRepr) drives the field
     * sequence per declared type and order. */
    class Cursor(private val source: String) {
        private var pos: Int = 0

        fun position(): Int = pos
        fun atEnd(): Boolean = pos >= source.length
        fun peek(): Char = source[pos]

        fun expect(c: Char): Either<String, Unit> {
            if (atEnd()) return Either.Left("expected '$c' at $pos but reached end of input")
            if (peek() != c) return Either.Left("expected '$c' at $pos but found '${peek()}'")
            pos += 1
            return Either.Right(Unit)
        }

        fun expectLiteral(lit: String): Either<String, Unit> {
            if (pos + lit.length > source.length) {
                return Either.Left("expected literal '$lit' at $pos but reached end of input")
            }
            for (i in lit.indices) {
                if (source[pos + i] != lit[i]) {
                    return Either.Left("expected literal '$lit' at $pos")
                }
            }
            pos += lit.length
            return Either.Right(Unit)
        }

        fun readUntilStructural(): String {
            val start = pos
            while (pos < source.length) {
                val c = source[pos]
                if (c == COL || c == HSH || c == OBR || c == CBR) {
                    return source.substring(start, pos)
                }
                pos += 1
            }
            return source.substring(start, pos)
        }

        fun readFixed(n: Int): Either<String, String> {
            if (pos + n > source.length) {
                return Either.Left("expected $n chars at $pos but only ${source.length - pos} remain")
            }
            val s = source.substring(pos, pos + n)
            pos += n
            return Either.Right(s)
        }

        fun readStrField(): Either<String, String> {
            val sb = StringBuilder()
            while (pos < source.length) {
                val c = source[pos]
                if (c == COL || c == HSH || c == OBR || c == CBR) {
                    return Either.Right(sb.toString())
                }
                if (c == BS) {
                    if (pos + 1 >= source.length) {
                        return Either.Left("trailing backslash at $pos")
                    }
                    val nxt = source[pos + 1]
                    if (nxt == BS || nxt == HSH || nxt == COL || nxt == OBR || nxt == CBR) {
                        sb.append(nxt)
                        pos += 2
                    } else {
                        return Either.Left("invalid escape at $pos")
                    }
                } else {
                    sb.append(c)
                    pos += 1
                }
            }
            return Either.Right(sb.toString())
        }
    }

    fun parseHeader(cursor: Cursor, expectedSimpleName: String, expectedVersion: String): Either<String, Unit> {
        val nameLit = cursor.readUntilStructural()
        if (nameLit != expectedSimpleName) {
            return Either.Left("expected name '$expectedSimpleName' but found '$nameLit'")
        }
        val afterName = cursor.expect(':')
        if (afterName is Either.Left) return afterName
        val verLit = cursor.readUntilStructural()
        if (verLit != expectedVersion) {
            return Either.Left("expected version '$expectedVersion' but found '$verLit'")
        }
        return cursor.expect('#')
    }

    fun parseFieldName(cursor: Cursor, expectedFieldName: String): Either<String, Unit> {
        val name = cursor.readUntilStructural()
        if (name != expectedFieldName) {
            return Either.Left("expected field name '$expectedFieldName' but found '$name'")
        }
        return cursor.expect(':')
    }
}
