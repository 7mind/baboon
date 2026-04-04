package baboon.runtime.shared

/**
 * Multiplatform decimal type compatible with .NET Decimal wire format.
 * Stores the raw 4 int components: lo, mid, hi (96-bit unsigned mantissa) and flags (sign + scale).
 */
data class BaboonDecimal(val lo: Int, val mid: Int, val hi: Int, val flags: Int) : Comparable<BaboonDecimal> {
    val scale: Int get() = (flags ushr 16) and 0xFF
    val isNegative: Boolean get() = (flags and 0x80000000.toInt()) != 0

    fun toDouble(): Double {
        val loLong = lo.toLong() and 0xFFFFFFFFL
        val midLong = mid.toLong() and 0xFFFFFFFFL
        val hiLong = hi.toLong() and 0xFFFFFFFFL
        val mantissa = hiLong.toDouble() * 4294967296.0 * 4294967296.0 + midLong * 4294967296.0 + loLong
        val sign = if (isNegative) -1.0 else 1.0
        var divisor = 1.0
        repeat(scale) { divisor *= 10.0 }
        return sign * mantissa / divisor
    }

    override fun toString(): String {
        val loLong = lo.toLong() and 0xFFFFFFFFL
        val midLong = mid.toLong() and 0xFFFFFFFFL
        val hiLong = hi.toLong() and 0xFFFFFFFFL

        // Build the mantissa as a string via repeated division
        var digits = mutableListOf<Char>()
        var carry = 0L
        val parts = longArrayOf(hiLong, midLong, loLong)
        do {
            carry = 0L
            for (i in parts.indices) {
                val cur = carry * 4294967296L + parts[i]
                parts[i] = cur / 10
                carry = cur % 10
            }
            digits.add(('0' + carry.toInt()))
        } while (parts.any { it != 0L })
        digits.reverse()

        val mantissaStr = digits.joinToString("")
        val result = if (scale > 0) {
            val padded = mantissaStr.padStart(scale + 1, '0')
            val intPart = padded.substring(0, padded.length - scale)
            val fracPart = padded.substring(padded.length - scale)
            "$intPart.$fracPart"
        } else {
            mantissaStr
        }
        return if (isNegative) "-$result" else result
    }

    override operator fun compareTo(other: BaboonDecimal): Int {
        return this.toDouble().compareTo(other.toDouble())
    }

    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (other !is BaboonDecimal) return false
        // Normalize: compare by value, not by representation
        return this.toString() == other.toString()
    }

    override fun hashCode(): Int = toString().hashCode()

    companion object {
        val ZERO = BaboonDecimal(0, 0, 0, 0)

        fun fromDouble(d: Double): BaboonDecimal {
            if (d == 0.0) return ZERO
            val s = doubleToPlainString(d)
            return fromString(s)
        }

        private fun doubleToPlainString(d: Double): String {
            val s = d.toString()
            if ('E' !in s && 'e' !in s) return s
            val eIdx = s.indexOfFirst { it == 'E' || it == 'e' }
            val mantissa = s.substring(0, eIdx)
            val exponent = s.substring(eIdx + 1).toInt()
            val negative = mantissa.startsWith("-")
            val digits = mantissa.replace("-", "").replace(".", "")
            val dotIdx = mantissa.indexOf('.')
            val intDigits = if (dotIdx >= 0) (if (negative) dotIdx - 1 else dotIdx) else digits.length
            val newDotPos = intDigits + exponent
            return buildString {
                if (negative) append('-')
                if (newDotPos <= 0) {
                    append("0.")
                    repeat(-newDotPos) { append('0') }
                    append(digits)
                } else if (newDotPos >= digits.length) {
                    append(digits)
                    repeat(newDotPos - digits.length) { append('0') }
                } else {
                    append(digits.substring(0, newDotPos))
                    append('.')
                    append(digits.substring(newDotPos))
                }
            }
        }

        fun fromString(s: String): BaboonDecimal {
            val trimmed = s.trim()
            val negative = trimmed.startsWith("-")
            val abs = if (negative) trimmed.substring(1) else trimmed

            val dotIndex = abs.indexOf('.')
            val (intPart, fracPart) = if (dotIndex >= 0) {
                abs.substring(0, dotIndex) to abs.substring(dotIndex + 1).trimEnd('0')
            } else {
                abs to ""
            }
            val scale = fracPart.length
            val digits = intPart + fracPart

            // Parse digits into lo/mid/hi (96-bit)
            var lo = 0L
            var mid = 0L
            var hi = 0L
            for (ch in digits) {
                // Multiply by 10
                val carry0 = lo * 10
                val carry1 = mid * 10 + (carry0 ushr 32)
                val carry2 = hi * 10 + (carry1 ushr 32)
                lo = carry0 and 0xFFFFFFFFL
                mid = carry1 and 0xFFFFFFFFL
                hi = carry2 and 0xFFFFFFFFL
                // Add digit
                lo += (ch - '0')
                if (lo > 0xFFFFFFFFL) { mid += lo ushr 32; lo = lo and 0xFFFFFFFFL }
                if (mid > 0xFFFFFFFFL) { hi += mid ushr 32; mid = mid and 0xFFFFFFFFL }
            }

            val sign = if (negative) 0x80000000.toInt() else 0
            val flags = sign or (scale shl 16)
            return BaboonDecimal(lo.toInt(), mid.toInt(), hi.toInt(), flags)
        }
    }
}
