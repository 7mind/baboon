package baboon.runtime.shared

import java.nio.charset.Charset
import java.nio.charset.StandardCharsets

class ByteString private constructor(private val bytes: ByteArray) : Comparable<ByteString> {
    val length: Int get() = bytes.size
    val isEmpty: Boolean get() = bytes.isEmpty()
    val isNotEmpty: Boolean get() = bytes.isNotEmpty()

    operator fun get(index: Int): Byte = bytes[index]

    fun toArray(): ByteArray = bytes.copyOf()

    fun underlyingUnsafe(): ByteArray = bytes

    fun concat(other: ByteString): ByteString {
        val result = ByteArray(bytes.size + other.bytes.size)
        System.arraycopy(bytes, 0, result, 0, bytes.size)
        System.arraycopy(other.bytes, 0, result, bytes.size, other.bytes.size)
        return ByteString(result)
    }

    operator fun plus(other: ByteString): ByteString = concat(other)

    override fun compareTo(other: ByteString): Int {
        val minLength = minOf(bytes.size, other.bytes.size)
        for (i in 0 until minLength) {
            val cmp = (bytes[i].toInt() and 0xFF) - (other.bytes[i].toInt() and 0xFF)
            if (cmp != 0) return cmp
        }
        return bytes.size - other.bytes.size
    }

    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (other !is ByteString) return false
        return bytes.contentEquals(other.bytes)
    }

    override fun hashCode(): Int {
        var hash = 17
        for (b in bytes) {
            hash = hash * 31 + b
        }
        return hash
    }

    fun toString(charset: Charset): String = String(bytes, charset)
    override fun toString(): String = toString(StandardCharsets.UTF_8)

    fun toHexString(): String = bytes.joinToString("") { "%02X".format(it) }

    fun substring(startIndex: Int, length: Int): ByteString {
        require(startIndex >= 0 && startIndex < bytes.size) { "Start index: $startIndex" }
        require(length >= 0 && startIndex + length <= bytes.size) { "Length: $length" }
        val result = ByteArray(length)
        System.arraycopy(bytes, startIndex, result, 0, length)
        return ByteString(result)
    }

    fun slice(from: Int, until: Int): ByteString = substring(from, until - from)

    fun take(n: Int): ByteString = substring(0, minOf(n, bytes.size))
    fun drop(n: Int): ByteString {
        val dropCount = minOf(n, bytes.size)
        return substring(dropCount, maxOf(0, bytes.size - dropCount))
    }

    companion object {
        fun of(bytes: ByteArray): ByteString = ByteString(bytes.copyOf())

        @JvmName("ofBytes")
        fun of(vararg bytes: Byte): ByteString = ByteString(bytes.copyOf())

        fun fromString(string: String, charset: Charset = StandardCharsets.UTF_8): ByteString =
            ByteString(string.toByteArray(charset))

        fun fromHexString(hexString: String): ByteString = parseHex(hexString)

        fun parseHex(hexString: String): ByteString {
            val cleanHex = hexString.filter { !it.isWhitespace() && it != ':' && it != '-' }
            if (cleanHex.isEmpty()) return empty

            require(cleanHex.length % 2 == 0) {
                "Invalid hex string length: ${cleanHex.length}. Hex string must have even length."
            }

            val bytes = cleanHex.chunked(2).map { it.toInt(16).toByte() }.toByteArray()
            return ByteString(bytes)
        }

        fun tryParse(hexString: String): Result<ByteString> = runCatching { parseHex(hexString) }

        val empty: ByteString = ByteString(ByteArray(0))
    }
}
