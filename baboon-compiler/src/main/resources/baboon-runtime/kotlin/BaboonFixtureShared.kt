package baboon.fixture

import baboon.runtime.shared.BaboonEnum
import baboon.runtime.shared.ByteString
import java.math.BigDecimal
import java.time.Instant
import java.time.OffsetDateTime
import java.time.ZoneOffset
import java.util.UUID
import kotlin.random.Random

interface BaboonRandom {
    fun nextBit(): Boolean

    fun nextI08(): Byte
    fun nextI16(): Short
    fun nextI32(): Int
    fun nextI64(): Long

    fun nextU08(): UByte
    fun nextU16(): UShort
    fun nextU32(): UInt
    fun nextU64(): ULong

    fun nextF32(): Float
    fun nextF64(): Double
    fun nextF128(): BigDecimal

    fun nextTsu(): OffsetDateTime
    fun nextTso(): OffsetDateTime

    fun nextUid(): UUID
    fun nextString(): String
    fun nextByteString(): ByteString

    fun <T> mkNullable(element: () -> T): T?
    fun <T> mkEnum(meta: BaboonEnum<T>): T
    fun <T> mkList(element: () -> T): List<T>
    fun <T> mkSet(element: () -> T): Set<T>
    fun <K, V> mkMap(k: () -> K, v: () -> V): Map<K, V>

    fun <T> oneOf(elements: List<(BaboonRandom) -> T>): T
    fun <T> randomElement(elements: List<T>): T
}

object BaboonRandomFactory {
    fun default(): BaboonRandom = BaboonRandomImpl(Random.Default)
}

class BaboonRandomImpl(private val rnd: Random) : BaboonRandom {
    override fun nextBit(): Boolean = rnd.nextBoolean()

    override fun nextI08(): Byte = rnd.nextInt().toByte()
    override fun nextI16(): Short = rnd.nextInt().toShort()
    override fun nextI32(): Int = rnd.nextInt()
    override fun nextI64(): Long = rnd.nextLong()

    override fun nextU08(): UByte = rnd.nextInt().toUByte()
    override fun nextU16(): UShort = rnd.nextInt().toUShort()
    override fun nextU32(): UInt = rnd.nextInt().toUInt()
    override fun nextU64(): ULong = rnd.nextLong().toULong()

    override fun nextF32(): Float = rnd.nextFloat()
    override fun nextF64(): Double = rnd.nextDouble()
    override fun nextF128(): BigDecimal = BigDecimal.valueOf(rnd.nextDouble())

    private val earliest: OffsetDateTime = OffsetDateTime.parse("2020-01-01T00:00:00Z")
    private val latest: OffsetDateTime = OffsetDateTime.parse("2099-12-31T23:59:59Z")

    private fun generateRandomOffsetDateTime(startInclusive: OffsetDateTime, endInclusive: OffsetDateTime): OffsetDateTime {
        val minSeconds = startInclusive.toEpochSecond()
        val maxSeconds = endInclusive.toEpochSecond()
        val randomSeconds = minSeconds + rnd.nextLong(maxSeconds - minSeconds)
        return OffsetDateTime.ofInstant(Instant.ofEpochSecond(randomSeconds), ZoneOffset.UTC)
    }

    override fun nextTsu(): OffsetDateTime {
        return generateRandomOffsetDateTime(earliest, latest).withOffsetSameInstant(ZoneOffset.UTC)
    }

    override fun nextTso(): OffsetDateTime {
        return generateRandomOffsetDateTime(earliest, latest).withOffsetSameInstant(ZoneOffset.ofHours(rnd.nextInt(37) - 18))
    }

    override fun nextUid(): UUID = UUID.randomUUID()
    override fun nextString(): String {
        val chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
        return (1..10).map { chars[rnd.nextInt(chars.length)] }.joinToString("")
    }
    override fun nextByteString(): ByteString {
        val length = rnd.nextInt(21)
        val bytes = ByteArray(length)
        rnd.nextBytes(bytes)
        return ByteString.of(bytes)
    }

    override fun <T> mkList(element: () -> T): List<T> = (0 until rnd.nextInt(20)).map { element() }.shuffled()
    override fun <T> mkSet(element: () -> T): Set<T> = (0 until rnd.nextInt(20)).map { element() }.toSet()
    override fun <K, V> mkMap(k: () -> K, v: () -> V): Map<K, V> = (0 until rnd.nextInt(20)).associate { k() to v() }
    override fun <T> mkNullable(element: () -> T): T? = element()

    override fun <T> mkEnum(meta: BaboonEnum<T>): T = meta.all().shuffled().first()

    override fun <T> oneOf(elements: List<(BaboonRandom) -> T>): T {
        return elements[rnd.nextInt(elements.size)](this)
    }

    override fun <T> randomElement(elements: List<T>): T {
        return elements[rnd.nextInt(elements.size)]
    }
}
