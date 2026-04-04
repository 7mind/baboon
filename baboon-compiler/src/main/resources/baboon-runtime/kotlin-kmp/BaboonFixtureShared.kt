@file:OptIn(ExperimentalUuidApi::class)

package baboon.fixture

import baboon.runtime.shared.BaboonDecimal
import baboon.runtime.shared.BaboonEnum
import baboon.runtime.shared.BaboonOffsetDateTime
import baboon.runtime.shared.ByteString
import kotlinx.datetime.Instant
import kotlinx.datetime.UtcOffset
import kotlin.random.Random
import kotlin.uuid.ExperimentalUuidApi
import kotlin.uuid.Uuid

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
    fun nextF128(): BaboonDecimal

    fun nextTsu(): Instant
    fun nextTso(): BaboonOffsetDateTime

    fun nextUid(): Uuid
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
    override fun nextF128(): BaboonDecimal = BaboonDecimal.fromDouble(rnd.nextDouble())

    override fun nextTsu(): Instant {
        val minSeconds = 1577836800L // 2020-01-01T00:00:00Z
        val maxSeconds = 4102444799L // 2099-12-31T23:59:59Z
        val randomSeconds = minSeconds + rnd.nextLong(maxSeconds - minSeconds)
        return Instant.fromEpochSeconds(randomSeconds)
    }

    override fun nextTso(): BaboonOffsetDateTime {
        val minSeconds = 1577836800L
        val maxSeconds = 4102444799L
        val randomSeconds = minSeconds + rnd.nextLong(maxSeconds - minSeconds)
        val instant = Instant.fromEpochSeconds(randomSeconds)
        val offsetHours = rnd.nextInt(37) - 18
        return BaboonOffsetDateTime(instant, UtcOffset(seconds = offsetHours * 3600))
    }

    override fun nextUid(): Uuid = Uuid.random()
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
