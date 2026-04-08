@file:OptIn(ExperimentalUuidApi::class)

package baboon.runtime.shared

import kotlinx.datetime.Instant
import kotlinx.datetime.UtcOffset
import kotlin.uuid.ExperimentalUuidApi
import kotlin.uuid.Uuid

object BaboonBinTools {
    private const val DOTNET_EPOCH_OFFSET_MS = 62135596800000L
    fun readUid(s: BaboonBinaryReader): Uuid {
        val bytes = ByteArray(16)
        s.readFully(bytes)

        // Convert from .NET GUID byte order to standard UUID byte order
        var b0 = bytes[0]; bytes[0] = bytes[3]; bytes[3] = b0
        var b1 = bytes[1]; bytes[1] = bytes[2]; bytes[2] = b1
        var b4 = bytes[4]; bytes[4] = bytes[5]; bytes[5] = b4
        var b6 = bytes[6]; bytes[6] = bytes[7]; bytes[7] = b6

        return Uuid.fromByteArray(bytes)
    }

    fun writeUid(writer: BaboonBinaryWriter, v: Uuid) {
        val bytes = v.toByteArray()

        // Convert to .NET GUID byte order
        var b0 = bytes[0]; bytes[0] = bytes[3]; bytes[3] = b0
        var b1 = bytes[1]; bytes[1] = bytes[2]; bytes[2] = b1
        var b4 = bytes[4]; bytes[4] = bytes[5]; bytes[5] = b4
        var b6 = bytes[6]; bytes[6] = bytes[7]; bytes[7] = b6

        writer.write(bytes)
    }

    fun readByteString(input: BaboonBinaryReader): ByteString {
        val length = input.readInt()
        val bytes = ByteArray(length)
        input.readFully(bytes)
        return ByteString.of(bytes)
    }

    fun writeByteString(output: BaboonBinaryWriter, bs: ByteString) {
        output.writeInt(bs.length)
        output.write(bs.underlyingUnsafe())
    }

    fun readString(input: BaboonBinaryReader): String {
        var length = 0
        var shift = 0
        var byteRead: Int

        do {
            byteRead = input.readByte().toInt() and 0xFF
            length = length or ((byteRead and 0x7F) shl shift)
            shift += 7
        } while ((byteRead and 0x80) != 0)

        val buffer = ByteArray(length)
        input.readFully(buffer)
        return buffer.decodeToString()
    }

    fun writeString(output: BaboonBinaryWriter, s: String) {
        val bytes = s.encodeToByteArray()
        var value = bytes.size

        do {
            var currentByte = (value and 0x7F).toByte()
            value = value ushr 7
            if (value != 0) currentByte = (currentByte.toInt() or 0x80).toByte()
            output.writeByte(currentByte.toInt())
        } while (value != 0)

        output.write(bytes)
    }

    fun readBaboonDecimal(input: BaboonBinaryReader): BaboonDecimal {
        val lo = input.readInt()
        val mid = input.readInt()
        val hi = input.readInt()
        val flags = input.readInt()
        return BaboonDecimal(lo, mid, hi, flags)
    }

    fun writeBaboonDecimal(output: BaboonBinaryWriter, value: BaboonDecimal) {
        output.writeInt(value.lo)
        output.writeInt(value.mid)
        output.writeInt(value.hi)
        output.writeInt(value.flags)
    }

    fun readTimestamp(wire: BaboonBinaryReader): Instant {
        val dotNetLocalMs = wire.readLong()
        val offsetMillis = wire.readLong()
        @Suppress("UNUSED_VARIABLE")
        val kind = wire.readByte()
        val dotNetUtcMs = dotNetLocalMs - offsetMillis
        val epochMs = dotNetUtcMs - DOTNET_EPOCH_OFFSET_MS
        return Instant.fromEpochMilliseconds(epochMs)
    }

    fun readTimestampOffset(wire: BaboonBinaryReader): BaboonOffsetDateTime {
        val dotNetLocalMs = wire.readLong()
        val offsetMillis = wire.readLong()
        @Suppress("UNUSED_VARIABLE")
        val kind = wire.readByte()
        val dotNetUtcMs = dotNetLocalMs - offsetMillis
        val epochMs = dotNetUtcMs - DOTNET_EPOCH_OFFSET_MS
        val offsetSeconds = (offsetMillis / 1000).toInt()
        return BaboonOffsetDateTime.fromEpochMilliseconds(epochMs, offsetSeconds)
    }

    fun writeTimestamp(writer: BaboonBinaryWriter, ref: Instant) {
        val epochMs = ref.toEpochMilliseconds()
        val dotNetUtcMs = epochMs + DOTNET_EPOCH_OFFSET_MS
        writer.writeLong(dotNetUtcMs) // local = UTC when offset = 0
        writer.writeLong(0L) // offset = 0 for UTC
        writer.writeByte(1) // kind = UTC
    }

    fun writeTimestampOffset(writer: BaboonBinaryWriter, ref: BaboonOffsetDateTime) {
        val epochMs = ref.instant.toEpochMilliseconds()
        val dotNetUtcMs = epochMs + DOTNET_EPOCH_OFFSET_MS
        val offsetMs = ref.offset.totalSeconds * 1000L
        val dotNetLocalMs = dotNetUtcMs + offsetMs
        val kind: Byte = if (ref.offset.totalSeconds == 0) 1 else 0
        writer.writeLong(dotNetLocalMs)
        writer.writeLong(offsetMs)
        writer.writeByte(kind.toInt())
    }
}

/**
 * Little-endian binary reader operating on a ByteArray.
 * KMP replacement for LEDataInputStream.
 */
class BaboonBinaryReader(private val data: ByteArray) {
    private var pos = 0

    fun readByte(): Byte {
        check(pos < data.size) { "End of data" }
        return data[pos++]
    }

    fun readBoolean(): Boolean = readByte() != 0.toByte()

    fun readShort(): Short {
        check(pos + 2 <= data.size) { "End of data" }
        val v = (data[pos].toInt() and 0xFF) or
            ((data[pos + 1].toInt() and 0xFF) shl 8)
        pos += 2
        return v.toShort()
    }

    fun readInt(): Int {
        check(pos + 4 <= data.size) { "End of data" }
        val v = (data[pos].toInt() and 0xFF) or
            ((data[pos + 1].toInt() and 0xFF) shl 8) or
            ((data[pos + 2].toInt() and 0xFF) shl 16) or
            ((data[pos + 3].toInt() and 0xFF) shl 24)
        pos += 4
        return v
    }

    fun readLong(): Long {
        check(pos + 8 <= data.size) { "End of data" }
        val lo = (data[pos].toLong() and 0xFF) or
            ((data[pos + 1].toLong() and 0xFF) shl 8) or
            ((data[pos + 2].toLong() and 0xFF) shl 16) or
            ((data[pos + 3].toLong() and 0xFF) shl 24)
        val hi = (data[pos + 4].toLong() and 0xFF) or
            ((data[pos + 5].toLong() and 0xFF) shl 8) or
            ((data[pos + 6].toLong() and 0xFF) shl 16) or
            ((data[pos + 7].toLong() and 0xFF) shl 24)
        pos += 8
        return lo or (hi shl 32)
    }

    fun readFloat(): Float = Float.fromBits(readInt())
    fun readDouble(): Double = Double.fromBits(readLong())

    fun readFully(b: ByteArray) {
        check(pos + b.size <= data.size) { "End of data" }
        data.copyInto(b, 0, pos, pos + b.size)
        pos += b.size
    }

    fun readUnsignedByte(): Int = readByte().toInt() and 0xFF
    fun readUnsignedShort(): Int = readShort().toInt() and 0xFFFF
}

/**
 * Little-endian binary writer with auto-growing buffer.
 * KMP replacement for LEDataOutputStream.
 */
class BaboonBinaryWriter {
    private var buf = ByteArray(256)
    private var pos = 0

    private fun ensureCapacity(needed: Int) {
        if (pos + needed > buf.size) {
            var newSize = buf.size * 2
            while (newSize < pos + needed) newSize *= 2
            buf = buf.copyOf(newSize)
        }
    }

    fun writeByte(v: Int) {
        ensureCapacity(1)
        buf[pos++] = v.toByte()
    }

    fun writeBoolean(v: Boolean) = writeByte(if (v) 1 else 0)

    fun writeShort(v: Int) {
        ensureCapacity(2)
        buf[pos++] = v.toByte()
        buf[pos++] = (v shr 8).toByte()
    }

    fun writeInt(v: Int) {
        ensureCapacity(4)
        buf[pos++] = v.toByte()
        buf[pos++] = (v shr 8).toByte()
        buf[pos++] = (v shr 16).toByte()
        buf[pos++] = (v shr 24).toByte()
    }

    fun writeLong(v: Long) {
        ensureCapacity(8)
        buf[pos++] = v.toByte()
        buf[pos++] = (v shr 8).toByte()
        buf[pos++] = (v shr 16).toByte()
        buf[pos++] = (v shr 24).toByte()
        buf[pos++] = (v shr 32).toByte()
        buf[pos++] = (v shr 40).toByte()
        buf[pos++] = (v shr 48).toByte()
        buf[pos++] = (v shr 56).toByte()
    }

    fun writeFloat(v: Float) = writeInt(v.toBits())
    fun writeDouble(v: Double) = writeLong(v.toBits())

    fun write(b: ByteArray) {
        ensureCapacity(b.size)
        b.copyInto(buf, pos)
        pos += b.size
    }

    fun toByteArray(): ByteArray = buf.copyOf(pos)

    fun size(): Int = pos
}
