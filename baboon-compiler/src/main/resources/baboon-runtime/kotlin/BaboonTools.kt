package baboon.runtime.shared

import java.io.*
import java.math.BigDecimal
import java.math.BigInteger
import java.nio.ByteBuffer
import java.nio.ByteOrder
import java.nio.charset.StandardCharsets
import java.time.Instant
import java.time.OffsetDateTime
import java.time.ZoneOffset
import java.util.UUID

object BaboonBinTools {
    fun readUid(s: LEDataInputStream): UUID {
        val bytes = ByteArray(16)
        s.readFully(bytes)

        // Convert from .NET GUID byte order to Java UUID byte order
        var b0 = bytes[0]; bytes[0] = bytes[3]; bytes[3] = b0
        var b1 = bytes[1]; bytes[1] = bytes[2]; bytes[2] = b1
        var b4 = bytes[4]; bytes[4] = bytes[5]; bytes[5] = b4
        var b6 = bytes[6]; bytes[6] = bytes[7]; bytes[7] = b6

        val msb = ((bytes[0].toLong() and 0xFFL) shl 56) or
            ((bytes[1].toLong() and 0xFFL) shl 48) or
            ((bytes[2].toLong() and 0xFFL) shl 40) or
            ((bytes[3].toLong() and 0xFFL) shl 32) or
            ((bytes[4].toLong() and 0xFFL) shl 24) or
            ((bytes[5].toLong() and 0xFFL) shl 16) or
            ((bytes[6].toLong() and 0xFFL) shl 8) or
            (bytes[7].toLong() and 0xFFL)

        val lsb = ((bytes[8].toLong() and 0xFFL) shl 56) or
            ((bytes[9].toLong() and 0xFFL) shl 48) or
            ((bytes[10].toLong() and 0xFFL) shl 40) or
            ((bytes[11].toLong() and 0xFFL) shl 32) or
            ((bytes[12].toLong() and 0xFFL) shl 24) or
            ((bytes[13].toLong() and 0xFFL) shl 16) or
            ((bytes[14].toLong() and 0xFFL) shl 8) or
            (bytes[15].toLong() and 0xFFL)

        return UUID(msb, lsb)
    }

    fun writeUid(writer: LEDataOutputStream, v: UUID) {
        val msb = v.mostSignificantBits
        val lsb = v.leastSignificantBits

        val bytes = ByteArray(16)
        bytes[0] = ((msb shr 56) and 0xFF).toByte()
        bytes[1] = ((msb shr 48) and 0xFF).toByte()
        bytes[2] = ((msb shr 40) and 0xFF).toByte()
        bytes[3] = ((msb shr 32) and 0xFF).toByte()
        bytes[4] = ((msb shr 24) and 0xFF).toByte()
        bytes[5] = ((msb shr 16) and 0xFF).toByte()
        bytes[6] = ((msb shr 8) and 0xFF).toByte()
        bytes[7] = (msb and 0xFF).toByte()
        bytes[8] = ((lsb shr 56) and 0xFF).toByte()
        bytes[9] = ((lsb shr 48) and 0xFF).toByte()
        bytes[10] = ((lsb shr 40) and 0xFF).toByte()
        bytes[11] = ((lsb shr 32) and 0xFF).toByte()
        bytes[12] = ((lsb shr 24) and 0xFF).toByte()
        bytes[13] = ((lsb shr 16) and 0xFF).toByte()
        bytes[14] = ((lsb shr 8) and 0xFF).toByte()
        bytes[15] = (lsb and 0xFF).toByte()

        // Convert to .NET GUID byte order
        var b0 = bytes[0]; bytes[0] = bytes[3]; bytes[3] = b0
        var b1 = bytes[1]; bytes[1] = bytes[2]; bytes[2] = b1
        var b4 = bytes[4]; bytes[4] = bytes[5]; bytes[5] = b4
        var b6 = bytes[6]; bytes[6] = bytes[7]; bytes[7] = b6

        writer.write(bytes)
    }

    fun readByteString(input: LEDataInputStream): ByteString {
        val length = input.readInt()
        val bytes = ByteArray(length)
        input.readFully(bytes)
        return ByteString.of(bytes)
    }

    fun writeByteString(output: LEDataOutputStream, bs: ByteString) {
        output.writeInt(bs.length)
        output.write(bs.underlyingUnsafe())
    }

    fun readString(input: LEDataInputStream): String {
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
        return String(buffer, StandardCharsets.UTF_8)
    }

    fun writeString(output: LEDataOutputStream, s: String) {
        val bytes = s.toByteArray(StandardCharsets.UTF_8)
        var value = bytes.size

        do {
            var currentByte = (value and 0x7F).toByte()
            value = value ushr 7
            if (value != 0) currentByte = (currentByte.toInt() or 0x80).toByte()
            output.writeByte(currentByte.toInt())
        } while (value != 0)

        output.write(bytes)
    }

    fun readBigDecimal(input: LEDataInputStream): BigDecimal {
        val lo = input.readInt()
        val mid = input.readInt()
        val hi = input.readInt()
        val flags = input.readInt()

        val scale = (flags shr 16) and 0xFF
        val isNegative = (flags and 0x80000000.toInt()) != 0

        val loLong = lo.toLong() and 0xFFFFFFFFL
        val midLong = mid.toLong() and 0xFFFFFFFFL
        val hiLong = hi.toLong() and 0xFFFFFFFFL

        val mantissa = (BigInteger.valueOf(hiLong) shl 64) or (BigInteger.valueOf(midLong) shl 32) or BigInteger.valueOf(loLong)
        val signedMantissa = if (isNegative) mantissa.negate() else mantissa

        return BigDecimal(signedMantissa, scale)
    }

    fun writeBigDecimal(output: LEDataOutputStream, value: BigDecimal) {
        val unscaled = value.unscaledValue()
        val scale = value.scale()
        require(scale in 0..28) { "C# Decimal supports scale 0-28" }
        require(unscaled.abs().bitLength() <= 96) { "C# Decimal mantissa must fit in 96 bits" }

        val absUnscaled = unscaled.abs()
        val lo = absUnscaled.toInt()
        val mid = absUnscaled.shiftRight(32).toInt()
        val hi = absUnscaled.shiftRight(64).toInt()

        val sign = if (unscaled.signum() < 0) 0x80000000.toInt() else 0
        val flags = sign or (scale shl 16)

        output.writeInt(lo)
        output.writeInt(mid)
        output.writeInt(hi)
        output.writeInt(flags)
    }

    fun readTimestamp(wire: LEDataInputStream): OffsetDateTime {
        val b0 = wire.readByte().toLong() and 0xFFL
        val b1 = wire.readByte().toLong() and 0xFFL
        val b2 = wire.readByte().toLong() and 0xFFL
        val b3 = wire.readByte().toLong() and 0xFFL
        val b4 = wire.readByte().toLong() and 0xFFL
        val b5 = wire.readByte().toLong() and 0xFFL
        val b6 = wire.readByte().toLong() and 0xFFL
        val b7 = wire.readByte().toLong() and 0xFFL
        val dotNetLocalTicksMs = b0 or (b1 shl 8) or (b2 shl 16) or (b3 shl 24) or (b4 shl 32) or (b5 shl 40) or (b6 shl 48) or (b7 shl 56)

        val b8 = wire.readByte().toLong() and 0xFFL
        val b9 = wire.readByte().toLong() and 0xFFL
        val b10 = wire.readByte().toLong() and 0xFFL
        val b11 = wire.readByte().toLong() and 0xFFL
        val b12 = wire.readByte().toLong() and 0xFFL
        val b13 = wire.readByte().toLong() and 0xFFL
        val b14 = wire.readByte().toLong() and 0xFFL
        val b15 = wire.readByte().toLong() and 0xFFL
        val offsetMs = b8 or (b9 shl 8) or (b10 shl 16) or (b11 shl 24) or (b12 shl 32) or (b13 shl 40) or (b14 shl 48) or (b15 shl 56)

        @Suppress("UNUSED_VARIABLE")
        val kind = wire.readByte()

        val dotNetUtcTicksMs = dotNetLocalTicksMs - offsetMs
        val epochMs = dotNetUtcTicksMs - 62135596800000L
        val offsetSeconds = (offsetMs / 1000).toInt()
        val offset = ZoneOffset.ofTotalSeconds(offsetSeconds)
        val instant = Instant.ofEpochMilli(epochMs)
        return OffsetDateTime.ofInstant(instant, offset)
    }

    fun writeTimestamp(writer: LEDataOutputStream, ref: OffsetDateTime) {
        val epochMs = ref.toInstant().toEpochMilli()
        val dotNetUtcTicksMs = epochMs + 62135596800000L
        val offsetMs = ref.offset.totalSeconds * 1000L
        val dotNetLocalTicksMs = dotNetUtcTicksMs + offsetMs
        val kind: Byte = if (ref.offset.totalSeconds == 0) 1 else 0

        writer.writeByte((dotNetLocalTicksMs and 0xFF).toInt())
        writer.writeByte(((dotNetLocalTicksMs shr 8) and 0xFF).toInt())
        writer.writeByte(((dotNetLocalTicksMs shr 16) and 0xFF).toInt())
        writer.writeByte(((dotNetLocalTicksMs shr 24) and 0xFF).toInt())
        writer.writeByte(((dotNetLocalTicksMs shr 32) and 0xFF).toInt())
        writer.writeByte(((dotNetLocalTicksMs shr 40) and 0xFF).toInt())
        writer.writeByte(((dotNetLocalTicksMs shr 48) and 0xFF).toInt())
        writer.writeByte(((dotNetLocalTicksMs shr 56) and 0xFF).toInt())

        writer.writeByte((offsetMs and 0xFF).toInt())
        writer.writeByte(((offsetMs shr 8) and 0xFF).toInt())
        writer.writeByte(((offsetMs shr 16) and 0xFF).toInt())
        writer.writeByte(((offsetMs shr 24) and 0xFF).toInt())
        writer.writeByte(((offsetMs shr 32) and 0xFF).toInt())
        writer.writeByte(((offsetMs shr 40) and 0xFF).toInt())
        writer.writeByte(((offsetMs shr 48) and 0xFF).toInt())
        writer.writeByte(((offsetMs shr 56) and 0xFF).toInt())

        writer.writeByte(kind.toInt())
    }
}

class LEDataInputStream(stream: InputStream) : InputStream(), DataInput {
    private val dataIn = DataInputStream(stream)
    private val buffer = ByteBuffer.allocate(8)
    private val order: ByteOrder = ByteOrder.LITTLE_ENDIAN

    override fun read(b: ByteArray): Int = dataIn.read(b)
    override fun read(b: ByteArray, off: Int, len: Int): Int = dataIn.read(b, off, len)

    override fun readBoolean(): Boolean = dataIn.readBoolean()
    override fun readByte(): Byte = dataIn.readByte()
    override fun read(): Int = readByte().toInt()

    override fun markSupported(): Boolean = dataIn.markSupported()
    override fun mark(readlimit: Int) = dataIn.mark(readlimit)
    override fun reset() = dataIn.reset()

    override fun readChar(): Char = dataIn.readChar()
    override fun readFully(b: ByteArray) = dataIn.readFully(b)
    override fun readFully(b: ByteArray, off: Int, len: Int) = dataIn.readFully(b, off, len)
    override fun readUTF(): String = dataIn.readUTF()
    override fun skipBytes(n: Int): Int = dataIn.skipBytes(n)

    @Suppress("DEPRECATION")
    @Deprecated("readLine() is deprecated", ReplaceWith(""))
    override fun readLine(): String = dataIn.readLine()

    override fun readDouble(): Double {
        val tmp = readLong()
        return java.lang.Double.longBitsToDouble(tmp)
    }

    override fun readFloat(): Float {
        val tmp = readInt()
        return java.lang.Float.intBitsToFloat(tmp)
    }

    override fun readInt(): Int {
        buffer.clear()
        buffer.order(ByteOrder.BIG_ENDIAN).putInt(dataIn.readInt()).flip()
        return buffer.order(order).getInt()
    }

    override fun readLong(): Long {
        buffer.clear()
        buffer.order(ByteOrder.BIG_ENDIAN).putLong(dataIn.readLong()).flip()
        return buffer.order(order).getLong()
    }

    override fun readShort(): Short {
        buffer.clear()
        buffer.order(ByteOrder.BIG_ENDIAN).putShort(dataIn.readShort()).flip()
        return buffer.order(order).getShort()
    }

    override fun readUnsignedByte(): Int = dataIn.readByte().toInt() and 0xFF
    override fun readUnsignedShort(): Int = readShort().toInt() and 0xFFFF
}

class LEDataOutputStream(stream: OutputStream) : OutputStream(), DataOutput {
    private val dataOut = DataOutputStream(stream)
    private val buffer = ByteBuffer.allocate(8)
    private val order: ByteOrder = ByteOrder.LITTLE_ENDIAN

    override fun write(b: Int) = dataOut.write(b)
    override fun write(b: ByteArray) = dataOut.write(b)
    override fun write(b: ByteArray, off: Int, len: Int) = dataOut.write(b, off, len)
    override fun flush() = dataOut.flush()
    override fun close() = dataOut.close()

    override fun writeBoolean(v: Boolean) = dataOut.writeBoolean(v)
    override fun writeByte(v: Int) = dataOut.writeByte(v)
    override fun writeBytes(s: String) = dataOut.writeBytes(s)

    override fun writeChar(v: Int) {
        buffer.clear()
        buffer.order(order).putChar(v.toChar())
        buffer.flip()
        dataOut.write(buffer.array(), 0, 2)
    }

    override fun writeChars(s: String) = s.forEach { writeChar(it.code) }

    override fun writeDouble(v: Double) = writeLong(java.lang.Double.doubleToLongBits(v))
    override fun writeFloat(v: Float) = writeInt(java.lang.Float.floatToIntBits(v))

    override fun writeInt(v: Int) {
        buffer.clear()
        buffer.order(order).putInt(v)
        buffer.flip()
        dataOut.write(buffer.array(), 0, 4)
    }

    override fun writeLong(v: Long) {
        buffer.clear()
        buffer.order(order).putLong(v)
        buffer.flip()
        dataOut.write(buffer.array(), 0, 8)
    }

    override fun writeShort(v: Int) {
        buffer.clear()
        buffer.order(order).putShort(v.toShort())
        buffer.flip()
        dataOut.write(buffer.array(), 0, 2)
    }

    override fun writeUTF(s: String) = dataOut.writeUTF(s)
}
