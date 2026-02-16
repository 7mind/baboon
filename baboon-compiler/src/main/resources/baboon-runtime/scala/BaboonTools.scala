package baboon.runtime.shared {

  import java.math.{BigDecimal, BigInteger}
  import java.nio.charset.StandardCharsets
  import java.time.format.DateTimeFormatter
  import java.time.{Instant, OffsetDateTime, ZoneOffset}
  import java.util.UUID
  import scala.util.Try

  object BaboonTimeFormats {

    // Use 3 fractional digits (milliseconds) to match C# DateTime precision
    // Use XXX (uppercase) to handle both '+00:00' and 'Z' for UTC
    val tsuFormat: DateTimeFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss.SSSXXX")
    val tsoFormat: DateTimeFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss.SSSXXX")

    def parseTso(s: String): Try[OffsetDateTime] = Try(OffsetDateTime.parse(s))
    def parseTsu(s: String): Try[OffsetDateTime] = Try(OffsetDateTime.parse(s))

    def formatTsu(s: OffsetDateTime): String = s.format(tsuFormat)
    def formatTso(s: OffsetDateTime): String = s.format(tsoFormat)

    def decodeTsuFromBin(s: LEDataInputStream): OffsetDateTime = decodeFromBin(s)
    def decodeTsoFromBin(s: LEDataInputStream): OffsetDateTime = decodeFromBin(s)

    def decodeFromBin(s: LEDataInputStream): OffsetDateTime = {
      val millis       = s.readLong()
      val offsetMillis = s.readLong()
      val kind         = s.readByte()
      assert(kind >= 0 && kind <= 2)

      val instant = Instant.ofEpochMilli(millis)
      val offset  = ZoneOffset.ofTotalSeconds((offsetMillis / 1000).toInt)

      OffsetDateTime.ofInstant(instant, offset)
    }

    def encodeTsuToBin(dt: OffsetDateTime, writer: LEDataOutputStream): Unit = {
      encodeToBin(dt, writer, 1)
    }

    def encodeTsoToBin(dt: OffsetDateTime, writer: LEDataOutputStream): Unit = {
      encodeToBin(dt, writer, 0)
    }

    def encodeToBin(dt: OffsetDateTime, writer: LEDataOutputStream, kind: Byte): Unit = {
      val millis       = dt.toInstant.toEpochMilli
      val offsetMillis = dt.getOffset.getTotalSeconds * 1000L
      writer.writeLong(millis)
      writer.writeLong(offsetMillis)
      writer.writeByte(kind.toInt)
    }

  }

  object BaboonBinTools {
    def toUnsignedBigInt(l: Long): BigInt = {
      if (l >= 0) {
        BigInt(l)
      } else {
        val upper = (l >>> 32).toInt
        val lower = l.toInt
        BigInteger
          .valueOf(java.lang.Integer.toUnsignedLong(upper))
          .shiftLeft(32)
          .add(BigInteger.valueOf(java.lang.Integer.toUnsignedLong(lower)))
      }
    }

    def readUid(s: LEDataInputStream): UUID = {
      // Read 16 bytes in .NET GUID format (mixed-endian)
      val bytes = new Array[Byte](16)
      s.readFully(bytes)

      // Convert from .NET GUID byte order to Java UUID byte order
      // .NET uses little-endian for first 8 bytes (Data1, Data2, Data3),
      // then big-endian for last 8 bytes (Data4)
      // We need to reverse the byte order of the first 3 fields

      // Reverse bytes 0-3 (Data1 - 4 bytes)
      val b0 = bytes(0); bytes(0) = bytes(3); bytes(3) = b0
      val b1 = bytes(1); bytes(1) = bytes(2); bytes(2) = b1

      // Reverse bytes 4-5 (Data2 - 2 bytes)
      val b4 = bytes(4); bytes(4) = bytes(5); bytes(5) = b4

      // Reverse bytes 6-7 (Data3 - 2 bytes)
      val b6 = bytes(6); bytes(6) = bytes(7); bytes(7) = b6

      // Bytes 8-15 (Data4) are already in the correct order (big-endian)

      // Now construct UUID from the corrected big-endian bytes
      val msb = ((bytes(0) & 0xFFL) << 56) |
        ((bytes(1) & 0xFFL) << 48) |
        ((bytes(2) & 0xFFL) << 40) |
        ((bytes(3) & 0xFFL) << 32) |
        ((bytes(4) & 0xFFL) << 24) |
        ((bytes(5) & 0xFFL) << 16) |
        ((bytes(6) & 0xFFL) << 8) |
        (bytes(7) & 0xFFL)

      val lsb = ((bytes(8) & 0xFFL) << 56) |
        ((bytes(9) & 0xFFL) << 48) |
        ((bytes(10) & 0xFFL) << 40) |
        ((bytes(11) & 0xFFL) << 32) |
        ((bytes(12) & 0xFFL) << 24) |
        ((bytes(13) & 0xFFL) << 16) |
        ((bytes(14) & 0xFFL) << 8) |
        (bytes(15) & 0xFFL)

      new UUID(msb, lsb)
    }
    def writeUid(fakeWriter: LEDataOutputStream, v: UUID): Unit = {
      // Convert Java UUID (big-endian) to .NET GUID format (mixed-endian)
      val msb = v.getMostSignificantBits
      val lsb = v.getLeastSignificantBits

      // Extract bytes in big-endian order
      val bytes = new Array[Byte](16)
      bytes(0)  = ((msb >> 56) & 0xFF).toByte
      bytes(1)  = ((msb >> 48) & 0xFF).toByte
      bytes(2)  = ((msb >> 40) & 0xFF).toByte
      bytes(3)  = ((msb >> 32) & 0xFF).toByte
      bytes(4)  = ((msb >> 24) & 0xFF).toByte
      bytes(5)  = ((msb >> 16) & 0xFF).toByte
      bytes(6)  = ((msb >> 8) & 0xFF).toByte
      bytes(7)  = (msb & 0xFF).toByte
      bytes(8)  = ((lsb >> 56) & 0xFF).toByte
      bytes(9)  = ((lsb >> 48) & 0xFF).toByte
      bytes(10) = ((lsb >> 40) & 0xFF).toByte
      bytes(11) = ((lsb >> 32) & 0xFF).toByte
      bytes(12) = ((lsb >> 24) & 0xFF).toByte
      bytes(13) = ((lsb >> 16) & 0xFF).toByte
      bytes(14) = ((lsb >> 8) & 0xFF).toByte
      bytes(15) = (lsb & 0xFF).toByte

      // Convert to .NET GUID byte order by reversing the first 3 fields
      // Reverse bytes 0-3 (Data1 - 4 bytes)
      val b0 = bytes(0); bytes(0) = bytes(3); bytes(3) = b0
      val b1 = bytes(1); bytes(1) = bytes(2); bytes(2) = b1

      // Reverse bytes 4-5 (Data2 - 2 bytes)
      val b4 = bytes(4); bytes(4) = bytes(5); bytes(5) = b4

      // Reverse bytes 6-7 (Data3 - 2 bytes)
      val b6 = bytes(6); bytes(6) = bytes(7); bytes(7) = b6

      // Bytes 8-15 (Data4) stay in big-endian order

      // Write all 16 bytes in .NET GUID format
      fakeWriter.write(bytes)
    }

    def readByteString(input: LEDataInputStream): ByteString = {
      val length = input.readInt()
      val bytes  = new Array[Byte](length)
      input.readFully(bytes)
      ByteString(bytes)
    }

    def writeByteString(output: LEDataOutputStream, bs: ByteString): Unit = {
      output.writeInt(bs.length)
      output.write(bs.underlyingUnsafe)
    }

    def readString(input: LEDataInputStream): String = {
      var length   = 0
      var shift    = 0
      var byteRead = 0

      do {
        byteRead = input.readByte() & 0xFF // Read unsigned byte
        length |= (byteRead & 0x7F) << shift
        shift += 7
      } while ((byteRead & 0x80) != 0)

      val buffer = new Array[Byte](length)
      input.readFully(buffer)
      new String(buffer, StandardCharsets.UTF_8)
    }

    def writeString(output: LEDataOutputStream, s: String): Unit = {
      val bytes = s.getBytes(StandardCharsets.UTF_8)
      var value = bytes.length

      do {
        var currentByte = (value & 0x7F).toByte
        value >>>= 7
        if (value != 0) currentByte = (currentByte | 0x80).toByte
        output.writeByte(currentByte.toInt)
      } while (value != 0)

      output.write(bytes)
    }

    def readBigDecimal(input: LEDataInputStream): BigDecimal = {
      val lo    = input.readInt()
      val mid   = input.readInt()
      val hi    = input.readInt()
      val flags = input.readInt()

      val scale      = (flags >> 16) & 0xFF
      val isNegative = (flags & 0x80000000) != 0

      // Reconstruct 96-bit mantissa - properly mask all values to unsigned
      val loLong  = lo.toLong & 0xFFFFFFFFL
      val midLong = mid.toLong & 0xFFFFFFFFL
      val hiLong  = hi.toLong & 0xFFFFFFFFL

      val mantissa       = (BigInt(hiLong) << 64) | (BigInt(midLong) << 32) | BigInt(loLong)
      val signedMantissa = if (isNegative) -mantissa else mantissa

      new java.math.BigDecimal(signedMantissa.bigInteger, scale)
    }

    def writeBigDecimal(output: LEDataOutputStream, value: scala.math.BigDecimal): Unit = {
      // .NET decimal is 16 bytes: lo (int32), mid (int32), hi (int32), flags (int32)
      // flags contains: sign bit (bit 31) and scale (bits 16-23)
      val unscaled = value.bigDecimal.unscaledValue()
      val scale    = value.scale
      require(scale >= 0 && scale <= 28, "C# Decimal supports scale 0–28")
      require(unscaled.bitLength() <= 96, "C# Decimal mantissa must fit in 96 bits")

      // Get the 96-bit unscaled value as 3 x 32-bit integers using bit shifting
      val absUnscaled = unscaled.abs()
      val lo          = absUnscaled.intValue()
      val mid         = absUnscaled.shiftRight(32).intValue()
      val hi          = absUnscaled.shiftRight(64).intValue()

      // Build flags: sign in bit 31, scale in bits 16-23
      val sign  = if (unscaled.signum() < 0) 0x80000000 else 0
      val flags = sign | (scale << 16)

      output.writeInt(lo)
      output.writeInt(mid)
      output.writeInt(hi)
      output.writeInt(flags)
    }

    def readTimestamp(wire: LEDataInputStream): OffsetDateTime = {
      // Read timestamp: 8 bytes (local ticks in ms) + 8 bytes (offset in ms) + 1 byte (kind) = 17 bytes
      // C# writes DateTimeOffset.Ticks which is LOCAL time, not UTC!
      val b0                 = wire.readByte() & 0xFFL
      val b1                 = wire.readByte() & 0xFFL
      val b2                 = wire.readByte() & 0xFFL
      val b3                 = wire.readByte() & 0xFFL
      val b4                 = wire.readByte() & 0xFFL
      val b5                 = wire.readByte() & 0xFFL
      val b6                 = wire.readByte() & 0xFFL
      val b7                 = wire.readByte() & 0xFFL
      val dotNetLocalTicksMs = b0 | (b1 << 8) | (b2 << 16) | (b3 << 24) | (b4 << 32) | (b5 << 40) | (b6 << 48) | (b7 << 56)

      val b8       = wire.readByte() & 0xFFL
      val b9       = wire.readByte() & 0xFFL
      val b10      = wire.readByte() & 0xFFL
      val b11      = wire.readByte() & 0xFFL
      val b12      = wire.readByte() & 0xFFL
      val b13      = wire.readByte() & 0xFFL
      val b14      = wire.readByte() & 0xFFL
      val b15      = wire.readByte() & 0xFFL
      val offsetMs = b8 | (b9 << 8) | (b10 << 16) | (b11 << 24) | (b12 << 32) | (b13 << 40) | (b14 << 48) | (b15 << 56)

      @annotation.unused
      val kind = wire.readByte()

      // Convert local time to UTC by subtracting offset
      val dotNetUtcTicksMs = dotNetLocalTicksMs - offsetMs
      // Convert from .NET epoch to Unix epoch
      val epochMs       = dotNetUtcTicksMs - 62135596800000L
      val offsetSeconds = (offsetMs / 1000).toInt
      val offset        = ZoneOffset.ofTotalSeconds(offsetSeconds)
      val instant       = Instant.ofEpochMilli(epochMs)
      OffsetDateTime.ofInstant(instant, offset)
    }

    def writeTimestamp(wref: LEDataOutputStream, ref: OffsetDateTime): Unit = {
      // Write timestamp: 8 bytes (local ticks in ms) + 8 bytes (offset in ms) + 1 byte (kind) = 17 bytes
      // C# expects DateTimeOffset.Ticks which is LOCAL time, not UTC!
      val ts_offsetDt         = ref
      val ts_epochMs          = ts_offsetDt.toInstant.toEpochMilli
      val ts_dotNetUtcTicksMs = ts_epochMs + 62135596800000L
      val ts_offsetMs         = ts_offsetDt.getOffset.getTotalSeconds * 1000L
      // Convert UTC to local time by adding offset (C# expects local ticks)
      val ts_dotNetLocalTicksMs = ts_dotNetUtcTicksMs + ts_offsetMs
      val ts_kind: Byte         = if (ts_offsetDt.getOffset.getTotalSeconds == 0) 1.toByte else 0.toByte

      // Write dotNetLocalTicksMs (8 bytes, little-endian)
      wref.writeByte((ts_dotNetLocalTicksMs & 0xFF).toInt)
      wref.writeByte(((ts_dotNetLocalTicksMs >> 8) & 0xFF).toInt)
      wref.writeByte(((ts_dotNetLocalTicksMs >> 16) & 0xFF).toInt)
      wref.writeByte(((ts_dotNetLocalTicksMs >> 24) & 0xFF).toInt)
      wref.writeByte(((ts_dotNetLocalTicksMs >> 32) & 0xFF).toInt)
      wref.writeByte(((ts_dotNetLocalTicksMs >> 40) & 0xFF).toInt)
      wref.writeByte(((ts_dotNetLocalTicksMs >> 48) & 0xFF).toInt)
      wref.writeByte(((ts_dotNetLocalTicksMs >> 56) & 0xFF).toInt)

      // Write offsetMs (8 bytes, little-endian)
      wref.writeByte((ts_offsetMs & 0xFF).toInt)
      wref.writeByte(((ts_offsetMs >> 8) & 0xFF).toInt)
      wref.writeByte(((ts_offsetMs >> 16) & 0xFF).toInt)
      wref.writeByte(((ts_offsetMs >> 24) & 0xFF).toInt)
      wref.writeByte(((ts_offsetMs >> 32) & 0xFF).toInt)
      wref.writeByte(((ts_offsetMs >> 40) & 0xFF).toInt)
      wref.writeByte(((ts_offsetMs >> 48) & 0xFF).toInt)
      wref.writeByte(((ts_offsetMs >> 56) & 0xFF).toInt)

      // Write kind (1 byte)
      wref.writeByte(ts_kind.toInt)
    }
  }

}
