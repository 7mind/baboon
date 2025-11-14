package baboon.runtime.shared {

  import java.io.*
  import java.math.{BigDecimal, BigInteger}
  import java.nio.charset.StandardCharsets
  import java.nio.{ByteBuffer, ByteOrder}
  import java.time.format.DateTimeFormatter
  import java.time.{Instant, OffsetDateTime, ZoneOffset}
  import java.util.UUID
  import java.util.concurrent.atomic.AtomicReference
  import scala.reflect.ClassTag

  trait BaboonGenerated {}
  trait BaboonAdtMemberMeta {}
  trait BaboonGeneratedLatest {}
  trait BaboonTypeCodecs {}

  trait GenericConversion {}

  trait BaboonAbstractConversion[F, T] extends GenericConversion {
    def doConvert[C](
      context: C,
      conversions: BaboonAbstractConversions,
      from: F,
    ): T

    def versionFrom: String
    def versionTo: String
    def typeId: String
  }

  case class ConversionKey(from: Class[?], to: Class[?])

  trait BaboonAbstractConversions {
    private val registry = scala.collection.mutable.Map.empty[ConversionKey, GenericConversion]

    def register[F: ClassTag, T: ClassTag](conversion: BaboonAbstractConversion[F, T]): Unit = {
      val key = ConversionKey(implicitly[ClassTag[F]].runtimeClass, implicitly[ClassTag[T]].runtimeClass)
      registry.put(key, conversion)
    }

    def convertWithContext[C, F: ClassTag, T: ClassTag](
      context: C,
      from: F,
    ): T = {
      val key = ConversionKey(implicitly[ClassTag[F]].runtimeClass, implicitly[ClassTag[T]].runtimeClass)
      registry(key).asInstanceOf[BaboonAbstractConversion[F, T]].doConvert[C](context, this, from).asInstanceOf[T]
    }

    def versionsFrom: List[String]
    def versionTo: String
  }

  trait BaboonAbstractCodecs {}

  trait BaboonEnum[T] {
    def parse(s: String): Option[T]
    def all: List[T]
  }

  trait BaboonMeta {}

  trait BaboonFixture[T] {
    def random(gen: BaboonRandom): T
  }

  trait BaboonAdtFixture[T] {
    def randomAll(gen: BaboonRandom): List[T]
  }

  trait BaboonRandom {
    def nextBit(): Boolean

    def nextI08(): Byte
    def nextI16(): Short
    def nextI32(): Int
    def nextI64(): Long

    def nextU08(): Byte
    def nextU16(): Short
    def nextU32(): Int
    def nextU64(): Long

    def nextF32(): Float
    def nextF64(): Double
    def nextF128(): BigDecimal

    def nextTsu(): OffsetDateTime
    def nextTso(): OffsetDateTime

    def nextUid(): java.util.UUID
    def nextString(): String

    def mkOption[T](element: => T): Option[T]
    def mkEnum[T](meta: BaboonEnum[T]): T
    def mkList[T](element: => T): List[T]
    def mkSet[T](element: => T): Set[T]
    def mkMap[K, V](k: => K, v: => V): Map[K, V]

    def oneOf[T](elements: List[BaboonRandom => T]): T
    def randomElement[T](elements: List[T]): T
  }

  object BaboonRandom {
    def default(): BaboonRandom = {
      new BaboonRandomImpl(scala.util.Random)
    }
  }

  class BaboonRandomImpl(rnd: scala.util.Random) extends BaboonRandom {
    def nextBit(): Boolean = rnd.nextBoolean()

    def nextI08(): Byte  = rnd.nextInt().toByte
    def nextI16(): Short = rnd.nextInt().toShort
    def nextI32(): Int   = rnd.nextInt().toInt
    def nextI64(): Long  = rnd.nextInt().toLong

    def nextU08(): Byte  = rnd.nextInt().toByte
    def nextU16(): Short = rnd.nextInt().toShort
    def nextU32(): Int   = rnd.nextInt().toInt
    def nextU64(): Long  = rnd.nextInt().toLong

    def nextF32(): Float       = rnd.nextFloat()
    def nextF64(): Double      = rnd.nextDouble()
    def nextF128(): BigDecimal = scala.math.BigDecimal(rnd.nextDouble()).bigDecimal

    val earliest: OffsetDateTime = OffsetDateTime.parse("2020-01-01T00:00:00Z")
    val latest: OffsetDateTime   = OffsetDateTime.parse("2099-12-31T23:59:59Z")

    def generateRandomOffsetDateTime(startInclusive: OffsetDateTime, endInclusive: OffsetDateTime): OffsetDateTime = {
      val minSeconds    = startInclusive.toEpochSecond
      val maxSeconds    = endInclusive.toEpochSecond
      val randomSeconds = minSeconds + rnd.nextLong(maxSeconds - minSeconds)
      OffsetDateTime.ofInstant(Instant.ofEpochSecond(randomSeconds), ZoneOffset.UTC)
    }
    def nextTsu(): OffsetDateTime = {
      generateRandomOffsetDateTime(earliest, latest).withOffsetSameInstant(ZoneOffset.UTC)
    }
    def nextTso(): OffsetDateTime = {
      generateRandomOffsetDateTime(earliest, latest).withOffsetSameInstant(ZoneOffset.ofHours(rnd.nextInt(36) - 18))
    }

    def nextUid(): java.util.UUID = java.util.UUID.randomUUID()
    def nextString(): String      = rnd.alphanumeric.take(10).mkString

    def mkList[T](element: => T): List[T]        = rnd.shuffle(List.fill(rnd.nextInt(20))(element))
    def mkSet[T](element: => T): Set[T]          = rnd.shuffle(Set.fill(rnd.nextInt(20))(element))
    def mkMap[K, V](k: => K, v: => V): Map[K, V] = rnd.shuffle(List.fill(rnd.nextInt(20))((k, v)).toMap)
    def mkOption[T](element: => T): Option[T]    = Some(element)

    def mkEnum[T](meta: BaboonEnum[T]): T = rnd.shuffle(meta.all).head

    def oneOf[T](elements: List[BaboonRandom => T]): T = {
      elements(rnd.nextInt(elements.size))(this)
    }

    def randomElement[T](elements: List[T]): T = {
      elements(rnd.nextInt(elements.size))
    }
  }

  trait BaboonCodecContext {
    def useIndices: Boolean
  }
  object BaboonCodecContext {
    val Default: BaboonCodecContext = Compact

    object Indexed extends BaboonCodecContext {
      override def useIndices: Boolean = true
    }

    object Compact extends BaboonCodecContext {
      override def useIndices: Boolean = false
    }
  }

  trait BaboonJsonCodec[T] {
    def encode(ctx: BaboonCodecContext, value: T): io.circe.Json
    def decode(ctx: BaboonCodecContext, wire: io.circe.Json): Either[String, T]
  }

  final class Lazy[T](private val initializer: () => T) {
    private val valueRef = new AtomicReference[Option[T]](None)

    def value: T = valueRef.get() match {
      case Some(existing) => existing
      case None =>
        val computed = initializer()
        if (valueRef.compareAndSet(None, Some(computed))) computed
        else valueRef.get().get
    }

    def isValueCreated: Boolean = valueRef.get().isDefined
  }

  object Lazy {
    def apply[T](initializer: => T): Lazy[T] =
      new Lazy(() => initializer)
  }

  object BaboonTimeFormats {

    // Use 3 fractional digits (milliseconds) to match C# DateTime precision
    // Use XXX (uppercase) to handle both '+00:00' and 'Z' for UTC
    val tsuFormat: DateTimeFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss.SSSXXX")
    val tsoFormat: DateTimeFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss.SSSXXX")

    def parseTso(s: String): Option[OffsetDateTime] = Some(OffsetDateTime.parse(s, tsoFormat))
    def parseTsu(s: String): Option[OffsetDateTime] = Some(OffsetDateTime.parse(s, tsuFormat))

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
      writer.writeByte(kind)
    }

  }

  trait BaboonBinCodec[T] {
    def encode(ctx: BaboonCodecContext, writer: LEDataOutputStream, value: T): Unit
    def decode(ctx: BaboonCodecContext, wire: LEDataInputStream): T
  }

  case class BaboonIndexEntry(offset: Long, length: Long)

  trait BaboonBinCodecIndexed {
    def indexElementsCount(ctx: BaboonCodecContext): Short

    def readIndex(ctx: BaboonCodecContext, wire: LEDataInputStream): List[BaboonIndexEntry] = {
      val header           = wire.readByte()
      val isIndexed        = (header & 0x01) != 0
      val result           = scala.collection.mutable.ListBuffer.empty[BaboonIndexEntry]
      var prevOffset: Long = 0L
      var prevLen: Long    = 0L

      if (isIndexed) {
        var left = indexElementsCount(ctx).toInt
        while (left > 0) {
          val offset = wire.readInt()
          val len    = wire.readInt()

          require(len > 0, "Length must be positive")
          require(offset >= prevOffset + prevLen, s"Offset violation: $offset not >= ${prevOffset + prevLen}")

          result += BaboonIndexEntry(offset, len)
          left       = left - 1
          prevOffset = offset
          prevLen    = len
        }
      }

      result.toList
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
      bytes(0) = ((msb >> 56) & 0xFF).toByte
      bytes(1) = ((msb >> 48) & 0xFF).toByte
      bytes(2) = ((msb >> 40) & 0xFF).toByte
      bytes(3) = ((msb >> 32) & 0xFF).toByte
      bytes(4) = ((msb >> 24) & 0xFF).toByte
      bytes(5) = ((msb >> 16) & 0xFF).toByte
      bytes(6) = ((msb >> 8) & 0xFF).toByte
      bytes(7) = (msb & 0xFF).toByte
      bytes(8) = ((lsb >> 56) & 0xFF).toByte
      bytes(9) = ((lsb >> 48) & 0xFF).toByte
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
        output.writeByte(currentByte)
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

      // Reconstruct 96-bit mantissa
      val mantissa       = (BigInt(hi) << 64) | (BigInt(mid) << 32) | (BigInt(lo) & 0xFFFFFFFFL)
      val signedMantissa = if (isNegative) -mantissa else mantissa

      new java.math.BigDecimal(signedMantissa.bigInteger, scale)
    }

    def writeBigDecimal(output: LEDataOutputStream, value: scala.math.BigDecimal): Unit = {
      val unscaled = value.bigDecimal.unscaledValue()
      val scale    = value.scale
      require(scale >= 0 && scale <= 28, "C# Decimal supports scale 0–28")
      require(unscaled.bitLength <= 96, "C# Decimal mantissa must fit in 96 bits")

      val sign  = if (value.signum < 0) 0x80000000 else 0x00000000
      val flags = (scale << 16) | sign

      val mantissa       = unscaled.abs().toByteArray
      val mantissaPadded = Array.fill[Byte](12 - mantissa.length)(0) ++ mantissa.takeRight(12)

      // Split into 32-bit chunks (big-endian byte order)
      val lo  = mantissaPadded.slice(8, 12).foldLeft(0)((acc, b) => (acc << 8) | (b & 0xFF))
      val mid = mantissaPadded.slice(4, 8).foldLeft(0)((acc, b) => (acc << 8) | (b & 0xFF))
      val hi  = mantissaPadded.slice(0, 4).foldLeft(0)((acc, b) => (acc << 8) | (b & 0xFF))

      output.writeInt(lo)
      output.writeInt(mid)
      output.writeInt(hi)
      output.writeInt(flags)
    }

    def readTimestamp(wire: LEDataInputStream): OffsetDateTime = {
      // Read timestamp: 8 bytes (local ticks in ms) + 8 bytes (offset in ms) + 1 byte (kind) = 17 bytes
      // C# writes DateTimeOffset.Ticks which is LOCAL time, not UTC!
      val b0 = wire.readByte() & 0xFFL
      val b1 = wire.readByte() & 0xFFL
      val b2 = wire.readByte() & 0xFFL
      val b3 = wire.readByte() & 0xFFL
      val b4 = wire.readByte() & 0xFFL
      val b5 = wire.readByte() & 0xFFL
      val b6 = wire.readByte() & 0xFFL
      val b7 = wire.readByte() & 0xFFL
      val dotNetLocalTicksMs = b0 | (b1 << 8) | (b2 << 16) | (b3 << 24) | (b4 << 32) | (b5 << 40) | (b6 << 48) | (b7 << 56)

      val b8 = wire.readByte() & 0xFFL
      val b9 = wire.readByte() & 0xFFL
      val b10 = wire.readByte() & 0xFFL
      val b11 = wire.readByte() & 0xFFL
      val b12 = wire.readByte() & 0xFFL
      val b13 = wire.readByte() & 0xFFL
      val b14 = wire.readByte() & 0xFFL
      val b15 = wire.readByte() & 0xFFL
      val offsetMs = b8 | (b9 << 8) | (b10 << 16) | (b11 << 24) | (b12 << 32) | (b13 << 40) | (b14 << 48) | (b15 << 56)

      val kind = wire.readByte()

      // Convert local time to UTC by subtracting offset
      val dotNetUtcTicksMs = dotNetLocalTicksMs - offsetMs
      // Convert from .NET epoch to Unix epoch
      val epochMs = dotNetUtcTicksMs - 62135596800000L
      val offsetSeconds = (offsetMs / 1000).toInt
      val offset = ZoneOffset.ofTotalSeconds(offsetSeconds)
      val instant = Instant.ofEpochMilli(epochMs)
      OffsetDateTime.ofInstant(instant, offset)
    }

    def writeTimestamp(wref: LEDataOutputStream, ref: OffsetDateTime): Unit = {
      // Write timestamp: 8 bytes (local ticks in ms) + 8 bytes (offset in ms) + 1 byte (kind) = 17 bytes
      // C# expects DateTimeOffset.Ticks which is LOCAL time, not UTC!
      val ts_offsetDt = ref
      val ts_epochMs = ts_offsetDt.toInstant.toEpochMilli
      val ts_dotNetUtcTicksMs = ts_epochMs + 62135596800000L
      val ts_offsetMs = ts_offsetDt.getOffset.getTotalSeconds * 1000L
      // Convert UTC to local time by adding offset (C# expects local ticks)
      val ts_dotNetLocalTicksMs = ts_dotNetUtcTicksMs + ts_offsetMs
      val ts_kind: Byte = if (ts_offsetDt.getOffset.getTotalSeconds == 0) 1.toByte else 0.toByte

      // Write dotNetLocalTicksMs (8 bytes, little-endian)
      wref.writeByte((ts_dotNetLocalTicksMs & 0xFF).toByte)
      wref.writeByte(((ts_dotNetLocalTicksMs >> 8) & 0xFF).toByte)
      wref.writeByte(((ts_dotNetLocalTicksMs >> 16) & 0xFF).toByte)
      wref.writeByte(((ts_dotNetLocalTicksMs >> 24) & 0xFF).toByte)
      wref.writeByte(((ts_dotNetLocalTicksMs >> 32) & 0xFF).toByte)
      wref.writeByte(((ts_dotNetLocalTicksMs >> 40) & 0xFF).toByte)
      wref.writeByte(((ts_dotNetLocalTicksMs >> 48) & 0xFF).toByte)
      wref.writeByte(((ts_dotNetLocalTicksMs >> 56) & 0xFF).toByte)

      // Write offsetMs (8 bytes, little-endian)
      wref.writeByte((ts_offsetMs & 0xFF).toByte)
      wref.writeByte(((ts_offsetMs >> 8) & 0xFF).toByte)
      wref.writeByte(((ts_offsetMs >> 16) & 0xFF).toByte)
      wref.writeByte(((ts_offsetMs >> 24) & 0xFF).toByte)
      wref.writeByte(((ts_offsetMs >> 32) & 0xFF).toByte)
      wref.writeByte(((ts_offsetMs >> 40) & 0xFF).toByte)
      wref.writeByte(((ts_offsetMs >> 48) & 0xFF).toByte)
      wref.writeByte(((ts_offsetMs >> 56) & 0xFF).toByte)

      // Write kind (1 byte)
      wref.writeByte(ts_kind)
    }
  }

  class LEDataInputStream(stream: InputStream) extends InputStream with DataInput {
    private val dataIn           = new DataInputStream(stream)
    private val buffer           = ByteBuffer.allocate(8)
    private var order: ByteOrder = ByteOrder.LITTLE_ENDIAN

    @throws[IOException]
    override def read(b: Array[Byte]): Int = dataIn.read(b)

    @throws[IOException]
    override def read(b: Array[Byte], off: Int, len: Int): Int = dataIn.read(b, off, len)

    @throws[IOException]
    override def readLine(): String = dataIn.readLine()

    @throws[IOException]
    override def readBoolean(): Boolean = dataIn.readBoolean()

    @throws[IOException]
    override def readByte(): Byte = dataIn.readByte()

    @throws[IOException]
    override def read(): Int = readByte()

    override def markSupported(): Boolean = dataIn.markSupported()

    override def mark(readlimit: Int): Unit = dataIn.mark(readlimit)

    @throws[IOException]
    override def reset(): Unit = dataIn.reset()

    @throws[IOException]
    override def readChar(): Char = dataIn.readChar()

    @throws[IOException]
    override def readFully(b: Array[Byte]): Unit = dataIn.readFully(b)

    @throws[IOException]
    override def readFully(b: Array[Byte], off: Int, len: Int): Unit = dataIn.readFully(b, off, len)

    @throws[IOException]
    override def readUTF(): String = dataIn.readUTF()

    @throws[IOException]
    override def skipBytes(n: Int): Int = dataIn.skipBytes(n)

    @throws[IOException]
    override def readDouble(): Double = {
      val tmp = readLong()
      java.lang.Double.longBitsToDouble(tmp)
    }

    @throws[IOException]
    override def readFloat(): Float = {
      val tmp = readInt()
      java.lang.Float.intBitsToFloat(tmp)
    }

    @throws[IOException]
    override def readInt(): Int = {
      buffer.clear()
      buffer
        .order(ByteOrder.BIG_ENDIAN)
        .putInt(dataIn.readInt())
        .flip()
      buffer.order(order).getInt()
    }

    @throws[IOException]
    override def readLong(): Long = {
      buffer.clear()
      buffer
        .order(ByteOrder.BIG_ENDIAN)
        .putLong(dataIn.readLong())
        .flip()
      buffer.order(order).getLong()
    }

    @throws[IOException]
    override def readShort(): Short = {
      buffer.clear()
      buffer
        .order(ByteOrder.BIG_ENDIAN)
        .putShort(dataIn.readShort())
        .flip()
      buffer.order(order).getShort()
    }

    @throws[IOException]
    override def readUnsignedByte(): Int = dataIn.readByte() & 0xFF

    @throws[IOException]
    override def readUnsignedShort(): Int = readShort() & 0xFFFF
  }

  class LEDataOutputStream(stream: OutputStream) extends OutputStream with DataOutput {
    private val dataOut          = new DataOutputStream(stream)
    private val buffer           = ByteBuffer.allocate(8)
    private val order: ByteOrder = ByteOrder.LITTLE_ENDIAN

    // OutputStream implementation
    @throws[IOException]
    override def write(b: Int): Unit = dataOut.write(b)

    @throws[IOException]
    override def write(b: Array[Byte]): Unit = dataOut.write(b)

    @throws[IOException]
    override def write(b: Array[Byte], off: Int, len: Int): Unit =
      dataOut.write(b, off, len)

    @throws[IOException]
    override def flush(): Unit = dataOut.flush()

    @throws[IOException]
    override def close(): Unit = dataOut.close()

    // DataOutput implementation
    @throws[IOException]
    override def writeBoolean(v: Boolean): Unit = dataOut.writeBoolean(v)

    @throws[IOException]
    override def writeByte(v: Int): Unit = dataOut.writeByte(v)

    @throws[IOException]
    override def writeBytes(s: String): Unit = dataOut.writeBytes(s)

    @throws[IOException]
    override def writeChar(v: Int): Unit = {
      buffer.clear()
      buffer.order(order).putChar(v.toChar)
      buffer.flip()
      dataOut.write(buffer.array(), 0, 2)
    }

    @throws[IOException]
    override def writeChars(s: String): Unit =
      s.foreach(c => writeChar(c))

    @throws[IOException]
    override def writeDouble(v: Double): Unit =
      writeLong(java.lang.Double.doubleToLongBits(v))

    @throws[IOException]
    override def writeFloat(v: Float): Unit =
      writeInt(java.lang.Float.floatToIntBits(v))

    @throws[IOException]
    override def writeInt(v: Int): Unit = {
      buffer.clear()
      buffer.order(order).putInt(v)
      buffer.flip()
      dataOut.write(buffer.array(), 0, 4)
    }

    @throws[IOException]
    override def writeLong(v: Long): Unit = {
      buffer.clear()
      buffer.order(order).putLong(v)
      buffer.flip()
      dataOut.write(buffer.array(), 0, 8)
    }

    @throws[IOException]
    override def writeShort(v: Int): Unit = {
      buffer.clear()
      buffer.order(order).putShort(v.toShort)
      buffer.flip()
      dataOut.write(buffer.array(), 0, 2)
    }

    @throws[IOException]
    override def writeUTF(s: String): Unit = dataOut.writeUTF(s)
  }

}
