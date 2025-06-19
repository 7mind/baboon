package baboon.runtime.shared {

  import java.io.{DataInput, DataInputStream, DataOutput, DataOutputStream, IOException, InputStream, OutputStream}
  import java.math.{BigDecimal, BigInteger}
  import java.nio.charset.StandardCharsets
  import java.time.{Instant, OffsetDateTime, ZoneOffset}
  import java.time.format.DateTimeFormatter
  import java.util.UUID
  import java.util.concurrent.atomic.AtomicReference
  import java.nio.{ByteBuffer, ByteOrder}
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

    val tsuFormat: DateTimeFormatter = DateTimeFormatter.ISO_OFFSET_DATE_TIME // DateTimeFormatter.ofPattern("yyyy-MM-ddTHH:mm:ss.fffZ")
    val tsoFormat: DateTimeFormatter = DateTimeFormatter.ISO_OFFSET_DATE_TIME // DateTimeFormatter.ofPattern("yyyy-MM-ddTHH:mm:ss.fffzzz")

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

    def readUid(s: LEDataInputStream): UUID = new UUID(s.readLong(), s.readLong())
    def writeUid(fakeWriter: LEDataOutputStream, v: UUID): Unit = {
      fakeWriter.writeLong(v.getMostSignificantBits)
      fakeWriter.writeLong(v.getLeastSignificantBits)
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
