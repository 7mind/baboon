package baboon.runtime.shared {

  import java.io.*
  import java.math.{BigDecimal, BigInteger}
  import java.nio.charset.{Charset, StandardCharsets}
  import java.nio.{ByteBuffer, ByteOrder}
  import java.time.format.DateTimeFormatter
  import java.time.{Instant, OffsetDateTime, ZoneOffset}
  import java.util.UUID
  import java.util.concurrent.atomic.AtomicReference
  import scala.annotation.tailrec
  import scala.reflect.ClassTag
  import scala.util.Try

  trait BaboonGenerated {}
  trait BaboonAdtMemberMeta {}
  trait BaboonGeneratedLatest {}

  case class BaboonTypeCodecs[T](id: String, jsonCodec: BaboonJsonCodec[T], uebaCodec: BaboonBinCodec[T])

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
      val _   = registry.put(key, conversion)
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

  trait BaboonAbstractCodecs {
    def register[T](baboonTypeCodecs: BaboonTypeCodecs[T]): Unit = {
      // TODO
    }
  }

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
    def nextByteString(): ByteString

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
    def nextByteString(): ByteString = {
      val length = rnd.nextInt(21)
      val bytes  = new Array[Byte](length)
      rnd.nextBytes(bytes)
      ByteString(bytes)
    }

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

  /**
    * Minimal immutable ByteString implementation with comparison and concatenation
    */
  class ByteString private (private val bytes: Array[Byte]) extends Ordered[ByteString] {

    // Properties
    def length: Int       = bytes.length
    def isEmpty: Boolean  = bytes.isEmpty
    def nonEmpty: Boolean = bytes.nonEmpty

    // Indexed access
    def apply(index: Int): Byte = bytes(index)

    // Get a safe copy of the bytes
    def toArray: Array[Byte] = bytes.clone()

    // Direct access to underlying bytes - USE WITH CAUTION!
    // Modifying the returned array will break immutability
    def underlyingUnsafe: Array[Byte] = bytes

    // Concatenation
    def concat(other: ByteString): ByteString = {
      if (other == null) throw new NullPointerException("Cannot concatenate with null")
      val result = new Array[Byte](bytes.length + other.bytes.length)
      System.arraycopy(bytes, 0, result, 0, bytes.length)
      System.arraycopy(other.bytes, 0, result, bytes.length, other.bytes.length)
      new ByteString(result)
    }

    def concat(others: ByteString*): ByteString = {
      val totalLength = bytes.length + others.map(_.length).sum
      val result      = new Array[Byte](totalLength)
      var offset      = 0

      System.arraycopy(bytes, 0, result, offset, bytes.length)
      offset += bytes.length

      others.foreach {
        other =>
          if (other != null) {
            System.arraycopy(other.bytes, 0, result, offset, other.bytes.length)
            offset += other.bytes.length
          }
      }

      new ByteString(result)
    }

    // Operator for concatenation
    def +(other: ByteString): ByteString  = concat(other)
    def ++(other: ByteString): ByteString = concat(other)

    // Comparison (for Ordered trait)
    override def compare(that: ByteString): Int = {
      if (that == null) return 1

      val minLength = Math.min(bytes.length, that.bytes.length)
      var i         = 0
      while (i < minLength) {
        val cmp = (bytes(i) & 0xFF) - (that.bytes(i) & 0xFF) // Unsigned comparison
        if (cmp != 0) return cmp
        i += 1
      }
      bytes.length - that.bytes.length
    }

    // Equality
    override def equals(obj: Any): Boolean = obj match {
      case null => false
      case that: ByteString =>
        if (this.bytes.length != that.bytes.length) false
        else bytes.sameElements(that.bytes)
      case _ => false
    }

    override def hashCode(): Int = {
      var hash = 17
      bytes.foreach {
        b =>
          hash = hash * 31 + b
      }
      hash
    }

    // String conversions
    def toString(charset: Charset): String = new String(bytes, charset)
    override def toString: String          = toString(StandardCharsets.UTF_8)

    def toHexString: String = bytes.map("%02X".format(_)).mkString

    // Substring operations
    def substring(startIndex: Int, length: Int): ByteString = {
      if (startIndex < 0 || startIndex >= bytes.length)
        throw new IndexOutOfBoundsException(s"Start index: $startIndex")
      if (length < 0 || startIndex + length > bytes.length)
        throw new IndexOutOfBoundsException(s"Length: $length")

      val result = new Array[Byte](length)
      System.arraycopy(bytes, startIndex, result, 0, length)
      new ByteString(result)
    }

    def slice(from: Int, until: Int): ByteString = {
      substring(from, until - from)
    }

    def take(n: Int): ByteString = substring(0, Math.min(n, bytes.length))
    def drop(n: Int): ByteString = substring(Math.min(n, bytes.length), Math.max(0, bytes.length - n))

    def startsWith(other: ByteString): Boolean = {
      if (other == null || other.length > length) false
      else {
        var i = 0
        while (i < other.length) {
          if (bytes(i) != other.bytes(i)) return false
          i += 1
        }
        true
      }
    }

    def endsWith(other: ByteString): Boolean = {
      if (other == null || other.length > length) false
      else {
        val offset = length - other.length
        var i      = 0
        while (i < other.length) {
          if (bytes(offset + i) != other.bytes(i)) return false
          i += 1
        }
        true
      }
    }

    // Functional operations
    def map(f: Byte => Byte): ByteString = {
      new ByteString(bytes.map(f))
    }

    def filter(f: Byte => Boolean): ByteString = {
      new ByteString(bytes.filter(f))
    }

    def foreach(f: Byte => Unit): Unit = {
      bytes.foreach(f)
    }

    def foldLeft[B](z: B)(f: (B, Byte) => B): B = {
      bytes.foldLeft(z)(f)
    }

    def foldRight[B](z: B)(f: (Byte, B) => B): B = {
      bytes.foldRight(z)(f)
    }
  }

  /**
    * ByteString companion object with factory methods
    */
  object ByteString {

    // Factory methods
    def apply(bytes: Array[Byte]): ByteString = {
      if (bytes == null) throw new NullPointerException("bytes cannot be null")
      new ByteString(bytes.clone()) // Clone to ensure immutability
    }

    def apply(bytes: Byte*): ByteString = {
      new ByteString(bytes.toArray)
    }

    def apply(string: String, charset: Charset = StandardCharsets.UTF_8): ByteString = {
      new ByteString(string.getBytes(charset))
    }

    def fromString(string: String, charset: Charset = StandardCharsets.UTF_8): ByteString = {
      apply(string, charset)
    }

    // Static method to parse hex-encoded string into ByteString
    def parseHex(hexString: String): ByteString = {
      if (hexString == null)
        throw new NullPointerException("hexString cannot be null")

      // Remove common separators and whitespace
      val cleanHex = hexString.replaceAll("[\\s:-]", "").trim

      if (cleanHex.isEmpty)
        return empty

      if (cleanHex.length % 2 != 0)
        throw new IllegalArgumentException(
          s"Invalid hex string length: ${cleanHex.length}. Hex string must have even length."
        )

      try {
        val bytes = cleanHex
          .grouped(2).map {
            byteStr =>
              Integer.parseInt(byteStr, 16).toByte
          }.toArray
        new ByteString(bytes)
      } catch {
        case e: NumberFormatException =>
          throw new IllegalArgumentException(s"Invalid hex characters in string: ${e.getMessage}", e)
      }
    }

    // Try version that returns Try[ByteString]
    def tryParse(hexString: String): Try[ByteString] = {
      Try(parseHex(hexString))
    }

    // Empty ByteString singleton
    val empty: ByteString = new ByteString(Array.empty[Byte])

    // Builder for efficient concatenation
    class Builder {
      private val buffer = scala.collection.mutable.ArrayBuffer[Byte]()

      def +=(b: Byte): this.type = {
        buffer += b
        this
      }

      def +=(bs: ByteString): this.type = {
        buffer ++= bs.bytes
        this
      }

      def ++=(bytes: Array[Byte]): this.type = {
        buffer ++= bytes
        this
      }

      def result(): ByteString = new ByteString(buffer.toArray)
      def clear(): Unit        = buffer.clear()
    }

    def newBuilder: Builder = new Builder

    // Implicit conversions (optional, use with care)
    implicit class ByteStringOps(val sc: StringContext) extends AnyVal {
      def bs(args: Any*): ByteString = {
        val strings     = sc.parts.iterator
        val expressions = args.iterator
        val buf         = new StringBuilder(strings.next())

        while (strings.hasNext) {
          buf.append(expressions.next())
          buf.append(strings.next())
        }

        ByteString(buf.toString())
      }
    }

    // Utility methods
    @tailrec
    def concat(first: ByteString, others: ByteString*): ByteString = {
      if (first == null) {
        if (others.isEmpty) empty
        else concat(others.head, others.tail: _*)
      } else {
        first.concat(others: _*)
      }
    }
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
      writer.writeByte(kind.toInt)
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

          result += BaboonIndexEntry(offset.toLong, len.toLong)
          left       = left - 1
          prevOffset = offset.toLong
          prevLen    = len.toLong
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
      val ts_kind: Byte = {
        val offsetSecs = ts_offsetDt.getOffset.getTotalSeconds
        if (offsetSecs == 0) 1.toByte // UTC
        else {
          // Match C# DetectKind: if offset matches the local timezone's offset for this instant, it's Local (2)
          val localOffset = java.time.ZoneId.systemDefault().getRules.getOffset(ts_offsetDt.toInstant)
          if (offsetSecs == localOffset.getTotalSeconds) 2.toByte else 0.toByte
        }
      }

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

  class LEDataInputStream(stream: InputStream) extends InputStream with DataInput {
    private val dataIn           = new DataInputStream(stream)
    private val buffer           = ByteBuffer.allocate(8)
    private val order: ByteOrder = ByteOrder.LITTLE_ENDIAN

    @throws[IOException]
    override def read(b: Array[Byte]): Int = dataIn.read(b)

    @throws[IOException]
    override def read(b: Array[Byte], off: Int, len: Int): Int = dataIn.read(b, off, len)

    @throws[IOException]
    @deprecated("readLine() is deprecated", "1.0")
    override def readLine(): String = dataIn.readLine()

    @throws[IOException]
    override def readBoolean(): Boolean = dataIn.readBoolean()

    @throws[IOException]
    override def readByte(): Byte = dataIn.readByte()

    @throws[IOException]
    override def read(): Int = readByte().toInt

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
      s.foreach(c => writeChar(c.toInt))

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

  /** Runtime helpers for the `id` toString / parseRepr machinery defined in
    * `docs/spec/identifier-repr.md`. The Scala backend (PR-56) uses these
    * helpers from emitted code; other backends MUST implement an equivalent
    * helper surface conforming to the spec.
    *
    * Two copies live in the source tree:
    *   - `src/main/resources/baboon-runtime/scala/BaboonRuntimeShared.scala`
    *     (shipped to user code)
    *   - `src/main/scala/baboon/runtime/shared/BaboonRuntimeShared.scala`
    *     (compile-side mirror used by the property test)
    * They MUST stay byte-equal inside `object IdentifierRepr { ... }` so the
    * code emitted by the compiler matches what runs in user code.
    */
  object IdentifierRepr {

    // Defensive: numeric Char constant rather than a quoted-character literal.
    // The resource preprocessor in ScBaboonTranslator.scala mangles the literal
    // dot-escape sequence (used by Version.from below). Not strictly required
    // for code in this object today but maintained for consistency.
    private val BS: Char  = 92.toChar  // ASCII backslash
    private val HSH: Char = '#'
    private val COL: Char = ':'
    private val OBR: Char = '{'
    private val CBR: Char = '}'

    /** Backslash-escape the 5 metacharacters per spec §4.2. */
    def escapeStr(s: String): String = {
      val sb = new java.lang.StringBuilder(s.length)
      var i  = 0
      while (i < s.length) {
        val c = s.charAt(i)
        if (c == BS || c == HSH || c == COL || c == OBR || c == CBR) {
          sb.append(BS); sb.append(c)
        } else {
          sb.append(c)
        }
        i += 1
      }
      sb.toString
    }

    /** Lowercase hex, no separators, per spec §3 / §4.4. */
    def bytesToHex(bs: ByteString): String = {
      val arr = bs.underlyingUnsafe
      val sb  = new java.lang.StringBuilder(arr.length * 2)
      var i   = 0
      while (i < arr.length) {
        val b = arr(i) & 0xFF
        sb.append(Character.forDigit((b >>> 4) & 0xF, 16))
        sb.append(Character.forDigit(b & 0xF, 16))
        i += 1
      }
      sb.toString
    }

    // Identifier repr mandates 24-char tsu (always UTC `Z`) and 29-char tso
    // (always `±HH:MM` offset, never `Z` shorthand). These dedicated formats
    // enforce the widths the schema-directed parser depends on (spec §3 / §5.4).
    private val tsuReprFormat: java.time.format.DateTimeFormatter =
      java.time.format.DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'")
    private val tsoReprFormat: java.time.format.DateTimeFormatter =
      java.time.format.DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss.SSSxxx")

    /** Render a `tsu` per spec §3: RFC 3339 UTC, milliseconds, trailing `Z`,
      * exactly 24 characters. Normalises the input to UTC if it carries a
      * non-UTC offset.
      */
    def tsuToString(dt: java.time.OffsetDateTime): String = {
      val utc = dt.withOffsetSameInstant(java.time.ZoneOffset.UTC)
      utc.format(tsuReprFormat)
    }

    /** Render a `tso` per spec §3: RFC 3339 with explicit `±HH:MM` offset,
      * milliseconds, exactly 29 characters. Never emits `Z` shorthand.
      */
    def tsoToString(dt: java.time.OffsetDateTime): String = dt.format(tsoReprFormat)

    private val tsuLocalFormat: java.time.format.DateTimeFormatter =
      java.time.format.DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss.SSS")

    def parseTsuRepr(s: String): Either[String, java.time.OffsetDateTime] = {
      if (s.length != 24) Left(s"tsu repr must be 24 chars, got ${s.length}")
      else if (s.charAt(23) != 'Z') Left(s"tsu repr must end with 'Z', got: $s")
      else
        try {
          val ldt = java.time.LocalDateTime.parse(s.substring(0, 23), tsuLocalFormat)
          Right(ldt.atOffset(java.time.ZoneOffset.UTC))
        } catch { case _: java.time.format.DateTimeParseException => Left(s"could not parse tsu: $s") }
    }
    def parseTsoRepr(s: String): Either[String, java.time.OffsetDateTime] = {
      if (s.length != 29) Left(s"tso repr must be 29 chars, got ${s.length}")
      else
        try Right(java.time.OffsetDateTime.parse(s, tsoReprFormat))
        catch { case _: java.time.format.DateTimeParseException => Left(s"could not parse tso: $s") }
    }

    /** Render an unsigned-i64 (carried as signed Long) as decimal. */
    def u64ToString(v: Long): String = java.lang.Long.toUnsignedString(v)

    /** Render a `bit` per spec §3 — exact lowercase ASCII. */
    def bitToString(v: Boolean): String = if (v) "true" else "false"

    /** Cursor-based parser for `parseRepr` decoders. Schema-directed; the
      * caller (the emitted `<TypeName>Codec.parseRepr`) drives the field
      * sequence per declared type and order.
      */
    final class Cursor(val source: String) {
      private var pos: Int = 0

      def position: Int    = pos
      def atEnd: Boolean   = pos >= source.length
      def peek: Char       = source.charAt(pos)
      def advance(): Char  = { val c = source.charAt(pos); pos += 1; c }

      def expect(c: Char): Either[String, Unit] = {
        if (atEnd) Left(s"expected '$c' at $pos but reached end of input")
        else if (peek != c) Left(s"expected '$c' at $pos but found '${peek}'")
        else { pos += 1; Right(()) }
      }

      def expectLiteral(lit: String): Either[String, Unit] = {
        if (pos + lit.length > source.length) Left(s"expected literal '$lit' at $pos but reached end of input")
        else if (source.regionMatches(pos, lit, 0, lit.length)) { pos += lit.length; Right(()) }
        else Left(s"expected literal '$lit' at $pos")
      }

      /** Read until the next bare metachar in `:#{}`. Backslash escapes are
        * NOT processed here — see [[readStrField]] for that. Used for
        * primitive consumption (numbers, uuids, timestamps, hex bytes).
        */
      def readUntilStructural(): String = {
        val start = pos
        while (pos < source.length) {
          val c = source.charAt(pos)
          if (c == ':' || c == '#' || c == '{' || c == '}') return source.substring(start, pos)
          pos += 1
        }
        source.substring(start, pos)
      }

      /** Consume exactly `n` characters as a fixed-width lexeme. Used for
        * tsu/tso (which contain `:` characters inside the lexeme).
        */
      def readFixed(n: Int): Either[String, String] = {
        if (pos + n > source.length) Left(s"expected $n chars at $pos but only ${source.length - pos} remain")
        else {
          val s = source.substring(pos, pos + n)
          pos += n
          Right(s)
        }
      }

      /** Read a `str` field value with backslash-unescaping per spec §5.5. */
      def readStrField(): Either[String, String] = {
        val sb = new java.lang.StringBuilder()
        while (pos < source.length) {
          val c = source.charAt(pos)
          if (c == COL || c == HSH || c == OBR || c == CBR) {
            return Right(sb.toString)
          } else if (c == BS) {
            if (pos + 1 >= source.length) return Left(s"trailing backslash at $pos")
            val nxt = source.charAt(pos + 1)
            if (nxt == BS || nxt == HSH || nxt == COL || nxt == OBR || nxt == CBR) {
              sb.append(nxt); pos += 2
            } else {
              return Left(s"invalid escape at $pos")
            }
          } else {
            sb.append(c); pos += 1
          }
        }
        Right(sb.toString)
      }
    }

    /** Validate header of an identifier-repr: `<simpleName>:<version>#`.
      * Returns the cursor advanced past the `#`, or an error.
      */
    def parseHeader(cursor: Cursor, expectedSimpleName: String, expectedVersion: String): Either[String, Unit] = {
      val nameLit = cursor.readUntilStructural()
      if (nameLit != expectedSimpleName) return Left(s"expected name '$expectedSimpleName' but found '$nameLit'")
      cursor.expect(':') match {
        case Left(e)  => return Left(e)
        case Right(_) => ()
      }
      val verLit = cursor.readUntilStructural()
      if (verLit != expectedVersion) return Left(s"expected version '$expectedVersion' but found '$verLit'")
      cursor.expect('#')
    }

    /** Validate field-name segment: `<expectedFieldName>:`. */
    def parseFieldName(cursor: Cursor, expectedFieldName: String): Either[String, Unit] = {
      val name = cursor.readUntilStructural()
      if (name != expectedFieldName) return Left(s"expected field name '$expectedFieldName' but found '$name'")
      cursor.expect(':')
    }

    /** Decode `bytes` from lowercase hex. Empty string is legal (empty bytes). */
    def parseBytesHex(s: String): Either[String, ByteString] = {
      if (s.isEmpty) return Right(ByteString.empty)
      if ((s.length & 1) != 0) return Left(s"odd-length hex sequence: $s")
      val out = new Array[Byte](s.length / 2)
      var i   = 0
      while (i < s.length) {
        val hi = Character.digit(s.charAt(i), 16)
        val lo = Character.digit(s.charAt(i + 1), 16)
        if (hi < 0 || lo < 0) return Left(s"non-hex character in $s at $i")
        // Spec mandates lowercase; reject uppercase to keep exactly one canonical form.
        if (s.charAt(i).isUpper || s.charAt(i + 1).isUpper) return Left(s"uppercase hex not allowed: $s at $i")
        out(i / 2) = ((hi << 4) | lo).toByte
        i += 2
      }
      Right(ByteString(out))
    }

    /** Decode a `bit` literal. */
    def parseBit(s: String): Either[String, Boolean] = s match {
      case "true"  => Right(true)
      case "false" => Right(false)
      case other   => Left(s"expected 'true' or 'false' but found '$other'")
    }
  }

}
