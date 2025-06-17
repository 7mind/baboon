package baboon.runtime.shared {

  import java.io.{DataInputStream, DataOutputStream}
  import java.math.BigDecimal
  import java.nio.charset.StandardCharsets
  import java.time.OffsetDateTime
  import java.util.UUID
  import java.util.concurrent.atomic.AtomicReference

  trait BaboonGenerated {}
  trait BaboonAdtMemberMeta {}
  trait BaboonGeneratedLatest {}
  trait BaboonTypeCodecs {}
  trait BaboonAbstractConversion[F, T] {
    def doConvert[C](
      context: C,
      conversions: BaboonAbstractConversions,
      from: F,
    ): T

    def versionFrom: String
    def versionTo: String
    def typeId: String
  }

  trait BaboonAbstractConversions {
    def register[F, T](conversion: BaboonAbstractConversion[F, T]): Unit = ???

    def convertWithContext[C, F, T](
      context: C,
      from: F,
    ): T = ???

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

    def nextTsu(): OffsetDateTime = java.time.OffsetDateTime.MIN.plusNanos(rnd.nextLong())
    def nextTso(): OffsetDateTime = java.time.OffsetDateTime.MIN.plusNanos(rnd.nextLong())
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

  trait BaboonCodecContext {}
  object BaboonCodecContext {
    object Default extends BaboonCodecContext {}
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
    def parse(s: String): Option[OffsetDateTime]                             = ???
    def format(s: OffsetDateTime): String                                    = ???
    def decodeFromBin(s: DataInputStream): OffsetDateTime                    = ???
    def encodeToBin(f09: OffsetDateTime, fakeWriter: DataOutputStream): Unit = ???

  }

  trait BaboonBinCodec[T] {
    def encode(ctx: BaboonCodecContext, writer: DataOutputStream, value: T): Unit
    def decode(ctx: BaboonCodecContext, wire: DataInputStream): T
  }

  case class BaboonIndexEntry(offset: Long, length: Long)

  trait BaboonBinCodecIndexed {
    def indexElementsCount(ctx: BaboonCodecContext): Short

    def readIndex(ctx: BaboonCodecContext, wire: DataInputStream): List[BaboonIndexEntry] = {
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
    def readUid(s: DataInputStream): UUID = new UUID(s.readLong(), s.readLong())
    def writeUid(fakeWriter: DataOutputStream, v: UUID): Unit = {
      fakeWriter.writeLong(v.getMostSignificantBits)
      fakeWriter.writeLong(v.getLeastSignificantBits)
    }

    def readString(input: DataInputStream): String = {
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
    def writeString(output: DataOutputStream, s: String): Unit = {
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

    def readBigDecimal(input: DataInputStream): BigDecimal = {
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

    def writeBigDecimal(output: DataOutputStream, value: scala.math.BigDecimal): Unit = {
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

}
