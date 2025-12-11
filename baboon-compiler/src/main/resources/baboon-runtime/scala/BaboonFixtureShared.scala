package baboon.fixture {

  import baboon.runtime.shared.{BaboonEnum, ByteString}

  import java.math.BigDecimal
  import java.time.{Instant, OffsetDateTime, ZoneOffset}

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
}
