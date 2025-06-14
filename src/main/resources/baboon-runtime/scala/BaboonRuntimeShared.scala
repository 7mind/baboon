package baboon.runtime.shared {

  import java.time.OffsetDateTime
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
    def nextF128(): BigDecimal = scala.math.BigDecimal(rnd.nextDouble())

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
  }

  trait BaboonCodecContext {}

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
    def parse(s: String): Option[OffsetDateTime] = ???
    def format(s: OffsetDateTime): String        = ???
  }

}
