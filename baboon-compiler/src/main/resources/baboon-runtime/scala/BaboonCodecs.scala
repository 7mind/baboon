package baboon.runtime.shared {

  import io.circe.numbers.BiggerDecimal
  import io.circe.{Decoder, Json, JsonObject, KeyDecoder}

  import java.io.*
  import java.nio.{ByteBuffer, ByteOrder}
  import java.time.OffsetDateTime
  import scala.collection.mutable
  import scala.reflect.ClassTag
  import scala.util.Try

  trait BaboonCodecData {
    def baboonDomainVersion: String
    def baboonDomainIdentifier: String
    def baboonTypeIdentifier: String
  }

  /** Which lower bound the WRITER publishes as the UEBA envelope's `domainVersionMinCompat`
    * (the v1 binary envelope has a single bound slot; see docs/forward-compat.md, "Envelope
    * integration (UEBA)").
    *   - Strict: the byte-identical bound (`baboonSameInVersions.head`) — the default.
    *   - Tolerant: the prefix-read bound for the chosen index mode (`prefix-compact` for compact
    *     payloads, `prefix-any-mode` for indexed ones). Readers older than the writer then decode
    *     the payload with their newest codec, dropping the appended fields they do not know.
    *     A reader cannot distinguish such an envelope from a byte-identical one, so re-encoding
    *     intermediaries must run at the writer's version or newer.
    */
  sealed trait ForwardWritePolicy
  object ForwardWritePolicy {
    case object Strict extends ForwardWritePolicy
    case object Tolerant extends ForwardWritePolicy
  }

  /** Which top-level binary envelope layout the WRITER emits (docs/spec/codec-envelope.md §2.1).
    *   - V1 (default): single bound slot (`domainVersionMinCompat`), value chosen by [[ForwardWritePolicy]].
    *   - V2: JSON-equivalent layout carrying both the byte-identical bound and the prefix-read
    *     bound for the payload's index mode; the reader's `ForwardReadPolicy` then applies to
    *     binary exactly as it does to JSON. Only readers that know v2 can decode it.
    */
  sealed trait BaboonEnvelopeVersion
  object BaboonEnvelopeVersion {
    case object V1 extends BaboonEnvelopeVersion
    case object V2 extends BaboonEnvelopeVersion
  }

  trait BaboonCodecContext {
    def useIndices: Boolean
    def forwardWritePolicy: ForwardWritePolicy = ForwardWritePolicy.Strict
    def envelopeVersion: BaboonEnvelopeVersion = BaboonEnvelopeVersion.V1
    // Optional facade reference, supplied only when an `any`-bearing codec needs to cross-convert
    // between UEBA and JSON branches (`AnyOpaqueJson` → UEBA, `AnyOpaqueUeba` → JSON). Defaults to
    // `None` so existing call sites stay compatible; users who need cross-convert pass
    // `BaboonCodecContext.WithFacade(useIndices, facade)` instead of `Compact`/`Indexed`.
    def facade: Option[BaboonCodecsFacade] = None
  }
  object BaboonCodecContext {
    val Default: BaboonCodecContext = Compact

    object Indexed extends BaboonCodecContext {
      override def useIndices: Boolean = true
    }

    object Compact extends BaboonCodecContext {
      override def useIndices: Boolean = false
    }

    final case class WithFacade(useIndices: Boolean, baboonFacade: BaboonCodecsFacade) extends BaboonCodecContext {
      override def facade: Option[BaboonCodecsFacade] = Some(baboonFacade)
    }

    /** Fully specified context: index mode, writer-side forward policy, envelope layout and optional facade. */
    final case class Custom(
      useIndices: Boolean,
      override val forwardWritePolicy: ForwardWritePolicy,
      override val envelopeVersion: BaboonEnvelopeVersion,
      override val facade: Option[BaboonCodecsFacade],
    ) extends BaboonCodecContext
  }

  trait BaboonCodec[T] extends BaboonCodecData

  trait BaboonValueCodec[T, TWire] extends BaboonCodec[T] {
    def encode(ctx: BaboonCodecContext, instance: T): TWire
    def decode(ctx: BaboonCodecContext, wire: TWire): Either[Throwable, T]
  }
  trait BaboonStreamCodec[T, TOut, TIn] extends BaboonCodec[T] {
    def encode(ctx: BaboonCodecContext, writer: TOut, instance: T): Unit
    def decode(ctx: BaboonCodecContext, wire: TIn): Either[Throwable, T]
  }

  trait BaboonJsonCodec[T] extends BaboonValueCodec[T, Json] {
    protected final def getField(jsonObject: JsonObject, name: String): Either[Exception, Json] = {
      jsonObject(name).toRight(new RuntimeException(s"Cannot decode $jsonObject to $baboonTypeIdentifier: missing fields $name"))
    }

    final def circeDecoder: Decoder[T] = {
      Decoder.instanceTry(cursor => this.decode(BaboonCodecContext.Compact, cursor.value).toTry)
    }
  }

  object BaboonJsonCodec {
    val decodeTsu: Decoder[OffsetDateTime]    = Decoder.instanceTry(cursor => cursor.as[String].toTry.flatMap(BaboonTimeFormats.parseTsu))
    val decodeTso: Decoder[OffsetDateTime]    = Decoder.instanceTry(cursor => cursor.as[String].toTry.flatMap(BaboonTimeFormats.parseTso))
    val decodeByteString: Decoder[ByteString] = Decoder.instanceTry(cursor => cursor.as[String].toTry.flatMap(ByteString.tryParse))
    val decodeByte: Decoder[Byte]             = Decoder.instance(cursor => Decoder.decodeLong(cursor).map(_.toByte))
    val decodeShort: Decoder[Short]           = Decoder.instance(cursor => Decoder.decodeLong(cursor).map(_.toShort))
    val decodeInt: Decoder[Int]               = Decoder.instance(cursor => Decoder.decodeLong(cursor).map(_.toInt))
    val decodeLong: Decoder[Long]             = Decoder.instance(cursor => Decoder.decodeBigInt(cursor).map(_.longValue))
    val decodeBigDecimalLenient: Decoder[BigDecimal] = Decoder.instance {
      cursor =>
        cursor.as[BigDecimal](Decoder.decodeBigDecimal) match {
          case Right(v) => Right(v)
          case Left(_)  => cursor.as[String].map(s => BigDecimal(s))
        }
    }

    val decodeKeyBoolean: KeyDecoder[Boolean]        = _.toBooleanOption
    val decodeKeyFloat: KeyDecoder[Float]            = _.toFloatOption
    val keyDecoderBigDecimal: KeyDecoder[BigDecimal] = KeyDecoder.instance(s => BiggerDecimal.parseBiggerDecimal(s).flatMap(_.toBigDecimal).map(new BigDecimal(_)))
    val decodeKeyTso: KeyDecoder[OffsetDateTime]     = BaboonTimeFormats.parseTso(_).toOption
    val decodeKeyTsu: KeyDecoder[OffsetDateTime]     = BaboonTimeFormats.parseTsu(_).toOption
    val decodeKeyByteString: KeyDecoder[ByteString]  = ByteString.tryParse(_).toOption
    // PR-28.1 (M28): u64 map keys carry the canonical unsigned wire form on the JSON
    // string. circe ships only `decodeKeyLong` (signed two's-complement), which would
    // reject "18446744073709551615" (= u64 max). Parse via BigInt, validate the
    // unsigned-i64 range, then narrow to Long (two's-complement preserving). Symmetric
    // with the value-side `Json.fromBigInt(BaboonBinTools.toUnsignedBigInt(_))` encoder.
    val decodeKeyU64: KeyDecoder[Long] = KeyDecoder.instance {
      s =>
        scala.util.Try(scala.math.BigInt(s)).toOption.flatMap {
          bi =>
            if (bi >= 0 && bi < scala.math.BigInt(2).pow(64)) Some(bi.toLong) else None
        }
    }

    trait BaboonGeneratedJsonCodec[T <: BaboonGenerated, TCodec <: BaboonJsonCodec[T]] {
      self: Base[T, TCodec] =>
      final def encode(ctx: BaboonCodecContext, instance: BaboonGenerated)(implicit t: ClassTag[T]): Json = {
        instance match {
          case v: T => self.encode(ctx, v)
          case _    => throw new RuntimeException(s"Expected to have ${t.runtimeClass.getName} type")
        }
      }
    }
    trait Base[T, TCodec <: BaboonJsonCodec[T]] extends BaboonJsonCodec[T] with BaboonSingleton[TCodec]
    trait BaseGenerated[T <: BaboonGenerated, TCodec <: BaboonJsonCodec[T]] extends Base[T, TCodec] with BaboonGeneratedJsonCodec[T, TCodec]
    trait BaseGeneratedAdt[T <: BaboonGenerated, TCodec <: BaboonJsonCodec[T]] extends BaseGenerated[T, TCodec] with BaboonAdtMemberMeta
    trait NoEncoder[T, TCodec <: BaboonJsonCodec[T]] extends Base[T, TCodec] {
      override def encode(ctx: BaboonCodecContext, instance: T): Json = {
        if (this ne LazyInstance.value) {
          LazyInstance.value.encode(ctx, instance)
        } else {
          throw new RuntimeException(s"Type $baboonTypeIdentifier@$baboonDomainVersion is deprecated, encoder was not generated")
        }
      }
    }
    trait NoEncoderGenerated[T <: BaboonGenerated, TCodec <: BaboonJsonCodec[T]] extends BaseGenerated[T, TCodec] {
      override def encode(ctx: BaboonCodecContext, instance: T): Json = {
        if (this ne LazyInstance.value) {
          LazyInstance.value.encode(ctx, instance)
        } else {
          throw new RuntimeException(s"Type $baboonTypeIdentifier@$baboonDomainVersion is deprecated, encoder was not generated")
        }
      }
    }
    trait NoEncoderGeneratedAdt[T <: BaboonGenerated, TCodec <: BaboonJsonCodec[T]] extends BaseGeneratedAdt[T, TCodec] {
      override def encode(ctx: BaboonCodecContext, instance: T): Json = {
        if (this ne LazyInstance.value) {
          LazyInstance.value.encode(ctx, instance)
        } else {
          throw new RuntimeException(s"Type $baboonTypeIdentifier@$baboonDomainVersion is deprecated, encoder was not generated")
        }
      }
    }
  }

  trait BaboonBinCodec[T] extends BaboonStreamCodec[T, LEDataOutputStream, LEDataInputStream]

  object BaboonBinCodec {
    trait BaboonGeneratedBinCodec[T <: BaboonGenerated, TCodec <: BaboonBinCodec[T]] {
      self: Base[T, TCodec] =>
      final def encode(ctx: BaboonCodecContext, writer: LEDataOutputStream, instance: BaboonGenerated)(implicit t: ClassTag[T]): Unit = {
        instance match {
          case v: T => self.encode(ctx, writer, v)
          case _    => throw new RuntimeException(s"Expected to have ${t.runtimeClass.getName} type")
        }
      }
    }
    trait Base[T, TCodec <: BaboonBinCodec[T]] extends BaboonBinCodec[T] with BaboonSingleton[TCodec]
    trait BaseGenerated[T <: BaboonGenerated, TCodec <: BaboonBinCodec[T]] extends Base[T, TCodec] with BaboonGeneratedBinCodec[T, TCodec]
    trait BaseGeneratedAdt[T <: BaboonGenerated, TCodec <: BaboonBinCodec[T]] extends BaseGenerated[T, TCodec] with BaboonAdtMemberMeta
    trait NoEncoder[T, TCodec <: BaboonBinCodec[T]] extends Base[T, TCodec] {
      override def encode(ctx: BaboonCodecContext, writer: LEDataOutputStream, instance: T): Unit = {
        if (this ne LazyInstance.value) {
          LazyInstance.value.encode(ctx, writer, instance)
        }
        throw new RuntimeException(s"Type $baboonTypeIdentifier@$baboonDomainVersion is deprecated, encoder was not generated")
      }
    }
    trait NoEncoderGenerated[T <: BaboonGenerated, TCodec <: BaboonBinCodec[T]] extends BaseGenerated[T, TCodec] {
      override def encode(ctx: BaboonCodecContext, writer: LEDataOutputStream, instance: T): Unit = {
        if (this ne LazyInstance.value) {
          LazyInstance.value.encode(ctx, writer, instance)
        }
        throw new RuntimeException(s"Type $baboonTypeIdentifier@$baboonDomainVersion is deprecated, encoder was not generated")
      }
    }
    trait NoEncoderGeneratedAdt[T <: BaboonGenerated, TCodec <: BaboonBinCodec[T]] extends BaseGeneratedAdt[T, TCodec] {
      override def encode(ctx: BaboonCodecContext, writer: LEDataOutputStream, instance: T): Unit = {
        if (this ne LazyInstance.value) {
          LazyInstance.value.encode(ctx, writer, instance)
        }
        throw new RuntimeException(s"Type $baboonTypeIdentifier@$baboonDomainVersion is deprecated, encoder was not generated")
      }
    }
  }

  case class BaboonIndexEntry(offset: Long, length: Long)

  trait BaboonBinCodecIndexed {
    def indexElementsCount(ctx: BaboonCodecContext): Short

    def readIndex(ctx: BaboonCodecContext, wire: LEDataInputStream): Either[Throwable, List[BaboonIndexEntry]] = {
      Try {
        val result = scala.collection.mutable.ListBuffer.empty[BaboonIndexEntry]
        readIndexInto(ctx, wire, Some(result))
        result.toList
      }.toEither
    }

    def readIndexCount(ctx: BaboonCodecContext, wire: LEDataInputStream): Either[Throwable, Int] =
      Try(readIndexInto(ctx, wire, None)).toEither

    private def readIndexInto(
      ctx: BaboonCodecContext,
      wire: LEDataInputStream,
      entries: Option[scala.collection.mutable.ListBuffer[BaboonIndexEntry]],
    ): Int = {
      val header           = wire.readByte()
      val isIndexed        = (header & 0x01) != 0
      var prevOffset: Long = 0L
      var prevLen: Long    = 0L
      var count            = 0
      if (isIndexed) {
        var left = indexElementsCount(ctx).toInt
        while (left > 0) {
          val offset = wire.readInt()
          val len    = wire.readInt()
          require(len > 0, "Length must be positive")
          require(offset >= prevOffset + prevLen, s"Offset violation: $offset not >= ${prevOffset + prevLen}")
          entries match {
            case Some(result) => result += BaboonIndexEntry(offset.toLong, len.toLong)
            case None         => ()
          }
          count += 1
          left -= 1
          prevOffset = offset.toLong
          prevLen    = len.toLong
        }
      }
      count
    }
  }

  trait AbstractBaboonCodecs {
    private val registry = mutable.Map.empty[String, Lazy[BaboonCodecData]]

    def register(id: String, codec: Lazy[BaboonCodecData]): Unit = {
      val _ = registry.put(id, codec)
    }

    def find(id: String): Lazy[BaboonCodecData] = {
      registry(id)
    }

    def tryFind(id: String): Option[Lazy[BaboonCodecData]] = {
      registry.get(id)
    }
  }

  trait AbstractBaboonJsonCodecs extends AbstractBaboonCodecs
  trait AbstractBaboonUebaCodecs extends AbstractBaboonCodecs

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
}
