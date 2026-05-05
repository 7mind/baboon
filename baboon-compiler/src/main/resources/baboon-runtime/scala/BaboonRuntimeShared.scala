package baboon.runtime.shared {

  import baboon.runtime.shared.BaboonTypeMetaCodec.META_VERSION
  import io.circe.{DecodingFailure, Json}

  import java.util.concurrent.atomic.AtomicReference
  import scala.reflect.ClassTag
  import scala.util.{Success, Try}

  trait BaboonGenerated {
    def baboonDomainVersion: String
    def baboonDomainIdentifier: String
    def baboonSameInVersions: List[String]
    def baboonTypeIdentifier: String

    final def domainVersion: BaboonDomainVersion = BaboonDomainVersion(baboonDomainIdentifier, baboonDomainVersion)
  }

  trait BaboonGeneratedLatest extends BaboonGenerated

  trait BaboonAdtMemberMeta {
    def baboonAdtTypeIdentifier: String
    def baboonAdtType: Class[?]
  }

  trait BaboonMeta {
    def sameInVersions(typeId: String): List[String]
  }

  trait BaboonEnum[T] {
    def parse(s: String): Option[T]
    def all: List[T]
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
    private val BS: Char  = 92.toChar // ASCII backslash
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

      def position: Int   = pos
      def atEnd: Boolean  = pos >= source.length
      def peek: Char      = source.charAt(pos)
      def advance(): Char = { val c = source.charAt(pos); pos += 1; c }

      def expect(c: Char): Either[String, Unit] = {
        if (atEnd) Left(s"expected '$c' at $pos but reached end of input")
        else if (peek != c) Left(s"expected '$c' at $pos but found '$peek'")
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

  trait BaboonSingleton[T] {
    protected def LazyInstance: Lazy[T]
    def instance: T = LazyInstance.value
  }

  final case class BaboonDomainVersion(domainIdentifier: String, domainVersion: String) {
    private val LazyVersion = Lazy(Version.from(domainVersion))
    def version: Version    = LazyVersion.value
  }

  final case class Version(major: Int, minor: Int, patch: Int) extends Ordered[Version] {
    override def compare(that: Version): Int = Version.ordering.compare(this, that)
  }
  object Version {
    def from(version: String): Version = {
      val chunks = version.split("\\.")
      if (chunks.size <= 0) {
        throw new Exception(s"Expected to have version in format x.[y].[z], got $version");
      }

      val major = Try(chunks(0).trim.toInt) match {
        case Success(value) => value
        case _              => throw new Exception(s"Expected to have version in format x.[y].[z], got $version. Invalid major value.");
      }

      val minor = Try(chunks(1).trim.toInt) match {
        case Success(value) => value
        case _              => throw new Exception(s"Expected to have version in format x.[y].[z], got $version. Invalid minor value.");
      }

      val patch = Try(chunks(2).trim.toInt) match {
        case Success(value) => value
        case _              => throw new Exception(s"Expected to have version in format x.[y].[z], got $version. Invalid patch value.");
      }

      new Version(major, minor, patch)
    }

    implicit val ordering: Ordering[Version] = Ordering.by(v => (v.major, v.minor, v.patch))
  }

  final case class BaboonTypeMeta(
    metaVersion: Byte,
    domainIdentifier: String,
    domainVersion: String,
    domainVersionMinCompat: String,
    typeIdentifier: String,
  ) {
    def version: BaboonDomainVersion = BaboonDomainVersion(domainIdentifier, domainVersion)
    def versionMinCompat: Option[BaboonDomainVersion] = {
      domainVersionMinCompat match {
        case v if v.isEmpty          => None
        case v if v == domainVersion => None
        case _                       => Some(BaboonDomainVersion(domainIdentifier, domainVersionMinCompat))
      }
    }

    def writeBin(writer: LEDataOutputStream): Unit = {
      BaboonTypeMetaCodec.writeBin(this, writer)
    }

    def writeJson: Json = {
      BaboonTypeMetaCodec.writeJson(this)
    }
  }

  object BaboonTypeMeta {
    /* Codecs discovery with ADTs check to ensure that ADTs is encoded with a codec type desired by the user.
     *
     * - If user is trying to encode ADT branch with the base type we should encode it with ADT meta header:
     *      `Encode<ADT>(new ADT.A())` -> {"A": {<encoded A>}}
     * - If user is trying to encode ADT branch with its own type we should encode only branch, without any meta:
     *      `Encode<ADT.A>(new ADT.A())` -> {<encoded A>}
     */
    def from[T <: BaboonGenerated: ClassTag](value: T): BaboonTypeMeta = {
      val typeIdentifier = value match {
        case adt: BaboonAdtMemberMeta if typeIsTrait[T] => adt.baboonAdtTypeIdentifier
        case _                                          => value.baboonTypeIdentifier
      }

      new BaboonTypeMeta(
        META_VERSION,
        value.baboonDomainIdentifier,
        value.baboonDomainVersion,
        value.baboonSameInVersions.head,
        typeIdentifier,
      )
    }

    private def typeIsTrait[T](implicit ct: ClassTag[T]): Boolean = {
      ct.runtimeClass.isInterface
    }

    def readMeta(reader: LEDataInputStream): Option[BaboonTypeMeta] = {
      BaboonTypeMetaCodec.readMeta(reader)
    }

    def readMeta(json: Json): Option[BaboonTypeMeta] = {
      BaboonTypeMetaCodec.readMeta(json)
    }
  }

  object BaboonTypeMetaCodec {
    private val META_VERSION_1: Byte = 16
    val META_VERSION: Byte           = META_VERSION_1

    private val META_VERSION_KEY              = "$mv"
    private val DOMAIN_IDENTIFIER_KEY         = "$d"
    private val DOMAIN_VERSION_KEY            = "$v"
    private val DOMAIN_VERSION_MIN_COMPAT_KEY = "$uv"
    private val TYPE_IDENTIFIER_KEY           = "$t"

    def writeBin(meta: BaboonTypeMeta, writer: LEDataOutputStream): Unit = {
      writer.write(META_VERSION_1.toInt)
      BaboonBinTools.writeString(writer, meta.domainIdentifier)
      BaboonBinTools.writeString(writer, meta.domainVersion)
      if (meta.domainVersion == meta.domainVersionMinCompat) {
        writer.write(0)
      } else {
        writer.write(1)
        BaboonBinTools.writeString(writer, meta.domainVersionMinCompat)
      }
      BaboonBinTools.writeString(writer, meta.typeIdentifier)
    }

    def writeJson(meta: BaboonTypeMeta): Json = {
      val json = Json.obj(
        DOMAIN_IDENTIFIER_KEY -> Json.fromString(meta.domainIdentifier),
        DOMAIN_VERSION_KEY    -> Json.fromString(meta.domainVersion),
        TYPE_IDENTIFIER_KEY   -> Json.fromString(meta.typeIdentifier),
      )
      if (meta.domainVersion != meta.domainVersionMinCompat) {
        json.mapObject(_.add(DOMAIN_VERSION_MIN_COMPAT_KEY, Json.fromString(meta.domainVersionMinCompat)))
      } else json
    }

    def readMeta(reader: LEDataInputStream): Option[BaboonTypeMeta] = {
      val metaVersion = reader.readByte()
      if (metaVersion == META_VERSION_1) {
        readMetaV1(reader)
      } else None
    }

    def readMeta(json: Json): Option[BaboonTypeMeta] = {
      if (!json.isObject) return None

      json.hcursor.downField(META_VERSION_KEY).as[String].toOption match {
        case Some(value) =>
          if (value.toByte == META_VERSION_1) readMetaV1(json) else None
        case None => readMetaV1(json)
      }
    }

    private def readMetaV1(reader: LEDataInputStream): Option[BaboonTypeMeta] = {
      val domainIdentifier       = BaboonBinTools.readString(reader)
      val domainVersion          = BaboonBinTools.readString(reader)
      val domainVersionMinCompat = if (reader.readByte() == 1) BaboonBinTools.readString(reader) else domainVersion
      val typeIdentifier         = BaboonBinTools.readString(reader)

      Some(
        BaboonTypeMeta(
          META_VERSION_1,
          domainIdentifier,
          domainVersion,
          domainVersionMinCompat,
          typeIdentifier,
        )
      )
    }

    private def readMetaV1(json: Json): Option[BaboonTypeMeta] = {
      val cursor = json.hcursor
      (for {
        domainIdentifier <- cursor.downField(DOMAIN_IDENTIFIER_KEY).as[String]
        domainVersion    <- cursor.downField(DOMAIN_VERSION_KEY).as[String]
        typeIdentifier   <- cursor.downField(TYPE_IDENTIFIER_KEY).as[String]
        domainVersionMinCompat <- cursor
          .downField(DOMAIN_VERSION_MIN_COMPAT_KEY)
          .focus.fold[Either[DecodingFailure, String]](Right(domainVersion))(_.as[String])
      } yield BaboonTypeMeta(
        META_VERSION_1,
        domainIdentifier,
        domainVersion,
        domainVersionMinCompat,
        typeIdentifier,
      )).toOption
    }
  }
}
