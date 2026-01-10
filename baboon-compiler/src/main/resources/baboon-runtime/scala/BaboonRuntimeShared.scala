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
    private val META_VERSION_1: Byte = 1
    val META_VERSION: Byte           = META_VERSION_1

    private val META_VERSION_KEY              = "$mv"
    private val DOMAIN_IDENTIFIER_KEY         = "$d"
    private val DOMAIN_VERSION_KEY            = "$v"
    private val DOMAIN_VERSION_MIN_COMPAT_KEY = "$uv"
    private val TYPE_IDENTIFIER_KEY           = "$t"

    def writeBin(meta: BaboonTypeMeta, writer: LEDataOutputStream): Unit = {
      writer.write(META_VERSION_1)
      BaboonBinTools.writeString(writer, meta.domainIdentifier)
      BaboonBinTools.writeString(writer, meta.domainVersion)
      if (meta.domainVersion == meta.domainVersionMinCompat) {
        writer.write(0.toByte)
      } else {
        writer.write(1.toByte)
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
      if (metaVersion == 1) {
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
