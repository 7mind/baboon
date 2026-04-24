package baboon.runtime.shared {

  import io.circe.Json

  sealed trait AnyOpaque {
    def meta: AnyMeta
  }

  final case class AnyOpaqueUeba(meta: AnyMeta, bytes: Array[Byte]) extends AnyOpaque

  final case class AnyOpaqueJson(meta: AnyMeta, json: Json) extends AnyOpaque

  final case class AnyMeta(
    kind: Byte,
    domain: Option[String],
    version: Option[String],
    typeid: Option[String],
  ) {
    require(
      ((kind & AnyMetaCodec.DOMAIN_BIT) != 0) == domain.isDefined,
      s"AnyMeta: domain presence (${domain.isDefined}) does not match kind 0x${(kind & 0xFF).toHexString} bit 2",
    )
    require(
      ((kind & AnyMetaCodec.VERSION_BIT) != 0) == version.isDefined,
      s"AnyMeta: version presence (${version.isDefined}) does not match kind 0x${(kind & 0xFF).toHexString} bit 1",
    )
    require(
      ((kind & AnyMetaCodec.TYPEID_BIT) != 0) == typeid.isDefined,
      s"AnyMeta: typeid presence (${typeid.isDefined}) does not match kind 0x${(kind & 0xFF).toHexString} bit 0",
    )
    require(
      AnyMetaCodec.VALID_KINDS.contains(kind),
      f"AnyMeta: reserved or invalid meta-kind byte: 0x${kind.toInt & 0xFF}%02x",
    )
  }

  object AnyMetaCodec {
    val DOMAIN_BIT: Byte  = 0x04
    val VERSION_BIT: Byte = 0x02
    val TYPEID_BIT: Byte  = 0x01

    val ANY_KIND_KEY    = "$ak"
    val ANY_DOMAIN_KEY  = "$ad"
    val ANY_VERSION_KEY = "$av"
    val ANY_TYPEID_KEY  = "$at"

    val VALID_KINDS: Set[Byte] = Set[Byte](0x00.toByte, 0x01.toByte, 0x02.toByte, 0x03.toByte, 0x06.toByte, 0x07.toByte)

    def writeBin(meta: AnyMeta, writer: LEDataOutputStream): Unit = {
      writer.writeByte(meta.kind & 0xFF)
      meta.domain.foreach(s => BaboonBinTools.writeString(writer, s))
      meta.version.foreach(s => BaboonBinTools.writeString(writer, s))
      meta.typeid.foreach(s => BaboonBinTools.writeString(writer, s))
    }

    def readBin(reader: LEDataInputStream): AnyMeta = {
      val kind    = reader.readByte()
      val domain  = if ((kind & DOMAIN_BIT) != 0) Some(BaboonBinTools.readString(reader)) else None
      val version = if ((kind & VERSION_BIT) != 0) Some(BaboonBinTools.readString(reader)) else None
      val typeid  = if ((kind & TYPEID_BIT) != 0) Some(BaboonBinTools.readString(reader)) else None
      AnyMeta(kind, domain, version, typeid)
    }

    def writeJson(meta: AnyMeta): Json = {
      val pairs: List[(String, Json)] =
        (ANY_KIND_KEY -> Json.fromInt(meta.kind & 0xFF)) ::
        meta.domain.map(s => ANY_DOMAIN_KEY -> Json.fromString(s)).toList :::
        meta.version.map(s => ANY_VERSION_KEY -> Json.fromString(s)).toList :::
        meta.typeid.map(s => ANY_TYPEID_KEY -> Json.fromString(s)).toList
      Json.fromFields(pairs)
    }

    def readJson(json: Json): Either[BaboonCodecException, AnyMeta] = {
      val cursor     = json.hcursor
      val kindEither = cursor.downField(ANY_KIND_KEY).as[Int].map(_.toByte)
      kindEither match {
        case Left(err) =>
          Left(BaboonCodecException.DecoderFailure(s"AnyMetaCodec.readJson: missing or invalid '$ANY_KIND_KEY' field: ${err.getMessage}"))
        case Right(kind) =>
          for {
            domain  <- readOptString(cursor, ANY_DOMAIN_KEY, kind, DOMAIN_BIT, "domain")
            version <- readOptString(cursor, ANY_VERSION_KEY, kind, VERSION_BIT, "version")
            typeid  <- readOptString(cursor, ANY_TYPEID_KEY, kind, TYPEID_BIT, "typeid")
            meta <- scala.util
              .Try(AnyMeta(kind, domain, version, typeid)).toEither.left.map(
                e => BaboonCodecException.DecoderFailure(s"AnyMetaCodec.readJson: invalid meta: ${e.getMessage}")
              )
          } yield meta
      }
    }

    private def readOptString(
      cursor: io.circe.HCursor,
      key: String,
      kind: Byte,
      bit: Byte,
      name: String,
    ): Either[BaboonCodecException, Option[String]] = {
      val present = (kind & bit) != 0
      val value   = cursor.downField(key).as[String].toOption
      (present, value) match {
        case (true, Some(v)) => Right(Some(v))
        case (false, None)   => Right(None)
        case (true, None) =>
          Left(BaboonCodecException.DecoderFailure(s"AnyMetaCodec.readJson: kind 0x${(kind & 0xFF).toHexString} requires '$key' ($name) but it is missing"))
        case (false, Some(_)) =>
          Left(BaboonCodecException.DecoderFailure(s"AnyMetaCodec.readJson: kind 0x${(kind & 0xFF).toHexString} forbids '$key' ($name) but it is present"))
      }
    }
  }
}
