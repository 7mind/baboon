package baboon.runtime.shared

import io.circe.Json

object BaboonAnyJsonCodec {
  def encode(
    ctx: BaboonCodecContext,
    expectedKind: Byte,
    staticDomain: Option[String],
    staticVersion: Option[String],
    staticTypeid: Option[String],
    value: AnyOpaque,
  ): Json = {
    if (value.meta.kind != expectedKind) {
      throw BaboonCodecException.EncoderFailure(
        s"any: meta-kind mismatch on encode: expected 0x${(expectedKind & 0xFF).toHexString}, got 0x${(value.meta.kind & 0xFF).toHexString}"
      )
    }
    val innerJson: Json = value match {
      case anyJson: AnyOpaqueJson =>
        anyJson.json
      case anyUeba: AnyOpaqueUeba =>
        val f = ctx.facade.getOrElse(
          throw BaboonCodecException.EncoderFailure(
            "Cannot encode AnyOpaqueUeba into JSON without a facade reference. Pass BaboonCodecContext.WithFacade(useIndices, facade) into encode(), or supply AnyOpaqueJson directly."
          )
        )
        f.uebaToJson(ctx, anyUeba.meta, anyUeba.bytes, staticDomain, staticVersion, staticTypeid) match {
          case Right(j) => j
          case Left(e)  => throw e
        }
    }
    AnyMetaCodec.writeJson(value.meta).mapObject(_.add("$c", innerJson))
  }

  def decode(expectedKind: Byte, wire: Json): Either[Throwable, AnyOpaqueJson] = {
    AnyMetaCodec.readJson(wire) match {
      case Left(e) => Left(e)
      case Right(meta) =>
        if (meta.kind != expectedKind) {
          Left(BaboonCodecException.DecoderFailure(s"any: meta-kind mismatch: expected 0x${(expectedKind & 0xFF).toHexString}, got 0x${(meta.kind & 0xFF).toHexString}"))
        } else {
          wire.hcursor.downField("$c").as[Json] match {
            case Right(content) => Right(AnyOpaqueJson(meta, content))
            case Left(err)      => Left(BaboonCodecException.DecoderFailure(s"any: missing or unreadable content key: ${err.getMessage}"))
          }
        }
    }
  }
}
