package io.septimalmind.baboon.translator.csharp

import io.circe.Codec

case class VersionMeta(model: String, version: String)

object VersionMeta {
  implicit val codec: Codec.AsObject[VersionMeta] =
    io.circe.generic.semiauto.deriveCodec[VersionMeta]
}
