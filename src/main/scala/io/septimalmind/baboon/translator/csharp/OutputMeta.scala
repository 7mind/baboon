package io.septimalmind.baboon.translator.csharp

import io.circe.Codec

case class OutputMeta(versions: List[VersionMeta])

object OutputMeta {
  implicit val codec: Codec.AsObject[OutputMeta] =
    io.circe.generic.semiauto.deriveCodec[OutputMeta]
}
