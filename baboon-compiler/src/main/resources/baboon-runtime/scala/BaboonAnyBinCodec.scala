package baboon.runtime.shared

import java.io.ByteArrayOutputStream

object BaboonAnyBinCodec {
  def encode(
    ctx: BaboonCodecContext,
    writer: LEDataOutputStream,
    expectedKind: Byte,
    staticDomain: Option[String],
    staticVersion: Option[String],
    staticTypeid: Option[String],
    value: AnyOpaque,
  ): Unit = {
    if (value.meta.kind != expectedKind) {
      throw BaboonCodecException.EncoderFailure(
        s"any: meta-kind mismatch on encode: expected 0x${(expectedKind & 0xFF).toHexString}, got 0x${(value.meta.kind & 0xFF).toHexString}"
      )
    }
    val anyBytes: Array[Byte] = value match {
      case anyUeba: AnyOpaqueUeba =>
        anyUeba.bytes
      case anyJson: AnyOpaqueJson =>
        val f = ctx.facade.getOrElse(
          throw BaboonCodecException.EncoderFailure(
            "Cannot encode AnyOpaqueJson into UEBA without a facade reference. Pass BaboonCodecContext.WithFacade(useIndices, facade) into encode(), or supply AnyOpaqueUeba directly."
          )
        )
        f.jsonToUebaBytes(anyJson.meta, anyJson.json, staticDomain, staticVersion, staticTypeid) match {
          case Right(b) => b
          case Left(e)  => throw e
        }
    }
    val anyMetaBuf    = new ByteArrayOutputStream()
    val anyMetaWriter = new LEDataOutputStream(anyMetaBuf)
    try {
      AnyMetaCodec.writeBin(value.meta, anyMetaWriter)
    } finally {
      anyMetaWriter.close()
    }
    val anyMetaBytes = anyMetaBuf.toByteArray
    val anyLength    = 4 + anyMetaBytes.length + anyBytes.length
    writer.writeInt(anyLength)
    writer.writeInt(anyMetaBytes.length)
    writer.write(anyMetaBytes)
    writer.write(anyBytes)
  }

  def decode(wire: LEDataInputStream, expectedKind: Byte): AnyOpaqueUeba = {
    val anyLength                   = wire.readInt()
    val anyMetaLen                  = wire.readInt()
    val (anyMeta, anyMetaBytesRead) = AnyMetaCodec.readBinWithLength(wire)
    if (anyMetaBytesRead > anyMetaLen) {
      throw BaboonCodecException.DecoderFailure(s"any: meta consumed $anyMetaBytesRead bytes but meta-length=$anyMetaLen")
    }
    if (anyMetaBytesRead < anyMetaLen) {
      // Skip future meta extension bytes within the meta-length window (forward-compat).
      wire.skipBytes(anyMetaLen - anyMetaBytesRead)
    }
    if (anyMeta.kind != expectedKind) {
      throw new RuntimeException(s"any: meta-kind mismatch: expected 0x${(expectedKind & 0xFF).toHexString}, got 0x${(anyMeta.kind & 0xFF).toHexString}")
    }
    val anyBlobLen = anyLength - 4 - anyMetaLen
    if (anyBlobLen < 0) {
      throw new RuntimeException(s"any: negative blob length $anyBlobLen (length=$anyLength, metaLen=$anyMetaLen)")
    }
    val anyBlob = new Array[Byte](anyBlobLen)
    wire.readFully(anyBlob)
    AnyOpaqueUeba(anyMeta, anyBlob)
  }
}
