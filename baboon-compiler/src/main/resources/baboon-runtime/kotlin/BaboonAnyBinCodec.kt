package baboon.runtime.shared

import java.io.ByteArrayOutputStream

object BaboonAnyBinCodec {
  fun encode(
      ctx: BaboonCodecContext,
      writer: LEDataOutputStream,
      expectedKind: Byte,
      staticDomain: String?,
      staticVersion: String?,
      staticTypeid: String?,
      value: AnyOpaque,
  ) {
      if (value.meta.kind != expectedKind) {
          throw BaboonCodecException.EncoderFailure(
              "any: meta-kind 0x" + (value.meta.kind.toInt() and 0xFF).toString(16).padStart(2, '0') +
              " does not match field-declared 0x" + (expectedKind.toInt() and 0xFF).toString(16).padStart(2, '0')
          )
      }
      val anyBlob: ByteArray = when (value) {
          is AnyOpaqueUeba -> value.bytes
          // @baboon:json-start
          is AnyOpaqueJson -> {
              val anyFacade = ctx.facade ?: throw BaboonCodecException.EncoderFailure(
                  "Cannot encode AnyOpaqueJson into UEBA without a facade reference. Pass BaboonCodecContext.withFacade(useIndices, facade) into encode(), or supply AnyOpaqueUeba directly."
              )
              val anyConvResult = anyFacade.jsonToUebaBytes(value.meta, value.json, staticDomain, staticVersion, staticTypeid)
              when (anyConvResult) {
                  is Either.Left -> throw anyConvResult.value
                  is Either.Right -> anyConvResult.value
              }
          }
          // @baboon:json-end
      }
      // Buffer the meta to count its byte length precisely (the on-wire `meta-length` field).
      val anyMetaBuf = ByteArrayOutputStream()
      val anyMetaWriter = LEDataOutputStream(anyMetaBuf)
      AnyMetaCodec.writeBin(value.meta, anyMetaWriter)
      anyMetaWriter.flush()
      val anyMetaBytes = anyMetaBuf.toByteArray()
      val anyTotalLength: Int = 4 + anyMetaBytes.size + anyBlob.size
      writer.writeInt(anyTotalLength)
      writer.writeInt(anyMetaBytes.size)
      writer.write(anyMetaBytes)
      writer.write(anyBlob)
  }

  fun decode(wire: LEDataInputStream, expectedKind: Byte): AnyOpaqueUeba {
      val anyTotalLength = wire.readInt()
      if (anyTotalLength < 0) {
          throw BaboonCodecException.DecoderFailure(
              "any: negative total-length " + anyTotalLength
          )
      }
      val anyMetaLength = wire.readInt()
      if (anyMetaLength < 0) {
          throw BaboonCodecException.DecoderFailure(
              "any: negative meta-length " + anyMetaLength
          )
      }
      if (anyTotalLength < 4 + anyMetaLength) {
          throw BaboonCodecException.DecoderFailure(
              "any: total-length " + anyTotalLength + " smaller than 4 + meta-length " + anyMetaLength
          )
      }
      val anyReadResult = AnyMetaCodec.readBinWithLength(wire)
      val anyMeta = anyReadResult.first
      val anyBytesRead = anyReadResult.second
      if (anyBytesRead > anyMetaLength) {
          throw BaboonCodecException.DecoderFailure(
              "any: meta bytes-read " + anyBytesRead + " exceeded meta-length window " + anyMetaLength
          )
      }
      if (anyBytesRead < anyMetaLength) {
          // Forward-compat: skip future meta-extension bytes within the meta-length window.
          val anySkip = ByteArray(anyMetaLength - anyBytesRead)
          wire.readFully(anySkip)
      }
      if (anyMeta.kind != expectedKind) {
          throw BaboonCodecException.DecoderFailure(
              "any: wire kind 0x" + (anyMeta.kind.toInt() and 0xFF).toString(16).padStart(2, '0') +
              " does not match field-declared 0x" + (expectedKind.toInt() and 0xFF).toString(16).padStart(2, '0')
          )
      }
      val anyBlobLen = anyTotalLength - 4 - anyMetaLength
      val anyBlob = ByteArray(anyBlobLen)
      wire.readFully(anyBlob)
      return AnyOpaqueUeba(anyMeta, anyBlob)
  }
}
