package baboon.runtime.shared

import kotlinx.serialization.json.JsonElement
import kotlinx.serialization.json.JsonObject

object BaboonAnyJsonCodec {
  private val anyEnvelopeContentKey: String = "${'$'}c"

  fun encode(
      ctx: BaboonCodecContext,
      expectedKind: Byte,
      staticDomain: String?,
      staticVersion: String?,
      staticTypeid: String?,
      value: AnyOpaque,
  ): JsonElement {
      if (value.meta.kind != expectedKind) {
          throw BaboonCodecException.EncoderFailure(
              "any: meta-kind 0x" + (value.meta.kind.toInt() and 0xFF).toString(16).padStart(2, '0') +
              " does not match field-declared 0x" + (expectedKind.toInt() and 0xFF).toString(16).padStart(2, '0')
          )
      }
      val anyInner: JsonElement = when (value) {
          is AnyOpaqueJson -> value.json
          is AnyOpaqueUeba -> {
              val anyFacade = ctx.facade ?: throw BaboonCodecException.EncoderFailure(
                  "Cannot encode AnyOpaqueUeba into JSON without a facade reference. Pass BaboonCodecContext.withFacade(useIndices, facade) into encode(), or supply AnyOpaqueJson directly."
              )
              val anyConvResult = anyFacade.uebaToJson(value.meta, value.bytes, staticDomain, staticVersion, staticTypeid)
              when (anyConvResult) {
                  is Either.Left -> throw anyConvResult.value
                  is Either.Right -> anyConvResult.value
              }
          }
      }
      val anyMetaJson = AnyMetaCodec.writeJson(value.meta) as JsonObject
      return JsonObject(anyMetaJson.toMutableMap().apply { put(anyEnvelopeContentKey, anyInner) })
  }

  fun decode(expectedKind: Byte, wire: JsonElement): AnyOpaqueJson {
      val anyMetaResult = AnyMetaCodec.readJson(wire)
      val anyMeta = when (anyMetaResult) {
          is Either.Left -> throw anyMetaResult.value
          is Either.Right -> anyMetaResult.value
      }
      if (anyMeta.kind != expectedKind) {
          throw BaboonCodecException.DecoderFailure(
              "any: wire kind 0x" + (anyMeta.kind.toInt() and 0xFF).toString(16).padStart(2, '0') +
              " does not match field-declared 0x" + (expectedKind.toInt() and 0xFF).toString(16).padStart(2, '0')
          )
      }
      val anyContent = (wire as? JsonObject)?.get(anyEnvelopeContentKey)
          ?: throw BaboonCodecException.DecoderFailure(
              "any: JSON envelope missing '" + anyEnvelopeContentKey + "' content key"
          )
      return AnyOpaqueJson(anyMeta, anyContent)
  }
}
