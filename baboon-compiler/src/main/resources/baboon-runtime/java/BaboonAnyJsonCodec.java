package baboon.runtime.shared;

import baboon.runtime.shared.BaboonAnyOpaque.*;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import java.io.ByteArrayOutputStream;

public final class BaboonAnyJsonCodec {
    private BaboonAnyJsonCodec() {}

    public static JsonNode encode(
        BaboonCodecContext ctx,
        byte expectedKind,
        String staticDomain,
        String staticVersion,
        String staticTypeid,
        AnyOpaque value
    ) {
      if (value.meta().kind() != expectedKind) {
        throw new BaboonCodecException.EncoderFailure(
            "any: meta-kind 0x" + String.format("%02x", value.meta().kind() & 0xFF) +
            " does not match field-declared 0x" + String.format("%02x", expectedKind & 0xFF));
      }
      JsonNode anyInner;
      if (value instanceof AnyOpaqueJson anyJson) {
        anyInner = anyJson.json();
      } else if (value instanceof AnyOpaqueUeba anyUeba) {
        var anyFacade = ctx.facade();
        if (anyFacade == null) {
          throw new BaboonCodecException.EncoderFailure(
              "Cannot encode AnyOpaqueUeba into JSON without a facade reference. Pass BaboonCodecContext.withFacade(useIndices, facade) into encode(), or supply AnyOpaqueJson directly.");
        }
        var anyConvResult = anyFacade.uebaToJson(anyUeba.meta(), anyUeba.bytes(), staticDomain, staticVersion, staticTypeid);
        if (anyConvResult instanceof BaboonEither.Left<BaboonCodecException, JsonNode> anyConvL) {
          throw anyConvL.value();
        }
        anyInner = ((BaboonEither.Right<BaboonCodecException, JsonNode>) anyConvResult).value();
      } else {
        throw new BaboonCodecException.EncoderFailure(
            "unexpected AnyOpaque subclass: " + value.getClass().getName());
      }
      var anyEnvelope = (ObjectNode) AnyMetaCodec.writeJson(value.meta());
      anyEnvelope.set(AnyMetaCodec.ANY_CONTENT_KEY, anyInner);
      return anyEnvelope;
    }

    public static AnyOpaqueJson decode(byte expectedKind, JsonNode wire) {
      if (wire == null) {
        throw new BaboonCodecException.DecoderFailure(
            "any: missing JSON envelope (null token)");
      }
      var anyMetaResult = AnyMetaCodec.readJson(wire);
      if (anyMetaResult instanceof BaboonEither.Left<BaboonCodecException, AnyMeta> anyMetaL) {
        throw anyMetaL.value();
      }
      var anyMeta = ((BaboonEither.Right<BaboonCodecException, AnyMeta>) anyMetaResult).value();
      if (anyMeta.kind() != expectedKind) {
        throw new BaboonCodecException.DecoderFailure(
            "any: wire kind 0x" + String.format("%02x", anyMeta.kind() & 0xFF) +
            " does not match field-declared 0x" + String.format("%02x", expectedKind & 0xFF));
      }
      var anyContent = wire.get(AnyMetaCodec.ANY_CONTENT_KEY);
      if (anyContent == null) {
        throw new BaboonCodecException.DecoderFailure(
            "any: JSON envelope missing '" + AnyMetaCodec.ANY_CONTENT_KEY + "' content key");
      }
      return new AnyOpaqueJson(anyMeta, anyContent);
    }
}
