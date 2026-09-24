package baboon.runtime.shared;

import baboon.runtime.shared.BaboonAnyOpaque.*;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import java.io.ByteArrayOutputStream;

public final class BaboonAnyBinCodec {
    private BaboonAnyBinCodec() {}

    public static void encode(
        BaboonCodecContext ctx,
        LEDataOutputStream writer,
        byte expectedKind,
        String staticDomain,
        String staticVersion,
        String staticTypeid,
        AnyOpaque value
    ) throws Exception {
      if (value.meta().kind() != expectedKind) {
        throw new BaboonCodecException.EncoderFailure(
            "any: meta-kind 0x" + String.format("%02x", value.meta().kind() & 0xFF) +
            " does not match field-declared 0x" + String.format("%02x", expectedKind & 0xFF));
      }
      byte[] anyBlob;
      if (value instanceof AnyOpaqueUeba anyUeba) {
        anyBlob = anyUeba.bytes();
      } else if (value instanceof AnyOpaqueJson anyJson) {
        var anyFacade = ctx.facade();
        if (anyFacade == null) {
          throw new BaboonCodecException.EncoderFailure(
              "Cannot encode AnyOpaqueJson into UEBA without a facade reference. Pass BaboonCodecContext.withFacade(useIndices, facade) into encode(), or supply AnyOpaqueUeba directly.");
        }
        var anyConvResult = anyFacade.jsonToUebaBytes(ctx, anyJson.meta(), anyJson.json(), staticDomain, staticVersion, staticTypeid);
        if (anyConvResult instanceof BaboonEither.Left<BaboonCodecException, byte[]> anyConvL) {
          throw anyConvL.value();
        }
        anyBlob = ((BaboonEither.Right<BaboonCodecException, byte[]>) anyConvResult).value();
      } else {
        throw new BaboonCodecException.EncoderFailure(
            "unexpected AnyOpaque subclass: " + value.getClass().getName());
      }
      // Buffer the meta to count its byte length precisely (the on-wire `meta-length` field).
      var anyMetaBuf = new ByteArrayOutputStream();
      var anyMetaWriter = new LEDataOutputStream(anyMetaBuf);
      AnyMetaCodec.writeBin(value.meta(), anyMetaWriter);
      anyMetaWriter.flush();
      byte[] anyMetaBytes = anyMetaBuf.toByteArray();
      int anyTotalLength = 4 + anyMetaBytes.length + anyBlob.length;
      writer.writeInt(anyTotalLength);
      writer.writeInt(anyMetaBytes.length);
      writer.write(anyMetaBytes);
      writer.write(anyBlob);
    }

    public static AnyOpaqueUeba decode(LEDataInputStream wire, byte expectedKind) throws Exception {
      int anyTotalLength = wire.readInt();
      if (anyTotalLength < 0) {
        throw new BaboonCodecException.DecoderFailure(
            "any: negative total-length " + anyTotalLength);
      }
      int anyMetaLength = wire.readInt();
      if (anyMetaLength < 0) {
        throw new BaboonCodecException.DecoderFailure(
            "any: negative meta-length " + anyMetaLength);
      }
      if (anyTotalLength < 4 + anyMetaLength) {
        throw new BaboonCodecException.DecoderFailure(
            "any: total-length " + anyTotalLength + " smaller than 4 + meta-length " + anyMetaLength);
      }
      var anyReadResult = AnyMetaCodec.readBinWithLength(wire);
      var anyMeta = anyReadResult.meta();
      int anyBytesRead = anyReadResult.bytesRead();
      if (anyBytesRead > anyMetaLength) {
        throw new BaboonCodecException.DecoderFailure(
            "any: meta bytes-read " + anyBytesRead + " exceeded meta-length window " + anyMetaLength);
      }
      if (anyBytesRead < anyMetaLength) {
        // Forward-compat: skip future meta-extension bytes within the meta-length window.
        int anySkip = anyMetaLength - anyBytesRead;
        byte[] anySkipBuf = wire.readNBytes(anySkip);
        if (anySkipBuf.length != anySkip) {
          throw new BaboonCodecException.DecoderFailure(
              "any: short read while skipping meta-extension bytes, expected " + anySkip + " got " + anySkipBuf.length);
        }
      }
      if (anyMeta.kind() != expectedKind) {
        throw new BaboonCodecException.DecoderFailure(
            "any: wire kind 0x" + String.format("%02x", anyMeta.kind() & 0xFF) +
            " does not match field-declared 0x" + String.format("%02x", expectedKind & 0xFF));
      }
      int anyBlobLen = anyTotalLength - 4 - anyMetaLength;
      byte[] anyBlob = wire.readNBytes(anyBlobLen);
      if (anyBlob.length != anyBlobLen) {
        throw new BaboonCodecException.DecoderFailure(
            "any: short read on blob, expected " + anyBlobLen + " got " + anyBlob.length);
      }
      return new AnyOpaqueUeba(anyMeta, anyBlob);
    }
}
