#nullable enable

using System.IO;
using Newtonsoft.Json.Linq;

namespace Baboon.Runtime.Shared
{
    public static class BaboonAnyBinCodec
    {
        public static void Encode(
            BaboonCodecContext ctx,
            BinaryWriter writer,
            byte expectedKind,
            string? staticDomain,
            string? staticVersion,
            string? staticTypeid,
            AnyOpaque value)
        {
            if (value.Meta.Kind != expectedKind)
            {
                throw new BaboonCodecException.EncoderFailure(
                    $"any: meta-kind 0x{value.Meta.Kind & 0xFF:x2} does not match field-declared 0x{expectedKind & 0xFF:x2}");
            }
            byte[] anyBlob;
            if (value is AnyOpaqueUeba anyUeba)
            {
                anyBlob = anyUeba.Bytes;
            }
            else if (value is AnyOpaqueJson anyJson)
            {
                var f = ctx.Facade ?? throw new BaboonCodecException.EncoderFailure(
                    "Cannot encode AnyOpaqueJson into UEBA without a facade reference. Pass BaboonCodecContext.WithFacade(useIndices, facade) into Encode(), or supply AnyOpaqueUeba directly.");
                var anyConvResult = f.JsonToUebaBytes(ctx, anyJson.Meta, anyJson.Json, staticDomain, staticVersion, staticTypeid);
                if (anyConvResult is Either<BaboonCodecException, byte[]>.Left anyConvL)
                {
                    throw anyConvL.Value;
                }
                anyBlob = ((Either<BaboonCodecException, byte[]>.Right)anyConvResult).Value;
            }
            else
            {
                throw new BaboonCodecException.EncoderFailure(
                    $"unexpected AnyOpaque subclass: {value.GetType()}");
            }
            // Buffer the meta to count its byte length precisely (the on-wire `meta-length` field).
            using var anyMetaBuf = new MemoryStream();
            using (var anyMetaWriter = new BinaryWriter(anyMetaBuf))
            {
                AnyMetaCodec.WriteBin(value.Meta, anyMetaWriter);
                anyMetaWriter.Flush();
            }
            var anyMetaBytes = anyMetaBuf.ToArray();
            var anyTotalLength = 4 + anyMetaBytes.Length + anyBlob.Length;
            writer.Write(anyTotalLength);
            writer.Write(anyMetaBytes.Length);
            writer.Write(anyMetaBytes);
            writer.Write(anyBlob);
        }

        public static AnyOpaqueUeba Decode(BinaryReader wire, byte expectedKind)
        {
            var anyTotalLength = wire.ReadInt32();
            var anyMetaLength = wire.ReadInt32();
            if (anyMetaLength < 0)
            {
                throw new BaboonCodecException.DecoderFailure(
                    $"any: invalid meta-length {anyMetaLength}");
            }
            if (anyTotalLength < 4 + anyMetaLength)
            {
                throw new BaboonCodecException.DecoderFailure(
                    $"any: total-length {anyTotalLength} smaller than 4 + meta-length {anyMetaLength}");
            }
            var (anyMeta, anyBytesRead) = AnyMetaCodec.ReadBinWithLength(wire);
            if (anyBytesRead > anyMetaLength)
            {
                throw new BaboonCodecException.DecoderFailure(
                    $"any: meta-bytes-read {anyBytesRead} exceeded meta-length window {anyMetaLength}");
            }
            if (anyBytesRead < anyMetaLength)
            {
                // Skip future meta extension bytes within the meta-length window (forward-compat).
                var anySkip = anyMetaLength - anyBytesRead;
                wire.ReadBytes(anySkip);
            }
            if (anyMeta.Kind != expectedKind)
            {
                throw new BaboonCodecException.DecoderFailure(
                    $"any: wire kind 0x{anyMeta.Kind & 0xFF:x2} does not match field-declared 0x{expectedKind & 0xFF:x2}");
            }
            var anyBlobLen = anyTotalLength - 4 - anyMetaLength;
            var anyBlob = wire.ReadBytes(anyBlobLen);
            if (anyBlob.Length != anyBlobLen)
            {
                throw new BaboonCodecException.DecoderFailure(
                    $"any: short read on blob, expected {anyBlobLen} got {anyBlob.Length}");
            }
            return new AnyOpaqueUeba(anyMeta, anyBlob);
        }
    }
}
