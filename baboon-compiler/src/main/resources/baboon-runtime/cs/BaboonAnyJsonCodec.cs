#nullable enable

using System.IO;
using Newtonsoft.Json.Linq;

namespace Baboon.Runtime.Shared
{
    public static class BaboonAnyJsonCodec
    {
        private const string AnyEnvelopeContentKey = "$c";

        public static JToken Encode(
            BaboonCodecContext ctx,
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
            JToken anyInner;
            if (value is AnyOpaqueJson anyJson)
            {
                anyInner = anyJson.Json;
            }
            else if (value is AnyOpaqueUeba anyUeba)
            {
                var f = ctx.Facade ?? throw new BaboonCodecException.EncoderFailure(
                    "Cannot encode AnyOpaqueUeba into JSON without a facade reference. Pass BaboonCodecContext.WithFacade(useIndices, facade) into Encode(), or supply AnyOpaqueJson directly.");
                var anyConvResult = f.UebaToJson(ctx, anyUeba.Meta, anyUeba.Bytes, staticDomain, staticVersion, staticTypeid);
                if (anyConvResult is Either<BaboonCodecException, JToken>.Left anyConvL)
                {
                    throw anyConvL.Value;
                }
                anyInner = ((Either<BaboonCodecException, JToken>.Right)anyConvResult).Value;
            }
            else
            {
                throw new BaboonCodecException.EncoderFailure(
                    $"unexpected AnyOpaque subclass: {value.GetType()}");
            }
            var anyEnvelope = (JObject)AnyMetaCodec.WriteJson(value.Meta);
            anyEnvelope.Add(AnyEnvelopeContentKey, anyInner);
            return anyEnvelope;
        }

        public static AnyOpaqueJson Decode(byte expectedKind, JToken? wire)
        {
            if (wire is null)
            {
                throw new BaboonCodecException.DecoderFailure(
                    "any: missing JSON envelope (null token)");
            }
            var anyMetaResult = AnyMetaCodec.ReadJson(wire);
            if (anyMetaResult is Either<BaboonCodecException, AnyMeta>.Left anyMetaL)
            {
                throw anyMetaL.Value;
            }
            var anyMeta = ((Either<BaboonCodecException, AnyMeta>.Right)anyMetaResult).Value;
            if (anyMeta.Kind != expectedKind)
            {
                throw new BaboonCodecException.DecoderFailure(
                    $"any: wire kind 0x{anyMeta.Kind & 0xFF:x2} does not match field-declared 0x{expectedKind & 0xFF:x2}");
            }
            var anyContent = wire[AnyEnvelopeContentKey];
            if (anyContent is null)
            {
                throw new BaboonCodecException.DecoderFailure(
                    $"any: JSON envelope missing '{AnyEnvelopeContentKey}' content key");
            }
            return new AnyOpaqueJson(anyMeta, anyContent);
        }
    }
}
