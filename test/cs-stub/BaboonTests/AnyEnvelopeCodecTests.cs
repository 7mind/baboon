#nullable enable
using System.IO;
using Baboon.Runtime.Shared;
using Newtonsoft.Json.Linq;
using NUnit.Framework;

namespace ConversionsTest
{
    [TestFixture]
    public class AnyEnvelopeCodecTests
    {
        [TestCase(0x07, "domain", "1.2.3", "type")]
        [TestCase(0x03, null, "1.2.3", "type")]
        [TestCase(0x01, null, null, "type")]
        [TestCase(0x06, "domain", "1.2.3", null)]
        [TestCase(0x02, null, "1.2.3", null)]
        [TestCase(0x00, null, null, null)]
        public void NativeFormatsRoundTripAllKinds(byte kind, string? domain, string? version, string? type)
        {
            var meta = new AnyMeta(kind, domain, version, type);
            var json = new AnyOpaqueJson(meta, JValue.CreateNull());
            var encoded = BaboonAnyJsonCodec.Encode(BaboonCodecContext.Compact, kind, null, null, null, json);
            var decoded = BaboonAnyJsonCodec.Decode(kind, encoded);
            Assert.That(decoded.Meta, Is.EqualTo(meta));
            Assert.That(decoded.Json.Type, Is.EqualTo(JTokenType.Null));

            var bytes = new byte[] { 0xaa, 0xbb };
            using var output = new MemoryStream();
            using var writer = new BinaryWriter(output);
            BaboonAnyBinCodec.Encode(BaboonCodecContext.Compact, writer, kind, null, null, null, new AnyOpaqueUeba(meta, bytes));
            writer.Write((byte)0x42);
            using var reader = new BinaryReader(new MemoryStream(output.ToArray()));
            var binary = BaboonAnyBinCodec.Decode(reader, kind);
            Assert.That(binary.Meta, Is.EqualTo(meta));
            Assert.That(binary.Bytes, Is.EqualTo(bytes));
            Assert.That(reader.ReadByte(), Is.EqualTo(0x42));
            if (kind == 0)
                Assert.That(output.ToArray(), Is.EqualTo(new byte[] { 7, 0, 0, 0, 1, 0, 0, 0, 0, 0xaa, 0xbb, 0x42 }));
        }

        [Test]
        public void BinaryDecodeSkipsMetadataExtensionsWithoutConsumingTrailer()
        {
            using var reader = new BinaryReader(new MemoryStream(new byte[] { 9, 0, 0, 0, 3, 0, 0, 0, 0, 0x11, 0x22, 0xaa, 0xbb, 0x42 }));
            var value = BaboonAnyBinCodec.Decode(reader, 0);
            Assert.That(value.Bytes, Is.EqualTo(new byte[] { 0xaa, 0xbb }));
            Assert.That(reader.ReadByte(), Is.EqualTo(0x42));
        }

        [Test]
        public void KindValidationPrecedesCrossFormatConversion()
        {
            var meta = new AnyMeta(0, null, null, null);
            var binary = new AnyOpaqueUeba(meta, new byte[] { 1 });
            var mismatch = Assert.Throws<BaboonCodecException.EncoderFailure>(() =>
                BaboonAnyJsonCodec.Encode(BaboonCodecContext.Compact, 1, null, null, null, binary));
            Assert.That(mismatch!.Message, Does.Contain("does not match field-declared"));
            var missingFacade = Assert.Throws<BaboonCodecException.EncoderFailure>(() =>
                BaboonAnyJsonCodec.Encode(BaboonCodecContext.Compact, 0, null, null, null, binary));
            Assert.That(missingFacade!.Message, Does.Contain("without a facade reference"));
            using var writer = new BinaryWriter(new MemoryStream());
            Assert.Throws<BaboonCodecException.EncoderFailure>(() =>
                BaboonAnyBinCodec.Encode(BaboonCodecContext.Compact, writer, 0, null, null, null, new AnyOpaqueJson(meta, new JValue(1))));
        }

        [Test]
        public void RejectsMissingJsonContentAndShortBinaryPayload()
        {
            Assert.Throws<BaboonCodecException.DecoderFailure>(() => BaboonAnyJsonCodec.Decode(0, null));
            Assert.Throws<BaboonCodecException.DecoderFailure>(() => BaboonAnyJsonCodec.Decode(0, JObject.Parse("{\"$ak\":0}")));
            using var reader = new BinaryReader(new MemoryStream(new byte[] { 7, 0, 0, 0, 1, 0, 0, 0, 0, 0xaa }));
            var failure = Assert.Throws<BaboonCodecException.DecoderFailure>(() => BaboonAnyBinCodec.Decode(reader, 0));
            Assert.That(failure!.Message, Does.Contain("short read on blob"));
        }
    }
}
