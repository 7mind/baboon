// NOTE: This test references generated runtime symbols (BaboonCodecsFacade, BaboonCodecException, ...)
// AND generated DTO/codec symbols (My.Ok.Inner, ...) which are copied/generated into this stub
// only by `mdl :build :test-gen-regular-adt`. Running `dotnet test` directly from the source
// tree may fail with missing symbols; run the test suite from the codegen'd copy.
#nullable enable

using Baboon.Runtime.Shared;
using NUnit.Framework;

namespace ConversionsTest
{
    [TestFixture]
    public class FacadeLatestMethodsTests
    {
        private static BaboonCodecsFacade FreshFacade()
        {
            var f = new BaboonCodecsFacade();
            var dv = new BaboonDomainVersion(My.Ok.Inner.BaboonDomainIdentifierValue, My.Ok.Inner.BaboonDomainVersionValue);
            f.Register(
                dv,
                () => My.Ok.BaboonCodecsJson.Instance,
                () => My.Ok.BaboonCodecsUeba.Instance,
                () => My.Ok.BaboonMeta.Instance);
            return f;
        }

        private static readonly My.Ok.Inner SampleInner = new(42);

        [Test]
        public void Preload_DoesNotThrow_OnRegisteredFacade()
        {
            var f = FreshFacade();
            // Preload is fire-and-forget; the call itself must return without throwing.
            Assert.DoesNotThrow(() => f.Preload());
        }

        [Test]
        public void DecodeFromBinLatest_ByteArray_ReturnsRightWithOriginalValue()
        {
            var f = FreshFacade();
            var ctx = BaboonCodecContext.Compact;
            var encodeResult = f.EncodeToBin(ctx, SampleInner);
            Assert.That(encodeResult, Is.InstanceOf<Either<BaboonCodecException, byte[]>.Right>(),
                $"EncodeToBin must succeed; got {encodeResult}");
            var bytes = ((Either<BaboonCodecException, byte[]>.Right)encodeResult).Value;

            var decodeResult = f.DecodeFromBinLatest<My.Ok.Inner>(bytes);
            Assert.That(decodeResult, Is.InstanceOf<Either<BaboonCodecException, My.Ok.Inner>.Right>(),
                $"DecodeFromBinLatest must return Right; got {decodeResult}");
            var decoded = ((Either<BaboonCodecException, My.Ok.Inner>.Right)decodeResult).Value;
            Assert.That(decoded, Is.Not.Null);
            Assert.That(decoded, Is.EqualTo(SampleInner));
        }

        [Test]
        public void DecodeFromJsonLatest_JToken_ReturnsRightWithOriginalValue()
        {
            var f = FreshFacade();
            var encodeResult = f.EncodeToJson(SampleInner);
            Assert.That(encodeResult, Is.InstanceOf<Either<BaboonCodecException, Newtonsoft.Json.Linq.JToken>.Right>(),
                $"EncodeToJson must succeed; got {encodeResult}");
            var json = ((Either<BaboonCodecException, Newtonsoft.Json.Linq.JToken>.Right)encodeResult).Value;

            var decodeResult = f.DecodeFromJsonLatest<My.Ok.Inner>(json);
            Assert.That(decodeResult, Is.InstanceOf<Either<BaboonCodecException, My.Ok.Inner?>.Right>(),
                $"DecodeFromJsonLatest must return Right; got {decodeResult}");
            var decoded = ((Either<BaboonCodecException, My.Ok.Inner?>.Right)decodeResult).Value;
            Assert.That(decoded, Is.Not.Null);
            Assert.That(decoded, Is.EqualTo(SampleInner));
        }

    }
}
