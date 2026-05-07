// MFACADE-PR-6 stage B (C# pilot): the codegen emits a `Domain<PascalDomainId>Facade`
// per domain that subclasses BaboonCodecsFacade and Register()s every known version of
// the domain in its parameterless ctor. This test exercises the auto-registration end-to-end
// for the my.ok domain by encoding/decoding an Inner via the auto-registered facade.
//
// Generated symbols (My.Ok.DomainMyOkFacade, My.Ok.Inner, My.Ok.Inner_UEBACodec, ...) are
// produced by `mdl :build :test-gen-regular-adt`. Running `dotnet test` directly from the
// source tree may fail with missing symbols.
#nullable enable

using Baboon.Runtime.Shared;
using NUnit.Framework;

namespace ConversionsTest
{
    [TestFixture]
    public class DomainFacadeTests
    {
        [Test]
        public void ParameterlessCtor_AutoRegistersAllVersions_RoundTripsInner()
        {
            // No manual Register(...) calls — the generated ctor wires every version of my.ok.
            var facade = new My.Ok.DomainMyOkFacade();

            var sample = new My.Ok.Inner(42);
            var encoded = facade.EncodeToBin(BaboonCodecContext.Compact, sample);
            Assert.That(encoded, Is.InstanceOf<Either<BaboonCodecException, byte[]>.Right>(),
                "EncodeToBin via auto-registered facade must succeed.");
            var bytes = ((Either<BaboonCodecException, byte[]>.Right)encoded).Value;

            var decoded = facade.DecodeFromBin(bytes);
            Assert.That(decoded, Is.InstanceOf<Either<BaboonCodecException, IBaboonGenerated>.Right>(),
                "DecodeFromBin via auto-registered facade must succeed.");
            var value = ((Either<BaboonCodecException, IBaboonGenerated>.Right)decoded).Value;

            Assert.That(value, Is.InstanceOf<My.Ok.Inner>());
            Assert.That((My.Ok.Inner)value, Is.EqualTo(sample));
        }
    }
}
