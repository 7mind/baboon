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

        // MFACADE-PR-7 conformance pin: canonical byte sequence for `Inner(x=42)` of
        // domain `my.ok` v1.0.0 in compact UEBA mode through the auto-registered facade.
        // Per docs/spec/codec-envelope.md the envelope layout is normative and this
        // tripwire catches silent drift in the C# reference implementation. Other
        // backends are pinned for cross-backend agreement by the acceptance harness
        // (`mdl :test-acceptance` — encode in backend A, decode in backend B, assert
        // round-trip equality), so any backend that stops producing these exact bytes
        // would also fail the harness against unmodified peers.
        //
        // Layout, byte by byte:
        //   01                                           META_VERSION_1
        //   05 6D 79 2E 6F 6B                            VLQ-len + "my.ok"
        //   05 31 2E 30 2E 30                            VLQ-len + "1.0.0"
        //   00                                           hasMinCompat = 0 (version == minCompat)
        //   0D 6D 79 2E 6F 6B 2F 3A 23 49 6E 6E 65 72    VLQ-len + "my.ok/:#Inner"
        //   00                                           Inner UEBA mode-byte (0 = compact)
        //   2A 00 00 00                                  i32 little-endian = 42
        [Test]
        public void EncodeToBin_Inner42_Compact_ProducesCanonicalBytes()
        {
            var facade = new My.Ok.DomainMyOkFacade();
            var sample = new My.Ok.Inner(42);

            var encoded = facade.EncodeToBin(BaboonCodecContext.Compact, sample);
            Assert.That(encoded, Is.InstanceOf<Either<BaboonCodecException, byte[]>.Right>());
            var bytes = ((Either<BaboonCodecException, byte[]>.Right)encoded).Value;

            byte[] expected = new byte[] {
                0x01,
                0x05, 0x6D, 0x79, 0x2E, 0x6F, 0x6B,
                0x05, 0x31, 0x2E, 0x30, 0x2E, 0x30,
                0x00,
                0x0D, 0x6D, 0x79, 0x2E, 0x6F, 0x6B, 0x2F, 0x3A, 0x23, 0x49, 0x6E, 0x6E, 0x65, 0x72,
                0x00,
                0x2A, 0x00, 0x00, 0x00,
            };
            Assert.That(bytes, Is.EqualTo(expected),
                "Canonical bytes for Inner(x=42) v1.0.0 compact UEBA must match the spec layout.");
        }
    }
}
