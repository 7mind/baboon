// Cross-language golden bytes for the top-level binary envelope (docs/spec/codec-envelope.md §2.1, §2.1.2, §2.1.3; docs/forward-compat.md, "Worked examples"). The same values must produce these exact bytes in every backend; the Scala and TypeScript stubs assert the same sequences structurally.
using System;
using System.Linq;
using Baboon.Runtime.Shared;
using Fwde2e.Chain;
using Fwde2e.Fwd;
using NUnit.Framework;

namespace ConversionsTest
{
    [TestFixture]
    public class BinEnvelopeGoldenTests
    {
        private static string Hx(byte[] b) => string.Join(" ", b.Select(x => x.ToString("X2")));
        private static byte[] Enc(BaboonCodecsFacade f, BaboonCodecContext ctx, IBaboonGenerated v) => f.EncodeToBin(ctx, v).GetRight();

        private static readonly BaboonCodecContext V1Tolerant = BaboonCodecContext.Custom(false, ForwardWritePolicy.Tolerant, BaboonEnvelopeVersion.V1, null);
        private static readonly BaboonCodecContext V2Compact = BaboonCodecContext.Custom(false, ForwardWritePolicy.Strict, BaboonEnvelopeVersion.V2, null);
        private static readonly BaboonCodecContext V2Indexed = BaboonCodecContext.Custom(true, ForwardWritePolicy.Strict, BaboonEnvelopeVersion.V2, null);

        [Test]
        public void DefaultContextsWriteV1Strict()
        {
            Assert.That(BaboonCodecContext.Compact.EnvelopeVersion, Is.EqualTo(BaboonEnvelopeVersion.V1));
            Assert.That(BaboonCodecContext.Compact.ForwardWritePolicy, Is.EqualTo(ForwardWritePolicy.Strict));
            Assert.That(BaboonCodecContext.Indexed.EnvelopeVersion, Is.EqualTo(BaboonEnvelopeVersion.V1));
        }

        [Test]
        public void EnvelopesMatchTheCrossLanguageGoldenBytes()
        {
            var fwd = new DomainFwde2eFwdFacade();
            var chain = new DomainFwde2eChainFacade();
            var app = new FwdAppendVar(42, "hi", "t");
            // FwdAppendVar, v1 Strict (default) compact: identical bound elided
            Assert.That(Hx(Enc(fwd, BaboonCodecContext.Compact, app)), Is.EqualTo("01 0A 66 77 64 65 32 65 2E 66 77 64 05 32 2E 30 2E 30 00 19 66 77 64 65 32 65 2E 66 77 64 2F 3A 23 46 77 64 41 70 70 65 6E 64 56 61 72 00 2A 00 00 00 02 68 69 01 01 74"));
            // FwdAppendVar, v1 Tolerant compact: prefix-compact bound 1.0.0 in the single slot
            Assert.That(Hx(Enc(fwd, V1Tolerant, app)), Is.EqualTo("01 0A 66 77 64 65 32 65 2E 66 77 64 05 32 2E 30 2E 30 01 05 31 2E 30 2E 30 19 66 77 64 65 32 65 2E 66 77 64 2F 3A 23 46 77 64 41 70 70 65 6E 64 56 61 72 00 2A 00 00 00 02 68 69 01 01 74"));
            // FwdAppendVar, v2 compact: flags 0b10, readableMin 1.0.0
            Assert.That(Hx(Enc(fwd, V2Compact, app)), Is.EqualTo("02 0A 66 77 64 65 32 65 2E 66 77 64 05 32 2E 30 2E 30 02 05 31 2E 30 2E 30 19 66 77 64 65 32 65 2E 66 77 64 2F 3A 23 46 77 64 41 70 70 65 6E 64 56 61 72 00 2A 00 00 00 02 68 69 01 01 74"));
            // FwdAppendVar, v2 indexed: flags 0 (prefix-any-mode bound is 2.0.0)
            Assert.That(Hx(Enc(fwd, V2Indexed, app)), Is.EqualTo("02 0A 66 77 64 65 32 65 2E 66 77 64 05 32 2E 30 2E 30 00 19 66 77 64 65 32 65 2E 66 77 64 2F 3A 23 46 77 64 41 70 70 65 6E 64 56 61 72 01 04 00 00 00 03 00 00 00 07 00 00 00 03 00 00 00 2A 00 00 00 02 68 69 01 01 74"));
            // FwdStable, v1 Strict compact: byte-identical since 1.0.0 -> hasMinCompat 1
            Assert.That(Hx(Enc(fwd, BaboonCodecContext.Compact, new FwdStable("s"))), Is.EqualTo("01 0A 66 77 64 65 32 65 2E 66 77 64 05 32 2E 30 2E 30 01 05 31 2E 30 2E 30 16 66 77 64 65 32 65 2E 66 77 64 2F 3A 23 46 77 64 53 74 61 62 6C 65 00 01 73"));
            // FwdStable, v2 compact: flags 0b01, minCompat 1.0.0, readableMin elided
            Assert.That(Hx(Enc(fwd, V2Compact, new FwdStable("s"))), Is.EqualTo("02 0A 66 77 64 65 32 65 2E 66 77 64 05 32 2E 30 2E 30 01 05 31 2E 30 2E 30 16 66 77 64 65 32 65 2E 66 77 64 2F 3A 23 46 77 64 53 74 61 62 6C 65 00 01 73"));
            // FwdEnumHost, v2 compact: flags 0, no bound
            Assert.That(Hx(Enc(fwd, V2Compact, new FwdEnumHost(FwdEnumGrows.C))), Is.EqualTo("02 0A 66 77 64 65 32 65 2E 66 77 64 05 32 2E 30 2E 30 00 18 66 77 64 65 32 65 2E 66 77 64 2F 3A 23 46 77 64 45 6E 75 6D 48 6F 73 74 00 02"));
            // ChainAppend 3.0.0, v2 compact: flags 0b10, readableMin 1.0.0
            Assert.That(Hx(Enc(chain, V2Compact, new ChainAppend(1, "b", "c"))), Is.EqualTo("02 0C 66 77 64 65 32 65 2E 63 68 61 69 6E 05 33 2E 30 2E 30 02 05 31 2E 30 2E 30 1A 66 77 64 65 32 65 2E 63 68 61 69 6E 2F 3A 23 43 68 61 69 6E 41 70 70 65 6E 64 00 01 00 00 00 01 01 62 01 01 63"));
        }

        [Test]
        public void V2EnvelopeRoundTripsThroughItsOwnFacade()
        {
            var fwd = new DomainFwde2eFwdFacade();
            var app = new FwdAppendVar(42, "hi", "t");
            Assert.That(fwd.DecodeFromBin(Enc(fwd, V2Compact, app)).GetRight(), Is.EqualTo(app));
        }
    }
}
