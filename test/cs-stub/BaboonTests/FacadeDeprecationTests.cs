// `BaboonCodecsFacade.IsDeprecated` answers "does a chain of registered conversions lead
// from this stored value's type to the latest version" — it is the predicate that drives
// the `Deprecated` arm of `ConvertClassified`. It must walk the chain, i.e. follow the type
// each step produces, not keep looking for conversions out of the original type.
//
// Generated symbols (Fwde2e.Chain.*) are produced by `mdl :build :test-gen-regular-adt`;
// running `dotnet test` straight from the source tree may fail with missing symbols.
//
// Two levels of coverage:
//
//  * `fwde2e.chain` (1.0.0 -> 2.0.0 -> 3.0.0, a conversion registered at every step) is
//    real generated code, so the walk is exercised against a registry the compiler built.
//
//  * The synthetic registries below reproduce the two remaining registry shapes the
//    compiler emits, which no shared fixture combines with auto-derivable conversions:
//    a conversion that reaches the latest CLASS several versions early (C# deduplicates
//    types that are identical across versions, so no conversion is registered for the
//    versions in between — e.g. `Convert__T1_D2__From__1_0_0` in `testpkg.pkg0` is
//    registered under 2.0.0 yet targets `Testpkg.Pkg0.T1_D2`, the 3.0.0 class), and a
//    type that is simply gone from the latest version. Their converters throw from
//    `DoConvert`, which pins the "without executing converters" half of the contract.
#nullable enable

// Old-version generated types carry [Obsolete] — naming them is the point of these tests.
#pragma warning disable CS0618

using System;
using System.Collections.Generic;
using Baboon.Runtime.Shared;
using NUnit.Framework;

namespace ConversionsTest
{
    [TestFixture]
    public class FacadeDeprecationTests
    {
        private sealed class NoRequiredConversions
            : Fwde2e.Chain.RequiredConversions, Fwde2e.Chain.v2_0_0.RequiredConversions
        {
        }

        private static BaboonCodecsFacade ChainFacade()
        {
            var facade = new Fwde2e.Chain.DomainFwde2eChainFacade();
            var required = new NoRequiredConversions();
            facade.RegisterConversions(
                new BaboonDomainVersion("fwde2e.chain", "2.0.0"),
                () => new Fwde2e.Chain.v2_0_0.BaboonConversions(required));
            facade.RegisterConversions(
                new BaboonDomainVersion("fwde2e.chain", "3.0.0"),
                () => new Fwde2e.Chain.BaboonConversions(required));
            return facade;
        }

        [Test]
        public void IsDeprecated_False_WhenEveryStepHasAConversion()
        {
            var facade = ChainFacade();
            var value = new Fwde2e.Chain.v1_0_0.ChainAppend(42);

            var converted = facade.Convert<Fwde2e.Chain.v1_0_0.ChainAppend, Fwde2e.Chain.ChainAppend>(value);
            Assert.That(converted, Is.InstanceOf<Either<BaboonCodecException, Fwde2e.Chain.ChainAppend>.Right>(),
                $"Convert must succeed for a 1.0.0 ChainAppend; got {converted}");

            Assert.That(facade.IsDeprecated(value), Is.False,
                "ChainAppend has a registered conversion at every version step, so a path to latest exists.");
        }

        [Test]
        public void IsDeprecated_False_ForLatestValue()
        {
            var facade = ChainFacade();
            var value = new Fwde2e.Chain.ChainAppend(42, null, null);

            Assert.That(facade.IsDeprecated(value), Is.False,
                "A value that is already at the latest version is never deprecated.");
        }

        // ---- synthetic registries -------------------------------------------------------

        private const string SyntheticDomain = "synthetic.deprecation";

        private abstract class Stub : IBaboonGenerated
        {
            private readonly string _version;

            protected Stub(string version) => _version = version;

            public string BaboonDomainVersion() => _version;
            public string BaboonDomainIdentifier() => SyntheticDomain;
            public IReadOnlyList<string> BaboonSameInVersions() => new List<string> { _version };
            public IReadOnlyDictionary<string, string> BaboonForwardReadable() => new Dictionary<string, string>();
            public IReadOnlyDictionary<string, string> BaboonMinReaderVersions() => new Dictionary<string, string>();
            public string BaboonTypeIdentifier() => $"{SyntheticDomain}/:#{GetType().Name}";
        }

        private sealed class DtoV1 : Stub { public DtoV1() : base("1.0.0") {} }
        private sealed class DtoV2 : Stub { public DtoV2() : base("2.0.0") {} }
        private sealed class DtoV3 : Stub, IBaboonGeneratedLatest { public DtoV3() : base("3.0.0") {} }

        private sealed class GoneV1 : Stub { public GoneV1() : base("1.0.0") {} }
        private sealed class GoneV2 : Stub { public GoneV2() : base("2.0.0") {} }

        private abstract class AdtV1 : Stub
        {
            protected AdtV1() : base("1.0.0") {}

            public sealed class Branch : AdtV1, IBaboonAdtMemberMeta
            {
                public string BaboonAdtTypeIdentifier() => $"{SyntheticDomain}/:#AdtV1";
                public Type BaboonAdtType() => typeof(AdtV1);
            }
        }

        private abstract class AdtV2 : Stub { protected AdtV2() : base("2.0.0") {} }
        private abstract class AdtV3 : Stub, IBaboonGeneratedLatest { protected AdtV3() : base("3.0.0") {} }

        /// Converting must never happen while classifying: any execution fails the test.
        private sealed class NeverRuns<TFrom, TTo> : AbstractConversion<TFrom, TTo>
        {
            private readonly string _from;
            private readonly string _to;

            public NeverRuns(string from, string to)
            {
                _from = from;
                _to = to;
            }

            public override string VersionFrom() => _from;
            public override string VersionTo() => _to;
            public override string TypeId() => $"{SyntheticDomain}/:#{typeof(TTo).Name}";

            protected override TTo DoConvert<TCtx>(TCtx? context, AbstractBaboonConversions conversions, TFrom from)
                where TCtx : default =>
                throw new AssertionException("IsDeprecated must not execute converters.");
        }

        private sealed class Registry : AbstractBaboonConversions
        {
            private readonly string _versionTo;

            public Registry(string versionTo, params IConversion[] conversions)
            {
                _versionTo = versionTo;
                foreach (var conversion in conversions) Register(conversion);
            }

            public override List<string> VersionsFrom() => new List<string>();
            public override string VersionTo() => _versionTo;
        }

        private static BaboonCodecsFacade SyntheticFacade(Registry to2, Registry to3)
        {
            var facade = new BaboonCodecsFacade();
            facade.RegisterConversions(new BaboonDomainVersion(SyntheticDomain, "2.0.0"), () => to2);
            facade.RegisterConversions(new BaboonDomainVersion(SyntheticDomain, "3.0.0"), () => to3);
            return facade;
        }

        [Test]
        public void IsDeprecated_False_WhenOneConversionReachesTheLatestClassEarly()
        {
            // Deduplicated codegen: 2.0.0 and 3.0.0 share the class, so the 2.0.0 conversion
            // already produces the latest type and 3.0.0 registers nothing for it.
            var facade = SyntheticFacade(
                new Registry("2.0.0", new NeverRuns<DtoV1, DtoV3>("1.0.0", "2.0.0")),
                new Registry("3.0.0"));

            Assert.That(facade.IsDeprecated(new DtoV1()), Is.False,
                "The 2.0.0 conversion already produces the latest class; no 3.0.0 step is needed.");
        }

        [Test]
        public void IsDeprecated_False_ForAdtMemberWithAConversionAtEveryStep()
        {
            var facade = SyntheticFacade(
                new Registry("2.0.0", new NeverRuns<AdtV1, AdtV2>("1.0.0", "2.0.0")),
                new Registry("3.0.0", new NeverRuns<AdtV2, AdtV3>("2.0.0", "3.0.0")));

            Assert.That(facade.IsDeprecated(new AdtV1.Branch()), Is.False,
                "ADT conversions are registered on the ADT type; the walk must follow them from the branch.");
        }

        [Test]
        public void IsDeprecated_True_WhenTheChainDeadEndsBeforeLatest()
        {
            var facade = SyntheticFacade(
                new Registry("2.0.0", new NeverRuns<GoneV1, GoneV2>("1.0.0", "2.0.0")),
                new Registry("3.0.0"));

            Assert.That(facade.IsDeprecated(new GoneV1()), Is.True,
                "GoneV2 is not a latest type and nothing converts it further: the type is gone in 3.0.0.");
        }

        [Test]
        public void IsDeprecated_True_WhenNoConversionExistsAtAll()
        {
            var facade = SyntheticFacade(new Registry("2.0.0"), new Registry("3.0.0"));

            Assert.That(facade.IsDeprecated(new GoneV1()), Is.True,
                "No conversion leads out of the value's type.");
        }
    }
}
