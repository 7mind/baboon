#nullable enable
using System;
using System.Globalization;
using Baboon.Runtime.Shared;
using NUnit.Framework;

namespace ConversionsTest
{
    [TestFixture]
    public class FacadeRegistrationTests
    {
        private static void WithCulture(CultureInfo culture, Action test)
        {
            var previous = CultureInfo.CurrentCulture;
            try
            {
                CultureInfo.CurrentCulture = culture;
                test();
            }
            finally { CultureInfo.CurrentCulture = previous; }
        }

        private static BaboonCodecsFacade Registered(string version)
        {
            var facade = new BaboonCodecsFacade();
            facade.RegisterMeta(new BaboonDomainVersion("test", version), () => throw new AssertionException("metadata was forced"));
            return facade;
        }

        [Test]
        public void MutableCultureSignChangesAreObservedAfterFirstLookup()
        {
            var culture = (CultureInfo)CultureInfo.InvariantCulture.Clone();
            WithCulture(culture, () =>
            {
                var facade = Registered("+1.0.0");
                Assert.That(facade.Latest("test"), Is.EqualTo(new BaboonVersion(1, 0, 0)));
                culture.NumberFormat.PositiveSign = "p";
                Assert.Throws<Exception>(() => facade.Latest("test"));
                culture.NumberFormat.PositiveSign = "+";
                Assert.That(facade.Latest("test"), Is.EqualTo(new BaboonVersion(1, 0, 0)));
            });
        }

        [TestCase("+1.0.0", "p", null)]
        [TestCase("11.0.0", "1", 1)]
        public void DistinctReadOnlyCultureFormatsAreObserved(string version, string positiveSign, int? expectedMajor)
        {
            var first = CultureInfo.ReadOnly((CultureInfo)CultureInfo.InvariantCulture.Clone());
            var changed = (CultureInfo)CultureInfo.InvariantCulture.Clone();
            changed.NumberFormat.PositiveSign = positiveSign;
            var second = CultureInfo.ReadOnly(changed);
            WithCulture(first, () =>
            {
                var facade = Registered(version);
                Assert.That(facade.Latest("test"), Is.EqualTo(BaboonVersion.From(version)));
                CultureInfo.CurrentCulture = second;
                if (expectedMajor.HasValue)
                    Assert.That(facade.Latest("test"), Is.EqualTo(new BaboonVersion(expectedMajor.Value, 0, 0)));
                else
                    Assert.Throws<Exception>(() => facade.Latest("test"));
            });
        }

        private sealed class CustomCulture : CultureInfo
        {
            public readonly NumberFormatInfo ParsingFormat = (NumberFormatInfo)NumberFormatInfo.InvariantInfo.Clone();
            public CustomCulture() : base("") { }
            public override NumberFormatInfo NumberFormat
            {
                get => NumberFormatInfo.InvariantInfo;
                set => throw new InvalidOperationException();
            }
            public override object? GetFormat(Type? formatType)
                => formatType == typeof(NumberFormatInfo) ? ParsingFormat : base.GetFormat(formatType);
        }

        [Test]
        public void CultureSubclassFormattingRemainsDynamic()
        {
            var culture = new CustomCulture();
            WithCulture(culture, () =>
            {
                var facade = Registered("+1.0.0");
                Assert.That(facade.Latest("test"), Is.EqualTo(new BaboonVersion(1, 0, 0)));
                culture.ParsingFormat.PositiveSign = "p";
                Assert.Throws<Exception>(() => facade.Latest("test"));
            });
        }

        [Test]
        public void RegistrationOrdersNumericallyAndKeepsFactoriesLazy()
        {
            var facade = new BaboonCodecsFacade();
            foreach (var version in new[] { "1.2.0", "1.10.0", "1.3.0", "1.10.0" })
                facade.RegisterMeta(new BaboonDomainVersion("test", version), () => throw new AssertionException("metadata was forced"));
            Assert.That(facade.Latest("test"), Is.EqualTo(new BaboonVersion(1, 10, 0)));
            var other = new BaboonCodecsFacade();
            other.Register(facade);
            Assert.That(other.Latest("test"), Is.EqualTo(facade.Latest("test")));
        }

        [Test]
        public void MalformedSingleVersionIsParsedOnLookupNotRegistration()
        {
            var facade = new BaboonCodecsFacade();
            facade.RegisterMeta(new BaboonDomainVersion("test", "invalid"), () => throw new AssertionException("metadata was forced"));
            Assert.Throws<Exception>(() => facade.Latest("test"));
        }

        [Test]
        public void PublicDomainVersionRecordWithKeepsValueSemantics()
        {
            var first = new BaboonDomainVersion("test", "1.0.0");
            Assert.That(first.Version, Is.EqualTo(new BaboonVersion(1, 0, 0)));
            var second = first with { DomainVersion = "2.0.0" };
            Assert.That(second.Version, Is.EqualTo(new BaboonVersion(2, 0, 0)));
            Assert.That(second, Is.EqualTo(new BaboonDomainVersion("test", "2.0.0")));
            Assert.That(first.Version, Is.EqualTo(new BaboonVersion(1, 0, 0)));
        }
    }
}
