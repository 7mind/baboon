#nullable enable
using System;
using System.Collections.Generic;
using Baboon.Runtime.Shared;
using NUnit.Framework;

namespace ConversionsTest
{
    [TestFixture]
    public class ConversionValidationTests
    {
        private sealed class Conversions : AbstractBaboonConversions
        {
            public override List<string> VersionsFrom() => new() { "1.0.0" };
            public override string VersionTo() => "2.0.0";
        }

        private class Model : IBaboonGenerated
        {
            private readonly string _typeId;
            public Model(string typeId) { _typeId = typeId; }
            public string BaboonTypeIdentifier() => _typeId;
            public string BaboonDomainVersion() => "1.0.0";
            public string BaboonDomainIdentifier() => "test";
            public IReadOnlyList<string> BaboonSameInVersions() => new[] { "1.0.0" };
            public IReadOnlyDictionary<string, string> BaboonForwardReadable() => new Dictionary<string, string>();
            public IReadOnlyDictionary<string, string> BaboonMinReaderVersions() => new Dictionary<string, string>();
        }

        private sealed class Branch : Model, IBaboonAdtMemberMeta
        {
            private readonly string _parentId;
            public Branch(string typeId, string parentId) : base(typeId) { _parentId = parentId; }
            public string BaboonAdtTypeIdentifier() => _parentId;
            public Type BaboonAdtType() => typeof(Model);
        }

        private class LegacyConversion : AbstractConversion<object?, object?>
        {
            private readonly string _source;
            private readonly object? _result;
            private readonly bool _enabled;
            public readonly List<string> Calls = new();

            public LegacyConversion(string source, object? result, bool enabled)
            {
                _source = source;
                _result = result;
                _enabled = enabled;
            }

            public override string VersionFrom() => "1.0.0";
            public override string VersionTo() => "2.0.0";
            public override string TypeId() { Calls.Add("source"); return _source; }
            protected override bool ConversionValidationEnabled() => _enabled;
            public void ValidateSource(object? value) => ValidateBaboonType(value);
            protected override object? DoConvert<TCtx>(TCtx? context, AbstractBaboonConversions conversions, object? from) where TCtx : default
            {
                Calls.Add("convert");
                return _result;
            }
        }

        private sealed class RenamingConversion : LegacyConversion
        {
            private readonly string _target;
            public RenamingConversion(string source, string target, object? result, bool enabled) : base(source, result, enabled)
            {
                _target = target;
            }

            protected override string TargetTypeId() { Calls.Add("target"); return _target; }
        }

        [Test]
        public void RenamedResultUsesTargetIdAfterConversionWithoutChangingSourceId()
        {
            var result = new Model("new");
            var conversion = new RenamingConversion("old", "new", result, true);
            Assert.That(conversion.Convert<object>(null, new Conversions(), new Model("old")), Is.SameAs(result));
            Assert.That(conversion.Calls, Is.EqualTo(new[] { "convert", "source", "target" }));
            Assert.That(((IConversion)conversion).TypeId(), Is.EqualTo("old"));
        }

        [Test]
        public void WrongSourceStillFailsAfterDoConvertAndBeforeTargetValidation()
        {
            var conversion = new RenamingConversion("old", "new", new Model("new"), true);
            Assert.Throws<ArgumentException>(() => conversion.Convert<object>(null, new Conversions(), new Model("wrong")));
            Assert.That(conversion.Calls, Is.EqualTo(new[] { "convert", "source" }));
        }

        [Test]
        public void WrongResultFailsAgainstTargetId()
        {
            var conversion = new RenamingConversion("old", "new", new Model("old"), true);
            var error = Assert.Throws<ArgumentException>(() => conversion.Convert<object>(null, new Conversions(), new Model("old")));
            Assert.That(error!.Message, Does.Contain("must be new"));
        }

        [TestCase(true)]
        [TestCase(false)]
        public void AdtValuesAcceptParentOrExactBranchIds(bool parentIds)
        {
            var source = new Branch("old-branch", "old-parent");
            var result = new Branch("new-branch", "new-parent");
            var conversion = new RenamingConversion(parentIds ? "old-parent" : "old-branch", parentIds ? "new-parent" : "new-branch", result, true);
            Assert.That(conversion.Convert<object>(null, new Conversions(), source), Is.SameAs(result));
        }

        [Test]
        public void AdtResultRejectsUnrelatedParentAndBranch()
        {
            var conversion = new RenamingConversion("old-parent", "new-parent", new Branch("wrong-branch", "wrong-parent"), true);
            Assert.Throws<ArgumentException>(() => conversion.Convert<object>(null, new Conversions(), new Branch("old-branch", "old-parent")));
        }

        [Test]
        public void LegacySubclassKeepsSameIdValidationAndProtectedSourceHelper()
        {
            var result = new Model("old");
            var conversion = new LegacyConversion("old", result, true);
            Assert.That(conversion.Convert<object>(null, new Conversions(), new Model("old")), Is.SameAs(result));
            Assert.Throws<ArgumentException>(() => conversion.ValidateSource(new Model("new")));
            var invalidLegacy = new LegacyConversion("old", new Model("new"), true);
            Assert.Throws<ArgumentException>(() => invalidLegacy.Convert<object>(null, new Conversions(), new Model("old")));
            var renamed = new RenamingConversion("old", "new", new Model("new"), true);
            Assert.Throws<ArgumentException>(() => renamed.ValidateSource(new Model("new")));
        }

        [Test]
        public void DisabledValidationDoesNotResolveEitherTypeId()
        {
            var result = new Model("wrong");
            var conversion = new RenamingConversion("old", "new", result, false);
            Assert.That(conversion.Convert<object>(null, new Conversions(), new Model("wrong")), Is.SameAs(result));
            Assert.That(conversion.Calls, Is.EqualTo(new[] { "convert" }));
        }

        [TestCase(null)]
        [TestCase("foreign")]
        public void NonBaboonResultsDoNotResolveTargetId(object? result)
        {
            var conversion = new RenamingConversion("old", "new", result, true);
            Assert.That(conversion.Convert<object>(null, new Conversions(), new Model("old")), Is.SameAs(result));
            Assert.That(conversion.Calls, Is.EqualTo(new[] { "convert", "source" }));
        }
    }
}
