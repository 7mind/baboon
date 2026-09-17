using Baboon.Runtime.Shared;
using Convtest.Testpkg;
using NUnit.Framework;

namespace ConvTest
{
    [TestFixture]
    public class RenamedConversionTests
    {
        [Test]
        public void RenamedDtoRetainsPayload()
        {
            var conversions = new BaboonConversions(new RequiredConversionsImpl());
            var result = conversions.Convert(new Convtest.Testpkg.v1_0_0.OldTypeName("kept")).To<NewTypeName>();
            Assert.That(result.Value, Is.EqualTo("kept"));
        }

        [Test]
        public void RenamedAdtAndBranchConversionsValidateTheirOwnTargetIds()
        {
            var conversions = new BaboonConversions(new RequiredConversionsImpl());
            var source = new Convtest.Testpkg.v1_0_0.OldAdtName.Branch1("kept");
            var parentResult = conversions.Convert<Convtest.Testpkg.v1_0_0.OldAdtName>(source).To<NewAdtName>();
            var branchResult = conversions.Convert(source).To<NewAdtName.Branch1>();
            Assert.That(parentResult, Is.TypeOf<NewAdtName.Branch1>());
            Assert.That(((NewAdtName.Branch1)parentResult).F, Is.EqualTo("kept"));
            Assert.That(branchResult, Is.EqualTo(parentResult));
        }

        [Test]
        public void NamespacedRenamedAdtRetainsPayload()
        {
            var conversions = new BaboonConversions(new RequiredConversionsImpl());
            var source = new Convtest.Testpkg.v1_0_0.abs.core.OldAbsAdt.BranchAbs("namespaced");
            var result = conversions.Convert<Convtest.Testpkg.v1_0_0.abs.core.OldAbsAdt>(source)
                .To<Convtest.Testpkg.abs.core.deeper.NewAbsAdt>();
            Assert.That(result, Is.TypeOf<Convtest.Testpkg.abs.core.deeper.NewAbsAdt.BranchAbs>());
            Assert.That(((Convtest.Testpkg.abs.core.deeper.NewAbsAdt.BranchAbs)result).V, Is.EqualTo("namespaced"));
        }
    }
}
