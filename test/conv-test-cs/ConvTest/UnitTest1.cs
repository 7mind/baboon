using System;
using System.Collections.Generic;
using Convtest.Testpkg;

namespace ConvTest
{
    public class RequiredConversionsImpl : RequiredConversions
    {
    }

    public class Tests
    {
        [SetUp]
        public void Setup()
        {
        }

        [Test]
        public void Test_Adt_Auto_Upgrade()
        {
            var a1 = new Convtest.Testpkg.v1_0_0.Adt0.B1("val1");

            var conv = new BaboonConversions(new RequiredConversionsImpl());

            var a1u1 = conv.Convert(a1).To<Adt0.B1>();
            Assert.That(a1.F == ((Adt0.B1)a1u1).F);

            var a1u2 = conv.Convert<Convtest.Testpkg.v1_0_0.Adt0>(a1).To<Adt0>();
            Assert.That(a1u1 == a1u2);
        }

        // Regression test for `EnumVariant : was[OldName]` rename conversion
        // (see test/conv-test/pkg02.baboon `enum EnumMemberRename` and the
        // matching Rust/TypeScript regression tests in
        // test/conv-test-{rs,ts}/tests/). Invokes the generated rename
        // conversion against both the renamed variant (OldValue -> NewValue)
        // and the pass-through variant (KeepValue) to also exercise the
        // fallback arm of the switch.
        [Test]
        public void Test_EnumMemberRename_Renamed_Variant_Maps_To_New_Name()
        {
            var conv = new BaboonConversions(new RequiredConversionsImpl());
            var mapped = conv.ConvertWithContext<object,
                Convtest.Testpkg.v1_0_0.EnumMemberRename,
                EnumMemberRename>(null, Convtest.Testpkg.v1_0_0.EnumMemberRename.OldValue);
            Assert.That(mapped, Is.EqualTo(EnumMemberRename.NewValue));
        }

        [Test]
        public void Test_EnumMemberRename_NonRenamed_Variant_Passes_Through()
        {
            var conv = new BaboonConversions(new RequiredConversionsImpl());
            var mapped = conv.ConvertWithContext<object,
                Convtest.Testpkg.v1_0_0.EnumMemberRename,
                EnumMemberRename>(null, Convtest.Testpkg.v1_0_0.EnumMemberRename.KeepValue);
            Assert.That(mapped, Is.EqualTo(EnumMemberRename.KeepValue));
        }

        [Test]
        public void Test_Opt_List_Auto_Upgrade()
        {
            var t1 = new Convtest.Testpkg.v1_0_0.TransferOpt(
                Guid.Empty,
                new List<Convtest.Testpkg.v1_0_0.Adt0> { new Convtest.Testpkg.v1_0_0.Adt0.B1("val1") },
                new Dictionary<string, IReadOnlyList<Convtest.Testpkg.v1_0_0.Adt0>> { { "1", new List<Convtest.Testpkg.v1_0_0.Adt0> { new Convtest.Testpkg.v1_0_0.Adt0.B1("val2") } } }
            );

            var conv = new BaboonConversions(new RequiredConversionsImpl());

            var t1u = conv.Convert(t1).To<TransferOpt>();
            Assert.That(t1.U == t1u.U);
        }
    }
}