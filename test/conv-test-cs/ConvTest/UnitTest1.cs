using System;
using System.Collections.Generic;
using Convtest.Testpkg;
using Baboon.Runtime.Shared;
using Baboon.Time;
using Newtonsoft.Json.Linq;

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

        // RED repro for T149 / D39 (M69, G25).
        //
        // BUG UNDER TEST: BaboonCodecsFacade.DecodeFromJson(string) (BaboonCodecsFacade.cs:739)
        // parses the wire string with JToken.Parse(value). JToken.Parse uses
        // Newtonsoft's default DateParseHandling.DateTime, which pre-converts any
        // ISO-8601 date-like *string* value into a JTokenType.Date token BEFORE
        // baboon's codec ever reads it. The generated tsu/tso decoder then calls
        // token.Value<string>() (CSJsonCodecGenerator.scala:403) -> BaboonDateTimeFormats.FromString
        // -> DateTimeOffset.ParseExact(..., Tsz, DateTimeStyles.None) (BaboonTime.cs:427-431).
        // Because the token is no longer the verbatim wire string (Newtonsoft has
        // re-serialised the parsed DateTime, dropping the offset / sub-second form
        // the strict Tsz formats require), ParseExact throws FormatException — and a
        // plain `str` field carrying date-like text is likewise mutated.
        //
        // The existing codec round-trips do NOT catch this: Test_CrossLanguageCompat
        // decodes via JToken.Load(JsonTextReader{ DateParseHandling = None }) — it
        // deliberately bypasses the date pre-conversion. This repro MUST go through
        // the string overload (DecodeFromJson(string) -> JToken.Parse) to exercise the
        // live defect.
        //
        // This test is RED until T151/T152 land the fix (switch the string overload
        // to a DateParseHandling.None parse, e.g. BaboonTools.ParseWireJson). T153
        // verifies it goes GREEN.
        [Test]
        public void Test_DecodeFromJsonString_Preserves_Tsu_Tso_And_DateLike_Str()
        {
            // tso: non-zero offset (+05:30) + sub-second precision (123 ms).
            var tso = new RpDateTime(
                new DateTimeOffset(2026, 5, 2, 12, 0, 0, 123, new TimeSpan(5, 30, 0)));
            // tsu: UTC, trailing "Z" wire form, sub-second precision (456 ms).
            var tsu = new RpDateTime(
                new DateTimeOffset(2026, 5, 2, 6, 30, 0, 456, TimeSpan.Zero),
                DateTimeKind.Utc);
            // str: ISO-date-like text — Newtonsoft's date pre-conversion mangles this too.
            const string dateLikeStr = "2021-06-01T12:00:00Z";

            var original = MinimalCreateSample(tsu, tso, dateLikeStr);

            var facade = new BaboonCodecsFacade();
            facade.Register(
                new BaboonDomainVersion("convtest.testpkg", "2.0.0"),
                () => Convtest.Testpkg.BaboonCodecsJson.Instance,
                () => Convtest.Testpkg.BaboonCodecsUeba.Instance);

            // ENCODE to a JSON *string* (full meta envelope with the $c content key).
            var encoded = facade.EncodeToJson(original);
            Assert.That(encoded.IsRight, Is.True,
                "precondition: encode must succeed");
            var wireString = encoded.GetRight().ToString();

            // DECODE that string back via the STRING overload (the JToken.Parse path).
            var decodedResult = facade.DecodeFromJson(wireString);

            Assert.That(decodedResult.IsRight, Is.True,
                "DecodeFromJson(string) must not corrupt tsu/tso/date-str via JToken.Parse "
                + "date pre-conversion. Failure: "
                + (decodedResult.IsLeft ? decodedResult.GetLeft().ToString() : "<none>"));

            var decoded = (AllBasicTypes)decodedResult.GetRight()!;

            // EXACT equality: offset + sub-second preserved; str unchanged.
            Assert.That(decoded.Vtso, Is.EqualTo(original.Vtso), "tso offset/sub-second drift");
            Assert.That(decoded.Vtsu, Is.EqualTo(original.Vtsu), "tsu drift");
            Assert.That(decoded.Vstr, Is.EqualTo(original.Vstr), "date-like str mutated");
        }

        // Minimal AllBasicTypes sample; only vtsu/vtso/vstr are load-bearing for T149,
        // remaining fields are arbitrary valid values.
        private static AllBasicTypes MinimalCreateSample(RpDateTime tsu, RpDateTime tso, string str)
        {
            return new AllBasicTypes(
                Vi8: (sbyte)1, Vi16: (short)1, Vi32: 1, Vi64: 1L,
                Vu8: (byte)1, Vu16: (ushort)1, Vu32: 1U, Vu64: 1UL,
                Vf32: 1.0f, Vf64: 1.0, Vf128: 1.0m,
                Vstr: str,
                Vbstr: new ByteString(new byte[] { 0x01 }),
                Vuid: Guid.Parse("12345678-1234-5678-1234-567812345678"),
                Vbit: true,
                Vtsu: tsu,
                Vtso: tso,
                VoptStr: null,
                VlstI32: new List<int>(),
                VsetStr: System.Collections.Immutable.ImmutableHashSet<string>.Empty,
                VmapStrI32: System.Collections.Immutable.ImmutableDictionary<string, int>.Empty,
                VoptLst: null,
                VlstOpt: new List<int?>(),
                VmapLst: System.Collections.Immutable.ImmutableDictionary<string, IReadOnlyList<long>>.Empty,
                VWireEnum: WireEnum.Cafe,
                VPointId: new PointId(X: 1, Y: 1),
                VmapItemIdU32: System.Collections.Immutable.ImmutableDictionary<ItemId, uint>.Empty,
                VmapCompositeIdU32: System.Collections.Immutable.ImmutableDictionary<CompositeId, uint>.Empty
            );
        }
    }
}