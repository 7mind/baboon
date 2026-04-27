using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using Convtest.Testpkg;
using Baboon.Runtime.Shared;
using Newtonsoft.Json;
using Newtonsoft.Json.Linq;
using System.Linq;

namespace ConvTest
{
    [TestFixture]
    public class Test_CrossLanguageCompat
    {
        private readonly string baseDir =
            Path.GetFullPath(Path.Combine("..", "..", "..", "..", "..", "..", "target", "compat-test"));

        private readonly BaboonCodecContext ctx = BaboonCodecContext.Default;

        // Helper methods
        private AllBasicTypes ReadJsonFile(string source, string format)
        {
            var file = Path.Combine(baseDir, $"{source}-json", "all-basic-types.json");
            var jsonStr = File.ReadAllText(file, Encoding.UTF8);
            using var reader = new JsonTextReader(new StringReader(jsonStr))
                { DateParseHandling = DateParseHandling.None, FloatParseHandling = FloatParseHandling.Decimal };
            var jsonToken = JToken.Load(reader);
            return AllBasicTypes_JsonCodec.Instance.Decode(ctx, jsonToken);
        }

        private AllBasicTypes ReadUebaFile(string source, string format)
        {
            var file = Path.Combine(baseDir, $"{source}-ueba", "all-basic-types.ueba");
            var uebaBytes = File.ReadAllBytes(file);
            using var memoryStream = new MemoryStream(uebaBytes);
            using var reader = new BinaryReader(memoryStream);
            return AllBasicTypes_UEBACodec.Instance.Decode(ctx, reader);
        }

        private void AssertBasicFields(AllBasicTypes data, string label)
        {
            Console.WriteLine($"Successfully decoded {label}: {data.Vstr}");
            Assert.That(data.Vstr, Is.EqualTo("Hello, Baboon!"));
            Assert.That(data.Vi32, Is.EqualTo(123456));
            Assert.That(data.Vbit, Is.True);
        }

        private void PrintComparison(string label, string lang, AllBasicTypes langData, AllBasicTypes csData)
        {
            Console.WriteLine($"Comparing Scala and C# {label} data:");
            Console.WriteLine($"  {lang}: vi8={langData.Vi8}, vi16={langData.Vi16}, vi32={langData.Vi32}, vi64={langData.Vi64}");
            Console.WriteLine($"  C#:     vi8={csData.Vi8}, vi16={csData.Vi16}, vi32={csData.Vi32}, vi64={csData.Vi64}");
            Console.WriteLine($"  {lang}: vf32={langData.Vf32}, vf64={langData.Vf64}, vf128={langData.Vf128}");
            Console.WriteLine($"  C#:     vf32={csData.Vf32}, vf64={csData.Vf64}, vf128={csData.Vf128}");
            Console.WriteLine($"  {lang}: vtsu={langData.Vtsu}, vtso={langData.Vtso}");
            Console.WriteLine($"  C#:     vtsu={csData.Vtsu}, vtso={csData.Vtso}");
        }

        // JSON Tests
        [Test]
        public void CSharp_JSON_Deserialization_Should_Read_Scala_Generated_JSON()
        {
            AssertBasicFields(ReadJsonFile("scala", "Scala JSON"), "Scala JSON");
        }

        [Test]
        public void CSharp_JSON_Deserialization_Should_Read_CSharp_Generated_JSON()
        {
            AssertBasicFields(ReadJsonFile("cs", "C# JSON"), "C# JSON");
        }

        [Test]
        public void CSharp_JSON_Deserialization_Should_Read_Python_Generated_JSON()
        {
            AssertBasicFields(ReadJsonFile("python", "Python JSON"), "Python JSON");
        }

        [Test]
        public void CSharp_JSON_Deserialization_Should_Read_Rust_Generated_JSON()
        {
            AssertBasicFields(ReadJsonFile("rust", "Rust JSON"), "Rust JSON");
        }

        // UEBA Tests
        [Test]
        public void CSharp_UEBA_Deserialization_Should_Read_Scala_Generated_UEBA()
        {
            AssertBasicFields(ReadUebaFile("scala", "Scala UEBA"), "Scala UEBA");
        }

        [Test]
        public void CSharp_UEBA_Deserialization_Should_Read_CSharp_Generated_UEBA()
        {
            AssertBasicFields(ReadUebaFile("cs", "C# UEBA"), "C# UEBA");
        }

        [Test]
        public void CSharp_UEBA_Deserialization_Should_Read_Python_Generated_UEBA()
        {
            AssertBasicFields(ReadUebaFile("python", "Python UEBA"), "Python UEBA");
        }

        [Test]
        public void CSharp_UEBA_Deserialization_Should_Read_Rust_Generated_UEBA()
        {
            AssertBasicFields(ReadUebaFile("rust", "Rust UEBA"), "Rust UEBA");
        }

        // Cross-language comparison
        [Test]
        public void CrossLanguage_Comparison_Should_Verify_Scala_And_CSharp_JSON_Produce_Equivalent_Data()
        {
            var scalaData = ReadJsonFile("scala", "Scala JSON");
            var csData = ReadJsonFile("cs", "C# JSON");
            PrintComparison("JSON", "Scala", scalaData, csData);
            Assert.That(csData, Is.EqualTo(scalaData), "Scala and C# JSON data should be equal");
        }

        [Test]
        public void CrossLanguage_Comparison_Should_Verify_Scala_And_CSharp_UEBA_Produce_Equivalent_Data()
        {
            var scalaData = ReadUebaFile("scala", "Scala UEBA");
            var csData = ReadUebaFile("cs", "C# UEBA");
            PrintComparison("UEBA", "Scala", scalaData, csData);
            Assert.That(csData, Is.EqualTo(scalaData), "Scala and C# UEBA data should be equal");
        }

        [Test]
        public void CrossLanguage_Comparison_Should_Verify_Python_And_CSharp_JSON_Produce_Equivalent_Data()
        {
            var csData = ReadJsonFile("cs", "C# JSON");
            var pythonData = ReadJsonFile("python", "Python JSON");
            PrintComparison("JSON", "Python", pythonData, csData);
            Assert.That(csData, Is.EqualTo(pythonData), "Python and C# JSON data should be equal");
        }

        [Test]
        public void CrossLanguage_Comparison_Should_Verify_Python_And_CSharp_UEBA_Produce_Equivalent_Data()
        {
            var pythonData = ReadUebaFile("python", "Python UEBA");
            var csData = ReadUebaFile("cs", "C# UEBA");
            PrintComparison("UEBA", "Python", pythonData, csData);
            Assert.That(csData, Is.EqualTo(pythonData), "Python and C# UEBA data should be equal");
        }

        [Test]
        public void CrossLanguage_Comparison_Should_Verify_Rust_And_CSharp_JSON_Produce_Equivalent_Data()
        {
            var csData = ReadJsonFile("cs", "C# JSON");
            var rustData = ReadJsonFile("rust", "Rust JSON");
            PrintComparison("JSON", "Rust", rustData, csData);
            Assert.That(csData, Is.EqualTo(rustData), "Rust and C# JSON data should be equal");
        }

        [Test]
        public void CrossLanguage_Comparison_Should_Verify_Rust_And_CSharp_UEBA_Produce_Equivalent_Data()
        {
            var rustData = ReadUebaFile("rust", "Rust UEBA");
            var csData = ReadUebaFile("cs", "C# UEBA");
            PrintComparison("UEBA", "Rust", rustData, csData);
            Assert.That(csData, Is.EqualTo(rustData), "Rust and C# UEBA data should be equal");
        }

        [Test]
        public void CSharp_JSON_Deserialization_Should_Read_TypeScript_Generated_JSON()
        {
            AssertBasicFields(ReadJsonFile("typescript", "TypeScript JSON"), "TypeScript JSON");
        }

        [Test]
        public void CSharp_UEBA_Deserialization_Should_Read_TypeScript_Generated_UEBA()
        {
            AssertBasicFields(ReadUebaFile("typescript", "TypeScript UEBA"), "TypeScript UEBA");
        }

        [Test]
        public void CSharp_JSON_Deserialization_Should_Read_Kotlin_Generated_JSON()
        {
            AssertBasicFields(ReadJsonFile("kotlin", "Kotlin JSON"), "Kotlin JSON");
        }

        [Test]
        public void CSharp_UEBA_Deserialization_Should_Read_Kotlin_Generated_UEBA()
        {
            AssertBasicFields(ReadUebaFile("kotlin", "Kotlin UEBA"), "Kotlin UEBA");
        }

        [Test]
        public void CSharp_JSON_Deserialization_Should_Read_Java_Generated_JSON()
        {
            AssertBasicFields(ReadJsonFile("java", "Java JSON"), "Java JSON");
        }

        [Test]
        public void CSharp_UEBA_Deserialization_Should_Read_Java_Generated_UEBA()
        {
            AssertBasicFields(ReadUebaFile("java", "Java UEBA"), "Java UEBA");
        }

        [Test]
        public void CSharp_JSON_Deserialization_Should_Read_Dart_Generated_JSON()
        {
            AssertBasicFields(ReadJsonFile("dart", "Dart JSON"), "Dart JSON");
        }

        [Test]
        public void CSharp_UEBA_Deserialization_Should_Read_Dart_Generated_UEBA()
        {
            AssertBasicFields(ReadUebaFile("dart", "Dart UEBA"), "Dart UEBA");
        }

        [Test]
        public void CSharp_JSON_Deserialization_Should_Read_Swift_Generated_JSON()
        {
            var file = Path.Combine(baseDir, "swift-json", "all-basic-types.json");
            if (!File.Exists(file)) { Assert.Ignore("Swift JSON file not found, skipping"); return; }
            AssertBasicFields(ReadJsonFile("swift", "Swift JSON"), "Swift JSON");
        }

        [Test]
        public void CSharp_UEBA_Deserialization_Should_Read_Swift_Generated_UEBA()
        {
            var file = Path.Combine(baseDir, "swift-ueba", "all-basic-types.ueba");
            if (!File.Exists(file)) { Assert.Ignore("Swift UEBA file not found, skipping"); return; }
            AssertBasicFields(ReadUebaFile("swift", "Swift UEBA"), "Swift UEBA");
        }

        // ----------------------------------------------------------------------------------------
        // AnyShowcase cross-language tests (M13 / PR 13.1) — Scala + C# baseline.
        //
        // Mirror the Scala Test_CrossLanguageCompat additions. Each language emits an AnyShowcase
        // fixture with one slot per `any` variant (A/B/C/D1/D2/D3 + opt + lst); decoding either
        // language's fixture must yield the same canonical sequence of InnerPayload values.
        //
        // PR 13.2 will fan out to the remaining 7 languages.
        // ----------------------------------------------------------------------------------------

        private static IReadOnlyList<InnerPayload> ExpectedAnyPayloads()
        {
            return new List<InnerPayload>
            {
                new InnerPayload("variant-A", 1),
                new InnerPayload("variant-B", 2),
                new InnerPayload("variant-C", 3),
                new InnerPayload("variant-D1", 4),
                new InnerPayload("variant-D2", 5),
                new InnerPayload("variant-D3", 6),
                new InnerPayload("opt-any", 7),
                new InnerPayload("lst-any-0", 8),
            };
        }

        private AnyShowcase ReadAnyShowcaseJson(string source)
        {
            var file = Path.Combine(baseDir, $"{source}-json", "any-showcase.json");
            var jsonStr = File.ReadAllText(file, Encoding.UTF8);
            using var reader = new JsonTextReader(new StringReader(jsonStr))
                { DateParseHandling = DateParseHandling.None };
            var jsonToken = JToken.Load(reader);
            return AnyShowcase_JsonCodec.Instance.Decode(ctx, jsonToken);
        }

        private AnyShowcase ReadAnyShowcaseUeba(string source)
        {
            var file = Path.Combine(baseDir, $"{source}-ueba", "any-showcase.ueba");
            var bytes = File.ReadAllBytes(file);
            using var ms = new MemoryStream(bytes);
            using var reader = new BinaryReader(ms);
            return AnyShowcase_UEBACodec.Instance.Decode(ctx, reader);
        }

        // Decode the inner InnerPayload directly via InnerPayload_*Codec — facade.DecodeAny is not
        // used because partial-meta variants (B/C/D1/D2/D3) lack the full (domain, version,
        // typeid) triple required for facade resolution.
        private static InnerPayload DecodeInner(AnyOpaque o)
        {
            switch (o)
            {
                case AnyOpaqueUeba ueba:
                    using (var ms = new MemoryStream(ueba.Bytes))
                    using (var br = new BinaryReader(ms))
                    {
                        return InnerPayload_UEBACodec.Instance.Decode(BaboonCodecContext.Compact, br);
                    }
                case AnyOpaqueJson json:
                    return InnerPayload_JsonCodec.Instance.Decode(BaboonCodecContext.Compact, json.Json);
                default:
                    throw new InvalidOperationException($"unexpected AnyOpaque subclass: {o.GetType()}");
            }
        }

        private static IReadOnlyList<InnerPayload> DecodeAllPayloads(AnyShowcase v)
        {
            var slots = new List<AnyOpaque>
            {
                v.VAnyA,
                v.VAnyB,
                v.VAnyC,
                v.VAnyD1,
                v.VAnyD2,
                v.VAnyD3,
                v.OptAny ?? throw new InvalidOperationException("optAny was null; expected non-null"),
                v.LstAny.FirstOrDefault() ?? throw new InvalidOperationException("lstAny was empty; expected one element"),
            };
            return slots.Select(DecodeInner).ToList();
        }

        [Test]
        public void AnyShowcase_JSON_Should_Decode_Scala_Emitted_Into_Expected_Payloads()
        {
            var decoded = DecodeAllPayloads(ReadAnyShowcaseJson("scala"));
            Assert.That(decoded, Is.EqualTo(ExpectedAnyPayloads()));
        }

        [Test]
        public void AnyShowcase_JSON_Should_Decode_CSharp_Emitted_Into_Expected_Payloads()
        {
            var decoded = DecodeAllPayloads(ReadAnyShowcaseJson("cs"));
            Assert.That(decoded, Is.EqualTo(ExpectedAnyPayloads()));
        }

        [Test]
        public void AnyShowcase_JSON_Should_Produce_Same_Payloads_From_Scala_And_CSharp_Fixtures()
        {
            var scala = DecodeAllPayloads(ReadAnyShowcaseJson("scala"));
            var cs = DecodeAllPayloads(ReadAnyShowcaseJson("cs"));
            Assert.That(cs, Is.EqualTo(scala));
        }

        [Test]
        public void AnyShowcase_UEBA_Should_Decode_Scala_Emitted_Into_Expected_Payloads()
        {
            var decoded = DecodeAllPayloads(ReadAnyShowcaseUeba("scala"));
            Assert.That(decoded, Is.EqualTo(ExpectedAnyPayloads()));
        }

        [Test]
        public void AnyShowcase_UEBA_Should_Decode_CSharp_Emitted_Into_Expected_Payloads()
        {
            var decoded = DecodeAllPayloads(ReadAnyShowcaseUeba("cs"));
            Assert.That(decoded, Is.EqualTo(ExpectedAnyPayloads()));
        }

        [Test]
        public void AnyShowcase_UEBA_Should_Produce_Same_Payloads_From_Scala_And_CSharp_Fixtures()
        {
            var scala = DecodeAllPayloads(ReadAnyShowcaseUeba("scala"));
            var cs = DecodeAllPayloads(ReadAnyShowcaseUeba("cs"));
            Assert.That(cs, Is.EqualTo(scala));
        }

        [Test]
        public void AnyShowcase_UEBA_Should_Produce_Byte_Identical_Scala_And_CSharp_Fixtures()
        {
            var scalaBytes = File.ReadAllBytes(Path.Combine(baseDir, "scala-ueba", "any-showcase.ueba"));
            var csBytes = File.ReadAllBytes(Path.Combine(baseDir, "cs-ueba", "any-showcase.ueba"));
            Assert.That(csBytes, Is.EqualTo(scalaBytes), "Scala and C# UEBA bytes diverged");
        }
    }
}