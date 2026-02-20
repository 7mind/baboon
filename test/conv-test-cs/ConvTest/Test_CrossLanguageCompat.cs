using System;
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
    }
}