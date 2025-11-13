using System;
using System.IO;
using System.Text;
using Convtest.Testpkg;
using Baboon.Runtime.Shared;
using Newtonsoft.Json;
using Newtonsoft.Json.Linq;

namespace ConvTest
{
    [TestFixture]
    public class Test_CrossLanguageCompat
    {
        private readonly string baseDir = Path.GetFullPath(Path.Combine("..", "..", "..", "..", "..", "..", "target", "compat-test"));
        private readonly BaboonCodecContext ctx = BaboonCodecContext.Default;

        // Helper methods
        private AllBasicTypes ReadJsonFile(string source, string format)
        {
            var file = Path.Combine(baseDir, $"{source}-json", "all-basic-types.json");
            var jsonStr = File.ReadAllText(file, Encoding.UTF8);
            using var reader = new JsonTextReader(new StringReader(jsonStr)) { DateParseHandling = DateParseHandling.None };
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

        private void PrintComparison(string label, AllBasicTypes scalaData, AllBasicTypes csData)
        {
            Console.WriteLine($"Comparing Scala and C# {label} data:");
            Console.WriteLine($"  Scala: vi8={scalaData.Vi8}, vi16={scalaData.Vi16}, vi32={scalaData.Vi32}, vi64={scalaData.Vi64}");
            Console.WriteLine($"  C#:    vi8={csData.Vi8}, vi16={csData.Vi16}, vi32={csData.Vi32}, vi64={csData.Vi64}");
            Console.WriteLine($"  Scala: vf32={scalaData.Vf32}, vf64={scalaData.Vf64}, vf128={scalaData.Vf128}");
            Console.WriteLine($"  C#:    vf32={csData.Vf32}, vf64={csData.Vf64}, vf128={csData.Vf128}");
            Console.WriteLine($"  Scala: vtsu={scalaData.Vtsu}, vtso={scalaData.Vtso}");
            Console.WriteLine($"  C#:    vtsu={csData.Vtsu}, vtso={csData.Vtso}");
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

        // Cross-language comparison
        [Test]
        public void CrossLanguage_Comparison_Should_Verify_Scala_And_CSharp_JSON_Produce_Equivalent_Data()
        {
            var scalaData = ReadJsonFile("scala", "Scala JSON");
            var csData = ReadJsonFile("cs", "C# JSON");
            PrintComparison("JSON", scalaData, csData);
            Assert.That(csData, Is.EqualTo(scalaData), "Scala and C# JSON data should be equal");
        }

        [Test]
        public void CrossLanguage_Comparison_Should_Verify_Scala_And_CSharp_UEBA_Produce_Equivalent_Data()
        {
            var scalaData = ReadUebaFile("scala", "Scala UEBA");
            var csData = ReadUebaFile("cs", "C# UEBA");
            PrintComparison("UEBA", scalaData, csData);
            Assert.That(csData, Is.EqualTo(scalaData), "Scala and C# UEBA data should be equal");
        }
    }
}
