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
        private string scalaJsonFile;
        private string scalaUebaFile;
        private string csJsonFile;
        private string csUebaFile;

        [SetUp]
        public void Setup()
        {
            scalaJsonFile = Path.Combine(baseDir, "scala-json", "all-basic-types.json");
            scalaUebaFile = Path.Combine(baseDir, "scala-ueba", "all-basic-types.ueba");
            csJsonFile = Path.Combine(baseDir, "cs-json", "all-basic-types.json");
            csUebaFile = Path.Combine(baseDir, "cs-ueba", "all-basic-types.ueba");
        }

        [Test]
        public void CSharp_JSON_Deserialization_Should_Read_Scala_Generated_JSON()
        {
            var jsonStr = File.ReadAllText(scalaJsonFile, Encoding.UTF8);
            using var reader = new JsonTextReader(new StringReader(jsonStr)) { DateParseHandling = DateParseHandling.None };
            var jsonToken = JToken.Load(reader);

            var ctx = BaboonCodecContext.Default;
            var decoded = AllBasicTypes_JsonCodec.Instance.Decode(ctx, jsonToken);

            Console.WriteLine($"Successfully decoded Scala JSON: {decoded.Vstr}");
            Assert.That(decoded.Vstr, Is.EqualTo("Hello, Baboon!"));
            Assert.That(decoded.Vi32, Is.EqualTo(123456));
            Assert.That(decoded.Vbit, Is.True);
        }

        [Test]
        public void CSharp_JSON_Deserialization_Should_Read_CSharp_Generated_JSON()
        {
            var jsonStr = File.ReadAllText(csJsonFile, Encoding.UTF8);
            using var reader = new JsonTextReader(new StringReader(jsonStr)) { DateParseHandling = DateParseHandling.None };
            var jsonToken = JToken.Load(reader);

            var ctx = BaboonCodecContext.Default;
            var decoded = AllBasicTypes_JsonCodec.Instance.Decode(ctx, jsonToken);

            Console.WriteLine($"Successfully decoded C# JSON: {decoded.Vstr}");
            Assert.That(decoded.Vstr, Is.EqualTo("Hello, Baboon!"));
            Assert.That(decoded.Vi32, Is.EqualTo(123456));
            Assert.That(decoded.Vbit, Is.True);
        }

        [Test]
        public void CSharp_UEBA_Deserialization_Should_Read_Scala_Generated_UEBA()
        {
            var uebaBytes = File.ReadAllBytes(scalaUebaFile);
            using (var memoryStream = new MemoryStream(uebaBytes))
            using (var reader = new BinaryReader(memoryStream))
            {
                var ctx = BaboonCodecContext.Default;
                var decoded = AllBasicTypes_UEBACodec.Instance.Decode(ctx, reader);

                Console.WriteLine($"Successfully decoded Scala UEBA: {decoded.Vstr}");
                Assert.That(decoded.Vstr, Is.EqualTo("Hello, Baboon!"));
                Assert.That(decoded.Vi32, Is.EqualTo(123456));
                Assert.That(decoded.Vbit, Is.True);
            }
        }

        [Test]
        public void CSharp_UEBA_Deserialization_Should_Read_CSharp_Generated_UEBA()
        {
            var uebaBytes = File.ReadAllBytes(csUebaFile);
            using (var memoryStream = new MemoryStream(uebaBytes))
            using (var reader = new BinaryReader(memoryStream))
            {
                var ctx = BaboonCodecContext.Default;
                var decoded = AllBasicTypes_UEBACodec.Instance.Decode(ctx, reader);

                Console.WriteLine($"Successfully decoded C# UEBA: {decoded.Vstr}");
                Assert.That(decoded.Vstr, Is.EqualTo("Hello, Baboon!"));
                Assert.That(decoded.Vi32, Is.EqualTo(123456));
                Assert.That(decoded.Vbit, Is.True);
            }
        }

        [Test]
        public void CrossLanguage_Comparison_Should_Verify_Scala_And_CSharp_JSON_Produce_Equivalent_Data()
        {
            var scalaJsonStr = File.ReadAllText(scalaJsonFile, Encoding.UTF8);
            JToken scalaJson;
            using (var reader = new JsonTextReader(new StringReader(scalaJsonStr)) { DateParseHandling = DateParseHandling.None })
            {
                scalaJson = JToken.Load(reader);
            }

            var csJsonStr = File.ReadAllText(csJsonFile, Encoding.UTF8);
            JToken csJson;
            using (var reader = new JsonTextReader(new StringReader(csJsonStr)) { DateParseHandling = DateParseHandling.None })
            {
                csJson = JToken.Load(reader);
            }

            var ctx = BaboonCodecContext.Default;
            var scalaDecoded = AllBasicTypes_JsonCodec.Instance.Decode(ctx, scalaJson);
            var csDecoded = AllBasicTypes_JsonCodec.Instance.Decode(ctx, csJson);

            Console.WriteLine("Comparing Scala and C# JSON data:");
            Console.WriteLine($"  Scala: vi8={scalaDecoded.Vi8}, vi16={scalaDecoded.Vi16}, vi32={scalaDecoded.Vi32}, vi64={scalaDecoded.Vi64}");
            Console.WriteLine($"  C#:    vi8={csDecoded.Vi8}, vi16={csDecoded.Vi16}, vi32={csDecoded.Vi32}, vi64={csDecoded.Vi64}");
            Console.WriteLine($"  Scala: vf32={scalaDecoded.Vf32}, vf64={scalaDecoded.Vf64}, vf128={scalaDecoded.Vf128}");
            Console.WriteLine($"  C#:    vf32={csDecoded.Vf32}, vf64={csDecoded.Vf64}, vf128={csDecoded.Vf128}");
            Console.WriteLine($"  Scala: vtsu={scalaDecoded.Vtsu}, vtso={scalaDecoded.Vtso}");
            Console.WriteLine($"  C#:    vtsu={csDecoded.Vtsu}, vtso={csDecoded.Vtso}");

            Assert.That(csDecoded, Is.EqualTo(scalaDecoded), "Scala and C# JSON data should be equal");
        }

        [Test]
        public void CrossLanguage_Comparison_Should_Verify_Scala_And_CSharp_UEBA_Produce_Equivalent_Data()
        {
            var scalaUebaBytes = File.ReadAllBytes(scalaUebaFile);
            AllBasicTypes scalaDecoded;
            using (var memoryStream = new MemoryStream(scalaUebaBytes))
            using (var reader = new BinaryReader(memoryStream))
            {
                var ctx = BaboonCodecContext.Default;
                scalaDecoded = AllBasicTypes_UEBACodec.Instance.Decode(ctx, reader);
            }

            var csUebaBytes = File.ReadAllBytes(csUebaFile);
            AllBasicTypes csDecoded;
            using (var memoryStream = new MemoryStream(csUebaBytes))
            using (var reader = new BinaryReader(memoryStream))
            {
                var ctx = BaboonCodecContext.Default;
                csDecoded = AllBasicTypes_UEBACodec.Instance.Decode(ctx, reader);
            }

            Console.WriteLine("Comparing Scala and C# UEBA data:");
            Console.WriteLine($"  Scala: vi8={scalaDecoded.Vi8}, vi16={scalaDecoded.Vi16}, vi32={scalaDecoded.Vi32}, vi64={scalaDecoded.Vi64}");
            Console.WriteLine($"  C#:    vi8={csDecoded.Vi8}, vi16={csDecoded.Vi16}, vi32={csDecoded.Vi32}, vi64={csDecoded.Vi64}");
            Console.WriteLine($"  Scala: vf32={scalaDecoded.Vf32}, vf64={scalaDecoded.Vf64}, vf128={scalaDecoded.Vf128}");
            Console.WriteLine($"  C#:    vf32={csDecoded.Vf32}, vf64={csDecoded.Vf64}, vf128={csDecoded.Vf128}");
            Console.WriteLine($"  Scala: vtsu={scalaDecoded.Vtsu}, vtso={scalaDecoded.Vtso}");
            Console.WriteLine($"  C#:    vtsu={csDecoded.Vtsu}, vtso={csDecoded.Vtso}");

            Assert.That(csDecoded, Is.EqualTo(scalaDecoded), "Scala and C# UEBA data should be equal");
        }
    }
}
