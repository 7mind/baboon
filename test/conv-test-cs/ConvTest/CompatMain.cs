using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Text;
using Convtest.Testpkg;
using Baboon.Runtime.Shared;
using Baboon.Time;
using Newtonsoft.Json;
using Newtonsoft.Json.Linq;

namespace ConvTest
{
    public static class CompatMain
    {
        public static void Main(string[] args)
        {
            if (args.Length >= 1 && args[0] == "write")
            {
                var outputDir = args[1];
                var format = args[2];
                Directory.CreateDirectory(outputDir);
                var sampleData = CreateSampleData();
                var ctx = BaboonCodecContext.Default;

                if (format == "json")
                {
                    WriteJson(ctx, sampleData, outputDir);
                }
                else if (format == "ueba")
                {
                    WriteUeba(ctx, sampleData, outputDir);
                }
                else
                {
                    Console.Error.WriteLine($"Unknown format: {format}");
                    Environment.Exit(1);
                }
            }
            else if (args.Length >= 1 && args[0] == "read")
            {
                var filePath = args[1];
                ReadAndVerify(filePath);
            }
            else
            {
                RunLegacy();
            }
        }

        private static void RunLegacy()
        {
            var sampleData = CreateSampleData();

            var baseDir = Path.GetFullPath(Path.Combine("..", "..", "target", "compat-test"));
            var csJsonDir = Path.Combine(baseDir, "cs-json");
            var csUebaDir = Path.Combine(baseDir, "cs-ueba");

            Directory.CreateDirectory(csJsonDir);
            Directory.CreateDirectory(csUebaDir);

            var ctx = BaboonCodecContext.Default;
            WriteJson(ctx, sampleData, csJsonDir);
            WriteUeba(ctx, sampleData, csUebaDir);

            Console.WriteLine("C# serialization complete!");
        }

        private static void WriteJson(BaboonCodecContext ctx, AllBasicTypes data, string outputDir)
        {
            var jsonCodec = AllBasicTypes_JsonCodec.Instance;
            var jsonToken = jsonCodec.Encode(ctx, data);
            var settings = new JsonSerializerSettings { DateTimeZoneHandling = DateTimeZoneHandling.Utc };
            var jsonStr = JsonConvert.SerializeObject(jsonToken, Formatting.Indented, settings);
            var jsonPath = Path.Combine(outputDir, "all-basic-types.json");
            File.WriteAllText(jsonPath, jsonStr, new UTF8Encoding(false));
            Console.WriteLine($"Written JSON to {jsonPath}");
        }

        private static void WriteUeba(BaboonCodecContext ctx, AllBasicTypes data, string outputDir)
        {
            var uebaCodec = AllBasicTypes_UEBACodec.Instance;
            byte[] uebaBytes;
            using (var memoryStream = new MemoryStream())
            {
                using (var binaryWriter = new BinaryWriter(memoryStream))
                {
                    uebaCodec.Encode(ctx, binaryWriter, data);
                }
                memoryStream.Flush();
                uebaBytes = memoryStream.ToArray();
            }
            var uebaPath = Path.Combine(outputDir, "all-basic-types.ueba");
            File.WriteAllBytes(uebaPath, uebaBytes);
            Console.WriteLine($"Written UEBA to {uebaPath}");
        }

        private static void ReadAndVerify(string filePath)
        {
            var ctx = BaboonCodecContext.Default;
            AllBasicTypes data;

            try
            {
                if (filePath.EndsWith(".json"))
                {
                    var jsonStr = File.ReadAllText(filePath, Encoding.UTF8);
                    JToken jsonToken;
                    using (var reader = new JsonTextReader(new System.IO.StringReader(jsonStr)))
                    {
                        reader.DateParseHandling = DateParseHandling.None;
                        jsonToken = JToken.Load(reader);
                    }
                    data = AllBasicTypes_JsonCodec.Instance.Decode(ctx, jsonToken);
                }
                else if (filePath.EndsWith(".ueba"))
                {
                    var bytes = File.ReadAllBytes(filePath);
                    using var memoryStream = new MemoryStream(bytes);
                    using var reader = new BinaryReader(memoryStream);
                    data = AllBasicTypes_UEBACodec.Instance.Decode(ctx, reader);
                }
                else
                {
                    Console.Error.WriteLine($"Unknown file extension: {filePath}");
                    Environment.Exit(1);
                    return;
                }
            }
            catch (Exception e)
            {
                Console.Error.WriteLine($"Deserialization failed: {e.Message}");
                Environment.Exit(1);
                return;
            }

            if (data.Vstr != "Hello, Baboon!")
            {
                Console.Error.WriteLine($"vstr mismatch: expected 'Hello, Baboon!', got '{data.Vstr}'");
                Environment.Exit(1);
            }
            if (data.Vi32 != 123456)
            {
                Console.Error.WriteLine($"vi32 mismatch: expected 123456, got {data.Vi32}");
                Environment.Exit(1);
            }
            if (!data.Vbit)
            {
                Console.Error.WriteLine($"vbit mismatch: expected true, got {data.Vbit}");
                Environment.Exit(1);
            }

            // Roundtrip: re-encode, re-decode, compare
            try
            {
                if (filePath.EndsWith(".json"))
                {
                    var reEncoded = AllBasicTypes_JsonCodec.Instance.Encode(ctx, data);
                    var reDecoded = AllBasicTypes_JsonCodec.Instance.Decode(ctx, reEncoded);
                    if (!data.Equals(reDecoded))
                    {
                        Console.Error.WriteLine("JSON roundtrip mismatch");
                        Environment.Exit(1);
                    }
                }
                else
                {
                    using var ms = new MemoryStream();
                    using (var bw = new BinaryWriter(ms))
                    {
                        AllBasicTypes_UEBACodec.Instance.Encode(ctx, bw, data);
                    }
                    var reBytes = ms.ToArray();
                    using var ms2 = new MemoryStream(reBytes);
                    using var br2 = new BinaryReader(ms2);
                    var reDecoded = AllBasicTypes_UEBACodec.Instance.Decode(ctx, br2);
                    if (!data.Equals(reDecoded))
                    {
                        Console.Error.WriteLine("UEBA roundtrip mismatch");
                        Environment.Exit(1);
                    }
                }
            }
            catch (Exception e)
            {
                Console.Error.WriteLine($"Roundtrip failed: {e.Message}");
                Environment.Exit(1);
            }

            Console.WriteLine("OK");
        }

        private static AllBasicTypes CreateSampleData()
        {
            return new AllBasicTypes(
                Vi8: (SByte)42,
                Vi16: (Int16)1234,
                Vi32: 123456,
                Vi64: 123456789L,

                Vu8: (Byte)200,
                Vu16: (UInt16)50000,
                Vu32: 3000000000U,
                Vu64: 10000000000UL,

                Vf32: 3.14159f,
                Vf64: 2.718281828,
                Vf128: 123456789.987654321m,

                Vstr: "Hello, Baboon!",
                Vbstr: new ByteString(new byte[] { 0x48, 0x65, 0x6C, 0x6C, 0x6F, 0x20, 0x42, 0x79, 0x74, 0x65, 0x73 }),
                Vuid: Guid.Parse("12345678-1234-5678-1234-567812345678"),

                Vbit: true,

                Vtsu: new RpDateTime(new DateTimeOffset(2024, 6, 15, 12, 30, 45, 123, TimeSpan.Zero), DateTimeKind.Utc),
                Vtso: new RpDateTime(new DateTimeOffset(2024, 6, 15, 14, 30, 45, 987, TimeSpan.FromHours(2))),

                VoptStr: "optional value",
                VlstI32: new List<Int32> { 1, 2, 3, 4, 5 },
                VsetStr: ImmutableHashSet.Create("apple", "banana", "cherry"),
                VmapStrI32: new Dictionary<String, Int32>
                {
                    { "one", 1 },
                    { "two", 2 },
                    { "three", 3 }
                }.ToImmutableDictionary(),

                VoptLst: new List<String> { "nested", "list", "values" },
                VlstOpt: new List<Int32?>
                {
                    10,
                    null,
                    20,
                    30
                },
                VmapLst: new Dictionary<String, IReadOnlyList<Int64>>
                {
                    { "numbers", new List<Int64> { 1L, 2L, 3L } },
                    { "more", new List<Int64> { 4L, 5L, 6L } }
                }.ToImmutableDictionary()
            );
        }
    }
}
