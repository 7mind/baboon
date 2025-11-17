using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Text;
using Convtest.Testpkg;
using Baboon.Runtime.Shared;
using Baboon.Time;
using Newtonsoft.Json;

namespace ConvTest
{
    public static class CompatMain
    {
        public static void Main(string[] args)
        {
            // Create sample data with all basic types
            var sampleData = CreateSampleData();

            // Create output directories - use absolute path relative to project root
            var baseDir = Path.GetFullPath(Path.Combine("..", "..", "target", "compat-test"));
            var csJsonDir = Path.Combine(baseDir, "cs-json");
            var csUebaDir = Path.Combine(baseDir, "cs-ueba");

            Directory.CreateDirectory(csJsonDir);
            Directory.CreateDirectory(csUebaDir);

            // Serialize to JSON
            var ctx = BaboonCodecContext.Default;
            var jsonCodec = AllBasicTypes_JsonCodec.Instance;
            var jsonToken = jsonCodec.Encode(ctx, sampleData);
            // Use DateTimeZoneHandling.Utc to preserve timezone info in the output
            var settings = new JsonSerializerSettings { DateTimeZoneHandling = DateTimeZoneHandling.Utc };
            var jsonStr = JsonConvert.SerializeObject(jsonToken, Formatting.Indented, settings);
            var jsonPath = Path.Combine(csJsonDir, "all-basic-types.json");
            // Use UTF8 without BOM to ensure compatibility with other parsers
            File.WriteAllText(jsonPath, jsonStr, new UTF8Encoding(false));
            Console.WriteLine($"Written JSON to {jsonPath}");

            // Serialize to UEBA
            var uebaCodec = AllBasicTypes_UEBACodec.Instance;
            byte[] uebaBytes;
            using (var memoryStream = new MemoryStream())
            {
                using (var binaryWriter = new BinaryWriter(memoryStream))
                {
                    uebaCodec.Encode(ctx, binaryWriter, sampleData);
                }
                memoryStream.Flush();
                uebaBytes = memoryStream.ToArray();
            }
            var uebaPath = Path.Combine(csUebaDir, "all-basic-types.ueba");
            File.WriteAllBytes(uebaPath, uebaBytes);
            Console.WriteLine($"Written UEBA to {uebaPath}");

            Console.WriteLine("C# serialization complete!");
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
                Vbstr: new ByteString(new byte[] { 0x48, 0x65, 0x6C, 0x6C, 0x6F, 0x20, 0x42, 0x79, 0x74, 0x65, 0x73 }), // "Hello Bytes"
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
