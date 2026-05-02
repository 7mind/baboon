using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Linq;
using System.Text;
using Convtest.Testpkg;
// PR-I.1c (M24 Phase 3.1) — Custom-foreign KeyCodec hook fixture. Stringy
// foreign FStr maps to System.String; the default identity FStr_KeyCodec
// impl handles encode/decode of map keys without host registration.
using Convtest.M24foreign;
// PR-26.5 (M26) — non-string builtin map-key cross-language fixture.
using Convtest.M26builtinkeys;
using Baboon.Runtime.Shared;
using Baboon.Time;
using Newtonsoft.Json;
using Newtonsoft.Json.Linq;

namespace ConvTest
{
    public static class CompatMain
    {
        // Domain constants — match the convtest.testpkg domain at version 2.0.0 (where AnyShowcase + InnerPayload live).
        private const string DomainId = "convtest.testpkg";
        private const string DomainVer = "2.0.0";
        private const string InnerTypeId = "convtest.testpkg/:#InnerPayload";

        public static void Main(string[] args)
        {
            if (args.Length >= 1 && args[0] == "write")
            {
                var outputDir = args[1];
                var format = args[2];
                Directory.CreateDirectory(outputDir);
                var sampleData = CreateSampleData();
                var sampleAny = CreateSampleAnyShowcase();
                var ctx = BaboonCodecContext.Default;
                var facadeCtx = BaboonCodecContext.WithFacade(useIndices: false, FreshFacade());

                if (format == "json")
                {
                    WriteJson(ctx, sampleData, outputDir);
                    WriteJsonAny(facadeCtx, sampleAny, outputDir);
                }
                else if (format == "ueba")
                {
                    WriteUeba(ctx, sampleData, outputDir);
                    WriteUebaAny(facadeCtx, sampleAny, outputDir);
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
            var sampleAny = CreateSampleAnyShowcase();

            var baseDir = Path.GetFullPath(Path.Combine("..", "..", "target", "compat-test"));
            var csJsonDir = Path.Combine(baseDir, "cs-json");
            var csUebaDir = Path.Combine(baseDir, "cs-ueba");
            var csReprDir = Path.Combine(baseDir, "cs-repr");

            Directory.CreateDirectory(csJsonDir);
            Directory.CreateDirectory(csUebaDir);
            Directory.CreateDirectory(csReprDir);

            var ctx = BaboonCodecContext.Default;
            var facadeCtx = BaboonCodecContext.WithFacade(useIndices: false, FreshFacade());
            WriteJson(ctx, sampleData, csJsonDir);
            WriteUeba(ctx, sampleData, csUebaDir);
            WriteJsonAny(facadeCtx, sampleAny, csJsonDir);
            WriteUebaAny(facadeCtx, sampleAny, csUebaDir);
            WritePointIdRepr(sampleData.VPointId, csReprDir);
            WriteForeignKeyHolderJson(ctx, CreateForeignKeyHolderSample(), csJsonDir);
            WriteBuiltinMapKeyHolderJson(ctx, CreateBuiltinMapKeyHolderSample(), csJsonDir);
            WriteBuiltinMapKeyHolderUeba(ctx, CreateBuiltinMapKeyHolderSample(), csUebaDir);

            Console.WriteLine("C# serialization complete!");
        }

        // PR-26.5 (M26) — non-string builtin map-key cross-language fixture.
        // Closes PR-G-D01.
        private static BuiltinMapKeyHolder CreateBuiltinMapKeyHolderSample()
        {
            var mi32 = new Dictionary<int, string> { { 42, "v32" } };
            var mi64 = new Dictionary<long, string> { { 9223372036854775807L, "vmax" } };
            var mu32 = new Dictionary<uint, string> { { 7u, "vu32" } };
            var mbit = new Dictionary<bool, string> { { true, "vt" } };
            var muid = new Dictionary<Guid, string>
            {
                { Guid.Parse("00000000-0000-0000-0000-000000000001"), "vid" },
            };
            return new BuiltinMapKeyHolder(
                mi32.ToImmutableDictionary(),
                mi64.ToImmutableDictionary(),
                mu32.ToImmutableDictionary(),
                mbit.ToImmutableDictionary(),
                muid.ToImmutableDictionary());
        }

        private static void WriteBuiltinMapKeyHolderJson(BaboonCodecContext ctx, BuiltinMapKeyHolder data, string outputDir)
        {
            var json = BuiltinMapKeyHolder_JsonCodec.Instance.Encode(ctx, data);
            // Compact form so the cross-language byte-identity assertion compares
            // against the canonical reference JSON literally.
            var jsonStr = JsonConvert.SerializeObject(json, Formatting.None);
            var path = Path.Combine(outputDir, "m26-builtin-map-keys.json");
            File.WriteAllText(path, jsonStr, new UTF8Encoding(false));
            Console.WriteLine($"Written JSON to {path}");
        }

        private static void WriteBuiltinMapKeyHolderUeba(BaboonCodecContext ctx, BuiltinMapKeyHolder data, string outputDir)
        {
            byte[] bytes;
            using (var ms = new MemoryStream())
            {
                using (var bw = new BinaryWriter(ms))
                {
                    BuiltinMapKeyHolder_UEBACodec.Instance.Encode(ctx, bw, data);
                }
                ms.Flush();
                bytes = ms.ToArray();
            }
            var path = Path.Combine(outputDir, "m26-builtin-map-keys.ueba");
            File.WriteAllBytes(path, bytes);
            Console.WriteLine($"Written UEBA to {path}");
        }

        // PR-I.1c (M24 Phase 3.1) — Custom-foreign KeyCodec hook canonical fixture.
        // The map keys go through FStr_KeyCodec (default identity impl for the stringy
        // foreign), so the wire form is `{"m":{"alpha":"v1","beta":"v2"}}`.
        private static ForeignKeyHolder CreateForeignKeyHolderSample()
        {
            var m = new Dictionary<ItemKey, String>
            {
                { new ItemKey("alpha"), "v1" },
                { new ItemKey("beta"), "v2" },
            };
            return new ForeignKeyHolder(m.ToImmutableDictionary());
        }

        private static void WriteForeignKeyHolderJson(BaboonCodecContext ctx, ForeignKeyHolder data, string outputDir)
        {
            var json = ForeignKeyHolder_JsonCodec.Instance.Encode(ctx, data);
            // Compact (no Indented) so the byte-identity assertion against the canonical
            // wire form `{"m":{"alpha":"v1","beta":"v2"}}` matches across backends.
            var jsonStr = JsonConvert.SerializeObject(json, Formatting.None);
            var path = Path.Combine(outputDir, "m24-foreign-keycodec.json");
            File.WriteAllText(path, jsonStr, new UTF8Encoding(false));
            Console.WriteLine($"Written JSON to {path}");
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

        // PR-57e (M18.4e) — cross-language identifier repr (toString) byte-identity.
        // Per spec §7 the repr/toString form is a separate invariant from the JSON/UEBA wire bytes;
        // we write it as a per-language artifact so the Scala-side test can assert all 10 backends
        // produce byte-identical output for the same canonical PointId value.
        private static void WritePointIdRepr(PointId pid, string outputDir)
        {
            var reprPath = Path.Combine(outputDir, "point-id.txt");
            // No trailing newline — exact byte match across all languages.
            File.WriteAllText(reprPath, pid.ToString(), new UTF8Encoding(false));
            Console.WriteLine($"Written repr to {reprPath}");
        }

        private static void WriteJsonAny(BaboonCodecContext ctx, AnyShowcase data, string outputDir)
        {
            var jsonToken = AnyShowcase_JsonCodec.Instance.Encode(ctx, data);
            var settings = new JsonSerializerSettings { DateTimeZoneHandling = DateTimeZoneHandling.Utc };
            var jsonStr = JsonConvert.SerializeObject(jsonToken, Formatting.Indented, settings);
            var jsonPath = Path.Combine(outputDir, "any-showcase.json");
            File.WriteAllText(jsonPath, jsonStr, new UTF8Encoding(false));
            Console.WriteLine($"Written JSON to {jsonPath}");
        }

        private static void WriteUebaAny(BaboonCodecContext ctx, AnyShowcase data, string outputDir)
        {
            byte[] bytes;
            using (var ms = new MemoryStream())
            {
                using (var bw = new BinaryWriter(ms))
                {
                    AnyShowcase_UEBACodec.Instance.Encode(ctx, bw, data);
                }
                ms.Flush();
                bytes = ms.ToArray();
            }
            var path = Path.Combine(outputDir, "any-showcase.ueba");
            File.WriteAllBytes(path, bytes);
            Console.WriteLine($"Written UEBA to {path}");
        }

        private static void ReadAndVerify(string filePath)
        {
            if (filePath.EndsWith("any-showcase.json") || filePath.EndsWith("any-showcase.ueba"))
            {
                ReadAndVerifyAnyShowcase(filePath);
                return;
            }
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

        private static void ReadAndVerifyAnyShowcase(string filePath)
        {
            // Decode-side does not require a facade (see Scala CompatMain comment).
            var ctx = BaboonCodecContext.Default;
            AnyShowcase data;
            try
            {
                if (filePath.EndsWith(".json"))
                {
                    var jsonStr = File.ReadAllText(filePath, Encoding.UTF8);
                    JToken jsonToken;
                    using (var reader = new JsonTextReader(new StringReader(jsonStr)))
                    {
                        reader.DateParseHandling = DateParseHandling.None;
                        jsonToken = JToken.Load(reader);
                    }
                    data = AnyShowcase_JsonCodec.Instance.Decode(ctx, jsonToken);
                }
                else
                {
                    var bytes = File.ReadAllBytes(filePath);
                    using var ms = new MemoryStream(bytes);
                    using var br = new BinaryReader(ms);
                    data = AnyShowcase_UEBACodec.Instance.Decode(ctx, br);
                }
            }
            catch (Exception e)
            {
                Console.Error.WriteLine($"AnyShowcase deserialization failed: {e.Message}");
                Environment.Exit(1);
                return;
            }

            var expected = ExpectedInnerPayloads();
            var decoded = DecodeAllPayloads(data);
            for (var i = 0; i < expected.Count; i++)
            {
                if (!expected[i].Equals(decoded[i]))
                {
                    Console.Error.WriteLine($"AnyShowcase payload {i} mismatch: expected {expected[i]}, got {decoded[i]}");
                    Environment.Exit(1);
                }
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
                }.ToImmutableDictionary(),

                // Non-Pascal-case enum member; canonical JSON wire form is "Cafe" (PR-35-D06 regression guard).
                VWireEnum: WireEnum.Cafe,

                // Identifier (PR-57e). Wire form is `{"x": 42, "y": -7}` on JSON and two
                // i32 LE values on UEBA — byte-identical to a `data` of the same shape
                // per docs/spec/identifier-repr.md §1.3 / §7.
                VPointId: new PointId(X: 42, Y: -7),
                // PR-61 (M19.3) — id types as JSON map keys. Per PR-60 (M19.2) all id
                // types — single- or multi-field — use canonical repr ToString as the
                // key form: e.g. `ItemId:2.0.0#v:00000000-0000-0000-0000-000000000001`.
                // Canonical deterministic uuids ensure cross-language byte-identity.
                VmapItemIdU32: new Dictionary<ItemId, UInt32>
                {
                    { new ItemId(V: Guid.Parse("00000000-0000-0000-0000-000000000001")), 1u },
                    { new ItemId(V: Guid.Parse("00000000-0000-0000-0000-000000000002")), 2u },
                }.ToImmutableDictionary(),
                VmapCompositeIdU32: new Dictionary<CompositeId, UInt32>
                {
                    {
                        new CompositeId(
                            Tenant: Guid.Parse("00000000-0000-0000-0000-0000000000aa"),
                            User:   Guid.Parse("00000000-0000-0000-0000-0000000000bb")
                        ),
                        100u
                    },
                    {
                        new CompositeId(
                            Tenant: Guid.Parse("00000000-0000-0000-0000-0000000000cc"),
                            User:   Guid.Parse("00000000-0000-0000-0000-0000000000dd")
                        ),
                        200u
                    },
                }.ToImmutableDictionary()
            );
        }

        // Expected logical InnerPayload contents per AnyShowcase slot, in deterministic order:
        // [vAnyA, vAnyB, vAnyC, vAnyD1, vAnyD2, vAnyD3, optAny, lstAny[0]].
        // Must match the Scala fixture exactly so cross-language reads produce the same payloads.
        private static IReadOnlyList<InnerPayload> ExpectedInnerPayloads()
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

        // Decode every AnyOpaque slot directly via InnerPayload_*Codec — facade.DecodeAny is not
        // used because partial-meta variants (B/C/D1/D2/D3) lack the full triple required for
        // facade resolution. The wire format guarantees the inner payload is decodable; that's
        // what the cross-language test asserts.
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

        // A facade with the convtest.testpkg/2.0.0 codecs registered so cross-format conversions can
        // resolve InnerPayload via (domain, version, typeid). The 3-arg Register overload skips the
        // conversions/meta wires which the cross-format helpers do not need.
        private static BaboonCodecsFacade FreshFacade()
        {
            var f = new BaboonCodecsFacade();
            f.Register(
                new BaboonDomainVersion(DomainId, DomainVer),
                () => Convtest.Testpkg.BaboonCodecsJson.Instance,
                () => Convtest.Testpkg.BaboonCodecsUeba.Instance);
            return f;
        }

        // Build the canonical AnyShowcase fixture. Layout MUST mirror Scala's createSampleAnyShowcase
        // so cross-language reads observe identical payloads.
        private static AnyShowcase CreateSampleAnyShowcase()
        {
            var payloads = ExpectedInnerPayloads();
            var a = payloads[0]; var b = payloads[1]; var c = payloads[2];
            var d1 = payloads[3]; var d2 = payloads[4]; var d3 = payloads[5];
            var optP = payloads[6]; var lstP = payloads[7];

            byte[] UebaBytes(InnerPayload p)
            {
                using var ms = new MemoryStream();
                using (var bw = new BinaryWriter(ms))
                {
                    InnerPayload_UEBACodec.Instance.Encode(BaboonCodecContext.Compact, bw, p);
                }
                ms.Flush();
                return ms.ToArray();
            }
            JToken AsJson(InnerPayload p) => InnerPayload_JsonCodec.Instance.Encode(BaboonCodecContext.Compact, p);

            // Untyped variants A/B/C: AnyOpaqueJson with full wire meta (typeid present so cross-
            // format encoder can resolve InnerPayload from the wire alone).
            var metaA = new AnyMeta(0x07, DomainId, DomainVer, InnerTypeId);
            var metaB = new AnyMeta(0x03, null, DomainVer, InnerTypeId);
            var metaC = new AnyMeta(0x01, null, null, InnerTypeId);

            // Typed variants D1/D2/D3: meta omits whatever the kind byte says is absent; statics fill in.
            var metaD1 = new AnyMeta(0x06, DomainId, DomainVer, null);
            var metaD2 = new AnyMeta(0x02, null, DomainVer, null);
            var metaD3 = new AnyMeta(0x00, null, null, null);

            // Mix branches: A/B/C as JSON, D1/D2/D3 as UEBA, opt as JSON, lst element as UEBA.
            // Forces cross-format conversion in both directions through the facade for both wire formats.
            return new AnyShowcase(
                VAnyA: new AnyOpaqueJson(metaA, AsJson(a)),
                VAnyB: new AnyOpaqueJson(metaB, AsJson(b)),
                VAnyC: new AnyOpaqueJson(metaC, AsJson(c)),
                VAnyD1: new AnyOpaqueUeba(metaD1, UebaBytes(d1)),
                VAnyD2: new AnyOpaqueUeba(metaD2, UebaBytes(d2)),
                VAnyD3: new AnyOpaqueUeba(metaD3, UebaBytes(d3)),
                OptAny: new AnyOpaqueJson(new AnyMeta(0x07, DomainId, DomainVer, InnerTypeId), AsJson(optP)),
                LstAny: new List<AnyOpaque>
                {
                    new AnyOpaqueUeba(new AnyMeta(0x06, DomainId, DomainVer, null), UebaBytes(lstP)),
                });
        }
    }
}
