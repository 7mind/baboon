// NOTE: This test references generated runtime symbols (AnyMeta, AnyMetaCodec,
// BaboonCodecsFacade, BaboonCodecException, ...) which are copied into this stub
// only by the cs-stub codegen path (rsync + codegen into target/test-regular/cs-stub/).
// Running `dotnet test` directly from the source tree may fail with missing symbols;
// run the test suite from the codegen'd copy.
#nullable enable

using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using Baboon.Runtime.Shared;
using Newtonsoft.Json.Linq;
using NUnit.Framework;

namespace ConversionsTest
{
    [TestFixture]
    public class AnyMetaCodecTests
    {
        // (kind, domain?, version?, typeid?) for all six locked meta-kind bytes.
        private static readonly (byte Kind, string? Domain, string? Version, string? Typeid)[] Cases =
        {
            (0x07, "com.example.dom", "1.2.3", "MyType"), // A
            (0x03, null, "1.2.3", "MyType"),               // B
            (0x01, null, null, "MyType"),                  // C
            (0x06, "com.example.dom", "1.2.3", null),      // D1
            (0x02, null, "1.2.3", null),                   // D2
            (0x00, null, null, null),                      // D3
        };

        // ===== AnyMeta construction invariants =====

        [Test]
        public void AnyMeta_kind0x07_RequiresDomainPresent()
        {
            Assert.Throws<ArgumentException>(() => new AnyMeta(0x07, null, "v", "t"));
        }

        [Test]
        public void AnyMeta_kind0x03_RequiresDomainAbsent()
        {
            Assert.Throws<ArgumentException>(() => new AnyMeta(0x03, "d", "v", "t"));
        }

        [Test]
        public void AnyMeta_kind0x06_RequiresTypeidAbsent()
        {
            Assert.Throws<ArgumentException>(() => new AnyMeta(0x06, "d", "v", "t"));
        }

        [Test]
        public void AnyMeta_RejectsReservedKind0x04()
        {
            Assert.Throws<ArgumentException>(() => new AnyMeta(0x04, "d", null, null));
        }

        [Test]
        public void AnyMeta_RejectsReservedKind0x05()
        {
            Assert.Throws<ArgumentException>(() => new AnyMeta(0x05, "d", null, "t"));
        }

        // ===== Binary round-trips =====

        [Test]
        public void AnyMetaCodec_WriteBin_ReadBin_RoundTripsAcrossAllSixKinds()
        {
            foreach (var (kind, domain, version, typeid) in Cases)
            {
                var meta = new AnyMeta(kind, domain, version, typeid);
                using var ms = new MemoryStream();
                using var writer = new BinaryWriter(ms);
                AnyMetaCodec.WriteBin(meta, writer);
                writer.Flush();

                using var rms = new MemoryStream(ms.ToArray());
                using var reader = new BinaryReader(rms);
                var round = AnyMetaCodec.ReadBin(reader);
                Assert.That(round, Is.EqualTo(meta), $"binary round-trip failed for kind 0x{kind:x}");
            }
        }

        [Test]
        public void AnyMetaCodec_WriteBin_Kind0x07_Emits7Bytes()
        {
            // 1 byte kind + (1 byte ULEB128 length + 1 byte UTF-8) × 3 = 7 bytes.
            var meta = new AnyMeta(0x07, "a", "b", "c");
            using var ms = new MemoryStream();
            using var writer = new BinaryWriter(ms);
            AnyMetaCodec.WriteBin(meta, writer);
            writer.Flush();
            Assert.That(ms.ToArray().Length, Is.EqualTo(7));
        }

        [Test]
        public void AnyMetaCodec_WriteBin_Kind0x00_EmitsOneByte()
        {
            var meta = new AnyMeta(0x00, null, null, null);
            using var ms = new MemoryStream();
            using var writer = new BinaryWriter(ms);
            AnyMetaCodec.WriteBin(meta, writer);
            writer.Flush();
            var bytes = ms.ToArray();
            Assert.That(bytes.Length, Is.EqualTo(1));
            Assert.That(bytes[0], Is.EqualTo((byte)0x00));
        }

        [Test]
        public void AnyMetaCodec_WriteBin_NonAsciiUtf8_RoundTrips()
        {
            var meta = new AnyMeta(0x07, "日本語", "1.2.3", "タイプ");
            using var ms = new MemoryStream();
            using var writer = new BinaryWriter(ms);
            AnyMetaCodec.WriteBin(meta, writer);
            writer.Flush();
            using var rms = new MemoryStream(ms.ToArray());
            using var reader = new BinaryReader(rms);
            var round = AnyMetaCodec.ReadBin(reader);
            Assert.That(round, Is.EqualTo(meta));
        }

        [Test]
        public void AnyMetaCodec_WriteBin_EmptyString_RoundTrips()
        {
            var meta = new AnyMeta(0x01, null, null, "");
            using var ms = new MemoryStream();
            using var writer = new BinaryWriter(ms);
            AnyMetaCodec.WriteBin(meta, writer);
            writer.Flush();
            var bytes = ms.ToArray();
            // 1 (kind) + 1 (ULEB128 length 0) = 2 bytes
            Assert.That(bytes.Length, Is.EqualTo(2));
            Assert.That(bytes[0], Is.EqualTo((byte)0x01));
            Assert.That(bytes[1], Is.EqualTo((byte)0x00));

            using var rms = new MemoryStream(bytes);
            using var reader = new BinaryReader(rms);
            var round = AnyMetaCodec.ReadBin(reader);
            Assert.That(round, Is.EqualTo(meta));
        }

        [Test]
        public void AnyMetaCodec_WriteBin_128ByteString_RoundTripsWithMultiByteUleb()
        {
            // 128 = 0x80 0x01 in ULEB128 (multi-byte length prefix).
            var longStr = new string('a', 128);
            var meta = new AnyMeta(0x01, null, null, longStr);
            using var ms = new MemoryStream();
            using var writer = new BinaryWriter(ms);
            AnyMetaCodec.WriteBin(meta, writer);
            writer.Flush();
            var bytes = ms.ToArray();
            // 1 (kind) + 2 (ULEB128) + 128 (UTF-8) = 131
            Assert.That(bytes.Length, Is.EqualTo(131));
            Assert.That(bytes[0], Is.EqualTo((byte)0x01));
            Assert.That(bytes[1], Is.EqualTo((byte)0x80));
            Assert.That(bytes[2], Is.EqualTo((byte)0x01));

            using var rms = new MemoryStream(bytes);
            using var reader = new BinaryReader(rms);
            var round = AnyMetaCodec.ReadBin(reader);
            Assert.That(round, Is.EqualTo(meta));
        }

        [Test]
        public void AnyMetaCodec_ReadBinWithLength_ReportsBytesConsumedAndTolersTrailingMetaExtensionBytes()
        {
            // Write a valid kind 0x07 meta, then append 5 garbage bytes that the on-wire
            // meta-length window would claim are part of the meta block.
            var meta = new AnyMeta(0x07, "d", "v", "t");
            using var ms = new MemoryStream();
            using var writer = new BinaryWriter(ms);
            AnyMetaCodec.WriteBin(meta, writer);
            writer.Flush();
            var metaBytes = ms.ToArray();
            var tail = new byte[] { 0x11, 0x22, 0x33, 0x44, 0x55 };
            var combined = metaBytes.Concat(tail).ToArray();

            using var rms = new MemoryStream(combined);
            using var reader = new BinaryReader(rms);
            var (parsed, bytesRead) = AnyMetaCodec.ReadBinWithLength(reader);
            Assert.That(parsed, Is.EqualTo(meta));
            Assert.That(bytesRead, Is.EqualTo(metaBytes.Length));

            // Caller would skip (anyMetaLen - bytesRead) bytes, which equals tail.Length here.
            var anyMetaLen = metaBytes.Length + tail.Length;
            var skipBytes = anyMetaLen - bytesRead;
            var skipped = reader.ReadBytes(skipBytes);
            Assert.That(skipped.Length, Is.EqualTo(tail.Length));
        }

        // ===== JSON round-trips =====

        [Test]
        public void AnyMetaCodec_WriteJson_ReadJson_RoundTripsAcrossAllSixKinds()
        {
            foreach (var (kind, domain, version, typeid) in Cases)
            {
                var meta = new AnyMeta(kind, domain, version, typeid);
                var json = AnyMetaCodec.WriteJson(meta);
                var round = AnyMetaCodec.ReadJson(json);
                Assert.That(round, Is.InstanceOf<Either<BaboonCodecException, AnyMeta>.Right>(),
                    $"JSON read failed for kind 0x{kind:x}: {round}");
                var roundVal = ((Either<BaboonCodecException, AnyMeta>.Right)round).Value;
                Assert.That(roundVal, Is.EqualTo(meta), $"JSON round-trip failed for kind 0x{kind:x}");
            }
        }

        [Test]
        public void AnyMetaCodec_WriteJson_AlwaysReturnsObjectAcrossAllSixKinds()
        {
            // Locks the encoder envelope invariant: a future change that drops the JObject shape
            // would break this and avoid the silent-no-op bug PR-06-D08 calls out.
            foreach (var (kind, domain, version, typeid) in Cases)
            {
                var meta = new AnyMeta(kind, domain, version, typeid);
                var json = AnyMetaCodec.WriteJson(meta);
                Assert.That(json, Is.InstanceOf<JObject>(), $"writeJson must return JObject for kind 0x{kind:x}, got {json.Type}");
            }
        }

        [Test]
        public void AnyMetaCodec_ReadJson_RejectsMissingRequiredField()
        {
            // kind 0x07 (all bits) but missing $ad.
            var bad = new JObject { ["$ak"] = 0x07, ["$av"] = "v", ["$at"] = "t" };
            var result = AnyMetaCodec.ReadJson(bad);
            Assert.That(result, Is.InstanceOf<Either<BaboonCodecException, AnyMeta>.Left>());
            var msg = ((Either<BaboonCodecException, AnyMeta>.Left)result).Value.Message;
            Assert.That(msg, Does.Contain("$ad"));
        }

        [Test]
        public void AnyMetaCodec_ReadJson_RejectsForbiddenField()
        {
            // kind 0x00 (no bits) but extra $at present.
            var bad = new JObject { ["$ak"] = 0x00, ["$at"] = "t" };
            var result = AnyMetaCodec.ReadJson(bad);
            Assert.That(result, Is.InstanceOf<Either<BaboonCodecException, AnyMeta>.Left>());
            var msg = ((Either<BaboonCodecException, AnyMeta>.Left)result).Value.Message;
            Assert.That(msg, Does.Contain("$at"));
        }

        [Test]
        public void AnyMetaCodec_ReadJson_RejectsNonNumericKind()
        {
            var bad = new JObject { ["$ak"] = "not-a-number", ["$at"] = "T" };
            var result = AnyMetaCodec.ReadJson(bad);
            Assert.That(result, Is.InstanceOf<Either<BaboonCodecException, AnyMeta>.Left>());
            var msg = ((Either<BaboonCodecException, AnyMeta>.Left)result).Value.Message;
            Assert.That(msg, Does.Contain("$ak"));
        }

        // ===== BaboonTypeMetaCodec.ReadMeta(JToken) $mv envelope =====

        [Test]
        public void BaboonTypeMetaCodec_ReadMetaJson_RejectsFutureMetaVersion()
        {
            // Mirror Scala readMeta(json): $mv = "2" must round-trip to null.
            var bad = new JObject
            {
                ["$mv"] = "2",
                ["$d"] = "com.example",
                ["$v"] = "1.0.0",
                ["$t"] = "T",
            };
            var result = BaboonTypeMetaCodec.ReadMeta(bad);
            Assert.That(result, Is.Null);
        }

        [Test]
        public void BaboonTypeMetaCodec_ReadMetaJson_AcceptsMissingMetaVersionAsV1()
        {
            // Backward compat: envelopes without $mv are v1.
            var ok = new JObject
            {
                ["$d"] = "com.example",
                ["$v"] = "1.0.0",
                ["$t"] = "T",
            };
            var result = BaboonTypeMetaCodec.ReadMeta(ok);
            Assert.That(result, Is.Not.Null);
            Assert.That(result!.DomainIdentifier, Is.EqualTo("com.example"));
            Assert.That(result.DomainVersion, Is.EqualTo("1.0.0"));
            Assert.That(result.TypeIdentifier, Is.EqualTo("T"));
        }

        [Test]
        public void BaboonTypeMetaCodec_ReadMetaJson_AcceptsExplicitV1()
        {
            var ok = new JObject
            {
                ["$mv"] = "1",
                ["$d"] = "com.example",
                ["$v"] = "1.0.0",
                ["$t"] = "T",
            };
            var result = BaboonTypeMetaCodec.ReadMeta(ok);
            Assert.That(result, Is.Not.Null);
            Assert.That(result!.MetaVersion, Is.EqualTo((byte)1));
        }

        [Test]
        public void BaboonTypeMetaCodec_ReadMetaJson_AcceptsNumericMv()
        {
            // MFACADE-PR-3: $mv as JSON number is accepted (numeric form is now canonical;
            // string form retained for back-compat with M28-vintage fixtures).
            var ok = new JObject
            {
                ["$mv"] = 1,
                ["$d"] = "com.example",
                ["$v"] = "1.0.0",
                ["$t"] = "T",
            };
            var result = BaboonTypeMetaCodec.ReadMeta(ok);
            Assert.That(result, Is.Not.Null);
            Assert.That(result!.MetaVersion, Is.EqualTo((byte)1));
        }

        [Test]
        public void BaboonTypeMetaCodec_ReadMetaJson_RejectsUnparseableMv()
        {
            // $mv as non-numeric string — must return null.
            var bad = new JObject
            {
                ["$mv"] = "not-a-byte",
                ["$d"] = "com.example",
                ["$v"] = "1.0.0",
                ["$t"] = "T",
            };
            var result = BaboonTypeMetaCodec.ReadMeta(bad);
            Assert.That(result, Is.Null);
        }

        [Test]
        public void BaboonTypeMetaCodec_ReadMetaJson_RejectsNumericMvVersionMismatch()
        {
            // $mv as JSON integer 2 — in byte range but not META_VERSION_1, must return null.
            var bad = new JObject
            {
                ["$mv"] = 2,
                ["$d"] = "com.example",
                ["$v"] = "1.0.0",
                ["$t"] = "T",
            };
            var result = BaboonTypeMetaCodec.ReadMeta(bad);
            Assert.That(result, Is.Null);
        }

        // ===== BaboonTypeMetaCodec.WriteJson $mv envelope (MFACADE-PR-3-D04) =====

        [Test]
        public void WriteJson_EmitsMvAsNumber()
        {
            var meta = new BaboonTypeMeta(
                BaboonTypeMetaCodec.META_VERSION,
                "com.example",
                "1.0.0",
                "1.0.0",
                "T"
            );
            var json = BaboonTypeMetaCodec.WriteJson(meta);
            Assert.That(json, Is.InstanceOf<JObject>());
            var obj = (JObject)json;
            var mvToken = obj[BaboonTypeMetaCodec.META_VERSION_KEY];
            Assert.That(mvToken, Is.Not.Null);
            Assert.That(mvToken!.Type, Is.EqualTo(JTokenType.Integer));
            Assert.That(mvToken.Value<int>(), Is.EqualTo((int)BaboonTypeMetaCodec.META_VERSION));
        }

        // ===== ReadMeta(JToken) $mv edge-case matrix (MFACADE-PR-3-D06) =====

        [TestCase(300,  TestName = "ReadMetaJson_ReturnsNull_MvOutOfByteRange_300")]
        [TestCase(-1,   TestName = "ReadMetaJson_ReturnsNull_MvNegative")]
        public void BaboonTypeMetaCodec_ReadMetaJson_ReturnsNull_NumericMvOutOfRange(int mvValue)
        {
            var bad = new JObject
            {
                ["$mv"] = mvValue,
                ["$d"] = "com.example",
                ["$v"] = "1.0.0",
                ["$t"] = "T",
            };
            var result = BaboonTypeMetaCodec.ReadMeta(bad);
            Assert.That(result, Is.Null);
        }

        [Test]
        public void BaboonTypeMetaCodec_ReadMetaJson_ReturnsNull_MvFractional()
        {
            var bad = new JObject
            {
                ["$mv"] = 1.5,
                ["$d"] = "com.example",
                ["$v"] = "1.0.0",
                ["$t"] = "T",
            };
            var result = BaboonTypeMetaCodec.ReadMeta(bad);
            Assert.That(result, Is.Null);
        }

        [Test]
        public void BaboonTypeMetaCodec_ReadMetaJson_ReturnsNull_MvBoolean()
        {
            var bad = new JObject
            {
                ["$mv"] = true,
                ["$d"] = "com.example",
                ["$v"] = "1.0.0",
                ["$t"] = "T",
            };
            var result = BaboonTypeMetaCodec.ReadMeta(bad);
            Assert.That(result, Is.Null);
        }

        [Test]
        public void BaboonTypeMetaCodec_ReadMetaJson_ReturnsNull_MvArray()
        {
            var bad = new JObject
            {
                ["$mv"] = new JArray(),
                ["$d"] = "com.example",
                ["$v"] = "1.0.0",
                ["$t"] = "T",
            };
            var result = BaboonTypeMetaCodec.ReadMeta(bad);
            Assert.That(result, Is.Null);
        }

        [Test]
        public void BaboonTypeMetaCodec_ReadMetaJson_ReturnsNull_MvObject()
        {
            var bad = new JObject
            {
                ["$mv"] = new JObject(),
                ["$d"] = "com.example",
                ["$v"] = "1.0.0",
                ["$t"] = "T",
            };
            var result = BaboonTypeMetaCodec.ReadMeta(bad);
            Assert.That(result, Is.Null);
        }

        // ===== AnyOpaqueJson content equality =====

        [Test]
        public void AnyOpaqueJson_compares_json_by_content_not_reference()
        {
            var meta = new AnyMeta(0x07, "d", "v", "t");
            var j1 = JObject.Parse("{\"x\":1,\"y\":[2,3]}");
            var j2 = JObject.Parse("{\"x\":1,\"y\":[2,3]}");
            var a = new AnyOpaqueJson(meta, j1);
            var b = new AnyOpaqueJson(meta, j2);
            Assert.That(a, Is.EqualTo(b));
            Assert.That(a.GetHashCode(), Is.EqualTo(b.GetHashCode()));

            var j3 = JObject.Parse("{\"x\":1,\"y\":[2,4]}");
            var c = new AnyOpaqueJson(meta, j3);
            Assert.That(a, Is.Not.EqualTo(c));
        }

        // ===== AnyOpaqueUeba content equality =====

        [Test]
        public void AnyOpaqueUeba_EqualityIsContentBased()
        {
            var meta = new AnyMeta(0x07, "d", "v", "t");
            var a = new AnyOpaqueUeba(meta, new byte[] { 1, 2, 3 });
            var b = new AnyOpaqueUeba(meta, new byte[] { 1, 2, 3 });
            Assert.That(a, Is.EqualTo(b), "content-equal arrays must yield equal AnyOpaqueUeba");
            Assert.That(a.GetHashCode(), Is.EqualTo(b.GetHashCode()), "equal AnyOpaqueUeba must hash equal");

            var c = new AnyOpaqueUeba(meta, new byte[] { 1, 2, 4 });
            Assert.That(a, Is.Not.EqualTo(c));

            var emptyA = new AnyOpaqueUeba(meta, Array.Empty<byte>());
            var emptyB = new AnyOpaqueUeba(meta, new byte[0]);
            Assert.That(emptyA, Is.EqualTo(emptyB));
        }

        // ===== BaboonCodecContext.WithFacade =====

        [Test]
        public void BaboonCodecContext_WithFacade_ExposesFacade()
        {
            var facade = new BaboonCodecsFacade();
            Assert.That(BaboonCodecContext.Compact.Facade, Is.Null);
            Assert.That(BaboonCodecContext.Indexed.Facade, Is.Null);

            var withFacadeCompact = BaboonCodecContext.WithFacade(useIndices: false, facade);
            Assert.That(withFacadeCompact.Facade, Is.SameAs(facade));
            Assert.That(withFacadeCompact.UseIndices, Is.False);

            var withFacadeIndexed = BaboonCodecContext.WithFacade(useIndices: true, facade);
            Assert.That(withFacadeIndexed.Facade, Is.SameAs(facade));
            Assert.That(withFacadeIndexed.UseIndices, Is.True);
        }

        // ===== Facade Left paths =====

        [Test]
        public void DecodeAny_Left_OnIncompleteMeta_Ueba()
        {
            var facade = new BaboonCodecsFacade();
            var meta = new AnyMeta(0x01, null, null, "T"); // kind C: only typeid
            var result = facade.DecodeAny(new AnyOpaqueUeba(meta, Array.Empty<byte>()));
            Assert.That(result, Is.InstanceOf<Either<BaboonCodecException, IBaboonGenerated>.Left>());
            var msg = ((Either<BaboonCodecException, IBaboonGenerated>.Left)result).Value.Message;
            Assert.That(msg, Does.Contain("domain"));
            Assert.That(msg, Does.Contain("version"));
        }

        [Test]
        public void DecodeAny_Left_OnIncompleteMeta_Json()
        {
            var facade = new BaboonCodecsFacade();
            var meta = new AnyMeta(0x01, null, null, "T");
            var result = facade.DecodeAny(new AnyOpaqueJson(meta, JValue.CreateNull()));
            Assert.That(result, Is.InstanceOf<Either<BaboonCodecException, IBaboonGenerated>.Left>());
            var msg = ((Either<BaboonCodecException, IBaboonGenerated>.Left)result).Value.Message;
            Assert.That(msg, Does.Contain("domain"));
            Assert.That(msg, Does.Contain("version"));
        }

        [Test]
        public void JsonToUebaBytes_Left_OnIncompleteMeta()
        {
            var facade = new BaboonCodecsFacade();
            var meta = new AnyMeta(0x01, null, null, "T");
            var result = facade.JsonToUebaBytes(meta, JValue.CreateNull());
            Assert.That(result, Is.InstanceOf<Either<BaboonCodecException, byte[]>.Left>());
            var msg = ((Either<BaboonCodecException, byte[]>.Left)result).Value.Message;
            Assert.That(msg, Does.Contain("domain"));
            Assert.That(msg, Does.Contain("version"));
        }

        [Test]
        public void UebaToJson_Left_OnIncompleteMeta()
        {
            var facade = new BaboonCodecsFacade();
            var meta = new AnyMeta(0x01, null, null, "T");
            var result = facade.UebaToJson(meta, Array.Empty<byte>());
            Assert.That(result, Is.InstanceOf<Either<BaboonCodecException, JToken>.Left>());
            var msg = ((Either<BaboonCodecException, JToken>.Left)result).Value.Message;
            Assert.That(msg, Does.Contain("domain"));
            Assert.That(msg, Does.Contain("version"));
        }

        [Test]
        public void JsonToUebaBytes_Left_OnNoCodecRegistered()
        {
            var facade = new BaboonCodecsFacade();
            var meta = new AnyMeta(0x07, "com.example.unknown", "1.0.0", "Unknown");
            var result = facade.JsonToUebaBytes(meta, new JObject { ["x"] = 1 });
            Assert.That(result, Is.InstanceOf<Either<BaboonCodecException, byte[]>.Left>());
            var err = ((Either<BaboonCodecException, byte[]>.Left)result).Value;
            Assert.That(err, Is.InstanceOf<BaboonCodecException.CodecNotFound>());
        }

        [Test]
        public void UebaToJson_Left_OnNoCodecRegistered()
        {
            var facade = new BaboonCodecsFacade();
            var meta = new AnyMeta(0x07, "com.example.unknown", "1.0.0", "Unknown");
            var result = facade.UebaToJson(meta, Array.Empty<byte>());
            Assert.That(result, Is.InstanceOf<Either<BaboonCodecException, JToken>.Left>());
            var err = ((Either<BaboonCodecException, JToken>.Left)result).Value;
            Assert.That(err, Is.InstanceOf<BaboonCodecException.CodecNotFound>());
        }

        // ===== Static-fallback semantics (PR-06-D01) =====

        [Test]
        public void JsonToUebaBytes_StaticFallback_VariantB_DomainFromStatic()
        {
            var facade = new BaboonCodecsFacade();
            // kind 0x03 (B): no domain on wire, version + typeid present.
            var meta = new AnyMeta(0x03, null, "1.0.0", "T");
            var result = facade.JsonToUebaBytes(meta, JValue.CreateNull(), staticDomain: "com.example.unknown");
            // Should reach codec lookup (CodecNotFound), not the missing-meta DecoderFailure.
            Assert.That(result, Is.InstanceOf<Either<BaboonCodecException, byte[]>.Left>());
            var err = ((Either<BaboonCodecException, byte[]>.Left)result).Value;
            Assert.That(err, Is.InstanceOf<BaboonCodecException.CodecNotFound>(),
                $"expected CodecNotFound (static fallback wired), got {err}");
        }

        [Test]
        public void JsonToUebaBytes_StaticFallback_VariantC_DomainAndVersionFromStatic()
        {
            var facade = new BaboonCodecsFacade();
            // kind 0x01 (C): only typeid on wire.
            var meta = new AnyMeta(0x01, null, null, "T");
            var result = facade.JsonToUebaBytes(meta, JValue.CreateNull(),
                staticDomain: "com.example.unknown", staticVersion: "1.0.0");
            Assert.That(result, Is.InstanceOf<Either<BaboonCodecException, byte[]>.Left>());
            var err = ((Either<BaboonCodecException, byte[]>.Left)result).Value;
            Assert.That(err, Is.InstanceOf<BaboonCodecException.CodecNotFound>());
        }

        [Test]
        public void JsonToUebaBytes_StaticFallback_VariantD1_TypeidFromStatic()
        {
            var facade = new BaboonCodecsFacade();
            // kind 0x06 (D1): domain + version on wire, typeid absent.
            var meta = new AnyMeta(0x06, "com.example.unknown", "1.0.0", null);
            var result = facade.JsonToUebaBytes(meta, JValue.CreateNull(), staticTypeid: "Inner");
            Assert.That(result, Is.InstanceOf<Either<BaboonCodecException, byte[]>.Left>());
            var err = ((Either<BaboonCodecException, byte[]>.Left)result).Value;
            Assert.That(err, Is.InstanceOf<BaboonCodecException.CodecNotFound>());
        }

        [Test]
        public void JsonToUebaBytes_StaticFallback_VariantD3_AllThreeFromStatics()
        {
            var facade = new BaboonCodecsFacade();
            // kind 0x00 (D3): nothing on wire.
            var meta = new AnyMeta(0x00, null, null, null);
            var result = facade.JsonToUebaBytes(meta, JValue.CreateNull(),
                staticDomain: "com.example.unknown", staticVersion: "1.0.0", staticTypeid: "Inner");
            Assert.That(result, Is.InstanceOf<Either<BaboonCodecException, byte[]>.Left>());
            var err = ((Either<BaboonCodecException, byte[]>.Left)result).Value;
            Assert.That(err, Is.InstanceOf<BaboonCodecException.CodecNotFound>());
        }

        [Test]
        public void UebaToJson_StaticFallback_VariantD3_AllThreeFromStatics()
        {
            var facade = new BaboonCodecsFacade();
            var meta = new AnyMeta(0x00, null, null, null);
            var result = facade.UebaToJson(meta, Array.Empty<byte>(),
                staticDomain: "com.example.unknown", staticVersion: "1.0.0", staticTypeid: "Inner");
            Assert.That(result, Is.InstanceOf<Either<BaboonCodecException, JToken>.Left>());
            var err = ((Either<BaboonCodecException, JToken>.Left)result).Value;
            Assert.That(err, Is.InstanceOf<BaboonCodecException.CodecNotFound>());
        }

        [Test]
        public void JsonToUebaBytes_StaticFallback_MetaWinsOverStatic()
        {
            var facade = new BaboonCodecsFacade();
            // All three present on wire; statics differ — wire wins, so resolution must reference
            // meta.domain ("metawins"), not the static ("staticloses").
            var meta = new AnyMeta(0x07, "com.example.metawins", "1.0.0", "MetaT");
            var result = facade.JsonToUebaBytes(meta, JValue.CreateNull(),
                staticDomain: "com.example.staticloses", staticVersion: "9.9.9", staticTypeid: "StaticT");
            Assert.That(result, Is.InstanceOf<Either<BaboonCodecException, byte[]>.Left>());
            var err = ((Either<BaboonCodecException, byte[]>.Left)result).Value;
            Assert.That(err, Is.InstanceOf<BaboonCodecException.CodecNotFound>());
            Assert.That(err.Message, Does.Contain("metawins"),
                "expected error to reference wire-meta domain (override semantics)");
        }

        // ===== PR-07-D02: single-version-domain regression =====

        private sealed class StubMeta : IBaboonMeta
        {
            public IReadOnlyList<string> SameInVersions(string typeIdString) => Array.Empty<string>();
        }

        private sealed class StubJsonCodecs : AbstractBaboonJsonCodecs
        {
        }

        private sealed class StubUebaCodecs : AbstractBaboonUebaCodecs
        {
        }

        [Test]
        public void GetCodec_NonExact_AtMaxVersion_RoutesToExactLookup_SingleVersionDomain()
        {
            // Pre-fix: when minVersion == maxVersion == modelVersion and `exact=false`, the
            // dispatcher fell through every arm and returned "Unsupported domain version".
            // The fix added an explicit `!exact && v == max` arm routing to GetCodecExact.
            // This test calls JsonToUebaBytes (which uses exact=false); registering a single
            // version exercises the new arm. The codec lookup then fails with CodecNotFound for
            // a different reason ("No codec found for type..."), proving we reach the exact
            // lookup rather than the fall-through.
            var facade = new BaboonCodecsFacade();
            var dv = new BaboonDomainVersion("com.example.single", "1.0.0");
            facade.Register(
                dv,
                () => new StubJsonCodecs(),
                () => new StubUebaCodecs(),
                () => new StubMeta());
            var meta = new AnyMeta(0x07, "com.example.single", "1.0.0", "Unknown");
            var result = facade.JsonToUebaBytes(meta, JValue.CreateNull());
            Assert.That(result, Is.InstanceOf<Either<BaboonCodecException, byte[]>.Left>());
            var err = ((Either<BaboonCodecException, byte[]>.Left)result).Value;
            Assert.That(err, Is.InstanceOf<BaboonCodecException.CodecNotFound>());
            // Pre-fix message would have been "Unsupported domain version"; post-fix it's
            // "No codec found for type [...]" — i.e. we reached GetCodecExact.
            Assert.That(err.Message, Does.Not.Contain("Unsupported domain version"),
                "single-version-domain non-exact lookup must route to GetCodecExact, not fall through");
        }

        // ===== DecodeAny preserves all-required-meta contract =====

        [Test]
        public void DecodeAny_StillRequiresCompleteMeta_NoStaticFallbackForUserFacingPath()
        {
            // PR 2.1 (Scala) / PR 3.1 (C#) contract: DecodeAny is user-facing without static
            // context; its limitation to variant A (kind 0x07, all components) is preserved.
            var facade = new BaboonCodecsFacade();
            var meta = new AnyMeta(0x01, null, null, "T");
            var result = facade.DecodeAny(new AnyOpaqueUeba(meta, Array.Empty<byte>()));
            Assert.That(result, Is.InstanceOf<Either<BaboonCodecException, IBaboonGenerated>.Left>());
            var msg = ((Either<BaboonCodecException, IBaboonGenerated>.Left)result).Value.Message;
            Assert.That(msg, Does.Contain("domain"));
            Assert.That(msg, Does.Contain("version"));
        }

        // ===== PR-5/Q13: BaboonTypeMeta.From widening — ADT branch declared as abstract parent =====

        [Test]
        public void BaboonTypeMeta_From_AdtBranchDeclaredAsAbstractParent_UsesAdtTypeIdentifier()
        {
            // Fixture: testpkg.pkg0 / T4_A1 (current pkg03 schema) is a `root adt` with
            // branches B1, B2, B4. The C# generator emits `public abstract record T4_A1`
            // for the parent. Declaring a branch instance as the abstract parent type
            // exercises the PR-5/Q13 IsAbstract widening in BaboonTypeMeta.From.
            //
            // Pre-fix (IsInterface): typeof(Testpkg.Pkg0.T4_A1).IsInterface == false for an
            //   abstract record, so the ADT path was skipped — TypeIdentifier resolved to the
            //   branch's own id, not the ADT parent's.
            // Post-fix (IsAbstract): typeof(Testpkg.Pkg0.T4_A1).IsAbstract == true for an
            //   abstract record, so the ADT path fires — TypeIdentifier resolves to the ADT
            //   parent's type id (BaboonAdtTypeIdentifier()).
            Testpkg.Pkg0.T4_A1 value = new Testpkg.Pkg0.T4_A1.B4(null, "f1", "f");

            var meta = BaboonTypeMeta.From(value, typeof(Testpkg.Pkg0.T4_A1));

            // When declared as the abstract parent, From must use the ADT type identifier
            // (returned by IBaboonAdtMemberMeta.BaboonAdtTypeIdentifier()), not the branch's own.
            var branchMeta = BaboonTypeMeta.From(value, typeof(Testpkg.Pkg0.T4_A1.B4));
            Assert.That(meta.TypeIdentifier, Is.Not.EqualTo(branchMeta.TypeIdentifier),
                "abstract-parent declaration must yield the ADT type id, not the branch type id");
            Assert.That(meta.TypeIdentifier, Is.EqualTo(((IBaboonAdtMemberMeta)value).BaboonAdtTypeIdentifier()),
                "abstract-parent declaration must yield IBaboonAdtMemberMeta.BaboonAdtTypeIdentifier()");
        }
    }
}
