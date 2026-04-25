// NOTE: This test references generated runtime symbols (AnyMeta, AnyMetaCodec,
// BaboonCodecsFacade, BaboonCodecException, ...) AND generated DTO/codec symbols
// (My.Ok.Holder, My.Ok.Inner, ...) which are copied/generated into this stub
// only by `mdl :build :test-gen-regular-adt` (rsync + codegen into
// target/test-regular/cs-stub/). Running `dotnet test` directly from the source
// tree may fail with missing symbols; run the test suite from the codegen'd copy.
//
// Round-trip and cross-format tests for `any` fields (issue #69 PR 3.4 / M3 close).
// Mirrors Scala's AnyRoundTripSpec (PR 2.4). Exercises the `any-ok` fixture's six
// DSL variants (A=any, B=any[domain:this], C=any[domain:current], D1=any[Inner],
// D2=any[domain:this,Inner], D3=any[domain:current,Inner]) plus the three nested
// positions (opt/lst/map-value).
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
    public class AnyRoundTripTests
    {
        // Fresh per-spec facade: registers Holder/Inner codecs from the my.ok domain so the
        // cross-format helpers and DecodeAny can resolve `(domain, version, typeid)` triples.
        private static BaboonCodecsFacade FreshFacade()
        {
            var f = new BaboonCodecsFacade();
            var dv = new BaboonDomainVersion(My.Ok.Holder.BaboonDomainIdentifierValue, My.Ok.Holder.BaboonDomainVersionValue);
            f.Register(
                dv,
                () => My.Ok.BaboonCodecsJson.Instance,
                () => My.Ok.BaboonCodecsUeba.Instance,
                () => My.Ok.BaboonMeta.Instance);
            return f;
        }

        private const string DomainId = "my.ok";
        private const string VersionStr = "1.0.0";
        private const string InnerType = "my.ok/:#Inner";

        // Construct AnyMeta for each variant, populating only the bits the kind byte claims.
        private static AnyMeta MetaA() => new(0x07, DomainId, VersionStr, "opaque.Type"); // wire-only typeid for untyped variants
        private static AnyMeta MetaB() => new(0x03, null, VersionStr, "opaque.Type");
        private static AnyMeta MetaC() => new(0x01, null, null, "opaque.Type");
        private static AnyMeta MetaD1() => new(0x06, DomainId, VersionStr, null);
        private static AnyMeta MetaD2() => new(0x02, null, VersionStr, null);
        private static AnyMeta MetaD3() => new(0x00, null, null, null);

        // Encode an Inner via the generated UEBA codec — used when constructing AnyOpaqueUeba payloads
        // so cross-convert tests have a real Inner to deserialize.
        private static byte[] InnerToUebaBytes(My.Ok.Inner inner)
        {
            using var ms = new MemoryStream();
            using var bw = new BinaryWriter(ms);
            My.Ok.Inner_UEBACodec.Instance.Encode(BaboonCodecContext.Compact, bw, inner);
            bw.Flush();
            return ms.ToArray();
        }

        private static JToken InnerToJson(My.Ok.Inner inner)
        {
            return My.Ok.Inner_JsonCodec.Instance.Encode(BaboonCodecContext.Compact, inner);
        }

        private static readonly My.Ok.Inner SampleInner = new(42);

        // Build a complete Holder with one AnyOpaqueUeba per variant. UEBA round-trips natively
        // (no facade needed for encode/decode — facade is only consulted for cross-convert).
        private static My.Ok.Holder BuildUebaHolder()
        {
            var innerBytes = InnerToUebaBytes(SampleInner);
            return new My.Ok.Holder(
                FAny: new AnyOpaqueUeba(MetaA(), new byte[] { 1, 2, 3 }),
                FDomainThis: new AnyOpaqueUeba(MetaB(), new byte[] { 4, 5 }),
                FDomainCurrent: new AnyOpaqueUeba(MetaC(), new byte[] { 6 }),
                FUnderlying: new AnyOpaqueUeba(MetaD1(), innerBytes),
                FThisUnderlying: new AnyOpaqueUeba(MetaD2(), innerBytes),
                FCurrentUnderlying: new AnyOpaqueUeba(MetaD3(), innerBytes),
                FOpt: new AnyOpaqueUeba(MetaA(), new byte[] { 7 }),
                FLst: new List<AnyOpaque> { new AnyOpaqueUeba(MetaD1(), innerBytes) },
                FMapValue: new Dictionary<string, AnyOpaque> { { "k1", new AnyOpaqueUeba(MetaA(), new byte[] { 8 }) } });
        }

        // Build a Holder using AnyOpaqueJson branches everywhere with arbitrary inner JSON content.
        // Used as the "all native JSON branch" baseline for JSON round-trip tests.
        private static My.Ok.Holder BuildJsonNativeHolder()
        {
            var arbitraryJson = new JObject { ["payload"] = 42 };
            var innerJson = InnerToJson(SampleInner);
            return new My.Ok.Holder(
                FAny: new AnyOpaqueJson(MetaA(), arbitraryJson),
                FDomainThis: new AnyOpaqueJson(MetaB(), arbitraryJson),
                FDomainCurrent: new AnyOpaqueJson(MetaC(), arbitraryJson),
                FUnderlying: new AnyOpaqueJson(MetaD1(), innerJson),
                FThisUnderlying: new AnyOpaqueJson(MetaD2(), innerJson),
                FCurrentUnderlying: new AnyOpaqueJson(MetaD3(), innerJson),
                FOpt: new AnyOpaqueJson(MetaA(), arbitraryJson),
                FLst: new List<AnyOpaque> { new AnyOpaqueJson(MetaD1(), innerJson) },
                FMapValue: new Dictionary<string, AnyOpaque> { { "k1", new AnyOpaqueJson(MetaA(), arbitraryJson) } });
        }

        // Build a Holder using AnyOpaqueJson branches with REAL Inner JSON for D variants and
        // typeid=InnerType for A/B/C so cross-convert can resolve Inner via the registered facade.
        private static My.Ok.Holder BuildJsonHolderForCrossConvert()
        {
            var innerJson = InnerToJson(SampleInner);
            return new My.Ok.Holder(
                FAny: new AnyOpaqueJson(new AnyMeta(0x07, DomainId, VersionStr, InnerType), innerJson),
                FDomainThis: new AnyOpaqueJson(new AnyMeta(0x03, null, VersionStr, InnerType), innerJson),
                FDomainCurrent: new AnyOpaqueJson(new AnyMeta(0x01, null, null, InnerType), innerJson),
                FUnderlying: new AnyOpaqueJson(MetaD1(), innerJson),
                FThisUnderlying: new AnyOpaqueJson(MetaD2(), innerJson),
                FCurrentUnderlying: new AnyOpaqueJson(MetaD3(), innerJson),
                FOpt: new AnyOpaqueJson(new AnyMeta(0x07, DomainId, VersionStr, InnerType), innerJson),
                FLst: new List<AnyOpaque> { new AnyOpaqueJson(MetaD1(), innerJson) },
                FMapValue: new Dictionary<string, AnyOpaque> { { "k1", new AnyOpaqueJson(new AnyMeta(0x07, DomainId, VersionStr, InnerType), innerJson) } });
        }

        private static byte[] EncodeUebaBytes(My.Ok.Holder value, BaboonCodecContext ctx)
        {
            using var ms = new MemoryStream();
            using var bw = new BinaryWriter(ms);
            My.Ok.Holder_UEBACodec.Instance.Encode(ctx, bw, value);
            bw.Flush();
            return ms.ToArray();
        }

        private static My.Ok.Holder DecodeUebaBytes(byte[] bytes)
        {
            using var ms = new MemoryStream(bytes);
            using var br = new BinaryReader(ms);
            return My.Ok.Holder_UEBACodec.Instance.Decode(BaboonCodecContext.Compact, br);
        }

        // ===== 1. Per-variant UEBA round-trip =====

        [Test]
        public void UebaRoundTrip_AllSixVariantsPlusNestedPositions_PreserveContent()
        {
            var original = BuildUebaHolder();
            var bytes = EncodeUebaBytes(original, BaboonCodecContext.Compact);
            var decoded = DecodeUebaBytes(bytes);
            Assert.That(decoded, Is.EqualTo(original));
        }

        [Test]
        public void UebaRoundTrip_WithUseIndicesTrue_PreservesContent()
        {
            var original = BuildUebaHolder();
            var bytes = EncodeUebaBytes(original, BaboonCodecContext.Indexed);
            var decoded = DecodeUebaBytes(bytes);
            Assert.That(decoded, Is.EqualTo(original));
        }

        [Test]
        public void UebaDecode_YieldsAnyOpaqueUebaWithMatchingKindBytes()
        {
            var original = BuildUebaHolder();
            var bytes = EncodeUebaBytes(original, BaboonCodecContext.Compact);
            var decoded = DecodeUebaBytes(bytes);
            // Per-field kind assertion locks in PR 3.2's wire layout.
            Assert.That(decoded.FAny.Meta.Kind, Is.EqualTo((byte)0x07), "fAny variant A");
            Assert.That(decoded.FDomainThis.Meta.Kind, Is.EqualTo((byte)0x03), "fDomainThis variant B");
            Assert.That(decoded.FDomainCurrent.Meta.Kind, Is.EqualTo((byte)0x01), "fDomainCurrent variant C");
            Assert.That(decoded.FUnderlying.Meta.Kind, Is.EqualTo((byte)0x06), "fUnderlying variant D1");
            Assert.That(decoded.FThisUnderlying.Meta.Kind, Is.EqualTo((byte)0x02), "fThisUnderlying variant D2");
            Assert.That(decoded.FCurrentUnderlying.Meta.Kind, Is.EqualTo((byte)0x00), "fCurrentUnderlying variant D3");
            Assert.That(decoded.FAny, Is.InstanceOf<AnyOpaqueUeba>(), "UEBA decode must yield AnyOpaqueUeba");
        }

        // ===== 2. Per-variant JSON round-trip =====

        [Test]
        public void JsonRoundTrip_AllSixVariantsPlusNestedPositions_PreserveContent()
        {
            var original = BuildJsonNativeHolder();
            var json = My.Ok.Holder_JsonCodec.Instance.Encode(BaboonCodecContext.Compact, original);
            var decoded = My.Ok.Holder_JsonCodec.Instance.Decode(BaboonCodecContext.Compact, json);
            Assert.That(decoded, Is.EqualTo(original));
        }

        [Test]
        public void JsonDecode_YieldsAnyOpaqueJsonWithMatchingKindBytes()
        {
            var original = BuildJsonNativeHolder();
            var json = My.Ok.Holder_JsonCodec.Instance.Encode(BaboonCodecContext.Compact, original);
            var decoded = My.Ok.Holder_JsonCodec.Instance.Decode(BaboonCodecContext.Compact, json);
            Assert.That(decoded.FAny, Is.InstanceOf<AnyOpaqueJson>(), "JSON decode must yield AnyOpaqueJson");
            Assert.That(decoded.FAny.Meta.Kind, Is.EqualTo((byte)0x07));
            Assert.That(decoded.FDomainThis.Meta.Kind, Is.EqualTo((byte)0x03));
            Assert.That(decoded.FDomainCurrent.Meta.Kind, Is.EqualTo((byte)0x01));
            Assert.That(decoded.FUnderlying.Meta.Kind, Is.EqualTo((byte)0x06));
            Assert.That(decoded.FThisUnderlying.Meta.Kind, Is.EqualTo((byte)0x02));
            Assert.That(decoded.FCurrentUnderlying.Meta.Kind, Is.EqualTo((byte)0x00));
        }

        // ===== 3. Cross-format conversion via facade =====

        [Test]
        public void CrossFormat_JsonHolderToUeba_DecodeRoundTrip()
        {
            // BuildJsonHolderForCrossConvert uses AnyOpaqueJson branches for ALL fields with real
            // Inner JSON; encoding to UEBA forces JsonToUebaBytes per field. Decoding produces
            // AnyOpaqueUeba branches.
            var facade = FreshFacade();
            var ctxWithFacade = BaboonCodecContext.WithFacade(useIndices: false, facade);
            var original = BuildJsonHolderForCrossConvert();
            var bytes = EncodeUebaBytes(original, ctxWithFacade);
            var decoded = DecodeUebaBytes(bytes);

            // Re-encode to UEBA using the now-Ueba-branched holder (no facade needed) and assert byte
            // equality — proves the cross-converted bytes match a native UEBA encode of the same value.
            var rebytes = EncodeUebaBytes(decoded, BaboonCodecContext.Compact);
            Assert.That(rebytes, Is.EqualTo(bytes), "JSON->UEBA cross-convert produced non-canonical bytes");
        }

        [Test]
        public void CrossFormat_UebaHolderToJson_DecodeRoundTrip()
        {
            // BuildUebaHolder uses AnyOpaqueUeba branches everywhere. Encoding to JSON triggers
            // UebaToJson for each field. For untyped variants A/B/C the wire meta carries typeid;
            // we substitute typeid=InnerType so the registered Inner codec resolves and the bytes
            // deserialize as Inner. D variants resolve via static fallbacks emitted by codec gen.
            var facade = FreshFacade();
            var ctxWithFacade = BaboonCodecContext.WithFacade(useIndices: false, facade);
            var innerBytes = InnerToUebaBytes(SampleInner);
            var crossable = BuildUebaHolder() with
            {
                FAny = new AnyOpaqueUeba(new AnyMeta(0x07, DomainId, VersionStr, InnerType), innerBytes),
                FDomainThis = new AnyOpaqueUeba(new AnyMeta(0x03, null, VersionStr, InnerType), innerBytes),
                FDomainCurrent = new AnyOpaqueUeba(new AnyMeta(0x01, null, null, InnerType), innerBytes),
                FOpt = new AnyOpaqueUeba(new AnyMeta(0x07, DomainId, VersionStr, InnerType), innerBytes),
                FMapValue = new Dictionary<string, AnyOpaque> { { "k1", new AnyOpaqueUeba(new AnyMeta(0x07, DomainId, VersionStr, InnerType), innerBytes) } },
            };
            var json = My.Ok.Holder_JsonCodec.Instance.Encode(ctxWithFacade, crossable);
            // Sanity-decode the JSON to ensure the envelope is well-formed.
            var decoded = My.Ok.Holder_JsonCodec.Instance.Decode(BaboonCodecContext.Compact, json);
            Assert.That(decoded.FAny, Is.InstanceOf<AnyOpaqueJson>());
            Assert.That(decoded.FAny.Meta.Kind, Is.EqualTo((byte)0x07));
            Assert.That(decoded.FUnderlying.Meta.Kind, Is.EqualTo((byte)0x06));
            Assert.That(decoded.FCurrentUnderlying.Meta.Kind, Is.EqualTo((byte)0x00)); // D3: kind=0x00, statics filled
        }

        [Test]
        public void CrossFormat_D3IsolatedField_StaticFallbacksResolveEndToEnd()
        {
            // PR-06-D01 (C# analog) regression: D3 has all-None meta on wire; the codec generator
            // emits (currentDomain, currentVersion, underlyingFqid) as static fallbacks. Without
            // these the facade cannot resolve and cross-convert fails.
            var facade = FreshFacade();
            var ctxWithFacade = BaboonCodecContext.WithFacade(useIndices: false, facade);
            var innerJson = InnerToJson(SampleInner);
            var mixed = BuildUebaHolder() with
            {
                FCurrentUnderlying = new AnyOpaqueJson(MetaD3(), innerJson),
            };
            // No throw on encode means JsonToUebaBytes succeeded for the D3 field (statics resolved).
            var bytes = EncodeUebaBytes(mixed, ctxWithFacade);
            var decoded = DecodeUebaBytes(bytes);
            Assert.That(decoded.FCurrentUnderlying.Meta.Kind, Is.EqualTo((byte)0x00));
            Assert.That(decoded.FCurrentUnderlying, Is.InstanceOf<AnyOpaqueUeba>());
            var blob = ((AnyOpaqueUeba)decoded.FCurrentUnderlying).Bytes;
            using var rms = new MemoryStream(blob);
            using var br = new BinaryReader(rms);
            var inner = My.Ok.Inner_UEBACodec.Instance.Decode(BaboonCodecContext.Compact, br);
            Assert.That(inner, Is.EqualTo(SampleInner), "D3 cross-convert payload must decode as the original Inner");
        }

        // ===== 4. facade.DecodeAny end-to-end =====

        [Test]
        public void DecodeAny_ResolvesUebaInnerToTypedInner()
        {
            var facade = FreshFacade();
            var meta = new AnyMeta(0x07, DomainId, VersionStr, InnerType);
            var opaque = new AnyOpaqueUeba(meta, InnerToUebaBytes(SampleInner));
            var result = facade.DecodeAny(opaque);
            Assert.That(result, Is.InstanceOf<Either<BaboonCodecException, IBaboonGenerated>.Right>());
            var value = ((Either<BaboonCodecException, IBaboonGenerated>.Right)result).Value;
            Assert.That(value, Is.InstanceOf<My.Ok.Inner>());
            Assert.That((My.Ok.Inner)value, Is.EqualTo(SampleInner));
        }

        [Test]
        public void DecodeAny_ResolvesJsonInnerToTypedInner()
        {
            var facade = FreshFacade();
            var meta = new AnyMeta(0x07, DomainId, VersionStr, InnerType);
            var opaque = new AnyOpaqueJson(meta, InnerToJson(SampleInner));
            var result = facade.DecodeAny(opaque);
            Assert.That(result, Is.InstanceOf<Either<BaboonCodecException, IBaboonGenerated>.Right>());
            var value = ((Either<BaboonCodecException, IBaboonGenerated>.Right)result).Value;
            Assert.That(value, Is.InstanceOf<My.Ok.Inner>());
            Assert.That((My.Ok.Inner)value, Is.EqualTo(SampleInner));
        }

        // ===== 5. Forward-compat: trailing meta-extension bytes inside meta-length window =====

        [Test]
        public void ForwardCompat_ExtraMetaExtensionBytes_AreSkippedOnUebaDecode()
        {
            // Encode a Holder normally, then surgically patch the FIRST any-field's meta-length to
            // claim 5 extra bytes, and splice 5 bytes into the meta block. The decoder must consume
            // the meta, observe the gap (anyMetaLen - bytesRead), skip them, and continue parsing.
            var original = BuildUebaHolder();
            var bytes = EncodeUebaBytes(original, BaboonCodecContext.Compact);

            // Layout of the first any-field on the wire (Compact, useIndices=false):
            // [1 byte header][i32 anyLength][i32 anyMetaLen][... metaBytes ...][... blob ...]
            const int headerLen = 1;
            const int anyLengthOffset = headerLen;       // 4 bytes
            const int anyMetaLenOffset = headerLen + 4;  // 4 bytes
            const int anyMetaStartOffset = headerLen + 4 + 4;
            var origAnyLength = ReadI32Le(bytes, anyLengthOffset);
            var origAnyMetaLen = ReadI32Le(bytes, anyMetaLenOffset);

            var extension = new byte[] { 0x11, 0x22, 0x33, 0x44, 0x55 };
            var newAnyMetaLen = origAnyMetaLen + extension.Length;
            var newAnyLength = origAnyLength + extension.Length;

            var origMetaSlice = new byte[origAnyMetaLen];
            Array.Copy(bytes, anyMetaStartOffset, origMetaSlice, 0, origAnyMetaLen);
            var origBlobAndRestStart = anyMetaStartOffset + origAnyMetaLen;
            var origBlobAndRestLen = bytes.Length - origBlobAndRestStart;
            var origBlobAndRest = new byte[origBlobAndRestLen];
            Array.Copy(bytes, origBlobAndRestStart, origBlobAndRest, 0, origBlobAndRestLen);

            using var ms = new MemoryStream();
            using (var bw = new BinaryWriter(ms))
            {
                bw.Write(bytes[0]); // header
                bw.Write(newAnyLength);
                bw.Write(newAnyMetaLen);
                bw.Write(origMetaSlice);
                bw.Write(extension);
                bw.Write(origBlobAndRest);
            }
            var patched = ms.ToArray();

            var decoded = DecodeUebaBytes(patched);
            Assert.That(decoded, Is.EqualTo(original), "forward-compat decode must structurally match original");
        }

        private static int ReadI32Le(byte[] data, int offset)
        {
            return (data[offset] & 0xFF)
                 | ((data[offset + 1] & 0xFF) << 8)
                 | ((data[offset + 2] & 0xFF) << 16)
                 | ((data[offset + 3] & 0xFF) << 24);
        }

        // ===== 6. Fail-fast: missing-facade cross-convert =====

        [Test]
        public void EncodeJsonAnyIntoUeba_WithoutFacade_FailsFast()
        {
            var mixed = BuildUebaHolder() with
            {
                FAny = new AnyOpaqueJson(MetaA(), new JObject { ["x"] = 1 }),
            };
            var ex = Assert.Throws<BaboonCodecException.EncoderFailure>(() =>
            {
                using var ms = new MemoryStream();
                using var bw = new BinaryWriter(ms);
                My.Ok.Holder_UEBACodec.Instance.Encode(BaboonCodecContext.Compact, bw, mixed);
            });
            Assert.That(ex!.Message, Does.Contain("facade"));
            Assert.That(ex.Message, Does.Contain("WithFacade"));
        }

        [Test]
        public void EncodeUebaAnyIntoJson_WithoutFacade_FailsFast()
        {
            var mixed = BuildJsonNativeHolder() with
            {
                FAny = new AnyOpaqueUeba(MetaA(), new byte[] { 1, 2 }),
            };
            var ex = Assert.Throws<BaboonCodecException.EncoderFailure>(() =>
            {
                _ = My.Ok.Holder_JsonCodec.Instance.Encode(BaboonCodecContext.Compact, mixed);
            });
            Assert.That(ex!.Message, Does.Contain("facade"));
            Assert.That(ex.Message, Does.Contain("WithFacade"));
        }

        // ===== 7. JSON envelope shape lock-in =====

        [Test]
        public void JsonEnvelope_CarriesAkAndOptionalAdAvAtAndContentKey()
        {
            // Sanity: the JSON envelope produced by the codec embeds the AnyMeta keys ($ak, $ad?,
            // $av?, $at?) alongside the $c content key. Any change to the envelope that drops one of
            // these would break cross-language interop.
            var original = BuildJsonNativeHolder();
            var token = My.Ok.Holder_JsonCodec.Instance.Encode(BaboonCodecContext.Compact, original);
            var obj = (JObject)token;

            // fAny variant A → all four keys + $c present.
            var anyField = (JObject)obj["fAny"]!;
            Assert.That(anyField["$ak"], Is.Not.Null);
            Assert.That(anyField["$ad"], Is.Not.Null);
            Assert.That(anyField["$av"], Is.Not.Null);
            Assert.That(anyField["$at"], Is.Not.Null);
            Assert.That(anyField["$c"], Is.Not.Null);
            Assert.That(anyField["$ak"]!.Value<int>(), Is.EqualTo(0x07));

            // fCurrentUnderlying variant D3 → only $ak + $c (kind 0x00, no other meta on wire).
            var d3 = (JObject)obj["fCurrentUnderlying"]!;
            Assert.That(d3["$ak"], Is.Not.Null);
            Assert.That(d3["$c"], Is.Not.Null);
            Assert.That(d3["$ad"], Is.Null);
            Assert.That(d3["$av"], Is.Null);
            Assert.That(d3["$at"], Is.Null);
            Assert.That(d3["$ak"]!.Value<int>(), Is.EqualTo(0x00));

            // Sanity: the envelope keys appear exactly as documented (regression-proof key list).
            var expected = new HashSet<string> { "$ak", "$ad", "$av", "$at", "$c" };
            var present = new HashSet<string>(anyField.Properties().Select(p => p.Name));
            Assert.That(present.SetEquals(expected),
                "fAny envelope must expose exactly $ak/$ad/$av/$at/$c keys; got " + string.Join(",", present));
        }
    }
}
