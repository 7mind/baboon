// NOTE: This test references generated runtime symbols (AnyMeta, AnyMetaCodec,
// BaboonCodecsFacade, BaboonCodecException, ...) AND generated DTO/codec symbols
// (my.ok.Holder, my.ok.Inner, ...) which are copied/generated into this stub
// only by `mdl :build :test-gen-regular-adt` (rsync + codegen into
// target/test-regular/jv-stub/). Running `mvn test` directly from the source
// tree may fail with missing symbols; run the test suite from the codegen'd copy.
//
// Round-trip and cross-format tests for `any` fields (issue #69 PR 6.4 / M6 close).
// Mirrors Scala's AnyRoundTripSpec (PR 2.4) / C# AnyRoundTripTests (PR 3.4) /
// Rust any_round_trip_tests (PR 4.3) / Kotlin AnyRoundTripTest (PR 5.4). Exercises
// the `any-ok` fixture's six DSL variants (A=any, B=any[domain:this], C=any[domain:current],
// D1=any[Inner], D2=any[domain:this,Inner], D3=any[domain:current,Inner]) plus the three
// nested positions (opt/lst/map-value).
package runtime;

import baboon.runtime.shared.BaboonAnyOpaque.AnyMeta;
import baboon.runtime.shared.BaboonAnyOpaque.AnyOpaque;
import baboon.runtime.shared.BaboonAnyOpaque.AnyOpaqueJson;
import baboon.runtime.shared.BaboonAnyOpaque.AnyOpaqueUeba;
import baboon.runtime.shared.BaboonCodecContext;
import baboon.runtime.shared.BaboonCodecException;
import baboon.runtime.shared.BaboonCodecsFacade;
import baboon.runtime.shared.BaboonDomainVersion;
import baboon.runtime.shared.BaboonEither;
import baboon.runtime.shared.LEDataInputStream;
import baboon.runtime.shared.LEDataOutputStream;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.IntNode;
import com.fasterxml.jackson.databind.node.JsonNodeFactory;
import com.fasterxml.jackson.databind.node.ObjectNode;

import org.junit.jupiter.api.Test;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class AnyRoundTripTest {

    private static final JsonNodeFactory NF = JsonNodeFactory.instance;

    // Fresh per-test facade: registers Holder/Inner codecs from the my.ok domain so the
    // cross-format helpers and decodeAny can resolve `(domain, version, typeid)` triples.
    private static BaboonCodecsFacade freshFacade() {
        BaboonCodecsFacade f = new BaboonCodecsFacade();
        BaboonDomainVersion dv = new BaboonDomainVersion(my.ok.Holder.baboonDomainIdentifier, my.ok.Holder.baboonDomainVersion);
        f.register(
            dv,
            () -> new my.ok.BaboonCodecsJson(),
            () -> new my.ok.BaboonCodecsUeba(),
            () -> new my.ok.BaboonConversions(),
            () -> new my.ok.BaboonMetadata());
        return f;
    }

    private static final String DOMAIN_ID = "my.ok";
    private static final String VERSION_STR = "1.0.0";
    private static final String INNER_TYPE = "my.ok/:#Inner";

    // Construct AnyMeta for each variant, populating only the bits the kind byte claims.
    private static AnyMeta metaA() { return new AnyMeta((byte) 0x07, DOMAIN_ID, VERSION_STR, "opaque.Type"); }
    private static AnyMeta metaB() { return new AnyMeta((byte) 0x03, null, VERSION_STR, "opaque.Type"); }
    private static AnyMeta metaC() { return new AnyMeta((byte) 0x01, null, null, "opaque.Type"); }
    private static AnyMeta metaD1() { return new AnyMeta((byte) 0x06, DOMAIN_ID, VERSION_STR, null); }
    private static AnyMeta metaD2() { return new AnyMeta((byte) 0x02, null, VERSION_STR, null); }
    private static AnyMeta metaD3() { return new AnyMeta((byte) 0x00, null, null, null); }

    private static final my.ok.Inner SAMPLE_INNER = new my.ok.Inner(42);

    // Encode an Inner via the generated UEBA codec — used when constructing AnyOpaqueUeba payloads
    // so cross-convert tests have a real Inner to deserialize.
    private static byte[] innerToUebaBytes(my.ok.Inner inner) throws Exception {
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        LEDataOutputStream out = new LEDataOutputStream(baos);
        my.ok.Inner_UEBACodec.INSTANCE.encode(BaboonCodecContext.Compact, out, inner);
        out.flush();
        return baos.toByteArray();
    }

    private static JsonNode innerToJson(my.ok.Inner inner) {
        return my.ok.Inner_JsonCodec.INSTANCE.encode(BaboonCodecContext.Compact, inner);
    }

    // Build a complete Holder with one AnyOpaqueUeba per variant. UEBA round-trips natively
    // (no facade needed for encode/decode — facade is only consulted for cross-convert).
    private static my.ok.Holder buildUebaHolder() throws Exception {
        byte[] innerBytes = innerToUebaBytes(SAMPLE_INNER);
        return new my.ok.Holder(
            new AnyOpaqueUeba(metaA(), new byte[]{1, 2, 3}),
            new AnyOpaqueUeba(metaB(), new byte[]{4, 5}),
            new AnyOpaqueUeba(metaC(), new byte[]{6}),
            new AnyOpaqueUeba(metaD1(), innerBytes),
            new AnyOpaqueUeba(metaD2(), innerBytes),
            new AnyOpaqueUeba(metaD3(), innerBytes),
            Optional.of(new AnyOpaqueUeba(metaA(), new byte[]{7})),
            List.of(new AnyOpaqueUeba(metaD1(), innerBytes)),
            Map.of("k1", new AnyOpaqueUeba(metaA(), new byte[]{8})));
    }

    // Build a Holder using AnyOpaqueJson branches everywhere with arbitrary inner JSON content.
    // Used as the "all native JSON branch" baseline for JSON round-trip tests.
    private static my.ok.Holder buildJsonNativeHolder() {
        ObjectNode arbitraryJson = NF.objectNode();
        arbitraryJson.set("payload", IntNode.valueOf(42));
        JsonNode innerJson = innerToJson(SAMPLE_INNER);
        return new my.ok.Holder(
            new AnyOpaqueJson(metaA(), arbitraryJson),
            new AnyOpaqueJson(metaB(), arbitraryJson),
            new AnyOpaqueJson(metaC(), arbitraryJson),
            new AnyOpaqueJson(metaD1(), innerJson),
            new AnyOpaqueJson(metaD2(), innerJson),
            new AnyOpaqueJson(metaD3(), innerJson),
            Optional.of(new AnyOpaqueJson(metaA(), arbitraryJson)),
            List.of(new AnyOpaqueJson(metaD1(), innerJson)),
            Map.of("k1", new AnyOpaqueJson(metaA(), arbitraryJson)));
    }

    // Build a Holder using AnyOpaqueJson branches with REAL Inner JSON for D variants and
    // typeid=INNER_TYPE for A/B/C so cross-convert can resolve Inner via the registered facade.
    private static my.ok.Holder buildJsonHolderForCrossConvert() {
        JsonNode innerJson = innerToJson(SAMPLE_INNER);
        return new my.ok.Holder(
            new AnyOpaqueJson(new AnyMeta((byte) 0x07, DOMAIN_ID, VERSION_STR, INNER_TYPE), innerJson),
            new AnyOpaqueJson(new AnyMeta((byte) 0x03, null, VERSION_STR, INNER_TYPE), innerJson),
            new AnyOpaqueJson(new AnyMeta((byte) 0x01, null, null, INNER_TYPE), innerJson),
            new AnyOpaqueJson(metaD1(), innerJson),
            new AnyOpaqueJson(metaD2(), innerJson),
            new AnyOpaqueJson(metaD3(), innerJson),
            Optional.of(new AnyOpaqueJson(new AnyMeta((byte) 0x07, DOMAIN_ID, VERSION_STR, INNER_TYPE), innerJson)),
            List.of(new AnyOpaqueJson(metaD1(), innerJson)),
            Map.of("k1", new AnyOpaqueJson(new AnyMeta((byte) 0x07, DOMAIN_ID, VERSION_STR, INNER_TYPE), innerJson)));
    }

    private static byte[] encodeUebaBytes(my.ok.Holder value, BaboonCodecContext ctx) throws Exception {
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        LEDataOutputStream out = new LEDataOutputStream(baos);
        my.ok.Holder_UEBACodec.INSTANCE.encode(ctx, out, value);
        out.flush();
        return baos.toByteArray();
    }

    private static my.ok.Holder decodeUebaBytes(byte[] bytes, BaboonCodecContext ctx) throws Exception {
        LEDataInputStream in = new LEDataInputStream(new ByteArrayInputStream(bytes));
        return my.ok.Holder_UEBACodec.INSTANCE.decode(ctx, in);
    }

    private static my.ok.Holder decodeUebaBytes(byte[] bytes) throws Exception {
        return decodeUebaBytes(bytes, BaboonCodecContext.Compact);
    }

    // ===== 1. Per-variant UEBA round-trip =====

    @Test
    void uebaRoundTrip_allSixVariantsPlusNestedPositions_preserveContent() throws Exception {
        my.ok.Holder original = buildUebaHolder();
        byte[] bytes = encodeUebaBytes(original, BaboonCodecContext.Compact);
        my.ok.Holder decoded = decodeUebaBytes(bytes);
        assertEquals(original, decoded);
    }

    @Test
    void uebaRoundTrip_indexedMode_preserveContent() throws Exception {
        // Indexed mode (`useIndices=true`) emits a per-field length prefix — exercise that path so
        // the any-field-aware indexed encoder/decoder is covered. Java's Holder_UEBACodec emits real
        // statement blocks for indexed mode, so unlike Kotlin's PR-15-D01 we can run this test.
        my.ok.Holder original = buildUebaHolder();
        byte[] bytes = encodeUebaBytes(original, BaboonCodecContext.Indexed);
        my.ok.Holder decoded = decodeUebaBytes(bytes, BaboonCodecContext.Indexed);
        assertEquals(original, decoded);
    }

    @Test
    void uebaDecode_yieldsAnyOpaqueUebaWithMatchingKindBytes() throws Exception {
        my.ok.Holder original = buildUebaHolder();
        byte[] bytes = encodeUebaBytes(original, BaboonCodecContext.Compact);
        my.ok.Holder decoded = decodeUebaBytes(bytes);
        assertEquals((byte) 0x07, decoded.fAny().meta().kind(), "fAny variant A");
        assertEquals((byte) 0x03, decoded.fDomainThis().meta().kind(), "fDomainThis variant B");
        assertEquals((byte) 0x01, decoded.fDomainCurrent().meta().kind(), "fDomainCurrent variant C");
        assertEquals((byte) 0x06, decoded.fUnderlying().meta().kind(), "fUnderlying variant D1");
        assertEquals((byte) 0x02, decoded.fThisUnderlying().meta().kind(), "fThisUnderlying variant D2");
        assertEquals((byte) 0x00, decoded.fCurrentUnderlying().meta().kind(), "fCurrentUnderlying variant D3");
        assertInstanceOf(AnyOpaqueUeba.class, decoded.fAny(), "UEBA decode must yield AnyOpaqueUeba");
    }

    // ===== 2. Per-variant JSON round-trip =====

    @Test
    void jsonRoundTrip_allSixVariantsPlusNestedPositions_preserveContent() {
        my.ok.Holder original = buildJsonNativeHolder();
        JsonNode json = my.ok.Holder_JsonCodec.INSTANCE.encode(BaboonCodecContext.Compact, original);
        my.ok.Holder decoded = my.ok.Holder_JsonCodec.INSTANCE.decode(BaboonCodecContext.Compact, json);
        assertEquals(original, decoded);
    }

    @Test
    void jsonDecode_yieldsAnyOpaqueJsonWithMatchingKindBytes() {
        my.ok.Holder original = buildJsonNativeHolder();
        JsonNode json = my.ok.Holder_JsonCodec.INSTANCE.encode(BaboonCodecContext.Compact, original);
        my.ok.Holder decoded = my.ok.Holder_JsonCodec.INSTANCE.decode(BaboonCodecContext.Compact, json);
        assertInstanceOf(AnyOpaqueJson.class, decoded.fAny(), "JSON decode must yield AnyOpaqueJson");
        assertEquals((byte) 0x07, decoded.fAny().meta().kind());
        assertEquals((byte) 0x03, decoded.fDomainThis().meta().kind());
        assertEquals((byte) 0x01, decoded.fDomainCurrent().meta().kind());
        assertEquals((byte) 0x06, decoded.fUnderlying().meta().kind());
        assertEquals((byte) 0x02, decoded.fThisUnderlying().meta().kind());
        assertEquals((byte) 0x00, decoded.fCurrentUnderlying().meta().kind());
    }

    // ===== 3. Cross-format conversion via facade =====

    @Test
    void crossFormat_jsonHolderToUeba_decodeRoundTrip() throws Exception {
        // buildJsonHolderForCrossConvert uses AnyOpaqueJson branches for ALL fields with real Inner
        // JSON; encoding to UEBA forces jsonToUebaBytes per field. After decode the branches are
        // AnyOpaqueUeba. Re-encoding the now-Ueba-branched holder (no facade) must produce identical
        // bytes — proves the cross-converted bytes match a native UEBA encode of the same value.
        BaboonCodecsFacade facade = freshFacade();
        BaboonCodecContext ctxWithFacade = BaboonCodecContext.withFacade(false, facade);
        my.ok.Holder original = buildJsonHolderForCrossConvert();
        byte[] bytes = encodeUebaBytes(original, ctxWithFacade);
        my.ok.Holder decoded = decodeUebaBytes(bytes);

        byte[] rebytes = encodeUebaBytes(decoded, BaboonCodecContext.Compact);
        assertArrayEquals(bytes, rebytes, "JSON->UEBA cross-convert produced non-canonical bytes");
    }

    @Test
    void crossFormat_uebaHolderToJson_decodeRoundTrip() throws Exception {
        // buildUebaHolder uses AnyOpaqueUeba branches everywhere; encoding to JSON triggers
        // uebaToJson for each field. For untyped variants A/B/C the wire meta carries typeid;
        // we substitute typeid=INNER_TYPE so the registered Inner codec resolves and the bytes
        // deserialize as Inner. D variants resolve via static fallbacks emitted by codec gen.
        BaboonCodecsFacade facade = freshFacade();
        BaboonCodecContext ctxWithFacade = BaboonCodecContext.withFacade(false, facade);
        byte[] innerBytes = innerToUebaBytes(SAMPLE_INNER);
        my.ok.Holder base = buildUebaHolder();
        my.ok.Holder crossable = new my.ok.Holder(
            new AnyOpaqueUeba(new AnyMeta((byte) 0x07, DOMAIN_ID, VERSION_STR, INNER_TYPE), innerBytes),
            new AnyOpaqueUeba(new AnyMeta((byte) 0x03, null, VERSION_STR, INNER_TYPE), innerBytes),
            new AnyOpaqueUeba(new AnyMeta((byte) 0x01, null, null, INNER_TYPE), innerBytes),
            base.fUnderlying(),
            base.fThisUnderlying(),
            base.fCurrentUnderlying(),
            Optional.of(new AnyOpaqueUeba(new AnyMeta((byte) 0x07, DOMAIN_ID, VERSION_STR, INNER_TYPE), innerBytes)),
            base.fLst(),
            Map.of("k1", new AnyOpaqueUeba(new AnyMeta((byte) 0x07, DOMAIN_ID, VERSION_STR, INNER_TYPE), innerBytes)));
        JsonNode json = my.ok.Holder_JsonCodec.INSTANCE.encode(ctxWithFacade, crossable);
        // Sanity-decode the JSON to ensure the envelope is well-formed.
        my.ok.Holder decoded = my.ok.Holder_JsonCodec.INSTANCE.decode(BaboonCodecContext.Compact, json);
        assertInstanceOf(AnyOpaqueJson.class, decoded.fAny());
        assertEquals((byte) 0x07, decoded.fAny().meta().kind());
        assertEquals((byte) 0x06, decoded.fUnderlying().meta().kind());
        assertEquals((byte) 0x00, decoded.fCurrentUnderlying().meta().kind()); // D3: kind=0x00, statics filled
    }

    @Test
    void crossFormat_d3IsolatedField_staticFallbacksResolveEndToEnd() throws Exception {
        // PR-06-D01 (Java analog) regression: D3 has all-None meta on wire; the codec generator
        // emits (currentDomain, currentVersion, underlyingFqid) as static fallbacks. Without these
        // the facade cannot resolve and cross-convert fails.
        BaboonCodecsFacade facade = freshFacade();
        BaboonCodecContext ctxWithFacade = BaboonCodecContext.withFacade(false, facade);
        JsonNode innerJson = innerToJson(SAMPLE_INNER);
        my.ok.Holder base = buildUebaHolder();
        my.ok.Holder mixed = new my.ok.Holder(
            base.fAny(),
            base.fDomainThis(),
            base.fDomainCurrent(),
            base.fUnderlying(),
            base.fThisUnderlying(),
            new AnyOpaqueJson(metaD3(), innerJson),
            base.fOpt(),
            base.fLst(),
            base.fMapValue());
        // No throw on encode means jsonToUebaBytes succeeded for the D3 field (statics resolved).
        byte[] bytes = encodeUebaBytes(mixed, ctxWithFacade);
        my.ok.Holder decoded = decodeUebaBytes(bytes);
        assertEquals((byte) 0x00, decoded.fCurrentUnderlying().meta().kind());
        assertInstanceOf(AnyOpaqueUeba.class, decoded.fCurrentUnderlying());
        byte[] blob = ((AnyOpaqueUeba) decoded.fCurrentUnderlying()).bytes();
        LEDataInputStream in = new LEDataInputStream(new ByteArrayInputStream(blob));
        my.ok.Inner inner = my.ok.Inner_UEBACodec.INSTANCE.decode(BaboonCodecContext.Compact, in);
        assertEquals(SAMPLE_INNER, inner, "D3 cross-convert payload must decode as the original Inner");
    }

    // ===== 4. facade.decodeAny end-to-end =====

    @Test
    void decodeAny_resolvesUebaInnerToTypedInner() throws Exception {
        BaboonCodecsFacade facade = freshFacade();
        AnyMeta meta = new AnyMeta((byte) 0x07, DOMAIN_ID, VERSION_STR, INNER_TYPE);
        AnyOpaqueUeba opaque = new AnyOpaqueUeba(meta, innerToUebaBytes(SAMPLE_INNER));
        BaboonEither<BaboonCodecException, ?> result = facade.decodeAny(opaque);
        assertInstanceOf(BaboonEither.Right.class, result, "decodeAny must succeed: " + result);
        Object value = ((BaboonEither.Right<?, ?>) result).value();
        assertInstanceOf(my.ok.Inner.class, value, "decoded must be an Inner, got " + value.getClass().getSimpleName());
        assertEquals(SAMPLE_INNER, value);
    }

    @Test
    void decodeAny_resolvesJsonInnerToTypedInner() {
        BaboonCodecsFacade facade = freshFacade();
        AnyMeta meta = new AnyMeta((byte) 0x07, DOMAIN_ID, VERSION_STR, INNER_TYPE);
        AnyOpaqueJson opaque = new AnyOpaqueJson(meta, innerToJson(SAMPLE_INNER));
        BaboonEither<BaboonCodecException, ?> result = facade.decodeAny(opaque);
        assertInstanceOf(BaboonEither.Right.class, result, "decodeAny must succeed: " + result);
        Object value = ((BaboonEither.Right<?, ?>) result).value();
        assertInstanceOf(my.ok.Inner.class, value, "decoded must be an Inner, got " + value.getClass().getSimpleName());
        assertEquals(SAMPLE_INNER, value);
    }

    // ===== 5. Forward-compat: trailing meta-extension bytes inside meta-length window =====

    @Test
    void forwardCompat_extraMetaExtensionBytes_areSkippedOnUebaDecode() throws Exception {
        // Encode a Holder normally, then surgically patch the FIRST any-field's meta-length to
        // claim 5 extra bytes, and splice 5 bytes into the meta block. The decoder must consume
        // the meta, observe the gap (anyMetaLen - bytesRead), skip them, and continue parsing.
        my.ok.Holder original = buildUebaHolder();
        byte[] bytes = encodeUebaBytes(original, BaboonCodecContext.Compact);

        // Layout of the first any-field on the wire (Compact, useIndices=false):
        // [1 byte header][i32 anyLength][i32 anyMetaLen][... metaBytes ...][... blob ...]
        int headerLen = 1;
        int anyLengthOffset = headerLen;
        int anyMetaLenOffset = headerLen + 4;
        int anyMetaStartOffset = headerLen + 4 + 4;
        int origAnyLength = readI32Le(bytes, anyLengthOffset);
        int origAnyMetaLen = readI32Le(bytes, anyMetaLenOffset);

        byte[] extension = new byte[]{0x11, 0x22, 0x33, 0x44, 0x55};
        int newAnyMetaLen = origAnyMetaLen + extension.length;
        int newAnyLength = origAnyLength + extension.length;

        byte[] origMetaSlice = new byte[origAnyMetaLen];
        System.arraycopy(bytes, anyMetaStartOffset, origMetaSlice, 0, origAnyMetaLen);
        int origBlobAndRestStart = anyMetaStartOffset + origAnyMetaLen;
        byte[] origBlobAndRest = new byte[bytes.length - origBlobAndRestStart];
        System.arraycopy(bytes, origBlobAndRestStart, origBlobAndRest, 0, origBlobAndRest.length);

        ByteArrayOutputStream patched = new ByteArrayOutputStream();
        LEDataOutputStream pOut = new LEDataOutputStream(patched);
        pOut.writeByte(bytes[0]);
        pOut.writeInt(newAnyLength);
        pOut.writeInt(newAnyMetaLen);
        pOut.write(origMetaSlice);
        pOut.write(extension);
        pOut.write(origBlobAndRest);
        pOut.flush();
        byte[] patchedBytes = patched.toByteArray();

        my.ok.Holder decoded = decodeUebaBytes(patchedBytes);
        assertEquals(original, decoded, "forward-compat decode must structurally match original");
    }

    private static int readI32Le(byte[] data, int offset) {
        return (data[offset] & 0xFF)
            | ((data[offset + 1] & 0xFF) << 8)
            | ((data[offset + 2] & 0xFF) << 16)
            | ((data[offset + 3] & 0xFF) << 24);
    }

    // ===== 6. Fail-fast: missing-facade cross-convert =====

    @Test
    void encodeJsonAnyIntoUeba_withoutFacade_failsFast() throws Exception {
        my.ok.Holder base = buildUebaHolder();
        ObjectNode payload = NF.objectNode();
        payload.set("x", IntNode.valueOf(1));
        my.ok.Holder mixed = new my.ok.Holder(
            new AnyOpaqueJson(metaA(), payload),
            base.fDomainThis(),
            base.fDomainCurrent(),
            base.fUnderlying(),
            base.fThisUnderlying(),
            base.fCurrentUnderlying(),
            base.fOpt(),
            base.fLst(),
            base.fMapValue());
        BaboonCodecException.EncoderFailure ex = assertThrows(BaboonCodecException.EncoderFailure.class, () -> {
            ByteArrayOutputStream baos = new ByteArrayOutputStream();
            LEDataOutputStream out = new LEDataOutputStream(baos);
            my.ok.Holder_UEBACodec.INSTANCE.encode(BaboonCodecContext.Compact, out, mixed);
        });
        String msg = ex.getMessage() != null ? ex.getMessage() : "";
        assertTrue(msg.contains("facade"), "expected facade-mention error, got: " + msg);
    }

    @Test
    void encodeUebaAnyIntoJson_withoutFacade_failsFast() {
        my.ok.Holder base = buildJsonNativeHolder();
        my.ok.Holder mixed = new my.ok.Holder(
            new AnyOpaqueUeba(metaA(), new byte[]{1, 2}),
            base.fDomainThis(),
            base.fDomainCurrent(),
            base.fUnderlying(),
            base.fThisUnderlying(),
            base.fCurrentUnderlying(),
            base.fOpt(),
            base.fLst(),
            base.fMapValue());
        BaboonCodecException.EncoderFailure ex = assertThrows(BaboonCodecException.EncoderFailure.class, () -> {
            my.ok.Holder_JsonCodec.INSTANCE.encode(BaboonCodecContext.Compact, mixed);
        });
        String msg = ex.getMessage() != null ? ex.getMessage() : "";
        assertTrue(msg.contains("facade"), "expected facade-mention error, got: " + msg);
    }

    // ===== 7. JSON envelope shape lock-in =====

    @Test
    void jsonEnvelope_carriesAkAndOptionalAdAvAtAndContentKey() {
        // Sanity: the JSON envelope produced by the codec embeds the AnyMeta keys ($ak, $ad?,
        // $av?, $at?) alongside the $c content key. Any change to the envelope that drops one of
        // these would break cross-language interop.
        my.ok.Holder original = buildJsonNativeHolder();
        JsonNode token = my.ok.Holder_JsonCodec.INSTANCE.encode(BaboonCodecContext.Compact, original);
        ObjectNode obj = (ObjectNode) token;

        // fAny variant A → all four meta keys + $c present.
        ObjectNode anyField = (ObjectNode) obj.get("fAny");
        assertNotNull(anyField.get("$ak"));
        assertNotNull(anyField.get("$ad"));
        assertNotNull(anyField.get("$av"));
        assertNotNull(anyField.get("$at"));
        assertNotNull(anyField.get("$c"));
        assertEquals(0x07, anyField.get("$ak").intValue());

        // fCurrentUnderlying variant D3 → only $ak + $c (kind 0x00, no other meta on wire).
        ObjectNode d3 = (ObjectNode) obj.get("fCurrentUnderlying");
        assertNotNull(d3.get("$ak"));
        assertNotNull(d3.get("$c"));
        assertNull(d3.get("$ad"));
        assertNull(d3.get("$av"));
        assertNull(d3.get("$at"));
        assertEquals(0x00, d3.get("$ak").intValue());

        // Sanity: the envelope keys appear exactly as documented (regression-proof key list).
        java.util.Set<String> expected = java.util.Set.of("$ak", "$ad", "$av", "$at", "$c");
        java.util.Set<String> present = new java.util.HashSet<>();
        anyField.fieldNames().forEachRemaining(present::add);
        assertEquals(expected, present, "fAny envelope must expose exactly $ak/$ad/$av/$at/$c keys");
    }
}
