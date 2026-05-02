// NOTE: This test references generated runtime symbols (AnyMeta, AnyMetaCodec,
// BaboonCodecsFacade, BaboonCodecException, ...) AND generated DTO/codec symbols
// (my.ok.Holder, my.ok.Inner, ...) which are copied/generated into this stub
// only by `mdl :build :test-gen-regular-adt` (rsync + codegen into
// target/test-regular/kt-stub/). Running `gradle test` directly from the source
// tree may fail with missing symbols; run the test suite from the codegen'd copy.
//
// Round-trip and cross-format tests for `any` fields (issue #69 PR 5.4 / M5 close).
// Mirrors Scala's AnyRoundTripSpec (PR 2.4) / C# AnyRoundTripTests (PR 3.4) /
// Rust any_round_trip_tests (PR 4.3). Exercises the `any-ok` fixture's six DSL
// variants (A=any, B=any[domain:this], C=any[domain:current], D1=any[Inner],
// D2=any[domain:this,Inner], D3=any[domain:current,Inner]) plus the three nested
// positions (opt/lst/map-value).
package runtime

import baboon.runtime.shared.AnyMeta
import baboon.runtime.shared.AnyOpaque
import baboon.runtime.shared.AnyOpaqueJson
import baboon.runtime.shared.AnyOpaqueUeba
import baboon.runtime.shared.BaboonCodecContext
import baboon.runtime.shared.BaboonCodecException
import baboon.runtime.shared.BaboonCodecsFacade
import baboon.runtime.shared.BaboonDomainVersion
import baboon.runtime.shared.Either
import baboon.runtime.shared.LEDataInputStream
import baboon.runtime.shared.LEDataOutputStream
import kotlinx.serialization.json.JsonElement
import kotlinx.serialization.json.JsonObject
import kotlinx.serialization.json.JsonPrimitive
import kotlinx.serialization.json.buildJsonObject
import kotlinx.serialization.json.put
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertNotNull
import org.junit.jupiter.api.Assertions.assertNull
import org.junit.jupiter.api.Assertions.assertThrows
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.Test
import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream

class AnyRoundTripTest {
    // Fresh per-test facade: registers Holder/Inner codecs from the my.ok domain so the
    // cross-format helpers and decodeAny can resolve `(domain, version, typeid)` triples.
    private fun freshFacade(): BaboonCodecsFacade {
        val f = BaboonCodecsFacade()
        f.register(
            BaboonDomainVersion(my.ok.Holder.baboonDomainIdentifier, my.ok.Holder.baboonDomainVersion),
            { my.ok.BaboonCodecsJson },
            { my.ok.BaboonCodecsUeba },
            { my.ok.BaboonMetadata },
        )
        return f
    }

    // Domain/version/typeid strings the codec generator emits as static fallbacks.
    private val domainId = "my.ok"
    private val versionStr = "1.0.0"
    private val innerType = "my.ok/:#Inner"

    // Construct AnyMeta for each variant, populating only the bits the kind byte claims.
    private fun metaA(): AnyMeta = AnyMeta(0x07.toByte(), domainId, versionStr, "opaque.Type")
    private fun metaB(): AnyMeta = AnyMeta(0x03.toByte(), null, versionStr, "opaque.Type")
    private fun metaC(): AnyMeta = AnyMeta(0x01.toByte(), null, null, "opaque.Type")
    private fun metaD1(): AnyMeta = AnyMeta(0x06.toByte(), domainId, versionStr, null)
    private fun metaD2(): AnyMeta = AnyMeta(0x02.toByte(), null, versionStr, null)
    private fun metaD3(): AnyMeta = AnyMeta(0x00.toByte(), null, null, null)

    private val sampleInner: my.ok.Inner = my.ok.Inner(42)

    // Encode an Inner via the generated UEBA codec — used when constructing AnyOpaqueUeba payloads
    // so cross-convert tests have a real Inner to deserialize.
    private fun innerToUebaBytes(inner: my.ok.Inner): ByteArray {
        val baos = ByteArrayOutputStream()
        val out = LEDataOutputStream(baos)
        my.ok.Inner_UEBACodec.instance.encode(BaboonCodecContext.Compact, out, inner)
        out.flush()
        return baos.toByteArray()
    }

    private fun innerToJson(inner: my.ok.Inner): JsonElement {
        return my.ok.Inner_JsonCodec.encode(BaboonCodecContext.Compact, inner)
    }

    // Build a complete Holder with one AnyOpaqueUeba per variant. UEBA round-trips natively
    // (no facade needed for encode/decode — facade is only consulted for cross-convert).
    private fun buildUebaHolder(): my.ok.Holder {
        val innerBytes = innerToUebaBytes(sampleInner)
        return my.ok.Holder(
            fAny = AnyOpaqueUeba(metaA(), byteArrayOf(1, 2, 3)),
            fDomainThis = AnyOpaqueUeba(metaB(), byteArrayOf(4, 5)),
            fDomainCurrent = AnyOpaqueUeba(metaC(), byteArrayOf(6)),
            fUnderlying = AnyOpaqueUeba(metaD1(), innerBytes),
            fThisUnderlying = AnyOpaqueUeba(metaD2(), innerBytes),
            fCurrentUnderlying = AnyOpaqueUeba(metaD3(), innerBytes),
            fOpt = AnyOpaqueUeba(metaA(), byteArrayOf(7)),
            fLst = listOf(AnyOpaqueUeba(metaD1(), innerBytes)),
            fMapValue = mapOf("k1" to AnyOpaqueUeba(metaA(), byteArrayOf(8))),
        )
    }

    // Build a Holder using AnyOpaqueJson branches everywhere with arbitrary inner JSON content.
    // Used as the "all native JSON branch" baseline for JSON round-trip tests.
    private fun buildJsonNativeHolder(): my.ok.Holder {
        val arbitraryJson = buildJsonObject { put("payload", 42) }
        val innerJson = innerToJson(sampleInner)
        return my.ok.Holder(
            fAny = AnyOpaqueJson(metaA(), arbitraryJson),
            fDomainThis = AnyOpaqueJson(metaB(), arbitraryJson),
            fDomainCurrent = AnyOpaqueJson(metaC(), arbitraryJson),
            fUnderlying = AnyOpaqueJson(metaD1(), innerJson),
            fThisUnderlying = AnyOpaqueJson(metaD2(), innerJson),
            fCurrentUnderlying = AnyOpaqueJson(metaD3(), innerJson),
            fOpt = AnyOpaqueJson(metaA(), arbitraryJson),
            fLst = listOf(AnyOpaqueJson(metaD1(), innerJson)),
            fMapValue = mapOf("k1" to AnyOpaqueJson(metaA(), arbitraryJson)),
        )
    }

    // Build a Holder using AnyOpaqueJson branches with REAL Inner JSON for D variants and
    // typeid=innerType for A/B/C so cross-convert can resolve Inner via the registered facade.
    private fun buildJsonHolderForCrossConvert(): my.ok.Holder {
        val innerJson = innerToJson(sampleInner)
        return my.ok.Holder(
            fAny = AnyOpaqueJson(AnyMeta(0x07.toByte(), domainId, versionStr, innerType), innerJson),
            fDomainThis = AnyOpaqueJson(AnyMeta(0x03.toByte(), null, versionStr, innerType), innerJson),
            fDomainCurrent = AnyOpaqueJson(AnyMeta(0x01.toByte(), null, null, innerType), innerJson),
            fUnderlying = AnyOpaqueJson(metaD1(), innerJson),
            fThisUnderlying = AnyOpaqueJson(metaD2(), innerJson),
            fCurrentUnderlying = AnyOpaqueJson(metaD3(), innerJson),
            fOpt = AnyOpaqueJson(AnyMeta(0x07.toByte(), domainId, versionStr, innerType), innerJson),
            fLst = listOf(AnyOpaqueJson(metaD1(), innerJson)),
            fMapValue = mapOf("k1" to AnyOpaqueJson(AnyMeta(0x07.toByte(), domainId, versionStr, innerType), innerJson)),
        )
    }

    private fun encodeUebaBytes(value: my.ok.Holder, ctx: BaboonCodecContext): ByteArray {
        val baos = ByteArrayOutputStream()
        val out = LEDataOutputStream(baos)
        my.ok.Holder_UEBACodec.instance.encode(ctx, out, value)
        out.flush()
        return baos.toByteArray()
    }

    private fun decodeUebaBytes(bytes: ByteArray, ctx: BaboonCodecContext = BaboonCodecContext.Compact): my.ok.Holder {
        val ins = LEDataInputStream(ByteArrayInputStream(bytes))
        return my.ok.Holder_UEBACodec.instance.decode(ctx, ins)
    }

    // ===== 1. Per-variant UEBA round-trip =====

    @Test
    fun ueba_round_trip_allSixVariantsPlusNestedPositions_preserveContent() {
        val original = buildUebaHolder()
        val bytes = encodeUebaBytes(original, BaboonCodecContext.Compact)
        val decoded = decodeUebaBytes(bytes)
        assertEquals(original, decoded)
    }

    @Test
    fun ueba_round_trip_withUseIndicesTrue_preservesContent() {
        val original = buildUebaHolder()
        val bytes = encodeUebaBytes(original, BaboonCodecContext.Indexed)
        // Discriminator: broken emission produces only header bytes, well below 16.
        assertTrue(bytes.size > 16, "Indexed-encoded body must contain real field content; got ${bytes.size} bytes")
        val decoded = decodeUebaBytes(bytes, BaboonCodecContext.Indexed)
        assertEquals(original, decoded)
    }

    @Test
    fun ueba_decode_yieldsAnyOpaqueUebaWithMatchingKindBytes() {
        val original = buildUebaHolder()
        val bytes = encodeUebaBytes(original, BaboonCodecContext.Compact)
        val decoded = decodeUebaBytes(bytes)
        assertEquals(0x07.toByte(), decoded.fAny.meta.kind, "fAny variant A")
        assertEquals(0x03.toByte(), decoded.fDomainThis.meta.kind, "fDomainThis variant B")
        assertEquals(0x01.toByte(), decoded.fDomainCurrent.meta.kind, "fDomainCurrent variant C")
        assertEquals(0x06.toByte(), decoded.fUnderlying.meta.kind, "fUnderlying variant D1")
        assertEquals(0x02.toByte(), decoded.fThisUnderlying.meta.kind, "fThisUnderlying variant D2")
        assertEquals(0x00.toByte(), decoded.fCurrentUnderlying.meta.kind, "fCurrentUnderlying variant D3")
        assertTrue(decoded.fAny is AnyOpaqueUeba, "UEBA decode must yield AnyOpaqueUeba")
    }

    // ===== 2. Per-variant JSON round-trip =====

    @Test
    fun json_round_trip_allSixVariantsPlusNestedPositions_preserveContent() {
        val original = buildJsonNativeHolder()
        val json = my.ok.Holder_JsonCodec.encode(BaboonCodecContext.Compact, original)
        val decoded = my.ok.Holder_JsonCodec.decode(BaboonCodecContext.Compact, json)
        assertEquals(original, decoded)
    }

    @Test
    fun json_decode_yieldsAnyOpaqueJsonWithMatchingKindBytes() {
        val original = buildJsonNativeHolder()
        val json = my.ok.Holder_JsonCodec.encode(BaboonCodecContext.Compact, original)
        val decoded = my.ok.Holder_JsonCodec.decode(BaboonCodecContext.Compact, json)
        assertTrue(decoded.fAny is AnyOpaqueJson, "JSON decode must yield AnyOpaqueJson")
        assertEquals(0x07.toByte(), decoded.fAny.meta.kind)
        assertEquals(0x03.toByte(), decoded.fDomainThis.meta.kind)
        assertEquals(0x01.toByte(), decoded.fDomainCurrent.meta.kind)
        assertEquals(0x06.toByte(), decoded.fUnderlying.meta.kind)
        assertEquals(0x02.toByte(), decoded.fThisUnderlying.meta.kind)
        assertEquals(0x00.toByte(), decoded.fCurrentUnderlying.meta.kind)
    }

    // ===== 3. Cross-format conversion via facade =====

    @Test
    fun crossFormat_jsonHolderToUeba_decodeRoundTrip() {
        // buildJsonHolderForCrossConvert uses AnyOpaqueJson branches for ALL fields with real Inner
        // JSON; encoding to UEBA forces jsonToUebaBytes per field. After decode the branches are
        // AnyOpaqueUeba. Re-encoding the now-Ueba-branched holder (no facade) must produce identical
        // bytes — proves the cross-converted bytes match a native UEBA encode of the same value.
        val facade = freshFacade()
        val ctxWithFacade = BaboonCodecContext.withFacade(useIndices = false, baboonFacade = facade)
        val original = buildJsonHolderForCrossConvert()
        val bytes = encodeUebaBytes(original, ctxWithFacade)
        val decoded = decodeUebaBytes(bytes)

        val rebytes = encodeUebaBytes(decoded, BaboonCodecContext.Compact)
        assertTrue(bytes.contentEquals(rebytes), "JSON->UEBA cross-convert produced non-canonical bytes")
    }

    @Test
    fun crossFormat_uebaHolderToJson_decodeRoundTrip() {
        // buildUebaHolder uses AnyOpaqueUeba branches everywhere; encoding to JSON triggers
        // uebaToJson for each field. For untyped variants A/B/C the wire meta carries typeid;
        // we substitute typeid=innerType so the registered Inner codec resolves and the bytes
        // deserialize as Inner. D variants resolve via static fallbacks emitted by codec gen.
        val facade = freshFacade()
        val ctxWithFacade = BaboonCodecContext.withFacade(useIndices = false, baboonFacade = facade)
        val innerBytes = innerToUebaBytes(sampleInner)
        val crossable = buildUebaHolder().copy(
            fAny = AnyOpaqueUeba(AnyMeta(0x07.toByte(), domainId, versionStr, innerType), innerBytes),
            fDomainThis = AnyOpaqueUeba(AnyMeta(0x03.toByte(), null, versionStr, innerType), innerBytes),
            fDomainCurrent = AnyOpaqueUeba(AnyMeta(0x01.toByte(), null, null, innerType), innerBytes),
            fOpt = AnyOpaqueUeba(AnyMeta(0x07.toByte(), domainId, versionStr, innerType), innerBytes),
            fMapValue = mapOf("k1" to AnyOpaqueUeba(AnyMeta(0x07.toByte(), domainId, versionStr, innerType), innerBytes)),
        )
        val json = my.ok.Holder_JsonCodec.encode(ctxWithFacade, crossable)
        // Sanity-decode the JSON to ensure the envelope is well-formed.
        val decoded = my.ok.Holder_JsonCodec.decode(BaboonCodecContext.Compact, json)
        assertTrue(decoded.fAny is AnyOpaqueJson)
        assertEquals(0x07.toByte(), decoded.fAny.meta.kind)
        assertEquals(0x06.toByte(), decoded.fUnderlying.meta.kind)
        assertEquals(0x00.toByte(), decoded.fCurrentUnderlying.meta.kind) // D3: kind=0x00, statics filled
    }

    @Test
    fun crossFormat_d3IsolatedField_staticFallbacksResolveEndToEnd() {
        // PR-06-D01 (Kotlin analog) regression: D3 has all-None meta on wire; the codec generator
        // emits (currentDomain, currentVersion, underlyingFqid) as static fallbacks. Without these
        // the facade cannot resolve and cross-convert fails.
        val facade = freshFacade()
        val ctxWithFacade = BaboonCodecContext.withFacade(useIndices = false, baboonFacade = facade)
        val innerJson = innerToJson(sampleInner)
        val mixed = buildUebaHolder().copy(
            fCurrentUnderlying = AnyOpaqueJson(metaD3(), innerJson),
        )
        // No throw on encode means jsonToUebaBytes succeeded for the D3 field (statics resolved).
        val bytes = encodeUebaBytes(mixed, ctxWithFacade)
        val decoded = decodeUebaBytes(bytes)
        assertEquals(0x00.toByte(), decoded.fCurrentUnderlying.meta.kind)
        assertTrue(decoded.fCurrentUnderlying is AnyOpaqueUeba)
        val blob = (decoded.fCurrentUnderlying as AnyOpaqueUeba).bytes
        val ins = LEDataInputStream(ByteArrayInputStream(blob))
        val inner = my.ok.Inner_UEBACodec.instance.decode(BaboonCodecContext.Compact, ins)
        assertEquals(sampleInner, inner, "D3 cross-convert payload must decode as the original Inner")
    }

    // ===== 4. facade.decodeAny end-to-end =====

    @Test
    fun decodeAny_resolvesUebaInnerToTypedInner() {
        val facade = freshFacade()
        val meta = AnyMeta(0x07.toByte(), domainId, versionStr, innerType)
        val opaque = AnyOpaqueUeba(meta, innerToUebaBytes(sampleInner))
        val result = facade.decodeAny(opaque)
        assertTrue(result is Either.Right, "decodeAny must succeed: $result")
        val value = (result as Either.Right).value
        assertTrue(value is my.ok.Inner, "decoded must be an Inner, got ${value::class.simpleName}")
        assertEquals(sampleInner, value)
    }

    @Test
    fun decodeAny_resolvesJsonInnerToTypedInner() {
        val facade = freshFacade()
        val meta = AnyMeta(0x07.toByte(), domainId, versionStr, innerType)
        val opaque = AnyOpaqueJson(meta, innerToJson(sampleInner))
        val result = facade.decodeAny(opaque)
        assertTrue(result is Either.Right, "decodeAny must succeed: $result")
        val value = (result as Either.Right).value
        assertTrue(value is my.ok.Inner, "decoded must be an Inner, got ${value::class.simpleName}")
        assertEquals(sampleInner, value)
    }

    // ===== 5. Forward-compat: trailing meta-extension bytes inside meta-length window =====

    @Test
    fun forwardCompat_extraMetaExtensionBytes_areSkippedOnUebaDecode() {
        // Encode a Holder normally, then surgically patch the FIRST any-field's meta-length to
        // claim 5 extra bytes, and splice 5 bytes into the meta block. The decoder must consume
        // the meta, observe the gap (anyMetaLen - bytesRead), skip them, and continue parsing.
        val original = buildUebaHolder()
        val bytes = encodeUebaBytes(original, BaboonCodecContext.Compact)

        // Layout of the first any-field on the wire (Compact, useIndices=false):
        // [1 byte header][i32 anyLength][i32 anyMetaLen][... metaBytes ...][... blob ...]
        val headerLen = 1
        val anyLengthOffset = headerLen
        val anyMetaLenOffset = headerLen + 4
        val anyMetaStartOffset = headerLen + 4 + 4
        val origAnyLength = readI32Le(bytes, anyLengthOffset)
        val origAnyMetaLen = readI32Le(bytes, anyMetaLenOffset)

        val extension = byteArrayOf(0x11, 0x22, 0x33, 0x44, 0x55)
        val newAnyMetaLen = origAnyMetaLen + extension.size
        val newAnyLength = origAnyLength + extension.size

        val origMetaSlice = bytes.copyOfRange(anyMetaStartOffset, anyMetaStartOffset + origAnyMetaLen)
        val origBlobAndRestStart = anyMetaStartOffset + origAnyMetaLen
        val origBlobAndRest = bytes.copyOfRange(origBlobAndRestStart, bytes.size)

        val patched = ByteArrayOutputStream()
        val pOut = LEDataOutputStream(patched)
        pOut.writeByte(bytes[0].toInt())
        pOut.writeInt(newAnyLength)
        pOut.writeInt(newAnyMetaLen)
        pOut.write(origMetaSlice)
        pOut.write(extension)
        pOut.write(origBlobAndRest)
        pOut.flush()
        val patchedBytes = patched.toByteArray()

        val decoded = decodeUebaBytes(patchedBytes)
        assertEquals(original, decoded, "forward-compat decode must structurally match original")
    }

    private fun readI32Le(data: ByteArray, offset: Int): Int {
        return (data[offset].toInt() and 0xFF) or
                ((data[offset + 1].toInt() and 0xFF) shl 8) or
                ((data[offset + 2].toInt() and 0xFF) shl 16) or
                ((data[offset + 3].toInt() and 0xFF) shl 24)
    }

    // ===== 6. Fail-fast: missing-facade cross-convert =====

    @Test
    fun encodeJsonAnyIntoUeba_withoutFacade_failsFast() {
        val mixed = buildUebaHolder().copy(
            fAny = AnyOpaqueJson(metaA(), buildJsonObject { put("x", 1) }),
        )
        val ex = assertThrows(BaboonCodecException.EncoderFailure::class.java) {
            val baos = ByteArrayOutputStream()
            val out = LEDataOutputStream(baos)
            my.ok.Holder_UEBACodec.instance.encode(BaboonCodecContext.Compact, out, mixed)
        }
        val msg = ex.message ?: ""
        assertTrue(msg.contains("facade"), "expected facade-mention error, got: $msg")
    }

    @Test
    fun encodeUebaAnyIntoJson_withoutFacade_failsFast() {
        val mixed = buildJsonNativeHolder().copy(
            fAny = AnyOpaqueUeba(metaA(), byteArrayOf(1, 2)),
        )
        val ex = assertThrows(BaboonCodecException.EncoderFailure::class.java) {
            my.ok.Holder_JsonCodec.encode(BaboonCodecContext.Compact, mixed)
        }
        val msg = ex.message ?: ""
        assertTrue(msg.contains("facade"), "expected facade-mention error, got: $msg")
    }

    // ===== 7. JSON envelope shape lock-in =====

    @Test
    fun jsonEnvelope_carriesAkAndOptionalAdAvAtAndContentKey() {
        // Sanity: the JSON envelope produced by the codec embeds the AnyMeta keys ($ak, $ad?,
        // $av?, $at?) alongside the $c content key. Any change to the envelope that drops one of
        // these would break cross-language interop.
        val original = buildJsonNativeHolder()
        val token = my.ok.Holder_JsonCodec.encode(BaboonCodecContext.Compact, original)
        val obj = token as JsonObject

        // fAny variant A → all four keys + $c present.
        val anyField = obj["fAny"] as JsonObject
        assertNotNull(anyField["\$ak"])
        assertNotNull(anyField["\$ad"])
        assertNotNull(anyField["\$av"])
        assertNotNull(anyField["\$at"])
        assertNotNull(anyField["\$c"])
        assertEquals(0x07, (anyField["\$ak"] as JsonPrimitive).content.toInt())

        // fCurrentUnderlying variant D3 → only $ak + $c (kind 0x00, no other meta on wire).
        val d3 = obj["fCurrentUnderlying"] as JsonObject
        assertNotNull(d3["\$ak"])
        assertNotNull(d3["\$c"])
        assertNull(d3["\$ad"])
        assertNull(d3["\$av"])
        assertNull(d3["\$at"])
        assertEquals(0x00, (d3["\$ak"] as JsonPrimitive).content.toInt())

        // Sanity: the envelope keys appear exactly as documented (regression-proof key list).
        val expected = setOf("\$ak", "\$ad", "\$av", "\$at", "\$c")
        val present = anyField.keys
        assertEquals(expected, present, "fAny envelope must expose exactly \$ak/\$ad/\$av/\$at/\$c keys")
    }
}
