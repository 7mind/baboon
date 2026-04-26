// NOTE: This test references generated runtime symbols (AnyMeta, AnyMetaCodec,
// BaboonCodecsFacade, BaboonCodecException, ...) which are copied into this stub
// only by the kt-stub codegen path (rsync + codegen into target/test-regular/kt-stub/).
// Running `gradle test` directly from the source tree may fail with missing symbols;
// run the test suite from the codegen'd copy.
package runtime

import baboon.runtime.shared.AbstractBaboonJsonCodecs
import baboon.runtime.shared.AbstractBaboonUebaCodecs
import baboon.runtime.shared.AnyMeta
import baboon.runtime.shared.AnyMetaCodec
import baboon.runtime.shared.AnyOpaqueJson
import baboon.runtime.shared.AnyOpaqueUeba
import baboon.runtime.shared.BaboonCodecContext
import baboon.runtime.shared.BaboonCodecException
import baboon.runtime.shared.BaboonDomainVersion
import baboon.runtime.shared.BaboonCodecsFacade
import baboon.runtime.shared.BaboonMeta
import baboon.runtime.shared.Either
import baboon.runtime.shared.LEDataInputStream
import baboon.runtime.shared.LEDataOutputStream
import kotlinx.serialization.json.JsonObject
import kotlinx.serialization.json.JsonPrimitive
import kotlinx.serialization.json.buildJsonObject
import kotlinx.serialization.json.put
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertFalse
import org.junit.jupiter.api.Assertions.assertNotEquals
import org.junit.jupiter.api.Assertions.assertNotNull
import org.junit.jupiter.api.Assertions.assertSame
import org.junit.jupiter.api.Assertions.assertThrows
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.Test
import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream

class AnyMetaCodecTest {
    // (kind, domain?, version?, typeid?) for all six locked meta-kind bytes.
    private val cases: List<Quad<Byte, String?, String?, String?>> = listOf(
        Quad(0x07.toByte(), "com.example.dom", "1.2.3", "MyType"), // A
        Quad(0x03.toByte(), null, "1.2.3", "MyType"),               // B
        Quad(0x01.toByte(), null, null, "MyType"),                  // C
        Quad(0x06.toByte(), "com.example.dom", "1.2.3", null),      // D1
        Quad(0x02.toByte(), null, "1.2.3", null),                   // D2
        Quad(0x00.toByte(), null, null, null),                      // D3
    )

    private data class Quad<A, B, C, D>(val a: A, val b: B, val c: C, val d: D)

    // ===== AnyMeta construction invariants =====

    @Test
    fun anyMeta_kind0x07_requiresDomainPresent() {
        assertThrows(IllegalArgumentException::class.java) {
            AnyMeta(0x07.toByte(), null, "v", "t")
        }
    }

    @Test
    fun anyMeta_kind0x03_requiresDomainAbsent() {
        assertThrows(IllegalArgumentException::class.java) {
            AnyMeta(0x03.toByte(), "d", "v", "t")
        }
    }

    @Test
    fun anyMeta_kind0x06_requiresTypeidAbsent() {
        assertThrows(IllegalArgumentException::class.java) {
            AnyMeta(0x06.toByte(), "d", "v", "t")
        }
    }

    @Test
    fun anyMeta_rejectsReservedKind0x04() {
        assertThrows(IllegalArgumentException::class.java) {
            AnyMeta(0x04.toByte(), "d", null, null)
        }
    }

    @Test
    fun anyMeta_rejectsReservedKind0x05() {
        assertThrows(IllegalArgumentException::class.java) {
            AnyMeta(0x05.toByte(), "d", null, "t")
        }
    }

    // ===== Binary round-trips =====

    @Test
    fun anyMetaCodec_writeBin_readBin_roundTripsAcrossAllSixKinds() {
        for ((kind, domain, version, typeid) in cases) {
            val meta = AnyMeta(kind, domain, version, typeid)
            val baos = ByteArrayOutputStream()
            val out = LEDataOutputStream(baos)
            AnyMetaCodec.writeBin(meta, out)
            out.flush()

            val reader = LEDataInputStream(ByteArrayInputStream(baos.toByteArray()))
            val round = AnyMetaCodec.readBin(reader)
            assertEquals(meta, round, "binary round-trip failed for kind 0x${(kind.toInt() and 0xFF).toString(16)}")
        }
    }

    @Test
    fun anyMetaCodec_writeBin_kind0x07_emits7Bytes() {
        // 1 byte kind + (1 byte ULEB128 length + 1 byte UTF-8) × 3 = 7 bytes.
        val meta = AnyMeta(0x07.toByte(), "a", "b", "c")
        val baos = ByteArrayOutputStream()
        val out = LEDataOutputStream(baos)
        AnyMetaCodec.writeBin(meta, out)
        out.flush()
        assertEquals(7, baos.toByteArray().size)
    }

    @Test
    fun anyMetaCodec_writeBin_kind0x00_emitsOneByte() {
        val meta = AnyMeta(0x00.toByte(), null, null, null)
        val baos = ByteArrayOutputStream()
        val out = LEDataOutputStream(baos)
        AnyMetaCodec.writeBin(meta, out)
        out.flush()
        val bytes = baos.toByteArray()
        assertEquals(1, bytes.size)
        assertEquals(0x00.toByte(), bytes[0])
    }

    @Test
    fun anyMetaCodec_writeBin_nonAsciiUtf8_roundTrips() {
        val meta = AnyMeta(0x07.toByte(), "日本語", "1.2.3", "タイプ")
        val baos = ByteArrayOutputStream()
        val out = LEDataOutputStream(baos)
        AnyMetaCodec.writeBin(meta, out)
        out.flush()
        val reader = LEDataInputStream(ByteArrayInputStream(baos.toByteArray()))
        val round = AnyMetaCodec.readBin(reader)
        assertEquals(meta, round)
    }

    @Test
    fun anyMetaCodec_writeBin_emptyString_roundTrips() {
        val meta = AnyMeta(0x01.toByte(), null, null, "")
        val baos = ByteArrayOutputStream()
        val out = LEDataOutputStream(baos)
        AnyMetaCodec.writeBin(meta, out)
        out.flush()
        val bytes = baos.toByteArray()
        // 1 (kind) + 1 (ULEB128 length 0) = 2 bytes
        assertEquals(2, bytes.size)
        assertEquals(0x01.toByte(), bytes[0])
        assertEquals(0x00.toByte(), bytes[1])

        val reader = LEDataInputStream(ByteArrayInputStream(bytes))
        val round = AnyMetaCodec.readBin(reader)
        assertEquals(meta, round)
    }

    @Test
    fun anyMetaCodec_writeBin_128ByteString_roundTripsWithMultiByteUleb() {
        // 128 = 0x80 0x01 in ULEB128 (multi-byte length prefix).
        val longStr = "a".repeat(128)
        val meta = AnyMeta(0x01.toByte(), null, null, longStr)
        val baos = ByteArrayOutputStream()
        val out = LEDataOutputStream(baos)
        AnyMetaCodec.writeBin(meta, out)
        out.flush()
        val bytes = baos.toByteArray()
        // 1 (kind) + 2 (ULEB128) + 128 (UTF-8) = 131
        assertEquals(131, bytes.size)
        assertEquals(0x01.toByte(), bytes[0])
        assertEquals(0x80.toByte(), bytes[1])
        assertEquals(0x01.toByte(), bytes[2])

        val reader = LEDataInputStream(ByteArrayInputStream(bytes))
        val round = AnyMetaCodec.readBin(reader)
        assertEquals(meta, round)
    }

    @Test
    fun anyMetaCodec_readBinWithLength_reportsBytesConsumed_andTolersTrailingExtensionBytes() {
        // Write a valid kind 0x07 meta, then append 5 garbage bytes that the on-wire
        // meta-length window would claim are part of the meta block.
        val meta = AnyMeta(0x07.toByte(), "d", "v", "t")
        val baos = ByteArrayOutputStream()
        val out = LEDataOutputStream(baos)
        AnyMetaCodec.writeBin(meta, out)
        out.flush()
        val metaBytes = baos.toByteArray()
        val tail = byteArrayOf(0x11, 0x22, 0x33, 0x44, 0x55)
        val combined = metaBytes + tail

        val reader = LEDataInputStream(ByteArrayInputStream(combined))
        val (parsed, bytesRead) = AnyMetaCodec.readBinWithLength(reader)
        assertEquals(meta, parsed)
        assertEquals(metaBytes.size, bytesRead)

        // Caller would skip (anyMetaLen - bytesRead) bytes, which equals tail.size here.
        val anyMetaLen = metaBytes.size + tail.size
        val skipBytes = anyMetaLen - bytesRead
        val skipped = ByteArray(skipBytes)
        reader.readFully(skipped)
        assertEquals(tail.size, skipped.size)
        assertTrue(skipped.contentEquals(tail))
    }

    // ===== JSON round-trips =====

    @Test
    fun anyMetaCodec_writeJson_readJson_roundTripsAcrossAllSixKinds() {
        for ((kind, domain, version, typeid) in cases) {
            val meta = AnyMeta(kind, domain, version, typeid)
            val json = AnyMetaCodec.writeJson(meta)
            val round = AnyMetaCodec.readJson(json)
            assertTrue(
                round is Either.Right,
                "JSON read failed for kind 0x${(kind.toInt() and 0xFF).toString(16)}: $round"
            )
            assertEquals(meta, (round as Either.Right).value, "JSON round-trip failed for kind 0x${(kind.toInt() and 0xFF).toString(16)}")
        }
    }

    @Test
    fun anyMetaCodec_writeJson_alwaysReturnsObjectAcrossAllSixKinds() {
        // Locks the encoder envelope invariant: a future change that drops the JsonObject shape
        // would break this and avoid the silent-no-op bug PR-06-D08 calls out.
        for ((kind, domain, version, typeid) in cases) {
            val meta = AnyMeta(kind, domain, version, typeid)
            val json = AnyMetaCodec.writeJson(meta)
            assertTrue(
                json is JsonObject,
                "writeJson must return JsonObject for kind 0x${(kind.toInt() and 0xFF).toString(16)}, got ${json::class.simpleName}"
            )
        }
    }

    @Test
    fun anyMetaCodec_readJson_rejectsMissingRequiredField() {
        // kind 0x07 (all bits) but missing $ad.
        val bad = buildJsonObject {
            put("\$ak", 0x07)
            put("\$av", "v")
            put("\$at", "t")
        }
        val result = AnyMetaCodec.readJson(bad)
        assertTrue(result is Either.Left)
        val msg = (result as Either.Left).value.message ?: ""
        assertTrue(msg.contains("\$ad"), "expected message to mention \$ad, got: $msg")
    }

    @Test
    fun anyMetaCodec_readJson_rejectsForbiddenField() {
        // kind 0x00 (no bits) but extra $at present.
        val bad = buildJsonObject {
            put("\$ak", 0x00)
            put("\$at", "t")
        }
        val result = AnyMetaCodec.readJson(bad)
        assertTrue(result is Either.Left)
        val msg = (result as Either.Left).value.message ?: ""
        assertTrue(msg.contains("\$at"), "expected message to mention \$at, got: $msg")
    }

    @Test
    fun anyMetaCodec_readJson_rejectsNonNumericKind() {
        val bad = buildJsonObject {
            put("\$ak", "not-a-number")
            put("\$at", "T")
        }
        val result = AnyMetaCodec.readJson(bad)
        assertTrue(result is Either.Left)
        val msg = (result as Either.Left).value.message ?: ""
        assertTrue(msg.contains("\$ak"), "expected message to mention \$ak, got: $msg")
    }

    // ===== AnyOpaqueUeba content equality (PR-05-D08) =====

    @Test
    fun anyOpaqueUeba_equalityIsContentBased() {
        val meta = AnyMeta(0x07.toByte(), "d", "v", "t")
        val a = AnyOpaqueUeba(meta, byteArrayOf(1, 2, 3))
        val b = AnyOpaqueUeba(meta, byteArrayOf(1, 2, 3))
        assertEquals(a, b, "content-equal arrays must yield equal AnyOpaqueUeba")
        assertEquals(a.hashCode(), b.hashCode(), "equal AnyOpaqueUeba must hash equal")

        val c = AnyOpaqueUeba(meta, byteArrayOf(1, 2, 4))
        assertNotEquals(a, c)

        val emptyA = AnyOpaqueUeba(meta, ByteArray(0))
        val emptyB = AnyOpaqueUeba(meta, byteArrayOf())
        assertEquals(emptyA, emptyB)
    }

    // ===== AnyOpaqueJson content equality (PR-08-D06 — for kotlinx.serialization.JsonElement,
    // data-class auto-generated equals is already content-wise; verify here) =====

    @Test
    fun anyOpaqueJson_comparesJsonByContentNotReference() {
        val meta = AnyMeta(0x07.toByte(), "d", "v", "t")
        val j1: JsonObject = buildJsonObject {
            put("x", 1)
            put("y", JsonPrimitive(2))
        }
        val j2: JsonObject = buildJsonObject {
            put("x", 1)
            put("y", JsonPrimitive(2))
        }
        val a = AnyOpaqueJson(meta, j1)
        val b = AnyOpaqueJson(meta, j2)
        assertEquals(a, b)
        assertEquals(a.hashCode(), b.hashCode())

        val j3 = buildJsonObject {
            put("x", 1)
            put("y", JsonPrimitive(3))
        }
        val c = AnyOpaqueJson(meta, j3)
        assertNotEquals(a, c)
    }

    // ===== BaboonCodecContext.withFacade =====

    @Test
    fun baboonCodecContext_withFacade_exposesFacade() {
        val facade = BaboonCodecsFacade()
        assertEquals(null, BaboonCodecContext.Compact.facade)
        assertEquals(null, BaboonCodecContext.Indexed.facade)

        val withFacadeCompact = BaboonCodecContext.withFacade(useIndices = false, baboonFacade = facade)
        assertSame(facade, withFacadeCompact.facade)
        assertFalse(withFacadeCompact.useIndices)

        val withFacadeIndexed = BaboonCodecContext.withFacade(useIndices = true, baboonFacade = facade)
        assertSame(facade, withFacadeIndexed.facade)
        assertTrue(withFacadeIndexed.useIndices)
    }

    // ===== Facade Left paths =====

    @Test
    fun decodeAny_left_onIncompleteMeta_ueba() {
        val facade = BaboonCodecsFacade()
        val meta = AnyMeta(0x01.toByte(), null, null, "T") // kind C: only typeid
        val result = facade.decodeAny(AnyOpaqueUeba(meta, ByteArray(0)))
        assertTrue(result is Either.Left)
        val msg = (result as Either.Left).value.message ?: ""
        assertTrue(msg.contains("domain"))
        assertTrue(msg.contains("version"))
    }

    @Test
    fun decodeAny_left_onIncompleteMeta_json() {
        val facade = BaboonCodecsFacade()
        val meta = AnyMeta(0x01.toByte(), null, null, "T")
        val result = facade.decodeAny(AnyOpaqueJson(meta, JsonPrimitive("x")))
        assertTrue(result is Either.Left)
        val msg = (result as Either.Left).value.message ?: ""
        assertTrue(msg.contains("domain"))
        assertTrue(msg.contains("version"))
    }

    @Test
    fun jsonToUebaBytes_left_onIncompleteMeta() {
        val facade = BaboonCodecsFacade()
        val meta = AnyMeta(0x01.toByte(), null, null, "T")
        val result = facade.jsonToUebaBytes(meta, JsonPrimitive("x"))
        assertTrue(result is Either.Left)
        val msg = (result as Either.Left).value.message ?: ""
        assertTrue(msg.contains("domain"))
        assertTrue(msg.contains("version"))
    }

    @Test
    fun uebaToJson_left_onIncompleteMeta() {
        val facade = BaboonCodecsFacade()
        val meta = AnyMeta(0x01.toByte(), null, null, "T")
        val result = facade.uebaToJson(meta, ByteArray(0))
        assertTrue(result is Either.Left)
        val msg = (result as Either.Left).value.message ?: ""
        assertTrue(msg.contains("domain"))
        assertTrue(msg.contains("version"))
    }

    @Test
    fun jsonToUebaBytes_left_onNoCodecRegistered() {
        val facade = BaboonCodecsFacade()
        val meta = AnyMeta(0x07.toByte(), "com.example.unknown", "1.0.0", "Unknown")
        val result = facade.jsonToUebaBytes(meta, buildJsonObject { put("x", 1) })
        assertTrue(result is Either.Left)
        val err = (result as Either.Left).value
        assertTrue(
            err is BaboonCodecException.CodecNotFound,
            "expected CodecNotFound, got ${err::class.simpleName}: ${err.message}"
        )
    }

    @Test
    fun uebaToJson_left_onNoCodecRegistered() {
        val facade = BaboonCodecsFacade()
        val meta = AnyMeta(0x07.toByte(), "com.example.unknown", "1.0.0", "Unknown")
        val result = facade.uebaToJson(meta, ByteArray(0))
        assertTrue(result is Either.Left)
        val err = (result as Either.Left).value
        assertTrue(err is BaboonCodecException.CodecNotFound)
    }

    // ===== Static-fallback semantics (PR-06-D01) =====

    @Test
    fun jsonToUebaBytes_staticFallback_variantB_domainFromStatic() {
        val facade = BaboonCodecsFacade()
        // kind 0x03 (B): no domain on wire, version + typeid present.
        val meta = AnyMeta(0x03.toByte(), null, "1.0.0", "T")
        val result = facade.jsonToUebaBytes(meta, JsonPrimitive("x"), staticDomain = "com.example.unknown")
        // Should reach codec lookup (CodecNotFound), not the missing-meta DecoderFailure.
        assertTrue(result is Either.Left)
        val err = (result as Either.Left).value
        assertTrue(
            err is BaboonCodecException.CodecNotFound,
            "expected CodecNotFound (static fallback wired), got ${err::class.simpleName}: ${err.message}"
        )
    }

    @Test
    fun jsonToUebaBytes_staticFallback_variantC_domainAndVersionFromStatic() {
        val facade = BaboonCodecsFacade()
        // kind 0x01 (C): only typeid on wire.
        val meta = AnyMeta(0x01.toByte(), null, null, "T")
        val result = facade.jsonToUebaBytes(
            meta, JsonPrimitive("x"),
            staticDomain = "com.example.unknown",
            staticVersion = "1.0.0",
        )
        assertTrue(result is Either.Left)
        val err = (result as Either.Left).value
        assertTrue(err is BaboonCodecException.CodecNotFound)
    }

    @Test
    fun jsonToUebaBytes_staticFallback_variantD1_typeidFromStatic() {
        val facade = BaboonCodecsFacade()
        // kind 0x06 (D1): domain + version on wire, typeid absent.
        val meta = AnyMeta(0x06.toByte(), "com.example.unknown", "1.0.0", null)
        val result = facade.jsonToUebaBytes(meta, JsonPrimitive("x"), staticTypeid = "Inner")
        assertTrue(result is Either.Left)
        val err = (result as Either.Left).value
        assertTrue(err is BaboonCodecException.CodecNotFound)
    }

    @Test
    fun jsonToUebaBytes_staticFallback_variantD3_allThreeFromStatics() {
        val facade = BaboonCodecsFacade()
        // kind 0x00 (D3): nothing on wire.
        val meta = AnyMeta(0x00.toByte(), null, null, null)
        val result = facade.jsonToUebaBytes(
            meta, JsonPrimitive("x"),
            staticDomain = "com.example.unknown",
            staticVersion = "1.0.0",
            staticTypeid = "Inner",
        )
        assertTrue(result is Either.Left)
        val err = (result as Either.Left).value
        assertTrue(err is BaboonCodecException.CodecNotFound)
    }

    @Test
    fun uebaToJson_staticFallback_variantD3_allThreeFromStatics() {
        val facade = BaboonCodecsFacade()
        val meta = AnyMeta(0x00.toByte(), null, null, null)
        val result = facade.uebaToJson(
            meta, ByteArray(0),
            staticDomain = "com.example.unknown",
            staticVersion = "1.0.0",
            staticTypeid = "Inner",
        )
        assertTrue(result is Either.Left)
        val err = (result as Either.Left).value
        assertTrue(err is BaboonCodecException.CodecNotFound)
    }

    @Test
    fun jsonToUebaBytes_staticFallback_metaWinsOverStatic() {
        val facade = BaboonCodecsFacade()
        // All three present on wire; statics differ — wire wins, so resolution must reference
        // meta.domain ("metawins"), not the static ("staticloses").
        val meta = AnyMeta(0x07.toByte(), "com.example.metawins", "1.0.0", "MetaT")
        val result = facade.jsonToUebaBytes(
            meta, JsonPrimitive("x"),
            staticDomain = "com.example.staticloses",
            staticVersion = "9.9.9",
            staticTypeid = "StaticT",
        )
        assertTrue(result is Either.Left)
        val err = (result as Either.Left).value
        assertTrue(err is BaboonCodecException.CodecNotFound)
        val msg = err.message ?: ""
        assertTrue(
            msg.contains("metawins"),
            "expected error to reference wire-meta domain (override semantics), got: $msg"
        )
    }

    // ===== PR-07-D02: single-version-domain regression =====

    private class StubMeta : BaboonMeta {
        override fun sameInVersions(typeId: String): List<String> = emptyList()
    }

    private class StubJsonCodecs : AbstractBaboonJsonCodecs()
    private class StubUebaCodecs : AbstractBaboonUebaCodecs()

    @Test
    fun getCodec_nonExact_atMaxVersion_routesToExactLookup_singleVersionDomain() {
        // Pre-fix: when minVersion == maxVersion == modelVersion and `exact=false`, the
        // dispatcher fell through every arm and returned "Unsupported domain version".
        // The fix added an explicit `!exact && v == max` arm routing to GetCodecExact.
        val facade = BaboonCodecsFacade()
        val dv = BaboonDomainVersion("com.example.single", "1.0.0")
        facade.register(
            dv,
            { StubJsonCodecs() },
            { StubUebaCodecs() },
            { StubMeta() },
        )
        val meta = AnyMeta(0x07.toByte(), "com.example.single", "1.0.0", "Unknown")
        val result = facade.jsonToUebaBytes(meta, JsonPrimitive("x"))
        assertTrue(result is Either.Left)
        val err = (result as Either.Left).value
        assertTrue(err is BaboonCodecException.CodecNotFound)
        val msg = err.message ?: ""
        assertFalse(
            msg.contains("Unsupported domain version"),
            "single-version-domain non-exact lookup must route to getCodecExact, not fall through; got: $msg"
        )
    }

    // ===== DecodeAny preserves all-required-meta contract =====

    @Test
    fun decodeAny_stillRequiresCompleteMeta_noStaticFallbackForUserFacingPath() {
        // PR 2.1 (Scala) / PR 3.1 (C#) / PR 5.1 (Kotlin) contract: decodeAny is user-facing
        // without static context; its limitation to variant A (kind 0x07) is preserved.
        val facade = BaboonCodecsFacade()
        val meta = AnyMeta(0x01.toByte(), null, null, "T")
        val result = facade.decodeAny(AnyOpaqueUeba(meta, ByteArray(0)))
        assertTrue(result is Either.Left)
        val msg = (result as Either.Left).value.message ?: ""
        assertTrue(msg.contains("domain"))
        assertTrue(msg.contains("version"))
        // sanity: the BaboonCodecsFacade reference here exercises the path
        assertNotNull(facade)
    }
}
