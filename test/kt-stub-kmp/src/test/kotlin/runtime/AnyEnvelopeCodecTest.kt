package runtime

import baboon.runtime.shared.*
import kotlinx.serialization.json.JsonObject
import kotlinx.serialization.json.JsonPrimitive
import kotlin.test.Test
import kotlin.test.assertContentEquals
import kotlin.test.assertEquals
import kotlin.test.assertFailsWith

class AnyEnvelopeCodecTest {
    private val meta = AnyMeta(0, null, null, null)
    private val payload = byteArrayOf(1, 2)

    private fun encode(value: AnyOpaque, kind: Byte): ByteArray {
        val writer = BaboonBinaryWriter()
        BaboonAnyBinCodec.encode(BaboonCodecContext.Compact, writer, kind, null, null, null, value)
        return writer.toByteArray()
    }

    private fun decode(bytes: ByteArray, kind: Byte): AnyOpaqueUeba =
        BaboonAnyBinCodec.decode(BaboonBinaryReader(bytes), kind)

    @Test
    fun binaryEnvelopeRetainsExactBytes() {
        val expected = byteArrayOf(7, 0, 0, 0, 1, 0, 0, 0, 0, 1, 2)
        assertContentEquals(expected, encode(AnyOpaqueUeba(meta, payload), 0))
        assertEquals(AnyOpaqueUeba(meta, payload), decode(expected, 0))
    }

    @Test
    fun binaryEnvelopeSkipsFutureMetadataExtensions() {
        val extended = byteArrayOf(9, 0, 0, 0, 3, 0, 0, 0, 0, 42, 43, 1, 2)
        assertEquals(AnyOpaqueUeba(meta, payload), decode(extended, 0))
    }

    @Test
    fun jsonEnvelopeRetainsContentAndChecksKind() {
        val content = JsonObject(mapOf("x" to JsonPrimitive(1)))
        val value = AnyOpaqueJson(meta, content)
        val expected = JsonObject(mapOf("${'$'}ak" to JsonPrimitive(0), "${'$'}c" to content))
        assertEquals(expected, BaboonAnyJsonCodec.encode(BaboonCodecContext.Compact, 0, null, null, null, value))
        assertEquals(value, BaboonAnyJsonCodec.decode(0, expected))
        assertFailsWith<BaboonCodecException.DecoderFailure> { BaboonAnyJsonCodec.decode(1, expected) }
        assertFailsWith<BaboonCodecException.DecoderFailure> {
            BaboonAnyJsonCodec.decode(0, JsonObject(mapOf("${'$'}ak" to JsonPrimitive(0))))
        }
    }

    @Test
    fun encodingRetainsKindAndFacadeRequirements() {
        val binary = AnyOpaqueUeba(meta, payload)
        assertFailsWith<BaboonCodecException.EncoderFailure> { encode(binary, 1) }
        assertFailsWith<BaboonCodecException.EncoderFailure> {
            BaboonAnyJsonCodec.encode(BaboonCodecContext.Compact, 0, null, null, null, binary)
        }
        assertFailsWith<BaboonCodecException.EncoderFailure> {
            encode(AnyOpaqueJson(meta, JsonPrimitive(1)), 0)
        }
    }
}
