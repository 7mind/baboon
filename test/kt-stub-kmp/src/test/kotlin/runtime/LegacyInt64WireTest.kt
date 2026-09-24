// 64-bit integers are written as decimal strings (docs/json-codecs.md, "64-bit integers").
// Readers stay lenient about the JSON-number form an older compiler produced, and that
// leniency is what keeps documents written before the change readable.
//
// Nothing writes numbers any more, so without this test the number arm of every decoder is
// dead as far as the suite is concerned.
//
// Uses the identifier.ok fixture (id LongId { x: i64 }; id UInts { a: u08, b: u16, c: u32,
// d: u64 }); generated symbols are produced by mdl :test-gen-regular-adt.
package runtime

import baboon.runtime.shared.BaboonCodecContext
import identifier.ok.LongId_JsonCodec
import identifier.ok.UInts_JsonCodec
import kotlinx.serialization.json.Json
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.Test

class LegacyInt64WireTest {

    private val ctx = BaboonCodecContext.Compact

    @Test
    fun i64DecodesFromTheLegacyNumericForm() {
        val decoded = LongId_JsonCodec.decode(ctx, Json.parseToJsonElement("""{"x":-9007199254740991}"""))
        assertEquals(-9007199254740991L, decoded.x)
    }

    @Test
    fun i64DecodesFromTheStringForm() {
        val decoded = LongId_JsonCodec.decode(ctx, Json.parseToJsonElement("""{"x":"-9223372036854775808"}"""))
        assertEquals(Long.MIN_VALUE, decoded.x)
    }

    @Test
    fun u64DecodesFromTheLegacyNumericForm() {
        val decoded = UInts_JsonCodec.decode(ctx, Json.parseToJsonElement("""{"a":1,"b":2,"c":3,"d":42}"""))
        assertTrue(decoded.d == 42uL, "expected 42, got ${decoded.d}")
    }

    @Test
    fun u64DecodesFromTheStringForm() {
        val decoded = UInts_JsonCodec.decode(ctx, Json.parseToJsonElement("""{"a":1,"b":2,"c":3,"d":"42"}"""))
        assertTrue(decoded.d == 42uL, "expected 42, got ${decoded.d}")
    }

    // An older Kotlin writer emitted u64 as `JsonPrimitive(value.toLong())`, so anything above
    // Long.MAX_VALUE went out as a negative number. That form has to keep reading back as the
    // same value.
    @Test
    fun u64DecodesFromTheLegacySignedNumericForm() {
        val decoded = UInts_JsonCodec.decode(ctx, Json.parseToJsonElement("""{"a":1,"b":2,"c":3,"d":-1}"""))
        assertTrue(decoded.d == ULong.MAX_VALUE, "expected ULong.MAX_VALUE, got ${decoded.d}")
    }
}
