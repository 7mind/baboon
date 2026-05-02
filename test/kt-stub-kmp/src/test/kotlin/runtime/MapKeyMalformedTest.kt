// PR-F (M24) — cross-language malformed map-key error consistency.
//
// Verifies that decoding a JSON object whose map-key cannot be parsed back into the
// id type throws BaboonCodecException.DecoderFailure with message containing
// "malformed key". Replaces the prior unchecked `as Either.Right` cast.
//
// Mirrored from test/kt-stub/src/test/kotlin/runtime/MapKeyMalformedTest.kt for
// PR-25.5a (M25 KMP runtime test parity). No KMP-specific adaptation needed:
// the test uses only kotlinx.serialization (multiplatform-friendly) and generated
// Holder_JsonCodec. Uses the my.ok.m19.singleid fixture; generated symbols are
// produced by mdl :test-gen-regular-adt under target/test-regular/kt-stub-kmp/.
package runtime

import baboon.runtime.shared.BaboonCodecContext
import baboon.runtime.shared.BaboonCodecException
import kotlinx.serialization.json.Json
import my.ok.m19.singleid.Holder_JsonCodec
import org.junit.jupiter.api.Assertions.assertThrows
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.Test

class MapKeyMalformedTest {

    private val ctx = BaboonCodecContext.Compact

    @Test
    fun holderJsonDecodeThrowsDecoderFailureForMalformedMapKey() {
        val badJson = """{"m":{"not_a_valid_id":"v"}}"""
        val node = Json.parseToJsonElement(badJson)
        val ex = assertThrows(BaboonCodecException.DecoderFailure::class.java) {
            Holder_JsonCodec.decode(ctx, node)
        }
        assertTrue(
            ex.message?.contains("malformed key") == true,
            "unexpected message: ${ex.message}"
        )
    }
}
