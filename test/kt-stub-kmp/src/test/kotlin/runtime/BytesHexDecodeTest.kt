// Unified `bytes` JSON hex grammar (https://github.com/7mind/baboon/issues/102, /103):
// ASCII space, TAB, LF, VT, FF, CR, ':' and '-' are ignored; the remainder must be an even
// number of ASCII hex digits. Anything else throws IllegalArgumentException, never bytes.
//
// Generated JSON codecs decode `bytes` values and map keys through ByteString.fromHexString.
//
// Mirrored from test/kt-stub/src/test/kotlin/runtime/BytesHexDecodeTest.kt; no KMP-specific
// adaptation needed.
package runtime

import baboon.runtime.shared.ByteString
import org.junit.jupiter.api.Assertions.assertArrayEquals
import org.junit.jupiter.api.Assertions.assertThrows
import org.junit.jupiter.api.Test

class BytesHexDecodeTest {

    private val abcd = byteArrayOf(0xAB.toByte(), 0xCD.toByte())

    private val valid: List<Pair<String, ByteArray>> = listOf(
        "" to byteArrayOf(),
        "ABCD" to abcd,
        "abcd" to abcd,
        "aBcD" to abcd,
        "AB CD" to abcd,
        "AB:CD" to abcd,
        "AB-CD" to abcd,
        "A B C D" to abcd,
        "\tAB\r\nCD " to abcd,
        "A:B-C D" to abcd,
        "\u000BAB\u000CCD" to abcd,
        "00FF7f" to byteArrayOf(0x00, 0xFF.toByte(), 0x7F),
        " - : " to byteArrayOf(),
    )

    private val invalid: List<String> = listOf(
        "ZZ", "AQ", "+1", "0x01", "q80=", "ABC", "A", "-1",
        "٣٣", "ＡＡ", "aéb", "AB CD",
    )

    @Test
    fun validHexSpellingsDecode() {
        for ((input, expected) in valid) {
            assertArrayEquals(expected, ByteString.fromHexString(input).toArray(), input)
        }
    }

    @Test
    fun malformedHexIsRejected() {
        for (input in invalid) {
            assertThrows(IllegalArgumentException::class.java, { ByteString.fromHexString(input) }, input)
        }
    }
}
