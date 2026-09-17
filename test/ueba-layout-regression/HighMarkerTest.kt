import baboon.runtime.shared.*
import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import kotlin.test.*
import ueba.layout.high.Choice

class HighMarkerTest {
    @Test fun unsignedOrdinalsRoundTrip() {
        fun <T : Any> check(value: T, codec: BaboonBinCodec<T>, ordinal: Int) {
            val buffer = ByteArrayOutputStream()
            codec.encode(BaboonCodecContext.Compact, LEDataOutputStream(buffer), value)
            assertContentEquals(byteArrayOf(ordinal.toByte(), 0), buffer.toByteArray())
            val decoded = codec.decode(BaboonCodecContext.Compact, LEDataInputStream(ByteArrayInputStream(buffer.toByteArray())))
            assertEquals(value::class, decoded::class)
        }
        check(Choice.B0(), Choice.B0_UEBACodec, 0)
        check(Choice.B127(), Choice.B127_UEBACodec, 127)
        check(Choice.B128(), Choice.B128_UEBACodec, 128)
        assertFailsWith<IllegalArgumentException> {
            Choice.B128_UEBACodec.decode(BaboonCodecContext.Compact, LEDataInputStream(ByteArrayInputStream(byteArrayOf(0, 0))))
        }
    }
}
