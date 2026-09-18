package runtime

import baboon.runtime.shared.*
import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.nio.ByteBuffer
import java.nio.ByteOrder
import kotlin.test.*
import testpkg.pkg0.T1_E2
import testpkg.pkg0.T1_E2_RET
import testpkg.pkg0.T1_E2_RET_UEBACodec

class IndexValidationTest {
    private val codec = object : BaboonBinCodecIndexed {
        override fun indexElementsCount(ctx: BaboonCodecContext): Short = 2
    }

    @Test fun generatedDecoderRejectsMissingRequiredIndex() {
        val buffer = ByteArrayOutputStream()
        T1_E2_RET_UEBACodec.encode(BaboonCodecContext.Compact, LEDataOutputStream(buffer), T1_E2_RET(T1_E2.A, null))
        assertFailsWith<IllegalArgumentException> {
            T1_E2_RET_UEBACodec.decode(BaboonCodecContext.Indexed, LEDataInputStream(ByteArrayInputStream(buffer.toByteArray())))
        }
    }

    @Test fun invalidEntriesThrowNormalErrors() {
        for (entries in listOf(listOf(0, 0, 1, 1), listOf(-1, 1, 1, 1), listOf(0, -1, 1, 1), listOf(0, 2, 1, 1), listOf(Int.MAX_VALUE, 1, 0, 1))) {
            val bytes = ByteBuffer.allocate(1 + entries.size * Int.SIZE_BYTES).order(ByteOrder.LITTLE_ENDIAN)
            bytes.put(1)
            entries.forEach { bytes.putInt(it) }
            assertFailsWith<IllegalArgumentException> {
                codec.readIndex(BaboonCodecContext.Compact, LEDataInputStream(ByteArrayInputStream(bytes.array())))
            }
        }
    }
}
