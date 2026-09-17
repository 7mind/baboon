package runtime

import baboon.runtime.shared.*
import kotlin.test.*
import testpkg.pkg0.T1_E2
import testpkg.pkg0.T1_E2_RET
import testpkg.pkg0.T1_E2_RET_UEBACodec

class IndexValidationTest {
    private val codec = object : BaboonBinCodecIndexed {
        override fun indexElementsCount(ctx: BaboonCodecContext): Short = 2
    }

    @Test fun generatedDecoderRejectsMissingRequiredIndex() {
        val writer = BaboonBinaryWriter()
        T1_E2_RET_UEBACodec.encode(BaboonCodecContext.Compact, writer, T1_E2_RET(T1_E2.A, null))
        assertFailsWith<IllegalArgumentException> {
            T1_E2_RET_UEBACodec.decode(BaboonCodecContext.Indexed, BaboonBinaryReader(writer.toByteArray()))
        }
    }

    @Test fun invalidEntriesThrowNormalErrors() {
        for (entries in listOf(listOf(0, 0, 1, 1), listOf(-1, 1, 1, 1), listOf(0, -1, 1, 1), listOf(0, 2, 1, 1), listOf(Int.MAX_VALUE, 1, 0, 1))) {
            val writer = BaboonBinaryWriter()
            writer.writeByte(1)
            entries.forEach { writer.writeInt(it) }
            assertFailsWith<IllegalArgumentException> {
                codec.readIndex(BaboonCodecContext.Compact, BaboonBinaryReader(writer.toByteArray()))
            }
        }
    }
}
