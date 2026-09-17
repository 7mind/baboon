package runtime

import baboon.runtime.shared.*
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test

class CodecVersionSelectionTest {
    private fun version(value: String) = BaboonDomainVersion("example", value)
    private fun meta(writer: String, compatible: String, readable: String) =
        BaboonTypeMeta(1, "example", writer, compatible, "Example", readable)

    @Test
    fun latestCurrentAndOlderWritersRetainSelectionRules() {
        val min = version("1.0.0")
        val max = version("3.0.0")
        for (exact in listOf(false, true)) {
            assertEquals(BaboonCodecVersionSelection.Exact(max),
                BaboonCodecVersionSelection.select(meta("3.0.0", "3.0.0", "3.0.0"), min, max, exact, false))
            assertEquals(BaboonCodecVersionSelection.Compatible(version("2.0.0")),
                BaboonCodecVersionSelection.select(meta("2.0.0", "2.0.0", "2.0.0"), min, max, exact, false))
            assertEquals(BaboonCodecVersionSelection.Compatible(min),
                BaboonCodecVersionSelection.select(meta("0.5.0", "0.5.0", "0.5.0"), min, max, exact, false))
        }
        assertEquals(BaboonCodecVersionSelection.Exact(min),
            BaboonCodecVersionSelection.select(meta("1.0.0", "1.0.0", "1.0.0"), min, min, false, false))
    }

    @Test
    fun forwardBoundsRespectExactAndTolerantPolicies() {
        val min = version("1.0.0")
        val max = version("3.0.0")
        val forward = meta("4.0.0", "4.0.0", "2.0.0")
        assertEquals(BaboonCodecVersionSelection.Exact(max),
            BaboonCodecVersionSelection.select(forward, min, max, false, true))
        assertEquals(BaboonCodecVersionSelection.UnsupportedForward(version("4.0.0")),
            BaboonCodecVersionSelection.select(forward, min, max, false, false))
        assertEquals(BaboonCodecVersionSelection.Unsupported(version("4.0.0")),
            BaboonCodecVersionSelection.select(forward, min, max, true, true))
        assertEquals(BaboonCodecVersionSelection.Exact(max),
            BaboonCodecVersionSelection.select(meta("4.0.0", "3.0.0", ""), min, max, false, false))
        assertEquals(BaboonCodecVersionSelection.UnsupportedForward(version("4.0.0")),
            BaboonCodecVersionSelection.select(meta("4.0.0", "", ""), min, max, false, true))
    }
}
