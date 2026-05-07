// MFACADE-PR-6 stage B (kt-kmp): mirrors the kt-stub variant.
package runtime

import baboon.runtime.shared.BaboonCodecContext
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test

class DomainFacadeTest {
    @Test
    fun parameterlessCtor_autoRegistersAndRoundTripsInner() {
        val facade = my.ok.DomainMyOkFacade()
        val sample = my.ok.Inner(42)

        val bytes = facade.encodeToBin(BaboonCodecContext.Compact, sample)
        val decoded = facade.decodeFromBin(bytes)
        assertEquals(sample, decoded)
    }
}
