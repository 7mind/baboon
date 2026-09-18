// Cross-language golden bytes for the top-level binary envelope (docs/spec/codec-envelope.md §2.1, §2.1.2, §2.1.3; docs/forward-compat.md, "Worked examples"). The same values must produce these exact bytes in every backend; the Scala and TypeScript stubs assert the same sequences structurally.
package runtime

import baboon.runtime.shared.BaboonCodecContext
import baboon.runtime.shared.BaboonCodecsFacade
import baboon.runtime.shared.BaboonEnvelopeVersion
import baboon.runtime.shared.BaboonGenerated
import baboon.runtime.shared.ForwardWritePolicy
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test

class BinEnvelopeGoldenTest {
    private fun hx(b: ByteArray): String = b.joinToString(" ") { (it.toInt() and 0xFF).toString(16).padStart(2, '0').uppercase() }
    private fun <T : BaboonGenerated> enc(f: BaboonCodecsFacade, ctx: BaboonCodecContext, v: T): ByteArray = f.encodeToBin(ctx, v)

    private val v1Tolerant = BaboonCodecContext.custom(false, ForwardWritePolicy.Tolerant, BaboonEnvelopeVersion.V1, null)
    private val v2Compact = BaboonCodecContext.custom(false, ForwardWritePolicy.Strict, BaboonEnvelopeVersion.V2, null)
    private val v2Indexed = BaboonCodecContext.custom(true, ForwardWritePolicy.Strict, BaboonEnvelopeVersion.V2, null)

    @Test
    fun defaultContextsWriteV1Strict() {
        assertEquals(BaboonEnvelopeVersion.V1, BaboonCodecContext.Compact.envelopeVersion)
        assertEquals(ForwardWritePolicy.Strict, BaboonCodecContext.Compact.forwardWritePolicy)
        assertEquals(BaboonEnvelopeVersion.V1, BaboonCodecContext.Indexed.envelopeVersion)
    }

    @Test
    fun envelopesMatchTheCrossLanguageGoldenBytes() {
        val fwd = fwde2e.fwd.DomainFwde2eFwdFacade()
        val chain = fwde2e.chain.DomainFwde2eChainFacade()
        val app = fwde2e.fwd.FwdAppendVar(42, "hi", "t")
        // FwdAppendVar, v1 Strict (default) compact: identical bound elided
        assertEquals("01 0A 66 77 64 65 32 65 2E 66 77 64 05 32 2E 30 2E 30 00 19 66 77 64 65 32 65 2E 66 77 64 2F 3A 23 46 77 64 41 70 70 65 6E 64 56 61 72 00 2A 00 00 00 02 68 69 01 01 74", hx(enc(fwd, BaboonCodecContext.Compact, app)))
        // FwdAppendVar, v1 Tolerant compact: prefix-compact bound 1.0.0 in the single slot
        assertEquals("01 0A 66 77 64 65 32 65 2E 66 77 64 05 32 2E 30 2E 30 01 05 31 2E 30 2E 30 19 66 77 64 65 32 65 2E 66 77 64 2F 3A 23 46 77 64 41 70 70 65 6E 64 56 61 72 00 2A 00 00 00 02 68 69 01 01 74", hx(enc(fwd, v1Tolerant, app)))
        // FwdAppendVar, v2 compact: flags 0b10, readableMin 1.0.0
        assertEquals("02 0A 66 77 64 65 32 65 2E 66 77 64 05 32 2E 30 2E 30 02 05 31 2E 30 2E 30 19 66 77 64 65 32 65 2E 66 77 64 2F 3A 23 46 77 64 41 70 70 65 6E 64 56 61 72 00 2A 00 00 00 02 68 69 01 01 74", hx(enc(fwd, v2Compact, app)))
        // FwdAppendVar, v2 indexed: flags 0 (prefix-any-mode bound is 2.0.0)
        assertEquals("02 0A 66 77 64 65 32 65 2E 66 77 64 05 32 2E 30 2E 30 00 19 66 77 64 65 32 65 2E 66 77 64 2F 3A 23 46 77 64 41 70 70 65 6E 64 56 61 72 01 04 00 00 00 03 00 00 00 07 00 00 00 03 00 00 00 2A 00 00 00 02 68 69 01 01 74", hx(enc(fwd, v2Indexed, app)))
        // FwdStable, v1 Strict compact: byte-identical since 1.0.0 -> hasMinCompat 1
        assertEquals("01 0A 66 77 64 65 32 65 2E 66 77 64 05 32 2E 30 2E 30 01 05 31 2E 30 2E 30 16 66 77 64 65 32 65 2E 66 77 64 2F 3A 23 46 77 64 53 74 61 62 6C 65 00 01 73", hx(enc(fwd, BaboonCodecContext.Compact, fwde2e.fwd.FwdStable("s"))))
        // FwdStable, v2 compact: flags 0b01, minCompat 1.0.0, readableMin elided
        assertEquals("02 0A 66 77 64 65 32 65 2E 66 77 64 05 32 2E 30 2E 30 01 05 31 2E 30 2E 30 16 66 77 64 65 32 65 2E 66 77 64 2F 3A 23 46 77 64 53 74 61 62 6C 65 00 01 73", hx(enc(fwd, v2Compact, fwde2e.fwd.FwdStable("s"))))
        // FwdEnumHost, v2 compact: flags 0, no bound
        assertEquals("02 0A 66 77 64 65 32 65 2E 66 77 64 05 32 2E 30 2E 30 00 18 66 77 64 65 32 65 2E 66 77 64 2F 3A 23 46 77 64 45 6E 75 6D 48 6F 73 74 00 02", hx(enc(fwd, v2Compact, fwde2e.fwd.FwdEnumHost(fwde2e.fwd.FwdEnumGrows.C))))
        // ChainAppend 3.0.0, v2 compact: flags 0b10, readableMin 1.0.0
        assertEquals("02 0C 66 77 64 65 32 65 2E 63 68 61 69 6E 05 33 2E 30 2E 30 02 05 31 2E 30 2E 30 1A 66 77 64 65 32 65 2E 63 68 61 69 6E 2F 3A 23 43 68 61 69 6E 41 70 70 65 6E 64 00 01 00 00 00 01 01 62 01 01 63", hx(enc(chain, v2Compact, fwde2e.chain.ChainAppend(1, "b", "c"))))
    }

    @Test
    fun v2EnvelopeRoundTripsThroughItsOwnFacade() {
        val fwd = fwde2e.fwd.DomainFwde2eFwdFacade()
        val app = fwde2e.fwd.FwdAppendVar(42, "hi", "t")
        assertEquals(app, fwd.decodeFromBin(enc(fwd, v2Compact, app)))
    }
}
