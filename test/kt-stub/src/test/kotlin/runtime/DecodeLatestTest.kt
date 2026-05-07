// References generated DTO/codec symbols (my.ok.Inner, my.ok.BaboonCodecsUeba,
// my.ok.BaboonCodecsJson, my.ok.BaboonMetadata) emitted only into the kt-stub codegen path.
// Run via `mdl :test-kotlin-regular`, not `gradle test` against the source tree.
package runtime

import baboon.runtime.shared.BaboonCodecContext
import baboon.runtime.shared.BaboonCodecsFacade
import baboon.runtime.shared.BaboonDomainVersion
import baboon.runtime.shared.Either
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.Test

class DecodeLatestTest {

    private fun freshFacade(): BaboonCodecsFacade {
        val f = BaboonCodecsFacade()
        f.register(
            BaboonDomainVersion(my.ok.Inner.baboonDomainIdentifier, my.ok.Inner.baboonDomainVersion),
            { my.ok.BaboonCodecsJson },
            { my.ok.BaboonCodecsUeba },
            { my.ok.BaboonMetadata },
        )
        return f
    }

    @Test
    fun preload_doesNotThrow() {
        val facade = freshFacade()
        facade.preload()
    }

    @Test
    fun decodeFromBinLatest_bytes_roundTripsInner() {
        val facade = freshFacade()
        val original = my.ok.Inner(42)
        val encoded = facade.encodeToBin(BaboonCodecContext.Compact, original)
        val result = facade.decodeFromBinLatest(encoded, my.ok.Inner::class.java)
        assertTrue(result is Either.Right, "decodeFromBinLatest must succeed: $result")
        assertEquals(original, (result as Either.Right).value)
    }

    @Test
    fun decodeFromJsonLatest_string_roundTripsInner() {
        val facade = freshFacade()
        val original = my.ok.Inner(42)
        // PR-7 aligned kt encodeToJson to emit the full envelope, so round-trip works without
        // manual wrapping (matches Scala / cs / etc.).
        val envelope = facade.encodeToJson(BaboonCodecContext.Compact, original)
        val result = facade.decodeFromJsonLatest(envelope.toString(), my.ok.Inner::class.java)
        assertTrue(result is Either.Right, "decodeFromJsonLatest must succeed: $result")
        assertEquals(original, (result as Either.Right).value)
    }
}
