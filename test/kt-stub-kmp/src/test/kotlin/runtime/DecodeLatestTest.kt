// References generated DTO/codec symbols (my.ok.Inner, my.ok.BaboonCodecsUeba,
// my.ok.BaboonCodecsJson, my.ok.BaboonMetadata) emitted only into the kt-stub-kmp codegen path.
// Run via `mdl :test-kotlin-kmp-regular`, not `gradle test` against the source tree.
//
// KMP variant: uses KClass<T> instead of Class<T> for the targetClass parameter.
package runtime

import baboon.runtime.shared.BaboonCodecContext
import baboon.runtime.shared.BaboonCodecsFacade
import baboon.runtime.shared.BaboonDomainVersion
import baboon.runtime.shared.Either
import kotlinx.serialization.json.buildJsonObject
import kotlinx.serialization.json.put
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
        val result = facade.decodeFromBinLatest(encoded, my.ok.Inner::class)
        assertTrue(result is Either.Right, "decodeFromBinLatest must succeed: $result")
        assertEquals(original, (result as Either.Right).value)
    }

    @Test
    fun decodeFromJsonLatest_string_roundTripsInner() {
        val facade = freshFacade()
        val original = my.ok.Inner(42)
        // kt-kmp facade's encodeToJson emits content-only (asymmetric with decodeFromJson
        // which expects an envelope); construct the {$d,$v,$t,$c} envelope manually for the test.
        val contentJson = facade.encodeToJson(BaboonCodecContext.Compact, original)
        val envelope = buildJsonObject {
            put("\$d", original.baboonDomainIdentifier)
            put("\$v", original.baboonDomainVersion)
            put("\$t", original.baboonTypeIdentifier)
            put("\$c", contentJson)
        }
        val result = facade.decodeFromJsonLatest(envelope.toString(), my.ok.Inner::class)
        assertTrue(result is Either.Right, "decodeFromJsonLatest must succeed: $result")
        assertEquals(original, (result as Either.Right).value)
    }
}
