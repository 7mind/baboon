// NOTE: This test references generated DTO/codec symbols (my.ok.Inner, my.ok.BaboonMetadata, ...)
// which are copied/generated into this stub only by `mdl :build :test-gen-regular-adt`
// (rsync + codegen into target/test-regular/kt-stub-kmp/). Running `gradle test` directly from
// the source tree may fail with missing symbols; run the test suite from the codegen'd copy.
//
// Mirrored from test/kt-stub/src/test/kotlin/runtime/BaboonExtTest.kt for KMP parity.
// No I/O adaptation needed — BaboonExt helpers are pure value computations.
package runtime

import baboon.runtime.shared.BaboonDomainVersion
import baboon.runtime.shared.baboonUnmodifiedSinceVersion
import baboon.runtime.shared.unmodifiedSinceVersion
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test

class BaboonExtTest {

    private val subject = my.ok.Inner(42)

    @Test
    fun domainVersion_returnsExpectedIdentifierAndVersion() {
        val dv: BaboonDomainVersion = subject.domainVersion()
        assertEquals(my.ok.Inner.baboonDomainIdentifier, dv.domainIdentifier)
        assertEquals(my.ok.Inner.baboonDomainVersion, dv.domainVersion)
    }

    @Test
    fun baboonUnmodifiedSinceVersion_returnsFirstSameInVersion() {
        val result = subject.baboonUnmodifiedSinceVersion()
        assertEquals(my.ok.Inner.baboonSameInVersions[0], result)
    }

    @Test
    fun unmodifiedSinceVersion_returnsFirstSameInVersionForTypeId() {
        val meta = my.ok.BaboonMetadata
        val typeId = my.ok.Inner.baboonTypeIdentifier
        val result = meta.unmodifiedSinceVersion(typeId)
        assertEquals(meta.sameInVersions(typeId)[0], result)
    }
}
