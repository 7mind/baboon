package example

// Regression test for the `EnumVariant : was[OldName]` rename conversion
// (Kotlin Multiplatform mirror of the JVM-Kotlin test). See
// test/conv-test/pkg02.baboon `enum EnumMemberRename` and the matching
// Rust/TypeScript regression tests in test/conv-test-{rs,ts}/tests/.
// Invokes the generated rename conversion against:
//   1. a renamed source variant (OldValue -> NewValue);
//   2. a non-renamed variant (KeepValue) — passes through the fallback
//      arm of the mapping.

import convtest.testpkg.BaboonConversions
import convtest.testpkg.Convert__EnumMemberRename__From__1_0_0
import convtest.testpkg.EnumMemberRename
import convtest.testpkg.RequiredConversions
import kotlin.test.Test
import kotlin.test.assertEquals

class EnumMemberRenameTest {

    private val convs = BaboonConversions(object : RequiredConversions {})

    @Test
    fun renamedVariantMapsToNewName() {
        val mapped = Convert__EnumMemberRename__From__1_0_0.convert<Unit>(
            null, convs, convtest.testpkg.v1_0_0.EnumMemberRename.OldValue
        )
        assertEquals(EnumMemberRename.NewValue, mapped)
    }

    @Test
    fun nonRenamedVariantPassesThrough() {
        val mapped = Convert__EnumMemberRename__From__1_0_0.convert<Unit>(
            null, convs, convtest.testpkg.v1_0_0.EnumMemberRename.KeepValue
        )
        assertEquals(EnumMemberRename.KeepValue, mapped)
    }
}
