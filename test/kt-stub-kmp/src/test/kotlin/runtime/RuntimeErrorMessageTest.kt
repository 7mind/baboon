// The KMP runtime sources are emitted verbatim, so `"$$x"` in them is a literal `$` followed by
// the template `$x` (https://github.com/7mind/baboon/issues/108).
package runtime

import baboon.runtime.shared.ByteString
import baboon.runtime.shared.Version
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertThrows
import org.junit.jupiter.api.Test

class RuntimeErrorMessageTest {

    @Test
    fun byteStringBoundsMessagesInterpolateValues() {
        val bytes = ByteString.of(1, 2)
        assertEquals("Start index: 5", assertThrows(IllegalArgumentException::class.java) { bytes.substring(5, 1) }.message)
        assertEquals("Length: 7", assertThrows(IllegalArgumentException::class.java) { bytes.substring(0, 7) }.message)
    }

    @Test
    fun versionParseMessageInterpolatesInput() {
        val e = assertThrows(Exception::class.java) { Version.from("x.1.2") }
        assertEquals("Expected to have version in format x.[y].[z], got x.1.2. Invalid major value.", e.message)
    }
}
