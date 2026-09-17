package example

import convtest.testpkg.*
import kotlin.test.Test
import kotlin.test.assertEquals

class RenamedAdtConversionTest {
    private val conversions = BaboonConversions(object : RequiredConversions {})

    @Test fun renamedParentRetainsBranchPayload() {
        val result = Convert__OldAdtName__From__1_0_0.convert<Unit>(
            null, conversions, convtest.testpkg.v1_0_0.OldAdtName.Branch1("kept")
        )
        assertEquals(NewAdtName.Branch1("kept"), result)
    }

    @Test fun renamedBranchAddsOptionalDefault() {
        val result = Convert__AdtBranchRename__From__1_0_0.convert<Unit>(
            null, conversions, convtest.testpkg.v1_0_0.AdtBranchRename.OldBranch("kept")
        )
        assertEquals(AdtBranchRename.NewBranch("kept", null), result)
    }
}
