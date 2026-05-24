package example;

// Regression test for the `EnumVariant : was[OldName]` rename conversion
// (see test/conv-test/pkg02.baboon `enum EnumMemberRename` and the matching
// Rust/TypeScript regression tests in test/conv-test-{rs,ts}/tests/).
// Invokes the generated rename conversion against:
//   1. a renamed source variant (OldValue -> NewValue);
//   2. a non-renamed variant (KeepValue) — passes through the fallback
//      arm of the switch in the generated mapper.

import convtest.testpkg.BaboonConversions;
import convtest.testpkg.Convert__EnumMemberRename__From__1_0_0;
import convtest.testpkg.EnumMemberRename;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class EnumMemberRenameTest {

    private final BaboonConversions convs = new BaboonConversions();

    @Test
    public void renamedVariantMapsToNewName() {
        EnumMemberRename mapped = Convert__EnumMemberRename__From__1_0_0.INSTANCE.doConvert(
            (Object) null,
            convs,
            convtest.testpkg.v1_0_0.EnumMemberRename.OldValue
        );
        assertEquals(EnumMemberRename.NewValue, mapped);
    }

    @Test
    public void nonRenamedVariantPassesThrough() {
        EnumMemberRename mapped = Convert__EnumMemberRename__From__1_0_0.INSTANCE.doConvert(
            (Object) null,
            convs,
            convtest.testpkg.v1_0_0.EnumMemberRename.KeepValue
        );
        assertEquals(EnumMemberRename.KeepValue, mapped);
    }
}
