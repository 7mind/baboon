package example

import convtest.testpkg.BaboonConversions
import convtest.testpkg.Convert__EnumMemberRename__From__1_0_0
import convtest.testpkg.EnumMemberRename
import convtest.testpkg.RequiredConversions
import org.scalatest.flatspec.AnyFlatSpec

class Test_Conversions extends AnyFlatSpec {
  "derived conversions" should "support ADT auto upgrades" in {
    val a1   = new convtest.testpkg.v1_0_0.Adt0.B1("val1")
    val conv = new BaboonConversions(new RequiredConversions() {})

    val a1u1 = conv.convertWithContext[Unit, convtest.testpkg.v1_0_0.Adt0.B1, convtest.testpkg.Adt0.B1](None, a1)

    assert(a1.f == a1u1.f)

    val a1u2 = conv.convertWithContext[Unit, convtest.testpkg.v1_0_0.Adt0, convtest.testpkg.Adt0](None, a1)

    assert(a1u1 == a1u2)

  }

  // Regression test for `EnumVariant : was[OldName]` rename conversion
  // (see test/conv-test/pkg02.baboon `enum EnumMemberRename` and the
  // matching Rust/TypeScript regression tests in
  // test/conv-test-{rs,ts}/tests/). Invokes the generated rename
  // conversion against both the renamed variant (OldValue -> NewValue)
  // and the pass-through variant (KeepValue) to also exercise the
  // fallback arm of the mapping.
  "EnumMemberRename rename conversion" should "map OldValue to NewValue" in {
    val conv     = new BaboonConversions(new RequiredConversions() {})
    val oldValue = convtest.testpkg.v1_0_0.EnumMemberRename.OldValue
    val mapped   = Convert__EnumMemberRename__From__1_0_0.convert[Unit](None, conv, oldValue)
    assert(mapped == EnumMemberRename.NewValue)
  }

  it should "pass KeepValue through unchanged" in {
    val conv      = new BaboonConversions(new RequiredConversions() {})
    val keepValue = convtest.testpkg.v1_0_0.EnumMemberRename.KeepValue
    val mapped    = Convert__EnumMemberRename__From__1_0_0.convert[Unit](None, conv, keepValue)
    assert(mapped == EnumMemberRename.KeepValue)
  }
}
