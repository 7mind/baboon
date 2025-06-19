package example

import convtest.testpkg.{Adt0, BaboonConversions, RequiredConversions}
import org.scalatest.flatspec.AnyFlatSpec

class Test_Conversions extends AnyFlatSpec {
  "derived conversions" should "support ADT auto upgrades" in {
    val a1   = new convtest.testpkg.v1_0_0.Adt0.B1("val1")
    val conv = new BaboonConversions(new RequiredConversions() {})

    val a1u1 = conv.convertWithContext[Unit, convtest.testpkg.v1_0_0.Adt0.B1, convtest.testpkg.Adt0.B1]((), a1)

    assert(a1.f == a1u1.f)

    val a1u2 = conv.convertWithContext[Unit, convtest.testpkg.v1_0_0.Adt0, convtest.testpkg.Adt0]((), a1)

    assert(a1u1 == a1u2)

  }
}
