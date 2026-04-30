// NOTE: This test references generated symbols from the m19-ok/wrapper-around-foreign.baboon
// fixture (my.ok.m19.foreign.{FStr, ItemKey, Holder, Holder_JsonCodec}) which are copied/generated
// into this stub only by `mdl :build :test-gen-regular-adt`
// (rsync + codegen into target/test-regular/sc-stub/). Running `sbt test`
// directly from the source tree will fail with missing symbols; run the
// test suite from the codegen'd copy.
//
// Round-trip test for Custom-foreign map keys (PR-66-D01 fix).
// Verifies that Holder_JsonCodec.encode(...).decode(...) round-trips cleanly when the
// map key is a wrapper DTO (ItemKey) whose single field is a Custom-foreign type (FStr)
// that maps to java.lang.String in Scala.
package runtime

import baboon.runtime.shared.BaboonCodecContext
import org.scalatest.funsuite.AnyFunSuite

class ForeignMapKeyRoundTripSpec extends AnyFunSuite {

  private val ctx = BaboonCodecContext.Compact

  test("Holder with map[ItemKey, str]: JSON encode then decode round-trips cleanly (PR-66-D01)") {
    val original = my.ok.m19.foreign.Holder(
      m = Map(
        my.ok.m19.foreign.ItemKey(v = "alpha") -> "1",
        my.ok.m19.foreign.ItemKey(v = "beta")  -> "2",
      )
    )
    val encoded = my.ok.m19.foreign.Holder_JsonCodec.instance.encode(ctx, original)
    val decoded = my.ok.m19.foreign.Holder_JsonCodec.instance.decode(ctx, encoded) match {
      case Right(h) => h
      case Left(t)  => fail(s"JSON decode failed: $t")
    }
    assert(decoded == original, s"round-trip diverged: $decoded vs $original")
  }

  test("Holder with empty map: JSON encode then decode round-trips cleanly") {
    val original = my.ok.m19.foreign.Holder(m = Map.empty)
    val encoded  = my.ok.m19.foreign.Holder_JsonCodec.instance.encode(ctx, original)
    val decoded = my.ok.m19.foreign.Holder_JsonCodec.instance.decode(ctx, encoded) match {
      case Right(h) => h
      case Left(t)  => fail(s"JSON decode failed: $t")
    }
    assert(decoded == original)
  }
}
