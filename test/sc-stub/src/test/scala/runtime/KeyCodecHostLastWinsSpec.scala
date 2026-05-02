// PR-26.2 (M26) — KeyCodec Host last-wins concurrency contract regression test.
//
// Asserts that calling `FStr_KeyCodec.register(impl)` twice overwrites the
// previously registered implementation (last-wins). Scala already used a
// `@volatile var` mutable singleton pre-PR-26.2 — this test pins that
// behavior so a future refactor toward an OnceLock-shaped impl would fail.
//
// NOTE: This test references generated symbols from m19-ok/wrapper-around-foreign.baboon
// (`my.ok.m19.foreign.{FStr_KeyCodec, Holder, Holder_JsonCodec}`) which are
// emitted into this stub only by `mdl :build :test-gen-regular-adt`
// (rsync + codegen into target/test-regular/sc-stub/). Running `sbt test`
// directly from the source tree will fail with missing symbols.
package runtime

import baboon.runtime.shared.BaboonCodecContext
import org.scalatest.BeforeAndAfter
import org.scalatest.funsuite.AnyFunSuite

class KeyCodecHostLastWinsSpec extends AnyFunSuite with BeforeAndAfter {

  private val ctx = BaboonCodecContext.Compact

  private final class PrefixCodec(tag: String) extends my.ok.m19.foreign.FStr_KeyCodec {
    def encodeKey(value: String): String = s"$tag:$value"
    def decodeKey(s: String): String = {
      val pfx = s"$tag:"
      if (s.startsWith(pfx)) s.substring(pfx.length) else s
    }
  }

  private final class IdentityCodec extends my.ok.m19.foreign.FStr_KeyCodec {
    def encodeKey(value: String): String = value
    def decodeKey(s: String): String     = s
  }

  // PR-26.2-D01: restore identity impl after each test so the global
  // FStr_KeyCodec singleton does not leak a PrefixCodec into sibling specs
  // (e.g., ForeignMapKeyRoundTripSpec) sharing the JVM. Runs even on
  // assertion failure, unlike the prior inline restore at end-of-test.
  after {
    my.ok.m19.foreign.FStr_KeyCodec.register(new IdentityCodec)
  }

  test("PR-26.2: register(B) after register(A) → encode observes B (last-wins, NOT A)") {
    val original = my.ok.m19.foreign.Holder(
      m = Map(my.ok.m19.foreign.ItemKey(v = "k") -> "v")
    )

    my.ok.m19.foreign.FStr_KeyCodec.register(new PrefixCodec("A"))
    val encodedA = my.ok.m19.foreign.Holder_JsonCodec.instance.encode(ctx, original)
    assert(
      encodedA.noSpaces.contains("A:k"),
      s"expected A: prefix in encoded wire form, got ${encodedA.noSpaces}",
    )

    my.ok.m19.foreign.FStr_KeyCodec.register(new PrefixCodec("B"))
    val encodedB = my.ok.m19.foreign.Holder_JsonCodec.instance.encode(ctx, original)
    assert(
      encodedB.noSpaces.contains("B:k"),
      s"PR-26.2 last-wins regression: expected B: prefix after re-register, got ${encodedB.noSpaces}",
    )
    assert(
      !encodedB.noSpaces.contains("A:k"),
      s"PR-26.2 last-wins regression: A: prefix still present after B re-register, got ${encodedB.noSpaces}",
    )
  }
}
