// T43 — Interface-shape assertions for extracted contracts (Scala stub).
//
// Generated symbols (my.ok.extracted.contracts.*) are emitted by
// `mdl :build :test-gen-regular-adt`. Running `sbt test` directly from the
// source tree will fail with missing symbols; run from the codegen'd copy.
//
// Coverage:
//   (a) Contract variant — host instantiation is assignable to B (compile-time
//       val typed as B), and a B-declared member is readable at runtime
//       through the B-typed reference. Failures throw unconditionally.
//   (b) Mirror variant — B exists as a standalone trait with the expected
//       member set (compile-time usage of every member), AND the host type
//       does NOT extend it. Negative verified via `isInstanceOf` check.
//   (c) Member-set spot-check — field names and types verified through
//       B-typed bindings (compile-time + runtime).
//
// Runtime checks use explicit throws, not org.scalatest assert, because
// assert may be eliminated by the JVM assertion flag.
package runtime

import org.scalatest.funsuite.AnyFunSuite
import my.ok.extracted.contracts._

class ExtractedContractsShapeSpec extends AnyFunSuite {

  private def require(condition: Boolean, message: => String): Unit =
    if (!condition) throw new RuntimeException(s"ExtractedContractsShapeSpec: $message")

  // ── (a) Contract variant: host instantiation assignable to B ───────────────

  test("IBox contract variant: IntBox is assignable to IBox, count readable") {
    val host = IntBox(count = 7, item = 42)

    // Compile-time: IntBox extends IBox — the type annotation enforces it.
    val b: IBox = host

    val countViaB: Int = b.count
    require(countViaB == 7, s"IBox.count must equal 7 but was $countViaB")
  }

  test("IBox contract variant: StrBox also assignable to same IBox (req 8 — shared B)") {
    val strHost = StrBox(count = 3, item = "hello")
    val b: IBox = strHost
    val countViaB: Int = b.count
    require(countViaB == 3, s"IBox.count (via StrBox) must equal 3 but was $countViaB")
  }

  test("IKey contract variant: IntKey is assignable to IKey, key readable") {
    val host = IntKey(key = 999L, v = 1)
    val b: IKey = host
    val keyViaB: Long = b.key
    require(keyViaB == 999L, s"IKey.key must equal 999 but was $keyViaB")
  }

  test("ITagged contract variant: IntTagged is assignable to ITagged, label readable") {
    val host = IntTagged(label = "hello", extra = 5)
    val b: ITagged = host
    val labelViaB: String = b.label
    require(labelViaB == "hello", s"ITagged.label must equal 'hello' but was '$labelViaB'")
  }

  test("IContainer contract variant: IntContainer is assignable to IContainer, members readable") {
    val host = IntContainer(own = 1, item = 99, first = 2, second = 3, base_field = 4)
    val b: IContainer = host

    // Spot-check all three B-declared param-free fields.
    require(b.own == 1, s"IContainer.own must equal 1 but was ${b.own}")
    require(b.second == 3, s"IContainer.second must equal 3 but was ${b.second}")
    require(b.base_field == 4, s"IContainer.base_field must equal 4 but was ${b.base_field}")
  }

  test("IResult contract variant: IntResult.Ok is assignable to IResult, tag readable") {
    val host = IntResult.Ok(tag = "ok", result = 0)
    val b: IResult = host
    // IResult extends ResultBase which declares tag.
    val tagViaB: String = b.tag
    require(tagViaB == "ok", s"IResult.tag must equal 'ok' but was '$tagViaB'")
  }

  // ── (b) Mirror variant: standalone trait + negative check ─────────────────

  test("IMirroredPayload mirror variant: IntPayloadProbe is assignable to IMirroredPayload, label readable") {
    val probe = IntPayloadProbe(label = "mirror-test")
    val b: IMirroredPayload = probe
    val labelViaB: String = b.label
    require(labelViaB == "mirror-test",
      s"IMirroredPayload.label must equal 'mirror-test' but was '$labelViaB'")
  }

  test("IMirroredPayload mirror variant: IntPayload does NOT extend IMirroredPayload") {
    // Negative (mirror variant): IntPayload is the host template; it does NOT extend
    // IMirroredPayload. Only the probe (IntPayloadProbe) carries the mirror B.
    //
    // The following would NOT compile (type mismatch):
    //   val b: IMirroredPayload = IntPayload(label = "x", value = 1)  // type error
    //
    // Scala final case classes cause "fruitless type test" warnings (promoted to errors
    // via -Wconf) on direct isInstanceOf checks. Cast to Any first to make the check
    // runtime-evaluated without triggering the static analysis warning.
    val host: IntPayload = IntPayload(label = "x", value = 1)
    val hostIsB: Boolean = (host: Any).isInstanceOf[IMirroredPayload]
    require(!hostIsB,
      "IntPayload must NOT be an IMirroredPayload (it is the mirror host, not a probe)")
  }

  test("ITagMirror mirror variant: IntTagMirrorProbe is assignable to ITagMirror, label readable") {
    val probe = IntTagMirrorProbe(label = "tag-mirror")
    val b: ITagMirror = probe
    val labelViaB: String = b.label
    require(labelViaB == "tag-mirror",
      s"ITagMirror.label must equal 'tag-mirror' but was '$labelViaB'")
  }

  test("ITagMirror mirror variant: IntTagged does NOT extend ITagMirror") {
    // IntTagged implements ITagged (contract), not ITagMirror (mirror).
    //
    //   val b: ITagMirror = IntTagged(label = "x", extra = 1)  // type error
    //
    // Cast to Any to avoid "fruitless type test" promoted to error by -Wconf.
    val host: IntTagged = IntTagged(label = "x", extra = 1)
    val hostIsB: Boolean = (host: Any).isInstanceOf[ITagMirror]
    require(!hostIsB,
      "IntTagged must NOT be an ITagMirror (it carries ITagged, the contract variant)")
  }

  // ── (c) Member-set spot-check ──────────────────────────────────────────────

  test("IMirroredPayload member set: has exactly label: String") {
    val probe = IntPayloadProbe(label = "spot")
    val b: IMirroredPayload = probe
    val label: String = b.label  // compile-time type assertion: String
    require(label == "spot", s"IMirroredPayload.label spot-check failed: '$label'")
  }

  test("IBox member set: has exactly count: Int") {
    val box = IntBox(count = 11, item = 0)
    val b: IBox = box
    val count: Int = b.count  // compile-time: Int
    require(count == 11, s"IBox.count spot-check failed: $count")
  }

  test("IKey member set: has exactly key: Long") {
    val key = IntKey(key = 12345L, v = 0)
    val b: IKey = key
    val k: Long = b.key  // compile-time: Long
    require(k == 12345L, s"IKey.key spot-check failed: $k")
  }

  test("IContainer member set: has own: Int, second: Int, base_field: Int") {
    val c = IntContainer(own = 10, item = 0, first = 0, second = 20, base_field = 30)
    val b: IContainer = c
    require(b.own == 10, s"IContainer.own spot-check failed: ${b.own}")
    require(b.second == 20, s"IContainer.second spot-check failed: ${b.second}")
    require(b.base_field == 30, s"IContainer.base_field spot-check failed: ${b.base_field}")
  }
}
