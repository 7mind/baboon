// NOTE: This test references generated symbols (identifier.ok.PointIdCodec,
// identifier.ok.LongIdCodec) which are produced by `mdl :build :test-gen-regular-adt`
// into target/test-regular/sc-stub/. Running `sbt test` directly from the source
// tree will fail with missing symbols; run via the mdl test pipeline.
//
// Regression test for PR-C (M24): Spec §5.4 prohibits a leading '+' on signed integer
// wire forms. Asserts that PointIdCodec.parseRepr("+42:label") and
// LongIdCodec.parseRepr("+1:x") return Left rather than silently accepting the value.
package runtime

import org.scalatest.funsuite.AnyFunSuite

class IdentifierSignedPlusRejectionSpec extends AnyFunSuite {

  // PointId has one i32 field (x) and one str field (label).
  // Feed a '+'-prefixed x value; expect Left with the rejection message.
  test("PointIdCodec.parseRepr rejects leading '+' on i32 field (Spec §5.4)") {
    val result = identifier.ok.PointIdCodec.parseRepr("PointId:1.0.0#x:+42:label:hello")
    assert(result.isLeft, s"expected Left but got: $result")
    val msg = result.left.getOrElse("")
    assert(msg.contains("leading '+'"), s"error message should mention the offending value; got: $msg")
  }

  // LongId has one i64 field (x).
  test("LongIdCodec.parseRepr rejects leading '+' on i64 field (Spec §5.4)") {
    val result = identifier.ok.LongIdCodec.parseRepr("LongId:1.0.0#x:+1")
    assert(result.isLeft, s"expected Left but got: $result")
    val msg = result.left.getOrElse("")
    assert(msg.contains("leading '+'"), s"error message should mention the offending value; got: $msg")
  }
}
