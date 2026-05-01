// PR-F (M24) — cross-language malformed map-key error consistency.
//
// Verifies that decoding a JSON object whose map-key cannot be parsed back into the
// id type throws BaboonCodecException.DecoderFailure with message "malformed key: <repr>".
// Replaces the prior silent `.toOption` discard which produced an empty/wrong map.
//
// Uses the my.ok.m19.singleid fixture (id ItemId { v: uid }; root data Holder { m: map[ItemId, str] }).
// Generated symbols are emitted into target/test-regular/sc-stub/ by mdl :test-gen-regular-adt.
package runtime

import baboon.runtime.shared.{BaboonCodecContext, BaboonCodecException}
import io.circe.parser.parse
import org.scalatest.funsuite.AnyFunSuite

class MapKeyMalformedSpec extends AnyFunSuite {

  private val ctx = BaboonCodecContext.Compact

  test("Holder JSON decode throws DecoderFailure for malformed map key") {
    val badJson = """{"m":{"not_a_valid_id":"v"}}"""
    val parsed  = parse(badJson).getOrElse(fail("test setup: invalid JSON literal"))
    val ex = intercept[BaboonCodecException.DecoderFailure] {
      my.ok.m19.singleid.Holder_JsonCodec.instance.decode(ctx, parsed) match {
        case Right(_) => fail("expected exception, got Right")
        case Left(_)  => fail("expected exception, got Left")
      }
    }
    assert(ex.getMessage.contains("malformed key"), s"unexpected message: ${ex.getMessage}")
  }
}
