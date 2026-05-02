// NOTE: This test references generated symbols from the m28-ok/u64-map-key.baboon
// fixture (my.ok.m28.u64key.{Holder, Holder_JsonCodec}) which are copied/generated
// into this stub only by `mdl :build :test-gen-regular-adt`
// (rsync + codegen into target/test-regular/sc-stub/). Running `sbt test`
// directly from the source tree will fail with missing symbols; run the
// test suite from the codegen'd copy.
//
// Round-trip test for u64 map keys (PR-28.1, closes M26-N02(b)).
// Verifies:
//   1. Encoded JSON keys carry the canonical unsigned wire form
//      (u64 max = "18446744073709551615", NOT signed "-1").
//   2. Decode accepts unsigned-i64 strings (KeyDecoder[Long], not Decoder[Long]).
//   3. The full Map[Long, String] round-trips for the two boundary values
//      (Long.MaxValue, -1L = u64 max) plus zero.
package runtime

import baboon.runtime.shared.BaboonCodecContext
import org.scalatest.funsuite.AnyFunSuite

class U64MapKeyRoundTripSpec extends AnyFunSuite {

  private val ctx = BaboonCodecContext.Compact

  test("Holder with map[u64, str]: canonical unsigned wire form on encode + round-trip") {
    val holder = my.ok.m28.u64key.Holder(
      m = Map(
        Long.MaxValue -> "vmax",
        -1L           -> "vu64max",
        0L            -> "vzero",
      )
    )
    val encoded    = my.ok.m28.u64key.Holder_JsonCodec.instance.encode(ctx, holder)
    val encodedStr = encoded.noSpaces
    // Canonical: u64 max = "18446744073709551615", Long.MaxValue = "9223372036854775807".
    // The signed two's-complement form ("-1") MUST NOT appear.
    assert(encodedStr.contains("\"18446744073709551615\""), s"u64 max key missing: $encodedStr")
    assert(encodedStr.contains("\"9223372036854775807\""), s"Long.MaxValue key missing: $encodedStr")
    assert(encodedStr.contains("\"0\""), s"zero key missing: $encodedStr")
    assert(!encodedStr.contains("\"-1\""), s"signed two's-complement leaked: $encodedStr")

    val decoded = my.ok.m28.u64key.Holder_JsonCodec.instance.decode(ctx, encoded) match {
      case Right(h) => h
      case Left(t)  => fail(s"JSON decode failed: $t")
    }
    assert(decoded == holder, s"round-trip diverged: $decoded vs $holder")
  }
}
