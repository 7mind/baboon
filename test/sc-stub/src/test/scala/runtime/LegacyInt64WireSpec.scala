// 64-bit integers are written as decimal strings (docs/json-codecs.md, "64-bit integers").
// Readers stay lenient about the JSON-number form an older compiler produced, and that
// leniency is what keeps documents written before the change readable.
//
// Nothing writes numbers any more, so without this test the number arm of every decoder is
// dead as far as the suite is concerned.
//
// Uses the identifier.ok fixture (id LongId { x: i64 }; id UInts { a: u08, b: u16, c: u32,
// d: u64 }); generated symbols are produced by mdl :test-gen-regular-adt.
package runtime

import baboon.runtime.shared.BaboonCodecContext
import io.circe.parser.parse
import org.scalatest.funsuite.AnyFunSuite

class LegacyInt64WireSpec extends AnyFunSuite {

  private val ctx = BaboonCodecContext.Compact

  private def json(s: String) = parse(s).getOrElse(fail(s"test setup: invalid JSON literal: $s"))

  test("i64 decodes from the legacy numeric form") {
    val decoded = identifier.ok.LongId_JsonCodec.instance.decode(ctx, json("""{"x":-9007199254740991}"""))
    assert(decoded == Right(identifier.ok.LongId(-9007199254740991L)))
  }

  test("i64 decodes from the string form") {
    val decoded = identifier.ok.LongId_JsonCodec.instance.decode(ctx, json("""{"x":"-9223372036854775808"}"""))
    assert(decoded == Right(identifier.ok.LongId(Long.MinValue)))
  }

  test("u64 decodes from the legacy numeric form") {
    val decoded = identifier.ok.UInts_JsonCodec.instance.decode(ctx, json("""{"a":1,"b":2,"c":3,"d":42}"""))
    assert(decoded.map(_.d) == Right(42L))
  }

  test("u64 decodes from the string form") {
    val decoded = identifier.ok.UInts_JsonCodec.instance.decode(ctx, json("""{"a":1,"b":2,"c":3,"d":"42"}"""))
    assert(decoded.map(_.d) == Right(42L))
  }
}
