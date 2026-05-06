// NOTE: This test references generated runtime symbols (BaboonTypeMeta,
// BaboonTypeMetaCodec, ...) which are copied into this stub only by
// `mdl :build :test-gen-regular-adt` (rsync + codegen into
// target/test-regular/sc-stub/). Running `sbt test` directly from the source
// tree will fail with missing symbols; run from the codegen'd copy.
package runtime

import baboon.runtime.shared._
import io.circe.Json
import org.scalatest.funsuite.AnyFunSuite

class BaboonTypeMetaCodecSpec extends AnyFunSuite {

  private def buildMeta(): BaboonTypeMeta =
    BaboonTypeMeta(
      BaboonTypeMetaCodec.META_VERSION,
      "com.example.dom",
      "1.0.0",
      "1.0.0",
      "MyType",
    )

  // [MFACADE-PR-3-D04] writer emits numeric $mv
  // BaboonTypeMetaCodec.writeJson must emit "$mv" as a JSON number equal to 1.
  // The reader was already accepting string "1"; option β requires the writer to
  // emit a numeric value so round-trip consumers that only accept numbers also work.
  test("BaboonTypeMetaCodec.writeJson emits $mv as a JSON number equal to 1") {
    val meta = buildMeta()
    val json = meta.writeJson
    assert(
      json.hcursor.downField("$mv").focus.exists(_.isNumber),
      s"$$mv must be a JSON number; got: $json",
    )
    assert(
      json.hcursor.downField("$mv").as[Int].toOption.contains(1),
      s"$$mv must equal 1; got: $json",
    )
  }

  // [MFACADE-PR-3-D05] reader accepts numeric $mv
  // BaboonTypeMeta.readMeta must parse a JSON envelope where "$mv" is the
  // number 1 (not the string "1") and return a non-empty Option[BaboonTypeMeta].
  test("BaboonTypeMeta.readMeta accepts numeric $mv = 1") {
    val json = Json.obj(
      "$mv" -> Json.fromInt(1),
      "$d"  -> Json.fromString("com.example.dom"),
      "$v"  -> Json.fromString("1.0.0"),
      "$t"  -> Json.fromString("MyType"),
    )
    val result = BaboonTypeMeta.readMeta(json)
    assert(result.isDefined, s"readMeta must return Some(...) for numeric $$mv=1; got None")
    assert(
      result.exists(_.domainIdentifier == "com.example.dom"),
      s"parsed meta must carry domainIdentifier; got: $result",
    )
  }
}
