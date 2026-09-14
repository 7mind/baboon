// NOTE: This test references generated runtime symbols (BaboonTypeMeta,
// BaboonTypeMetaCodec, ...) which are copied into this stub only by
// `mdl :build :test-gen-regular-adt` (rsync + codegen into
// target/test-regular/sc-stub/). Running `sbt test` directly from the source
// tree will fail with missing symbols; run from the codegen'd copy.
package runtime

import baboon.runtime.shared._
import io.circe.Json
import org.scalatest.funsuite.AnyFunSuite

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}

class BaboonTypeMetaCodecSpec extends AnyFunSuite {

  // Hand-assembled v1 binary envelope head with an arbitrary hasMinCompat flag byte.
  private def binEnvelope(flag: Int, minCompat: Option[String]): Array[Byte] = {
    val baos = new ByteArrayOutputStream()
    val dos  = new LEDataOutputStream(baos)
    dos.write(BaboonTypeMetaCodec.META_VERSION.toInt)
    BaboonBinTools.writeString(dos, "com.example.dom")
    BaboonBinTools.writeString(dos, "2.0.0")
    dos.write(flag)
    minCompat.foreach(v => BaboonBinTools.writeString(dos, v))
    BaboonBinTools.writeString(dos, "MyType")
    baos.toByteArray
  }

  private def readBin(bytes: Array[Byte]): Option[BaboonTypeMeta] =
    BaboonTypeMeta.readMeta(new LEDataInputStream(new ByteArrayInputStream(bytes)))

  // codec-envelope.md §2.1: flag 0x00 = minCompat elided, 0x01 = minCompat follows,
  // "other flag values are illegal; readers reject them".
  test("binary readMeta honours flag 0 (elided) and 1 (present)") {
    assert(readBin(binEnvelope(0, None)).map(_.domainVersionMinCompat).contains("2.0.0"))
    assert(readBin(binEnvelope(1, Some("1.0.0"))).map(_.domainVersionMinCompat).contains("1.0.0"))
  }

  test("binary readMeta rejects an unknown hasMinCompat flag byte instead of misparsing it as elided") {
    // a lenient reader would treat 0x02 as "absent" and then read the min-compat string as the type id
    assert(readBin(binEnvelope(2, Some("1.0.0"))).isEmpty, "flag 0x02 must be rejected (None)")
    assert(readBin(binEnvelope(0xFF, None)).isEmpty, "flag 0xFF must be rejected (None)")
  }

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

  // [MFACADE-PR-3-D10] cross-backend rejection matrix for malformed $mv values.
  // Mirrors the cs/sw/dt/ts/py matrices (D06) so sc has parity. Reject: 1.5 (fractional),
  // true (boolean), 300 (out of byte range), -1 (negative), [] (array), {} (object).
  // Whole-valued doubles (1.0) ARE accepted on circe — circe normalises to Long when
  // representable, so the source-type info is lost; documented in spec § 4.
  Seq(
    ("fractional", Json.fromDoubleOrNull(1.5)),
    ("boolean", Json.fromBoolean(true)),
    ("out-of-range-300", Json.fromInt(300)),
    ("negative", Json.fromInt(-1)),
    ("array", Json.arr()),
    ("object", Json.obj()),
  ).foreach {
    case (label, mvValue) =>
      test(s"BaboonTypeMeta.readMeta rejects malformed $$mv ($label)") {
        val json = Json.obj(
          "$mv" -> mvValue,
          "$d"  -> Json.fromString("com.example.dom"),
          "$v"  -> Json.fromString("1.0.0"),
          "$t"  -> Json.fromString("MyType"),
        )
        val result = BaboonTypeMeta.readMeta(json)
        assert(
          result.isEmpty,
          s"readMeta must return None for malformed $$mv=$mvValue ($label); got: $result",
        )
      }
  }
}
