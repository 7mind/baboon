package io.septimalmind.baboon.typer

import io.circe.Json
import io.septimalmind.baboon.parser.BaboonParser
import io.septimalmind.baboon.parser.model.FSPath
import io.septimalmind.baboon.parser.model.issues.{BaboonIssue, RuntimeCodecIssue}
import io.septimalmind.baboon.tests.BaboonTest
import io.septimalmind.baboon.typer.model.{BaboonFamily, Pkg, Version}
import izumi.fundamentals.collections.nonempty.{NEList, NEString}

import scala.util.Try

/** JSON→UEBA conversion of `bytes` values and map keys follows the unified hex grammar
  * (https://github.com/7mind/baboon/issues/102, https://github.com/7mind/baboon/issues/103):
  * ASCII whitespace, `:` and `-` are ignored, the rest must be an even number of ASCII hex digits,
  * and anything else is reported as an issue instead of being thrown or silently accepted.
  */
final class BaboonRuntimeCodecBytesTest extends BaboonTest[Either] {
  private val specBody =
    """model test.bytes.codec
      |version "1.0.0"
      |root data Holder {
      |  b: bytes
      |  m: map[bytes, str]
      |}
      |""".stripMargin

  private val pkg     = Pkg(NEList("test", "bytes", "codec"))
  private val version = Version.parse("1.0.0")
  private val typeId  = "test.bytes.codec/:#Holder"

  private val valid: List[(String, String)] = List(
    ""             -> "",
    "ABCD"         -> "ABCD",
    "abcd"         -> "ABCD",
    "aBcD"         -> "ABCD",
    "AB CD"        -> "ABCD",
    "AB:CD"        -> "ABCD",
    "AB-CD"        -> "ABCD",
    "A B C D"      -> "ABCD",
    "\tAB\r\nCD "  -> "ABCD",
    "A:B-C D"      -> "ABCD",
    "\u000BAB\fCD" -> "ABCD",
    "00FF7f"       -> "00FF7F",
    " - : "        -> "",
  )

  private val invalid: List[String] = List(
    "ZZ",
    "AQ",
    "+1",
    "0x01",
    "q80=",
    "ABC",
    "A",
    "-1",
    "٣٣",
    "ＡＡ",
    "aéb",
    "AB CD",
  )

  private def load(manager: BaboonFamilyManager[Either]): BaboonFamily = {
    val input = BaboonParser.Input(FSPath.parse(NEString.unsafeFrom("bytes-codec-test.baboon")), specBody)
    manager.load(List(input)).fold(issues => fail(s"fixture failed to load: $issues"), identity)
  }

  private def holder(value: String, key: Option[String]): Json =
    Json.obj("b" -> Json.fromString(value), "m" -> Json.obj(key.map(k => k -> Json.fromString("x")).toList*))

  /** A thrown exception is the defect's other form, so it is captured rather than propagated. */
  private def encode(codec: BaboonRuntimeCodec[Either], family: BaboonFamily, json: Json): Either[String, Either[BaboonIssue, Vector[Byte]]] =
    Try(codec.encode(family, pkg, version, typeId, json, indexed = false)).toEither.left.map(e => s"thrown: $e")

  "BaboonRuntimeCodec bytes encoder" should {
    "accept every valid hex spelling as a value and as a map key" in {
      (manager: BaboonFamilyManager[Either], codec: BaboonRuntimeCodec[Either]) =>
        val family = load(manager)
        valid.foreach {
          case (input, canonical) =>
            List(holder(input, None) -> holder(canonical, None), holder("", Some(input)) -> holder("", Some(canonical))).foreach {
              case (json, expected) =>
                val decoded = encode(codec, family, json).flatMap(_.left.map(_.toString)).flatMap {
                  bytes => codec.decode(family, pkg, version, typeId, bytes).left.map(_.toString)
                }
                assert(decoded == Right(expected), s"input ${json.noSpaces}")
            }
        }
    }

    "report malformed hex as InvalidHexString instead of throwing or accepting it" in {
      (manager: BaboonFamilyManager[Either], codec: BaboonRuntimeCodec[Either]) =>
        val family = load(manager)
        invalid.foreach {
          input =>
            List(holder(input, None), holder("", Some(input))).foreach {
              json =>
                encode(codec, family, json) match {
                  case Right(Left(BaboonIssue.RuntimeCodec(issue: RuntimeCodecIssue.InvalidHexString))) =>
                    assert(issue.value == input)
                  case other =>
                    fail(s"input ${json.noSpaces}: expected InvalidHexString, got $other")
                }
            }
        }
    }
  }
}
