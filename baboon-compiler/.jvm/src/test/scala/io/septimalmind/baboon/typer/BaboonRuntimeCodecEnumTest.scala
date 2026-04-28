package io.septimalmind.baboon.typer

import io.circe.Json
import io.septimalmind.baboon.parser.BaboonParser
import io.septimalmind.baboon.parser.model.FSPath
import io.septimalmind.baboon.parser.model.issues.{BaboonIssue, RuntimeCodecIssue}
import io.septimalmind.baboon.tests.BaboonTest
import io.septimalmind.baboon.tests.BaboonTest.BaboonTestModule
import io.septimalmind.baboon.typer.BaboonRuntimeCodec
import io.septimalmind.baboon.typer.model.{Pkg, Version}
import izumi.functional.bio.{Error2, F}
import izumi.fundamentals.collections.nonempty.{NEList, NEString}
import izumi.reflect.TagKK

final class BaboonRuntimeCodecEnumTest extends BaboonRuntimeCodecEnumTestBase[Either]

// Tests for BaboonRuntimeCodec enum encode/decode behaviors introduced in M15 PR-35:
// - encoder matches members by wire name (Pascal), not raw name
// - encoder rejects input that does not match any member's wire name
// - decoder emits the Pascal wire name, not the raw member name
abstract class BaboonRuntimeCodecEnumTestBase[F[+_, +_]: Error2: TagKK: BaboonTestModule]
    extends BaboonTest[F] {

  // Single-version fixture: enum with non-Pascal member names.
  // Wire names: "cafe" -> "Cafe", "bar_pub" -> "Bar_pub" (via EnumWireStyle.wireName).
  // Enums cannot be marked `root`; a wrapper DTO is used to pull MyEnum into the root set.
  private val specBody =
    """model test.enum.codec
      |version "1.0.0"
      |enum MyEnum { cafe  bar_pub }
      |root data Holder { e: MyEnum }
      |""".stripMargin

  // TypeId.User(Pkg(NEList("test","enum","codec")), Owner.Toplevel, TypeName("MyEnum")).toString
  // = "test.enum.codec/:#MyEnum"
  private val pkg     = Pkg(NEList("test", "enum", "codec"))
  private val version = Version.parse("1.0.0")
  private val typeId  = "test.enum.codec/:#MyEnum"

  private def makeInput(name: String, body: String): BaboonParser.Input =
    BaboonParser.Input(FSPath.parse(NEString.unsafeFrom(name)), body)

  "BaboonRuntimeCodec enum encoder" should {

    // The encoder looks up wire names via EnumWireStyle.wireName; "Cafe" is the wire form of
    // the source member "cafe".  Encoding should succeed and round-trip back to "Cafe".
    "accept the Pascal wire name and encode it as the member index" in {
      (manager: BaboonFamilyManager[F], codec: BaboonRuntimeCodec[F]) =>
        for {
          family  <- manager.load(List(makeInput("codec-test.baboon", specBody)))
          encoded <- codec.encode(family, pkg, version, typeId, Json.fromString("Cafe"), indexed = false)
          decoded <- codec.decode(family, pkg, version, typeId, encoded)
        } yield {
          assert(decoded == Json.fromString("Cafe"))
        }
    }

    // The encoder matches JSON string input against EnumWireStyle.wireName(member.name).
    // "cafe" (lowercase) does not equal "Cafe", so no member is found -> UnknownEnumValue.
    "reject a lowercase member name that does not match any wire name" in {
      (manager: BaboonFamilyManager[F], codec: BaboonRuntimeCodec[F]) =>
        for {
          family <- manager.load(List(makeInput("codec-test.baboon", specBody)))
          result <- F.attempt(
            codec.encode(family, pkg, version, typeId, Json.fromString("cafe"), indexed = false)
          )
        } yield {
          result match {
            case Left(BaboonIssue.RuntimeCodec(_: RuntimeCodecIssue.UnknownEnumValue)) =>
              succeed
            case other =>
              fail(s"Expected UnknownEnumValue, got: $other")
          }
        }
    }
  }

  "BaboonRuntimeCodec enum decoder" should {

    // The decoder reads a byte index and returns EnumWireStyle.wireName(member.name).
    // For member "cafe" at index 0, the result is "Cafe" — not the raw source name.
    // Synthetic bytes (just the ordinal byte for index 0) are passed directly so this
    // test exercises only the decoder, independent of the encoder's behavior.
    "emit the Pascal wire name, not the raw source member name" in {
      (manager: BaboonFamilyManager[F], codec: BaboonRuntimeCodec[F]) =>
        for {
          family  <- manager.load(List(makeInput("codec-test.baboon", specBody)))
          decoded <- codec.decode(family, pkg, version, typeId, Vector(0.toByte))
        } yield {
          // Must be "Cafe" (Pascal), NOT "cafe" (raw source name)
          assert(decoded == Json.fromString("Cafe"))
          assert(decoded != Json.fromString("cafe"))
        }
    }
  }
}
