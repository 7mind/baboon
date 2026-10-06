package io.septimalmind.baboon.tests

import io.circe.Json
import io.circe.parser.parse
import io.septimalmind.baboon.BaboonLoader
import io.septimalmind.baboon.parser.model.issues.{BaboonIssue, IssuePrinter}
import io.septimalmind.baboon.typer.BaboonRuntimeCodec
import io.septimalmind.baboon.typer.model.{BaboonFamily, Pkg, Version}
import izumi.fundamentals.collections.nonempty.NEList
import izumi.fundamentals.platform.files.IzFiles
import izumi.fundamentals.platform.resources.IzResources

import scala.util.Try

/** The runtime codec accepts each integer type's exact range, in values and in map keys, and rejects
  * everything outside it instead of wrapping (https://github.com/7mind/baboon/issues/97).
  */
final class RuntimeCodecIntegerRangeTest extends BaboonTest[Either] {
  private val Shapes  = Pkg(NEList("zipdemo", "shapes"))
  private val V1      = Version.parse("1.0.0")
  private val Ranges  = "zipdemo.shapes/:#Ranges"
  private val Keys    = "zipdemo.shapes/:#RangeKeys"
  private val Minimum = """{"a":-128,"b":0,"c":-32768,"d":0,"e":-2147483648,"u":0,"f":"-9223372036854775808","g":"0"}"""
  private val Maximum =
    """{"a":127,"b":255,"c":32767,"d":65535,"e":2147483647,"u":4294967295,"f":"9223372036854775807","g":"18446744073709551615"}"""

  private def load(loader: BaboonLoader[Either]): BaboonFamily = {
    val path  = IzResources.getPath("scheme-zip-ok/shapes").get.asInstanceOf[IzResources.LoadablePathReference].path
    val files = IzFiles.walk(path.toFile).toList.filter(p => p.toFile.isFile && p.toFile.getName.endsWith(".baboon"))
    loader.load(files).fold(issues => fail(s"fixture failed to load: $issues"), identity)
  }

  private def json(s: String): Json = parse(s).fold(e => fail(e.toString), identity)

  /** A failure, whether reported as an issue or (the defect's other form) thrown. */
  private def encoded(codec: BaboonRuntimeCodec[Either], family: BaboonFamily, id: String, value: String): Either[String, Vector[Byte]] = {
    Try(codec.encode(family, Shapes, V1, id, json(value), indexed = false)).toEither.left
      .map(e => s"thrown: $e").flatMap(_.left.map(issue => IssuePrinter[BaboonIssue].stringify(issue)))
  }

  private def roundTrips(codec: BaboonRuntimeCodec[Either], family: BaboonFamily, id: String, value: String): Unit = {
    val bytes = encoded(codec, family, id, value).fold(e => fail(s"$value: $e"), identity)
    assert(codec.decode(family, Shapes, V1, id, bytes).map(_.noSpaces) == Right(json(value).noSpaces), value)
  }

  "the runtime codec" should {
    "round-trip every integer type at both ends of its range" in {
      (loader: BaboonLoader[Either], codec: BaboonRuntimeCodec[Either]) =>
        val family = load(loader)
        roundTrips(codec, family, Ranges, Minimum)
        roundTrips(codec, family, Ranges, Maximum)
        roundTrips(
          codec,
          family,
          Keys,
          """{"a":{"-128":"x","127":"y"},"b":{"0":"x","255":"y"},"d":{"65535":"y"},"u":{"4294967295":"y"},"g":{"18446744073709551615":"y"}}""",
        )
    }

    "reject integers outside their type's range instead of wrapping them" in {
      (loader: BaboonLoader[Either], codec: BaboonRuntimeCodec[Either]) =>
        val family = load(loader)
        val outside = List(
          "a" -> "128",
          "a" -> "-129",
          "b" -> "256",
          "b" -> "-1",
          "c" -> "32768",
          "c" -> "-32769",
          "d" -> "65536",
          "d" -> "-1",
          "e" -> "2147483648",
          "u" -> "4294967296",
          "u" -> "-1",
          "f" -> "\"9223372036854775808\"",
          "f" -> "\"-9223372036854775809\"",
          "g" -> "\"18446744073709551616\"",
          "g" -> "\"-1\"",
          "g" -> "18446744073709551616",
          "a" -> "1.5",
        )
        outside.foreach {
          case (field, value) =>
            val input = json(Maximum).mapObject(_.add(field, json(value))).noSpaces
            val error = encoded(codec, family, Ranges, input).fold(identity, bytes => fail(s"$field=$value was encoded as $bytes"))
            assert(error.startsWith("Expected "), s"$field=$value: $error")
        }
        for ((field, key) <- List("a" -> "128", "b" -> "256", "b" -> "-1", "d" -> "65536", "u" -> "4294967296", "g" -> "18446744073709551616", "g" -> "-1", "b" -> "x")) {
          val input = s"""{"a":{},"b":{},"d":{},"u":{},"g":{},"$field":{"$key":"v"}}""".replace(s""""$field":{},""", "").replace(s""","$field":{}}""", "}")
          val error = encoded(codec, family, Keys, input).fold(identity, bytes => fail(s"$field key $key was encoded as $bytes"))
          assert(error.startsWith("Expected "), s"$field key $key: $error")
        }
    }
  }
}
