package io.septimalmind.baboon.tests

import fastparse.*
import io.septimalmind.baboon.parser.ParserContext
import io.septimalmind.baboon.parser.model.{FSPath, RawDomain}
import io.septimalmind.baboon.translator.{ServiceContextResolver, ServiceResultResolver}
import izumi.fundamentals.collections.nonempty.NEString
import org.scalatest.wordspec.AnyWordSpec

/** Parser tests for pragma keys (D14).
  *
  * The canonical resolver key `{lang}.service.result.no-errors` is hyphenated, but
  * pragma keys used to be parsed as plain dot-separated identifiers, so the key was
  * impossible to express as an in-file pragma (parser error at the hyphen) while the
  * CLI (`--pragma`, `--service-result-no-errors`) accepted it. Pins:
  * - hyphenated segments parse and land in RawDomain.pragmas verbatim
  * - EVERY resolver-known pragma key round-trips through the in-file pragma parser
  * - hyphens are word-chaining only: leading/trailing/double hyphens stay rejected
  */
final class PragmaKeyParserTest extends AnyWordSpec {

  private val dummyPath = FSPath.parse(NEString.unsafeFrom("pragma-key-parser-test.baboon"))

  private def ctx(input: String): ParserContext = ParserContext(dummyPath, input)

  private def parseModel(source: String): RawDomain = {
    val c = ctx(source)
    fastparse.parse(c.content, c.defModel.model(_)) match {
      case Parsed.Success(value, idx) =>
        assert(idx == source.length, s"parser left unconsumed input: [${source.drop(idx)}]")
        value
      case f: Parsed.Failure =>
        fail(s"expected parse success, got: ${f.msg}\nfor source:\n$source")
    }
  }

  private def expectParseFailure(source: String): Unit = {
    val c = ctx(source)
    fastparse.parse(c.content, c.defModel.model(_)) match {
      case Parsed.Success(_, idx) if idx == source.length =>
        fail(s"expected parse failure, but the full input parsed:\n$source")
      case _ => ()
    }
  }

  private def modelWithPragma(key: String): String =
    s"""model x.y
       |version "1"
       |pragma $key = "v"
       |data Foo { x: i32 }
       |""".stripMargin

  "pragma key parser (D14)" should {

    "accept the hyphenated no-errors key in-file" in {
      val m = parseModel(modelWithPragma("rust.service.result.no-errors"))
      assert(m.pragmas.map(p => (p.key, p.value)) == Seq(("rust.service.result.no-errors", "v")))
    }

    "accept every resolver-known pragma key in-file" in {
      val knownKeys = (ServiceResultResolver.knownPragmaKeys ++ ServiceContextResolver.knownPragmaKeys).map(_._1)
      assert(knownKeys.nonEmpty)
      knownKeys.foreach {
        key =>
          val m = parseModel(modelWithPragma(key))
          assert(m.pragmas.map(_.key) == Seq(key), s"pragma key '$key' did not round-trip")
      }
    }

    "still accept plain dotted identifier keys" in {
      val m = parseModel(modelWithPragma("scala.service.result.type"))
      assert(m.pragmas.map(_.key) == Seq("scala.service.result.type"))
    }

    "keep multiple pragmas with mixed key shapes" in {
      val src =
        """model x.y
          |version "1"
          |pragma scala.service.result.pattern = "[$error, $success]"
          |pragma scala.service.result.no-errors = "false"
          |data Foo { x: i32 }
          |""".stripMargin
      val m = parseModel(src)
      assert(
        m.pragmas.map(_.key) == Seq(
          "scala.service.result.pattern",
          "scala.service.result.no-errors",
        )
      )
    }

    "reject a leading hyphen in a key segment" in {
      expectParseFailure(modelWithPragma("a.-b"))
    }

    "reject a trailing hyphen in a key segment" in {
      expectParseFailure(modelWithPragma("a.b-"))
    }

    "reject a double hyphen in a key segment" in {
      expectParseFailure(modelWithPragma("a.b--c"))
    }
  }
}
