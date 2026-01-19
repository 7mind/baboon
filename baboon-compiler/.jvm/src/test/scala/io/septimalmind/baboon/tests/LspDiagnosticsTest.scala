package io.septimalmind.baboon.tests

import fastparse.Parsed
import io.septimalmind.baboon.lsp.features.DiagnosticsProvider
import io.septimalmind.baboon.lsp.state.CompilationResult
import io.septimalmind.baboon.lsp.util.{JvmPathOps, PositionConverter}
import io.septimalmind.baboon.parser.ParserContext
import io.septimalmind.baboon.parser.model.FSPath
import io.septimalmind.baboon.parser.model.issues.{BaboonIssue, ParserIssue}
import izumi.fundamentals.collections.nonempty.NEString
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

final class LspDiagnosticsTest extends AnyWordSpec with Matchers {
  "DiagnosticsProvider" should {
    "map parser failures to the actual error position" in {
      val content =
        """model test.lsp
          |
          |version "1.0.0"
          |
          |data X {
          |  a: i32
          |  b
          |}
          |""".stripMargin

      val path    = FSPath.parse(NEString.unsafeFrom("test.lsp.baboon"))
      val context = ParserContext(path, content)
      val failure = fastparse.parse(context.content, context.defModel.model(_)) match {
        case f: Parsed.Failure => f
        case Parsed.Success(_, _) =>
          fail("Expected a parse failure for malformed input")
      }

      val issue     = BaboonIssue.Parser(ParserIssue.ParserFailed(failure, path))
      val converter = new PositionConverter(JvmPathOps)
      val provider  = new DiagnosticsProvider(converter)
      val uri       = converter.pathToUri(path.asString)
      val result    = CompilationResult(None, Seq(issue), Map(uri -> Seq(issue)))
      val diag      = provider.getDiagnostics(uri, result).head

      val Array(line, column, _*) = failure.extra.input.prettyIndex(failure.index).split(":")
      diag.range.start.line shouldBe line.toInt - 1
      diag.range.start.character shouldBe column.toInt - 1
    }
  }
}
