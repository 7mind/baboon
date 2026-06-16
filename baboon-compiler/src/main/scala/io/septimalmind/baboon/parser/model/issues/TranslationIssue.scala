package io.septimalmind.baboon.parser.model.issues

import io.septimalmind.baboon.translator.OutputFile
import izumi.fundamentals.platform.exceptions.Issue
import izumi.fundamentals.platform.exceptions.Issue.IssueContext
import izumi.fundamentals.platform.strings.IzString.toRichIterable

import scala.language.implicitConversions

sealed trait TranslationIssue extends IssueGroup

object TranslationIssue {
  implicit def wrap(issue: TranslationIssue): BaboonIssue = BaboonIssue.Translation(issue)

  case class NonUniqueOutputFiles(c: Map[String, List[OutputFile]]) extends TranslationIssue

  case class ScalaMcpRequiresEither(resultType: String) extends TranslationIssue

  case class TranslationBug()(implicit val context: IssueContext) extends TranslationIssue with BaboonBug with Issue

  implicit val nonUniqueOutputFilesPrinter: IssuePrinter[NonUniqueOutputFiles] =
    (issue: NonUniqueOutputFiles) => {
      s"""Non unique output files:${issue.c.niceList()}
         |""".stripMargin
    }

  implicit val scalaMcpRequiresEitherPrinter: IssuePrinter[ScalaMcpRequiresEither] =
    (issue: ScalaMcpRequiresEither) => {
      s"""Scala MCP server generation (--scala-generate-mcp-server=true) requires the Either-mode serviceResult,
         |but the configured serviceResult is `${issue.resultType}`.
         |The Scala MCP dispatch runtime (AbstractBaboonMcpServer.handle) is synchronous and Either-shaped:
         |it matches Right/Left on the wiring's `invokeJson` result to map MCP Channel-A / Channel-B.
         |A non-Either (HKT / custom) serviceResult is not supported on this dispatch surface.
         |Resolution: either drop --scala-generate-mcp-server, or set the serviceResult to Either
         |(--service-result-type=Either, no --service-result-hkt, --service-result-no-errors=false).
         |""".stripMargin
    }

  implicit val bugPrinter: IssuePrinter[TranslationBug] =
    (issue: TranslationBug) => {
      import izumi.fundamentals.platform.exceptions.IzThrowable.*
      s"""Tranlation BUG at ${issue.context.sourceFilePosition}, ${issue.context.stackTrace.shortTrace}""".stripMargin
    }

}
