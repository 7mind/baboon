package io.septimalmind.baboon.parser.model.issues

import fastparse.Parsed
import io.septimalmind.baboon.parser.model.*

sealed trait ParserIssue extends IssueGroup

object ParserIssue {
  implicit def wrap(issue: ParserIssue): BaboonIssue = BaboonIssue.Parser(issue)

  case class ParserFailed(error: Parsed.Failure, path: FSPath) extends ParserIssue

  case class IncludeNotFound(path: String) extends ParserIssue

  implicit val parserFailedPrinter: IssuePrinter[ParserFailed] =
    (issue: ParserFailed) => {
      val Array(line, character, _*) =
        issue.error.extra.input.prettyIndex(issue.error.index).split(":")
      s"Parser error occurred in ${issue.path} @ line:$line position:$character".stripMargin
    }

  implicit val includeNotFoundPrinter: IssuePrinter[IncludeNotFound] =
    (issue: IncludeNotFound) => {
      s"Failed to find inclusion `${issue.path}``".stripMargin
    }

}
