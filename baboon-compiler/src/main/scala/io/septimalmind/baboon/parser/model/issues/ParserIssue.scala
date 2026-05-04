package io.septimalmind.baboon.parser.model.issues

import fastparse.Parsed
import io.septimalmind.baboon.parser.model.*

sealed trait ParserIssue extends IssueGroup

object ParserIssue {
  implicit def wrap(issue: ParserIssue): BaboonIssue = BaboonIssue.Parser(issue)

  case class ParserFailed(error: Parsed.Failure, path: FSPath) extends ParserIssue

  case class IncludeNotFound(path: String) extends ParserIssue

  // Two prefix doc blocks appear back-to-back with no intervening
  // declaration. Per `docs/spec/docstrings.md` §4 (Q3 lock) stacked
  // prefix docs are a parse error; `pos` cites the position of the
  // SECOND block.
  case class StackedDocComments(pos: InputPointer) extends ParserIssue

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

  implicit val stackedDocCommentsPrinter: IssuePrinter[StackedDocComments] =
    (issue: StackedDocComments) => {
      val locStr = issue.pos match {
        case s: InputPointer.StartOffsetKnown => s"${s.file.asString} @ ${s.start.line}:${s.start.column}"
        case f: InputPointer.JustFile         => f.file.asString
        case InputPointer.Undefined           => "<unknown>"
      }
      s"Stacked prefix doc comments at $locStr — merge into a single doc block per declaration."
    }

}
