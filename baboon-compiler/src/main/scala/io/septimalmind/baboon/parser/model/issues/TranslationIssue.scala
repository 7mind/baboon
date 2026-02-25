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

  case class TranslationBug()(implicit val context: IssueContext) extends TranslationIssue with BaboonBug with Issue

  implicit val nonUniqueOutputFilesPrinter: IssuePrinter[NonUniqueOutputFiles] =
    (issue: NonUniqueOutputFiles) => {
      s"""Non unique output files:${issue.c.niceList()}
         |""".stripMargin
    }

  implicit val bugPrinter: IssuePrinter[TranslationBug] =
    (issue: TranslationBug) => {
      import izumi.fundamentals.platform.exceptions.IzThrowable.*
      s"""Tranlation BUG at ${issue.context.sourceFilePosition}, ${issue.context.stackTrace.shortTrace}""".stripMargin
    }

}
