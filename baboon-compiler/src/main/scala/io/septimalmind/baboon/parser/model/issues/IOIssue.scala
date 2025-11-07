package io.septimalmind.baboon.parser.model.issues

import izumi.fundamentals.platform.exceptions.IzThrowable.*
import izumi.fundamentals.platform.strings.IzString.toRichIterable

sealed trait IOIssue extends IssueGroup

object IOIssue {
  implicit def wrap(issue: IOIssue): BaboonIssue = BaboonIssue.IO(issue)

  case class CantReadInput(path: String, throwable: Throwable) extends IOIssue

  case class CantWriteOutput(path: String, throwable: Throwable) extends IOIssue

  case class CantCleanupTarget(paths: Seq[String], safeToRemoveExtensions: Seq[String], error: Option[Throwable]) extends IOIssue

  implicit val cantReadInputPrinter: IssuePrinter[CantReadInput] =
    (issue: CantReadInput) => {
      s"""Can't read form file: ${issue.path}
         |Due to: ${issue.throwable.stacktraceString}
         |""".stripMargin
    }

  implicit val cantWriteOutputPrinter: IssuePrinter[CantWriteOutput] =
    (issue: CantWriteOutput) => {
      s"""Can't write to file: ${issue.path}
         |Due to: ${issue.throwable.stacktraceString}
         |""".stripMargin
    }

  implicit val cantCleanupTargetPrinter: IssuePrinter[CantCleanupTarget] =
    (issue: CantCleanupTarget) => {
      s"""Refusing to remove target directory, there are unexpected files: ${issue.paths.niceList()}
         |Extensions allowed for removal: ${issue.safeToRemoveExtensions.mkString(", ")}
         |""".stripMargin
    }
}
