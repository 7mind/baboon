package io.septimalmind.baboon.parser.model.issues

import io.septimalmind.baboon.parser.model.issues.BaboonIssue.*
import magnolia1.Magnolia
import magnolia1.*

trait IssuePrinter[T] {
  def stringify(issue: T): String
}

trait BugPrinter[T <: BaboonBug & IssueGroup] extends IssuePrinter[T] {
  final override def stringify(issue: T): String = {
    s"""BABOON BUG!!!
       |${errorMessage(issue)}
       |Report: ${IssuePrinter.issuesUrl}
       |""".stripMargin
  }

  def errorMessage(bug: T): String
}

object IssuePrinter {
  private[issues] val issuesUrl = "https://github.com/7mind/baboon/issues"

  def apply[T](
    implicit printer: IssuePrinter[T]
  ): IssuePrinter[T] = printer

  implicit val baboonIssuePrinter: IssuePrinter[BaboonIssue] = {
    case issue: IO           => apply[IO].stringify(issue)
    case issue: Parser       => apply[Parser].stringify(issue)
    case issue: Typer        => apply[Typer].stringify(issue)
    case issue: Evolution    => apply[Evolution].stringify(issue)
    case issue: Verification => apply[Verification].stringify(issue)
    case issue: Translation  => apply[Translation].stringify(issue)
  }

  implicit class IssuePrinterOps[T <: BaboonIssue: IssuePrinter](issue: T) {
    def stringify: String = apply[T].stringify(issue)
  }

  implicit class IssuePrinterListOps[T <: BaboonIssue: IssuePrinter](
    issues: Seq[T]
  ) {
    def stringifyIssues: String = {
      issues.map(_.stringify).mkString("\n")
    }
  }

  type Typeclass[T] = IssuePrinter[T]

  def split[T](ctx: SealedTrait[Typeclass, T]): Typeclass[T] =
    new IssuePrinter[T] {
      def stringify(t: T): String =
        ctx.split(t) {
          sub =>
            sub.typeclass.stringify(sub.cast(t))
        }
    }

  implicit def gen[T]: IssuePrinter[T] = macro Magnolia.gen[T]
}
