package io.septimalmind.baboon.explore.commands

import io.septimalmind.baboon.explore.{EitherF, ExploreContext}
import io.septimalmind.baboon.parser.model.issues.IssuePrinter.IssuePrinterListOps

object ReloadCommand extends Command {
  def name: String = "reload"
  def help: String = "Reload models from disk"

  def execute(args: Seq[String], ctx: ExploreContext[EitherF]): Either[String, String] = {
    if (args.nonEmpty) {
      Left("reload does not accept arguments")
    } else {
      ctx.reload match {
        case Right(family) =>
          Right(s"Reloaded ${family.domains.size} domain(s)")
        case Left(issues) =>
          Left(issues.toList.stringifyIssues)
      }
    }
  }

  def complete(args: Seq[String], ctx: ExploreContext[EitherF]): Seq[String] = Seq.empty
}
