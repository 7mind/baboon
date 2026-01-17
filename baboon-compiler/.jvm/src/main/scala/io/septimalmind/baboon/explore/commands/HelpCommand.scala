package io.septimalmind.baboon.explore.commands

import io.septimalmind.baboon.explore.{Colors, EitherF, ExploreContext}

object HelpCommand extends Command {
  def name: String = "help"
  def help: String = "Show this help message"

  def execute(args: Seq[String], ctx: ExploreContext[EitherF]): Either[String, String] = {
    val sb = new StringBuilder
    sb.append(s"${Colors.CYAN}Available commands:${Colors.RESET}\n\n")

    val commands = Seq(
      ("domains", "List all available domain packages"),
      ("versions", "List versions in current domain"),
      ("switch <domain> [version]", "Switch to domain (and optionally version)"),
      ("types [-a] [-r] [filter]", "List types. -a=all domains, -r=regex filter"),
      ("show <type>", "Print type structure in baboon syntax"),
      ("example <type>", "Generate random instance (JSON + UEBA hex)"),
      ("decode <type> <ueba-hex>", "Decode UEBA hex to JSON with offset map"),
      ("deps <type>", "Show types that depend on this type"),
      ("depsof <type>", "Show types this type depends on"),
      ("deptree <type>", "Show dependency tree"),
      ("evo [-v] <type>", "Show evolution history. -v=verbose"),
      ("reload", "Reload models from disk"),
      ("help", "Show this help message"),
      ("exit / quit", "Exit the explorer"),
    )

    commands.foreach { case (cmd, desc) =>
      sb.append(s"  ${Colors.GREEN}$cmd${Colors.RESET}\n")
      sb.append(s"      $desc\n")
    }

    sb.append(s"\n${Colors.DIM}Tab completion is available for commands and type names.${Colors.RESET}")

    Right(sb.toString())
  }

  def complete(args: Seq[String], ctx: ExploreContext[EitherF]): Seq[String] = Seq.empty
}
