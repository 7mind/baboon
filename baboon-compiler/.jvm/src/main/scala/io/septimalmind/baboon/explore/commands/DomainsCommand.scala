package io.septimalmind.baboon.explore.commands

import io.septimalmind.baboon.explore.{Colors, EitherF, ExploreContext}

object DomainsCommand extends Command {
  def name: String = "domains"
  def help: String = "List all available domain packages"

  def execute(args: Seq[String], ctx: ExploreContext[EitherF]): Either[String, String] = {
    val sb      = new StringBuilder
    val domains = ctx.allDomains

    if (domains.isEmpty) {
      sb.append(s"${Colors.DIM}No domains loaded${Colors.RESET}")
    } else {
      sb.append(s"${Colors.CYAN}Available domains:${Colors.RESET}\n")
      domains.foreach {
        pkg =>
          val pkgStr       = pkg.path.mkString(".")
          val marker       = if (ctx.currentPkg.contains(pkg)) s"${Colors.GREEN}*${Colors.RESET} " else "  "
          val versionCount = ctx.family.domains(pkg).versions.toMap.size
          sb.append(s"$marker$pkgStr ${Colors.DIM}($versionCount version(s))${Colors.RESET}\n")
      }
    }

    Right(sb.toString().stripSuffix("\n"))
  }

  def complete(args: Seq[String], ctx: ExploreContext[EitherF]): Seq[String] = Seq.empty
}
