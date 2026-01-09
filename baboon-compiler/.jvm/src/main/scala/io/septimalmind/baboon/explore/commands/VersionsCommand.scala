package io.septimalmind.baboon.explore.commands

import io.septimalmind.baboon.explore.{Colors, ExploreContext}

object VersionsCommand extends Command {
  def name: String = "versions"
  def help: String = "List versions in current domain"

  def execute(args: Seq[String], ctx: ExploreContext): Either[String, String] = {
    ctx.currentLineage match {
      case None =>
        Left("No domain selected. Use 'switch <domain>' first.")
      case Some(lineage) =>
        val sb = new StringBuilder
        val latest = lineage.evolution.latest
        val versions = lineage.versions.toMap.keys.toSeq.sorted

        sb.append(s"${Colors.CYAN}Versions in ${lineage.pkg.path.mkString(".")}:${Colors.RESET}\n")
        versions.foreach { v =>
          val marker = if (ctx.currentVersion.contains(v)) s"${Colors.GREEN}*${Colors.RESET} " else "  "
          val latestMarker = if (v == latest) s" ${Colors.YELLOW}(latest)${Colors.RESET}" else ""
          sb.append(s"$marker$v$latestMarker\n")
        }

        Right(sb.toString().stripSuffix("\n"))
    }
  }

  def complete(args: Seq[String], ctx: ExploreContext): Seq[String] = Seq.empty
}
