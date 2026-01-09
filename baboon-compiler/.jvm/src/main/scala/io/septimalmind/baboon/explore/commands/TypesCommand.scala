package io.septimalmind.baboon.explore.commands

import io.septimalmind.baboon.explore.{Colors, ExploreContext, TypeRenderer}
import io.septimalmind.baboon.typer.model.DomainMember

import scala.util.matching.Regex

object TypesCommand extends Command {
  def name: String = "types"
  def help: String = "types [-a] [-r] [filter] - List types. -a=all domains/versions, -r=regex filter"

  def execute(args: Seq[String], ctx: ExploreContext): Either[String, String] = {
    val searchAll = args.contains("-a")
    val useRegex = args.contains("-r")
    val filterArgs = args.filterNot(_.startsWith("-"))
    val filter = filterArgs.headOption

    val regexFilter: Option[Regex] = if (useRegex) {
      filter.flatMap { f =>
        scala.util.Try(f.r).toOption
      }
    } else None

    if (useRegex && filter.isDefined && regexFilter.isEmpty) {
      return Left(s"Invalid regex pattern: ${filter.get}")
    }

    val sb = new StringBuilder

    if (searchAll) {
      ctx.family.domains.toMap.foreach { case (pkg, lineage) =>
        lineage.versions.toMap.foreach { case (ver, dom) =>
          val renderer = new TypeRenderer(dom)
          val types = collectTypes(dom.defs.meta.nodes.values.toSeq, filter, regexFilter, useRegex)
          if (types.nonEmpty) {
            sb.append(s"${Colors.CYAN}${pkg.path.mkString(".")} v:$ver${Colors.RESET}\n")
            types.foreach { u =>
              sb.append(s"  ${renderer.renderTypeName(u)}\n")
            }
            sb.append("\n")
          }
        }
      }
    } else {
      ctx.currentDomain match {
        case None =>
          return Left("No domain selected. Use 'switch <domain>' first, or use -a to search all.")
        case Some(dom) =>
          val renderer = new TypeRenderer(dom)
          val types = collectTypes(dom.defs.meta.nodes.values.toSeq, filter, regexFilter, useRegex)
          if (types.isEmpty) {
            sb.append(s"${Colors.DIM}No types found${filter.map(f => s" matching '$f'").getOrElse("")}${Colors.RESET}")
          } else {
            sb.append(s"${Colors.CYAN}Types${filter.map(f => s" matching '$f'").getOrElse("")}:${Colors.RESET}\n")
            types.foreach { u =>
              sb.append(s"  ${renderer.renderTypeName(u)}\n")
            }
          }
      }
    }

    Right(sb.toString().stripSuffix("\n"))
  }

  private def collectTypes(
    members: Seq[DomainMember],
    filter: Option[String],
    regexFilter: Option[Regex],
    useRegex: Boolean
  ): Seq[DomainMember.User] = {
    members.collect {
      case u: DomainMember.User => u
    }.filter { u =>
      val name = u.id.name.name
      (filter, regexFilter) match {
        case (_, Some(regex)) => regex.findFirstIn(name).isDefined
        case (Some(f), _) if !useRegex => name.toLowerCase.contains(f.toLowerCase)
        case _ => true
      }
    }.sortBy(_.id.name.name)
  }

  def complete(args: Seq[String], ctx: ExploreContext): Seq[String] = {
    val nonFlags = args.filterNot(_.startsWith("-"))
    val lastArg = nonFlags.lastOption.getOrElse("")

    if (lastArg.startsWith("-") || nonFlags.isEmpty) {
      Seq("-a", "-r").filter(_.startsWith(args.lastOption.getOrElse("")))
    } else {
      Seq.empty
    }
  }
}
