package io.septimalmind.baboon.explore.commands

import io.septimalmind.baboon.explore.{Colors, EitherF, ExploreContext}
import io.septimalmind.baboon.typer.model.{DomainMember, TypeId}

object DepsOfCommand extends Command {
  def name: String = "depsof"
  def help: String = "depsof <type> - Show types this type depends on"

  def execute(args: Seq[String], ctx: ExploreContext[EitherF]): Either[String, String] = {
    args.headOption match {
      case None =>
        Left("Usage: depsof <type>")

      case Some(typeName) =>
        ctx.currentDomain match {
          case None =>
            Left("No domain selected. Use 'switch <domain>' first.")
          case Some(dom) =>
            ctx.findType(typeName) match {
              case None =>
                Left(s"Type not found: $typeName")
              case Some(member) =>
                val deps = ctx.enquiries.fullDepsOfDefn(member)
                val userDeps = deps.collect {
                  case u: TypeId.User => u
                }.toSeq.sortBy(_.name.name)

                if (userDeps.isEmpty) {
                  Right(s"${Colors.DIM}${Colors.GREEN}$typeName${Colors.DIM} has no dependencies${Colors.RESET}")
                } else {
                  val sb = new StringBuilder
                  sb.append(s"${Colors.CYAN}${Colors.GREEN}$typeName${Colors.CYAN} depends on:${Colors.RESET}\n")
                  userDeps.foreach {
                    id =>
                      val kind = dom.defs.meta.nodes.get(id) match {
                        case Some(u: DomainMember.User) => kindOf(u)
                        case _                          => ""
                      }
                      sb.append(s"  ${Colors.BLUE}$kind${Colors.RESET} ${Colors.GREEN}${id.name.name}${Colors.RESET}\n")
                  }
                  Right(sb.toString().stripSuffix("\n"))
                }
            }
        }
    }
  }

  private def kindOf(member: DomainMember.User): String = {
    import io.septimalmind.baboon.typer.model.Typedef.*
    member.defn match {
      case _: Dto      => "data"
      case _: Adt      => "adt"
      case _: Enum     => "enum"
      case _: Foreign  => "foreign"
      case _: Contract => "mixin"
      case _: Service  => "service"
    }
  }

  def complete(args: Seq[String], ctx: ExploreContext[EitherF]): Seq[String] = {
    val partial = args.lastOption.getOrElse("")
    ctx.allTypeIds
      .map(_.name.name)
      .filter(_.toLowerCase.contains(partial.toLowerCase))
  }
}
