package io.septimalmind.baboon.explore.commands

import io.septimalmind.baboon.explore.{Colors, EitherF, ExploreContext}
import io.septimalmind.baboon.typer.model.{DomainMember, TypeId}

object DepsCommand extends Command {
  def name: String = "deps"
  def help: String = "deps <type> - Show types that depend on this type"

  def execute(args: Seq[String], ctx: ExploreContext[EitherF]): Either[String, String] = {
    args.headOption match {
      case None =>
        Left("Usage: deps <type>")

      case Some(typeName) =>
        ctx.currentDomain match {
          case None =>
            Left("No domain selected. Use 'switch <domain>' first.")
          case Some(dom) =>
            ctx.findTypeId(typeName) match {
              case None =>
                Left(s"Type not found: $typeName")
              case Some(typeId) =>
                val dependents = dom.defs.successors.links.getOrElse(typeId, Set.empty)
                val userDependents = dependents.collect {
                  case u: TypeId.User => u
                }.toSeq.sortBy(_.name.name)

                if (userDependents.isEmpty) {
                  Right(s"${Colors.DIM}No types depend on ${Colors.GREEN}$typeName${Colors.DIM}${Colors.RESET}")
                } else {
                  val sb = new StringBuilder
                  sb.append(s"${Colors.CYAN}Types that depend on ${Colors.GREEN}$typeName${Colors.CYAN}:${Colors.RESET}\n")
                  userDependents.foreach { id =>
                    val kind = dom.defs.meta.nodes.get(id) match {
                      case Some(u: DomainMember.User) => kindOf(u)
                      case _ => ""
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
      case _: Dto => "data"
      case _: Adt => "adt"
      case _: Enum => "enum"
      case _: Foreign => "foreign"
      case _: Contract => "mixin"
      case _: Service => "service"
    }
  }

  def complete(args: Seq[String], ctx: ExploreContext[EitherF]): Seq[String] = {
    val partial = args.lastOption.getOrElse("")
    ctx.allTypeIds
      .map(_.name.name)
      .filter(_.toLowerCase.contains(partial.toLowerCase))
  }
}
