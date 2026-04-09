package io.septimalmind.baboon.explore.commands

import io.septimalmind.baboon.explore.{EitherF, ExploreContext, TypeRenderer}

object ShowCommand extends Command {
  def name: String = "show"
  def help: String = "show <type> - Print type structure in baboon syntax"

  def execute(args: Seq[String], ctx: ExploreContext[EitherF]): Either[String, String] = {
    args.headOption match {
      case None =>
        Left("Usage: show <type>")

      case Some(typeName) =>
        ctx.currentDomain match {
          case None =>
            Left("No domain selected. Use 'switch <domain>' first.")
          case Some(dom) =>
            val renderer = new TypeRenderer(dom)
            ctx.findType(typeName) match {
              case Some(member) =>
                Right(renderer.render(member))
              case None =>
                ctx.findAlias(typeName) match {
                  case Some(alias) =>
                    Right(renderer.renderAlias(alias))
                  case None =>
                    Left(s"Type not found: $typeName")
                }
            }
        }
    }
  }

  def complete(args: Seq[String], ctx: ExploreContext[EitherF]): Seq[String] = {
    val partial = args.lastOption.getOrElse("")
    val typeNames = ctx.allTypeIds.map(_.name.name)
    val aliasNames = ctx.allAliases.map(_.name.name)
    (typeNames ++ aliasNames)
      .filter(_.toLowerCase.contains(partial.toLowerCase))
      .sorted
  }
}
