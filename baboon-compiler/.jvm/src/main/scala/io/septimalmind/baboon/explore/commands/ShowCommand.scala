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
            ctx.findType(typeName) match {
              case None =>
                Left(s"Type not found: $typeName")
              case Some(member) =>
                val renderer = new TypeRenderer(dom)
                Right(renderer.render(member))
            }
        }
    }
  }

  def complete(args: Seq[String], ctx: ExploreContext[EitherF]): Seq[String] = {
    val partial = args.lastOption.getOrElse("")
    ctx.allTypeIds
      .map(_.name.name)
      .filter(_.toLowerCase.contains(partial.toLowerCase))
  }
}
