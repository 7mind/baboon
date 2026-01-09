package io.septimalmind.baboon.explore.commands

import io.septimalmind.baboon.explore.{Colors, ExploreContext}
import io.septimalmind.baboon.typer.model.{Domain, DomainMember, TypeId}

import scala.collection.mutable

object DeptreeCommand extends Command {
  def name: String = "deptree"
  def help: String = "deptree <type> - Show dependency tree"

  def execute(args: Seq[String], ctx: ExploreContext): Either[String, String] = {
    args.headOption match {
      case None =>
        Left("Usage: deptree <type>")

      case Some(typeName) =>
        ctx.currentDomain match {
          case None =>
            Left("No domain selected. Use 'switch <domain>' first.")
          case Some(dom) =>
            ctx.findType(typeName) match {
              case None =>
                Left(s"Type not found: $typeName")
              case Some(member) =>
                val sb = new StringBuilder
                sb.append(s"${Colors.CYAN}Dependency tree for ${Colors.GREEN}$typeName${Colors.CYAN}:${Colors.RESET}\n")
                printTree(dom, ctx, member.id, sb, indent = 0, visited = mutable.Set.empty)
                Right(sb.toString().stripSuffix("\n"))
            }
        }
    }
  }

  private def printTree(
    dom: Domain,
    ctx: ExploreContext,
    typeId: TypeId,
    sb: StringBuilder,
    indent: Int,
    visited: mutable.Set[TypeId]
  ): Unit = {
    val prefix = "  " * indent
    val connector = if (indent > 0) "├─ " else ""

    typeId match {
      case u: TypeId.User =>
        val kind = dom.defs.meta.nodes.get(u) match {
          case Some(m: DomainMember.User) => kindOf(m)
          case _ => ""
        }

        val isRecursive = visited.contains(typeId)
        val recursiveMarker = if (isRecursive) s" ${Colors.YELLOW}(recursive)${Colors.RESET}" else ""

        sb.append(s"$prefix$connector${Colors.BLUE}$kind${Colors.RESET} ${Colors.GREEN}${u.name.name}${Colors.RESET}$recursiveMarker\n")

        if (!isRecursive) {
          visited += typeId
          dom.defs.meta.nodes.get(u) match {
            case Some(m: DomainMember.User) =>
              val deps = ctx.enquiries.fullDepsOfDefn(m)
              val userDeps = deps.collect {
                case uid: TypeId.User => uid
              }.toSeq.sortBy(_.name.name)

              userDeps.foreach { depId =>
                printTree(dom, ctx, depId, sb, indent + 1, visited)
              }
            case _ =>
          }
        }

      case b: TypeId.Builtin =>
        sb.append(s"$prefix$connector${Colors.CYAN}${b.name.name}${Colors.RESET}\n")
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

  def complete(args: Seq[String], ctx: ExploreContext): Seq[String] = {
    val partial = args.lastOption.getOrElse("")
    ctx.allTypeIds
      .map(_.name.name)
      .filter(_.toLowerCase.contains(partial.toLowerCase))
  }
}
