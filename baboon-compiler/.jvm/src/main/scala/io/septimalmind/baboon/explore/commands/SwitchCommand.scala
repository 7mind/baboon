package io.septimalmind.baboon.explore.commands

import io.septimalmind.baboon.explore.{Colors, ExploreContext}

object SwitchCommand extends Command {
  def name: String = "switch"
  def help: String = "switch <domain> [version] - Switch to domain (and optionally version)"

  def execute(args: Seq[String], ctx: ExploreContext): Either[String, String] = {
    args match {
      case Seq() =>
        Left("Usage: switch <domain> [version]")

      case Seq(pkgStr) =>
        ctx.parsePkg(pkgStr) match {
          case None => Left(s"Invalid package format: $pkgStr")
          case Some(pkg) =>
            ctx.switchTo(pkg, None) match {
              case Right(_) =>
                Right(s"${Colors.GREEN}Switched to ${pkgStr} v:${ctx.currentVersion.get}${Colors.RESET}")
              case Left(err) => Left(err)
            }
        }

      case Seq(pkgStr, verStr) =>
        ctx.parsePkg(pkgStr) match {
          case None => Left(s"Invalid package format: $pkgStr")
          case Some(pkg) =>
            ctx.parseVersion(verStr) match {
              case None => Left(s"Invalid version format: $verStr")
              case Some(ver) =>
                ctx.switchTo(pkg, Some(ver)) match {
                  case Right(_) =>
                    Right(s"${Colors.GREEN}Switched to $pkgStr v:$ver${Colors.RESET}")
                  case Left(err) => Left(err)
                }
            }
        }

      case _ =>
        Left("Usage: switch <domain> [version]")
    }
  }

  def complete(args: Seq[String], ctx: ExploreContext): Seq[String] = {
    args match {
      case Seq(partial) =>
        ctx.allDomains
          .map(_.path.mkString("."))
          .filter(_.toLowerCase.contains(partial.toLowerCase))

      case Seq(pkgStr, partial) =>
        ctx.parsePkg(pkgStr).flatMap(pkg => ctx.family.domains.toMap.get(pkg)) match {
          case Some(lineage) =>
            lineage.versions.toMap.keys
              .map(_.toString)
              .filter(_.startsWith(partial))
              .toSeq
          case None => Seq.empty
        }

      case Seq(pkgStr) if args.isEmpty || pkgStr.isEmpty =>
        ctx.allDomains.map(_.path.mkString("."))

      case _ => Seq.empty
    }
  }
}
