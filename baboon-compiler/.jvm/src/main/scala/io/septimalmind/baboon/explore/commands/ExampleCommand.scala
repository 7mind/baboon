package io.septimalmind.baboon.explore.commands

import io.septimalmind.baboon.explore.{Colors, EitherF, ExploreContext, RandomJsonGenerator, UebaDecodeRenderer}

object ExampleCommand extends Command {
  def name: String = "example"
  def help: String = "example <type> - Generate random instance (JSON + UEBA hex)"

  def execute(args: Seq[String], ctx: ExploreContext[EitherF]): Either[String, String] = {
    args.headOption match {
      case None =>
        Left("Usage: example <type>")

      case Some(typeName) =>
        ctx.currentDomain match {
          case None =>
            Left("No domain selected. Use 'switch <domain>' first.")
          case Some(dom) =>
            ctx.findType(typeName) match {
              case None =>
                Left(s"Type not found: $typeName")
              case Some(member) =>
                val generator = new RandomJsonGenerator(dom, ctx.enquiries)
                generator.generate(member) match {
                  case Left(err) => Left(err)
                  case Right(json) =>
                    val sb = new StringBuilder

                    sb.append(s"${Colors.CYAN}JSON:${Colors.RESET}\n")
                    sb.append(json.spaces2)
                    sb.append("\n")

                    val pkg     = ctx.currentPkg.get
                    val version = ctx.currentVersion.get

                    val renderer = new UebaDecodeRenderer(dom, ctx.enquiries)

                    ctx.encode(pkg, version, member.id.toString, json, indexed = false) match {
                      case Right(bytes) =>
                        sb.append(s"\n${Colors.CYAN}UEBA (compact, ${bytes.length} bytes):${Colors.RESET}\n")
                        sb.append(renderer.renderOffsets(bytes, json, member))
                      case Left(err) =>
                        sb.append(s"\n${Colors.YELLOW}UEBA encoding failed: $err${Colors.RESET}")
                    }

                    ctx.encode(pkg, version, member.id.toString, json, indexed = true) match {
                      case Right(bytes) =>
                        sb.append(s"\n\n${Colors.CYAN}UEBA (indexed, ${bytes.length} bytes):${Colors.RESET}\n")
                        sb.append(renderer.renderOffsets(bytes, json, member))
                      case Left(_) =>
                    }

                    Right(sb.toString())
                }
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
