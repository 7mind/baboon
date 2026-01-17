package io.septimalmind.baboon.explore.commands

import io.septimalmind.baboon.explore.{Colors, EitherF, ExploreContext, UebaDecodeRenderer}

object DecodeCommand extends Command {
  def name: String = "decode"
  def help: String = "decode <type> <ueba-hex> - Decode UEBA hex to JSON with offset map (hex can be space-separated)"

  def execute(args: Seq[String], ctx: ExploreContext[EitherF]): Either[String, String] = {
    args match {
      case typeName +: hexParts if hexParts.nonEmpty =>
        ctx.currentDomain match {
          case None =>
            Left("No domain selected. Use 'switch <domain>' first.")
          case Some(dom) =>
            ctx.findType(typeName) match {
              case None =>
                Left(s"Type not found: $typeName")
              case Some(member) =>
                val hexStr = hexParts.mkString("")
                val cleanHex = hexStr.replaceAll("[^0-9A-Fa-f]", "")
                if (cleanHex.length % 2 != 0) {
                  Left("Invalid hex string: odd number of characters")
                } else {
                  val bytes = cleanHex.grouped(2).map(s => Integer.parseInt(s, 16).toByte).toVector

                  val pkg = ctx.currentPkg.get
                  val version = ctx.currentVersion.get

                  ctx.decode(pkg, version, member.id.toString, bytes) match {
                    case Right(json) =>
                      val sb = new StringBuilder

                      sb.append(s"${Colors.CYAN}Decoded JSON:${Colors.RESET}\n")
                      sb.append(json.spaces2)
                      sb.append("\n\n")

                      val renderer = new UebaDecodeRenderer(dom, ctx.enquiries)
                      sb.append(renderer.renderOffsets(bytes, json, member))

                      Right(sb.toString())

                    case Left(err) =>
                      Left(s"Decode failed: $err")
                  }
                }
            }
        }

      case _ =>
        Left("Usage: decode <type> <ueba-hex>")
    }
  }

  def complete(args: Seq[String], ctx: ExploreContext[EitherF]): Seq[String] = {
    args match {
      case Seq(partial) =>
        ctx.allTypeIds
          .map(_.name.name)
          .filter(_.toLowerCase.contains(partial.toLowerCase))
      case _ => Seq.empty
    }
  }
}
