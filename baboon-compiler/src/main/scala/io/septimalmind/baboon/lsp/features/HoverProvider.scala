package io.septimalmind.baboon.lsp.features

import io.septimalmind.baboon.lsp.LspLogging
import io.septimalmind.baboon.lsp.protocol.{Hover, MarkupContent, MarkupKind, Position}
import io.septimalmind.baboon.lsp.state.{DocumentState, WorkspaceState}
import io.septimalmind.baboon.parser.model.RawMemberMeta
import io.septimalmind.baboon.typer.model._
import io.septimalmind.baboon.util.BLogger

class HoverProvider(
  documentState: DocumentState,
  workspaceState: WorkspaceState,
  logger: BLogger,
) {

  def getHover(uri: String, position: Position): Option[Hover] = {
    logger.message(LspLogging.Context, s"getHover: uri=$uri, position=$position")

    val hasContent = documentState.getContent(uri).isDefined
    logger.message(LspLogging.Context, s"getHover: hasContent=$hasContent")

    val wordAtCursor = getWordAtPosition(uri, position)
    logger.message(LspLogging.Context, s"getHover: wordAtCursor=$wordAtCursor")

    val hasFamily = workspaceState.getFamily.isDefined
    logger.message(LspLogging.Context, s"getHover: hasFamily=$hasFamily")

    wordAtCursor.flatMap {
      typeName =>
        findTypeInfo(typeName).map {
          info =>
            logger.message(LspLogging.Context, s"getHover: found type info for '$typeName'")
            val content = MarkupContent(MarkupKind.Markdown, info)
            Hover(content)
        }
    }
  }

  private def getWordAtPosition(uri: String, position: Position): Option[String] = {
    documentState.getContent(uri).flatMap {
      content =>
        val lines = content.split("\n", -1)
        if (position.line < lines.length) {
          val line = lines(position.line)
          val col  = position.character
          if (col <= line.length) {
            val wordChars = (c: Char) => c.isLetterOrDigit || c == '_'
            var start     = col
            while (start > 0 && wordChars(line.charAt(start - 1))) start -= 1
            var end = col
            while (end < line.length && wordChars(line.charAt(end))) end += 1
            if (start < end) Some(line.substring(start, end)) else None
          } else None
        } else None
    }
  }

  private def findTypeInfo(typeName: String): Option[String] = {
    workspaceState.getFamily.flatMap {
      family =>
        family.domains.toMap.values.flatMap {
          lineage =>
            lineage.versions.toMap.values.flatMap {
              domain =>
                domain.defs.meta.nodes.values.collectFirst {
                  case u: DomainMember.User if u.id.name.name == typeName =>
                    renderTypeInfo(u, domain)
                }
            }
        }.headOption
    }
  }

  private def renderTypeInfo(member: DomainMember.User, @annotation.unused domain: Domain): String = {
    val sb = new StringBuilder

    val kind = member.defn match {
      case _: Typedef.Dto      => "data"
      case _: Typedef.Adt      => "adt"
      case _: Typedef.Enum     => "enum"
      case _: Typedef.Foreign  => "foreign"
      case _: Typedef.Contract => "mixin"
      case _: Typedef.Service  => "service"
    }

    sb.append(s"```baboon\n$kind ${member.id.name.name}\n```\n\n")

    if (member.root) sb.append("**@root**\n\n")
    member.derivations.foreach {
      case RawMemberMeta.Derived(id) => sb.append(s"`: derived[$id]`\n")
      case _                         =>
    }

    member.defn match {
      case dto: Typedef.Dto =>
        if (dto.fields.nonEmpty) {
          sb.append("\n**Fields:**\n")
          dto.fields.foreach {
            f =>
              sb.append(s"- `${f.name.name}`: ${renderTypeRef(f.tpe)}\n")
          }
        }
        if (dto.contracts.nonEmpty) {
          sb.append(s"\n**Implements:** ${dto.contracts.map(_.name.name).mkString(", ")}\n")
        }

      case adt: Typedef.Adt =>
        sb.append(s"\n**Branches:** ${adt.members.size}\n")
        adt.members.toList.foreach {
          branchId =>
            sb.append(s"- `${branchId.name.name}`\n")
        }

      case enum: Typedef.Enum =>
        sb.append("\n**Values:**\n")
        enum.members.toList.foreach {
          m =>
            val constPart = m.const.map(c => s" = $c").getOrElse("")
            sb.append(s"- `${m.name}$constPart`\n")
        }

      case foreign: Typedef.Foreign =>
        sb.append("\n**Bindings:**\n")
        foreign.bindings.foreach {
          case (lang, entry) =>
            entry.mapping match {
              case Typedef.ForeignMapping.Custom(decl, _) =>
                sb.append(s"- ${lang.asString}: `$decl`\n")
              case Typedef.ForeignMapping.BaboonRef(typeRef) =>
                sb.append(s"- ${lang.asString}: $typeRef (baboon alias)\n")
            }
        }

      case contract: Typedef.Contract =>
        if (contract.fields.nonEmpty) {
          sb.append("\n**Required fields:**\n")
          contract.fields.foreach {
            f =>
              sb.append(s"- `${f.name.name}`: ${renderTypeRef(f.tpe)}\n")
          }
        }

      case service: Typedef.Service =>
        sb.append("\n**Methods:**\n")
        service.methods.foreach {
          m =>
            val outPart = m.out.map(o => s" -> ${renderTypeRef(o)}").getOrElse("")
            val errPart = m.err.map(e => s" !> ${renderTypeRef(e)}").getOrElse("")
            sb.append(s"- `${m.name.name}(${renderTypeRef(m.sig)})$outPart$errPart`\n")
        }
    }

    sb.append(s"\n---\n*Package: ${member.id.pkg}*")

    sb.toString()
  }

  private def renderTypeRef(ref: TypeRef): String = ref match {
    case TypeRef.Scalar(id) => id.name.name
    case TypeRef.Constructor(id, args) =>
      s"${id.name.name}[${args.toList.map(renderTypeRef).mkString(", ")}]"
  }
}
