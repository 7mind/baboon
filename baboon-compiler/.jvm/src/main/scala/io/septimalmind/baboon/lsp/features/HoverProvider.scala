package io.septimalmind.baboon.lsp.features

import io.septimalmind.baboon.lsp.state.{DocumentState, WorkspaceState}
import io.septimalmind.baboon.parser.model.RawMemberMeta
import io.septimalmind.baboon.typer.model._
import org.eclipse.lsp4j._

class HoverProvider(
  documentState: DocumentState,
  workspaceState: WorkspaceState
) {

  def getHover(uri: String, position: Position): Option[Hover] = {
    val wordAtCursor = getWordAtPosition(uri, position)

    wordAtCursor.flatMap { typeName =>
      findTypeInfo(typeName).map { info =>
        val content = new MarkupContent(MarkupKind.MARKDOWN, info)
        new Hover(content)
      }
    }
  }

  private def getWordAtPosition(uri: String, position: Position): Option[String] = {
    documentState.getContent(uri).flatMap { content =>
      val lines = content.split("\n", -1)
      if (position.getLine < lines.length) {
        val line = lines(position.getLine)
        val col  = position.getCharacter
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
    workspaceState.getFamily.flatMap { family =>
      family.domains.toMap.values.flatMap { lineage =>
        lineage.versions.toMap.values.flatMap { domain =>
          domain.defs.meta.nodes.values.collectFirst {
            case u: DomainMember.User if u.id.name.name == typeName =>
              renderTypeInfo(u, domain)
          }
        }
      }.headOption
    }
  }

  private def renderTypeInfo(member: DomainMember.User, domain: Domain): String = {
    val sb = new StringBuilder

    // Type kind and name
    val kind = member.defn match {
      case _: Typedef.Dto      => "data"
      case _: Typedef.Adt      => "adt"
      case _: Typedef.Enum     => "enum"
      case _: Typedef.Foreign  => "foreign"
      case _: Typedef.Contract => "mixin"
      case _: Typedef.Service  => "service"
    }

    sb.append(s"```baboon\n$kind ${member.id.name.name}\n```\n\n")

    // Annotations
    if (member.root) sb.append("**@root**\n\n")
    member.derivations.foreach {
      case RawMemberMeta.Derived(id) => sb.append(s"`: derived[$id]`\n")
      case _                         =>
    }

    // Type-specific info
    member.defn match {
      case dto: Typedef.Dto =>
        if (dto.fields.nonEmpty) {
          sb.append("\n**Fields:**\n")
          dto.fields.foreach { f =>
            sb.append(s"- `${f.name.name}`: ${renderTypeRef(f.tpe)}\n")
          }
        }
        if (dto.contracts.nonEmpty) {
          sb.append(s"\n**Implements:** ${dto.contracts.map(_.name.name).mkString(", ")}\n")
        }

      case adt: Typedef.Adt =>
        sb.append(s"\n**Branches:** ${adt.members.size}\n")
        adt.members.toList.foreach { branchId =>
          sb.append(s"- `${branchId.name.name}`\n")
        }

      case enum: Typedef.Enum =>
        sb.append("\n**Values:**\n")
        enum.members.toList.foreach { m =>
          val constPart = m.const.map(c => s" = $c").getOrElse("")
          sb.append(s"- `${m.name}$constPart`\n")
        }

      case foreign: Typedef.Foreign =>
        sb.append("\n**Bindings:**\n")
        foreign.bindings.foreach { case (lang, entry) =>
          sb.append(s"- $lang: `${entry.decl}`\n")
        }

      case contract: Typedef.Contract =>
        if (contract.fields.nonEmpty) {
          sb.append("\n**Required fields:**\n")
          contract.fields.foreach { f =>
            sb.append(s"- `${f.name.name}`: ${renderTypeRef(f.tpe)}\n")
          }
        }

      case service: Typedef.Service =>
        sb.append("\n**Methods:**\n")
        service.methods.foreach { m =>
          val outPart = m.out.map(o => s" -> ${renderTypeRef(o)}").getOrElse("")
          val errPart = m.err.map(e => s" !> ${renderTypeRef(e)}").getOrElse("")
          sb.append(s"- `${m.name.name}(${renderTypeRef(m.sig)})$outPart$errPart`\n")
        }
    }

    // Package info
    sb.append(s"\n---\n*Package: ${member.id.pkg}*")

    sb.toString()
  }

  private def renderTypeRef(ref: TypeRef): String = ref match {
    case TypeRef.Scalar(id) => id.name.name
    case TypeRef.Constructor(id, args) =>
      s"${id.name.name}[${args.toList.map(renderTypeRef).mkString(", ")}]"
  }
}
