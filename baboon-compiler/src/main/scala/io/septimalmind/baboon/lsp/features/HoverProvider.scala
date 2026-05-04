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
        findTypeInfo(uri, position, typeName).map {
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

  // Spec §2.3: a type-parameter name shadows any top-level type with the same name inside the
  // template body.  We detect this by scanning the document text backward from the cursor to find
  // an enclosing `data/adt/contract/service Name[…]` declaration header.  If found, the cursor is
  // considered to be inside that template's body and the type-param lookup takes priority.
  private def enclosingTemplateName(uri: String, position: Position): Option[String] = {
    documentState.getContent(uri).flatMap {
      content =>
        val lines = content.split("\n", -1)
        // Regex that matches a template declaration header on a single line.
        val templateHeaderRe = """^\s*(?:root\s+)?(?:data|adt|contract|service)\s+(\w+)\s*\[""".r
        // Scan from the cursor line upward; stop as soon as we find a template header.
        // We do not track brace depth — a simple backward scan is sufficient for the typical
        // single-level nesting of baboon type bodies.
        var lineIdx = position.line
        while (lineIdx >= 0) {
          templateHeaderRe.findFirstMatchIn(lines(lineIdx)) match {
            case Some(m) => return Some(m.group(1))
            case None    => lineIdx -= 1
          }
        }
        None
    }
  }

  private def findTypeInfo(uri: String, position: Position, typeName: String): Option[String] = {
    workspaceState.getFamily.flatMap {
      family =>
        // §2.3 shadowing: if the cursor sits inside a template body whose type-param list contains
        // `typeName`, resolve to the type-param entry before checking top-level definitions.
        val enclosingTemplate = enclosingTemplateName(uri, position)
        val typeParamInfo: Option[String] = enclosingTemplate.flatMap {
          tplName =>
            family.domains.toMap.values.flatMap {
              lineage =>
                lineage.versions.toMap.values.flatMap {
                  domain =>
                    domain.templateRegistry.templates.collectFirst {
                      case ((_, _, name), body) if name.name == tplName && body.typeParams.exists(_.name == typeName) =>
                        s"*Type parameter of template `${name.name}`*"
                    }
                }
            }.headOption
        }

        typeParamInfo.orElse {
          val fromMembers = family.domains.toMap.values.flatMap {
            lineage =>
              lineage.versions.toMap.values.flatMap {
                domain =>
                  domain.defs.meta.nodes.values.collectFirst {
                    case u: DomainMember.User if u.id.name.name == typeName =>
                      renderTypeInfo(u, domain)
                  }
              }
          }.headOption

          fromMembers.orElse {
            family.domains.toMap.values.flatMap {
              lineage =>
                lineage.versions.toMap.values.flatMap {
                  domain =>
                    domain.aliases.collectFirst {
                      case a if a.name.name == typeName =>
                        s"```baboon\ntype ${a.name.name} = ${a.targetRepr}\n```\n\n---\n*Package: ${domain.id}*"
                    }
                }
            }.headOption
          }.orElse {
            // Look up in template registry: templates are not in domain.defs (they are removed
            // after monomorphisation) but are accessible via domain.templateRegistry.
            family.domains.toMap.values.flatMap {
              lineage =>
                lineage.versions.toMap.values.flatMap {
                  domain =>
                    domain.templateRegistry.templates.collectFirst {
                      case ((_, _, name), body) if name.name == typeName =>
                        renderTemplateInfo(name.name, body, domain)
                    }
                }
            }.headOption
          }.orElse {
            // Look up type parameter names: if `typeName` matches a type-param in any template,
            // show "type parameter of template X".  This branch is reached only when the cursor
            // is NOT inside a template body (the shadowing branch above handles that case).
            family.domains.toMap.values.flatMap {
              lineage =>
                lineage.versions.toMap.values.flatMap {
                  domain =>
                    domain.templateRegistry.templates.collectFirst {
                      case ((_, _, templateName), body) if body.typeParams.exists(_.name == typeName) =>
                        s"*Type parameter of template `${templateName.name}`*"
                    }
                }
            }.headOption
          }
        }
    }
  }

  private def renderTemplateInfo(canonicalName: String, body: TemplateBody, domain: Domain): String = {
    val params = body.typeParams.map(_.name).mkString(", ")
    val kind = body.rawDefn match {
      case _: RawTemplateDefn.Dto      => "data"
      case _: RawTemplateDefn.Adt      => "adt"
      case _: RawTemplateDefn.Contract => "contract"
      case _: RawTemplateDefn.Service  => "service"
    }
    s"```baboon\n$kind $canonicalName[$params] { … }\n```\n\n*Template — instantiate via `type Alias = $canonicalName[…]`*\n\n---\n*Package: ${domain.id}*"
  }

  /** Render a single `Docs` slot as markdown text (prefix and/or suffix).
    * Returns `None` when both slots are absent.
    */
  private def renderDocs(docs: Docs): Option[String] = {
    val parts = List(
      docs.prefix.map(_.cleaned),
      docs.suffix.map(d => s"*(${d.cleaned})*"),
    ).flatten
    if (parts.isEmpty) None else Some(parts.mkString(" "))
  }

  private def renderTypeInfo(member: DomainMember.User, @annotation.unused domain: Domain): String = {
    val sb = new StringBuilder

    val kind = member.defn match {
      case _: Typedef.Dto      => "data"
      case _: Typedef.Adt      => "adt"
      case _: Typedef.Enum     => "enum"
      case _: Typedef.Foreign  => "foreign"
      case _: Typedef.Contract => "contract"
      case _: Typedef.Service  => "service"
    }

    sb.append(s"```baboon\n$kind ${member.id.name.name}\n```\n\n")

    if (member.root) sb.append("**@root**\n\n")
    member.derivations.foreach {
      case RawMemberMeta.Derived(id) => sb.append(s"`: derived[$id]`\n")
      case _                         =>
    }

    // Type-level doc comment (prefix block only; suffix cannot appear on a type declaration).
    renderDocs(member.docs).foreach(d => sb.append(s"\n$d\n"))

    member.defn match {
      case dto: Typedef.Dto =>
        if (dto.fields.nonEmpty) {
          sb.append("\n**Fields:**\n")
          dto.fields.foreach {
            f =>
              val docSuffix = renderDocs(f.docs).map(d => s" — $d").getOrElse("")
              sb.append(s"- `${f.name.name}`: ${renderTypeRef(f.tpe)}$docSuffix\n")
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
        foreign.runtimeMapping.foreach {
          rtRef =>
            sb.append(s"\n**Runtime mapping:** $rtRef\n")
        }
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
              val docSuffix = renderDocs(f.docs).map(d => s" — $d").getOrElse("")
              sb.append(s"- `${f.name.name}`: ${renderTypeRef(f.tpe)}$docSuffix\n")
          }
        }

      case service: Typedef.Service =>
        sb.append("\n**Methods:**\n")
        service.methods.foreach {
          m =>
            val outPart = m.out.map(o => s" -> ${renderTypeRef(o)}").getOrElse("")
            val errPart = m.err.map(e => s" !> ${renderTypeRef(e)}").getOrElse("")
            val docSuffix = renderDocs(m.docs).map(d => s" — $d").getOrElse("")
            sb.append(s"- `${m.name.name}(${renderTypeRef(m.sig)})$outPart$errPart`$docSuffix\n")
        }
    }

    sb.append(s"\n---\n*Package: ${member.id.pkg}*")

    sb.toString()
  }

  private def renderTypeRef(ref: TypeRef): String = ref match {
    case TypeRef.Scalar(id) => id.name.name
    case TypeRef.Constructor(id, args) =>
      s"${id.name.name}[${args.toList.map(renderTypeRef).mkString(", ")}]"
    case a: TypeRef.Any =>
      // Render the DSL form so hover works on models containing `any` fields before the per-language
      // codec milestones (M2..M10) land. No exception here — LSP hover must remain functional.
      val qualifier = a.variant match {
        case TypeRef.AnyVariant.Global  => None
        case TypeRef.AnyVariant.ThisDom => Some("domain:this")
        case TypeRef.AnyVariant.Current => Some("domain:current")
      }
      val inside = (qualifier.toList ++ a.underlying.toList.map(renderTypeRef)).mkString(", ")
      if (inside.isEmpty) "any" else s"any[$inside]"
  }
}
