package io.septimalmind.baboon.lsp.features

import io.septimalmind.baboon.lsp.state.{DocumentState, WorkspaceState}
import io.septimalmind.baboon.typer.model._
import org.eclipse.lsp4j._

class CompletionProvider(
  documentState: DocumentState,
  workspaceState: WorkspaceState
) {

  // Baboon keywords
  private val keywords = Seq(
    "model",
    "version",
    "import",
    "root",
    "data",
    "adt",
    "enum",
    "foreign",
    "mixin",
    "service",
    "derived",
    "was"
  )

  // Builtin types
  private val builtinTypes = Seq(
    "i08",
    "i16",
    "i32",
    "i64",
    "u08",
    "u16",
    "u32",
    "u64",
    "f32",
    "f64",
    "f128",
    "str",
    "bit",
    "uid",
    "tsu",
    "tso",
    "bytes",
    "opt",
    "lst",
    "set",
    "map"
  )

  def getCompletions(uri: String, position: Position): Seq[CompletionItem] = {
    val context = getCompletionContext(uri, position)

    context match {
      case CompletionContext.TypePosition =>
        getTypeCompletions ++ getBuiltinCompletions

      case CompletionContext.KeywordPosition =>
        getKeywordCompletions

      case CompletionContext.FieldName =>
        Seq.empty // No suggestions for field names

      case CompletionContext.Unknown =>
        getKeywordCompletions ++ getTypeCompletions ++ getBuiltinCompletions
    }
  }

  private sealed trait CompletionContext
  private object CompletionContext {
    case object TypePosition    extends CompletionContext
    case object KeywordPosition extends CompletionContext
    case object FieldName       extends CompletionContext
    case object Unknown         extends CompletionContext
  }

  private def getCompletionContext(uri: String, position: Position): CompletionContext = {
    documentState
      .getContent(uri)
      .map { content =>
        val lines = content.split("\n", -1)
        if (position.getLine < lines.length) {
          val line         = lines(position.getLine)
          val beforeCursor = line.take(position.getCharacter)

          // Simple heuristics for context detection
          if (beforeCursor.matches(".*:\\s*$")) {
            // After a colon - expecting type
            CompletionContext.TypePosition
          } else if (beforeCursor.matches(".*\\[\\s*$") || beforeCursor.matches(".*,\\s*$")) {
            // Inside brackets or after comma - expecting type
            CompletionContext.TypePosition
          } else if (beforeCursor.trim.isEmpty || beforeCursor.matches("^\\s*$")) {
            // Start of line - keywords
            CompletionContext.KeywordPosition
          } else if (beforeCursor.matches(".*\\{\\s*$")) {
            // Inside braces - could be field or type member
            CompletionContext.FieldName
          } else {
            CompletionContext.Unknown
          }
        } else {
          CompletionContext.Unknown
        }
      }
      .getOrElse(CompletionContext.Unknown)
  }

  private def getKeywordCompletions: Seq[CompletionItem] = {
    keywords.map { kw =>
      val item = new CompletionItem(kw)
      item.setKind(CompletionItemKind.Keyword)
      item.setDetail("keyword")
      item
    }
  }

  private def getBuiltinCompletions: Seq[CompletionItem] = {
    builtinTypes.map { bt =>
      val item = new CompletionItem(bt)
      item.setKind(CompletionItemKind.TypeParameter)
      item.setDetail("builtin type")
      item
    }
  }

  private def getTypeCompletions: Seq[CompletionItem] = {
    workspaceState.getFamily
      .map { family =>
        family.domains.toMap.values.flatMap { lineage =>
          lineage.versions.toMap.values.flatMap { domain =>
            domain.defs.meta.nodes.values.collect { case u: DomainMember.User =>
              val item = new CompletionItem(u.id.name.name)
              val kind = u.defn match {
                case _: Typedef.Dto      => CompletionItemKind.Class
                case _: Typedef.Adt      => CompletionItemKind.Interface
                case _: Typedef.Enum     => CompletionItemKind.Enum
                case _: Typedef.Foreign  => CompletionItemKind.Reference
                case _: Typedef.Contract => CompletionItemKind.Interface
                case _: Typedef.Service  => CompletionItemKind.Module
              }
              item.setKind(kind)
              item.setDetail(u.id.pkg.toString)
              item
            }
          }
        }.toSeq
      }
      .getOrElse(Seq.empty)
  }
}
