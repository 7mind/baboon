package io.septimalmind.baboon.lsp.features

import io.septimalmind.baboon.lsp.protocol.{CompletionItem, CompletionItemKind, Position}
import io.septimalmind.baboon.lsp.state.{DocumentState, WorkspaceState}
import io.septimalmind.baboon.typer.model._

class CompletionProvider(
  documentState: DocumentState,
  workspaceState: WorkspaceState
) {

  private val keywords = Seq(
    "model", "version", "import", "root", "data", "adt",
    "enum", "foreign", "mixin", "service", "derived", "was"
  )

  private val builtinTypes = Seq(
    "i08", "i16", "i32", "i64", "u08", "u16", "u32", "u64",
    "f32", "f64", "f128", "str", "bit", "uid", "tsu", "tso",
    "bytes", "opt", "lst", "set", "map"
  )

  def getCompletions(uri: String, position: Position): Seq[CompletionItem] = {
    val (context, prefix) = getCompletionContext(uri, position)

    val candidates = context match {
      case CompletionContext.TypePosition =>
        getTypeCompletions ++ getBuiltinCompletions

      case CompletionContext.KeywordPosition =>
        getKeywordCompletions

      case CompletionContext.FieldName =>
        Seq.empty

      case CompletionContext.Unknown =>
        getKeywordCompletions ++ getTypeCompletions ++ getBuiltinCompletions
    }

    // Filter by prefix if one exists
    prefix match {
      case Some(p) if p.nonEmpty =>
        val lowerPrefix = p.toLowerCase
        candidates.filter(_.label.toLowerCase.startsWith(lowerPrefix))
      case _ =>
        candidates
    }
  }

  private sealed trait CompletionContext
  private object CompletionContext {
    case object TypePosition    extends CompletionContext
    case object KeywordPosition extends CompletionContext
    case object FieldName       extends CompletionContext
    case object Unknown         extends CompletionContext
  }

  private def getCompletionContext(uri: String, position: Position): (CompletionContext, Option[String]) = {
    documentState
      .getContent(uri)
      .map { content =>
        val lines = content.split("\n", -1)
        if (position.line < lines.length) {
          val line         = lines(position.line)
          val beforeCursor = line.take(position.character)

          // Pattern to match type position with optional prefix: "field: TypePre" -> ("field: ", "TypePre")
          val typeWithPrefixPattern = """^(.*:\s*)(\w*)$""".r
          // Pattern for generic args: "opt[TypePre" or "map[K, ValuePre"
          val genericArgPattern = """^(.*[\[,]\s*)(\w*)$""".r

          beforeCursor match {
            case typeWithPrefixPattern(_, prefix) =>
              (CompletionContext.TypePosition, Some(prefix))
            case genericArgPattern(_, prefix) =>
              (CompletionContext.TypePosition, Some(prefix))
            case _ if beforeCursor.trim.isEmpty || beforeCursor.matches("^\\s*$") =>
              (CompletionContext.KeywordPosition, None)
            case _ if beforeCursor.matches(".*\\{\\s*$") =>
              (CompletionContext.FieldName, None)
            case _ =>
              // Try to extract word prefix at cursor for general completion
              val wordPattern = """^.*?(\w*)$""".r
              beforeCursor match {
                case wordPattern(prefix) => (CompletionContext.Unknown, Some(prefix))
                case _                   => (CompletionContext.Unknown, None)
              }
          }
        } else {
          (CompletionContext.Unknown, None)
        }
      }
      .getOrElse((CompletionContext.Unknown, None))
  }

  private def getKeywordCompletions: Seq[CompletionItem] = {
    keywords.map { kw =>
      CompletionItem(kw, Some(CompletionItemKind.Keyword), Some("keyword"))
    }
  }

  private def getBuiltinCompletions: Seq[CompletionItem] = {
    builtinTypes.map { bt =>
      CompletionItem(bt, Some(CompletionItemKind.TypeParameter), Some("builtin type"))
    }
  }

  private def getTypeCompletions: Seq[CompletionItem] = {
    val hasFamily = workspaceState.getFamily.isDefined
    System.err.println(s"[LSP] getTypeCompletions: hasFamily=$hasFamily")

    workspaceState.getFamily
      .map { family =>
        val types = family.domains.toMap.values.flatMap { lineage =>
          lineage.versions.toMap.values.flatMap { domain =>
            domain.defs.meta.nodes.values.collect { case u: DomainMember.User =>
              val kind = u.defn match {
                case _: Typedef.Dto      => CompletionItemKind.Class
                case _: Typedef.Adt      => CompletionItemKind.Interface
                case _: Typedef.Enum     => CompletionItemKind.Enum
                case _: Typedef.Foreign  => CompletionItemKind.Reference
                case _: Typedef.Contract => CompletionItemKind.Interface
                case _: Typedef.Service  => CompletionItemKind.Module
              }
              CompletionItem(u.id.name.name, Some(kind), Some(u.id.pkg.toString))
            }
          }
        }.toSeq
        System.err.println(s"[LSP] getTypeCompletions: found ${types.size} types")
        types
      }
      .getOrElse(Seq.empty)
  }
}
