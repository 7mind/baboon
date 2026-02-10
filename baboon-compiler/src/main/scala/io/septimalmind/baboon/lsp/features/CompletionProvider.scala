package io.septimalmind.baboon.lsp.features

import io.septimalmind.baboon.lsp.LspLogging
import io.septimalmind.baboon.lsp.protocol.{CompletionItem, CompletionItemKind, Position}
import io.septimalmind.baboon.lsp.state.{DocumentState, WorkspaceState}
import io.septimalmind.baboon.translator.{ServiceContextResolver, ServiceResultResolver}
import io.septimalmind.baboon.typer.model._
import io.septimalmind.baboon.util.BLogger

class CompletionProvider(
  documentState: DocumentState,
  workspaceState: WorkspaceState,
  logger: BLogger
) {

  private val keywords = Seq(
    "model", "version", "import", "root", "data", "adt",
    "enum", "foreign", "mixin", "contract", "service", "pragma", "derived", "was"
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

      case CompletionContext.PragmaKeyPosition =>
        getPragmaKeyCompletions

      case CompletionContext.PragmaValuePosition(key) =>
        getPragmaValueCompletions(key)

      case CompletionContext.Unknown =>
        getKeywordCompletions ++ getTypeCompletions ++ getBuiltinCompletions ++ getPragmaKeyCompletions
    }

    // Filter by prefix - match against both filterText (simple name) and label (full path)
    // Supports: prefix match, camel hump match (e.g., "PaSta" matches "PaymentState")
    val filtered = prefix match {
      case Some(p) if p.nonEmpty =>
        candidates.filter { item =>
          val labelMatches      = matchesCamelCase(p, item.label)
          val filterTextMatches = item.filterText.exists(matchesCamelCase(p, _))
          labelMatches || filterTextMatches
        }
      case _ =>
        candidates
    }

    deduplicateCompletions(filtered)
  }

  private sealed trait CompletionContext
  private object CompletionContext {
    case object TypePosition       extends CompletionContext
    case object KeywordPosition    extends CompletionContext
    case object FieldName          extends CompletionContext
    case object PragmaKeyPosition  extends CompletionContext
    case class PragmaValuePosition(key: String) extends CompletionContext
    case object Unknown            extends CompletionContext
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
          // Pragma key: "pragma scala.service.result." or "pragma sc"
          val pragmaKeyPattern = """^\s*pragma\s+([\w.]*)$""".r
          // Pragma value: 'pragma scala.service.result.no-errors = "tru'
          val pragmaValuePattern = """^\s*pragma\s+([\w.]+)\s*=\s*"?(\w*)$""".r

          beforeCursor match {
            case pragmaValuePattern(key, prefix) =>
              (CompletionContext.PragmaValuePosition(key), Some(prefix))
            case pragmaKeyPattern(prefix) =>
              (CompletionContext.PragmaKeyPosition, Some(prefix))
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
      CompletionItem(
        label = bt,
        kind = Some(CompletionItemKind.TypeParameter),
        detail = Some("builtin type"),
        sortText = Some(s"1_$bt")
      )
    }
  }

  private def getPragmaKeyCompletions: Seq[CompletionItem] = {
    (ServiceResultResolver.knownPragmaKeys ++ ServiceContextResolver.knownPragmaKeys).map { case (key, description) =>
      CompletionItem(
        label = key,
        kind = Some(CompletionItemKind.Property),
        detail = Some(description),
        insertText = Some(key),
        sortText = Some(s"0_$key"),
        filterText = Some(key)
      )
    }
  }

  private def getPragmaValueCompletions(key: String): Seq[CompletionItem] = {
    val booleanSuffixes = Set("no-errors", "hkt")
    val isBoolean = booleanSuffixes.exists(s => key.endsWith(s".$s"))
    val isContextMode = key.endsWith(".service.context")
    if (isBoolean) {
      Seq("true", "false").map { v =>
        CompletionItem(
          label = v,
          kind = Some(CompletionItemKind.Value),
          detail = Some("boolean value"),
        )
      }
    } else if (isContextMode) {
      Seq("none", "abstract", "type").map { v =>
        CompletionItem(
          label = v,
          kind = Some(CompletionItemKind.Value),
          detail = Some("context mode"),
        )
      }
    } else {
      Seq.empty
    }
  }

  private def getTypeCompletions: Seq[CompletionItem] = {
    val hasFamily = workspaceState.getFamily.isDefined
    logger.message(LspLogging.Context, s"getTypeCompletions: hasFamily=$hasFamily")

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
              val simpleName = u.id.name.name
              // Use owner (namespace path) + name, without the package prefix
              val pathParts  = u.id.owner.asPseudoPkg :+ simpleName
              val typePath   = pathParts.mkString(".")
              CompletionItem(
                label = typePath,
                kind = Some(kind),
                detail = Some(u.id.pkg.toString),
                insertText = Some(typePath),
                sortText = Some(s"0_$simpleName"),
                filterText = Some(simpleName)
              )
            }
          }
        }.toSeq
        logger.message(LspLogging.Context, s"getTypeCompletions: found ${types.size} types")
        types
      }
      .getOrElse(Seq.empty)
  }

  private def matchesCamelCase(query: String, candidate: String): Boolean =
    CamelCaseMatcher.matches(query, candidate)

  private final case class CompletionKey(
    label: String,
    kind: Option[Int],
    detail: Option[String],
    insertText: Option[String],
    filterText: Option[String]
  )

  private def deduplicateCompletions(items: Seq[CompletionItem]): Seq[CompletionItem] = {
    val seen = scala.collection.mutable.LinkedHashSet.empty[CompletionKey]
    val out  = scala.collection.mutable.ArrayBuffer.empty[CompletionItem]

    items.foreach { item =>
      val key = CompletionKey(item.label, item.kind, item.detail, item.insertText, item.filterText)
      if (!seen.contains(key)) {
        seen.add(key)
        out += item
      }
    }

    out.toSeq
  }
}

/** IntelliJ-style camel case matching.
  * "PaSta" matches "PaymentState" because:
  * - P matches P (start of hump)
  * - a matches a (within hump)
  * - S matches S (start of next hump)
  * - ta matches ta (within hump)
  *
  * Also supports plain prefix matching and dot-separated paths.
  */
object CamelCaseMatcher {
  def matches(query: String, candidate: String): Boolean = {
    if (query.isEmpty) return true
    if (candidate.isEmpty) return false

    // Plain lowercase prefix match (most common case)
    if (candidate.toLowerCase.startsWith(query.toLowerCase)) return true

    // Camel hump matching
    matchCamelHumps(query, 0, candidate, 0)
  }

  private def matchCamelHumps(query: String, qi: Int, candidate: String, ci: Int): Boolean = {
    if (qi >= query.length) return true
    if (ci >= candidate.length) return false

    val qc = query.charAt(qi)
    val cc = candidate.charAt(ci)

    if (qc.toLower == cc.toLower) {
      // Characters match - if query char is uppercase, candidate char should also be uppercase (hump start)
      // or we're at the start of the candidate
      val humpMatch = !qc.isUpper || cc.isUpper || ci == 0
      if (humpMatch) {
        matchCamelHumps(query, qi + 1, candidate, ci + 1)
      } else {
        // Query has uppercase but candidate doesn't - skip to next hump in candidate
        skipToNextHump(query, qi, candidate, ci + 1)
      }
    } else if (qc.isUpper) {
      // Query char is uppercase, skip to next hump in candidate
      skipToNextHump(query, qi, candidate, ci + 1)
    } else {
      // No match, try skipping candidate char (within current hump)
      if (cc.isUpper && ci > 0) {
        // We hit a new hump without matching - fail this path
        false
      } else {
        matchCamelHumps(query, qi, candidate, ci + 1)
      }
    }
  }

  private def skipToNextHump(query: String, qi: Int, candidate: String, ci: Int): Boolean = {
    if (ci >= candidate.length) return false

    val cc = candidate.charAt(ci)
    if (cc.isUpper || cc == '.' || cc == '_') {
      // Found a hump boundary, try matching from here
      matchCamelHumps(query, qi, candidate, ci)
    } else {
      skipToNextHump(query, qi, candidate, ci + 1)
    }
  }
}
