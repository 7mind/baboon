package io.septimalmind.baboon.translator.dart

import io.septimalmind.baboon.translator.DocCommentEscaping
import io.septimalmind.baboon.typer.model.Docs
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

trait DtTreeTools {
  def inLib(tree: TextTree[DtValue]): TextTree[DtValue]

  /** Render Dart outer-line `///` doc comments before an item for a `Docs` value.
    *
    * Returns `""` when both `docs.prefix` and `docs.suffix` are absent
    * (`Docs.empty`), so callers that prepend this to a symbol emit no extra
    * whitespace for symbols without docs.
    *
    * When content is present each cleaned doc line is emitted as `<indent>/// <line>`
    * (or `<indent>///` for blank paragraph-separator lines). Suffix docs (`//!`
    * postfix) are merged after the prefix body, separated by a blank `///` line.
    *
    * Per spec §7.8: Dart uses outer-line `///` only. Body text is treated as
    * Markdown by `dart doc`; no escaping is needed.
    *
    * Output ends with `\n`.
    */
  def renderDocs(docs: Docs, indent: String): String
}

object DtTreeTools {
  class DtTreeToolsImpl extends DtTreeTools {
    def inLib(tree: TextTree[DtValue]): TextTree[DtValue] = {
      tree
    }

    def renderDocs(docs: Docs, indent: String): String = {
      val prefixLines = docs.prefix.map(_.cleaned.split("\n", -1).toList).getOrElse(Nil)
      val suffixLines = docs.suffix.map(_.cleaned.split("\n", -1).toList).getOrElse(Nil)

      if (prefixLines.isEmpty && suffixLines.isEmpty) return ""

      val allLines: List[String] =
        if (suffixLines.isEmpty) prefixLines
        else if (prefixLines.isEmpty) suffixLines
        else prefixLines ++ List("") ++ suffixLines

      // D35: renderDocs introduces no backslashes (plain `///`), so the
      // backslash-escape is applied at the renderDocs boundary. See
      // DocCommentEscaping.escapeBackslashForQInterpolation.
      val rendered = allLines.map {
        case "" => s"$indent///"
        case l  => s"$indent/// ${DocCommentEscaping.escapeBackslashForQInterpolation(l)}"
      }.mkString("\n")

      s"$rendered\n"
    }
  }
}
