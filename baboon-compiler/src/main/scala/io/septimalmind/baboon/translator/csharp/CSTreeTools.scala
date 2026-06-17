package io.septimalmind.baboon.translator.csharp

import io.septimalmind.baboon.translator.DocCommentEscaping
import io.septimalmind.baboon.typer.model.Docs
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

trait CSTreeTools {
  def inNs(nss: Seq[String], tree: TextTree[CSValue]): TextTree[CSValue]

  /** Render C# XML doc comment lines for a `Docs` value.
    *
    * Returns the empty string when both `docs.prefix` and `docs.suffix` are
    * absent (`Docs.empty`), so callers that prepend this to a symbol emit no
    * extra whitespace for symbols without docs.
    *
    * Spec §7.5 shape: the first paragraph is wrapped in `<summary>…</summary>`;
    * additional paragraphs (separated by blank lines in the cleaned text) are
    * emitted in a `<remarks>…</remarks>` block. Suffix docs (`//!` postfix) are
    * appended as an additional paragraph inside `<remarks>` when a prefix doc
    * exists, or as the sole `<summary>` content when no prefix doc is present.
    *
    * Body text is XML-escaped per spec §7.5: `<` → `&lt;`, `>` → `&gt;`,
    * `&` → `&amp;`, `"` → `&quot;`. The escaping is applied inline here; no
    * external library is required.
    *
    * The `indent` parameter is prepended to every emitted `///` line, matching
    * the indentation of the surrounding context.
    */
  def renderDocs(docs: Docs, indent: String): String
}

object CSTreeTools {
  class CSTreeToolsImpl() extends CSTreeTools {
    def inNs(nss: Seq[String], tree: TextTree[CSValue]): TextTree[CSValue] = {
      if (nss.isEmpty) {
        tree
      } else {
        q"""namespace ${nss.mkString(".")} {
           |    ${tree.shift(4).trim}
           |}""".stripMargin
      }
    }

    def renderDocs(docs: Docs, indent: String): String = {
      val prefixLines = docs.prefix.map(_.cleaned.split("\n", -1).toList).getOrElse(Nil)
      val suffixLines = docs.suffix.map(_.cleaned.split("\n", -1).toList).getOrElse(Nil)

      if (prefixLines.isEmpty && suffixLines.isEmpty) return ""

      // Inline XML-escape: spec §7.5 — applied to every text fragment emitted.
      // D35: backslash-escape the PROSE FIRST (before the XML entity
      // replacements, which never introduce backslashes), so a lone `\`
      // survives the downstream izumi q-interpolation render. See
      // DocCommentEscaping.escapeBackslashForQInterpolation.
      def xmlEscape(s: String): String =
        DocCommentEscaping
          .escapeBackslashForQInterpolation(s)
          .replace("&", "&amp;")
          .replace("<", "&lt;")
          .replace(">", "&gt;")
          .replace("\"", "&quot;")

      // Split cleaned lines into paragraphs (blank-line separated).
      def toParagraphs(lines: List[String]): List[List[String]] = {
        val result = scala.collection.mutable.ListBuffer[List[String]]()
        val cur    = scala.collection.mutable.ListBuffer[String]()
        for (line <- lines) {
          if (line.isEmpty) {
            if (cur.nonEmpty) { result += cur.toList; cur.clear() }
          } else {
            cur += line
          }
        }
        if (cur.nonEmpty) result += cur.toList
        result.toList
      }

      val prefixParas = toParagraphs(prefixLines)
      val suffixParas = toParagraphs(suffixLines)

      // All paragraphs in order: prefix first, then suffix.
      val allParas = prefixParas ++ suffixParas

      if (allParas.isEmpty) return ""

      val sb = new StringBuilder

      // First paragraph → <summary>
      val summaryLines = allParas.head.map(l => xmlEscape(l))
      if (summaryLines.size == 1) {
        sb.append(s"$indent/// <summary>${summaryLines.head}</summary>\n")
      } else {
        sb.append(s"$indent/// <summary>\n")
        summaryLines.foreach(l => sb.append(s"$indent/// $l\n"))
        sb.append(s"$indent/// </summary>\n")
      }

      // Remaining paragraphs → <remarks>
      val remarksParas = allParas.tail
      if (remarksParas.nonEmpty) {
        sb.append(s"$indent/// <remarks>\n")
        remarksParas.zipWithIndex.foreach {
          case (para, idx) =>
            if (idx > 0) sb.append(s"$indent///\n")
            para.foreach(l => sb.append(s"$indent/// ${xmlEscape(l)}\n"))
        }
        sb.append(s"$indent/// </remarks>\n")
      }

      sb.toString()
    }
  }
}
