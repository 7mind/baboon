package io.septimalmind.baboon.translator.java

import io.septimalmind.baboon.typer.model.Docs
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

trait JvTreeTools {
  def inPkg(nss: Seq[String], tree: TextTree[JvValue]): TextTree[JvValue]

  /** Render a Javadoc `/** … */` comment block for a `Docs` value.
    *
    * Returns the empty string when both `docs.prefix` and `docs.suffix` are
    * absent (`Docs.empty`), so callers that prepend this to a symbol emit no
    * extra whitespace for symbols without docs.
    *
    * When content is present, the returned string is a `/** … */` block with
    * each interior line prefixed by ` * `, followed by a newline so that the
    * calling site can immediately emit the symbol on the next line.
    *
    * Suffix docs (`//!` postfix) are merged into the same Javadoc block,
    * separated from the prefix body by a blank ` *` line.
    *
    * The `indent` parameter is prepended to every line of the block (including
    * the opening `/**` and closing ` */`), matching the indentation of the
    * surrounding context.
    *
    * Body text is HTML-escaped per spec §7.2: `<` → `&lt;`, `>` → `&gt;`,
    * `&` → `&amp;` (Javadoc renders the body as HTML).
    */
  def renderDocs(docs: Docs, indent: String): String
}

object JvTreeTools {
  class JvTreeToolsImpl extends JvTreeTools {
    def inPkg(nss: Seq[String], tree: TextTree[JvValue]): TextTree[JvValue] = {
      if (nss.isEmpty) {
        tree
      } else {
        q"""package ${nss.mkString(".")};
           |
           |${tree.trim}""".stripMargin
      }
    }

    def renderDocs(docs: Docs, indent: String): String = {
      val prefixLines = docs.prefix.map(_.cleaned.split("\n", -1).toList).getOrElse(Nil)
      val suffixLines = docs.suffix.map(_.cleaned.split("\n", -1).toList).getOrElse(Nil)

      if (prefixLines.isEmpty && suffixLines.isEmpty) return ""

      val allLines: List[String] =
        if (suffixLines.isEmpty) prefixLines
        else if (prefixLines.isEmpty) suffixLines
        else prefixLines ++ List("") ++ suffixLines

      val escaped = allLines.map(htmlEscape)

      // Single-line compact form: /** text */
      val rendered =
        if (escaped.size == 1) {
          s"$indent/** ${escaped.head} */\n"
        } else {
          val middle = escaped.map {
            case "" => s"$indent *"
            case l  => s"$indent * $l"
          }.mkString("\n")
          s"$indent/**\n$middle\n$indent */\n"
        }
      rendered
    }

    private def htmlEscape(s: String): String =
      s.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;")
  }
}
