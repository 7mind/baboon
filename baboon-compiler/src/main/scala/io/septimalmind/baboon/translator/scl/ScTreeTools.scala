package io.septimalmind.baboon.translator.scl

import io.septimalmind.baboon.translator.DocCommentEscaping
import io.septimalmind.baboon.typer.model.Docs
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

trait ScTreeTools {
  def inNs(nss: Seq[String], tree: TextTree[ScValue]): TextTree[ScValue]

  /** Render a Javadoc-style `/** … */` comment block for a `Docs` value.
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
    */
  def renderDocs(docs: Docs, indent: String): String
}

object ScTreeTools {
  class ScTreeToolsImpl extends ScTreeTools {
    def inNs(nss: Seq[String], tree: TextTree[ScValue]): TextTree[ScValue] = {
      if (nss.isEmpty) {
        tree
      } else {
        q"""package ${nss.mkString(".")} {
           |  ${tree.shift(2).trim}
           |}""".stripMargin
      }
    }

    def renderDocs(docs: Docs, indent: String): String = {
      val prefixLines = docs.prefix.map(_.cleaned.split("\n", -1).toList).getOrElse(Nil)
      val suffixLines = docs.suffix.map(_.cleaned.split("\n", -1).toList).getOrElse(Nil)

      if (prefixLines.isEmpty && suffixLines.isEmpty) return ""

      val mergedLines: List[String] =
        if (suffixLines.isEmpty) prefixLines
        else if (prefixLines.isEmpty) suffixLines
        else prefixLines ++ List("") ++ suffixLines

      // D35: renderDocs introduces no backslashes (plain `/** */`), so the
      // backslash-escape is applied at the renderDocs boundary. See
      // DocCommentEscaping.escapeBackslashForQInterpolation.
      val allLines: List[String] =
        mergedLines.map(DocCommentEscaping.escapeBackslashForQInterpolation)

      // Single-line compact form: /** text */
      val rendered =
        if (allLines.size == 1) {
          s"$indent/** ${allLines.head} */\n"
        } else {
          val middle = allLines.map {
            case "" => s"$indent *"
            case l  => s"$indent * $l"
          }.mkString("\n")
          s"$indent/**\n$middle\n$indent */\n"
        }
      rendered
    }
  }
}
