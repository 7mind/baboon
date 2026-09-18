package io.septimalmind.baboon.translator.scl

import io.septimalmind.baboon.translator.DocumentationRenderer
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

    def renderDocs(docs: Docs, indent: String): String = DocumentationRenderer.javadoc(docs, indent)
  }
}
