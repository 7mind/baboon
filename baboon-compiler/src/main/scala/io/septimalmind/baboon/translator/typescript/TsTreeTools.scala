package io.septimalmind.baboon.translator.typescript

import io.septimalmind.baboon.translator.DocumentationRenderer
import io.septimalmind.baboon.typer.model.Docs

trait TsTreeTools {

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
    *
    * Per spec §7.4: TypeScript uses Javadoc-style `/** … */` blocks.
    */
  def renderDocs(docs: Docs, indent: String): String
}

object TsTreeTools {
  class TsTreeToolsImpl extends TsTreeTools {
    def renderDocs(docs: Docs, indent: String): String = DocumentationRenderer.javadoc(docs, indent)
  }
}
