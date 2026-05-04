package io.septimalmind.baboon.translator.rust

import io.septimalmind.baboon.typer.model.Docs
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*

trait RsTreeTools {
  def inMod(modPath: Seq[String], tree: TextTree[RsValue]): TextTree[RsValue]

  /** Render Rust outer-line `///` doc comments before an item for a `Docs` value.
    *
    * Returns `""` when both `docs.prefix` and `docs.suffix` are absent
    * (`Docs.empty`), so callers that prepend this to a symbol emit no extra
    * whitespace for symbols without docs.
    *
    * When content is present each cleaned doc line is emitted as `<indent>/// <line>`
    * (or `<indent>///` for blank paragraph-separator lines). Suffix docs (`//!`
    * postfix) are merged after the prefix body, separated by a blank `///` line.
    *
    * Per spec §7.7: Rust uses outer-line `///` only. The `//!` module-inner form
    * is NOT used in generated output.
    *
    * Output ends with `\n`.
    */
  def renderDocs(docs: Docs, indent: String): String
}

object RsTreeTools {
  class RsTreeToolsImpl extends RsTreeTools {
    def inMod(modPath: Seq[String], tree: TextTree[RsValue]): TextTree[RsValue] = {
      // In Rust, we don't wrap in nested modules in the same file.
      // The module hierarchy is determined by the file path.
      // So we just return the tree as-is.
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

      val rendered = allLines.map {
        case "" => s"$indent///"
        case l  => s"$indent/// $l"
      }.mkString("\n")

      s"$rendered\n"
    }
  }
}
