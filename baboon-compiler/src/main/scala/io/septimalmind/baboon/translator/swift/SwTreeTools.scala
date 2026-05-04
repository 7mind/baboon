package io.septimalmind.baboon.translator.swift

import io.septimalmind.baboon.typer.model.{Docs, Owner}
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.Quote

trait SwTreeTools {
  def inLib(tree: TextTree[SwValue], owner: Owner): TextTree[SwValue]

  /** Render Swift outer-line `///` doc comments before an item for a `Docs` value.
    *
    * Returns `""` when both `docs.prefix` and `docs.suffix` are absent
    * (`Docs.empty`), so callers that prepend this to a symbol emit no extra
    * whitespace for symbols without docs.
    *
    * When content is present each cleaned doc line is emitted as `<indent>/// <line>`
    * (or `<indent>///` for blank paragraph-separator lines). Suffix docs (`//!`
    * postfix) are merged after the prefix body, separated by a blank `///` line.
    *
    * Per spec §7.9: Swift uses outer-line `///` only. Body text is treated as
    * Markdown by DocC; no escaping is needed.
    *
    * Output ends with `\n`.
    */
  def renderDocs(docs: Docs, indent: String): String
}

object SwTreeTools {
  class SwTreeToolsImpl(trans: SwTypeTranslator) extends SwTreeTools {
    def inLib(tree: TextTree[SwValue], owner: Owner): TextTree[SwValue] = {
      owner match {
        case Owner.Toplevel | _: Owner.Adt => tree
        case Owner.Ns(path) =>
          val nsPath = path.map(s => trans.escapeSwiftKeyword(s.name.toLowerCase)).mkString(".")
          q"""extension $nsPath {
             |${tree.shift(4).trim}
             |}""".stripMargin
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

      val rendered = allLines.map {
        case "" => s"$indent///"
        case l  => s"$indent/// $l"
      }.mkString("\n")

      s"$rendered\n"
    }
  }
}
