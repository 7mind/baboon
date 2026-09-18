package io.septimalmind.baboon.translator

import io.septimalmind.baboon.typer.model.Docs

object DocumentationRenderer {
  def javadoc(docs: Docs, indent: String): String = {
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
