package io.septimalmind.baboon.translator.python

import io.septimalmind.baboon.translator.DocCommentEscaping
import io.septimalmind.baboon.typer.model.Docs

/** Python docstring emission helpers per spec §7.6 / Q2 lock.
  *
  * Class-level docs use PEP 257 `"""…"""` first-statement form.  Field docs
  * are NOT emitted as per-field statements; they are folded into the
  * class-level docstring as a Sphinx/Google-style `Attributes:` section.
  *
  * Method docs use standard PEP 257 single-paragraph form.
  */
trait PyTreeTools {

  /** Build the combined class docstring for a type.
    *
    * Returns `""` when there are no docs at all (empty `typeDocs` AND every
    * field has empty docs).
    *
    * Otherwise returns a `"""…"""` block that is ready to be inserted as the
    * FIRST statement of the class body.  `indent` must be the class-body
    * indentation level (typically 4 spaces); it is prepended to every line of
    * the block.
    *
    * Shape (spec §7.6):
    *   <indent>"""<type-doc paragraph>
    *   <indent>
    *   <indent>Attributes:
    *   <indent>    <fieldName>: <field doc>
    *   <indent>"""
    *
    * Any literal `"""` in the doc body is escaped to `\"\"\"`.
    */
  def renderClassDocstring(
    typeDocs: Docs,
    fieldDocs: Seq[(String, Docs)],
    indent: String,
  ): String

  /** Build a PEP 257 method docstring.
    *
    * Returns `""` when `methodDocs` is empty (`Docs.empty`).
    *
    * Otherwise returns `<indent>"""<text>\n<indent>"""` ready to be the first
    * statement of the method body.
    */
  def renderMethodDocstring(methodDocs: Docs, indent: String): String
}

object PyTreeTools {

  class PyTreeToolsImpl extends PyTreeTools {

    override def renderClassDocstring(
      typeDocs: Docs,
      fieldDocs: Seq[(String, Docs)],
      indent: String,
    ): String = {
      val typeBody     = mergedBody(typeDocs)
      val fieldsWithDoc = fieldDocs.flatMap { case (name, docs) => mergedBody(docs).map(b => (name, b)) }

      if (typeBody.isEmpty && fieldsWithDoc.isEmpty) return ""

      val sb = new StringBuilder
      sb.append(s"""$indent\"\"\"""")

      typeBody match {
        case Some(body) =>
          // First line directly after opening `"""`
          val bodyLines = splitLines(body)
          bodyLines.headOption.foreach(h => sb.append(escape(h)))
          bodyLines.tail.foreach(l => sb.append(s"\n$indent${escape(l)}"))

          if (fieldsWithDoc.nonEmpty) {
            sb.append(s"\n$indent")
            sb.append(s"\n${indent}Attributes:")
            fieldsWithDoc.foreach {
              case (name, fieldBody) =>
                val fieldLines = splitLines(fieldBody)
                fieldLines.headOption.foreach(h => sb.append(s"\n$indent    $name: ${escape(h)}"))
                fieldLines.tail.foreach(l => sb.append(s"\n$indent        ${escape(l)}"))
            }
          }

        case None =>
          // No type doc — emit only the Attributes section
          sb.append(s"\nAttributes:")
          fieldsWithDoc.foreach {
            case (name, fieldBody) =>
              val fieldLines = splitLines(fieldBody)
              fieldLines.headOption.foreach(h => sb.append(s"\n$indent    $name: ${escape(h)}"))
              fieldLines.tail.foreach(l => sb.append(s"\n$indent        ${escape(l)}"))
          }
      }

      sb.append(s"\n$indent\"\"\"")
      sb.toString()
    }

    override def renderMethodDocstring(methodDocs: Docs, indent: String): String = {
      val body = mergedBody(methodDocs)
      body match {
        case None => ""
        case Some(b) =>
          val lines = splitLines(b)
          if (lines.size == 1) {
            s"""$indent\"\"\"${escape(lines.head)}\"\"\""""
          } else {
            val sb = new StringBuilder
            sb.append(s"""$indent\"\"\"${escape(lines.head)}""")
            lines.tail.foreach(l => sb.append(s"\n$indent${escape(l)}"))
            sb.append(s"\n$indent\"\"\"")
            sb.toString()
          }
      }
    }

    // ─── helpers ───────────────────────────────────────────────────────────

    /** Merge prefix and suffix into a single text body.
      *
      * Prefix and suffix are joined with a blank line separator (spec §7.6:
      * "if a field has both prefix and suffix docs, concat them with a blank
      * line in the Attributes: entry").  Returns `None` when both are absent.
      */
    private def mergedBody(docs: Docs): Option[String] = {
      val prefixBody = docs.prefix.map(_.cleaned.trim).filter(_.nonEmpty)
      val suffixBody = docs.suffix.map(_.cleaned.trim).filter(_.nonEmpty)
      (prefixBody, suffixBody) match {
        case (None, None)       => None
        case (Some(p), None)    => Some(p)
        case (None, Some(s))    => Some(s)
        case (Some(p), Some(s)) => Some(s"$p\n\n$s")
      }
    }

    private def splitLines(s: String): List[String] =
      s.replace("\r\n", "\n").split("\n", -1).toList

    /** Escape doc body text for embedding in a Python `"""…"""` docstring that
      * is interpolated into an izumi `q"…"` quote.
      *
      * D35 ORDERING: the lone-backslash escape MUST run on the RAW line FIRST,
      * BEFORE the `"""` -> `\"\"\"` replacement — because that triple-quote
      * replacement INTRODUCES backslashes. Escaping after it would double-escape
      * those introduced backslashes, corrupting the Python triple-quote
      * escaping. See DocCommentEscaping.escapeBackslashForQInterpolation.
      */
    private def escape(s: String): String =
      DocCommentEscaping
        .escapeBackslashForQInterpolation(s)
        .replace("\"\"\"", "\\\"\\\"\\\"")
  }
}
