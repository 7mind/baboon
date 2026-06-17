package io.septimalmind.baboon.translator.mcp

import io.septimalmind.baboon.typer.model.Docs

/** Shared, language-agnostic helper that flattens a [[Docs]] value into a
  * plain-text string suitable for use as an MCP tool or parameter
  * `description` (a JSON string value).
  *
  * This is intentionally DISTINCT from the per-backend `renderDocs` methods
  * (e.g. `KtTreeTools`, `TsTreeTools`): those emit language-specific comment
  * delimiters (`/** */`, `///`, `#`, etc.) and are not usable as JSON string
  * values. This helper strips all comment syntax and produces plain text.
  *
  * === Normalisation contract ===
  *
  *   - `Docs.empty` (both `prefix` and `suffix` absent) → `None`.
  *   - `DocComment.cleaned` is preferred over `raw` (cleaned has comment
  *     delimiters already stripped by the typer's `DocFormat.liftDocs`).
  *   - If only one of prefix / suffix is present, its cleaned text is used
  *     as-is.
  *   - If both are present they are concatenated with a single blank line
  *     separator (`\n\n`), preserving internal newlines.
  *   - Trailing whitespace is stripped from every line; leading/trailing
  *     blank lines of the whole result are trimmed.
  *   - The result never contains `/**`, `*/`, `///`, `//!`, `'''`, or `#`
  *     doc-comment delimiters — it is safe to embed directly in a JSON string.
  *   - All 9 backends call this single helper so the normalisation is a
  *     single source of truth.
  */
object McpDocs {

  /** Convert `docs` to an MCP-safe plain-text description.
    *
    * @param docs
    *   the `Docs` value from a `MethodDef`, `Field`, or `DomainMember.User`
    * @return
    *   `None` when `docs == Docs.empty`; otherwise `Some` of the flattened,
    *   whitespace-normalised cleaned text.
    */
  def flatten(docs: Docs): Option[String] = {
    val prefixText = docs.prefix.map(_.cleaned)
    val suffixText = docs.suffix.map(_.cleaned)

    (prefixText, suffixText) match {
      case (None, None) => None
      case _ =>
        val combined = List(prefixText, suffixText).flatten.mkString("\n\n")
        val normalised = normalise(combined)
        if (normalised.isEmpty) None else Some(normalised)
    }
  }

  /** Strip trailing whitespace from every line and trim leading/trailing blank
    * lines from the whole text.
    */
  private def normalise(text: String): String = {
    text
      .split("\n", -1)
      .map(line => line.stripTrailing())
      .mkString("\n")
      .trim
  }
}
