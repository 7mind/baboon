package io.septimalmind.baboon.typer

import io.septimalmind.baboon.parser.model.{RawDocComment, RawDocs}
import io.septimalmind.baboon.typer.model.{DocComment, Docs}

/** Canonical doc-body cleanup per `docs/spec/docstrings.md` §5.2 (prefix
  * `/** … */`) and §5.3 (postfix `//! …`).
  *
  * The cleanup is performed exactly once at the typer stage; both raw and
  * cleaned forms are kept on `DocComment` so backends can pick whichever
  * they need (cleaned is the canonical plain-text representation; raw is
  * preserved for round-trip / debugging).
  *
  * Pure / deterministic / idempotent on already-cleaned input.
  */
object DocFormat {

  /** Cleanup for prefix `/** … */` doc bodies.
    *
    * Input: the verbatim source bytes captured by the parser BETWEEN the
    * `/**` and `*/` delimiters (the parser's `RawDocComment.raw` includes
    * the delimiters, so we strip them first if present; passing a body
    * without delimiters is also accepted for symmetry).
    *
    * Steps (spec §5.2):
    *   1. Strip `/**` and `*/` delimiters.
    *   2. Split on line boundaries (collapse `\r\n` → `\n`).
    *   3. Compute the longest common `\s*\*?\s*` prefix across content
    *      (non-separator, non-blank) lines and strip it from EVERY interior
    *      line including separator lines.
    *   4. Right-trim each line.
    *   5. Collapse leading and trailing blank lines.
    *   6. Preserve internal blank lines as paragraph separators.
    *
    * Returns the cleaned body. May be empty (caller silently drops empty
    * results per spec §5.4).
    */
  def cleanPrefix(raw: String): String = {
    val stripped = stripPrefixDelimiters(raw)
    val normalized = stripped.replace("\r\n", "\n")
    val lines = normalized.split("\n", -1).toList

    val commonPrefix = computeCommonPrefix(lines)

    val stripPrefixed = lines.map(line => stripLeadingPrefix(line, commonPrefix))
    val rightTrimmed  = stripPrefixed.map(rstrip)
    val collapsed     = collapseLeadingTrailingBlankLines(rightTrimmed)

    collapsed.mkString("\n")
  }

  /** Cleanup for postfix `//! …` doc bodies (spec §5.3).
    *
    * Input: the verbatim text after `//!` and before end-of-line, OR the
    * full `//! …` line (we strip the marker if present).
    *
    * Steps:
    *   1. Strip the `//!` marker.
    *   2. Strip a single optional leading space.
    *   3. Right-trim trailing whitespace.
    */
  def cleanSuffix(raw: String): String = {
    val stripped =
      if (raw.startsWith("//!")) raw.substring(3)
      else raw
    val withoutLeadingSpace =
      if (stripped.startsWith(" ")) stripped.substring(1)
      else stripped
    rstrip(withoutLeadingSpace)
  }

  /** Lift parser-side `RawDocs` to typer-side `Docs`.
    *
    * Empty / whitespace-only doc bodies are silently dropped per spec §5.4.
    */
  def liftDocs(raw: RawDocs): Docs = {
    Docs(
      prefix = raw.prefix.flatMap(liftPrefix),
      suffix = raw.suffix.flatMap(liftSuffix),
    )
  }

  def liftPrefix(raw: RawDocComment): Option[DocComment] = {
    val cleaned = cleanPrefix(raw.raw)
    if (cleaned.trim.isEmpty) None
    else Some(DocComment(raw.raw, cleaned))
  }

  def liftSuffix(raw: RawDocComment): Option[DocComment] = {
    val cleaned = cleanSuffix(raw.raw)
    if (cleaned.trim.isEmpty) None
    else Some(DocComment(raw.raw, cleaned))
  }

  // ─── helpers ───────────────────────────────────────────────────────────

  private def stripPrefixDelimiters(raw: String): String = {
    val withoutOpen =
      if (raw.startsWith("/**")) raw.substring(3)
      else raw
    val withoutClose =
      if (withoutOpen.endsWith("*/")) withoutOpen.substring(0, withoutOpen.length - 2)
      else withoutOpen
    withoutClose
  }

  /** A line is a "separator line" if it matches `\s*\*?\s*` end-to-end. */
  private def isSeparatorLine(line: String): Boolean = {
    val n          = line.length
    var i          = 0
    while (i < n && (line.charAt(i) == ' ' || line.charAt(i) == '\t')) i += 1
    if (i < n && line.charAt(i) == '*') i += 1
    while (i < n && (line.charAt(i) == ' ' || line.charAt(i) == '\t')) i += 1
    i == n
  }

  /** Compute the maximal `\s*\*?\s*` prefix shared by all content (non-
    * separator, non-blank) lines.
    *
    * Implementation: among content lines, compute each one's maximal
    * `\s*\*?\s*` prefix; the common prefix is the longest string that is a
    * prefix of every such per-line prefix.
    */
  private def computeCommonPrefix(lines: List[String]): String = {
    val contentLines = lines.filterNot(isSeparatorLine)
    if (contentLines.isEmpty) return ""
    val perLinePrefixes = contentLines.map(maximalLeadingPrefix)
    perLinePrefixes.reduce(longestCommonStringPrefix)
  }

  private def maximalLeadingPrefix(line: String): String = {
    val n  = line.length
    var i  = 0
    while (i < n && (line.charAt(i) == ' ' || line.charAt(i) == '\t')) i += 1
    if (i < n && line.charAt(i) == '*') {
      i += 1
      while (i < n && (line.charAt(i) == ' ' || line.charAt(i) == '\t')) i += 1
    }
    line.substring(0, i)
  }

  private def longestCommonStringPrefix(a: String, b: String): String = {
    val n = math.min(a.length, b.length)
    var i = 0
    while (i < n && a.charAt(i) == b.charAt(i)) i += 1
    a.substring(0, i)
  }

  /** Strip the computed common prefix from a line. If the line is shorter
    * than the prefix (separator lines), strip only the matching characters
    * and reduce to empty rather than indexing past end (spec §5.2 step 3c).
    */
  private def stripLeadingPrefix(line: String, prefix: String): String = {
    val n = math.min(line.length, prefix.length)
    var i = 0
    while (i < n && line.charAt(i) == prefix.charAt(i)) i += 1
    line.substring(i)
  }

  private def rstrip(s: String): String = {
    var end = s.length
    while (end > 0 && {
      val c = s.charAt(end - 1)
      c == ' ' || c == '\t'
    }) end -= 1
    s.substring(0, end)
  }

  private def collapseLeadingTrailingBlankLines(lines: List[String]): List[String] = {
    val dropLeading = lines.dropWhile(_.isEmpty)
    dropLeading.reverse.dropWhile(_.isEmpty).reverse
  }

}
