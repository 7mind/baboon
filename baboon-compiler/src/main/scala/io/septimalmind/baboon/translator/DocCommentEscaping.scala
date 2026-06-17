package io.septimalmind.baboon.translator

/** Shared helper for doc-comment prose that is interpolated into an izumi
  * `q"…"` quote (`TextTree`).
  *
  * Defect D35: a `String` interpolated into a `q"…"` becomes a
  * `TextTree.StringNode`; the rendering pass (`TextTree.mapRender` / `.dump`,
  * `TextTree.scala:125`) runs `scala.StringContext.processEscapes` over every
  * `StringNode.value`. A lone backslash followed by a char that is NOT a valid
  * Scala escape (e.g. the `\W` in a `C:\Windows` path) is an INVALID Scala
  * escape, so `processEscapes` throws
  * `scala.StringContext$InvalidEscapeException` and codegen aborts.
  *
  * The fix pre-escapes every `\` to `\\` in the cleaned prose. `processEscapes`
  * then round-trips `\\` back to a single `\`, so the FINAL emitted doc comment
  * shows the ORIGINAL single backslash — and never throws.
  *
  * Ordering invariant (see callers): this MUST run on the raw prose BEFORE any
  * structural escaper that itself INTRODUCES backslashes (e.g. Python's
  * `"""` -> `\"\"\"` replacement, `PyTreeTools.escape`). Running it afterwards
  * would double-escape the backslashes the structural escaper produced.
  *
  * We do NOT patch izumi (external dep `fundamentals-platform` 1.2.24).
  */
object DocCommentEscaping {

  /** Escape `\` -> `\\` so the value survives `StringContext.processEscapes`
    * inside an izumi `q"…"` interpolation and round-trips to the original
    * single backslash at render time.
    */
  def escapeBackslashForQInterpolation(s: String): String =
    s.replace("\\", "\\\\")
}
