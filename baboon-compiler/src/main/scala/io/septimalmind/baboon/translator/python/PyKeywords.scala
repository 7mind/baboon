package io.septimalmind.baboon.translator.python

/** Utilities for handling Python reserved words in generated identifiers.
  *
  * Python has no escape syntax (unlike Scala backticks or C# @-prefix), so
  * identifiers that collide with keywords must be RENAMED using the PEP 8
  * trailing-underscore convention (e.g. `class` → `class_`).
  *
  * Wire-key preservation: when a field is renamed the JSON codec must continue
  * to emit/consume the ORIGINAL model-name as the dict key. Callers use
  * `isKeyword` to detect this case and emit the appropriate pydantic
  * `Field(alias=..., serialization_alias=...)`.
  *
  * UEBA codecs are positional and therefore rename-neutral.
  */
object PyKeywords {

  // Python hard keywords (3.x, as returned by `keyword.kwlist`):
  //   False, None, True, and, as, assert, async, await, break, class, continue,
  //   def, del, elif, else, except, finally, for, from, global, if, import, in,
  //   is, lambda, nonlocal, not, or, pass, raise, return, try, while, with, yield
  //
  // Python soft keywords (contextually reserved — also escaped for safety):
  //   match, case, type
  //
  // Additionally the lowercase spellings "none", "true", "false" are escaped
  // because `.capitalize` on "none"/"true"/"false" yields "None"/"True"/"False"
  // (the builtin constants), which are hard keywords after capitalisation. Since
  // Python is case-sensitive, lowercase "none" etc. are NOT hard keywords, but
  // we escape them here to prevent `.capitalize` anywhere in the pipeline from
  // silently producing a collision.
  private val keywords: Set[String] = Set(
    // Hard keywords
    "False", "None", "True",
    "and", "as", "assert", "async", "await",
    "break",
    "class", "continue",
    "def", "del",
    "elif", "else", "except",
    "finally", "for", "from",
    "global",
    "if", "import", "in", "is",
    "lambda",
    "nonlocal", "not",
    "or",
    "pass",
    "raise", "return",
    "try",
    "while", "with",
    "yield",
    // Soft keywords (contextually reserved)
    "match", "case", "type",
    // Lowercase spellings whose `.capitalize` produces a hard keyword
    "none", "true", "false",
  )

  /** Returns true if `name` is a Python keyword or soft-keyword (or a
    * lowercase spelling that `.capitalize` would turn into one).
    */
  def isKeyword(name: String): Boolean = keywords.contains(name)

  /** Returns `name + "_"` when `name` is a Python keyword, otherwise returns
    * `name` unchanged (identity for non-keywords).
    *
    * This is the PEP 8-recommended trailing-underscore convention.
    */
  def escapePyKeyword(name: String): String =
    if (isKeyword(name)) s"${name}_" else name
}
