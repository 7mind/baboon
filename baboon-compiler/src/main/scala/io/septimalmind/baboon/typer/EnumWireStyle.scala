package io.septimalmind.baboon.typer

/** Canonical wire-format for enum member names across all backends.
  *
  * Spec: `docs/drafts/20260428-1700-enum-wire-format-spec.md`. Pascal-case
  * (first character uppercased, rest unchanged) is the sole supported form
  * for cross-language JSON interop. UEBA encodes enums as ordinal bytes; this
  * helper governs only the string identifier used in JSON wire and the
  * in-memory target-language enum case.
  */
object EnumWireStyle {

  /** Pascal-case wire-format string for an enum member name.
    *
    * First character only is uppercased; underscores are NOT treated as word
    * boundaries (i.e. `bar_pub` becomes `Bar_pub`, not `BarPub`).
    */
  def wireName(memberName: String): String = memberName.capitalize
}
