package io.septimalmind.baboon.typer.model

/** Placeholder thrown by translator sites that don't yet handle `TypeRef.Any`.
  * Removed per-language as each milestone (M2..M10) implements the real codec.
  */
object AnyPlaceholder {
  def notSupportedYet(site: String): Nothing =
    throw new RuntimeException(s"BUG: `any` field reached $site before its milestone implementation landed")
}
