package io.septimalmind.baboon.translator.typescript

import scala.util.matching.Regex

/** Applies `--ts-import-suffix` to TypeScript source written by hand — the static runtime
  * resources — whose relative imports are extensionless (https://github.com/7mind/baboon/issues/96).
  * Generated modules get the suffix when their imports are rendered.
  */
object TsImportSuffix {
  // `from "./x"`, `from '../x'`, `import "./x"`, `import("./x")`
  private val RelativeSpecifier: Regex = """((?:\bfrom|\bimport)\s*\(?\s*)(["'])(\.\.?/[^"']*)\2""".r

  def apply(source: String, suffix: String): String = {
    if (suffix.isEmpty) {
      source
    } else {
      RelativeSpecifier.replaceAllIn(
        source,
        m => {
          val specifier = m.group(3)
          val suffixed  = if (specifier.endsWith(suffix)) specifier else specifier + suffix
          Regex.quoteReplacement(m.group(1) + m.group(2) + suffixed + m.group(2))
        },
      )
    }
  }
}
