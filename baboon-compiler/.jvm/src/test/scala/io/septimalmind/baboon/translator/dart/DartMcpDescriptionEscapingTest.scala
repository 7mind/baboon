package io.septimalmind.baboon.translator.dart

import io.septimalmind.baboon.translator.dart.DtMcpServerGenerator.dartString
import org.scalatest.wordspec.AnyWordSpec

/** RED reproduction test for D36 — Dart MCP `dartString` passes exotic C0 control
  * characters VERBATIM into a Dart single-quoted string literal.
  *
  * Defect root cause (pinned by T129 reviewer):
  *   `DtMcpServerGenerator.dartString` escapes only `\` and `'`; it does NOT
  *   escape control characters below 0x20. A description carrying e.g. a vertical
  *   tab (U+000B, 0x0B) therefore produces a Dart literal with a raw control byte
  *   embedded — invalid in Dart source (Dart requires `\uXXXX` or `\xXX` for
  *   such code points in single-quoted string literals).
  *
  * This test expresses the POST-FIX contract (D36/T136 expectation): every code
  * point below U+0020 that is not `\n`, `\r`, `\t` must be emitted as a `\uXXXX`
  * escape. The assertion FAILS at HEAD because the current implementation omits
  * that step — confirming the defect is live.
  *
  * Observed (HEAD): the returned Dart literal contains the raw 0x0B byte.
  * Expected (post T136 fix): the returned Dart literal contains ``.
  *
  * Scope is CONTROL-CHAR-ONLY (no backslash in the probe string) to isolate D36
  * from D35 (backslash-escaping).
  */
class DartMcpDescriptionEscapingTest extends AnyWordSpec {

  private val verticalTab: Char = '' // U+000B, 0x0B

  "DtMcpServerGenerator.dartString (D36 — C0 control char escaping)" should {

    // ── RED: vertical tab (0x0B) must be emitted as \\u000b, not as a raw byte ─

    "escape a vertical-tab (0x0B) in a description as \\u000b, not a raw C0 byte (D36/RED)" in {
      val input  = s"description with${verticalTab}vertical tab"
      val result = dartString(input)

      // Observed at HEAD: result contains the raw 0x0B byte.
      // This assertion captures the failure: at HEAD result still embeds the raw
      // control char, so the assert on `!result.contains(verticalTab.toString)` fails.

      // POST-FIX contract (T136): the raw control char must NOT appear in the
      // emitted Dart literal.
      assert(
        !result.contains(verticalTab.toString),
        s"D36: dartString embedded raw U+000B (0x0B vertical tab) into Dart literal — " +
          s"this is invalid Dart source. Observed: ${result.map(c => if (c < 0x20) f"<U+${c.toInt}%04X>" else c.toString).mkString}",
      )

      // And the escaped form should be present.
      assert(
        result.contains("\\u000b"),
        s"D36: expected dartString to emit \\u000b for U+000B, got: ${result.map(c => if (c < 0x20) f"<U+${c.toInt}%04X>" else c.toString).mkString}",
      )
    }

    // ── Supporting check: ordinary printable chars are unaffected ──────────────

    "leave printable ASCII unmodified (regression guard)" in {
      val result = dartString("hello world")
      assert(result == "'hello world'", s"unexpected: $result")
    }

    "escape backslash and single-quote (existing invariants must hold)" in {
      assert(dartString("a\\b") == "'a\\\\b'", "backslash not escaped")
      assert(dartString("it's") == "'it\\'s'", "single-quote not escaped")
    }
  }
}
