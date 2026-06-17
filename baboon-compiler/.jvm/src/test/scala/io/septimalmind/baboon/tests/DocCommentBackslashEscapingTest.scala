package io.septimalmind.baboon.tests

import io.septimalmind.baboon.translator.csharp.{CSTreeTools, CSValue}
import io.septimalmind.baboon.translator.python.PyTreeTools
import io.septimalmind.baboon.translator.rust.{RsTreeTools, RsValue}
import io.septimalmind.baboon.typer.model.{DocComment, Docs}
import izumi.fundamentals.platform.strings.TextTree
import izumi.fundamentals.platform.strings.TextTree.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** RED reproduction for defect D35 (task T133).
  *
  * A lone backslash (a `\` followed by a character that is NOT a valid Scala
  * escape, e.g. the `\W` in `path C:\Windows`) in a cleaned doc-comment body
  * aborts codegen.
  *
  * Root cause (confirmed hypothesis H33): the per-backend `renderDocs`
  * (`*TreeTools.scala`) escapes at most XML/HTML/triple-quote but NEVER a lone
  * backslash. `prependDocs` (`*DefnTranslator.scala`) interpolates that rendered
  * String into the izumi `q"${block}$tree"` quote. A `String` interpolation
  * argument becomes a `TextTree.StringNode` (verified against izumi 1.2.24
  * `TextTree$LowPrioInterpolationArg_1$$anon$1.asNode`), and the rendering pass
  * `TextTree.mapRender`/`dump` runs `scala.StringContext.processEscapes` over
  * every `StringNode.value`. A lone backslash is an invalid Scala escape, so
  * `processEscapes` throws `scala.StringContext$InvalidEscapeException` and
  * codegen aborts.
  *
  * Python is structurally distinct: `PyTreeTools.renderClassDocstring` /
  * `renderMethodDocstring` escape only `"""` (not a lone backslash), so the
  * lone `\` survives to the q-site reproduced from `PyDefnTranslator.scala:269`
  * (`q"$classDocstring"`) and crashes there too.
  *
  * These assertions express the POST-FIX contract (the rendered, interpolated,
  * and RENDERED output round-trips the backslash with NO exception). They
  * therefore FAIL at HEAD with `scala.StringContext$InvalidEscapeException`,
  * pinning the defect at the izumi q-interpolation render. The fix is task T135;
  * this task adds the RED repro only.
  *
  * The Python DOUBLE-ESCAPE GUARD (`pyEscapesTripleQuoteExactlyOnce`) asserts
  * the pre-existing `"""` escaping is preserved; it PASSES at HEAD and is the
  * regression guard for T135's ordering fix.
  *
  * Host-toolchain-free: instantiates the `*TreeToolsImpl` classes directly and
  * renders via `TextTree.dump`; no full compiler stack, runs under
  * `sbt baboonJVM/test`.
  */
class DocCommentBackslashEscapingTest extends AnyFlatSpec with Matchers {

  // A cleaned doc body holding exactly one lone backslash followed by a
  // char that is NOT a valid Scala escape. `\W` (as in `C:\Windows`) is an
  // INVALID Scala escape — `scala.StringContext.processEscapes` rejects it,
  // which is the precise D35 trigger. (Note `\t`/`\n` are VALID escapes and
  // would be silently rewritten rather than throwing, so they do NOT
  // reproduce the defect — the trigger char must be a non-escape letter.)
  private val backslashLiteral: String  = "\\" + "Windows"
  private val loneBackslashBody: String = "Windows path example: C:" + backslashLiteral + " directory."

  // The post-fix expectation: the literal backslash must survive rendering.
  private val expectedBackslashSubstring: String = "C:" + backslashLiteral

  private def loneBackslashDocs: Docs =
    Docs(prefix = Some(DocComment(raw = loneBackslashBody, cleaned = loneBackslashBody)), suffix = None)

  private val csTrees: CSTreeTools = new CSTreeTools.CSTreeToolsImpl()
  private val rsTrees: RsTreeTools = new RsTreeTools.RsTreeToolsImpl
  private val pyTrees: PyTreeTools = new PyTreeTools.PyTreeToolsImpl

  // Faithful reproduction of CSDefnTranslator.prependDocs (CSDefnTranslator:276):
  //   q"${block}$tree" rendered via dump().
  private def csPrependDocsRendered: String = {
    val block: String           = csTrees.renderDocs(loneBackslashDocs, "")
    val tree: TextTree[CSValue] = q"public sealed record DocItem;"
    q"${block}$tree".dump
  }

  // Faithful reproduction of RsDefnTranslator.prependDocs (RsDefnTranslator:54).
  private def rsPrependDocsRendered: String = {
    val block: String           = rsTrees.renderDocs(loneBackslashDocs, "")
    val tree: TextTree[RsValue] = q"pub struct DocItem;"
    q"${block}$tree".dump
  }

  // Faithful reproduction of the Python class-docstring q-site
  // (PyDefnTranslator:269 `q"$classDocstring"`, assembled into the class body
  // at PyDefnTranslator:282-284).
  private def pyClassDocstringRendered: String = {
    val classDocstring: String = pyTrees.renderClassDocstring(loneBackslashDocs, Seq.empty, "    ")
    val docTree: TextTree[Nothing] = q"$classDocstring"
    val classTree =
      q"""class DocItem(BaboonGenerated):
         |    $docTree
         |    pass
         |""".stripMargin
    classTree.dump
  }

  // Faithful reproduction of the Python method-docstring q-site.
  private def pyMethodDocstringRendered: String = {
    val methodDocstring: String    = pyTrees.renderMethodDocstring(loneBackslashDocs, "        ")
    val docTree: TextTree[Nothing] = q"$methodDocstring"
    docTree.dump
  }

  // ─── (a) renderDocs + prependDocs path: C# plus Rust ─────────────────────

  "C# prependDocs path (CSDefnTranslator:276)" should
    "render a lone-backslash doc body without aborting and round-trip the backslash (D35 post-fix contract)" in {
    // POST-FIX: no throw, backslash survives. At HEAD this FAILS with
    // scala.StringContext$InvalidEscapeException at the izumi q-interpolation render.
    val rendered = csPrependDocsRendered
    rendered should include(expectedBackslashSubstring)
  }

  "Rust prependDocs path (RsDefnTranslator:54)" should
    "render a lone-backslash doc body without aborting and round-trip the backslash (D35 post-fix contract)" in {
    val rendered = rsPrependDocsRendered
    rendered should include(expectedBackslashSubstring)
  }

  // ─── (b) Python q-site: class + method docstrings ────────────────────────

  "Python class-docstring q-site (PyDefnTranslator:269)" should
    "render a lone-backslash class doc body without aborting and round-trip the backslash (D35 post-fix contract)" in {
    val rendered = pyClassDocstringRendered
    rendered should include(expectedBackslashSubstring)
  }

  "Python method-docstring q-site (PyDefnTranslator:269)" should
    "render a lone-backslash method doc body without aborting and round-trip the backslash (D35 post-fix contract)" in {
    val rendered = pyMethodDocstringRendered
    rendered should include(expectedBackslashSubstring)
  }

  // ─── (c) Python double-escape guard: PASSES at HEAD ──────────────────────
  //
  // Prose containing a literal `"""` must be escaped exactly once to
  // `\"\"\"`. escape() already handles this; this is the regression guard for
  // T135's ordering fix (the backslash fix must not double-escape the
  // triple-quote that escape() already produced).

  "Python double-escape guard" should
    "escape a literal triple-quote in prose exactly once (regression guard, PASSES at HEAD)" in {
    val tq = "\"\"\""
    val docs =
      Docs(prefix = Some(DocComment(raw = s"Has $tq inside", cleaned = s"Has $tq inside")), suffix = None)
    val result = pyTrees.renderClassDocstring(docs, Seq.empty, "    ")

    // Exactly one occurrence of the escaped form `\"\"\"`.
    val escaped = "\\\"\\\"\\\""
    countOccurrences(result, escaped) shouldBe 1
    // The raw, unescaped triple-quote must not survive in the body.
    result should not include (tq + " inside")
  }

  private def countOccurrences(haystack: String, needle: String): Int = {
    var count = 0
    var idx   = haystack.indexOf(needle)
    while (idx >= 0) {
      count += 1
      idx = haystack.indexOf(needle, idx + needle.length)
    }
    count
  }
}
