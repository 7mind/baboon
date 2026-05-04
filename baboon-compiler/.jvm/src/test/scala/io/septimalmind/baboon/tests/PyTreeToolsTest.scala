package io.septimalmind.baboon.tests

import io.septimalmind.baboon.translator.python.PyTreeTools
import io.septimalmind.baboon.typer.model.{DocComment, Docs}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Unit tests for PyTreeTools docstring rendering per spec §7.6 / Q2 lock.
  *
  * These tests exercise the rendering logic in isolation (no full compiler
  * stack) and also serve as a specification-level sample of expected Python
  * output.
  */
class PyTreeToolsTest extends AnyFlatSpec with Matchers {

  private val tools: PyTreeTools = new PyTreeTools.PyTreeToolsImpl

  private val tq = "\"\"\""

  private def docs(prefix: String): Docs =
    Docs(prefix = Some(DocComment(raw = prefix, cleaned = prefix)), suffix = None)

  private def docsWithSuffix(prefix: String, suffix: String): Docs =
    Docs(
      prefix = Some(DocComment(raw = prefix, cleaned = prefix)),
      suffix = Some(DocComment(raw = suffix, cleaned = suffix)),
    )

  "PyTreeTools.renderClassDocstring" should "return empty string when no docs" in {
    tools.renderClassDocstring(Docs.empty, Seq.empty, "    ") shouldBe ""
  }

  it should "emit type-only docstring when only type doc is present" in {
    val result = tools.renderClassDocstring(docs("Paged doc results."), Seq.empty, "    ")
    result shouldBe s"    ${tq}Paged doc results.\n    ${tq}"
  }

  it should "emit Attributes-only docstring when only field docs are present" in {
    val fieldDocs = Seq("total" -> docs("total item count"))
    val result    = tools.renderClassDocstring(Docs.empty, fieldDocs, "    ")
    result should include("Attributes:")
    result should include("total: total item count")
    result should not include "Paged doc results."
  }

  it should "emit type + Attributes section for type with field docs" in {
    val typeDocs = docs("A simple item with field-level docs.")
    val fieldDocs = Seq(
      "name"  -> docs("Display name of the item."),
      "price" -> docsWithSuffix("Unit price in store currency.", "never negative"),
    )
    val result = tools.renderClassDocstring(typeDocs, fieldDocs, "    ")

    result should include("A simple item with field-level docs.")
    result should include("Attributes:")
    result should include("name: Display name of the item.")
    result should include("price: Unit price in store currency.")
    result should include("never negative")
  }

  it should "escape triple-quotes in body text" in {
    val body   = s"Has $tq inside"
    val result = tools.renderClassDocstring(docs(body), Seq.empty, "    ")
    result should include("\\\"\\\"\\\"")
    result should not include tq + " inside"
  }

  "PyTreeTools.renderMethodDocstring" should "return empty string when no docs" in {
    tools.renderMethodDocstring(Docs.empty, "        ") shouldBe ""
  }

  it should "emit single-line docstring for one-line method doc" in {
    val result = tools.renderMethodDocstring(docs("Create an item."), "        ")
    result shouldBe s"        ${tq}Create an item.${tq}"
  }

  it should "emit multi-line docstring for multi-line method doc" in {
    val result = tools.renderMethodDocstring(docs("Line one.\nLine two."), "        ")
    result should include("Line one.")
    result should include("Line two.")
    result should startWith(s"        $tq")
    result should endWith(s"        $tq")
  }
}
