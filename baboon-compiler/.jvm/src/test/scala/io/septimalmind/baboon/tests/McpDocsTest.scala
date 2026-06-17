package io.septimalmind.baboon.tests

import io.septimalmind.baboon.translator.mcp.McpDocs
import io.septimalmind.baboon.typer.model.{DocComment, Docs}
import org.scalatest.wordspec.AnyWordSpec

/** Unit tests for [[McpDocs.flatten]].
  *
  * Verifies the normalisation contract documented on the object:
  *   - `Docs.empty` → `None`
  *   - prefix-only → `Some(cleaned)`
  *   - suffix-only → `Some(cleaned)`
  *   - both present → `Some` with blank-line separator
  *   - trailing whitespace stripped, leading/trailing blank lines trimmed
  *   - no comment-delimiter characters in result
  */
final class McpDocsTest extends AnyWordSpec {

  private def doc(cleaned: String): DocComment = DocComment(raw = cleaned, cleaned = cleaned)

  "McpDocs.flatten" should {

    "return None for Docs.empty" in {
      assert(McpDocs.flatten(Docs.empty) == None)
    }

    "return None when both prefix and suffix are None" in {
      assert(McpDocs.flatten(Docs(None, None)) == None)
    }

    "return Some(cleaned) for prefix-only Docs" in {
      val docs = Docs(prefix = Some(doc("Create a new item")), suffix = None)
      assert(McpDocs.flatten(docs) == Some("Create a new item"))
    }

    "return Some(cleaned) for suffix-only Docs" in {
      val docs = Docs(prefix = None, suffix = Some(doc("never negative")))
      assert(McpDocs.flatten(docs) == Some("never negative"))
    }

    "concatenate prefix and suffix with a blank line separator" in {
      val docs = Docs(
        prefix = Some(doc("Unit price in store currency")),
        suffix = Some(doc("never negative")),
      )
      assert(McpDocs.flatten(docs) == Some("Unit price in store currency\n\nnever negative"))
    }

    "strip trailing whitespace from lines" in {
      val docs = Docs(prefix = Some(doc("line one   \nline two  ")), suffix = None)
      assert(McpDocs.flatten(docs) == Some("line one\nline two"))
    }

    "trim leading and trailing blank lines from the result" in {
      val docs = Docs(prefix = Some(doc("\n\nsome text\n\n")), suffix = None)
      assert(McpDocs.flatten(docs) == Some("some text"))
    }

    "preserve internal newlines in multi-line docs" in {
      val docs = Docs(prefix = Some(doc("first line\nsecond line\nthird line")), suffix = None)
      assert(McpDocs.flatten(docs) == Some("first line\nsecond line\nthird line"))
    }

    "return None when cleaned text is whitespace-only after normalisation" in {
      val docs = Docs(prefix = Some(doc("   \n   \n   ")), suffix = None)
      assert(McpDocs.flatten(docs) == None)
    }

    "not emit any comment-delimiter characters" in {
      val result = McpDocs.flatten(
        Docs(
          prefix = Some(doc("A service for item management")),
          suffix = Some(doc("see also: createItem")),
        )
      )
      val text = result.getOrElse(fail("expected Some"))
      assert(!text.contains("/**"), "must not contain /**")
      assert(!text.contains("*/"), "must not contain */")
      assert(!text.contains("///"), "must not contain ///")
      assert(!text.contains("//!"), "must not contain //!")
      assert(!text.contains("'''"), "must not contain '''")
    }
  }
}
