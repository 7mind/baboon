package io.septimalmind.baboon.tests

import io.septimalmind.baboon.parser.model.{InputPointer, RawNodeMeta, RawTypeName}
import io.septimalmind.baboon.parser.model.issues.IssuePrinter
import io.septimalmind.baboon.parser.model.issues.TyperIssue
import io.septimalmind.baboon.parser.model.issues.TyperIssue.AdtDeltaConflictDetail
import io.septimalmind.baboon.typer.model.{Owner, Pkg, TypeId, TypeName}
import izumi.fundamentals.collections.nonempty.NEList
import org.scalatest.wordspec.AnyWordSpec

/** Unit test for T163 (G27): the `AdtDeltaConflict` TyperIssue + its sealed `detail` enum.
  *
  * DEFINE + WIRE only — no detection logic. This constructs each of the six detail variants,
  * renders the issue via the existing `IssuePrinter` path, and asserts the rendered message is
  * non-empty and actionable (names the offending branch and/or the host ADT).
  */
class T163AdtDeltaConflictIssueTest extends AnyWordSpec {

  private val adtId: TypeId.User =
    TypeId.User(Pkg(NEList("my", "pkg")), Owner.Toplevel, TypeName("Foo"))

  private val meta: RawNodeMeta = RawNodeMeta(InputPointer.Undefined)

  private val branchName = "Bar"
  private val branch     = RawTypeName(branchName)

  private def render(detail: AdtDeltaConflictDetail): String =
    IssuePrinter[TyperIssue.AdtDeltaConflict].stringify(TyperIssue.AdtDeltaConflict(adtId, detail, meta))

  private def assertActionable(rendered: String, mustMention: String*): Unit = {
    assert(rendered.trim.nonEmpty, s"rendered message must be non-empty, got: '$rendered'")
    assert(rendered.contains(adtId.name.name), s"rendered message must name the host ADT '${adtId.name.name}', got: '$rendered'")
    mustMention.foreach(token => assert(rendered.contains(token), s"rendered message must mention '$token', got: '$rendered'"))
  }

  "AdtDeltaConflict rendering" should {

    "render DropOfAbsent naming the branch and the ADT" in {
      assertActionable(render(AdtDeltaConflictDetail.DropOfAbsent(branch)), branchName)
    }

    "render KeepDropSame naming the branch and the ADT" in {
      assertActionable(render(AdtDeltaConflictDetail.KeepDropSame(branch)), branchName)
    }

    "render KeepRedefineSame naming the branch and the ADT" in {
      assertActionable(render(AdtDeltaConflictDetail.KeepRedefineSame(branch)), branchName)
    }

    "render DropRedefineSame naming the branch and the ADT" in {
      assertActionable(render(AdtDeltaConflictDetail.DropRedefineSame(branch)), branchName)
    }

    "render MissingImportHeader naming the ADT and the required import" in {
      assertActionable(render(AdtDeltaConflictDetail.MissingImportHeader), "import")
    }

    "render KeepOfAbsent naming the branch and the ADT" in {
      assertActionable(render(AdtDeltaConflictDetail.KeepOfAbsent(branch)), branchName)
    }
  }
}
