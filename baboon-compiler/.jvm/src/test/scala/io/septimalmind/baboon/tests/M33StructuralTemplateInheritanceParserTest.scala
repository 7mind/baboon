package io.septimalmind.baboon.tests

import fastparse.*
import io.septimalmind.baboon.parser.ParserContext
import io.septimalmind.baboon.parser.model.{FSPath, RawDto, RawDtoMember, RawTypeName, RawTypeRef}
import izumi.fundamentals.collections.nonempty.{NEList, NEString}
import org.scalatest.wordspec.AnyWordSpec

/** Parser-only tests for PR-33.1: optional `[…]` type-argument list on `+` / `-` / `^`
  * structural-composition arms inside a `data` body.
  *
  * Verifies that:
  * - `+ Foo[i32]` parses to `ParentDef(ScopedRef("Foo"), args = Some(NEList(Simple(i32))))`.
  * - `- Foo[str]` parses to `UnparentDef(ScopedRef("Foo"), args = Some(NEList(Simple(str))))`.
  * - `^ Foo[i32]` parses to `IntersectionDef(ScopedRef("Foo"), args = Some(NEList(Simple(i32))))`.
  * - `+ ns.Foo[i32]` (cross-namespace) preserves the prefix in the ScopedRef.
  * - `+ Foo` (legacy, no args) parses to `ParentDef(ScopedRef("Foo"), args = None)` — backward compat.
  * - Mixed `+ Foo; + Bar[i32]; + Baz` arms in the same DTO body all parse correctly.
  * - `+ Foo[]` (empty args) is rejected by the parser (NEList requires min=1).
  *
  * Does NOT exercise the typer. Typer-side lowering of `args.isDefined` arms is PR-33.2.
  */
final class M33StructuralTemplateInheritanceParserTest extends AnyWordSpec {

  private val dummyPath = FSPath.parse(NEString.unsafeFrom("m33-structural-template-inheritance-parser-test.baboon"))

  private def ctx(input: String): ParserContext = ParserContext(dummyPath, input)

  private def parseDto(source: String): RawDto = {
    val c = ctx(source)
    fastparse.parse(c.content, c.defDto.dtoEnclosed(_)) match {
      case Parsed.Success(v, idx) =>
        assert(idx == source.length, s"parser did not consume all input; left: [${source.drop(idx)}]")
        v
      case f: Parsed.Failure =>
        fail(s"expected parse success, got failure: ${f.msg}")
    }
  }

  private def assertDtoFails(source: String): Unit = {
    val c = ctx(source)
    fastparse.parse(c.content, c.defDto.dtoEnclosed(_)) match {
      case _: Parsed.Failure => ()
      case Parsed.Success(v, idx) =>
        fail(s"expected parse failure for [$source], but succeeded with $v (consumed $idx chars)")
    }
  }

  "M33 structural template inheritance parser (PR-33.1)" should {

    // ─── positive: ParentDef with type args ─────────────────────────────────

    "parse `+ Foo[i32]` as ParentDef with args = Some(NEList(Simple(i32)))" in {
      val dto = parseDto("data X { + Foo[i32] }")
      assert(dto.members.size == 1)
      dto.members.head match {
        case RawDtoMember.ParentDef(parent, _, args) =>
          assert(parent.path.toList.map(_.name) == List("Foo"))
          assert(args == Some(NEList(RawTypeRef.Simple(RawTypeName("i32"), Nil))))
        case other =>
          fail(s"expected ParentDef, got $other")
      }
    }

    // ─── positive: UnparentDef with type args ────────────────────────────────

    "parse `- Foo[str]` as UnparentDef with args = Some(NEList(Simple(str)))" in {
      val dto = parseDto("data X { - Foo[str] }")
      assert(dto.members.size == 1)
      dto.members.head match {
        case RawDtoMember.UnparentDef(parent, _, args) =>
          assert(parent.path.toList.map(_.name) == List("Foo"))
          assert(args == Some(NEList(RawTypeRef.Simple(RawTypeName("str"), Nil))))
        case other =>
          fail(s"expected UnparentDef, got $other")
      }
    }

    // ─── positive: IntersectionDef with type args ────────────────────────────

    "parse `^ Foo[i32]` as IntersectionDef with args = Some(NEList(Simple(i32)))" in {
      val dto = parseDto("data X { ^ Foo[i32] }")
      assert(dto.members.size == 1)
      dto.members.head match {
        case RawDtoMember.IntersectionDef(parent, _, args) =>
          assert(parent.path.toList.map(_.name) == List("Foo"))
          assert(args == Some(NEList(RawTypeRef.Simple(RawTypeName("i32"), Nil))))
        case other =>
          fail(s"expected IntersectionDef, got $other")
      }
    }

    // ─── positive: cross-namespace head ─────────────────────────────────────

    "parse `+ ns.Foo[i32]` as ParentDef with two-segment ref and args = Some(NEList(Simple(i32)))" in {
      val dto = parseDto("data X { + ns.Foo[i32] }")
      assert(dto.members.size == 1)
      dto.members.head match {
        case RawDtoMember.ParentDef(parent, _, args) =>
          assert(parent.path.toList.map(_.name) == List("ns", "Foo"))
          assert(args == Some(NEList(RawTypeRef.Simple(RawTypeName("i32"), Nil))))
        case other =>
          fail(s"expected ParentDef with two-segment ref, got $other")
      }
    }

    // ─── positive regression: legacy `+ Foo` (no args) ──────────────────────

    "parse `+ Foo` (no type args) as ParentDef with args = None — legacy backward compat" in {
      val dto = parseDto("data X { + Foo }")
      assert(dto.members.size == 1)
      dto.members.head match {
        case RawDtoMember.ParentDef(parent, _, args) =>
          assert(parent.path.toList.map(_.name) == List("Foo"))
          assert(args == None)
        case other =>
          fail(s"expected ParentDef with args=None, got $other")
      }
    }

    // ─── positive: mixed arms in same DTO body ───────────────────────────────

    "parse mixed `+ Foo; + Bar[i32]; + Baz` — all three arms in same DTO body" in {
      val source =
        """|data X {
           |  + Foo
           |  + Bar[i32]
           |  + Baz
           |}""".stripMargin
      val dto = parseDto(source)
      assert(dto.members.size == 3)

      dto.members(0) match {
        case RawDtoMember.ParentDef(parent, _, args) =>
          assert(parent.path.toList.map(_.name) == List("Foo"))
          assert(args == None)
        case other => fail(s"expected ParentDef(Foo, None) at index 0, got $other")
      }

      dto.members(1) match {
        case RawDtoMember.ParentDef(parent, _, args) =>
          assert(parent.path.toList.map(_.name) == List("Bar"))
          assert(args == Some(NEList(RawTypeRef.Simple(RawTypeName("i32"), Nil))))
        case other => fail(s"expected ParentDef(Bar, Some([i32])) at index 1, got $other")
      }

      dto.members(2) match {
        case RawDtoMember.ParentDef(parent, _, args) =>
          assert(parent.path.toList.map(_.name) == List("Baz"))
          assert(args == None)
        case other => fail(s"expected ParentDef(Baz, None) at index 2, got $other")
      }
    }

    // ─── negative: empty args `+ Foo[]` rejected ────────────────────────────

    "reject `+ Foo[]` (empty type-arg list) — the empty-bracket form is not accepted by the DTO body grammar" in {
      // NOTE: the rejection does NOT fire from the NEList min=1 contract inside parentDef.
      // parentDef = "+" ~ nonGenericTypeRef ~ typeParams.?; when typeParams fails on `[]`
      // the `.?` swallows the failure and parentDef succeeds with (Foo, None), leaving `[]`
      // unconsumed. The outer dtoEnclosed then fails because `]` cannot start a new dtoMember.
      // See D01 fix: the micro-test below pins the min=1 contract on parentDef directly.
      assertDtoFails("data X { + Foo[] }")
    }

    "parentDef alone rejects `+ Foo[]` — typeParams NEList min=1 contract" in {
      // Calls parentDef directly so the failure is unambiguously attributed to the
      // rep(min=1) inside typeParams, not to a downstream dtoMember-sequence rejection.
      val c = ctx("+ Foo[]")
      fastparse.parse(c.content, c.defDto.parentDef(_)) match {
        case Parsed.Success((_, args), idx) =>
          // parentDef may succeed with args=None and leave `[]` unconsumed; idx must
          // be < input length for the assertion to hold (i.e. `[]` was NOT parsed as args).
          assert(args.isEmpty, s"expected args=None (typeParams skipped `[]`), got args=$args")
          assert(idx < "+ Foo[]".length, s"parentDef consumed `[]` as typeParams — min=1 contract broken (idx=$idx)")
        case _: Parsed.Failure =>
          // A hard failure is also acceptable — it means min=1 is enforced before the `.?` escape.
          ()
      }
    }

    // ─── positive: mixed `+`, `-`, `^` arms with args in same DTO body ──────

    "parse mixed `+ Foo[i32]; - Bar[str]; ^ Baz[i32]` — all three operators with args in same body" in {
      val source =
        """|data X {
           |  + Foo[i32]
           |  - Bar[str]
           |  ^ Baz[i32]
           |}""".stripMargin
      val dto = parseDto(source)
      assert(dto.members.size == 3)

      dto.members(0) match {
        case RawDtoMember.ParentDef(parent, _, args) =>
          assert(parent.path.toList.map(_.name) == List("Foo"))
          assert(args == Some(NEList(RawTypeRef.Simple(RawTypeName("i32"), Nil))))
        case other => fail(s"expected ParentDef(Foo, Some([i32])) at index 0, got $other")
      }

      dto.members(1) match {
        case RawDtoMember.UnparentDef(parent, _, args) =>
          assert(parent.path.toList.map(_.name) == List("Bar"))
          assert(args == Some(NEList(RawTypeRef.Simple(RawTypeName("str"), Nil))))
        case other => fail(s"expected UnparentDef(Bar, Some([str])) at index 1, got $other")
      }

      dto.members(2) match {
        case RawDtoMember.IntersectionDef(parent, _, args) =>
          assert(parent.path.toList.map(_.name) == List("Baz"))
          assert(args == Some(NEList(RawTypeRef.Simple(RawTypeName("i32"), Nil))))
        case other => fail(s"expected IntersectionDef(Baz, Some([i32])) at index 2, got $other")
      }
    }

    // ─── positive: nested template arg ──────────────────────────────────────

    "parse `+ Foo[Bar[i32]]` as ParentDef with nested Constructor arg" in {
      val dto = parseDto("data X { + Foo[Bar[i32]] }")
      assert(dto.members.size == 1)
      dto.members.head match {
        case RawDtoMember.ParentDef(parent, _, args) =>
          assert(parent.path.toList.map(_.name) == List("Foo"))
          val expectedArg = RawTypeRef.Constructor(
            RawTypeName("Bar"),
            NEList(RawTypeRef.Simple(RawTypeName("i32"), Nil)),
            Nil
          )
          assert(args == Some(NEList(expectedArg)))
        case other =>
          fail(s"expected ParentDef with nested Constructor arg, got $other")
      }
    }

    // ─── cross-line whitespace binding: `[…]` on next line binds as args ────

    "parse `+ Foo` with `[i32]` on the next line — cross-line binding tripwire (PR-33.1-D04)" in {
      // BaboonWhitespace consumes newlines, so typeParams.? will silently bind `[i32]`
      // even when it appears on a separate line. This is harmless today because no other
      // dtoMember alternative starts with `[`. This test pins the behaviour: if a future
      // grammar change tightens the whitespace contract, this test will fail and force a
      // deliberate decision. See D04 comment in DefDto.scala.
      val source =
        """|data X {
           |  + Foo
           |  [i32]
           |}""".stripMargin
      val dto = parseDto(source)
      assert(dto.members.size == 1)
      dto.members.head match {
        case RawDtoMember.ParentDef(parent, _, args) =>
          assert(parent.path.toList.map(_.name) == List("Foo"))
          assert(args == Some(NEList(RawTypeRef.Simple(RawTypeName("i32"), Nil))))
        case other =>
          fail(s"expected ParentDef(Foo, Some([i32])) for cross-line binding, got $other")
      }
    }

  }
}
