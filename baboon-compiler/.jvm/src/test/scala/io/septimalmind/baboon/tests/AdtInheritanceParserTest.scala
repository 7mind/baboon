package io.septimalmind.baboon.tests

import fastparse.*
import io.septimalmind.baboon.parser.ParserContext
import io.septimalmind.baboon.parser.model.{FSPath, RawAdt, RawAdtMember, RawAdtMemberContract, RawAdtMemberDto}
import izumi.fundamentals.collections.nonempty.NEString
import org.scalatest.wordspec.AnyWordSpec

/** Parser-only tests for M20 BAB-A03: ADT branch inheritance syntax.
  *
  * Verifies that `+ X`, `- X`, `^ X`, `- X.Foo`, and `+ X.Foo` parse into the correct
  * `RawAdtMember` variants (3-variant shape: Include / Exclude / Intersect), each carrying the
  * unsplit `ScopedRef`. Does NOT exercise the typer (PR-63 scope).
  *
  * The All-vs-Branch decision is deferred to PR-63's typer-early pass. The parser emits the full
  * ref unsplit — e.g. `+ ErrorAtom.Foo` → `Include(ScopedRef([ErrorAtom, Foo]))` — so that
  * namespace-qualified ADTs (`+ pkg.sub.ErrorAtom`) are not misclassified at parse time.
  */
final class AdtInheritanceParserTest extends AnyWordSpec {

  private val dummyPath = FSPath.parse(NEString.unsafeFrom("adt-inheritance-parser-test.baboon"))

  private def ctx(input: String): ParserContext = ParserContext(dummyPath, input)

  private def parseAdt(source: String): RawAdt = {
    val c = ctx(source)
    fastparse.parse(c.content, c.defAdt.adtEnclosed(_)) match {
      case Parsed.Success(v, idx) =>
        assert(idx == source.length, s"parser did not consume all input; left: ${source.drop(idx)}")
        v
      case f: Parsed.Failure =>
        fail(s"expected parse success, got failure: ${f.msg}")
    }
  }

  private def assertParseAdtFails(source: String): Unit = {
    val c = ctx(source)
    fastparse.parse(c.content, c.defAdt.adtEnclosed(_)) match {
      case _: Parsed.Failure => ()
      case Parsed.Success(v, idx) =>
        fail(s"expected parse failure for [$source], but succeeded with $v (consumed $idx)")
    }
  }

  "ADT inheritance parser (M20 BAB-A03)" should {

    "parse `+ X` as Include with single-segment ref" in {
      val adt = parseAdt("adt MyAdt { + ErrorAtom }")
      assert(adt.members.size == 1)
      adt.members.head match {
        case RawAdtMember.Include(ref, _) =>
          assert(ref.path.toList.map(_.name) == List("ErrorAtom"))
        case other =>
          fail(s"expected Include, got $other")
      }
    }

    "parse `- X` as Exclude with single-segment ref" in {
      val adt = parseAdt("adt MyAdt { - ErrorAtom }")
      assert(adt.members.size == 1)
      adt.members.head match {
        case RawAdtMember.Exclude(ref, _) =>
          assert(ref.path.toList.map(_.name) == List("ErrorAtom"))
        case other =>
          fail(s"expected Exclude, got $other")
      }
    }

    "parse `^ X` as Intersect with single-segment ref" in {
      val adt = parseAdt("adt MyAdt { ^ ErrorAtom }")
      assert(adt.members.size == 1)
      adt.members.head match {
        case RawAdtMember.Intersect(ref, _) =>
          assert(ref.path.toList.map(_.name) == List("ErrorAtom"))
        case other =>
          fail(s"expected Intersect, got $other")
      }
    }

    "parse `- X.Foo` as Exclude with unsplit two-segment ref (D01 fix)" in {
      val adt = parseAdt("adt MyAdt { - ErrorAtom.Forbidden }")
      assert(adt.members.size == 1)
      adt.members.head match {
        case RawAdtMember.Exclude(ref, _) =>
          assert(ref.path.toList.map(_.name) == List("ErrorAtom", "Forbidden"))
        case other =>
          fail(s"expected Exclude with two-segment ref, got $other")
      }
    }

    "parse `+ X.Foo` as Include with unsplit two-segment ref (D01 fix)" in {
      val adt = parseAdt("adt MyAdt { + ErrorAtom.Forbidden }")
      assert(adt.members.size == 1)
      adt.members.head match {
        case RawAdtMember.Include(ref, _) =>
          assert(ref.path.toList.map(_.name) == List("ErrorAtom", "Forbidden"))
        case other =>
          fail(s"expected Include with two-segment ref, got $other")
      }
    }

    "allow inheritance arms interleaved with branch declarations (Q-M20-7)" in {
      val source =
        """|adt SomeError {
           |  + ErrorAtom
           |  data Foo { value: i32 }
           |  - ErrorAtom.Bar
           |}""".stripMargin
      val adt = parseAdt(source)
      assert(adt.members.size == 3)
      assert(adt.members(0).isInstanceOf[RawAdtMember.Include])
      assert(adt.members(1).isInstanceOf[RawAdtMemberDto])
      assert(adt.members(2).isInstanceOf[RawAdtMember.Exclude])
    }

    "parse multiple inheritance arms together" in {
      val source =
        """|adt X {
           |  + A
           |  - B
           |  ^ C
           |}""".stripMargin
      val adt = parseAdt(source)
      assert(adt.members.size == 3)
      assert(adt.members(0).isInstanceOf[RawAdtMember.Include])
      assert(adt.members(1).isInstanceOf[RawAdtMember.Exclude])
      assert(adt.members(2).isInstanceOf[RawAdtMember.Intersect])
    }

    "parse ADT with only branch declarations (regression — no inheritance arms)" in {
      val source =
        """|adt MyAdt {
           |  data Foo { x: i32 }
           |  data Bar { y: str }
           |}""".stripMargin
      val adt = parseAdt(source)
      assert(adt.members.size == 2)
      assert(adt.members.forall(_.isInstanceOf[RawAdtMemberDto]))
    }

    "parse `- X.Foo` with qualified ADT ref as Exclude with three-segment ref (D01 fix)" in {
      val adt = parseAdt("adt E { - Ns.MyAdt.SomeBranch }")
      assert(adt.members.size == 1)
      adt.members.head match {
        case RawAdtMember.Exclude(ref, _) =>
          assert(ref.path.toList.map(_.name) == List("Ns", "MyAdt", "SomeBranch"))
        case other =>
          fail(s"expected Exclude with three-segment ref, got $other")
      }
    }

    "parse `+ pkg.subpkg.X` as Include with three-segment ref for namespace-qualified ADT (D01 invariant)" in {
      // This test pins the D01 fix: a namespace-qualified ADT reference must NOT be split at
      // parse time. `+ pkg.subpkg.X` must produce Include(ScopedRef([pkg, subpkg, X])), not
      // IncludeBranch(adtRef=[pkg, subpkg], branchName=X). The All-vs-Branch decision is
      // deferred to PR-63's typer-early pass which has full scope-resolution context.
      val adt = parseAdt("adt E { + pkg.subpkg.X }")
      assert(adt.members.size == 1)
      adt.members.head match {
        case RawAdtMember.Include(ref, _) =>
          assert(ref.path.toList.map(_.name) == List("pkg", "subpkg", "X"))
        case other =>
          fail(s"expected Include with three-segment ref, got $other")
      }
    }

    "allow Intersect interleaved with branch declarations (Q-M20-7)" in {
      // Verifies that `^` arms may appear anywhere among `data` branch declarations, not only at
      // the top or bottom. Members must appear in declaration order.
      val source =
        """|adt X {
           |  data Foo { x: i32 }
           |  ^ Y
           |  data Bar { y: str }
           |}""".stripMargin
      val adt = parseAdt(source)
      assert(adt.members.size == 3)
      assert(adt.members(0).isInstanceOf[RawAdtMemberDto])
      assert(adt.members(0).asInstanceOf[RawAdtMemberDto].dto.name.name == "Foo")
      adt.members(1) match {
        case RawAdtMember.Intersect(ref, _) =>
          assert(ref.path.toList.map(_.name) == List("Y"))
        case other =>
          fail(s"expected Intersect(Y) as second member, got $other")
      }
      assert(adt.members(2).isInstanceOf[RawAdtMemberDto])
      assert(adt.members(2).asInstanceOf[RawAdtMemberDto].dto.name.name == "Bar")
    }

    "parse all three inheritance operators plus a qualified Exclude in one ADT (regression coverage)" in {
      // Regression guard for multiple mixed inheritance arms: Include, Exclude, Intersect, and a
      // two-segment Exclude must all survive in declaration order.
      val source =
        """|adt X {
           |  + Y
           |  - Z
           |  ^ W
           |  - Q.SomeBranch
           |}""".stripMargin
      val adt = parseAdt(source)
      assert(adt.members.size == 4)
      adt.members(0) match {
        case RawAdtMember.Include(ref, _) =>
          assert(ref.path.toList.map(_.name) == List("Y"))
        case other => fail(s"expected Include(Y), got $other")
      }
      adt.members(1) match {
        case RawAdtMember.Exclude(ref, _) =>
          assert(ref.path.toList.map(_.name) == List("Z"))
        case other => fail(s"expected Exclude(Z), got $other")
      }
      adt.members(2) match {
        case RawAdtMember.Intersect(ref, _) =>
          assert(ref.path.toList.map(_.name) == List("W"))
        case other => fail(s"expected Intersect(W), got $other")
      }
      adt.members(3) match {
        case RawAdtMember.Exclude(ref, _) =>
          assert(ref.path.toList.map(_.name) == List("Q", "SomeBranch"))
        case other => fail(s"expected Exclude(Q.SomeBranch), got $other")
      }
    }

    "reject `+ }` (Include with no ref) as a parse error" in {
      // `+` without a following identifier ref must not parse successfully. FastParse backtracks
      // past `adtIncludeDef` and the `adtEnclosed` struct fails because `}` cannot follow the
      // members without consuming the dangling `+`.
      assertParseAdtFails("adt X { + }")
    }

    "reject `+ .Foo` (leading dot in ref) as a parse error" in {
      // A ref starting with `.` is not a valid `symbolSeq` — each segment must begin with a
      // letter or `_`. The parser must fail rather than partially consuming `+` alone.
      assertParseAdtFails("adt X { + .Foo }")
    }

  }
}
