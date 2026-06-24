package io.septimalmind.baboon.tests

import fastparse.*
import io.septimalmind.baboon.parser.ParserContext
import io.septimalmind.baboon.parser.model.{FSPath, RawAdt, RawAdtMember, RawAdtMemberDto, RawDtoMember, RawTypeName}
import izumi.fundamentals.collections.nonempty.NEString
import org.scalatest.wordspec.AnyWordSpec

/** Parser-only tests for T161: `keep *` / `keep A, B` / `drop X` ADT delta members parsed as
  * soft keywords inside an `adt` body (no-cut backtrack).
  *
  * Acceptance criteria:
  * - (a) `adt Foo { keep *; drop B3; data B4 { x: i32 } }` parses with members containing
  *   `Keep(None)`, `Drop(B3)`, and a `RawAdtMemberDto(B4)`.
  * - (b) `keep A, B` yields `Keep(Some([A, B]))`.
  * - (c) TRIPWIRE: an ADT branch named `keep` (`data keep { ... }`) and a DTO field named `drop`
  *   (`drop: i32`) still parse without error — proving the soft-keyword (no-cut) backtrack.
  */
final class T161AdtDeltaKeepDropParserTest extends AnyWordSpec {

  private val dummyPath = FSPath.parse(NEString.unsafeFrom("t161-adt-delta-keep-drop-parser-test.baboon"))

  private def ctx(input: String): ParserContext = ParserContext(dummyPath, input)

  private def parseAdt(source: String): RawAdt = {
    val c = ctx(source)
    fastparse.parse(c.content, c.defAdt.adtEnclosed(_)) match {
      case Parsed.Success(v, idx) =>
        assert(idx == source.length, s"parser did not consume all input; left: [${source.drop(idx)}]")
        v
      case f: Parsed.Failure =>
        fail(s"expected parse success, got failure: ${f.msg}")
    }
  }

  "T161 keep/drop ADT delta parser" should {

    // --- (a) wildcard keep + drop + a branch DTO ---

    "parse `keep *`, `drop B3`, and a `data B4` branch in one adt body" in {
      val source =
        """|adt Foo {
           |  keep *
           |  drop B3
           |  data B4 { x: i32 }
           |}""".stripMargin
      val adt = parseAdt(source)
      assert(adt.members.size == 3)

      adt.members(0) match {
        case RawAdtMember.Keep(branches, _) => assert(branches.isEmpty, s"expected Keep(None), got Keep($branches)")
        case other                          => fail(s"expected Keep(None) at index 0, got $other")
      }
      adt.members(1) match {
        case RawAdtMember.Drop(branch, _) => assert(branch == RawTypeName("B3"))
        case other                        => fail(s"expected Drop(B3) at index 1, got $other")
      }
      adt.members(2) match {
        case RawAdtMemberDto(dto, _) => assert(dto.name == RawTypeName("B4"))
        case other                   => fail(s"expected RawAdtMemberDto(B4) at index 2, got $other")
      }
    }

    // --- (b) selective keep A, B ---

    "parse `keep A, B` as Keep(Some([A, B]))" in {
      val source =
        """|adt Foo {
           |  keep A, B
           |  data B4 { x: i32 }
           |}""".stripMargin
      val adt = parseAdt(source)
      assert(adt.members.size == 2)
      adt.members(0) match {
        case RawAdtMember.Keep(Some(branches), _) =>
          assert(branches.toList == List(RawTypeName("A"), RawTypeName("B")))
        case other => fail(s"expected Keep(Some([A, B])) at index 0, got $other")
      }
    }

    "parse a single selective `keep A` as Keep(Some([A]))" in {
      val adt = parseAdt("adt Foo { keep A }")
      assert(adt.members.size == 1)
      adt.members.head match {
        case RawAdtMember.Keep(Some(branches), _) =>
          assert(branches.toList == List(RawTypeName("A")))
        case other => fail(s"expected Keep(Some([A])) got $other")
      }
    }

    // --- (c) TRIPWIRE: soft-keyword backtrack ---

    "TRIPWIRE: an ADT branch named `keep` with a field named `drop` still parses (no-cut backtrack)" in {
      // `data keep { ... }` is headed by `data`, so `adtKeepDef` (which starts at `kw.keep`)
      // never matches and the branch parses as an ordinary DTO named `keep`. A field named
      // `drop` inside it is unaffected -- keep/drop are only soft keywords at the adt-body level.
      val source =
        """|adt Foo {
           |  data keep { drop: i32 }
           |}""".stripMargin
      val adt = parseAdt(source)
      assert(adt.members.size == 1)
      adt.members.head match {
        case RawAdtMemberDto(dto, _) =>
          assert(dto.name == RawTypeName("keep"), s"expected branch named keep, got ${dto.name}")
          assert(dto.members.size == 1)
          dto.members.head match {
            case RawDtoMember.FieldDef(field, _) => assert(field.name.name == "drop")
            case other                           => fail(s"expected FieldDef(drop), got $other")
          }
        case other => fail(s"expected RawAdtMemberDto(keep), got $other")
      }
    }

    "TRIPWIRE: a branch named `drop` still parses as a DTO (no-cut backtrack)" in {
      val adt = parseAdt("adt Foo { data drop { x: i32 } }")
      assert(adt.members.size == 1)
      adt.members.head match {
        case RawAdtMemberDto(dto, _) => assert(dto.name == RawTypeName("drop"))
        case other                   => fail(s"expected RawAdtMemberDto(drop), got $other")
      }
    }

    // --- mixed: keep/drop coexisting with a `keep`-named branch ---

    "parse `keep *` delta alongside a branch literally named `keep`" in {
      val source =
        """|adt Foo {
           |  keep *
           |  data keep { x: i32 }
           |  drop Other
           |}""".stripMargin
      val adt = parseAdt(source)
      assert(adt.members.size == 3)
      adt.members(0) match {
        case RawAdtMember.Keep(None, _) => ()
        case other                      => fail(s"expected Keep(None) at index 0, got $other")
      }
      adt.members(1) match {
        case RawAdtMemberDto(dto, _) => assert(dto.name == RawTypeName("keep"))
        case other                   => fail(s"expected RawAdtMemberDto(keep) at index 1, got $other")
      }
      adt.members(2) match {
        case RawAdtMember.Drop(branch, _) => assert(branch == RawTypeName("Other"))
        case other                        => fail(s"expected Drop(Other) at index 2, got $other")
      }
    }
  }
}
