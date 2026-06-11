package io.septimalmind.baboon.tests

import fastparse.*
import io.septimalmind.baboon.parser.ParserContext
import io.septimalmind.baboon.parser.model.{ExtractionKind, FSPath, RawAdt, RawDtoMember, RawIdentifier, RawTypeName}
import izumi.fundamentals.collections.nonempty.NEString
import org.scalatest.wordspec.AnyWordSpec

/** Parser-only tests for T37: `has (mirror|contract) <Name>` extraction clauses in dto-shaped and
  * adt-level bodies.
  *
  * Acceptance criteria:
  * - Positive: template `data` host with `has mirror B`, with `has contract B`, multiple clauses
  *   with distinct names, adt-level clause, an `id` body containing a `has` clause, clause
  *   interleaved with fields/parents.
  * - Negative/back-compat: fields named `has` and `mirror` still parse as fields; `has contract`
  *   without a name fails; `has frobnicate B` fails; `has mirror foo.B` (scoped) fails.
  */
final class T37ExtractionClauseParserTest extends AnyWordSpec {

  private val dummyPath = FSPath.parse(NEString.unsafeFrom("t37-extraction-clause-parser-test.baboon"))

  private def ctx(input: String): ParserContext = ParserContext(dummyPath, input)

  private def parseDto(source: String) = {
    val c = ctx(source)
    fastparse.parse(c.content, c.defDto.dtoEnclosed(_)) match {
      case Parsed.Success(v, idx) =>
        assert(idx == source.length, s"parser did not consume all input; left: [${source.drop(idx)}]")
        v
      case f: Parsed.Failure =>
        fail(s"expected parse success, got failure: ${f.msg}")
    }
  }

  private def parseId(source: String): RawIdentifier = {
    val c = ctx(source)
    fastparse.parse(c.content, c.defDto.identifierEnclosed(_)) match {
      case Parsed.Success(v, idx) =>
        assert(idx == source.length, s"parser did not consume all input; left: [${source.drop(idx)}]")
        v
      case f: Parsed.Failure =>
        fail(s"expected parse success, got failure: ${f.msg}")
    }
  }

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

  private def assertDtoFails(source: String): Unit = {
    val c = ctx(source)
    fastparse.parse(c.content, c.defDto.dtoEnclosed(_)) match {
      case _: Parsed.Failure => ()
      case Parsed.Success(v, idx) =>
        fail(s"expected parse failure for [$source], but succeeded with $v (consumed $idx chars)")
    }
  }

  private def assertAdtFails(source: String): Unit = {
    val c = ctx(source)
    fastparse.parse(c.content, c.defAdt.adtEnclosed(_)) match {
      case _: Parsed.Failure => ()
      case Parsed.Success(v, idx) =>
        fail(s"expected parse failure for [$source], but succeeded with $v (consumed $idx chars)")
    }
  }

  "T37 extraction clause parser" should {

    // ─── positive: data body with has mirror ────────────────────────────────

    "parse `has mirror B` in a data body as ExtractionDef(Mirror, B)" in {
      val dto = parseDto("data X { has mirror B }")
      assert(dto.members.size == 1)
      dto.members.head match {
        case RawDtoMember.ExtractionDef(kind, name, _) =>
          assert(kind == ExtractionKind.Mirror)
          assert(name == RawTypeName("B"))
        case other =>
          fail(s"expected ExtractionDef(Mirror, B), got $other")
      }
    }

    // ─── positive: data body with has contract ───────────────────────────────

    "parse `has contract B` in a data body as ExtractionDef(Contract, B)" in {
      val dto = parseDto("data X { has contract B }")
      assert(dto.members.size == 1)
      dto.members.head match {
        case RawDtoMember.ExtractionDef(kind, name, _) =>
          assert(kind == ExtractionKind.Contract)
          assert(name == RawTypeName("B"))
        case other =>
          fail(s"expected ExtractionDef(Contract, B), got $other")
      }
    }

    // ─── positive: multiple extraction clauses with distinct names ───────────

    "parse multiple `has` clauses with distinct names in a data body" in {
      val source =
        """|data X {
           |  has mirror Foo
           |  has contract Bar
           |}""".stripMargin
      val dto = parseDto(source)
      assert(dto.members.size == 2)

      dto.members(0) match {
        case RawDtoMember.ExtractionDef(ExtractionKind.Mirror, name, _) =>
          assert(name == RawTypeName("Foo"))
        case other => fail(s"expected ExtractionDef(Mirror, Foo) at index 0, got $other")
      }

      dto.members(1) match {
        case RawDtoMember.ExtractionDef(ExtractionKind.Contract, name, _) =>
          assert(name == RawTypeName("Bar"))
        case other => fail(s"expected ExtractionDef(Contract, Bar) at index 1, got $other")
      }
    }

    // ─── positive: extraction clause interleaved with fields ─────────────────

    "parse extraction clause interleaved with fields and parent refs" in {
      val source =
        """|data X {
           |  f1: i32
           |  has mirror SomeTrait
           |  + SomeParent
           |  has contract AnotherContract
           |}""".stripMargin
      val dto = parseDto(source)
      assert(dto.members.size == 4)

      dto.members(0) match {
        case RawDtoMember.FieldDef(field, _) => assert(field.name.name == "f1")
        case other => fail(s"expected FieldDef(f1) at index 0, got $other")
      }
      dto.members(1) match {
        case RawDtoMember.ExtractionDef(ExtractionKind.Mirror, name, _) => assert(name.name == "SomeTrait")
        case other => fail(s"expected ExtractionDef(Mirror, SomeTrait) at index 1, got $other")
      }
      dto.members(2) match {
        case RawDtoMember.ParentDef(parent, _, _) => assert(parent.path.last.name == "SomeParent")
        case other => fail(s"expected ParentDef(SomeParent) at index 2, got $other")
      }
      dto.members(3) match {
        case RawDtoMember.ExtractionDef(ExtractionKind.Contract, name, _) => assert(name.name == "AnotherContract")
        case other => fail(s"expected ExtractionDef(Contract, AnotherContract) at index 3, got $other")
      }
    }

    // ─── positive: id body containing a has clause (PARSES — host validity is typer's call) ───

    "parse `has mirror B` inside an `id` body — host validity is the typer's call" in {
      val ident = parseId("id X { has mirror B }")
      assert(ident.members.size == 1)
      ident.members.head match {
        case RawDtoMember.ExtractionDef(ExtractionKind.Mirror, name, _) =>
          assert(name == RawTypeName("B"))
        case other =>
          fail(s"expected ExtractionDef(Mirror, B) in id body, got $other")
      }
    }

    // ─── positive: adt-level has clause ─────────────────────────────────────

    "parse `has mirror B` at the adt level — collected in RawAdt.extractions" in {
      val source =
        """|adt X {
           |  has mirror B
           |  data Branch {}
           |}""".stripMargin
      val adt = parseAdt(source)
      assert(adt.extractions.size == 1)
      val e = adt.extractions.head
      assert(e.kind == ExtractionKind.Mirror)
      assert(e.name == RawTypeName("B"))
      // The adt also has one branch
      assert(adt.members.size == 1)
    }

    "parse `has contract C` at the adt level" in {
      val source =
        """|adt X {
           |  has contract C
           |  data Branch {}
           |}""".stripMargin
      val adt = parseAdt(source)
      assert(adt.extractions.size == 1)
      assert(adt.extractions.head.kind == ExtractionKind.Contract)
      assert(adt.extractions.head.name == RawTypeName("C"))
    }

    // ─── back-compat: field named `has` still parses as a field ─────────────

    "field named `has` still parses as a plain FieldDef — back-compat" in {
      val dto = parseDto("data X { has: i32 }")
      assert(dto.members.size == 1)
      dto.members.head match {
        case RawDtoMember.FieldDef(field, _) =>
          assert(field.name.name == "has")
        case other =>
          fail(s"expected FieldDef with name=has, got $other")
      }
    }

    // ─── back-compat: field named `mirror` still parses as a field ──────────

    "field named `mirror` still parses as a plain FieldDef — back-compat" in {
      val dto = parseDto("data X { mirror: i32 }")
      assert(dto.members.size == 1)
      dto.members.head match {
        case RawDtoMember.FieldDef(field, _) =>
          assert(field.name.name == "mirror")
        case other =>
          fail(s"expected FieldDef with name=mirror, got $other")
      }
    }

    // ─── back-compat: field named `contract` still parses as a field ─────────

    "field named `contract` still parses as a plain FieldDef — back-compat" in {
      val dto = parseDto("data X { contract: i32 }")
      assert(dto.members.size == 1)
      dto.members.head match {
        case RawDtoMember.FieldDef(field, _) =>
          assert(field.name.name == "contract")
        case other =>
          fail(s"expected FieldDef with name=contract, got $other")
      }
    }

    // ─── negative: `has contract` without a name fails ──────────────────────

    "reject `has contract` without a name — incomplete extraction clause" in {
      assertDtoFails("data X { has contract }")
    }

    // ─── negative: `has frobnicate B` fails — unknown kind ──────────────────

    "reject `has frobnicate B` — unknown extraction kind" in {
      // `has frobnicate B` — `frobnicate` is not `mirror` or `contract`, so the extraction
      // branch fails. The parser then backtracks and tries `fieldDef`: `has: ...` fails because
      // there is no `:` after `has`. The whole dtoMember fails, causing dtoEnclosed to fail.
      assertDtoFails("data X { has frobnicate B }")
    }

    // ─── negative: `has mirror foo.B` (scoped) fails ────────────────────────

    "reject `has mirror foo.B` — scoped identifier not accepted (bare only)" in {
      // `extractionDefCore` uses `idt.symbol` (bare, no dots) for the name.
      // `foo.B` will cause the parser to succeed with `foo` as the name, leaving `.B` unconsumed.
      // The outer `dtoEnclosed` then fails because `.B` cannot start a new dtoMember.
      assertDtoFails("data X { has mirror foo.B }")
    }

  }
}
