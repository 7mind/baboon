package io.septimalmind.baboon.tests

import fastparse.*
import io.septimalmind.baboon.parser.ParserContext
import io.septimalmind.baboon.parser.BaboonParser
import io.septimalmind.baboon.parser.model.issues.{BaboonIssue, ParserIssue}
import io.septimalmind.baboon.parser.model.{FSPath, RawAdt, RawAdtMemberDto, RawContent, RawContract, RawDocs, RawDomain, RawDtoMember, RawEnum, RawField, RawFieldName, RawNodeMeta, RawService, RawTLDef}
import izumi.functional.bio.Error2
import izumi.fundamentals.collections.nonempty.{NEList, NEString}
import izumi.reflect.TagKK
import org.scalatest.wordspec.AnyWordSpec

/** Parser-only tests for PR-30.2: doc-comment capture into RawNodeMeta.docs.
  *
  * Verifies:
  * - prefix doc on type, enum member, ADT-arm DTO, service method, field
  * - postfix `//!` on a field
  * - both prefix and suffix on the same field
  * - multi-line prefix docs, with and without `*` line prefix
  * - non-doc comments (`//`, `/* */`) NOT captured
  * - prefix doc on its own line between two declarations binds to FOLLOWING
  * - degenerate empty prefix-doc forms are silently dropped
  * - postfix `//!` does NOT bleed into the next field
  * - stacked prefix doc blocks produce ParserIssue.StackedDocComments
  * - existing fixtures with no docs continue to parse with RawDocs.empty
  */
final class DocCommentParserTest extends AnyWordSpec {

  private val dummyPath = FSPath.parse(NEString.unsafeFrom("doc-comment-parser-test.baboon"))

  private def ctx(input: String): ParserContext = ParserContext(dummyPath, input)

  private def parseModel(source: String): RawDomain = {
    val c = ctx(source)
    fastparse.parse(c.content, c.defModel.model(_)) match {
      case Parsed.Success(value, idx) =>
        assert(idx == source.length, s"parser left unconsumed input: [${source.drop(idx)}]")
        value
      case f: Parsed.Failure =>
        fail(s"expected parse success, got: ${f.msg}\nfor source:\n$source")
    }
  }

  private def findDto(content: RawContent, name: String): Option[(RawTLDef.DTO, RawNodeMeta)] = {
    content.defs.collectFirst {
      case d: RawTLDef.DTO if d.value.name.name == name => (d, d.value.meta)
    }
  }

  private def findEnum(content: RawContent, name: String): Option[RawEnum] = {
    content.defs.collectFirst {
      case d: RawTLDef.Enum if d.value.name.name == name => d.value
    }
  }

  private def findAdt(content: RawContent, name: String): Option[RawAdt] = {
    content.defs.collectFirst {
      case d: RawTLDef.ADT if d.value.name.name == name => d.value
    }
  }

  private def findService(content: RawContent, name: String): Option[RawService] = {
    content.defs.collectFirst {
      case d: RawTLDef.Service if d.value.name.name == name => d.value
    }
  }

  private def findContract(content: RawContent, name: String): Option[RawContract] = {
    content.defs.collectFirst {
      case d: RawTLDef.Contract if d.value.name.name == name => d.value
    }
  }

  private def fieldOf(dto: RawTLDef.DTO, fieldName: String): Option[(RawField, RawNodeMeta)] = {
    dto.value.members.collectFirst {
      case RawDtoMember.FieldDef(f, m) if f.name.name == fieldName => (f, m)
    }
  }

  // ─── prefix doc on type ──────────────────────────────────────────────────────

  "doc-comment parser (PR-30.2)" should {

    "capture single-line prefix doc on a top-level data declaration" in {
      val src =
        """model x.y
          |version "1"
          |/** the foo type */
          |data Foo { x: i32 }
          |""".stripMargin
      val m = parseModel(src)
      val (_, meta) = findDto(m.members, "Foo").getOrElse(fail("Foo not found"))
      assert(meta.docs.prefix.isDefined, s"expected prefix doc on Foo, got: ${meta.docs}")
      assert(meta.docs.prefix.get.raw.contains("the foo type"))
      assert(meta.docs.suffix.isEmpty)
    }

    "capture multi-line prefix doc with star line prefix" in {
      val src =
        """model x.y
          |version "1"
          |/**
          | * line one
          | * line two
          | */
          |data Foo { x: i32 }
          |""".stripMargin
      val m = parseModel(src)
      val (_, meta) = findDto(m.members, "Foo").getOrElse(fail("Foo not found"))
      val raw       = meta.docs.prefix.getOrElse(fail("expected doc")).raw
      assert(raw.contains("line one"))
      assert(raw.contains("line two"))
    }

    "capture multi-line prefix doc without star line prefix" in {
      val src =
        """model x.y
          |version "1"
          |/**
          |  text without star
          |  more text
          |  */
          |data Foo { x: i32 }
          |""".stripMargin
      val m = parseModel(src)
      val (_, meta) = findDto(m.members, "Foo").getOrElse(fail("Foo not found"))
      val raw       = meta.docs.prefix.getOrElse(fail("expected doc")).raw
      assert(raw.contains("text without star"))
      assert(raw.contains("more text"))
    }

    "capture prefix doc on field" in {
      val src =
        """model x.y
          |version "1"
          |data Foo {
          |  /** the x field */
          |  x: i32
          |}
          |""".stripMargin
      val m   = parseModel(src)
      val dto = findDto(m.members, "Foo").map(_._1).getOrElse(fail("Foo not found"))
      val (_, fmeta) = fieldOf(dto, "x").getOrElse(fail("field x not found"))
      assert(fmeta.docs.prefix.isDefined)
      assert(fmeta.docs.prefix.get.raw.contains("the x field"))
    }

    // Note: per spec §3.2 / §9, individual enum values are NOT a doc-bearing position
    // in the typed model. The parser captures them uniformly at this stage; the typer
    // (PR-30.3) will silently drop docs at non-position carriers. This test pins the
    // load-bearing parser-stage capture; the corresponding typer-stage drop is tested
    // in PR-30.3.
    "capture prefix doc on enum member" in {
      val src =
        """model x.y
          |version "1"
          |enum E {
          |  /** alpha branch */
          |  A
          |  B
          |}
          |""".stripMargin
      val m = parseModel(src)
      val e = findEnum(m.members, "E").getOrElse(fail("enum E not found"))
      val mA = e.members.find(_.value == "A").getOrElse(fail("member A"))
      assert(mA.meta.docs.prefix.isDefined, s"expected prefix doc on A, got ${mA.meta.docs}")
      assert(mA.meta.docs.prefix.get.raw.contains("alpha branch"))
    }

    "capture prefix doc on ADT branch DTO" in {
      val src =
        """model x.y
          |version "1"
          |adt A {
          |  /** the ok branch */
          |  data Ok { v: i32 }
          |  data Err { e: str }
          |}
          |""".stripMargin
      val m = parseModel(src)
      val adt = findAdt(m.members, "A").getOrElse(fail("adt A"))
      val ok  = adt.members.collectFirst { case RawAdtMemberDto(d, mt) if d.name.name == "Ok" => mt }.getOrElse(fail("Ok arm"))
      assert(ok.docs.prefix.isDefined, s"expected prefix doc on Ok, got: ${ok.docs}")
      assert(ok.docs.prefix.get.raw.contains("the ok branch"))
    }

    "capture prefix doc on service method" in {
      val src =
        """model x.y
          |version "1"
          |service S {
          |  /** fetch by key */
          |  def get (Key): Value
          |}
          |""".stripMargin
      val m = parseModel(src)
      val s = findService(m.members, "S").getOrElse(fail("service S"))
      val getM = s.defns.find(_.name == "get").getOrElse(fail("get method"))
      assert(getM.meta.docs.prefix.isDefined, s"expected prefix doc on get, got: ${getM.meta.docs}")
      assert(getM.meta.docs.prefix.get.raw.contains("fetch by key"))
    }

    // ─── postfix `//!` ─────────────────────────────────────────────────────────

    "capture postfix //! on a field" in {
      val src =
        """model x.y
          |version "1"
          |data Foo {
          |  x: i32  //! the x value
          |}
          |""".stripMargin
      val m   = parseModel(src)
      val dto = findDto(m.members, "Foo").map(_._1).getOrElse(fail("Foo"))
      val (_, fmeta) = fieldOf(dto, "x").getOrElse(fail("field x"))
      assert(fmeta.docs.suffix.isDefined, s"expected suffix doc, got ${fmeta.docs}")
      assert(fmeta.docs.suffix.get.raw.contains("the x value"))
    }

    "capture both prefix and postfix on the same field" in {
      val src =
        """model x.y
          |version "1"
          |data Foo {
          |  /** prefix doc */
          |  x: i32  //! suffix doc
          |}
          |""".stripMargin
      val m   = parseModel(src)
      val dto = findDto(m.members, "Foo").map(_._1).getOrElse(fail("Foo"))
      val (_, fmeta) = fieldOf(dto, "x").getOrElse(fail("field x"))
      assert(fmeta.docs.prefix.isDefined && fmeta.docs.suffix.isDefined)
      assert(fmeta.docs.prefix.get.raw.contains("prefix doc"))
      assert(fmeta.docs.suffix.get.raw.contains("suffix doc"))
    }

    "postfix //! does NOT bleed into the next field" in {
      val src =
        """model x.y
          |version "1"
          |data Foo {
          |  x: i32  //! x doc
          |  y: i32
          |}
          |""".stripMargin
      val m   = parseModel(src)
      val dto = findDto(m.members, "Foo").map(_._1).getOrElse(fail("Foo"))
      val (_, fx) = fieldOf(dto, "x").getOrElse(fail("x"))
      val (_, fy) = fieldOf(dto, "y").getOrElse(fail("y"))
      assert(fx.docs.suffix.isDefined && fx.docs.suffix.get.raw.contains("x doc"))
      assert(fy.docs.suffix.isEmpty, s"expected no suffix on y, got ${fy.docs}")
      assert(fy.docs.prefix.isEmpty, s"expected no prefix on y, got ${fy.docs}")
    }

    // ─── non-doc comments NOT captured ────────────────────────────────────────

    "plain // line comment is NOT captured" in {
      val src =
        """model x.y
          |version "1"
          |// just a regular comment
          |data Foo { x: i32 }
          |""".stripMargin
      val m = parseModel(src)
      val (_, meta) = findDto(m.members, "Foo").getOrElse(fail("Foo"))
      assert(meta.docs == RawDocs.empty)
    }

    "plain block comment is NOT captured" in {
      val src =
        """model x.y
          |version "1"
          |/* just a regular block comment */
          |data Foo { x: i32 }
          |""".stripMargin
      val m = parseModel(src)
      val (_, meta) = findDto(m.members, "Foo").getOrElse(fail("Foo"))
      assert(meta.docs == RawDocs.empty)
    }

    // ─── empty doc bodies silently dropped ────────────────────────────────────

    "empty prefix-doc body is silently dropped" in {
      val srcs = List(
        """model x.y
          |version "1"
          |/**/
          |data Foo { x: i32 }
          |""".stripMargin,
        """model x.y
          |version "1"
          |/** */
          |data Foo { x: i32 }
          |""".stripMargin,
        """model x.y
          |version "1"
          |/**
          |*/
          |data Foo { x: i32 }
          |""".stripMargin,
      )
      srcs.foreach { src =>
        val m = parseModel(src)
        val (_, meta) = findDto(m.members, "Foo").getOrElse(fail(s"Foo not found in:\n$src"))
        assert(meta.docs.prefix.isEmpty, s"expected empty doc dropped, got ${meta.docs} for:\n$src")
      }
    }

    "empty postfix //! body is silently dropped" in {
      val src =
        """model x.y
          |version "1"
          |data Foo {
          |  x: i32 //!
          |  y: i32 //!
          |}
          |""".stripMargin
      val m   = parseModel(src)
      val dto = findDto(m.members, "Foo").map(_._1).getOrElse(fail("Foo"))
      val (_, fx) = fieldOf(dto, "x").getOrElse(fail("x"))
      val (_, fy) = fieldOf(dto, "y").getOrElse(fail("y"))
      assert(fx.docs.suffix.isEmpty)
      assert(fy.docs.suffix.isEmpty)
    }

    // ─── doc on its own line between two declarations binds to following ─────

    "doc on a line by itself between two declarations binds to the FOLLOWING declaration" in {
      val src =
        """model x.y
          |version "1"
          |data First { a: i32 }
          |
          |/** this binds to Second */
          |data Second { b: i32 }
          |""".stripMargin
      val m = parseModel(src)
      val (_, mFirst)  = findDto(m.members, "First").getOrElse(fail("First"))
      val (_, mSecond) = findDto(m.members, "Second").getOrElse(fail("Second"))
      assert(mFirst.docs.prefix.isEmpty, s"unexpected prefix on First: ${mFirst.docs}")
      assert(mSecond.docs.prefix.isDefined, s"expected prefix on Second, got: ${mSecond.docs}")
      assert(mSecond.docs.prefix.get.raw.contains("this binds to Second"))
    }

    // ─── stacked prefix docs are a parser error ──────────────────────────────

    "stacked prefix docs surface as ParserIssue.StackedDocComments via the driver" in {
      val src =
        """model x.y
          |version "1"
          |/** first */
          |/** second */
          |data Foo { x: i32 }
          |""".stripMargin
      val resolver = new io.septimalmind.baboon.parser.BaboonInclusionResolver[Either] {
        def resolveInclude(inc: io.septimalmind.baboon.parser.model.RawInclude): Option[(FSPath, String)] = None
      }
      val parser = new BaboonParser.BaboonParserImpl[Either](resolver)
      val res    = parser.parse(BaboonParser.Input(dummyPath, src))
      res match {
        case Left(issues) =>
          val stacked = issues.toList.collectFirst { case BaboonIssue.Parser(p: ParserIssue.StackedDocComments) => p }
          assert(stacked.isDefined, s"expected StackedDocComments, got: ${issues.toList}")
        case Right(v) =>
          fail(s"expected parse failure, got success: $v")
      }
    }

    // ─── regression: existing fixtures with no docs are unaffected ────────────

    "non-doc-bearing source produces RawDocs.empty everywhere" in {
      val src =
        """model x.y
          |version "1"
          |data Foo {
          |  x: i32
          |  y: i32
          |}
          |""".stripMargin
      val m = parseModel(src)
      val (dto, meta) = findDto(m.members, "Foo").getOrElse(fail("Foo"))
      assert(meta.docs == RawDocs.empty)
      // Stronger assertion: empty must be the canonical (None, None) shape, not a
      // `Some(RawDocComment(""))` no-op. Catches a regression where capture returns
      // a sentinel empty-body doc rather than dropping it. [PR-30.2-D06]
      assert(meta.docs == RawDocs(None, None))
      assert(meta.docs.prefix.isEmpty && meta.docs.suffix.isEmpty)
      dto.value.members.foreach {
        case RawDtoMember.FieldDef(_, m) =>
          assert(m.docs == RawDocs.empty)
          assert(m.docs == RawDocs(None, None))
          assert(m.docs.prefix.isEmpty && m.docs.suffix.isEmpty)
        case _ => ()
      }
    }

    // ─── PR-30.2-D01 / D06 negative: orphan //! does not bind across newlines ───

    "orphan //! between declarations does not bind to either" in {
      // D01 across declaration boundaries: a `//!` on A's last field MUST NOT
      // bleed its binding to the following declaration B. The parse succeeds
      // here (the `//!` is syntactically valid on the field line), so the
      // no-binding assertions on B actually execute — this is the non-vacuous
      // check that the original between-fields test could not provide.
      val src =
        """model x.y
          |version "1"
          |data A {
          |  x: i32 //! field doc
          |}
          |data B {}
          |""".stripMargin
      val m = parseModel(src)
      val dtoA = findDto(m.members, "A").map(_._1).getOrElse(fail("A not found"))
      val (_, metaB) = findDto(m.members, "B").getOrElse(fail("B not found"))
      // The //! belongs to A.x and must not cross the declaration boundary.
      val (_, metaX) = fieldOf(dtoA, "x").getOrElse(fail("A.x not found"))
      assert(metaX.docs.suffix.isDefined, s"A.x must carry the //! suffix, got: ${metaX.docs}")
      assert(metaB.docs.prefix.isEmpty, s"//! must NOT bleed to B's prefix, got: ${metaB.docs}")
      assert(metaB.docs.suffix.isEmpty, s"B must have no suffix, got: ${metaB.docs}")
    }

    // ─── PR-30.2-D06: separator-only doc body silently drops ───────────────────

    "separator-only prefix-doc body is silently dropped" in {
      val src =
        """model x.y
          |version "1"
          |/**
          | *
          | */
          |data Foo { x: i32 }
          |""".stripMargin
      val m = parseModel(src)
      val (_, meta) = findDto(m.members, "Foo").getOrElse(fail("Foo not found"))
      assert(meta.docs.prefix.isEmpty, s"separator-only body must drop, got: ${meta.docs}")
    }

    // ─── PR-30.2-D06: prefix doc on contract and contract field ────────────────

    "capture prefix doc on a top-level contract declaration" in {
      val src =
        """model x.y
          |version "1"
          |/** A contract */
          |contract C { x: i32 }
          |""".stripMargin
      val m = parseModel(src)
      val c = findContract(m.members, "C").getOrElse(fail("contract C not found"))
      assert(c.meta.docs.prefix.isDefined, s"expected prefix doc on contract C, got: ${c.meta.docs}")
      assert(c.meta.docs.prefix.get.raw.contains("A contract"))
    }

    "capture prefix doc on a contract field" in {
      val src =
        """model x.y
          |version "1"
          |contract C {
          |  /** the x field of contract C */
          |  x: i32
          |}
          |""".stripMargin
      val m = parseModel(src)
      val c = findContract(m.members, "C").getOrElse(fail("contract C not found"))
      val (field, fmeta) = c.members.collectFirst {
        case RawDtoMember.FieldDef(f, mt) if f.name.name == "x" => (f, mt)
      }.getOrElse(fail("field x not found on contract C"))
      val _ = field
      assert(fmeta.docs.prefix.isDefined, s"expected prefix doc on contract C.x, got: ${fmeta.docs}")
      assert(fmeta.docs.prefix.get.raw.contains("the x field of contract C"))
    }

    // ─── PR-30.2-D03: stacked-doc detection across non-doc separators ──────────

    "stacked prefix doc blocks separated by a // line comment surface as StackedDocComments" in {
      val src =
        """model x.y
          |version "1"
          |/** first */
          |// stray separator
          |/** second */
          |data Foo { x: i32 }
          |""".stripMargin
      val resolver = new io.septimalmind.baboon.parser.BaboonInclusionResolver[Either] {
        def resolveInclude(inc: io.septimalmind.baboon.parser.model.RawInclude): Option[(FSPath, String)] = None
      }
      val parser = new BaboonParser.BaboonParserImpl[Either](resolver)
      val res    = parser.parse(BaboonParser.Input(dummyPath, src))
      res match {
        case Left(issues) =>
          val stacked = issues.toList.collectFirst { case BaboonIssue.Parser(p: ParserIssue.StackedDocComments) => p }
          assert(stacked.isDefined, s"expected StackedDocComments across // separator, got: ${issues.toList}")
        case Right(v) =>
          fail(s"expected parse failure (stacked docs across // separator), got success: $v")
      }
    }
  }
}
