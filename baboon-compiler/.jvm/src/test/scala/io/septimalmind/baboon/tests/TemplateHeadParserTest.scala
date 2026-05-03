package io.septimalmind.baboon.tests

import fastparse.*
import io.septimalmind.baboon.parser.ParserContext
import io.septimalmind.baboon.parser.model.{FSPath, RawAdt, RawAdtMemberDto, RawContract, RawDto, RawService, RawTypeName}
import izumi.fundamentals.collections.nonempty.NEString
import org.scalatest.wordspec.AnyWordSpec

/** Parser-only tests for PR-29.2: type-parameter head on `data` / `adt` / `contract` / `service`.
  *
  * Verifies that:
  * - Each of the four declaration kinds accepts an optional `[T1, T2, …]` clause between the name
  *   and the body, surfacing the type-param list as `typeParams` on the raw AST node.
  * - Declarations without the clause continue to produce `typeParams == Nil` (backward compat).
  * - `data X[] { … }` fails to parse (empty bracket clause forbidden).
  * - `id Foo[T] { … }` fails to parse (identifiers are not generifiable per spec §6.7).
  *
  * Does NOT exercise the typer. Template-body contents (field refs to `T`) are not validated here.
  */
final class TemplateHeadParserTest extends AnyWordSpec {

  private val dummyPath = FSPath.parse(NEString.unsafeFrom("template-head-parser-test.baboon"))

  private def ctx(input: String): ParserContext = ParserContext(dummyPath, input)

  // ─── helpers ─────────────────────────────────────────────────────────────────

  private def parseDto(source: String): RawDto = {
    val c = ctx(source)
    fastparse.parse(c.content, c.defDto.dtoEnclosed(_)) match {
      case Parsed.Success(v, idx) =>
        assert(idx == source.length, s"parser left unconsumed input: [${source.drop(idx)}]")
        v
      case f: Parsed.Failure =>
        fail(s"expected parse success for [$source], got: ${f.msg}")
    }
  }

  private def parseAdt(source: String): RawAdt = {
    val c = ctx(source)
    fastparse.parse(c.content, c.defAdt.adtEnclosed(_)) match {
      case Parsed.Success(v, idx) =>
        assert(idx == source.length, s"parser left unconsumed input: [${source.drop(idx)}]")
        v
      case f: Parsed.Failure =>
        fail(s"expected parse success for [$source], got: ${f.msg}")
    }
  }

  private def parseContract(source: String): RawContract = {
    val c = ctx(source)
    fastparse.parse(c.content, c.defContract.contractEnclosed(_)) match {
      case Parsed.Success(v, idx) =>
        assert(idx == source.length, s"parser left unconsumed input: [${source.drop(idx)}]")
        v
      case f: Parsed.Failure =>
        fail(s"expected parse success for [$source], got: ${f.msg}")
    }
  }

  private def parseService(source: String): RawService = {
    val c = ctx(source)
    fastparse.parse(c.content, c.defService.service(_)) match {
      case Parsed.Success(v, idx) =>
        assert(idx == source.length, s"parser left unconsumed input: [${source.drop(idx)}]")
        v
      case f: Parsed.Failure =>
        fail(s"expected parse success for [$source], got: ${f.msg}")
    }
  }

  private def assertDtoFails(source: String): Unit = {
    val c = ctx(source)
    fastparse.parse(c.content, c.defDto.dtoEnclosed(_)) match {
      case _: Parsed.Failure => ()
      case Parsed.Success(v, idx) =>
        fail(s"expected parse failure for [$source], but succeeded: $v (consumed $idx)")
    }
  }

  private def assertIdentifierFails(source: String): Unit = {
    val c = ctx(source)
    fastparse.parse(c.content, c.defDto.identifierEnclosed(_)) match {
      case _: Parsed.Failure => ()
      case Parsed.Success(v, idx) =>
        fail(s"expected parse failure for [$source], but succeeded: $v (consumed $idx)")
    }
  }

  // ─── positive: template declarations ─────────────────────────────────────────

  "template-head parser (PR-29.2)" should {

    "parse data X[T] { f: T } with single type param" in {
      val dto = parseDto("data X[T] { f: T }")
      assert(dto.typeParams == List(RawTypeName("T")))
      assert(dto.name.name == "X")
    }

    "parse data X[T, U] { … } with two type params" in {
      val dto = parseDto("data X[T, U] { f: T }")
      assert(dto.typeParams == List(RawTypeName("T"), RawTypeName("U")))
    }

    "parse data X[T, E, R] { … } with three type params" in {
      val dto = parseDto("data X[T, E, R] { f: T }")
      assert(dto.typeParams == List(RawTypeName("T"), RawTypeName("E"), RawTypeName("R")))
    }

    "parse adt X[T] { data Ok { v: T } } with single type param" in {
      val adt = parseAdt("adt X[T] { data Ok { v: T } }")
      assert(adt.typeParams == List(RawTypeName("T")))
      assert(adt.name.name == "X")
      assert(adt.members.head.isInstanceOf[RawAdtMemberDto])
    }

    "parse adt X[T, E] { … } with two type params" in {
      val adt = parseAdt("adt X[T, E] { data Ok { v: T } data Err { e: E } }")
      assert(adt.typeParams == List(RawTypeName("T"), RawTypeName("E")))
    }

    "parse contract X[T] { f: T } with single type param" in {
      val c = parseContract("contract X[T] { f: T }")
      assert(c.typeParams == List(RawTypeName("T")))
      assert(c.name.name == "X")
    }

    "parse contract X[T, U] { … } with two type params" in {
      val c = parseContract("contract X[T, U] { f: T }")
      assert(c.typeParams == List(RawTypeName("T"), RawTypeName("U")))
    }

    "parse service X[T] { def create(T): T } with single type param" in {
      val svc = parseService("service X[T] { def create(T): T }")
      assert(svc.typeParams == List(RawTypeName("T")))
      assert(svc.name.name == "X")
    }

    "parse service X[T, U] { … } with two type params" in {
      val svc = parseService("service X[T, U] { def get(T): U }")
      assert(svc.typeParams == List(RawTypeName("T"), RawTypeName("U")))
    }

    // ─── backward compat: no template head → typeParams == Nil ─────────────────

    "parse data X { f: i32 } without template head (typeParams == Nil)" in {
      val dto = parseDto("data X { f: i32 }")
      assert(dto.typeParams == Nil)
    }

    "parse adt X { data Ok { v: i32 } } without template head (typeParams == Nil)" in {
      val adt = parseAdt("adt X { data Ok { v: i32 } }")
      assert(adt.typeParams == Nil)
    }

    "parse contract X { f: i32 } without template head (typeParams == Nil)" in {
      val c = parseContract("contract X { f: i32 }")
      assert(c.typeParams == Nil)
    }

    "parse service X { def get(i32): str } without template head (typeParams == Nil)" in {
      val svc = parseService("service X { def get(i32): str }")
      assert(svc.typeParams == Nil)
    }

    // ─── negative: empty bracket clause must fail ─────────────────────────────

    "reject data X[] { … } — empty type-param list" in {
      assertDtoFails("data X[] { f: i32 }")
    }

    // ─── negative: id Foo[T] { … } must fail — §6.7 identifiers excluded ──────

    "reject id Foo[T] { … } — identifiers are not generifiable (spec §6.7)" in {
      // The identifierEnclosed rule does not parse a templateHead clause, so [T] is not consumed
      // before { and the parse fails.
      assertIdentifierFails("id Foo[T] { f: i32 }")
    }

    // ─── negative: non-bare-identifier in bracket clause must fail (spec §2.2) ─

    "reject data X[lst[i32]] { … } — type constructor is not a bare identifier" in {
      // templateHead uses idt.symbol (bare identifier); a type constructor expression
      // like lst[i32] must fail, locking the contract against regressions that swap
      // idt.symbol for typeRef.
      assertDtoFails("data X[lst[i32]] { f: i32 }")
    }

    "reject data X[Foo.Bar] { … } — qualified name is not a bare identifier" in {
      // A dotted qualified name must also fail in the bracket clause (spec §2.2).
      assertDtoFails("data X[Foo.Bar] { f: i32 }")
    }

    // ─── positive: lowercase type-param name accepted (spec §2.2 — convention only) ─

    "parse data X[t] { f: t } — lowercase type-param name is valid (spec §2.2)" in {
      // The parser must not enforce uppercase; casing is convention only.
      val dto = parseDto("data X[t] { f: t }")
      assert(dto.typeParams == List(RawTypeName("t")))
    }

  }
}
