package io.septimalmind.baboon.tests

import fastparse.*
import io.septimalmind.baboon.parser.ParserContext
import io.septimalmind.baboon.parser.model.{FSPath, RawTypeName, RawTypeRef}
import izumi.fundamentals.collections.nonempty.NEString
import org.scalatest.wordspec.AnyWordSpec

final class AnyParserTest extends AnyWordSpec {

  private val dummyPath = FSPath.parse(NEString.unsafeFrom("any-parser-test.baboon"))

  private def ctx(input: String): ParserContext = ParserContext(dummyPath, input)

  private def parseTypeRef(input: String): Parsed[RawTypeRef] = {
    val c = ctx(input)
    fastparse.parse(c.content, c.defDto.typeRef(_))
  }

  // Parse a full DTO document: `data D { f: <input> }`. Used by expectFailure to demand a clean,
  // non-recoverable failure at the document level — the natural context for a field typeref.
  private def parseDtoFixture(typeRefInput: String): Parsed[?] = {
    val src = s"data D { f: $typeRefInput }"
    val c   = ctx(src)
    fastparse.parse(c.content, c.defDto.dtoEnclosed(_))
  }

  private def expectSuccess(input: String): RawTypeRef = {
    parseTypeRef(input) match {
      case Parsed.Success(v, idx) =>
        assert(idx == input.length, s"parser did not consume all input; left: ${input.drop(idx)}")
        v
      case f: Parsed.Failure =>
        fail(s"expected success for [$input], got failure: ${f.msg}")
    }
  }

  // Strict failure: the field-bearing DTO fixture MUST NOT parse (Parsed.Failure).
  // Silent partial-consume at the typeref level is NOT accepted — we verify at the document level,
  // where the surrounding `}` cannot be consumed by any leftover dangling input.
  private def expectFailure(input: String): Unit = {
    parseDtoFixture(input) match {
      case Parsed.Success(v, idx) =>
        fail(s"expected failure for [$input], but parsed fully as $v (consumed $idx)")
      case _: Parsed.Failure =>
        ()
    }
  }

  // Tight typeref-level failure: the typeRef parser MUST fail to consume the entire input.
  // This distinguishes "clean rejection at the typeRef level" from "partial-consume that bubbles
  // up as a document-level failure for the wrong reason" — the exact regression mode D02/D03
  // exposed. `~ End` forces the parser to consume all input; anything less counts as failure.
  private def expectTypeRefFailure(input: String): Unit = {
    val c = ctx(input)
    def exact[$: P]: P[RawTypeRef] = {
      // `NoWhitespace` makes the `~ End` boundary strict: no trailing whitespace is tolerated.
      import NoWhitespace.noWhitespaceImplicit
      val _ = noWhitespaceImplicit // keep import referenced to silence unused-import warning
      c.defDto.typeRef ~ End
    }
    fastparse.parse(c.content, exact(_)) match {
      case _: Parsed.Failure => ()
      case s: Parsed.Success[?] =>
        fail(s"expected typeRef parse of [$input] to fail, got success: $s (consumed ${s.index})")
    }
  }

  // Some convenient shortcuts
  private def simple(name: String): RawTypeRef = RawTypeRef.Simple(RawTypeName(name), Nil)

  "any type parser" should {

    // Positive cases — the six canonical DSL forms

    "parse bare any (A)" in {
      // At parser level, bare `any` parses as an ordinary identifier; the typer resolves it to the
      // builtin variant A. The parser only special-cases `any[...]`.
      assert(expectSuccess("any") == simple("any"))
    }

    "parse any[domain:this] (B)" in {
      assert(expectSuccess("any[domain:this]") == RawTypeRef.AnyRef(Some(RawTypeRef.AnyRef.DomainThis), None))
    }

    "parse any[domain:current] (C)" in {
      assert(expectSuccess("any[domain:current]") == RawTypeRef.AnyRef(Some(RawTypeRef.AnyRef.DomainCurrent), None))
    }

    "parse any[SomeType] (D1)" in {
      assert(expectSuccess("any[SomeType]") == RawTypeRef.AnyRef(None, Some(simple("SomeType"))))
    }

    "parse any[domain:this, SomeType] (D2)" in {
      assert(expectSuccess("any[domain:this, SomeType]") == RawTypeRef.AnyRef(Some(RawTypeRef.AnyRef.DomainThis), Some(simple("SomeType"))))
    }

    "parse any[domain:current, SomeType] (D3)" in {
      assert(expectSuccess("any[domain:current, SomeType]") == RawTypeRef.AnyRef(Some(RawTypeRef.AnyRef.DomainCurrent), Some(simple("SomeType"))))
    }

    // Nested positive cases

    "parse opt[any]" in {
      val r = expectSuccess("opt[any]")
      r match {
        case RawTypeRef.Constructor(RawTypeName("opt"), params, Nil) =>
          // At parser level, the inner `any` is a plain identifier; typer decides resolution.
          assert(params.toList == List(simple("any")))
        case other => fail(s"unexpected: $other")
      }
    }

    "parse lst[any[domain:current]]" in {
      val r = expectSuccess("lst[any[domain:current]]")
      r match {
        case RawTypeRef.Constructor(RawTypeName("lst"), params, Nil) =>
          assert(params.toList == List(RawTypeRef.AnyRef(Some(RawTypeRef.AnyRef.DomainCurrent), None)))
        case other => fail(s"unexpected: $other")
      }
    }

    "parse map[str, any]" in {
      val r = expectSuccess("map[str, any]")
      r match {
        case RawTypeRef.Constructor(RawTypeName("map"), params, Nil) =>
          assert(params.toList == List(simple("str"), simple("any")))
        case other => fail(s"unexpected: $other")
      }
    }

    "parse any[opt[str]] (nested underlying)" in {
      val r = expectSuccess("any[opt[str]]")
      r match {
        case RawTypeRef.AnyRef(None, Some(RawTypeRef.Constructor(RawTypeName("opt"), inner, Nil))) =>
          assert(inner.toList == List(simple("str")))
        case other => fail(s"unexpected: $other")
      }
    }

    // Whitespace variants (D05)

    "parse any[domain:this,SomeType] (no space after comma)" in {
      assert(expectSuccess("any[domain:this,SomeType]") == RawTypeRef.AnyRef(Some(RawTypeRef.AnyRef.DomainThis), Some(simple("SomeType"))))
    }

    "parse any[ domain:current , SomeType ] (multi-space around content)" in {
      assert(expectSuccess("any[ domain:current , SomeType ]") == RawTypeRef.AnyRef(Some(RawTypeRef.AnyRef.DomainCurrent), Some(simple("SomeType"))))
    }

    // Prefixed / ambiguous references — `any` is NOT reserved at the parser level

    "parse foo.any as prefixed identifier" in {
      // The typer later decides whether `foo.any` resolves to a user type; the parser accepts it.
      assert(expectSuccess("foo.any") == RawTypeRef.Simple(RawTypeName("any"), List(RawTypeName("foo"))))
    }

    "parse any.Foo as prefixed identifier" in {
      // `any` here is a path prefix, not the builtin — because it's not followed by `[`.
      assert(expectSuccess("any.Foo") == RawTypeRef.Simple(RawTypeName("Foo"), List(RawTypeName("any"))))
    }

    "parse opt[foo.any] without truncation" in {
      val r = expectSuccess("opt[foo.any]")
      r match {
        case RawTypeRef.Constructor(RawTypeName("opt"), params, Nil) =>
          assert(params.toList == List(RawTypeRef.Simple(RawTypeName("any"), List(RawTypeName("foo")))))
        case other => fail(s"unexpected: $other")
      }
    }

    // `any` remains a legal user identifier at the definition level

    "allow a user-defined DTO named any" in {
      // `any` must remain a legal user identifier at the definition site — the parser is blind to
      // its builtin meaning except in the `any[...]` syntactic form.
      val src = "data any { x: i32 }"
      val c   = ctx(src)
      fastparse.parse(c.content, c.defDto.dtoEnclosed(_)) match {
        case Parsed.Success(_, idx) =>
          assert(idx == src.length, s"parser must consume full `data any` definition")
        case f: Parsed.Failure =>
          fail(s"expected success for user type `any`, got failure: ${f.msg}")
      }
    }

    "allow a DTO field referring to user type any via bare and prefixed forms" in {
      // Field declarations `a: any` and `b: any.X` must both parse as ordinary identifiers —
      // the parser never commits to the builtin-any shape without a following `[`.
      val src = "data D { a: any b: any.X }"
      val c   = ctx(src)
      fastparse.parse(c.content, c.defDto.dtoEnclosed(_)) match {
        case Parsed.Success(_, idx) =>
          assert(idx == src.length, s"parser must consume full DTO fixture")
        case f: Parsed.Failure =>
          fail(s"expected success for DTO with bare/prefixed `any` fields, got failure: ${f.msg}")
      }
    }

    // Round-trip: render then re-parse must yield the same AST

    "round-trip all constructible AnyRef forms" in {
      // Note: `AnyRef(None, None)` is unconstructible by design (see `require` in RawTypeRef.AnyRef).
      // Bare `any` is represented as `Simple(RawTypeName("any"), Nil)` at the parse level; the
      // typer resolves it to variant A. Only the bracketed forms round-trip through `AnyRef`.
      val samples: List[RawTypeRef.AnyRef] = List(
        RawTypeRef.AnyRef(Some(RawTypeRef.AnyRef.DomainThis), None),
        RawTypeRef.AnyRef(Some(RawTypeRef.AnyRef.DomainCurrent), None),
        RawTypeRef.AnyRef(None, Some(simple("SomeType"))),
        RawTypeRef.AnyRef(Some(RawTypeRef.AnyRef.DomainThis), Some(simple("SomeType"))),
        RawTypeRef.AnyRef(Some(RawTypeRef.AnyRef.DomainCurrent), Some(simple("SomeType"))),
      )
      samples.foreach {
        s =>
          val rendered = s.render
          val reparsed = expectSuccess(rendered)
          assert(reparsed == s, s"round-trip failed: rendered=[$rendered], reparsed=[$reparsed], original=[$s]")
      }
    }

    "reject construction of AnyRef(None, None)" in {
      // The `require` in `RawTypeRef.AnyRef` guards the invariant that the parser never produces
      // `AnyRef(None, None)` — bare `any` is `Simple(RawTypeName("any"), Nil)`. Hand-construction
      // must fail fast so tests/pretty-printers/AST rewrites cannot re-introduce the foot-gun.
      intercept[IllegalArgumentException] {
        RawTypeRef.AnyRef(None, None)
      }
    }

    // Negative cases
    //
    // Each case is checked twice: `expectTypeRefFailure` demands a tight, typeref-level Failure
    // (guards against the D02/D03-class silent partial-consume regression), and `expectFailure`
    // additionally checks document-level integration (belt-and-braces).

    "reject any[domain:bogus]" in {
      expectTypeRefFailure("any[domain:bogus]")
      expectFailure("any[domain:bogus]")
    }

    "reject any[,SomeType]" in {
      expectTypeRefFailure("any[,SomeType]")
      expectFailure("any[,SomeType]")
    }

    "reject any[SomeType, domain:this] (wrong arg order)" in {
      expectTypeRefFailure("any[SomeType, domain:this]")
      expectFailure("any[SomeType, domain:this]")
    }

    "reject any[SomeType, SomeOther] (two types)" in {
      expectTypeRefFailure("any[SomeType, SomeOther]")
      expectFailure("any[SomeType, SomeOther]")
    }

    "reject any[domain:this, SomeType, Extra] (qualifier + 2 types)" in {
      expectTypeRefFailure("any[domain:this, SomeType, Extra]")
      expectFailure("any[domain:this, SomeType, Extra]")
    }

    "reject any[domain:this,] (trailing comma after qualifier)" in {
      expectTypeRefFailure("any[domain:this,]")
      expectFailure("any[domain:this,]")
    }

    "reject any[T,] (trailing comma after type)" in {
      expectTypeRefFailure("any[T,]")
      expectFailure("any[T,]")
    }

    "reject any[domain: this] (whitespace around colon)" in {
      expectTypeRefFailure("any[domain: this]")
      expectFailure("any[domain: this]")
    }

    "reject any[] (empty brackets)" in {
      expectTypeRefFailure("any[]")
      expectFailure("any[]")
    }

    // D15: whitespace between `any` and `[` is tolerated (parses same as `any[T]`).
    // Previously this silently truncated to `Simple("any", Nil)` with `[T]` dangling.

    "parse any [T] (space before bracket) as equivalent to any[T]" in {
      assert(expectSuccess("any [T]") == RawTypeRef.AnyRef(None, Some(simple("T"))))
    }

    "parse any\\t[T] (tab before bracket) as equivalent to any[T]" in {
      assert(expectSuccess("any\t[T]") == RawTypeRef.AnyRef(None, Some(simple("T"))))
    }

    "parse any [domain:this] (space before bracket, qualifier inside)" in {
      assert(expectSuccess("any [domain:this]") == RawTypeRef.AnyRef(Some(RawTypeRef.AnyRef.DomainThis), None))
    }

    // D16: nested / prefixed underlying — recursive dispatch coverage.

    "parse any[foo.any] (prefixed identifier as underlying)" in {
      val r = expectSuccess("any[foo.any]")
      assert(r == RawTypeRef.AnyRef(None, Some(RawTypeRef.Simple(RawTypeName("any"), List(RawTypeName("foo"))))))
    }

    "parse any[any] (bare `any` as underlying — parser treats inner as Simple)" in {
      // The inner `any` has no brackets, so it is a plain `Simple` at the parser level; the typer
      // resolves it to variant A later. The outer `any[...]` still produces `AnyRef`.
      val r = expectSuccess("any[any]")
      assert(r == RawTypeRef.AnyRef(None, Some(simple("any"))))
    }

    "parse any[any[T]] (nested AnyRef as underlying)" in {
      val r = expectSuccess("any[any[T]]")
      assert(r == RawTypeRef.AnyRef(None, Some(RawTypeRef.AnyRef(None, Some(simple("T"))))))
    }

    // Sanity: `anybody` is still a valid user identifier (the `any` keyword must not swallow it)

    "allow anybody as a user type name" in {
      val r = expectSuccess("anybody")
      assert(r == simple("anybody"))
    }
  }
}
