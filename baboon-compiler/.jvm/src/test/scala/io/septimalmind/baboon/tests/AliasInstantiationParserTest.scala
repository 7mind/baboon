package io.septimalmind.baboon.tests

import fastparse.*
import io.septimalmind.baboon.parser.ParserContext
import io.septimalmind.baboon.parser.model.{FSPath, RawAlias, RawMemberMeta, RawTLDef, RawTypeName, RawTypeRef}
import izumi.fundamentals.collections.nonempty.{NEList, NEString}
import org.scalatest.wordspec.AnyWordSpec

/** Parser-only tests for PR-29.3: alias instantiation RHS (`type Y = X[Foo]`) and
  * optional `: derived[…]` clause on aliases.
  *
  * Verifies that:
  * - `type Y = X[Foo]` produces `RawAlias.target = RawTypeRef.Constructor(name=X, params=NEList(Simple(Foo)), prefix=Nil)`.
  * - `type Y = pkg.X[Foo]` populates the prefix list.
  * - `type Y = X[Foo, Bar]` produces two elements in `params`.
  * - `type Y = SimpleType` (no brackets) produces `RawTypeRef.Simple` — backward compat.
  * - `type Y = X[Foo] : derived[json]` populates `RawAlias.derived`.
  * - `type Y = X[Foo] : derived[json], derived[ueba]` populates both derivations.
  * - `type Y = X` (no derivation clause) leaves `RawAlias.derived == Set.empty` — backward compat.
  * - `type Y = Foo[Bar[i32]]` (nested instantiation, matrix #2): documents parser behaviour
  *   (accepted at parse time; validator-side rejection is PR-29.7).
  *
  * Does NOT exercise the typer. Typer-side resolution of `RawTypeRef.Constructor` against
  * user templates is PR-29.5; until then, alias-RHS instantiation parses but fails downstream.
  */
final class AliasInstantiationParserTest extends AnyWordSpec {

  private val dummyPath = FSPath.parse(NEString.unsafeFrom("alias-instantiation-parser-test.baboon"))

  private def ctx(input: String): ParserContext = ParserContext(dummyPath, input)

  // ─── helpers ─────────────────────────────────────────────────────────────────

  private def parseAlias(source: String): RawAlias = {
    val c = ctx(source)
    fastparse.parse(c.content, c.defModel.alias(_)) match {
      case Parsed.Success(RawTLDef.Alias(_, alias), idx) =>
        assert(idx == source.length, s"parser left unconsumed input: [${source.drop(idx)}]")
        alias
      case Parsed.Success(other, _) =>
        fail(s"expected RawTLDef.Alias for [$source], got: $other")
      case f: Parsed.Failure =>
        fail(s"expected parse success for [$source], got: ${f.msg}")
    }
  }

  private def assertAliasFails(source: String): Unit = {
    val c = ctx(source)
    fastparse.parse(c.content, c.defModel.alias(_)) match {
      case _: Parsed.Failure => ()
      case Parsed.Success(v, idx) =>
        fail(s"expected parse failure for [$source], but succeeded: $v (consumed $idx)")
    }
  }

  // ─── positive: alias instantiation RHS (type Y = X[Foo]) ─────────────────────

  "alias instantiation parser (PR-29.3)" should {

    "parse type Y = X[Foo] to RawTypeRef.Constructor with single param" in {
      val alias = parseAlias("type Y = X[Foo]")
      assert(alias.name == RawTypeName("Y"))
      alias.target match {
        case RawTypeRef.Constructor(name, params, prefix) =>
          assert(name == RawTypeName("X"))
          assert(prefix == Nil)
          assert(params == NEList(RawTypeRef.Simple(RawTypeName("Foo"), Nil)))
        case other =>
          fail(s"expected RawTypeRef.Constructor, got: $other")
      }
    }

    "parse type Y = pkg.X[Foo] with non-empty prefix" in {
      val alias = parseAlias("type Y = pkg.X[Foo]")
      alias.target match {
        case RawTypeRef.Constructor(name, params, prefix) =>
          assert(name == RawTypeName("X"))
          assert(prefix == List(RawTypeName("pkg")))
          assert(params == NEList(RawTypeRef.Simple(RawTypeName("Foo"), Nil)))
        case other =>
          fail(s"expected RawTypeRef.Constructor with prefix, got: $other")
      }
    }

    "parse type Y = X[Foo, Bar] to RawTypeRef.Constructor with two params" in {
      val alias = parseAlias("type Y = X[Foo, Bar]")
      alias.target match {
        case RawTypeRef.Constructor(name, params, prefix) =>
          assert(name == RawTypeName("X"))
          assert(prefix == Nil)
          assert(params.toList.length == 2)
          assert(params.toList.head == RawTypeRef.Simple(RawTypeName("Foo"), Nil))
          assert(params.toList(1) == RawTypeRef.Simple(RawTypeName("Bar"), Nil))
        case other =>
          fail(s"expected RawTypeRef.Constructor with two params, got: $other")
      }
    }

    "parse type Y = SimpleType (no brackets) to RawTypeRef.Simple — backward compat" in {
      val alias = parseAlias("type Y = SimpleType")
      alias.target match {
        case RawTypeRef.Simple(name, prefix) =>
          assert(name == RawTypeName("SimpleType"))
          assert(prefix == Nil)
        case other =>
          fail(s"expected RawTypeRef.Simple, got: $other")
      }
    }

    // ─── positive: derived clause on aliases ──────────────────────────────────

    "parse type Y = X[Foo] : derived[json] — single derivation" in {
      val alias = parseAlias("type Y = X[Foo] : derived[json]")
      assert(alias.derived == Set(RawMemberMeta.Derived("json")))
    }

    "parse type Y = X[Foo] : derived[json], derived[ueba] — two derivations" in {
      val alias = parseAlias("type Y = X[Foo] : derived[json], derived[ueba]")
      assert(alias.derived == Set(RawMemberMeta.Derived("json"), RawMemberMeta.Derived("ueba")))
    }

    "parse type Y = X (no derivation clause) — derived == Set.empty backward compat" in {
      val alias = parseAlias("type Y = X")
      assert(alias.derived == Set.empty)
    }

    "parse type Y = SimpleType : derived[json] — derived clause on non-instantiation alias" in {
      val alias = parseAlias("type Y = SimpleType : derived[json]")
      assert(alias.derived == Set(RawMemberMeta.Derived("json")))
      alias.target match {
        case RawTypeRef.Simple(name, _) => assert(name == RawTypeName("SimpleType"))
        case other                      => fail(s"expected RawTypeRef.Simple, got: $other")
      }
    }

    // ─── matrix #2: nested instantiation in alias RHS ─────────────────────────
    // Spec §2.5.2 forbids `type Y = Foo[Bar[i32]]` at compile time (validator-side, PR-29.7).
    // The parser accepts it because `typeRef` recursively allows brackets (typeParams calls typeRef).
    // This test documents the observed parser behaviour and locks it against unintended changes.

    "accept type Y = Foo[Bar[i32]] at parse time — validator-side rejection is PR-29.7" in {
      // The parser produces RawTypeRef.Constructor(Foo, NEList(Constructor(Bar, NEList(Simple(i32)))), Nil).
      // This is NOT a parse error; the spec-forbidden rejection is deferred to the validator.
      val alias = parseAlias("type Y = Foo[Bar[i32]]")
      alias.target match {
        case RawTypeRef.Constructor(outerName, outerParams, _) =>
          assert(outerName == RawTypeName("Foo"))
          outerParams.head match {
            case RawTypeRef.Constructor(innerName, innerParams, _) =>
              assert(innerName == RawTypeName("Bar"))
              assert(innerParams.head == RawTypeRef.Simple(RawTypeName("i32"), Nil))
            case other =>
              fail(s"expected inner RawTypeRef.Constructor, got: $other")
          }
        case other =>
          fail(s"expected outer RawTypeRef.Constructor, got: $other")
      }
    }

  }
}
