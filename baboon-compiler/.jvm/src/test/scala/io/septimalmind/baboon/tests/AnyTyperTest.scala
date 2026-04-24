package io.septimalmind.baboon.tests

import io.septimalmind.baboon.parser.BaboonParser
import io.septimalmind.baboon.parser.model.FSPath
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.tests.BaboonTest.BaboonTestModule
import io.septimalmind.baboon.typer.BaboonTyper
import io.septimalmind.baboon.typer.model.*
import io.septimalmind.baboon.typer.model.TypeRef.AnyVariant
import izumi.functional.bio.Error2
import izumi.fundamentals.collections.nonempty.{NEList, NEString}
import izumi.reflect.TagKK

final class AnyTyperTest extends AnyTyperTestBase[Either]

abstract class AnyTyperTestBase[F[+_, +_]: Error2: TagKK: BaboonTestModule] extends BaboonTest[F] {

  // Minimal DSL fixture that exercises all six DSL forms of `any`. The underlying type `Inner` is
  // a plain DTO with `: derived[ueba]`; PR 1.3 enforces the derivation rule, but here we only need
  // the type to exist and to be reachable from the typer.
  private val fixtureModel: String =
    """model any.typer.test
      |
      |version "1.0.0"
      |
      |data Inner : derived[ueba] {
      |  x: i32
      |}
      |
      |root data Outer {
      |  a:  any
      |  b:  any[domain:this]
      |  c:  any[domain:current]
      |  d1: any[Inner]
      |  d2: any[domain:this, Inner]
      |  d3: any[domain:current, Inner]
      |  e:  any[i32]
      |  g:  any[lst[Inner]]
      |}
      |""".stripMargin

  private def fixtureInput: BaboonParser.Input = BaboonParser.Input(
    FSPath.parse(NEString.unsafeFrom("any-typer-test.baboon")),
    fixtureModel,
  )

  private def runTyper(parser: BaboonParser[F], typer: BaboonTyper[F]): F[NEList[BaboonIssue], Domain] = {
    for {
      parsed <- parser.parse(fixtureInput)
      domain <- typer.process(parsed)
    } yield domain
  }

  private def outerOf(domain: Domain): Typedef.Dto = {
    val ids = domain.defs.meta.nodes.keys.collect { case u: TypeId.User => u }.toList
    val outerId = ids
      .find(_.name.name == "Outer")
      .getOrElse(throw new AssertionError(s"Outer not found in: $ids"))
    domain.defs.meta.nodes(outerId) match {
      case DomainMember.User(_, dto: Typedef.Dto, _, _) => dto
      case other                                        => throw new AssertionError(s"Outer must be a DTO, got $other")
    }
  }

  private def fieldTpe(dto: Typedef.Dto, name: String): TypeRef = {
    dto.fields
      .find(_.name.name == name)
      .getOrElse(throw new AssertionError(s"Field $name missing in Outer"))
      .tpe
  }

  "any typer" should {

    "resolve bare `any` (Simple(\"any\", Nil)) to TypeRef.Any(Global, None) — variant A" in {
      (parser: BaboonParser[F], typer: BaboonTyper[F]) =>
        for {
          domain <- runTyper(parser, typer)
        } yield {
          val outer = outerOf(domain)
          assert(fieldTpe(outer, "a") == TypeRef.Any(AnyVariant.Global, None))
        }
    }

    "resolve `any[domain:this]` to TypeRef.Any(ThisDom, None) — variant B" in {
      (parser: BaboonParser[F], typer: BaboonTyper[F]) =>
        for {
          domain <- runTyper(parser, typer)
        } yield {
          val outer = outerOf(domain)
          assert(fieldTpe(outer, "b") == TypeRef.Any(AnyVariant.ThisDom, None))
        }
    }

    "resolve `any[domain:current]` to TypeRef.Any(Current, None) — variant C" in {
      (parser: BaboonParser[F], typer: BaboonTyper[F]) =>
        for {
          domain <- runTyper(parser, typer)
        } yield {
          val outer = outerOf(domain)
          assert(fieldTpe(outer, "c") == TypeRef.Any(AnyVariant.Current, None))
        }
    }

    "resolve `any[Inner]` to TypeRef.Any(Global, Some(Inner)) — variant D1" in {
      (parser: BaboonParser[F], typer: BaboonTyper[F]) =>
        for {
          domain <- runTyper(parser, typer)
        } yield {
          val outer = outerOf(domain)
          fieldTpe(outer, "d1") match {
            case TypeRef.Any(AnyVariant.Global, Some(TypeRef.Scalar(u: TypeId.User))) =>
              assert(u.name.name == "Inner")
            case other => fail(s"expected Any(Global, Some(Inner)), got $other")
          }
        }
    }

    "resolve `any[domain:this, Inner]` to TypeRef.Any(ThisDom, Some(Inner)) — variant D2" in {
      (parser: BaboonParser[F], typer: BaboonTyper[F]) =>
        for {
          domain <- runTyper(parser, typer)
        } yield {
          val outer = outerOf(domain)
          fieldTpe(outer, "d2") match {
            case TypeRef.Any(AnyVariant.ThisDom, Some(TypeRef.Scalar(u: TypeId.User))) =>
              assert(u.name.name == "Inner")
            case other => fail(s"expected Any(ThisDom, Some(Inner)), got $other")
          }
        }
    }

    "resolve `any[domain:current, Inner]` to TypeRef.Any(Current, Some(Inner)) — variant D3" in {
      (parser: BaboonParser[F], typer: BaboonTyper[F]) =>
        for {
          domain <- runTyper(parser, typer)
        } yield {
          val outer = outerOf(domain)
          fieldTpe(outer, "d3") match {
            case TypeRef.Any(AnyVariant.Current, Some(TypeRef.Scalar(u: TypeId.User))) =>
              assert(u.name.name == "Inner")
            case other => fail(s"expected Any(Current, Some(Inner)), got $other")
          }
        }
    }

    "resolve `any[i32]` to TypeRef.Any(Global, Some(Scalar(i32)))" in {
      (parser: BaboonParser[F], typer: BaboonTyper[F]) =>
        for {
          domain <- runTyper(parser, typer)
        } yield {
          val outer = outerOf(domain)
          assert(
            fieldTpe(outer, "e") == TypeRef.Any(AnyVariant.Global, Some(TypeRef.Scalar(TypeId.Builtins.i32)))
          )
        }
    }

    "resolve `any[lst[Inner]]` to TypeRef.Any(Global, Some(Constructor(lst, [Scalar(Inner)])))" in {
      (parser: BaboonParser[F], typer: BaboonTyper[F]) =>
        for {
          domain <- runTyper(parser, typer)
        } yield {
          val outer = outerOf(domain)
          fieldTpe(outer, "g") match {
            case TypeRef.Any(AnyVariant.Global, Some(TypeRef.Constructor(TypeId.Builtins.lst, args))) =>
              args.toList match {
                case TypeRef.Scalar(u: TypeId.User) :: Nil =>
                  assert(u.name.name == "Inner")
                case other =>
                  fail(s"expected single-arg Scalar(Inner), got $other")
              }
            case other => fail(s"expected Any(Global, Some(Constructor(lst, [Inner]))), got $other")
          }
        }
    }

    "fail to typecheck `any[Unknown]` when the underlying type does not exist" in {
      // The parser accepts `any[Unknown]`, but the typer must reject it — translation recurses
      // into the underlying raw type, which fails when `Unknown` cannot be resolved.
      (parser: BaboonParser[F], typer: BaboonTyper[F]) =>
        val badModel =
          """model any.typer.test.bad
            |
            |version "1.0.0"
            |
            |root data D {
            |  f: any[Unknown]
            |}
            |""".stripMargin
        val badInput = BaboonParser.Input(
          FSPath.parse(NEString.unsafeFrom("any-typer-bad.baboon")),
          badModel,
        )
        import izumi.functional.bio.F
        for {
          parsed <- parser.parse(badInput)
          // Use `catchAll` to turn an error into a success for assertion purposes.
          outcome <- typer.process(parsed).map(Right(_): Either[NEList[BaboonIssue], Domain]).catchAll {
            errs => F.pure(Left(errs): Either[NEList[BaboonIssue], Domain])
          }
        } yield {
          assert(outcome.isLeft, s"expected typer failure for `any[Unknown]`, got: $outcome")
        }
    }
  }

  "AnyVariant.metaKindByte" should {
    // Per design spec §"Meta-kind byte table" (and plan §6.3): bit 0 = typeid present, bit 1 =
    // version present, bit 2 = domain present. `underlying` present => typeid is known statically
    // => typeid bit = 0.
    "return 0x07 for (Global, None)" in {
      (_: BaboonParser[F]) =>
        assert(AnyVariant.metaKindByte(AnyVariant.Global, hasUnderlying = false) == 0x07.toByte)
    }
    "return 0x06 for (Global, Some)" in {
      (_: BaboonParser[F]) =>
        assert(AnyVariant.metaKindByte(AnyVariant.Global, hasUnderlying = true) == 0x06.toByte)
    }
    "return 0x03 for (ThisDom, None)" in {
      (_: BaboonParser[F]) =>
        assert(AnyVariant.metaKindByte(AnyVariant.ThisDom, hasUnderlying = false) == 0x03.toByte)
    }
    "return 0x02 for (ThisDom, Some)" in {
      (_: BaboonParser[F]) =>
        assert(AnyVariant.metaKindByte(AnyVariant.ThisDom, hasUnderlying = true) == 0x02.toByte)
    }
    "return 0x01 for (Current, None)" in {
      (_: BaboonParser[F]) =>
        assert(AnyVariant.metaKindByte(AnyVariant.Current, hasUnderlying = false) == 0x01.toByte)
    }
    "return 0x00 for (Current, Some)" in {
      (_: BaboonParser[F]) =>
        assert(AnyVariant.metaKindByte(AnyVariant.Current, hasUnderlying = true) == 0x00.toByte)
    }
  }

  "user type named `any`" should {

    "be definable and referenceable via prefixed path (foo.pkg.any)" in {
      // Back-compat guarantee from PR 1.1: a user type literally named `any` must remain usable.
      // Bare `any` in a field position parses as `Simple("any", Nil)` and is reinterpreted by the
      // typer as variant A — so to reference the user type here we define it in a nested namespace
      // and reach it via a prefix.
      (parser: BaboonParser[F], typer: BaboonTyper[F]) =>
        val model =
          """model any.typer.usertype
            |
            |version "1.0.0"
            |
            |ns nested {
            |  data any {
            |    x: i32
            |  }
            |}
            |
            |root data Holder {
            |  f: nested.any
            |}
            |""".stripMargin
        val input = BaboonParser.Input(
          FSPath.parse(NEString.unsafeFrom("any-typer-usertype.baboon")),
          model,
        )
        for {
          parsed <- parser.parse(input)
          domain <- typer.process(parsed)
        } yield {
          val ids = domain.defs.meta.nodes.keys.collect { case u: TypeId.User => u }.toList
          val holderId = ids
            .find(_.name.name == "Holder")
            .getOrElse(throw new AssertionError(s"Holder not found in: $ids"))
          domain.defs.meta.nodes(holderId) match {
            case DomainMember.User(_, dto: Typedef.Dto, _, _) =>
              dto.fields.find(_.name.name == "f").map(_.tpe) match {
                case Some(TypeRef.Scalar(u: TypeId.User)) =>
                  assert(u.name.name == "any")
                case other => fail(s"expected Scalar(user#any), got $other")
              }
            case other => fail(s"Holder must be a DTO, got $other")
          }
        }
    }

    "be shadowed by the `any` builtin when referenced unprefixed at top level" in {
      // Paired with BaboonTranslator: bare `any` always resolves to `TypeRef.Any(Global, None)`
      // (the match fires BEFORE scope lookup). Top-level `data any { ... }` can still exist and be
      // reached via a prefix, but an unprefixed reference produces the builtin.
      //
      // NOTE: only `data`-type shadowing is exercised here. The DSL does not currently support a
      // top-level `alias any = ...` syntax, so the alias case is not covered.
      (parser: BaboonParser[F], typer: BaboonTyper[F]) =>
        val model =
          """model any.typer.shadow
            |
            |version "1.0.0"
            |
            |data any {
            |  x: i32
            |}
            |
            |root data Holder {
            |  f: any
            |}
            |""".stripMargin
        val input = BaboonParser.Input(
          FSPath.parse(NEString.unsafeFrom("any-typer-shadow.baboon")),
          model,
        )
        for {
          parsed <- parser.parse(input)
          domain <- typer.process(parsed)
        } yield {
          val userIds = domain.defs.meta.nodes.keys.collect { case u: TypeId.User => u }.toList
          // The user-defined top-level `data any` parses cleanly and does not conflict with the
          // builtin. It may be pruned from the retained definitions when nothing roots it (as is
          // the case here, since the unprefixed reference resolves to the builtin). The key
          // assertion is that `Holder.f` resolves to `TypeRef.Any(Global, None)`, NOT to a
          // user-type reference.
          val holderId = userIds
            .find(_.name.name == "Holder")
            .getOrElse(throw new AssertionError(s"Holder not found in: $userIds"))
          domain.defs.meta.nodes(holderId) match {
            case DomainMember.User(_, dto: Typedef.Dto, _, _) =>
              assert(
                dto.fields.find(_.name.name == "f").map(_.tpe)
                  == Some(TypeRef.Any(AnyVariant.Global, None)),
                s"unprefixed `any` must resolve to builtin variant A, got: ${dto.fields}",
              )
            case other => fail(s"Holder must be a DTO, got $other")
          }
        }
    }
  }
}
