package io.septimalmind.baboon.tests

import io.septimalmind.baboon.parser.BaboonParser
import io.septimalmind.baboon.parser.model.FSPath
import io.septimalmind.baboon.parser.model.issues.{BaboonIssue, TyperIssue}
import io.septimalmind.baboon.tests.BaboonTest.BaboonTestModule
import io.septimalmind.baboon.typer.BaboonTyper
import io.septimalmind.baboon.typer.model.Domain
import izumi.functional.bio.{Error2, F}
import izumi.fundamentals.collections.nonempty.{NEList, NEString}
import izumi.reflect.TagKK

import scala.reflect.ClassTag

/** PR-29.7 (M29) negative-test matrix unit tests.
  *
  * Covers the diagnostic cases added in PR-29.7:
  *   - Matrix #7  — template referenced without instantiation (`TemplateNotInstantiated`)
  *   - Matrix #8  — instantiating a non-template (`NotATemplate`)
  *   - D03        — template body carries `derived` annotation (`TemplateBodyCarriesDerived`)
  *
  * Matrix #1 in-body (incl. self-reference and container-mediated) is already covered by
  * `TemplateInstantiatorTest` (`TemplateInstantiationInForbiddenPosition`). Matrix #2 (nested
  * template instantiation in alias RHS args) is also caught by
  * `TemplateInstantiationInForbiddenPosition` during substitution (the inner template
  * constructor is detected in `substituteTypeRef`).
  *
  * Matrix #6 (`data Y { f: T }` outside a template) fires `NameNotFound` via the existing
  * `BaboonTranslator.convertTpe` path ("Type not found: T"). That diagnostic is clear and
  * accurate; no new case is added for matrix #6 (plan opt-out path).
  *
  * Regression test for matrix #2 (nested template in alias RHS args) confirms
  * `TemplateInstantiationInForbiddenPosition` fires.
  */
final class M29ValidatorTest extends M29ValidatorTestBase[Either]

abstract class M29ValidatorTestBase[F[+_, +_]: Error2: TagKK: BaboonTestModule] extends BaboonTest[F] {

  // ─── helpers ─────────────────────────────────────────────────────────────────

  private def makeInput(name: String, body: String): BaboonParser.Input =
    BaboonParser.Input(FSPath.parse(NEString.unsafeFrom(name)), body)

  private def runTyperFor(
    parser: BaboonParser[F],
    typer: BaboonTyper[F],
    content: String,
    name: String = "m29-validator-test.baboon",
  ): F[Nothing, Either[NEList[BaboonIssue], Domain]] = {
    val input = makeInput(name, content)
    parser.parse(input).flatMap(typer.process).map(Right(_): Either[NEList[BaboonIssue], Domain]).catchAll {
      errs => F.pure(Left(errs): Either[NEList[BaboonIssue], Domain])
    }
  }

  private def typerIssues(issues: NEList[BaboonIssue]): List[TyperIssue] =
    issues.toList.collect { case BaboonIssue.Typer(ti) => ti }

  private def assertProducesTyperIssue[T <: TyperIssue: ClassTag](
    outcome: Either[NEList[BaboonIssue], Domain]
  ): Unit = {
    val issues = outcome.left.getOrElse(throw new AssertionError(s"expected failure, got: $outcome"))
    val ti     = typerIssues(issues)
    val ct     = implicitly[ClassTag[T]]
    assert(
      ti.exists(ct.runtimeClass.isInstance),
      s"expected ${ct.runtimeClass.getSimpleName}, got: $ti",
    )
  }

  // ─── fixtures ────────────────────────────────────────────────────────────────

  /** Matrix #7: `type Y = X` — bare reference to a template without type args. */
  private val matrix7UninstantiatedTemplate: String =
    """model m29.validator.matrix7
      |
      |version "1.0.0"
      |
      |data X[T] { f: T }
      |type Y = X
      |""".stripMargin

  /** Matrix #8: `type Y = i32[str]` — `i32` is not a template or builtin collection. */
  private val matrix8NotATemplate: String =
    """model m29.validator.matrix8
      |
      |version "1.0.0"
      |
      |type Y = i32[str]
      |""".stripMargin

  /** D03: template body carries `: derived[json]` — must be on the alias, not the body. */
  private val d03TemplateBodyCarriesDerived: String =
    """model m29.validator.d03
      |
      |version "1.0.0"
      |
      |data Page[T] : derived[json] {
      |  items: lst[T]
      |}
      |
      |root type IntPage = Page[i32]
      |""".stripMargin

  /** Regression: matrix #2 — nested template instantiation in alias RHS args.
    * `type Y = X[Z[i32]]` where both X and Z are templates.
    * Should fire `TemplateInstantiationInForbiddenPosition` (caught during substitution of X's body args).
    */
  private val matrix2NestedTemplateInArgs: String =
    """model m29.validator.matrix2
      |
      |version "1.0.0"
      |
      |data Z[T] { v: T }
      |data X[T] { f: T }
      |type Y = X[Z[i32]]
      |""".stripMargin

  /** Positive regression: matrix #8 does NOT fire for legitimate builtin collections. */
  private val matrix8BuiltinPositive: String =
    """model m29.validator.matrix8pos
      |
      |version "1.0.0"
      |
      |data Holder[T] { items: lst[T] }
      |root type IntHolder = Holder[i32]
      |""".stripMargin

  /** D01: matrix #8 with a non-template user DTO — `type Y = MyDto[i32]` must fire NotATemplate. */
  private val matrix8NotATemplateUserDto: String =
    """model m29.validator.matrix8userdto
      |
      |version "1.0.0"
      |
      |data MyDto { f: str }
      |type Y = MyDto[i32]
      |""".stripMargin

  /** D02: template without `: derived[…]` — must succeed (no TemplateBodyCarriesDerived). */
  private val d02TemplateNoDerivedPositive: String =
    """model m29.validator.d02pos
      |
      |version "1.0.0"
      |
      |data X[T] { f: T }
      |root type Y = X[i32]
      |""".stripMargin

  /** D03: non-template alias `type Y = X` where X is a plain DTO — must succeed (no TemplateNotInstantiated). */
  private val d03NonTemplateAliasPositive: String =
    """model m29.validator.d03pos
      |
      |version "1.0.0"
      |
      |data X { f: i32 }
      |root type Y = X
      |""".stripMargin

  // ─── tests ────────────────────────────────────────────────────────────────────

  "M29 validator (PR-29.7)" should {

    // ── Matrix #7 ────────────────────────────────────────────────────────────

    "matrix #7: produce TemplateNotInstantiated for `type Y = X` where X is a template" in {
      (parser: BaboonParser[F], typer: BaboonTyper[F]) =>
        for {
          outcome <- runTyperFor(parser, typer, matrix7UninstantiatedTemplate)
        } yield {
          assertProducesTyperIssue[TyperIssue.TemplateNotInstantiated](outcome)
        }
    }

    "matrix #7: TemplateNotInstantiated carries templateName=X and aliasName=Y" in {
      (parser: BaboonParser[F], typer: BaboonTyper[F]) =>
        for {
          outcome <- runTyperFor(parser, typer, matrix7UninstantiatedTemplate)
        } yield {
          val issues = outcome.left.getOrElse(throw new AssertionError(s"expected failure, got: $outcome"))
          val issue = typerIssues(issues).collectFirst { case i: TyperIssue.TemplateNotInstantiated => i }
            .getOrElse(throw new AssertionError(s"no TemplateNotInstantiated in: $issues"))
          assert(issue.templateName == "X", s"expected templateName='X', got '${issue.templateName}'")
          assert(issue.aliasName == "Y", s"expected aliasName='Y',    got '${issue.aliasName}'")
        }
    }

    // ── Matrix #8 ────────────────────────────────────────────────────────────

    "matrix #8: produce NotATemplate for `type Y = i32[str]`" in {
      (parser: BaboonParser[F], typer: BaboonTyper[F]) =>
        for {
          outcome <- runTyperFor(parser, typer, matrix8NotATemplate)
        } yield {
          assertProducesTyperIssue[TyperIssue.NotATemplate](outcome)
        }
    }

    "matrix #8: NotATemplate carries head=i32 and aliasName=Y" in {
      (parser: BaboonParser[F], typer: BaboonTyper[F]) =>
        for {
          outcome <- runTyperFor(parser, typer, matrix8NotATemplate)
        } yield {
          val issues = outcome.left.getOrElse(throw new AssertionError(s"expected failure, got: $outcome"))
          val issue = typerIssues(issues).collectFirst { case i: TyperIssue.NotATemplate => i }
            .getOrElse(throw new AssertionError(s"no NotATemplate in: $issues"))
          assert(issue.head == "i32", s"expected head='i32', got '${issue.head}'")
          assert(issue.aliasName == "Y", s"expected aliasName='Y', got '${issue.aliasName}'")
        }
    }

    "matrix #8: builtin collections do NOT trigger NotATemplate (positive regression)" in {
      (parser: BaboonParser[F], typer: BaboonTyper[F]) =>
        for {
          outcome <- runTyperFor(parser, typer, matrix8BuiltinPositive)
        } yield {
          val domain = outcome.toOption.getOrElse(throw new AssertionError(s"expected success, got: $outcome"))
          val names  = domain.defs.meta.nodes.keys.collect { case u: io.septimalmind.baboon.typer.model.TypeId.User => u.name.name }.toSet
          assert(names.contains("IntHolder"), s"expected IntHolder in domain user types, got: $names")
        }
    }

    // ── D03: body-side derived ────────────────────────────────────────────────

    "D03: produce TemplateBodyCarriesDerived for template body with `:derived[json]`" in {
      (parser: BaboonParser[F], typer: BaboonTyper[F]) =>
        for {
          outcome <- runTyperFor(parser, typer, d03TemplateBodyCarriesDerived)
        } yield {
          assertProducesTyperIssue[TyperIssue.TemplateBodyCarriesDerived](outcome)
        }
    }

    "D03: TemplateBodyCarriesDerived carries templateName=Page" in {
      (parser: BaboonParser[F], typer: BaboonTyper[F]) =>
        for {
          outcome <- runTyperFor(parser, typer, d03TemplateBodyCarriesDerived)
        } yield {
          val issues = outcome.left.getOrElse(throw new AssertionError(s"expected failure, got: $outcome"))
          val issue = typerIssues(issues).collectFirst { case i: TyperIssue.TemplateBodyCarriesDerived => i }
            .getOrElse(throw new AssertionError(s"no TemplateBodyCarriesDerived in: $issues"))
          assert(issue.templateName == "Page", s"expected templateName='Page', got '${issue.templateName}'")
        }
    }

    // ── Matrix #2 regression ──────────────────────────────────────────────────

    "matrix #2 regression: produce TemplateInstantiationInForbiddenPosition for nested template in alias RHS args `type Y = X[Z[i32]]`" in {
      (parser: BaboonParser[F], typer: BaboonTyper[F]) =>
        for {
          outcome <- runTyperFor(parser, typer, matrix2NestedTemplateInArgs)
        } yield {
          assertProducesTyperIssue[TyperIssue.TemplateInstantiationInForbiddenPosition](outcome)
        }
    }

    // ── D01: matrix #8 with non-template user DTO ─────────────────────────────

    "matrix #8 (D01): produce NotATemplate for `type Y = MyDto[i32]` where MyDto is a non-template user DTO" in {
      (parser: BaboonParser[F], typer: BaboonTyper[F]) =>
        for {
          outcome <- runTyperFor(parser, typer, matrix8NotATemplateUserDto)
        } yield {
          val issues = outcome.left.getOrElse(throw new AssertionError(s"expected failure, got: $outcome"))
          val issue = typerIssues(issues).collectFirst { case i: TyperIssue.NotATemplate => i }
            .getOrElse(throw new AssertionError(s"no NotATemplate in: $issues"))
          assert(issue.head == "MyDto", s"expected head='MyDto', got '${issue.head}'")
          assert(issue.aliasName == "Y", s"expected aliasName='Y', got '${issue.aliasName}'")
        }
    }

    // ── D02: template without derived — success ───────────────────────────────

    "D02: template without `:derived[…]` succeeds (no TemplateBodyCarriesDerived)" in {
      (parser: BaboonParser[F], typer: BaboonTyper[F]) =>
        for {
          outcome <- runTyperFor(parser, typer, d02TemplateNoDerivedPositive)
        } yield {
          // Outcome must be Right — template without derived must not be rejected.
          assert(outcome.isRight, s"expected success, got: $outcome")
        }
    }

    // ── D03: non-template alias — success ─────────────────────────────────────

    "D03: non-template alias `type Y = X` where X is a plain DTO succeeds (no TemplateNotInstantiated)" in {
      (parser: BaboonParser[F], typer: BaboonTyper[F]) =>
        for {
          outcome <- runTyperFor(parser, typer, d03NonTemplateAliasPositive)
        } yield {
          // Pipeline must succeed — plain-DTO alias must not be rejected.
          // Note: `type Y = X` resolves as a structural alias; no separate TypeId.User for Y is
          // emitted in domain.defs.meta.nodes — the domain contains X but not Y.
          assert(outcome.isRight, s"expected success, got: $outcome")
        }
    }
  }
}
