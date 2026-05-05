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

  /** PR-29.15 Site A: `ns foo { data X[T] { f: T } } type Y = foo.X`
    * Prefixed bare ref to a namespace-scoped template without type args — must fire TemplateNotInstantiated.
    */
  private val pr2915SiteA_PrefixedBareRef: String =
    """model m29.validator.pr2915.siteA
      |
      |version "1.0.0"
      |
      |ns foo { data X[T] { f: T } }
      |type Y = foo.X
      |""".stripMargin

  /** PR-29.15 Site B: `ns foo { data Inner[T] { f: T } } data X[T] { f: T } type Y = X[foo.Inner[i32]]`
    * Nested template instantiation in alias-RHS args where the arg is in a different namespace —
    * must fire TemplateInstantiationInForbiddenPosition.
    */
  private val pr2915SiteB_PrefixedNestedArg: String =
    """model m29.validator.pr2915.siteB
      |
      |version "1.0.0"
      |
      |ns foo { data Inner[T] { f: T } }
      |data X[T] { g: T }
      |type Y = X[foo.Inner[i32]]
      |""".stripMargin

  /** PR-29.15 Site C: in-body field-position template instantiation with a namespace-qualified ref.
    * `data Y[T] { rec: foo.X[T] }` with an instantiating alias `type Z = Y[i32]` —
    * the check fires during materialisation of Y[i32], detecting that the body contains
    * a forbidden in-body instantiation of the namespaced template foo.X.
    */
  private val pr2915SiteC_PrefixedInBodyRef: String =
    """model m29.validator.pr2915.siteC
      |
      |version "1.0.0"
      |
      |ns foo { data X[T] { f: T } }
      |data Y[T] { rec: foo.X[T] }
      |type Z = Y[i32]
      |""".stripMargin

  /** PR-29.15-D03 regression: sibling template bare-name ref inside a namespace body.
    * `data Y[T] { rec: X[T] }` where X is also in `ns foo` — bare X reference must fire
    * TemplateInstantiationInForbiddenPosition even though X is not at Owner.Toplevel.
    * The empty-prefix code path in substituteTypeRef must use any-owner lookup (not Toplevel-only).
    */
  private val pr2915D03_CrossNsInBodyRegression: String =
    """model template.crossns.inbody.regression
      |
      |version "1.0.0"
      |
      |ns foo {
      |  data X[T] { f: T }
      |  data Y[T] { rec: X[T] }
      |}
      |type Z = foo.Y[i32]
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

  // ─── M33 §4 negative-path fixtures ───────────────────────────────────────────

  /** Row 1: `+ PlainDto[i32]` where the head resolves to a plain DTO (not a template).
    * Expected: `TyperIssue.NotATemplate` (the structural-arm lookup misses the template
    * registry because `PlainDto` was never registered as a template). User-type name and
    * diagnostic case-name are deliberately distinct so the assertion surface clearly
    * distinguishes "the diagnostic case is NotATemplate" from "the head name was …". */
  private val m33bad1NotATemplate: String =
    """model m33.bad1
      |
      |version "1.0.0"
      |
      |data PlainDto { v: str }
      |
      |root data Receiver {
      |  + PlainDto[i32]
      |}
      |""".stripMargin

  /** Row 2 (PR-33.3-D04): `+ MyGen` (no brackets) where MyGen IS a registered template.
    * Expected: `TyperIssue.TemplateNotInstantiated` with `templateName == "MyGen"` and
    * `aliasName == "X"`. Pins the diagnostic at the M29Validator level (i.e. fires through
    * the validator-level entry point in `validateNoBareTemplateRefs`). */
  private val m33bad2TemplateNotInstantiated: String =
    """model m33.bad2
      |
      |version "1.0.0"
      |
      |data MyGen[T] { v: T }
      |
      |root data X {
      |  + MyGen
      |}
      |""".stripMargin

  /** Row 5 / §3.d positive (PR-33.3-D05): cross-namespace structural-arm `+` instantiation.
    * `ns foo { data NsT[T] { v: T } } root data Receiver { + foo.NsT[i32] }` — the prefix
    * `foo.` resolves to `Owner.Ns(["foo"])`; PR-29.15 hardened the resolver so this case
    * works. Pins that breaking the prefix→Owner.Ns conversion in `lowerOneArm` would
    * regress this positive scenario. */
  private val m33ok5CrossNsStructuralArmPlus: String =
    """model m33.ok5
      |
      |version "1.0.0"
      |
      |ns foo { data NsT[T] { v: T } }
      |
      |root data Receiver {
      |  + foo.NsT[i32]
      |}
      |""".stripMargin

  /** Row 7 (PR-33.3-D06): template self-instantiation via structural arm.
    * `data X[T] { + X[T] }; root type Y = X[i32]`. Per §3.f the cycle-detection set in
    * `lowerOneArm` fires `TyperIssue.CircularInheritance`. The synthetic matrix carries one
    * edge so `niceList()` renders a non-empty diagnostic. */
  private val m33bad7TemplateSelfInstantiation: String =
    """model m33.bad7
      |
      |version "1.0.0"
      |
      |data X[T] {
      |  + X[T]
      |}
      |
      |root type Y = X[i32]
      |""".stripMargin

  /** Row 3: `+ MyGen[i32, str]` where `MyGen` is a 1-arg template — arity mismatch.
    * Expected: `TyperIssue.TemplateArityMismatch`. */
  private val m33bad3ArityMismatch: String =
    """model m33.bad3
      |
      |version "1.0.0"
      |
      |data MyGen[T] { v: T }
      |
      |root data Receiver {
      |  + MyGen[i32, str]
      |}
      |""".stripMargin

  /** Row 4: forbidden type-arg in structural-arm position (matrix #2).
    *
    * `data MyGen[T] { v: T }; data Other[T] { o: T }; root data Receiver { + MyGen[Other[i32]] }`.
    * The `+ MyGen[…]` arm is a structural-arm template instantiation; its single argument
    * `Other[i32]` is itself a Constructor whose head names a registered template — this is
    * matrix #2 (template instantiation in arg position; spec §4 / locked decision #3).
    * Expected: `TyperIssue.TemplateInstantiationInForbiddenPosition` with
    * `containingTemplateName == "MyGen"` and `instantiatedName == "Other"`.
    *
    * This mirrors `processMember`'s alias-RHS matrix-#2 walk (lines 686-708) at the
    * structural-arm position in `lowerOneArm`.
    */
  private val m33bad4ForbiddenTypeArg: String =
    """model m33.bad4
      |
      |version "1.0.0"
      |
      |data Other[T] { o: T }
      |data MyGen[T] { v: T }
      |
      |root data Receiver {
      |  + MyGen[Other[i32]]
      |}
      |""".stripMargin

  /** Row 6: namespace-prefix miss in structural-arm template instantiation.
    *
    * `+ otherpkg.OtherTemplate[i32]` — the prefix `otherpkg` resolves to
    * `Owner.Ns(["otherpkg"])` for the registry lookup. `OtherTemplate` is defined at
    * `Owner.Toplevel` (not inside any namespace), so the lookup
    * `(pkg, Owner.Ns(["otherpkg"]), TypeName("OtherTemplate"))` misses → `NotATemplate`.
    *
    * This is single-Pkg cross-namespace prefix miss; true multi-Pkg cross-package
    * instantiation is out of scope per spec §6 item 11.
    */
  private val m33bad6NamespacePrefixMiss: String =
    """model m33.bad6
      |
      |version "1.0.0"
      |
      |data OtherTemplate[T] { v: T }
      |
      |root data Receiver {
      |  + otherpkg.OtherTemplate[i32]
      |}
      |""".stripMargin

  /** Row 9: mutual recursion — `A[U] { + B[U] }; B[U] { + A[U] }` instantiated as `X = A[i32]`.
    *
    * Pass 1 (alias instantiation) materialises `X` from `A[i32]` with body `{ + B[i32] }`.
    * Pass 2 (structural-arm lowering) descends into `B[i32]`, substitutes its body `{ + A[i32] }`,
    * then tries `(X, B, i32)` which is already in the cycle-detection set → `CircularInheritance`.
    *
    * Verifies: (a) no stack overflow, (b) `CircularInheritance` is emitted,
    * (c) the rendered matrix mentions `B` (the template being lowered when the cycle fires).
    */
  private val m33bad9MutualRecursion: String =
    """model m33.bad9
      |
      |version "1.0.0"
      |
      |data A[U] {
      |  + B[U]
      |}
      |
      |data B[U] {
      |  + A[U]
      |}
      |
      |root type X = A[i32]
      |""".stripMargin

  /** Row 11: duplicate inline arm — `+ MyGen[i32]` twice in the same DTO.
    *
    * Both arms inline the same field `v: i32`; `BaboonTranslator` detects the
    * duplicate field names and fires `NonUniqueFields`.
    */
  private val m33bad11DuplicateArm: String =
    """model m33.bad11
      |
      |version "1.0.0"
      |
      |data MyGen[T] { v: T }
      |
      |root data X {
      |  + MyGen[i32]
      |  + MyGen[i32]
      |}
      |""".stripMargin

  /** Row 12: inline-then-collide — `+ MyGen[i32]` inlines `v: i32` which collides with
    * a locally declared field `v: str`. `BaboonTranslator` fires `NonUniqueFields`.
    */
  private val m33bad12InlineThenCollide: String =
    """model m33.bad12
      |
      |version "1.0.0"
      |
      |data MyGen[T] { v: T }
      |
      |root data X {
      |  + MyGen[i32]
      |  v: str
      |}
      |""".stripMargin

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

    // ── PR-29.15: prefixed cross-namespace detection ───────────────────────────

    // NOTE: prefixed forms drop the ns-prefix in the diagnostic's templateName/instantiatedName
    // field; the diagnostic location (meta.pos) carries the source position that allows the
    // reader to map back to the prefixed form.

    "PR-29.15 Site A: produce TemplateNotInstantiated for `type Y = foo.X` where foo.X is a namespaced template (matrix #7 prefixed)" in {
      (parser: BaboonParser[F], typer: BaboonTyper[F]) =>
        for {
          outcome <- runTyperFor(parser, typer, pr2915SiteA_PrefixedBareRef)
        } yield {
          // templateName is the simple name extracted via simple.name.name (prefix dropped).
          val issues = outcome.left.getOrElse(throw new AssertionError(s"expected failure, got: $outcome"))
          val issue = typerIssues(issues).collectFirst { case i: TyperIssue.TemplateNotInstantiated => i }
            .getOrElse(throw new AssertionError(s"no TemplateNotInstantiated in: $issues"))
          assert(issue.templateName == "X", s"expected templateName='X' (prefix dropped), got '${issue.templateName}'")
          assert(issue.aliasName == "Y", s"expected aliasName='Y', got '${issue.aliasName}'")
        }
    }

    "PR-29.15 Site B: produce TemplateInstantiationInForbiddenPosition for `type Y = X[foo.Inner[i32]]` where foo.Inner is a namespaced template (matrix #2 prefixed)" in {
      (parser: BaboonParser[F], typer: BaboonTyper[F]) =>
        for {
          outcome <- runTyperFor(parser, typer, pr2915SiteB_PrefixedNestedArg)
        } yield {
          // containingTemplateName is the outer alias template key (templateKey._3.name = "X");
          // instantiatedName is the bad nested arg's simple name (argCtor.name.name = "Inner", prefix dropped).
          val issues = outcome.left.getOrElse(throw new AssertionError(s"expected failure, got: $outcome"))
          val issue = typerIssues(issues).collectFirst { case i: TyperIssue.TemplateInstantiationInForbiddenPosition => i }
            .getOrElse(throw new AssertionError(s"no TemplateInstantiationInForbiddenPosition in: $issues"))
          assert(issue.containingTemplateName == "X", s"expected containingTemplateName='X', got '${issue.containingTemplateName}'")
          assert(issue.instantiatedName == "Inner", s"expected instantiatedName='Inner' (prefix dropped), got '${issue.instantiatedName}'")
        }
    }

    "PR-29.15 Site C: produce TemplateInstantiationInForbiddenPosition for `data Y[T] { rec: foo.X[T] }` where foo.X is a namespaced template (matrix #1 prefixed)" in {
      (parser: BaboonParser[F], typer: BaboonTyper[F]) =>
        for {
          outcome <- runTyperFor(parser, typer, pr2915SiteC_PrefixedInBodyRef)
        } yield {
          // containingTemplateName is the template being instantiated by the alias (templateKey._3.name = "Y");
          // instantiatedName is the forbidden in-body ref's simple name (name.name = "X", prefix dropped).
          val issues = outcome.left.getOrElse(throw new AssertionError(s"expected failure, got: $outcome"))
          val issue = typerIssues(issues).collectFirst { case i: TyperIssue.TemplateInstantiationInForbiddenPosition => i }
            .getOrElse(throw new AssertionError(s"no TemplateInstantiationInForbiddenPosition in: $issues"))
          assert(issue.containingTemplateName == "Y", s"expected containingTemplateName='Y', got '${issue.containingTemplateName}'")
          assert(issue.instantiatedName == "X", s"expected instantiatedName='X' (prefix dropped), got '${issue.instantiatedName}'")
        }
    }

    "PR-29.15-D03 regression: TemplateInstantiationInForbiddenPosition for bare sibling template ref inside namespace body (`ns foo { data X[T]; data Y[T] { rec: X[T] } }`)" in {
      (parser: BaboonParser[F], typer: BaboonTyper[F]) =>
        for {
          outcome <- runTyperFor(parser, typer, pr2915D03_CrossNsInBodyRegression)
        } yield {
          // X is a sibling template in Owner.Ns("foo"), not Owner.Toplevel.
          // Empty-prefix substituteTypeRef must use any-owner lookup to catch this.
          val issues = outcome.left.getOrElse(throw new AssertionError(s"expected failure, got: $outcome"))
          val issue = typerIssues(issues).collectFirst { case i: TyperIssue.TemplateInstantiationInForbiddenPosition => i }
            .getOrElse(throw new AssertionError(s"no TemplateInstantiationInForbiddenPosition in: $issues"))
          assert(issue.containingTemplateName == "Y", s"expected containingTemplateName='Y', got '${issue.containingTemplateName}'")
          assert(issue.instantiatedName == "X", s"expected instantiatedName='X', got '${issue.instantiatedName}'")
        }
    }

    // ─── M33 §4 negative-path tests (PR-33.3) ─────────────────────────────────

    // Row 1: + PlainDto[i32] where head is a plain DTO
    "m33_bad_1: produce NotATemplate for `+ PlainDto[i32]` where PlainDto is a plain DTO (structural-arm position)" in {
      (parser: BaboonParser[F], typer: BaboonTyper[F]) =>
        for {
          outcome <- runTyperFor(parser, typer, m33bad1NotATemplate, "m33-bad-1-not-a-template.baboon")
        } yield {
          val issues = outcome.left.getOrElse(throw new AssertionError(s"expected failure, got: $outcome"))
          val issue = typerIssues(issues).collectFirst { case i: TyperIssue.NotATemplate => i }
            .getOrElse(throw new AssertionError(s"no NotATemplate in: $issues"))
          assert(issue.head == "PlainDto", s"expected head='PlainDto', got '${issue.head}'")
          assert(issue.aliasName == "Receiver", s"expected aliasName='Receiver', got '${issue.aliasName}'")
        }
    }

    // Row 2: + MyGen (no brackets) where MyGen IS a registered template
    "m33_bad_2_template_not_instantiated" in {
      (parser: BaboonParser[F], typer: BaboonTyper[F]) =>
        for {
          outcome <- runTyperFor(parser, typer, m33bad2TemplateNotInstantiated, "m33-bad-2-template-not-instantiated.baboon")
        } yield {
          // Pinned at the M29Validator-level entry point: validateNoBareTemplateRefs walks the
          // receiving DTO's member list and emits TemplateNotInstantiated for any `+/-/^ Foo`
          // arm whose head names a registered template AND whose `args = None`.
          val issues = outcome.left.getOrElse(throw new AssertionError(s"expected failure, got: $outcome"))
          val issue = typerIssues(issues).collectFirst { case i: TyperIssue.TemplateNotInstantiated => i }
            .getOrElse(throw new AssertionError(s"no TemplateNotInstantiated in: $issues"))
          assert(issue.templateName == "MyGen", s"expected templateName='MyGen', got '${issue.templateName}'")
          assert(issue.aliasName == "X", s"expected aliasName='X', got '${issue.aliasName}'")
        }
    }

    // Row 3: + MyGen[i32, str] (arity mismatch — MyGen has 1 param, 2 args supplied)
    "m33_bad_3: produce TemplateArityMismatch for `+ MyGen[i32, str]` where MyGen[T] has arity 1 (structural-arm position)" in {
      (parser: BaboonParser[F], typer: BaboonTyper[F]) =>
        for {
          outcome <- runTyperFor(parser, typer, m33bad3ArityMismatch, "m33-bad-3-arity.baboon")
        } yield {
          val issues = outcome.left.getOrElse(throw new AssertionError(s"expected failure, got: $outcome"))
          val issue = typerIssues(issues).collectFirst { case i: TyperIssue.TemplateArityMismatch => i }
            .getOrElse(throw new AssertionError(s"no TemplateArityMismatch in: $issues"))
          assert(issue.templateName == "MyGen", s"expected templateName='MyGen', got '${issue.templateName}'")
          assert(issue.expected == 1, s"expected expected=1, got ${issue.expected}")
          assert(issue.actual == 2, s"expected actual=2, got ${issue.actual}")
        }
    }

    // Row 4: forbidden type-arg — `+ MyGen[Other[i32]]` (matrix #2: template-in-arg)
    "m33_bad_4: produce TemplateInstantiationInForbiddenPosition for `+ MyGen[Other[i32]]` where Other is a registered template in arg position (structural-arm matrix #2)" in {
      (parser: BaboonParser[F], typer: BaboonTyper[F]) =>
        for {
          outcome <- runTyperFor(parser, typer, m33bad4ForbiddenTypeArg, "m33-bad-4-forbidden-type-arg.baboon")
        } yield {
          // The `+ MyGen[…]` arm carries arg `Other[i32]` — Other is a registered template,
          // so the arg itself is a template instantiation. lowerOneArm's matrix-#2 walk
          // mirrors processMember's alias-RHS check (lines 686-708) and emits
          // TemplateInstantiationInForbiddenPosition with containingTemplateName=MyGen,
          // instantiatedName=Other.
          val issues = outcome.left.getOrElse(throw new AssertionError(s"expected failure, got: $outcome"))
          val issue = typerIssues(issues).collectFirst { case i: TyperIssue.TemplateInstantiationInForbiddenPosition => i }
            .getOrElse(throw new AssertionError(s"no TemplateInstantiationInForbiddenPosition in: $issues"))
          assert(issue.containingTemplateName == "MyGen", s"expected containingTemplateName='MyGen', got '${issue.containingTemplateName}'")
          assert(issue.instantiatedName == "Other", s"expected instantiatedName='Other', got '${issue.instantiatedName}'")
        }
    }

    // Row 6: namespace-prefix miss — prefix-qualified head misses the template registry
    "m33_bad_6: produce NotATemplate for `+ otherpkg.OtherTemplate[i32]` where OtherTemplate is at Owner.Toplevel (namespace-prefix miss)" in {
      (parser: BaboonParser[F], typer: BaboonTyper[F]) =>
        for {
          outcome <- runTyperFor(parser, typer, m33bad6NamespacePrefixMiss, "m33-bad-6-namespace-prefix-miss.baboon")
        } yield {
          // OtherTemplate is registered as (pkg, Owner.Toplevel, TypeName("OtherTemplate")).
          // The structural-arm lookup for `+ otherpkg.OtherTemplate[i32]` resolves the prefix to
          // Owner.Ns(["otherpkg"]), producing key (pkg, Ns(["otherpkg"]), "OtherTemplate") — a miss.
          // Pins "namespace-prefix miss" wording; true multi-Pkg cross-package case is deferred.
          val issues = outcome.left.getOrElse(throw new AssertionError(s"expected failure, got: $outcome"))
          val issue = typerIssues(issues).collectFirst { case i: TyperIssue.NotATemplate => i }
            .getOrElse(throw new AssertionError(s"no NotATemplate in: $issues"))
          assert(issue.head == "OtherTemplate", s"expected head='OtherTemplate', got '${issue.head}'")
          assert(issue.aliasName == "Receiver", s"expected aliasName='Receiver', got '${issue.aliasName}'")
        }
    }

    // Row 5 / §3.d positive: cross-namespace structural-arm `+` instantiation succeeds
    "m33_ok_cross_ns_structural_arm_plus" in {
      (parser: BaboonParser[F], typer: BaboonTyper[F]) =>
        for {
          outcome <- runTyperFor(parser, typer, m33ok5CrossNsStructuralArmPlus, "m33-ok-cross-ns-structural-arm-plus.baboon")
        } yield {
          // PR-29.15 hardened the resolver: `+ foo.NsT[i32]` → Owner.Ns(["foo"]) lookup hits.
          // After lowering, Receiver carries the substituted field `v: i32`.
          val domain = outcome.toOption.getOrElse(throw new AssertionError(s"expected success, got: $outcome"))
          val receiverName = io.septimalmind.baboon.typer.model.TypeName("Receiver")
          val receiverEntry = domain.defs.meta.nodes.collectFirst {
            case (id: io.septimalmind.baboon.typer.model.TypeId.User, dm) if id.name == receiverName => dm
          }.getOrElse(throw new AssertionError(s"no Receiver in domain: ${domain.defs.meta.nodes.keys}"))
          val dto = receiverEntry match {
            case io.septimalmind.baboon.typer.model.DomainMember.User(_, defn: io.septimalmind.baboon.typer.model.Typedef.Dto, _, _) => defn
            case other => throw new AssertionError(s"expected Receiver to be Typedef.Dto, got: $other")
          }
          val fieldNames = dto.fields.map(_.name.name).toSet
          assert(fieldNames.contains("v"), s"expected Receiver to have field 'v', got: $fieldNames")
          val vField = dto.fields.find(_.name.name == "v").get
          val isI32 = vField.tpe match {
            case io.septimalmind.baboon.typer.model.TypeRef.Scalar(id) => id.name.name == "i32"
            case _                                                     => false
          }
          assert(isI32, s"expected v: i32 in Receiver, got: ${vField.tpe}")
        }
    }

    // Row 7: template self-instantiation via structural arm — `data X[T] { + X[T] }`
    "m33_bad_7_template_self_instantiation" in {
      (parser: BaboonParser[F], typer: BaboonTyper[F]) =>
        for {
          outcome <- runTyperFor(parser, typer, m33bad7TemplateSelfInstantiation, "m33-bad-7-template-self-instantiation.baboon")
        } yield {
          // The cycle-detection set in `lowerOneArm` fires on the second iteration of the
          // self-recursion. Per §3.f the diagnostic is the existing `CircularInheritance`.
          assertProducesTyperIssue[TyperIssue.CircularInheritance](outcome)
          val ti = outcome.left.toOption.toList.flatMap(_.toList).collect { case BaboonIssue.Typer(t: TyperIssue.CircularInheritance) => t }
          assert(ti.nonEmpty, s"expected a CircularInheritance issue, got: $outcome")
          val matrix = ti.head.error match {
            case izumi.fundamentals.graphs.ToposortError.UnexpectedLoop(_, m) => m.links
            case _                                                            => Map.empty[io.septimalmind.baboon.typer.model.TypeId.User, Set[io.septimalmind.baboon.typer.model.TypeId.User]]
          }
          assert(matrix.nonEmpty, s"expected non-empty matrix in CircularInheritance payload")
          val rendered = matrix.flatMap { case (t, cs) => Iterator(t.name.name) ++ cs.iterator.map(_.name.name) }.toSet
          assert(rendered.contains("X"), s"expected matrix to mention 'X', got: $rendered")
        }
    }

    // Row 9: mutual recursion — A[U]{+B[U]}; B[U]{+A[U]}; type X = A[i32]
    "m33_bad_9: mutual-recursion structural arms terminate with CircularInheritance (no stack overflow); matrix mentions B" in {
      (parser: BaboonParser[F], typer: BaboonTyper[F]) =>
        for {
          outcome <- runTyperFor(parser, typer, m33bad9MutualRecursion, "m33-bad-9-mutual-recursion.baboon")
        } yield {
          // Pass 1 materialises X from A[i32] with body {+ B[i32]}.
          // Pass 2 lowers B[i32], substitutes body {+ A[i32]}, recurses; on the next iteration
          // cycleKey (X, B, i32) hits the cycle-detection set → CircularInheritance.
          assertProducesTyperIssue[TyperIssue.CircularInheritance](outcome)
          val ti = outcome.left.toOption.toList.flatMap(_.toList).collect { case BaboonIssue.Typer(t: TyperIssue.CircularInheritance) => t }
          assert(ti.nonEmpty, s"expected a CircularInheritance issue, got: $outcome")
          val matrix = ti.head.error match {
            case izumi.fundamentals.graphs.ToposortError.UnexpectedLoop(_, m) => m.links
            case _                                                            => Map.empty[io.septimalmind.baboon.typer.model.TypeId.User, Set[io.septimalmind.baboon.typer.model.TypeId.User]]
          }
          assert(matrix.nonEmpty, s"expected non-empty matrix in CircularInheritance payload")
          val rendered = matrix.flatMap { case (t, cs) => Iterator(t.name.name) ++ cs.iterator.map(_.name.name) }.toSet
          // The cycle fires when (X, B, i32) is found in cycleSet; the matrix encodes X → B.
          assert(rendered.exists(n => n == "B" || n == "A"), s"expected matrix to mention 'A' or 'B', got: $rendered")
        }
    }

    // Row 11: duplicate inline arm — same template + same args twice.
    // PR-33.3-D01 fix: the pre-`.distinct` duplicate-name check in BaboonTranslator.convertDto
    // now fires NonUniqueFields when two arms produce fields with the same name, even when the
    // fields are type-identical. `+ MyGen[i32]; + MyGen[i32]` produces two `v: i32` entries
    // in `converted`; the groupBy check catches them before `.distinct` erases the duplicate.
    "m33_bad_11: NonUniqueFields for duplicate template arm + MyGen[i32]; + MyGen[i32]" in {
      (parser: BaboonParser[F], typer: BaboonTyper[F]) =>
        for {
          outcome <- runTyperFor(parser, typer, m33bad11DuplicateArm, "m33-bad-11-duplicate-arm.baboon")
        } yield {
          assertProducesTyperIssue[TyperIssue.NonUniqueFields](outcome)
        }
    }

    // Row 12: inline-then-collide — + MyGen[i32] inlines v:i32 colliding with explicit v:str
    "m33_bad_12: produce NonUniqueFields for `+ MyGen[i32]; v: str` where MyGen[T] produces v:T" in {
      (parser: BaboonParser[F], typer: BaboonTyper[F]) =>
        for {
          outcome <- runTyperFor(parser, typer, m33bad12InlineThenCollide, "m33-bad-12-inline-collide.baboon")
        } yield {
          // + MyGen[i32] inlines v:i32; the explicit field v:str collides → NonUniqueFields.
          assertProducesTyperIssue[TyperIssue.NonUniqueFields](outcome)
        }
    }
  }
}
