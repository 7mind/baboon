package io.septimalmind.baboon.tests

import io.septimalmind.baboon.parser.BaboonParser
import io.septimalmind.baboon.parser.model.FSPath
import io.septimalmind.baboon.parser.model.issues.{BaboonIssue, TyperIssue}
import io.septimalmind.baboon.tests.BaboonTest.BaboonTestModule
import io.septimalmind.baboon.typer.BaboonTyper
import io.septimalmind.baboon.typer.model.*
import izumi.functional.bio.{Error2, F}
import izumi.fundamentals.collections.nonempty.{NEList, NEString}
import izumi.reflect.TagKK

import scala.reflect.ClassTag

/** T38 (auto-extracted-contracts) typer tests for the synthesis pass.
  *
  * Exercises the full path: parse → template-registry pass → ADT inheritance expander →
  * monomorphisation → T38 extraction synthesis → scope-build → type-check.
  *
  * Acceptance (verbatim from the task brief):
  *   (a) B exists as `Typedef.Contract` with EXACTLY the param-free resolved fields — direct
  *       fields, `+ ConcreteParent` contribution, `is C` materialization, and the through-parent
  *       case `+ Pair[T, i32]` → `second: i32` kept while `first: T` dropped — for a templated
  *       `data` host AND a templated `id` host.
  *   (b) `data: T` / `lst[T]` / `map[str, T]` / `Pair[T, i32]` fields all excluded.
  *   (c) host-invalid (incl. a NON-templated `id` body), name-collision (vs type, template,
  *       sibling extraction), empty-B errors each fire their dedicated TyperIssue.
  *   (d) zero-`has`-clause model → unchanged TyperOutput (no-op).
  */
final class T38ExtractionSynthesisTest extends T38ExtractionSynthesisTestBase[Either]

abstract class T38ExtractionSynthesisTestBase[F[+_, +_]: Error2: TagKK: BaboonTestModule] extends BaboonTest[F] {

  private def makeInput(name: String, content: String): BaboonParser.Input =
    BaboonParser.Input(FSPath.parse(NEString.unsafeFrom(name)), content)

  private def runTyperFor(
    parser: BaboonParser[F],
    typer: BaboonTyper[F],
    content: String,
    name: String = "t38-extraction-test.baboon",
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

  private def userTypeNames(domain: Domain): Set[String] =
    domain.defs.meta.nodes.keys.collect { case u: TypeId.User => u.name.name }.toSet

  private def findContract(domain: Domain, name: String): Typedef.Contract = {
    val id = domain.defs.meta.nodes.keys.collectFirst {
      case u: TypeId.User if u.name.name == name => u
    }.getOrElse(throw new AssertionError(s"$name not found in domain; user types: ${userTypeNames(domain)}"))
    domain.defs.meta.nodes(id) match {
      case u: DomainMember.User =>
        u.defn match {
          case c: Typedef.Contract => c
          case other               => throw new AssertionError(s"expected Typedef.Contract for $name, got: $other")
        }
      case other => throw new AssertionError(s"expected DomainMember.User for $name, got: $other")
    }
  }

  private def fieldNames(c: Typedef.Contract): Set[String] = c.fields.map(_.name.name).toSet

  private val i32 = TypeRef.Scalar(TypeId.Builtins.i32)
  private val str = TypeRef.Scalar(TypeId.Builtins.str)

  // ─── fixtures ──────────────────────────────────────────────────────────────

  /** (a)+(b): templated `data` host with every flavour of param-free / param-bearing member. */
  private val dataHostDomain: String =
    """model t38.data.host
      |
      |version "1.0.0"
      |
      |data Pair[A, B] {
      |  first: A
      |  second: B
      |}
      |
      |contract C {
      |  cf: str
      |}
      |
      |data ConcreteParent {
      |  pf: i32
      |}
      |
      |data Box[T] {
      |  x: i32
      |  data: T
      |  xs: lst[T]
      |  m: map[str, T]
      |  + Pair[T, i32]
      |  + ConcreteParent
      |  is C
      |  has contract BoxView
      |}
      |
      |root type IntBox = Box[i32]
      |
      |// Pull the synthesized BoxView into the root-reachable set. T38 does NOT wire `is B` on the
      |// host (that is T40's job), so reference it from a TEMPLATED probe whose instantiation lands
      |// in the post-`instantiate` member list — alongside BoxView — so its `is BoxView` resolves in
      |// the SECOND scope-build/order (a hand-written non-template `is BoxView` would instead fail
      |// the FIRST order, which runs before synthesis).
      |data Probe[T] {
      |  is BoxView
      |}
      |
      |root type IntProbe = Probe[i32]
      |""".stripMargin

  /** (a)+(b): templated `id` host. */
  private val idHostDomain: String =
    """model t38.id.host
      |
      |version "1.0.0"
      |
      |data Pair[A, B] {
      |  first: A
      |  second: B
      |}
      |
      |id Key[T] {
      |  x: i32
      |  data: T
      |  + Pair[T, i32]
      |  has contract KeyView
      |}
      |
      |root type IntKey = Key[i32]
      |
      |data Probe[T] {
      |  is KeyView
      |}
      |
      |root type IntProbe = Probe[i32]
      |""".stripMargin

  /** (c) host-invalid: a NON-templated `id` body carrying a `has` clause. */
  private val nonTemplatedIdHostDomain: String =
    """model t38.neg.nontemplated.id
      |
      |version "1.0.0"
      |
      |id Plain {
      |  x: i32
      |  has contract PlainView
      |}
      |""".stripMargin

  /** (c) host-invalid: a `has` clause in a (templated) contract body. */
  private val contractHostDomain: String =
    """model t38.neg.contract.host
      |
      |version "1.0.0"
      |
      |contract Ctr[T] {
      |  x: i32
      |  has contract CtrView
      |}
      |
      |root type IntCtr = Ctr[i32]
      |""".stripMargin

  /** (c) name-collision: extraction name equals an existing type. */
  private val collisionWithTypeDomain: String =
    """model t38.neg.collision.type
      |
      |version "1.0.0"
      |
      |data Existing {
      |  e: i32
      |}
      |
      |data Box[T] {
      |  x: i32
      |  data: T
      |  has contract Existing
      |}
      |
      |root type IntBox = Box[i32]
      |""".stripMargin

  /** (c) name-collision: extraction name equals a template name. */
  private val collisionWithTemplateDomain: String =
    """model t38.neg.collision.template
      |
      |version "1.0.0"
      |
      |data Other[T] {
      |  o: T
      |}
      |
      |data Box[T] {
      |  x: i32
      |  data: T
      |  has contract Other
      |}
      |
      |root type IntBox = Box[i32]
      |""".stripMargin

  /** (c) name-collision: two sibling extractions request the same name. */
  private val collisionWithSiblingDomain: String =
    """model t38.neg.collision.sibling
      |
      |version "1.0.0"
      |
      |data Box[T] {
      |  x: i32
      |  data: T
      |  has contract Dup
      |  has mirror Dup
      |}
      |
      |root type IntBox = Box[i32]
      |""".stripMargin

  /** (c) empty-B: every field depends on the template parameter, so the resolved set is empty. */
  private val emptyExtractionDomain: String =
    """model t38.neg.empty
      |
      |version "1.0.0"
      |
      |data Box[T] {
      |  data: T
      |  xs: lst[T]
      |  has contract BoxView
      |}
      |
      |root type IntBox = Box[i32]
      |""".stripMargin

  /** (d) no-op: a non-template domain with zero `has` clauses. */
  private val noopDomain: String =
    """model t38.noop
      |
      |version "1.0.0"
      |
      |root data Foo {
      |  x: i32
      |  y: str
      |}
      |
      |data Page[T] {
      |  items: lst[T]
      |}
      |
      |root type IntPage = Page[i32]
      |""".stripMargin

  // ─── tests ──────────────────────────────────────────────────────────────────

  "T38 extraction synthesis" should {

    "(a)+(b) templated data host: BoxView is a Contract with exactly {x, second, pf, cf}" in {
      (parser: BaboonParser[F], typer: BaboonTyper[F]) =>
        for {
          outcome <- runTyperFor(parser, typer, dataHostDomain)
        } yield {
          val domain = outcome.toOption.getOrElse(throw new AssertionError(s"expected success, got: $outcome"))
          assert(userTypeNames(domain).contains("BoxView"), s"BoxView missing; got: ${userTypeNames(domain)}")
          val view = findContract(domain, "BoxView")
          val names = fieldNames(view)
          // (a): direct field x, through-parent second:i32, + ConcreteParent's pf (a `+`-parent
          // contribution is flattened into the contract's own field list).
          assert(names == Set("x", "second", "pf"), s"expected direct fields {x, second, pf}, got: $names")
          // `is C` materialization: a contract referencing another contract carries it as a contract
          // edge (NOT flattened into `fields`), so C must appear in BoxView.contracts.
          val cId = domain.defs.meta.nodes.keys.collectFirst { case u: TypeId.User if u.name.name == "C" => u }
            .getOrElse(throw new AssertionError(s"C not found; user types: ${userTypeNames(domain)}"))
          assert(view.contracts.contains(cId), s"BoxView must carry `is C` as a contract edge, got: ${view.contracts}")
          // (b): param-bearing members excluded.
          assert(!names.contains("data"), s"data:T must be excluded, got: $names")
          assert(!names.contains("xs"), s"xs:lst[T] must be excluded, got: $names")
          assert(!names.contains("m"), s"m:map[str,T] must be excluded, got: $names")
          assert(!names.contains("first"), s"Pair.first:T must be excluded, got: $names")
          // field types
          assert(view.fields.exists(f => f.name.name == "x" && f.tpe == i32), s"x:i32 expected, got: ${view.fields}")
          assert(view.fields.exists(f => f.name.name == "second" && f.tpe == i32), s"second:i32 expected, got: ${view.fields}")
          assert(view.fields.exists(f => f.name.name == "pf" && f.tpe == i32), s"pf:i32 expected, got: ${view.fields}")
          // C itself carries cf:str (accessible to BoxView transitively via the contract edge).
          val cContract = findContract(domain, "C")
          assert(cContract.fields.exists(f => f.name.name == "cf" && f.tpe == str), s"C.cf:str expected, got: ${cContract.fields}")
        }
    }

    "(a)+(b) templated id host: KeyView is a Contract with exactly {x, second}" in {
      (parser: BaboonParser[F], typer: BaboonTyper[F]) =>
        for {
          outcome <- runTyperFor(parser, typer, idHostDomain)
        } yield {
          val domain = outcome.toOption.getOrElse(throw new AssertionError(s"expected success, got: $outcome"))
          assert(userTypeNames(domain).contains("KeyView"), s"KeyView missing; got: ${userTypeNames(domain)}")
          val view = findContract(domain, "KeyView")
          val names = fieldNames(view)
          assert(names == Set("x", "second"), s"expected {x, second}, got: $names")
          assert(view.fields.exists(f => f.name.name == "x" && f.tpe == i32), s"x:i32 expected, got: ${view.fields}")
          assert(view.fields.exists(f => f.name.name == "second" && f.tpe == i32), s"second:i32 expected, got: ${view.fields}")
        }
    }

    "(c) host-invalid: NON-templated id with `has` → ExtractionHostInvalid" in {
      (parser: BaboonParser[F], typer: BaboonTyper[F]) =>
        for {
          outcome <- runTyperFor(parser, typer, nonTemplatedIdHostDomain)
        } yield {
          assertProducesTyperIssue[TyperIssue.ExtractionHostInvalid](outcome)
        }
    }

    "(c) host-invalid: contract body with `has` → ExtractionHostInvalid" in {
      (parser: BaboonParser[F], typer: BaboonTyper[F]) =>
        for {
          outcome <- runTyperFor(parser, typer, contractHostDomain)
        } yield {
          assertProducesTyperIssue[TyperIssue.ExtractionHostInvalid](outcome)
        }
    }

    "(c) name-collision vs existing type → ExtractionNameCollision" in {
      (parser: BaboonParser[F], typer: BaboonTyper[F]) =>
        for {
          outcome <- runTyperFor(parser, typer, collisionWithTypeDomain)
        } yield {
          assertProducesTyperIssue[TyperIssue.ExtractionNameCollision](outcome)
        }
    }

    "(c) name-collision vs template name → ExtractionNameCollision" in {
      (parser: BaboonParser[F], typer: BaboonTyper[F]) =>
        for {
          outcome <- runTyperFor(parser, typer, collisionWithTemplateDomain)
        } yield {
          assertProducesTyperIssue[TyperIssue.ExtractionNameCollision](outcome)
        }
    }

    "(c) name-collision vs sibling extraction → ExtractionNameCollision" in {
      (parser: BaboonParser[F], typer: BaboonTyper[F]) =>
        for {
          outcome <- runTyperFor(parser, typer, collisionWithSiblingDomain)
        } yield {
          assertProducesTyperIssue[TyperIssue.ExtractionNameCollision](outcome)
        }
    }

    "(c) empty resolved field set → ExtractionEmpty" in {
      (parser: BaboonParser[F], typer: BaboonTyper[F]) =>
        for {
          outcome <- runTyperFor(parser, typer, emptyExtractionDomain)
        } yield {
          assertProducesTyperIssue[TyperIssue.ExtractionEmpty](outcome)
        }
    }

    "(d) no-op: zero-`has` model type-checks, no synthesized contract appears" in {
      (parser: BaboonParser[F], typer: BaboonTyper[F]) =>
        for {
          outcome <- runTyperFor(parser, typer, noopDomain)
        } yield {
          val domain = outcome.toOption.getOrElse(throw new AssertionError(s"expected success, got: $outcome"))
          val names  = userTypeNames(domain)
          assert(names.contains("Foo"), s"Foo missing; got: $names")
          assert(names.contains("IntPage"), s"IntPage missing; got: $names")
          // No synthesized Typedef.Contract sneaks in.
          val contracts = domain.defs.meta.nodes.values.collect {
            case u: DomainMember.User if u.defn.isInstanceOf[Typedef.Contract] => u.id.name.name
          }.toSet
          assert(contracts.isEmpty, s"no contract should be synthesized for a zero-has model, got: $contracts")
        }
    }
  }
}
