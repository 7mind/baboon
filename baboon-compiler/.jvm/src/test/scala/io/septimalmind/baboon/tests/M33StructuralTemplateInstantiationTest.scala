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

/** PR-33.2 (M33) typer-side tests for structural-arm template instantiation in DTO bodies.
  *
  * Exercises the lowered-arm pipeline: parse → template-registry → ADT-inheritance-expander →
  * PR-33.2 structural-arm lowering → scope-build → type-check.
  *
  * Architectural choice (documented in `RawDtoMember.IntersectionFields` scaladoc):
  *   - `+ Template[Args]`: M1 inline (substituted FieldDefs splice into receiver's member list).
  *   - `- Template[Args]`: M1 inline (substituted FieldDefs converted to UnfieldDefs).
  *   - `^ Template[Args]`: M3 carrier (substituted RawFields wrapped in IntersectionFields).
  *
  * Verifies (positive):
  *   - `+ MyGen[i32]`: receiver carries the substituted fields; template is NOT in Domain.defs.
  *   - `- MyGen[i32]`: removes the named fields by Field equality.
  *   - `^ MyGen[i32]`: intersects the receiver's parent-derived fields with the substituted set.
  *   - `+ ns.NsGen[i32]`: cross-namespace template instantiation in structural-arm position.
  *   - `+ Concrete; + MyGen[i32]`: mixed concrete + template arms in same head.
  *   - `template Outer[U] { + Inner[U] }` instantiated as `Outer[i32]`: recursive substitution
  *     produces a flat result with `Inner`'s substituted fields.
  *
  * Verifies (negative — termination guarantee, PR-33.4 will refine the diagnostic):
  *   - `template Self[T] { + Self[T] }` instantiated as `type X = Self[i32]`: recursion guard
  *     fires, terminating with `CircularInheritance` (per §3.f).
  */
final class M33StructuralTemplateInstantiationTest extends M33StructuralTemplateInstantiationTestBase[Either]

abstract class M33StructuralTemplateInstantiationTestBase[F[+_, +_]: Error2: TagKK: BaboonTestModule] extends BaboonTest[F] {

  private def makeInput(name: String, content: String): BaboonParser.Input =
    BaboonParser.Input(FSPath.parse(NEString.unsafeFrom(name)), content)

  private def runTyperFor(
    parser: BaboonParser[F],
    typer: BaboonTyper[F],
    content: String,
    name: String = "m33-structural-template-instantiation-test.baboon",
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

  private def findDto(domain: Domain, name: String): Typedef.Dto = {
    val id = domain.defs.meta.nodes.keys.collectFirst {
      case u: TypeId.User if u.name.name == name => u
    }.getOrElse(throw new AssertionError(s"$name not found in domain"))
    domain.defs.meta.nodes(id) match {
      case u: DomainMember.User =>
        u.defn match {
          case d: Typedef.Dto => d
          case other          => throw new AssertionError(s"expected Typedef.Dto for $name, got: $other")
        }
      case other => throw new AssertionError(s"expected DomainMember.User for $name, got: $other")
    }
  }

  // ─── fixtures ────────────────────────────────────────────────────────────

  /** `+ MyGen[i32]`: receiver inlines MyGen's fields with T=i32. */
  private val plusFixture: String =
    """model m33.plus
      |
      |version "1.0.0"
      |
      |data MyGen[T] {
      |  v: T
      |  total: u32
      |}
      |
      |root data Holder {
      |  + MyGen[i32]
      |  extra: str
      |}
      |""".stripMargin

  /** `- MyGen[i32]` removes the substituted fields by name (Field equality). */
  private val minusFixture: String =
    """model m33.minus
      |
      |version "1.0.0"
      |
      |data MyGen[T] {
      |  v: T
      |  total: u32
      |}
      |
      |data Wide {
      |  v: i32
      |  total: u32
      |  keep: str
      |}
      |
      |root data Narrow {
      |  + Wide
      |  - MyGen[i32]
      |}
      |""".stripMargin

  /** `^ MyGen[i32]` intersects parent fields with substituted set. */
  private val intersectionFixture: String =
    """model m33.intersection
      |
      |version "1.0.0"
      |
      |data MyGen[T] {
      |  v: T
      |  total: u32
      |}
      |
      |data Wide {
      |  v: i32
      |  total: u32
      |  drop: str
      |}
      |
      |root data Both {
      |  + Wide
      |  ^ MyGen[i32]
      |}
      |""".stripMargin

  /** Cross-namespace template `+ ns.NsGen[i32]`. */
  private val crossNsFixture: String =
    """model m33.crossns
      |
      |version "1.0.0"
      |
      |ns ns1 {
      |  data NsGen[T] {
      |    v: T
      |  }
      |}
      |
      |root data Holder {
      |  + ns1.NsGen[i32]
      |  extra: str
      |}
      |""".stripMargin

  /** Mixed concrete + template arms in same head. */
  private val mixedFixture: String =
    """model m33.mixed
      |
      |version "1.0.0"
      |
      |data Concrete {
      |  c: str
      |}
      |
      |data MyGen[T] {
      |  v: T
      |}
      |
      |root data Both {
      |  + Concrete
      |  + MyGen[i32]
      |}
      |""".stripMargin

  /** Recursive substitution: Outer's body uses + Inner[U]; instantiated at Outer[i32]. */
  private val recursiveFixture: String =
    """model m33.recursive
      |
      |version "1.0.0"
      |
      |data Inner[V] {
      |  inner: V
      |}
      |
      |data Outer[U] {
      |  + Inner[U]
      |  outer: u32
      |}
      |
      |root data Materialised {
      |  + Outer[i32]
      |}
      |""".stripMargin

  /** Negative: self-instantiating template via structural arm. Termination via depth/cycle guard. */
  private val selfRefStructuralFixture: String =
    """model m33.selfref
      |
      |version "1.0.0"
      |
      |data Self[T] {
      |  + Self[T]
      |}
      |
      |type X = Self[i32]
      |""".stripMargin

  // PR-33.2-D02: a template body that combines a typed-arg arm with a concrete `+ Base` arm.
  // Used under `+` (positive) and under `-`/`^` (must fail with TemplateBodyNotFlatForRemoval).

  /** Positive: `+ Mixed[i32]` works because `+` does not require flatness — the substituted
    * body's concrete `+ Base` arm is preserved through recursive lowering and contributes
    * Base's fields verbatim to the receiver.
    */
  private val mixedTemplatePlusFixture: String =
    """model m33.mixedplus
      |
      |version "1.0.0"
      |
      |data Base {
      |  b: str
      |}
      |
      |data Mixed[T] {
      |  + Base
      |  v: T
      |}
      |
      |root data Holder {
      |  + Mixed[i32]
      |}
      |""".stripMargin

  /** Negative: `- Mixed[i32]` must fail because the substituted body contains a non-FieldDef
    * member (`+ Base`); silently dropping it would mask a semantically incorrect removal.
    */
  private val mixedTemplateMinusFixture: String =
    """model m33.mixedminus
      |
      |version "1.0.0"
      |
      |data Base {
      |  b: str
      |}
      |
      |data Mixed[T] {
      |  + Base
      |  v: T
      |}
      |
      |data Wide {
      |  v: i32
      |  b: str
      |  keep: i32
      |}
      |
      |root data Narrow {
      |  + Wide
      |  - Mixed[i32]
      |}
      |""".stripMargin

  /** Negative: `^ Mixed[i32]` must fail for the same reason as `-`. */
  private val mixedTemplateCaretFixture: String =
    """model m33.mixedcaret
      |
      |version "1.0.0"
      |
      |data Base {
      |  b: str
      |}
      |
      |data Mixed[T] {
      |  + Base
      |  v: T
      |}
      |
      |data Wide {
      |  v: i32
      |  b: str
      |  drop: f32
      |}
      |
      |root data Both {
      |  + Wide
      |  ^ Mixed[i32]
      |}
      |""".stripMargin

  // PR-33.2-D05: bare template name (no brackets) in structural-arm position.

  private val bareTemplatePlusFixture: String =
    """model m33.bareplus
      |
      |version "1.0.0"
      |
      |data MyGen[T] {
      |  v: T
      |}
      |
      |root data Holder {
      |  + MyGen
      |}
      |""".stripMargin

  private val bareTemplateMinusFixture: String =
    """model m33.bareminus
      |
      |version "1.0.0"
      |
      |data MyGen[T] {
      |  v: T
      |}
      |
      |data Wide {
      |  v: i32
      |  keep: str
      |}
      |
      |root data Narrow {
      |  + Wide
      |  - MyGen
      |}
      |""".stripMargin

  private val bareTemplateCaretFixture: String =
    """model m33.barecaret
      |
      |version "1.0.0"
      |
      |data MyGen[T] {
      |  v: T
      |}
      |
      |data Wide {
      |  v: i32
      |  drop: str
      |}
      |
      |root data Both {
      |  + Wide
      |  ^ MyGen
      |}
      |""".stripMargin

  // PR-33.4: empty template body under `^` must fail at lowering time (not silently become a no-op).
  // BaboonTranslator.scala:319's `if (intersectionSet.isEmpty)` short-circuit would otherwise pass
  // all fields through, producing a DTO with no intersection applied — silent semantic gap.
  private val emptyBodyCaretFixture: String =
    """model m33.emptycaret
      |
      |version "1.0.0"
      |
      |data Empty[T] {
      |}
      |
      |data Wide {
      |  v: i32
      |  total: u32
      |}
      |
      |root data Holder {
      |  + Wide
      |  ^ Empty[i32]
      |}
      |""".stripMargin

  // PR-33.4-D05: positive-pass `+ Empty[i32]` — adding an empty-body template is an idempotent no-op.
  private val emptyBodyPlusFixture: String =
    """model m33.emptyplus
      |
      |version "1.0.0"
      |
      |data Empty[T] {
      |}
      |
      |root data Receiver {
      |  + Empty[i32]
      |}
      |""".stripMargin

  // PR-33.4-D05: regression-pin `- Empty[i32]` — removing an empty field set is an idempotent no-op.
  // `+ Foo[i32]` provides `v: i32`; `- Empty[i32]` removes nothing, so `v: i32` must survive.
  private val emptyBodyMinusFixture: String =
    """model m33.emptyminus
      |
      |version "1.0.0"
      |
      |data Empty[T] {
      |}
      |
      |data Foo[T] {
      |  v: T
      |}
      |
      |root data Receiver {
      |  + Foo[i32]
      |  - Empty[i32]
      |}
      |""".stripMargin

  // PR-33.2-D04: exercise the depth-limit branch. We use a single self-referential template that
  // wraps its argument in `lst[…]` at each level, so the cycle key (receiver, template, argTupleKey)
  // is fresh at every iteration and the cycle-set branch never fires. Depth must exceed
  // `structuralArmRecursionLimit` (default 32). Reached at depth=32 → emit CircularInheritance via
  // the depth-limit branch.
  private val depthLimitFixture: String =
    """model m33.depthlimit
      |
      |version "1.0.0"
      |
      |data Deep[T] {
      |  + Deep[lst[T]]
      |}
      |
      |type X = Deep[i32]
      |""".stripMargin

  // ─── tests ────────────────────────────────────────────────────────────────

  "M33 structural template instantiation (PR-33.2)" should {

    "+ MyGen[i32]: substitute and inline; template absent from Domain.defs" in {
      (parser: BaboonParser[F], typer: BaboonTyper[F]) =>
        for {
          outcome <- runTyperFor(parser, typer, plusFixture)
        } yield {
          val domain = outcome.toOption.getOrElse(throw new AssertionError(s"expected success, got: $outcome"))
          val names  = userTypeNames(domain)
          assert(names.contains("Holder"), s"expected Holder, got: $names")
          // PR-33.2-D03: stronger invariant — exact count of user-authored non-template top-level types.
          // plusFixture declares 1: Holder (MyGen is a template, removed from Domain.defs at registry-build time).
          assert(names == Set("Holder"), s"expected exactly {Holder}, got: $names")
          val holder = findDto(domain, "Holder")
          // Holder has v:i32, total:u32 (from + MyGen[i32]), and extra:str.
          assert(holder.fields.exists(f => f.name.name == "v" && f.tpe == TypeRef.Scalar(TypeId.Builtins.i32)), s"expected v:i32 in Holder, got: ${holder.fields}")
          assert(holder.fields.exists(f => f.name.name == "total" && f.tpe == TypeRef.Scalar(TypeId.Builtins.u32)), s"expected total:u32 in Holder, got: ${holder.fields}")
          assert(holder.fields.exists(f => f.name.name == "extra" && f.tpe == TypeRef.Scalar(TypeId.Builtins.str)), s"expected extra:str in Holder, got: ${holder.fields}")
        }
    }

    "- MyGen[i32]: removes the substituted fields by name; template absent" in {
      (parser: BaboonParser[F], typer: BaboonTyper[F]) =>
        for {
          outcome <- runTyperFor(parser, typer, minusFixture)
        } yield {
          val domain = outcome.toOption.getOrElse(throw new AssertionError(s"expected success, got: $outcome"))
          val names  = userTypeNames(domain)
          // PR-33.2-D03: only `@root`-reachable types are retained in Domain.defs after pruning;
          // Wide is consumed by `+ Wide` (its fields are inlined into Narrow) and is no longer
          // transitively reachable from any root, so only Narrow remains in the user types.
          assert(names == Set("Narrow"), s"expected exactly {Narrow}, got: $names")
          val narrow = findDto(domain, "Narrow")
          // Narrow = (Wide.fields {v:i32, total:u32, keep:str}) − {v:i32, total:u32} = {keep:str}.
          assert(narrow.fields.size == 1, s"expected exactly 1 field in Narrow after removal, got: ${narrow.fields}")
          assert(narrow.fields.exists(f => f.name.name == "keep"), s"expected keep field in Narrow, got: ${narrow.fields}")
        }
    }

    "^ MyGen[i32]: intersects parent fields with substituted set; template absent" in {
      (parser: BaboonParser[F], typer: BaboonTyper[F]) =>
        for {
          outcome <- runTyperFor(parser, typer, intersectionFixture)
        } yield {
          val domain = outcome.toOption.getOrElse(throw new AssertionError(s"expected success, got: $outcome"))
          val names  = userTypeNames(domain)
          // PR-33.2-D03: only `@root`-reachable types remain after pruning; Wide is consumed by
          // `+ Wide` (its fields are inlined into Both) and is no longer transitively reachable.
          assert(names == Set("Both"), s"expected exactly {Both}, got: $names")
          val both = findDto(domain, "Both")
          // Both = (Wide.fields ∩ MyGen[i32].fields) = {v:i32, total:u32}; drop:str excluded.
          assert(both.fields.size == 2, s"expected exactly 2 fields in Both after intersection, got: ${both.fields}")
          assert(both.fields.exists(f => f.name.name == "v"), s"expected v in Both, got: ${both.fields}")
          assert(both.fields.exists(f => f.name.name == "total"), s"expected total in Both, got: ${both.fields}")
          assert(!both.fields.exists(f => f.name.name == "drop"), s"drop must be excluded from Both, got: ${both.fields}")
        }
    }

    "+ ns1.NsGen[i32]: cross-namespace structural-arm instantiation" in {
      (parser: BaboonParser[F], typer: BaboonTyper[F]) =>
        for {
          outcome <- runTyperFor(parser, typer, crossNsFixture)
        } yield {
          val domain = outcome.toOption.getOrElse(throw new AssertionError(s"expected success, got: $outcome"))
          val names  = userTypeNames(domain)
          // PR-33.2-D03: crossNsFixture declares 1 non-template user type (Holder); the ns1 namespace
          // contains only the NsGen template, so it is dropped (empty-namespace optimization).
          assert(names == Set("Holder"), s"expected exactly {Holder}, got: $names")
          val holder = findDto(domain, "Holder")
          assert(holder.fields.exists(f => f.name.name == "v" && f.tpe == TypeRef.Scalar(TypeId.Builtins.i32)), s"expected v:i32 in Holder, got: ${holder.fields}")
          assert(holder.fields.exists(f => f.name.name == "extra"), s"expected extra in Holder, got: ${holder.fields}")
        }
    }

    "mixed `+ Concrete; + MyGen[i32]`: both arms inline correctly" in {
      (parser: BaboonParser[F], typer: BaboonTyper[F]) =>
        for {
          outcome <- runTyperFor(parser, typer, mixedFixture)
        } yield {
          val domain = outcome.toOption.getOrElse(throw new AssertionError(s"expected success, got: $outcome"))
          val both   = findDto(domain, "Both")
          assert(both.fields.exists(f => f.name.name == "c" && f.tpe == TypeRef.Scalar(TypeId.Builtins.str)), s"expected c:str (from Concrete), got: ${both.fields}")
          assert(both.fields.exists(f => f.name.name == "v" && f.tpe == TypeRef.Scalar(TypeId.Builtins.i32)), s"expected v:i32 (from MyGen[i32]), got: ${both.fields}")
        }
    }

    "recursive substitution: + Outer[i32] inlines + Inner[i32] which inlines Inner.fields[V=i32]" in {
      (parser: BaboonParser[F], typer: BaboonTyper[F]) =>
        for {
          outcome <- runTyperFor(parser, typer, recursiveFixture)
        } yield {
          val domain       = outcome.toOption.getOrElse(throw new AssertionError(s"expected success, got: $outcome"))
          val names        = userTypeNames(domain)
          // PR-33.2-D03: recursiveFixture declares 1 non-template user type (Materialised);
          // both Inner and Outer are templates and are removed at registry-build time.
          assert(names == Set("Materialised"), s"expected exactly {Materialised}, got: $names")
          val materialised = findDto(domain, "Materialised")
          // Materialised should have inner:i32 (from recursively-substituted Inner) and outer:u32 (from Outer).
          assert(materialised.fields.exists(f => f.name.name == "inner" && f.tpe == TypeRef.Scalar(TypeId.Builtins.i32)), s"expected inner:i32, got: ${materialised.fields}")
          assert(materialised.fields.exists(f => f.name.name == "outer" && f.tpe == TypeRef.Scalar(TypeId.Builtins.u32)), s"expected outer:u32, got: ${materialised.fields}")
        }
    }

    "self-instantiating template terminates with a diagnostic (no stack overflow / hang)" in {
      (parser: BaboonParser[F], typer: BaboonTyper[F]) =>
        for {
          outcome <- runTyperFor(parser, typer, selfRefStructuralFixture)
        } yield {
          // Per §3.f, reuse CircularInheritance. PR-33.4 may refine the wording.
          assertProducesTyperIssue[TyperIssue.CircularInheritance](outcome)
          // PR-33.2-D01: the synthetic CircularInheritance must carry a non-empty matrix
          // referencing the receiving DTO and the template name so the printer renders a
          // meaningful diagnostic instead of an empty payload.
          val ti = outcome.left.toOption.toList.flatMap(_.toList).collect { case BaboonIssue.Typer(t: TyperIssue.CircularInheritance) => t }
          assert(ti.nonEmpty, s"expected a CircularInheritance issue, got: $outcome")
          val matrix = ti.head.error match {
            case izumi.fundamentals.graphs.ToposortError.UnexpectedLoop(_, m) => m.links
            case _                                                            => Map.empty[TypeId.User, Set[TypeId.User]]
          }
          assert(matrix.nonEmpty, s"expected non-empty matrix in synthetic CircularInheritance payload, got: ${ti.head.error}")
          val rendered = matrix.flatMap { case (t, cs) => Iterator(t.name.name) ++ cs.iterator.map(_.name.name) }.toSet
          assert(rendered.contains("Self"), s"expected matrix to mention 'Self', got: $rendered")
          // The receiving DTO is `X` (the alias-materialised concrete DTO) — Pass 2 walks X and lowers `+ Self[i32]`.
          assert(rendered.contains("X"), s"expected matrix to mention the receiving DTO 'X', got: $rendered")
        }
    }

    // ─── PR-33.2-D02: `-` / `^` over a template body that is not a flat field list ────────────

    "+ Mixed[i32] (template body has + Base concrete arm): preserves Base's fields verbatim" in {
      (parser: BaboonParser[F], typer: BaboonTyper[F]) =>
        for {
          outcome <- runTyperFor(parser, typer, mixedTemplatePlusFixture)
        } yield {
          val domain = outcome.toOption.getOrElse(throw new AssertionError(s"expected success, got: $outcome"))
          val holder = findDto(domain, "Holder")
          assert(holder.fields.exists(f => f.name.name == "b" && f.tpe == TypeRef.Scalar(TypeId.Builtins.str)), s"expected b:str (from Base), got: ${holder.fields}")
          assert(holder.fields.exists(f => f.name.name == "v" && f.tpe == TypeRef.Scalar(TypeId.Builtins.i32)), s"expected v:i32 (from Mixed[T=i32]), got: ${holder.fields}")
        }
    }

    "- Mixed[i32] (template body has + Base concrete arm): rejected with TemplateBodyNotFlatForRemoval" in {
      (parser: BaboonParser[F], typer: BaboonTyper[F]) =>
        for {
          outcome <- runTyperFor(parser, typer, mixedTemplateMinusFixture)
        } yield {
          assertProducesTyperIssue[TyperIssue.TemplateBodyNotFlatForRemoval](outcome)
        }
    }

    "^ Mixed[i32] (template body has + Base concrete arm): rejected with TemplateBodyNotFlatForRemoval" in {
      (parser: BaboonParser[F], typer: BaboonTyper[F]) =>
        for {
          outcome <- runTyperFor(parser, typer, mixedTemplateCaretFixture)
        } yield {
          assertProducesTyperIssue[TyperIssue.TemplateBodyNotFlatForRemoval](outcome)
        }
    }

    // ─── PR-33.2-D05: bare template name (no brackets) in structural-arm position ─────────────

    "+ MyGen (no brackets) in DTO body: rejected with TemplateNotInstantiated" in {
      (parser: BaboonParser[F], typer: BaboonTyper[F]) =>
        for {
          outcome <- runTyperFor(parser, typer, bareTemplatePlusFixture)
        } yield {
          assertProducesTyperIssue[TyperIssue.TemplateNotInstantiated](outcome)
        }
    }

    "- MyGen (no brackets) in DTO body: rejected with TemplateNotInstantiated" in {
      (parser: BaboonParser[F], typer: BaboonTyper[F]) =>
        for {
          outcome <- runTyperFor(parser, typer, bareTemplateMinusFixture)
        } yield {
          assertProducesTyperIssue[TyperIssue.TemplateNotInstantiated](outcome)
        }
    }

    "^ MyGen (no brackets) in DTO body: rejected with TemplateNotInstantiated" in {
      (parser: BaboonParser[F], typer: BaboonTyper[F]) =>
        for {
          outcome <- runTyperFor(parser, typer, bareTemplateCaretFixture)
        } yield {
          assertProducesTyperIssue[TyperIssue.TemplateNotInstantiated](outcome)
        }
    }

    // ─── PR-33.4: empty template body under `^` must fail ─────────────────────────────────────

    "^ Empty[i32] (empty template body under ^): rejected with TemplateBodyNotFlatForRemoval (empty body sentinel)" in {
      (parser: BaboonParser[F], typer: BaboonTyper[F]) =>
        for {
          outcome <- runTyperFor(parser, typer, emptyBodyCaretFixture)
        } yield {
          // An empty-body template under `^` would silently become a no-op via
          // BaboonTranslator.scala:319's `if (intersectionSet.isEmpty)` short-circuit, passing all
          // fields through unchanged instead of producing an empty result. Catch at lowering time
          // via TemplateBodyNotFlatForRemoval with offendingMemberKind="empty body" (PR-33.4).
          assertProducesTyperIssue[TyperIssue.TemplateBodyNotFlatForRemoval](outcome)
          // PR-33.4-D02: pin the sentinel values so the test fails if a future regression fires
          // TemplateBodyNotFlatForRemoval for the wrong operator or the wrong offendingMemberKind.
          val ti = outcome.left.toOption.toList.flatMap(_.toList).collect {
            case BaboonIssue.Typer(t: TyperIssue.TemplateBodyNotFlatForRemoval) => t
          }
          assert(ti.nonEmpty, s"expected a TemplateBodyNotFlatForRemoval issue, got: $outcome")
          assert(
            ti.exists(t => t.kind == "caret" && t.offendingMemberKind == "empty body"),
            s"expected kind=caret and offendingMemberKind=empty body, got: ${ti.map(t => (t.kind, t.offendingMemberKind))}",
          )
        }
    }

    // ─── PR-33.4-D05: positive-pass and regression-pin for empty-body template ─────────────────

    "+ Empty[i32] (empty template body under +): accepted as an idempotent no-op" in {
      (parser: BaboonParser[F], typer: BaboonTyper[F]) =>
        for {
          outcome <- runTyperFor(parser, typer, emptyBodyPlusFixture)
        } yield {
          // Adding an empty-body template is a no-op: `+ Empty[T]` contributes zero fields.
          // The receiver must compile and carry zero fields (the `+ Empty[i32]` arm adds nothing).
          val domain   = outcome.toOption.getOrElse(throw new AssertionError(s"expected success, got: $outcome"))
          val receiver = findDto(domain, "Receiver")
          assert(receiver.fields.isEmpty, s"expected Receiver to have zero fields (empty-body no-op), got: ${receiver.fields}")
        }
    }

    "- Empty[i32] (empty template body under -): accepted as an idempotent no-op; surviving fields intact" in {
      (parser: BaboonParser[F], typer: BaboonTyper[F]) =>
        for {
          outcome <- runTyperFor(parser, typer, emptyBodyMinusFixture)
        } yield {
          // regression guard pinning current silent no-op behaviour; update when [PR-33.4-D01] is resolved
          // Removing an empty set of fields is idempotent — `- Empty[i32]` removes nothing, so `v: i32`
          // (contributed by `+ Foo[i32]`) must survive in the receiver.
          val domain   = outcome.toOption.getOrElse(throw new AssertionError(s"expected success, got: $outcome"))
          val receiver = findDto(domain, "Receiver")
          assert(
            receiver.fields.exists(f => f.name.name == "v" && f.tpe == TypeRef.Scalar(TypeId.Builtins.i32)),
            s"expected v:i32 to survive the - Empty[i32] no-op, got: ${receiver.fields}",
          )
        }
    }

    // ─── PR-33.2-D04: depth-limit branch of the recursion guard ───────────────────────────────

    "structural-arm chain deeper than the recursion limit terminates via the depth-limit branch" in {
      (parser: BaboonParser[F], typer: BaboonTyper[F]) =>
        // The fixture's argument grows by `lst[…]` at each recursion step, so the cycle key
        // (receiver, template, argTupleKey) is fresh at every iteration and the cycle-set
        // branch cannot fire. Termination must come from the depth-limit branch.
        for {
          outcome <- runTyperFor(parser, typer, depthLimitFixture)
        } yield {
          assertProducesTyperIssue[TyperIssue.CircularInheritance](outcome)
        }
    }
  }
}
