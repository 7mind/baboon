package io.septimalmind.baboon.tests

import io.septimalmind.baboon.parser.BaboonParser
import io.septimalmind.baboon.parser.model.FSPath
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.tests.BaboonTest.BaboonTestModule
import io.septimalmind.baboon.typer.BaboonTyper
import io.septimalmind.baboon.typer.model.*
import izumi.functional.bio.{Error2, F}
import izumi.fundamentals.collections.nonempty.{NEList, NEString}
import izumi.reflect.TagKK

/** T40 (auto-extracted-contracts) typer tests for instantiation lowering of the host↔B relationship.
  *
  * Exercises the full path: parse → template-registry pass → ADT inheritance expander →
  * monomorphisation (T40 lowering: `has contract` → `is B`, `has mirror` → reachability edge) →
  * T38 extraction synthesis → scope-build → type-check → @root GC.
  *
  * Acceptance (verbatim from the task brief):
  *   - contract variant: every instantiation's `contracts` list contains B, B's fields absorb
  *     without duplicate-field issues, two instantiations of one host share the single B.
  *   - adt host: instantiated ADT carries B at ADT level.
  *   - mirror variant: `contracts` lists do NOT contain B.
  *   - GC: (i) B present in output when a host instantiation is @root-reachable, (ii) B absent when
  *     the template is never instantiated or all instantiations are dead, (iii) B present when only
  *     an emitted third type references it via `is B`.
  */
final class T40ExtractionLoweringTest extends T40ExtractionLoweringTestBase[Either]

abstract class T40ExtractionLoweringTestBase[F[+_, +_]: Error2: TagKK: BaboonTestModule] extends BaboonTest[F] {

  private def makeInput(name: String, content: String): BaboonParser.Input =
    BaboonParser.Input(FSPath.parse(NEString.unsafeFrom(name)), content)

  private def runTyperFor(
    parser: BaboonParser[F],
    typer: BaboonTyper[F],
    content: String,
    name: String = "t40-extraction-lowering-test.baboon",
  ): F[Nothing, Either[NEList[BaboonIssue], Domain]] = {
    val input = makeInput(name, content)
    parser.parse(input).flatMap(typer.process).map(Right(_): Either[NEList[BaboonIssue], Domain]).catchAll {
      errs => F.pure(Left(errs): Either[NEList[BaboonIssue], Domain])
    }
  }

  private def userTypeNames(domain: Domain): Set[String] =
    domain.defs.meta.nodes.keys.collect { case u: TypeId.User => u.name.name }.toSet

  private def idOf(domain: Domain, name: String): TypeId.User =
    domain.defs.meta.nodes.keys.collectFirst {
      case u: TypeId.User if u.name.name == name => u
    }.getOrElse(throw new AssertionError(s"$name not found in domain; user types: ${userTypeNames(domain)}"))

  private def dtoOf(domain: Domain, name: String): Typedef.Dto =
    domain.defs.meta.nodes(idOf(domain, name)) match {
      case u: DomainMember.User =>
        u.defn match {
          case d: Typedef.Dto => d
          case other          => throw new AssertionError(s"expected Typedef.Dto for $name, got: $other")
        }
      case other => throw new AssertionError(s"expected DomainMember.User for $name, got: $other")
    }

  private def adtOf(domain: Domain, name: String): Typedef.Adt =
    domain.defs.meta.nodes(idOf(domain, name)) match {
      case u: DomainMember.User =>
        u.defn match {
          case a: Typedef.Adt => a
          case other          => throw new AssertionError(s"expected Typedef.Adt for $name, got: $other")
        }
      case other => throw new AssertionError(s"expected DomainMember.User for $name, got: $other")
    }

  private def contractOf(domain: Domain, name: String): Typedef.Contract =
    domain.defs.meta.nodes(idOf(domain, name)) match {
      case u: DomainMember.User =>
        u.defn match {
          case c: Typedef.Contract => c
          case other               => throw new AssertionError(s"expected Typedef.Contract for $name, got: $other")
        }
      case other => throw new AssertionError(s"expected DomainMember.User for $name, got: $other")
    }

  private val i32 = TypeRef.Scalar(TypeId.Builtins.i32)

  private def domainOf(outcome: Either[NEList[BaboonIssue], Domain]): Domain =
    outcome.toOption.getOrElse(throw new AssertionError(s"expected success, got: $outcome"))

  // ─── fixtures ──────────────────────────────────────────────────────────────

  /** contract variant: two instantiations of one host; B in each instantiation's contracts. */
  private val contractTwoInstantiations: String =
    """model t40.contract.two
      |
      |version "1.0.0"
      |
      |data Box[T] {
      |  x: i32
      |  data: T
      |  has contract BoxView
      |}
      |
      |root type IntBox = Box[i32]
      |root type StrBox = Box[str]
      |""".stripMargin

  /** adt host: a templated ADT with an ADT-level `is Base` (param-free contract). B (TreeView)
    * draws its field set from Base; the instantiated ADT carries TreeView at ADT level. */
  private val adtHost: String =
    """model t40.adt.host
      |
      |version "1.0.0"
      |
      |contract Base {
      |  bf: i32
      |}
      |
      |adt Tree[T] {
      |  is Base
      |  has contract TreeView
      |
      |  data Leaf {
      |    lv: i32
      |  }
      |  data Node {
      |    nv: i32
      |  }
      |}
      |
      |root type IntTree = Tree[i32]
      |""".stripMargin

  /** mirror variant: contracts lists do NOT contain B; B still reachable via the mirror GC edge. */
  private val mirrorHost: String =
    """model t40.mirror.host
      |
      |version "1.0.0"
      |
      |data Box[T] {
      |  x: i32
      |  data: T
      |  has mirror BoxView
      |}
      |
      |root type IntBox = Box[i32]
      |""".stripMargin

  /** GC (ii): template never instantiated → B must be GC'd out (an unrelated root keeps the GC alive). */
  private val uninstantiatedTemplate: String =
    """model t40.gc.dead
      |
      |version "1.0.0"
      |
      |data Box[T] {
      |  x: i32
      |  data: T
      |  has contract BoxView
      |}
      |
      |root data Anchor {
      |  a: i32
      |}
      |""".stripMargin

  /** GC (iii): host never instantiated, but a @root third type references B via `is B`. The
    * reference must come from a TEMPLATED probe whose instantiation lands in the post-`instantiate`
    * member list alongside the synthesized B (a hand-written non-template `is BoxView` would fail
    * the FIRST order, which runs before synthesis — cf. the T38 worker note). */
  private val thirdTypeReferencesB: String =
    """model t40.gc.third
      |
      |version "1.0.0"
      |
      |data Box[T] {
      |  x: i32
      |  data: T
      |  has contract BoxView
      |}
      |
      |data Third[T] {
      |  tf: i32
      |  is BoxView
      |}
      |
      |root type IntThird = Third[i32]
      |""".stripMargin

  /** D18 / H14 regression: cross-namespace prefixed-host `has contract`.
    *
    * The template Box[T] with `has contract BoxView` lives under namespace `some.ns`.
    * The alias `root type X = some.ns.Box[i32]` is at toplevel (different owner).
    * Before the fix, lowerHostExtractions built a bare ScopedRef(BoxView) resolved
    * relative to ownerForCurrent (toplevel), which fails to find BoxView synthesized
    * at the template owner (some.ns). After the fix the ScopedRef is prefixed with
    * the template owner's namespace path, so it resolves correctly.
    */
  private val crossNsPrefixedHostContract: String =
    """model t40.crossns.prefixed
      |
      |version "1.0.0"
      |
      |ns some {
      |  ns ns {
      |    data Box[T] {
      |      x: i32
      |      data: T
      |      has contract BoxView
      |    }
      |  }
      |}
      |
      |root type X = some.ns.Box[i32]
      |""".stripMargin

  // ─── tests ──────────────────────────────────────────────────────────────────

  "T40 instantiation lowering" should {

    "contract variant: both instantiations' contracts contain the single shared B, fields absorb without dup-field issues" in {
      (parser: BaboonParser[F], typer: BaboonTyper[F]) =>
        for {
          outcome <- runTyperFor(parser, typer, contractTwoInstantiations)
        } yield {
          val domain = domainOf(outcome)
          val bId    = idOf(domain, "BoxView")
          val intBox = dtoOf(domain, "IntBox")
          val strBox = dtoOf(domain, "StrBox")
          // Every monomorphisation implements the ONE shared non-generic B (Q8).
          assert(intBox.contracts.contains(bId), s"IntBox must carry BoxView in contracts, got: ${intBox.contracts}")
          assert(strBox.contracts.contains(bId), s"StrBox must carry BoxView in contracts, got: ${strBox.contracts}")
          // Single shared B: there is exactly one BoxView in the domain.
          val boxViewCount = domain.defs.meta.nodes.keys.count { case u: TypeId.User => u.name.name == "BoxView"; case _ => false }
          assert(boxViewCount == 1, s"expected exactly one shared BoxView, got: $boxViewCount")
          // B's param-free field set is {x} (data:T dropped); contract-field absorption deduped the
          // host's own `x` against B's `x` without a NonUniqueFields issue (the outcome is success).
          val bv = contractOf(domain, "BoxView")
          assert(bv.fields.map(_.name.name).toSet == Set("x"), s"BoxView fields expected {x}, got: ${bv.fields}")
          assert(intBox.fields.exists(f => f.name.name == "x" && f.tpe == i32), s"IntBox.x:i32 expected, got: ${intBox.fields}")
        }
    }

    "adt host: instantiated ADT carries B at ADT level" in {
      (parser: BaboonParser[F], typer: BaboonTyper[F]) =>
        for {
          outcome <- runTyperFor(parser, typer, adtHost)
        } yield {
          val domain = domainOf(outcome)
          val bId    = idOf(domain, "TreeView")
          val intTree = adtOf(domain, "IntTree")
          assert(intTree.contracts.contains(bId), s"IntTree must carry TreeView at ADT level, got: ${intTree.contracts}")
        }
    }

    "mirror variant: contracts lists do NOT contain B, yet B is reachable via the mirror GC edge" in {
      (parser: BaboonParser[F], typer: BaboonTyper[F]) =>
        for {
          outcome <- runTyperFor(parser, typer, mirrorHost)
        } yield {
          val domain = domainOf(outcome)
          assert(userTypeNames(domain).contains("BoxView"), s"BoxView must be reachable via mirror edge, got: ${userTypeNames(domain)}")
          val bId    = idOf(domain, "BoxView")
          val intBox = dtoOf(domain, "IntBox")
          assert(!intBox.contracts.contains(bId), s"mirror: IntBox.contracts must NOT contain BoxView, got: ${intBox.contracts}")
          // No Dto/Adt/Contract anywhere may list B in its contracts under the mirror variant.
          val anyContractMentions = domain.defs.meta.nodes.values.exists {
            case u: DomainMember.User =>
              u.defn match {
                case d: Typedef.Dto      => d.contracts.contains(bId)
                case a: Typedef.Adt      => a.contracts.contains(bId)
                case c: Typedef.Contract => c.contracts.contains(bId)
                case _                   => false
              }
            case _ => false
          }
          assert(!anyContractMentions, s"mirror: NO Typedef.{Dto,Adt,Contract}.contracts may contain BoxView")
        }
    }

    "GC (i): contract host instantiation @root-reachable → B in output" in {
      (parser: BaboonParser[F], typer: BaboonTyper[F]) =>
        for {
          outcome <- runTyperFor(parser, typer, contractTwoInstantiations)
        } yield {
          val domain = domainOf(outcome)
          assert(userTypeNames(domain).contains("BoxView"), s"BoxView must be in output, got: ${userTypeNames(domain)}")
        }
    }

    "GC (ii): template never instantiated → B absent from output" in {
      (parser: BaboonParser[F], typer: BaboonTyper[F]) =>
        for {
          outcome <- runTyperFor(parser, typer, uninstantiatedTemplate)
        } yield {
          val domain = domainOf(outcome)
          assert(userTypeNames(domain).contains("Anchor"), s"Anchor (root) must be present, got: ${userTypeNames(domain)}")
          assert(!userTypeNames(domain).contains("BoxView"), s"BoxView must be GC'd out (uninstantiated template), got: ${userTypeNames(domain)}")
        }
    }

    "GC (iii): only an emitted third type references B via `is B` → B in output" in {
      (parser: BaboonParser[F], typer: BaboonTyper[F]) =>
        for {
          outcome <- runTyperFor(parser, typer, thirdTypeReferencesB)
        } yield {
          val domain = domainOf(outcome)
          assert(userTypeNames(domain).contains("IntThird"), s"IntThird (root) must be present, got: ${userTypeNames(domain)}")
          assert(userTypeNames(domain).contains("BoxView"), s"BoxView must be kept alive by IntThird's `is BoxView`, got: ${userTypeNames(domain)}")
          val bId   = idOf(domain, "BoxView")
          val third = dtoOf(domain, "IntThird")
          assert(third.contracts.contains(bId), s"IntThird must carry BoxView in contracts, got: ${third.contracts}")
        }
    }

    "D18 cross-ns prefixed host `has contract`: B resolves to the template-owner-qualified ref" in {
      (parser: BaboonParser[F], typer: BaboonTyper[F]) =>
        for {
          outcome <- runTyperFor(parser, typer, crossNsPrefixedHostContract)
        } yield {
          val domain = domainOf(outcome)
          // BoxView is synthesized in some.ns scope (template owner) — must be present.
          assert(userTypeNames(domain).contains("BoxView"), s"BoxView must be in output (synthesized at some.ns scope), got: ${userTypeNames(domain)}")
          val bId = idOf(domain, "BoxView")
          // X is the monomorphisation of some.ns.Box[i32] at toplevel.
          val x = dtoOf(domain, "X")
          // The fix: BoxView must appear in X.contracts (previously it misresolved the bare ScopedRef).
          assert(x.contracts.contains(bId), s"X must carry BoxView in contracts (cross-ns prefixed-host D18), got: ${x.contracts}")
        }
    }
  }
}
