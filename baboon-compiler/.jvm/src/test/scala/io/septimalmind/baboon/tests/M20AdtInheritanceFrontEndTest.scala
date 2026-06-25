package io.septimalmind.baboon.tests

import io.septimalmind.baboon.BaboonLoader
import io.septimalmind.baboon.parser.model.issues.{BaboonIssue, TyperIssue}
import io.septimalmind.baboon.tests.BaboonTest.BaboonTestModule
import io.septimalmind.baboon.typer.model.{BaboonFamily, Domain, DomainMember, Owner, Typedef}
import izumi.functional.bio.{Error2, F}
import izumi.fundamentals.collections.nonempty.NEList
import izumi.fundamentals.platform.files.IzFiles
import izumi.fundamentals.platform.resources.IzResources
import izumi.reflect.TagKK

import java.nio.file.Path
import scala.reflect.ClassTag

/** PR-63 (M20.2) — front-end pipeline tests for ADT branch inheritance.
  *
  * Exercises the full path: parse → PR-63 typer-early expansion pass → standard convertAdt → final
  * typed AST. The pre-pass rewrites every `+ X` / `- X` / `^ X` arm into literal `RawAdtMemberDto`
  * entries before `convertAdt` runs; convertAdt itself stays unchanged.
  *
  * The tests assert two complementary properties:
  *   - Negative inputs (cycles, non-ADT targets, name collisions, empty intersections) fail with
  *     the right TyperIssue code.
  *   - Positive inputs successfully type-check, and the resulting `Typedef.Adt` carries the
  *     expected branch set (with branches owned by the receiving ADT, per Q-M20-1 re-emit
  *     semantics).
  *
  * PR-63-D01 note: `TyperIssue.CrossVersionAdtInclusion` is not exercisable from baboon source.
  * `AdtInheritanceExpander.resolveArm` emits it when `resolvedId.pkg != pkg`, but
  * `ScopeSupport.resolveTypeId` always constructs the returned `TypeId.User` with the current
  * domain's `pkg` (the parameter passed in from `runTyper`). Since each domain is typed
  * independently and `pkg` is a fixed constant throughout a single `BaboonTyper.process` call,
  * every ref resolved in that domain will have the same `pkg`. A baboon source file can only
  * reference types visible in its own scope (populated solely from the same domain), so the
  * cross-version predicate can never be true. The check is dead code from the source-input path
  * and would require in-memory AST construction to exercise.
  */
final class M20AdtInheritanceFrontEndTest extends M20AdtInheritanceFrontEndTestBase[Either]

abstract class M20AdtInheritanceFrontEndTestBase[F[+_, +_]: Error2: TagKK: BaboonTestModule] extends BaboonTest[F] {

  private def resolveBaboonFiles(resourcePath: String): List[Path] = {
    val root = IzResources
      .getPath(resourcePath)
      .getOrElse(throw new AssertionError(s"resource not found: $resourcePath"))
      .asInstanceOf[IzResources.LoadablePathReference]
      .path
    if (root.toFile.isDirectory) {
      IzFiles
        .walk(root.toFile)
        .toList
        .filter(p => p.toFile.isFile && p.toFile.getName.endsWith(".baboon"))
    } else {
      List(root)
    }
  }

  private def runPipeline(loader: BaboonLoader[F], paths: List[Path]): F[Nothing, Either[NEList[BaboonIssue], BaboonFamily]] = {
    loader.load(paths).map(Right(_): Either[NEList[BaboonIssue], BaboonFamily]).catchAll {
      errs => F.pure(Left(errs): Either[NEList[BaboonIssue], BaboonFamily])
    }
  }

  private def typerIssues(issues: NEList[BaboonIssue]): List[TyperIssue] = {
    issues.toList.collect { case BaboonIssue.Typer(ti) => ti }
  }

  private def assertProducesTyperIssue[T <: TyperIssue: ClassTag](outcome: Either[NEList[BaboonIssue], BaboonFamily]): Unit = {
    val issues = outcome.left.getOrElse(throw new AssertionError(s"expected failure, got: $outcome"))
    val ti     = typerIssues(issues)
    val ct     = implicitly[ClassTag[T]]
    assert(
      ti.exists(ct.runtimeClass.isInstance),
      s"expected ${ct.runtimeClass.getSimpleName}, got: $ti",
    )
  }

  private def expectedAdt(family: BaboonFamily, pkgPath: String, adtName: String): Typedef.Adt = {
    val domain = domainFor(family, pkgPath)
    adtFromDomain(domain, adtName, pkgPath)
  }

  /** Returns the single-version Domain for a package path. */
  private def domainFor(family: BaboonFamily, pkgPath: String): Domain = {
    val lineage = family.domains.toMap
      .find(_._1.path.mkString(".") == pkgPath).getOrElse {
        throw new AssertionError(s"no lineage for pkg '$pkgPath' in family with pkgs: ${family.domains.toMap.keys.map(_.path.mkString(".")).toList}")
      }._2
    lineage.versions.toMap.values.head
  }

  private def adtFromDomain(domain: Domain, adtName: String, pkgPath: String): Typedef.Adt = {
    domain.defs.meta.nodes.values.collectFirst {
      case DomainMember.User(_, a: Typedef.Adt, _, _) if a.id.name.name == adtName => a
    }.getOrElse {
      throw new AssertionError(
        s"no ADT '$adtName' in domain $pkgPath; ADTs found: ${domain.defs.meta.nodes.values.collect { case DomainMember.User(_, a: Typedef.Adt, _, _) => a.id.name.name }.toList}"
      )
    }
  }

  /** Extracts the normalized branch structure for an ADT as a sorted map of
    * branchName → sorted list of "fieldName: typeRepr" strings.
    * Used to compare ManualVer vs SugaredVer without relying on TypeId equality
    * (TypeIds encode the owning ADT's identity, which differs between the two).
    */
  private def branchStructure(adt: Typedef.Adt, domain: Domain): Map[String, List[String]] = {
    adt.members.toList.map {
      branchId =>
        val branchName = branchId.name.name
        val fieldSigs: List[String] = domain.defs.meta.nodes.get(branchId) match {
          case Some(DomainMember.User(_, dto: Typedef.Dto, _, _)) =>
            dto.fields.map(f => s"${f.name.name}: ${f.tpe}").sorted
          case other =>
            throw new AssertionError(s"branch $branchName resolved to unexpected node: $other")
        }
        branchName -> fieldSigs
    }.toMap
  }

  "M20 ADT inheritance front-end (PR-63)" should {

    // ---------- positive tests ----------

    "expand `+ ErrorAtom` into the receiving ADT's literal branches (Q-M20-1 re-emit)" in {
      (loader: BaboonLoader[F]) =>
        val paths = resolveBaboonFiles("baboon/m20-ok/simple-include.baboon")
        for {
          outcome <- runPipeline(loader, paths)
        } yield {
          val family      = outcome.toOption.getOrElse(throw new AssertionError(s"expected success, got: $outcome"))
          val adt         = expectedAdt(family, "my.ok.m20.simple", "SomeError")
          val branchNames = adt.members.toList.map(_.name.name).toSet
          // SomeError absorbs ErrorAtom.Forbidden plus its own Bar.
          assert(branchNames == Set("Forbidden", "Bar"), s"expected {Forbidden, Bar}, got $branchNames")
          // Q-M20-1: each re-emitted branch is owned by the receiving ADT, not the source.
          adt.members.toList.foreach {
            branchId =>
              branchId.owner match {
                case Owner.Adt(parent) =>
                  assert(parent.name.name == "SomeError", s"branch ${branchId.name.name} should be owned by SomeError, got ${parent.name.name}")
                case other =>
                  fail(s"branch ${branchId.name.name} owner expected Owner.Adt(SomeError), got $other")
              }
          }
        }
    }

    "transitively absorb branches through chained `+` (Q-M20-5)" in {
      (loader: BaboonLoader[F]) =>
        val paths = resolveBaboonFiles("baboon/m20-ok/chained-include.baboon")
        for {
          outcome <- runPipeline(loader, paths)
        } yield {
          val family   = outcome.toOption.getOrElse(throw new AssertionError(s"expected success, got: $outcome"))
          val adtA     = expectedAdt(family, "my.ok.m20.chained", "Lvl1")
          val branches = adtA.members.toList.map(_.name.name).toSet
          // Lvl1 `+ Lvl2`, Lvl2 `+ Lvl3`, Lvl3 { Foo, Bar } → Lvl1 absorbs both via topo-ordered expansion.
          assert(branches == Set("Foo", "Bar"), s"expected {Foo, Bar} via chained include, got $branches")
        }
    }

    "exclude a single branch via `- X.Foo` (Q-FU-2 dot-prefixed subtraction)" in {
      (loader: BaboonLoader[F]) =>
        val paths = resolveBaboonFiles("baboon/m20-ok/include-and-exclude.baboon")
        for {
          outcome <- runPipeline(loader, paths)
        } yield {
          val family   = outcome.toOption.getOrElse(throw new AssertionError(s"expected success, got: $outcome"))
          val adt      = expectedAdt(family, "my.ok.m20.exclude", "SomeError")
          val branches = adt.members.toList.map(_.name.name).toSet
          // SomeError = ErrorAtom { Forbidden, SomeBranch } − ErrorAtom.SomeBranch = { Forbidden }.
          assert(branches == Set("Forbidden"), s"expected {Forbidden}, got $branches")
        }
    }

    "keep a locally-redefined branch after `+ Base; - Base.X; data X {...}` (override form)" in {
      // Regression: `- Shared.S2` previously excluded by NAME across both inherited AND local
      // branches, silently dropping the local `data S2 { f1: i32 }`. The local declaration is
      // authoritative: subtraction targets only the inherited branch, so the redefined S2 survives.
      (loader: BaboonLoader[F]) =>
        val paths = resolveBaboonFiles("baboon/m20-ok/redefine-after-exclude.baboon")
        for {
          outcome <- runPipeline(loader, paths)
        } yield {
          val family   = outcome.toOption.getOrElse(throw new AssertionError(s"expected success, got: $outcome"))
          val domain   = domainFor(family, "my.ok.m20.redefine")
          val adt      = adtFromDomain(domain, "A1", "my.ok.m20.redefine")
          val branches = adt.members.toList.map(_.name.name).toSet
          // A1 = (+ Shared {S1, S2}) − Shared.S2 + local {S2(f1), B1} = { S1, S2, B1 }.
          assert(branches == Set("S1", "S2", "B1"), s"expected {S1, S2, B1}, got $branches")
          // The surviving S2 is the LOCAL redefinition (carries field f1), not the inherited empty one.
          val struct = branchStructure(adt, domain)
          assert(struct.get("S2").exists(_.exists(_.startsWith("f1:"))), s"S2 should be the local redefinition with field f1, got ${struct.get("S2")}")
        }
    }

    "compute branch intersection via `^ X`" in {
      (loader: BaboonLoader[F]) =>
        val paths = resolveBaboonFiles("baboon/m20-ok/intersect.baboon")
        for {
          outcome <- runPipeline(loader, paths)
        } yield {
          val family   = outcome.toOption.getOrElse(throw new AssertionError(s"expected success, got: $outcome"))
          val adt      = expectedAdt(family, "my.ok.m20.intersect", "IntX")
          val branches = adt.members.toList.map(_.name.name).toSet
          // IntX = (IntA ∪ ∅) ∩ IntB branch-names. IntA = {Foo, Bar, Baz}, IntB = {Foo, Bar, Other}; ∩ = {Foo, Bar}.
          assert(branches == Set("Foo", "Bar"), s"expected {Foo, Bar} from intersection, got $branches")
        }
    }

    "multi-`^` arms compose by UNION of intersect targets, not pairwise intersection (PR-E / PR-63-D04)" in {
      // Fixture: MiResult = (+ X + Y) ^ A ^ B
      //   candidate set after includes: X∪Y branch names = {X1, Common, Y1}
      //   intersectNames = ⋃{A,B} = {X1, Foo} ∪ {Y1, Bar} = {X1, Foo, Y1, Bar}
      //   afterIntersect = {X1, Y1}  (Common is filtered out — not in any intersect target)
      //
      //   Counter-check: pairwise intersection A∩B = {} — that path would emit EmptyAdt,
      //   which does NOT happen. Passing here proves union-of-targets semantics.
      (loader: BaboonLoader[F]) =>
        val paths = resolveBaboonFiles("baboon/m20-ok/multi-intersect.baboon")
        for {
          outcome <- runPipeline(loader, paths)
        } yield {
          val family   = outcome.toOption.getOrElse(throw new AssertionError(s"expected success, got: $outcome"))
          val adt      = expectedAdt(family, "my.ok.m20.multi_intersect", "MiResult")
          val branches = adt.members.toList.map(_.name.name).toSet
          // Union-of-targets: X1 (in A) and Y1 (in B) survive; Common (in neither A nor B) is filtered.
          assert(branches == Set("X1", "Y1"), s"expected {X1, Y1} from multi-^ union-of-targets, got $branches")
          // Confirm pairwise-intersection false-negative: neither arm alone would keep both X1 and Y1.
          assert(!branches.contains("Common"), s"Common should be filtered out by intersect, got $branches")
        }
    }

    "resolve namespace-qualified ADT include `+ sub.MyAdt` (PR-62-D01 invariant)" in {
      (loader: BaboonLoader[F]) =>
        val paths = resolveBaboonFiles("baboon/m20-ok/namespace-qualified.baboon")
        for {
          outcome <- runPipeline(loader, paths)
        } yield {
          val family   = outcome.toOption.getOrElse(throw new AssertionError(s"expected success, got: $outcome"))
          val adt      = expectedAdt(family, "my.ok.m20.nsqual", "E")
          val branches = adt.members.toList.map(_.name.name).toSet
          assert(branches == Set("Foo"), s"expected {Foo} from namespaced include, got $branches")
        }
    }

    "produce structurally identical branch set for hand-written vs sugared ADT (plan §5)" in {
      // PR-63-D02: the prior test compared branch names across two separate domains
      // (my.ok.m20.equivmanual vs my.ok.m20.equivsugared). Deep-schema-id comparison is
      // structurally impossible across different packages because `deepSchemaRepr` embeds the
      // package name via `enquiries.wrap` — different packages produce different hash inputs
      // regardless of structural identity.
      //
      // This test uses a single domain (my.ok.m20.equiv) containing both ManualVer (hand-written
      // branches) and SugaredVer (same branches via `+ ErrorAtom`). Both ADTs are extracted from
      // the same domain, so field TypeRef values are identical by construction for same-named
      // fields, and the structural comparison is meaningful.
      //
      // What is verified: the typer-early pass produces the same branch names AND the same field
      // signatures (name + TypeRef string) for each branch. This is the plan §5 invariant: the
      // sugared form is semantically equivalent to the manual form at the Typedef level.
      (loader: BaboonLoader[F]) =>
        val paths = resolveBaboonFiles("baboon/m20-ok/desugar-equiv.baboon")
        for {
          outcome <- runPipeline(loader, paths)
        } yield {
          val family        = outcome.toOption.getOrElse(throw new AssertionError(s"expected success, got: $outcome"))
          val domain        = domainFor(family, "my.ok.m20.equiv")
          val manualAdt     = adtFromDomain(domain, "ManualVer", "my.ok.m20.equiv")
          val sugaredAdt    = adtFromDomain(domain, "SugaredVer", "my.ok.m20.equiv")
          val manualStruct  = branchStructure(manualAdt, domain)
          val sugaredStruct = branchStructure(sugaredAdt, domain)
          assert(
            manualStruct == sugaredStruct,
            s"branch structures differ:\n  manual=$manualStruct\n  sugared=$sugaredStruct",
          )
        }
    }

    // ---------- negative tests ----------

    "reject cycle `A + B; B + A` with CircularInheritance" in {
      (loader: BaboonLoader[F]) =>
        val paths = resolveBaboonFiles("baboon-fixtures-bad/m20-bad/cycle.baboon")
        for {
          outcome <- runPipeline(loader, paths)
        } yield {
          assertProducesTyperIssue[TyperIssue.CircularInheritance](outcome)
        }
    }

    "reject `+ NonAdtType` with WrongAdtInclusion" in {
      (loader: BaboonLoader[F]) =>
        val paths = resolveBaboonFiles("baboon-fixtures-bad/m20-bad/non-adt-include.baboon")
        for {
          outcome <- runPipeline(loader, paths)
        } yield {
          assertProducesTyperIssue[TyperIssue.WrongAdtInclusion](outcome)
        }
    }

    "reject branch-name collision with DuplicatedAdtBranches (Q-M20-2)" in {
      (loader: BaboonLoader[F]) =>
        val paths = resolveBaboonFiles("baboon-fixtures-bad/m20-bad/name-collision.baboon")
        for {
          outcome <- runPipeline(loader, paths)
        } yield {
          assertProducesTyperIssue[TyperIssue.DuplicatedAdtBranches](outcome)
        }
    }

    "reject empty intersection `+ A; ^ B` (disjoint) with EmptyAdt (Q-M20-4)" in {
      (loader: BaboonLoader[F]) =>
        val paths = resolveBaboonFiles("baboon-fixtures-bad/m20-bad/empty-intersection.baboon")
        for {
          outcome <- runPipeline(loader, paths)
        } yield {
          assertProducesTyperIssue[TyperIssue.EmptyAdt](outcome)
        }
    }

  }
}
