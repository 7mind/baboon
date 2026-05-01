package io.septimalmind.baboon.tests

import io.septimalmind.baboon.BaboonLoader
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.tests.BaboonTest.BaboonTestModule
import io.septimalmind.baboon.typer.model.{BaboonFamily, Domain, Owner, Pkg, TypeId, TypeName, Version}
import izumi.functional.bio.{Error2, F}
import izumi.fundamentals.collections.nonempty.NEList
import izumi.fundamentals.platform.files.IzFiles
import izumi.fundamentals.platform.resources.IzResources
import izumi.reflect.TagKK

import java.nio.file.Path

/** PR-J (M24 Phase 3.4) — `derived[was]` policy (b) preserve-verbatim through ADT re-emit.
  *
  * Fixture: `baboon/m20-was-propagation/{v1,v2}.baboon`.
  *
  *   - v1 declares `adt Inner { data Foo { x: i32 } }` and `adt Outer { + Inner }`. After PR-63
  *     ADT inheritance expansion, both `Inner.Foo` and a re-emitted `Outer.Foo` exist.
  *   - v2 renames the branch: `adt Inner { data Bar : was[Foo] { x: i32 } }` and the same
  *     `adt Outer { + Inner }`. After expansion, `Inner.Bar` and re-emitted `Outer.Bar` exist;
  *     each carries the `was[Foo]` annotation verbatim.
  *
  * Policy (b) (locked in M24 planning): the receiving ADT's scope is the resolution context for
  * unprefixed `was` refs on re-emitted branches. The `was[Foo]` annotation propagated into
  * `Outer.Bar` via `+ Inner` must resolve `Foo` under `Owner.Adt(Outer)`, NOT under
  * `Owner.Adt(Inner)`.
  *
  * The implementation chain that makes this true (no code change needed in PR-J):
  *
  *   1. `AdtInheritanceExpander.resolveArm` All-form path copies the entire `RawAdtMemberDto`
  *      (including `dto.derived: Set[RawMemberMeta]`) verbatim from `Inner`'s expanded members
  *      into `Outer`'s member list.
  *   2. `BaboonTyper.runTyper` rebuilds the scope tree over the expanded `RawTLDef` list, so the
  *      re-emitted `Outer.Bar` becomes a NestedScope owned by `Outer`.
  *   3. `BaboonTyper.computeRenames` walks the rebuilt flattened scope list. For each
  *      `RawMemberMeta.Was(ref)` with empty prefix, `oldOwner = newId.owner`. The re-emitted
  *      `Outer.Bar` has `newId.owner = Owner.Adt(Outer)`, so unprefixed `Foo` resolves to
  *      `(pkg, Owner.Adt(Outer), Foo)` — policy (b) realised.
  *
  * Asserted via `domain.renames` (the `Map[TypeId.User, TypeId.User]` from new IDs to old IDs):
  *
  *   - v2's `(pkg, Owner.Adt(Inner), Bar)` maps to `(pkg, Owner.Adt(Inner), Foo)` (sanity check
  *     of the local-branch case).
  *   - v2's `(pkg, Owner.Adt(Outer), Bar)` maps to `(pkg, Owner.Adt(Outer), Foo)` (the policy-(b)
  *     assertion: re-emit re-anchors the unprefixed `was` ref under the receiving ADT).
  */
final class M20DerivedWasPropagationTest extends M20DerivedWasPropagationTestBase[Either]

abstract class M20DerivedWasPropagationTestBase[F[+_, +_]: Error2: TagKK: BaboonTestModule] extends BaboonTest[F] {

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

  private def domainAt(family: BaboonFamily, pkg: Pkg, version: Version): Domain = {
    val lineage = family.domains.toMap.getOrElse(
      pkg,
      throw new AssertionError(s"no lineage for pkg '$pkg' in family with pkgs: ${family.domains.toMap.keys.toList}"),
    )
    lineage.versions.toMap.getOrElse(
      version,
      throw new AssertionError(s"no version $version in lineage for $pkg; versions: ${lineage.versions.toMap.keys.toList}"),
    )
  }

  "M20 derived[was] propagation through ADT re-emit (PR-J / M24)" should {

    "preserve `was[Foo]` verbatim and resolve unprefixed Foo under the receiving ADT (policy (b))" in {
      (loader: BaboonLoader[F]) =>
        val paths = resolveBaboonFiles("baboon/m20-was-propagation")
        for {
          outcome <- runPipeline(loader, paths)
        } yield {
          val family = outcome.toOption.getOrElse(throw new AssertionError(s"expected success, got: $outcome"))
          val pkg    = Pkg(NEList("my", "ok", "m20", "was_propagation"))
          val v2     = domainAt(family, pkg, Version.parse("2.0.0"))

          val innerId = TypeId.User(pkg, Owner.Toplevel, TypeName("Inner"))
          val outerId = TypeId.User(pkg, Owner.Toplevel, TypeName("Outer"))

          val innerBarNew = TypeId.User(pkg, Owner.Adt(innerId), TypeName("Bar"))
          val innerFooOld = TypeId.User(pkg, Owner.Adt(innerId), TypeName("Foo"))
          val outerBarNew = TypeId.User(pkg, Owner.Adt(outerId), TypeName("Bar"))
          val outerFooOld = TypeId.User(pkg, Owner.Adt(outerId), TypeName("Foo"))

          // Sanity: local rename on the originally-declared branch is recorded.
          assert(
            v2.renames.get(innerBarNew).contains(innerFooOld),
            s"expected Inner.Bar -> Inner.Foo in renames; got: ${v2.renames}",
          )

          // Policy (b): the re-emitted Outer.Bar carries the `was[Foo]` annotation verbatim, and
          // the unprefixed `Foo` resolves under the RECEIVING ADT (Outer), not the source ADT
          // (Inner). If `was` propagation re-anchored to the source ADT we would see
          // `Outer.Bar -> Inner.Foo` instead — that's policy (a), explicitly rejected.
          assert(
            v2.renames.get(outerBarNew).contains(outerFooOld),
            s"expected re-emitted Outer.Bar -> Outer.Foo in renames (policy (b)); got: ${v2.renames}",
          )
          assert(
            !v2.renames.get(outerBarNew).contains(innerFooOld),
            s"Outer.Bar must NOT resolve `was[Foo]` under Inner (that would be policy (a)); got: ${v2.renames}",
          )
        }
    }
  }
}
