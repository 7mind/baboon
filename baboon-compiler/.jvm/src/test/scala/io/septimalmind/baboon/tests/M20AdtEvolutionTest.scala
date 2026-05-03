package io.septimalmind.baboon.tests

import io.septimalmind.baboon.parser.BaboonParser
import io.septimalmind.baboon.parser.model.FSPath
import io.septimalmind.baboon.tests.BaboonTest.BaboonTestModule
import io.septimalmind.baboon.typer.BaboonFamilyManager
import io.septimalmind.baboon.typer.model.Conversion.{CopyAdtBranchByName, CustomConversionRequired}
import io.septimalmind.baboon.typer.model.{AdtOp, BaboonLineage, EvolutionStep, Pkg, TypeId, TypedefDiff, Version}
import izumi.functional.bio.Error2
import izumi.fundamentals.collections.nonempty.{NEList, NEString}
import izumi.reflect.TagKK

/** PR-64 (M20.3): cross-version evolution test for the desugaring-equivalence invariant.
  *
  * Plan `docs/drafts/20260429-0025-m20-bab-a03-adt-inheritance-plan.md` §5 states:
  *
  *   "the source change `Foo, Bar` -> `+ ErrorAtom; Foo, Bar` is non-evolutionary if the
  *    resulting branch sets coincide".
  *
  * Mechanically: per `Q-FU-3` the typer-early pass rewrites every `+ X` arm into the
  * literal `RawAdtMemberDto` entries pulled from the included ADT. The rewrite happens
  * before deep-schema hashing (`BaboonTyper.deepSchemaRepr`), and the re-emitted branches
  * are owned by the *receiving* ADT (`Owner.Adt(SomeError)` per Q-M20-1). Therefore a v1
  * declaring `data Forbidden; data Bar` literally and a v2 declaring
  * `+ ErrorAtom; data Bar` (where `ErrorAtom` already contains `Forbidden`) yield branches
  * with identical `TypeId`s in both versions: same `Pkg`, same `Owner.Adt(SomeError)`,
  * same `TypeName`. `BaboonComparator.diffAdts` (non-renamed branch) compares by `TypeId`,
  * so the branch set is fully `keptMembers` — no `AddBranch` / `RemoveBranch` ops.
  *
  * Complement to `M20AdtInheritanceFrontEndTest`'s desugaring-equivalence test, which
  * compares two ADTs *within a single domain*. This test compares ACROSS versions: the
  * comparator and rules engine see v1's manual ADT and v2's sugared ADT and must agree
  * that the change is non-evolutionary.
  */
final class M20AdtEvolutionTest extends M20AdtEvolutionTestBase[Either]

abstract class M20AdtEvolutionTestBase[F[+_, +_]: Error2: TagKK: BaboonTestModule] extends BaboonTest[F] {

  private def makeInput(name: String, body: String): BaboonParser.Input =
    BaboonParser.Input(FSPath.parse(NEString.unsafeFrom(name)), body)

  // v1: SomeError declares Forbidden + Bar literally (no inheritance arms).
  private val v1Body =
    """model adt.evolution
      |version "1.0.0"
      |adt ErrorAtom {
      |  data Forbidden { code: i32 }
      |}
      |root adt SomeError {
      |  data Forbidden { code: i32 }
      |  data Bar { x: i32 }
      |}
      |""".stripMargin

  // v2: SomeError uses `+ ErrorAtom` to absorb Forbidden — same resulting branch set.
  private val v2Body =
    """model adt.evolution
      |version "2.0.0"
      |import "1.0.0" { * }
      |adt ErrorAtom {
      |  data Forbidden { code: i32 }
      |}
      |root adt SomeError {
      |  + ErrorAtom
      |  data Bar { x: i32 }
      |}
      |""".stripMargin

  "M20 ADT inheritance evolution (PR-64)" should {

    // Plan §5: the manual->sugared rewrite must produce no breaking change.
    //
    // Branches in v1 and v2 share `TypeId` (same `Pkg`, same `Owner.Adt(SomeError)`, same
    // `TypeName`) per the re-emit semantics — so `BaboonComparator.diffAdts` puts every
    // branch in `keptMembers` (no `AddBranch`, no `RemoveBranch`), and `BaboonRules` emits
    // a derivable `CopyAdtBranchByName` (NOT `CustomConversionRequired`).
    //
    // Note on `SomeError`'s classification: post-expansion `SomeError` lands in
    // `unmodified` because PR-D sorted ADT branches in `BaboonTyper.deepSchemaRepr`
    // before hashing. v1 sources `Forbidden, Bar` literally; v2's sugared expansion
    // via `AdtInheritanceExpander` produces `Bar, Forbidden` (localMembers first then
    // includeBranches). After sorting each branch repr by `mkString`, both orderings
    // produce the identical deep-schema string, so deep IDs match and `SomeError`
    // classifies as `unmodified`.
    "manual->sugared rewrite produces no breaking change in BaboonRules / BaboonComparator" in {
      (manager: BaboonFamilyManager[F]) =>
        for {
          family <- manager.load(
            List(
              makeInput("v1.baboon", v1Body),
              makeInput("v2.baboon", v2Body),
            )
          )
        } yield {
          val pkg  = Pkg(NEList("adt", "evolution"))
          val v1   = Version.parse("1.0.0")
          val v2   = Version.parse("2.0.0")
          val step = EvolutionStep(v1, v2)

          val lineage: BaboonLineage = family.domains.toMap(pkg)
          val evo                    = lineage.evolution
          val diff                   = evo.diffs(step)

          // SomeError must be classified as `unmodified`: after PR-D the branch sort in
          // `BaboonTyper.deepSchemaRepr` makes the manual->sugared rewrite produce identical
          // deep schema IDs regardless of declaration order.
          val unmodifiedIds: Set[TypeId.User] =
            diff.changes.unmodified.collect { case u: TypeId.User => u }

          val someErrorNonBreaking = unmodifiedIds.filter(_.name.name == "SomeError")
          assert(
            someErrorNonBreaking.size == 1,
            s"Expected SomeError in unmodified (identical deep schema IDs after branch sort); " +
            s"got unmodified=${diff.changes.unmodified}, shallowModified=${diff.changes.shallowModified}, " +
            s"deepModified=${diff.changes.deepModified}, fullyModified=${diff.changes.fullyModified}, " +
            s"removed=${diff.changes.removed}, renamed=${diff.changes.renamed}",
          )

          // Branches re-emitted from `+ ErrorAtom` must carry over with identical TypeId
          // and shape. `Forbidden` (the re-emitted branch) must be in the unmodified set.
          val unmodifiedNames = diff.changes.unmodified.collect { case u: TypeId.User => u.name.name }
          assert(
            unmodifiedNames.contains("Forbidden") && unmodifiedNames.contains("Bar"),
            s"Expected re-emitted Forbidden and local Bar in unmodified; got: $unmodifiedNames",
          )

          // Unmodified types are absent from `diff.diffs`. If SomeError appears (it
          // should not after the branch-sort fix), every op must still be KeepBranch.
          val someErrorId = someErrorNonBreaking.head
          diff.diffs.get(someErrorId) match {
            case None =>
              // Unmodified types are absent from `diff.diffs`. Strongest possible signal.
              ()
            case Some(TypedefDiff.AdtDiff(ops)) =>
              val nonKeep = ops.filterNot(_.isInstanceOf[AdtOp.KeepBranch])
              assert(
                nonKeep.isEmpty,
                s"Expected only KeepBranch ops for non-evolutionary rewrite; got: $nonKeep",
              )
              val keptNames = ops.collect { case k: AdtOp.KeepBranch => k.id.name.name }.toSet
              assert(
                keptNames == Set("Forbidden", "Bar"),
                s"Expected kept branches {Forbidden, Bar}, got $keptNames",
              )
            case other =>
              fail(s"Expected AdtDiff for SomeError, got: $other")
          }

          // BaboonRules must emit a derivable conversion for SomeError, not a custom one.
          // CopyAdtBranchByName == "compiler-derived branch-name copy"; CustomConversionRequired
          // == "user must implement". Plan §5 invariant: the manual->sugared rewrite must
          // land on the derivable path.
          val ruleset = evo.rules(step)
          val customForSomeError = ruleset.conversions.collect {
            case c: CustomConversionRequired if c.sourceTpe.name.name == "SomeError" => c
          }
          assert(
            customForSomeError.isEmpty,
            s"Expected no CustomConversionRequired for SomeError (non-breaking rewrite); got: $customForSomeError",
          )
          val derivedForSomeError = ruleset.conversions.collect {
            case c: CopyAdtBranchByName if c.sourceTpe.name.name == "SomeError" => c
          }
          assert(
            derivedForSomeError.size == 1,
            s"Expected CopyAdtBranchByName for SomeError; got: ${ruleset.conversions.filter(_.sourceTpe.name.name == "SomeError")}",
          )
          val branchMapping = derivedForSomeError.head.branchMapping
          assert(
            branchMapping.keySet == Set("Forbidden", "Bar"),
            s"Expected branchMapping keys {Forbidden, Bar}; got: ${branchMapping.keySet}",
          )
        }
    }
  }
}
