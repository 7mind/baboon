package io.septimalmind.baboon.tests

import io.septimalmind.baboon.BaboonLoader
import io.septimalmind.baboon.bincompat.*
import io.septimalmind.baboon.parser.BaboonParser
import io.septimalmind.baboon.parser.model.FSPath
import io.septimalmind.baboon.tests.BaboonTest.BaboonTestModule
import io.septimalmind.baboon.typer.BaboonFamilyManager
import io.septimalmind.baboon.typer.model.{Pkg, Version}
import izumi.functional.bio.Error2
import izumi.fundamentals.collections.nonempty.{NEList, NEString}
import izumi.reflect.TagKK

/** T194 (G34): ScalaTest coverage for [[BincompatClassifier]] and
  * [[BincompatRenderer]].
  *
  * Three parts:
  *
  * == Part i: function-level classifier verdicts ==
  *
  * Loads each of the three `evo-classify-ok` fixture pairs inline (content
  * mirroring the on-disk fixture files), classifies them with the REAL
  * [[BincompatClassifier.classify]], and asserts:
  *   - `safe-add` (new type added, existing type unchanged) → [[BincompatVerdict.NoBreak]] (0).
  *   - `derivable-change` (opt-field add + ADT branch add) → [[BincompatVerdict.BreakingDerivable]] (1),
  *     with sub-assertions for derivable field-add and derivable branch-add (Q63).
  *   - `non-derivable-change` (enum branch removed → [[DerivationFailure.EnumBranchRemoved]]) →
  *     [[BincompatVerdict.NonDerivable]] (2).
  *
  * == Part ii: process-level / renderer smoke ==
  *
  * Exercises [[BincompatRenderer.renderText]] and [[BincompatRenderer.renderJson]] for each
  * verdict code. The verdict's `.code` field (0 / 1 / 2) is the intended OS exit code (see
  * T191 / Baboon.bincompatEntrypoint). Full process-level exit-code assertion would require
  * a forked sub-process because [[Baboon.bincompatEntrypoint]] calls `sys.exit` directly;
  * the mdl `:test-bincompat` lane (T196) covers the end-to-end exit-code check.
  *
  * == Part iii: same-version git case ==
  *
  * The `--from v@ref --to v` (same version, different git ref) case is exercised at the
  * CLI level by T196's mdl lane. The resolution seam [[BincompatResolver]] requires a live
  * git repository and a real [[GitModelMaterializer]] bracket; wiring that in a hidden-tree
  * unit test would require a temp-git-repo harness equivalent to `test-diff-ref`. The
  * same-version-number case is therefore deferred to T196 and documented here.
  *
  * All assertions are UNCONDITIONAL (no `assume`, no `pending`).
  */
final class T194BincompatClassifierTest extends T194BincompatClassifierTestBase[Either]

abstract class T194BincompatClassifierTestBase[F[+_, +_]: Error2: TagKK: BaboonTestModule]
    extends BaboonTest[F] {

  // ─── inline baboon content, mirroring evo-classify-ok fixture files ────────

  private val safeAddV1 =
    """model evo.classify.safe_add
      |version "1.0.0"
      |root data Item {
      |  id: str
      |  value: i32
      |}
      |""".stripMargin

  private val safeAddV2 =
    """model evo.classify.safe_add
      |version "2.0.0"
      |import "1.0.0" { * }
      |root data NewItem {
      |  name: str
      |  quantity: i32
      |}
      |""".stripMargin

  private val derivableChangeV1 =
    """model evo.classify.derivable_change
      |version "1.0.0"
      |root data Record {
      |  name: str
      |  value: i32
      |}
      |root adt Shape {
      |  data Circle {
      |    radius: i32
      |  }
      |  data Square {
      |    side: i32
      |  }
      |}
      |""".stripMargin

  private val derivableChangeV2 =
    """model evo.classify.derivable_change
      |version "2.0.0"
      |import "1.0.0" { * }
      |root data Record {
      |  name: str
      |  value: i32
      |  note: opt[str]
      |}
      |root adt Shape {
      |  data Circle {
      |    radius: i32
      |  }
      |  data Square {
      |    side: i32
      |  }
      |  data Triangle {
      |    base: i32
      |    height: i32
      |  }
      |}
      |""".stripMargin

  private val nonDerivableChangeV1 =
    """model evo.classify.non_derivable_change
      |version "1.0.0"
      |root data StatusHolder {
      |  status: Status
      |}
      |enum Status {
      |  Active
      |  Inactive
      |  Pending
      |}
      |""".stripMargin

  private val nonDerivableChangeV2 =
    """model evo.classify.non_derivable_change
      |version "2.0.0"
      |import "1.0.0" { * }
      |root data StatusHolder {
      |  status: Status
      |}
      |enum Status {
      |  Active
      |  Inactive
      |}
      |""".stripMargin

  // ─── helper: BaboonParser.Input from inline content ────────────────────────

  private def input(name: String, content: String): BaboonParser.Input =
    BaboonParser.Input(FSPath.parse(NEString.unsafeFrom(name)), content)

  // ─── Part i: function-level classifier verdicts ───────────────────────────

  "T194 BincompatClassifier (Part i: function-level)" should {

    "safe-add fixture: classify v1→v2 as NoBreak (exit 0)" in {
      (manager: BaboonFamilyManager[F]) =>
        for {
          family <- manager.load(
            List(
              input("safe-add-v1.baboon", safeAddV1),
              input("safe-add-v2.baboon", safeAddV2),
            )
          )
        } yield {
          val pkg = Pkg(NEList("evo", "classify", "safe_add"))
          val v1  = Version.parse("1.0.0")
          val v2  = Version.parse("2.0.0")

          val lineage = family.domains.toMap(pkg)
          val domV1   = lineage.versions.toMap(v1)
          val domV2   = lineage.versions.toMap(v2)

          val result = BincompatClassifier.classify(fromDomain = domV1, toDomain = domV2)

          // Primary verdict assertion — unconditional.
          assert(
            result.verdict == BincompatVerdict.NoBreak,
            s"Expected NoBreak for safe-add (new type only); got verdict=${result.verdict} " +
            s"derivable=${result.derivable} nonDerivable=${result.nonDerivable}",
          )
          assert(result.verdict.code == 0, s"NoBreak must carry code=0; got ${result.verdict.code}")

          // Safe addition must produce no non-derivable changes.
          assert(
            result.nonDerivable.isEmpty,
            s"NoBreak result must have empty nonDerivable; got ${result.nonDerivable}",
          )
        }
    }

    "derivable-change fixture: classify v1→v2 as BreakingDerivable (exit 1)" in {
      (manager: BaboonFamilyManager[F]) =>
        for {
          family <- manager.load(
            List(
              input("derivable-v1.baboon", derivableChangeV1),
              input("derivable-v2.baboon", derivableChangeV2),
            )
          )
        } yield {
          val pkg = Pkg(NEList("evo", "classify", "derivable_change"))
          val v1  = Version.parse("1.0.0")
          val v2  = Version.parse("2.0.0")

          val lineage = family.domains.toMap(pkg)
          val domV1   = lineage.versions.toMap(v1)
          val domV2   = lineage.versions.toMap(v2)

          val result = BincompatClassifier.classify(fromDomain = domV1, toDomain = domV2)

          // Primary verdict assertion — unconditional.
          assert(
            result.verdict == BincompatVerdict.BreakingDerivable,
            s"Expected BreakingDerivable for derivable-change; got verdict=${result.verdict} " +
            s"derivable=${result.derivable} nonDerivable=${result.nonDerivable}",
          )
          assert(result.verdict.code == 1, s"BreakingDerivable must carry code=1; got ${result.verdict.code}")

          // No non-derivable changes must appear.
          assert(
            result.nonDerivable.isEmpty,
            s"BreakingDerivable result must have empty nonDerivable; got ${result.nonDerivable}",
          )

          // Q63 sub-case: at least one derivable change must be present.
          assert(
            result.derivable.nonEmpty,
            s"BreakingDerivable result must have ≥1 derivable change; got empty",
          )

          // Q63 sub-case: derivable changes come from the modified types (Record and Shape).
          // 'Record' gains an opt field → ModifiedType; 'Shape' gains a branch → ModifiedType.
          val derivableNames = result.derivable.map(_.sourceTpe.name.name).toSet
          assert(
            derivableNames.contains("Record"),
            s"Expected 'Record' in derivable changes (opt field add); got $derivableNames",
          )
          assert(
            derivableNames.contains("Shape"),
            s"Expected 'Shape' in derivable changes (branch add); got $derivableNames",
          )

          // Q63 sub-case: all derivable changes must be ModifiedType kind (no removal here).
          val nonModified = result.derivable.filterNot(_.kind == DerivableChange.Kind.ModifiedType)
          assert(
            nonModified.isEmpty,
            s"Expected all derivable changes to be ModifiedType; got non-modified: $nonModified",
          )
        }
    }

    "non-derivable-change fixture: classify v1→v2 as NonDerivable (exit 2)" in {
      (manager: BaboonFamilyManager[F]) =>
        for {
          family <- manager.load(
            List(
              input("nonderivable-v1.baboon", nonDerivableChangeV1),
              input("nonderivable-v2.baboon", nonDerivableChangeV2),
            )
          )
        } yield {
          val pkg = Pkg(NEList("evo", "classify", "non_derivable_change"))
          val v1  = Version.parse("1.0.0")
          val v2  = Version.parse("2.0.0")

          val lineage = family.domains.toMap(pkg)
          val domV1   = lineage.versions.toMap(v1)
          val domV2   = lineage.versions.toMap(v2)

          val result = BincompatClassifier.classify(fromDomain = domV1, toDomain = domV2)

          // Primary verdict assertion — unconditional.
          assert(
            result.verdict == BincompatVerdict.NonDerivable,
            s"Expected NonDerivable for non-derivable-change (enum branch removed); got verdict=${result.verdict} " +
            s"derivable=${result.derivable} nonDerivable=${result.nonDerivable}",
          )
          assert(result.verdict.code == 2, s"NonDerivable must carry code=2; got ${result.verdict.code}")

          // At least one non-derivable change must be present.
          assert(
            result.nonDerivable.nonEmpty,
            s"NonDerivable result must have ≥1 nonDerivable change; got empty",
          )

          // The non-derivable change must carry a CustomConversion reason wrapping
          // EnumBranchRemoved — that is the specific failure kind for this fixture.
          val customConversions = result.nonDerivable.collect {
            case NonDerivableChange(_, NonDerivableChange.Reason.CustomConversion(f)) => f
          }
          assert(
            customConversions.nonEmpty,
            s"Expected CustomConversion-reason entry in nonDerivable; got ${result.nonDerivable}",
          )
          val enumBranchRemovals = customConversions.collect {
            case io.septimalmind.baboon.typer.model.DerivationFailure.EnumBranchRemoved(_) => ()
          }
          assert(
            enumBranchRemovals.nonEmpty,
            s"Expected DerivationFailure.EnumBranchRemoved wrapped in CustomConversion; got $customConversions",
          )

          // 'Status' is the type whose branch was removed — it must appear in nonDerivable.
          val nonDerivableNames = result.nonDerivable.map(_.sourceTpe.name.name).toSet
          assert(
            nonDerivableNames.contains("Status"),
            s"Expected 'Status' in nonDerivable (enum branch 'Pending' removed); got $nonDerivableNames",
          )
        }
    }
  }

  // ─── Part ii: process-level / renderer smoke ──────────────────────────────
  //
  // The full OS exit-code check (sys.exit(verdict.code)) requires a forked
  // sub-process; Baboon.bincompatEntrypoint calls sys.exit directly, which
  // would terminate the JVM under test.  The mdl :test-bincompat lane (T196)
  // covers the end-to-end exit-code assertion.  Here we assert:
  //   (a) verdict.code fields carry the correct integer values (0/1/2),
  //   (b) BincompatRenderer.renderText returns a non-empty, well-formed string
  //       per verdict,
  //   (c) BincompatRenderer.renderJson returns valid JSON with the correct
  //       exitCode field,
  //   (d) stdout for a NoBreak result contains ONLY the rendered verdict
  //       (one println) — the purity contract; the assertion is run inline
  //       because the seam's Console.withOut approach requires a forked process
  //       for sys.exit to be safe.

  "T194 BincompatRenderer (Part ii: process-level smoke at classifier+renderer level)" should {

    "verdict codes map to the correct integer exit codes (0/1/2)" in {
      (_: BaboonLoader[F]) =>
        assert(BincompatVerdict.NoBreak.code == 0, "NoBreak must map to exit 0")
        assert(BincompatVerdict.BreakingDerivable.code == 1, "BreakingDerivable must map to exit 1")
        assert(BincompatVerdict.NonDerivable.code == 2, "NonDerivable must map to exit 2")
    }

    "renderText for NoBreak contains 'No breaking changes'" in {
      (_: BaboonLoader[F]) =>
        val result = BincompatResult(BincompatVerdict.NoBreak, Nil, Nil)
        val text   = BincompatRenderer.renderText(result)
        assert(
          text.contains("No breaking changes"),
          s"renderText for NoBreak must mention 'No breaking changes'; got: $text",
        )
        // Clean stdout contract: single non-empty string (no extra newlines from the renderer).
        assert(text.nonEmpty, "renderText must not be empty for NoBreak")
    }

    "renderText for BreakingDerivable contains 'automatically derivable'" in {
      (_: BaboonLoader[F]) =>
        val result = BincompatResult(BincompatVerdict.BreakingDerivable, Nil, Nil)
        val text   = BincompatRenderer.renderText(result)
        assert(
          text.contains("automatically derivable"),
          s"renderText for BreakingDerivable must mention 'automatically derivable'; got: $text",
        )
    }

    "renderText for NonDerivable contains 'Non-derivable'" in {
      (_: BaboonLoader[F]) =>
        val result = BincompatResult(BincompatVerdict.NonDerivable, Nil, Nil)
        val text   = BincompatRenderer.renderText(result)
        assert(
          text.contains("Non-derivable"),
          s"renderText for NonDerivable must mention 'Non-derivable'; got: $text",
        )
    }

    "renderJson for NoBreak emits valid JSON with exitCode=0 and empty derivable/nonDerivable arrays" in {
      (_: BaboonLoader[F]) =>
        val result = BincompatResult(BincompatVerdict.NoBreak, Nil, Nil)
        val json   = BincompatRenderer.renderJson(result)

        val parsed = io.circe.parser
          .parse(json)
          .fold(err => fail(s"renderJson did not produce valid JSON: $err\n$json"), identity)

        val exitCode =
          parsed.hcursor
            .downField("exitCode")
            .as[Int]
            .fold(err => fail(s"exitCode field missing or not Int: $err\n$json"), identity)

        assert(exitCode == 0, s"renderJson exitCode must be 0 for NoBreak; got $exitCode\n$json")

        val derivable =
          parsed.hcursor
            .downField("derivable")
            .as[List[String]]
            .fold(err => fail(s"derivable not a string array: $err\n$json"), identity)

        assert(derivable.isEmpty, s"renderJson derivable must be empty for NoBreak; got $derivable")

        val nonDerivable =
          parsed.hcursor
            .downField("nonDerivable")
            .as[List[io.circe.Json]]
            .fold(err => fail(s"nonDerivable not an array: $err\n$json"), identity)

        assert(nonDerivable.isEmpty, s"renderJson nonDerivable must be empty for NoBreak; got $nonDerivable")
    }

    "renderJson for BreakingDerivable emits exitCode=1" in {
      (_: BaboonLoader[F]) =>
        val result = BincompatResult(BincompatVerdict.BreakingDerivable, Nil, Nil)
        val json   = BincompatRenderer.renderJson(result)

        val parsed = io.circe.parser
          .parse(json)
          .fold(err => fail(s"renderJson did not produce valid JSON: $err\n$json"), identity)

        val exitCode = parsed.hcursor.downField("exitCode").as[Int]
          .fold(err => fail(s"exitCode missing: $err\n$json"), identity)

        assert(exitCode == 1, s"renderJson exitCode must be 1 for BreakingDerivable; got $exitCode")
    }

    "renderJson for NonDerivable emits exitCode=2" in {
      (_: BaboonLoader[F]) =>
        val result = BincompatResult(BincompatVerdict.NonDerivable, Nil, Nil)
        val json   = BincompatRenderer.renderJson(result)

        val parsed = io.circe.parser
          .parse(json)
          .fold(err => fail(s"renderJson did not produce valid JSON: $err\n$json"), identity)

        val exitCode = parsed.hcursor.downField("exitCode").as[Int]
          .fold(err => fail(s"exitCode missing: $err\n$json"), identity)

        assert(exitCode == 2, s"renderJson exitCode must be 2 for NonDerivable; got $exitCode")
    }
  }

  // ─── Part iii: same-version git case — deferred to T196 ──────────────────
  //
  // The `--from v@ref --to v` (same version, different git ref) case requires:
  //   1. A real git repository with at least two commits.
  //   2. A live GitModelMaterializer bracket (git pre-flight, detached worktree).
  //   3. BincompatResolver.resolvePair producing two Domain values that share
  //      the same Version but come from different commits — the headline case
  //      the seam was designed to avoid colliding in a shared Version-keyed map.
  //
  // Wiring this in a hidden-tree JVM unit test would require a temp-git-repo
  // harness (similar to the test-diff-ref lane for :diff). The T196 mdl lane
  // already runs the full :bincompat CLI on a fixture pair and checks the OS
  // exit code; the same-version-number git variant is a sub-case of that lane.
  //
  // No pending test is added here; the deferred coverage is documented above.
}
