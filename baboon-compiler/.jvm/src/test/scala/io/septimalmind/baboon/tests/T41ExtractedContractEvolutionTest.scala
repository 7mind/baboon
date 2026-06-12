package io.septimalmind.baboon.tests

import io.septimalmind.baboon.parser.BaboonParser
import io.septimalmind.baboon.parser.model.FSPath
import io.septimalmind.baboon.tests.BaboonTest.BaboonTestModule
import io.septimalmind.baboon.typer.BaboonFamilyManager
import io.septimalmind.baboon.typer.model.{ContractOp, EvolutionStep, Pkg, TypeId, TypedefDiff, Version}
import izumi.functional.bio.Error2
import izumi.fundamentals.collections.nonempty.{NEList, NEString}
import izumi.reflect.TagKK

/** T41: evolution machinery covers extracted contracts (ContractDiff/ContractOp) with ZERO
  * production-code change — the diff is generic over contracts.
  *
  * Acceptance (a): a two-version model where v2 of a `has contract`-carrying template adds one
  * param-free field and removes another; the ContractDiff for B between v1 and v2 shows exactly
  * AddField + RemoveField + KeepField ops.
  *
  * Acceptance (b): a model where v2 uses `import "1.0.0" { * }` carrying the same
  * has-carrying template compiles successfully and re-synthesizes B in the importing version.
  */
final class T41ExtractedContractEvolutionTest extends T41ExtractedContractEvolutionTestBase[Either]

abstract class T41ExtractedContractEvolutionTestBase[F[+_, +_]: Error2: TagKK: BaboonTestModule] extends BaboonTest[F] {

  private def makeInput(name: String, body: String): BaboonParser.Input =
    BaboonParser.Input(FSPath.parse(NEString.unsafeFrom(name)), body)

  // ─── Acceptance (a): ContractDiff/ContractOp between v1 and v2 ──────────────

  // v1: Host[T] has param-free fields `keep: str` and `remove_me: i32`; template param T is
  // used by `parameterized: T` which is excluded from the extracted contract B (sentinel substitution
  // drops param-dependent fields). B therefore carries {keep: str, remove_me: i32}.
  // A root concrete instantiation is required so the types are retained after root-GC.
  private val v1Body: String =
    """model t41.contract.evolution
      |
      |version "1.0.0"
      |
      |data Host[T] {
      |  has contract B
      |  keep: str
      |  remove_me: i32
      |  parameterized: T
      |}
      |
      |root type Inst = Host[u32]
      |""".stripMargin

  // v2: Host[T] adds `added_in_v2: u64` and drops `remove_me: i32`; `keep: str` survives.
  // B in v2 therefore carries {keep: str, added_in_v2: u64}.
  private val v2Body: String =
    """model t41.contract.evolution
      |
      |version "2.0.0"
      |
      |import "1.0.0" { * }
      |
      |data Host[T] {
      |  has contract B
      |  keep: str
      |  added_in_v2: u64
      |  parameterized: T
      |}
      |
      |root type Inst = Host[u32]
      |""".stripMargin

  "T41 extracted-contract evolution (acceptance a)" should {

    "ContractDiff for B between v1 and v2 shows AddField, RemoveField, KeepField ops" in {
      (manager: BaboonFamilyManager[F]) =>
        for {
          family <- manager.load(
            List(
              makeInput("t41-v1.baboon", v1Body),
              makeInput("t41-v2.baboon", v2Body),
            )
          )
        } yield {
          val pkg  = Pkg(NEList("t41", "contract", "evolution"))
          val v1   = Version.parse("1.0.0")
          val v2   = Version.parse("2.0.0")
          val step = EvolutionStep(v1, v2)

          val lineage = family.domains.toMap(pkg)
          val diff    = lineage.evolution.diffs(step)

          // B must appear in the domain at both versions.
          val v1Domain = lineage.versions(v1)
          val v2Domain = lineage.versions(v2)
          val bInV1 = v1Domain.defs.meta.nodes.keys.collect { case u: TypeId.User if u.name.name == "B" => u }.toList
          val bInV2 = v2Domain.defs.meta.nodes.keys.collect { case u: TypeId.User if u.name.name == "B" => u }.toList
          assert(bInV1.size == 1, s"Expected B synthesized in v1 domain, got: ${v1Domain.defs.meta.nodes.keys.map { case u: TypeId.User => u.name.name; case o => o.toString }.toSet}")
          assert(bInV2.size == 1, s"Expected B synthesized in v2 domain, got: ${v2Domain.defs.meta.nodes.keys.map { case u: TypeId.User => u.name.name; case o => o.toString }.toSet}")

          val bId = bInV1.head

          // B must appear in the diff (it changed: remove_me removed, added_in_v2 added).
          val bDiff = diff.diffs.get(bId)
          assert(bDiff.isDefined, s"Expected ContractDiff for B in diff; diff.diffs keys: ${diff.diffs.keys.map { case u: TypeId.User => u.name.name; case o => o.toString }.toSet}")

          val ops = bDiff.get match {
            case TypedefDiff.ContractDiff(ops) => ops
            case other                         => fail(s"Expected ContractDiff for B, got: $other")
          }

          // Expect: KeepField(keep), AddField(added_in_v2), RemoveField(remove_me).
          val addedFieldNames   = ops.collect { case ContractOp.AddField(f) => f.name.name }.toSet
          val removedFieldNames = ops.collect { case ContractOp.RemoveField(f) => f.name.name }.toSet
          val keptFieldNames    = ops.collect { case ContractOp.KeepField(f) => f.name.name }.toSet

          assert(
            addedFieldNames == Set("added_in_v2"),
            s"Expected AddField(added_in_v2), got addedFields: $addedFieldNames; all ops: $ops",
          )
          assert(
            removedFieldNames == Set("remove_me"),
            s"Expected RemoveField(remove_me), got removedFields: $removedFieldNames; all ops: $ops",
          )
          assert(
            keptFieldNames == Set("keep"),
            s"Expected KeepField(keep), got keptFields: $keptFieldNames; all ops: $ops",
          )

          // `parameterized: T` is param-dependent and must NOT appear in B's ops.
          val allOpFieldNames = addedFieldNames ++ removedFieldNames ++ keptFieldNames
          assert(
            !allOpFieldNames.contains("parameterized"),
            s"parameterized (template-dependent field) must be excluded from B; got ops: $ops",
          )
        }
    }
  }

  // ─── Acceptance (b): prev-import re-synthesizes B ─────────────────────────────

  // v1: same has-carrying template, standalone.
  private val prevV1Body: String =
    """model t41.contract.previmport
      |
      |version "1.0.0"
      |
      |data Widget[T] {
      |  has contract IWidget
      |  label: str
      |  payload: T
      |}
      |
      |root type W = Widget[i32]
      |""".stripMargin

  // v2: imports v1 wholesale; Widget (with `has contract IWidget`) is carried across.
  // The synthesizer must re-synthesize IWidget in v2's scope from the imported template.
  private val prevV2Body: String =
    """model t41.contract.previmport
      |
      |version "2.0.0"
      |
      |import "1.0.0" { * }
      |
      |root type W = Widget[i32]
      |""".stripMargin

  "T41 extracted-contract prev-import (acceptance b)" should {

    "has-carrying template imported via prev-import re-synthesizes B in the importing version" in {
      (manager: BaboonFamilyManager[F]) =>
        for {
          family <- manager.load(
            List(
              makeInput("t41-prev-v1.baboon", prevV1Body),
              makeInput("t41-prev-v2.baboon", prevV2Body),
            )
          )
        } yield {
          val pkg  = Pkg(NEList("t41", "contract", "previmport"))
          val v1   = Version.parse("1.0.0")
          val v2   = Version.parse("2.0.0")

          val lineage  = family.domains.toMap(pkg)
          val v1Domain = lineage.versions(v1)
          val v2Domain = lineage.versions(v2)

          // IWidget must be synthesized in v1 (direct synthesis).
          val iWidgetInV1 = v1Domain.defs.meta.nodes.keys.collect { case u: TypeId.User if u.name.name == "IWidget" => u }.toList
          assert(
            iWidgetInV1.size == 1,
            s"Expected IWidget synthesized in v1 domain; got: ${v1Domain.defs.meta.nodes.keys.map { case u: TypeId.User => u.name.name; case o => o.toString }.toSet}",
          )

          // IWidget must also be synthesized in v2 (re-synthesis from imported template).
          val iWidgetInV2 = v2Domain.defs.meta.nodes.keys.collect { case u: TypeId.User if u.name.name == "IWidget" => u }.toList
          assert(
            iWidgetInV2.size == 1,
            s"Expected IWidget re-synthesized in v2 domain (via prev-import template); got: ${v2Domain.defs.meta.nodes.keys.map { case u: TypeId.User => u.name.name; case o => o.toString }.toSet}",
          )

          // IWidget must have `label: str` in both versions (it is param-free).
          val step = EvolutionStep(v1, v2)
          val diff = lineage.evolution.diffs(step)

          // Since Widget and IWidget have identical content in both versions, IWidget should be
          // either unmodified (best case) or appear in the diff but with only KeepField ops.
          val iWidgetId = iWidgetInV1.head
          diff.diffs.get(iWidgetId) match {
            case None =>
              // Unmodified — strongest signal.
              ()
            case Some(TypedefDiff.ContractDiff(ops)) =>
              val nonKeep = ops.filterNot(_.isInstanceOf[ContractOp.KeepField])
              assert(
                nonKeep.isEmpty,
                s"Expected only KeepField ops for unchanged IWidget across versions; got: $nonKeep",
              )
            case other =>
              fail(s"Expected ContractDiff or absent diff for IWidget, got: $other")
          }

          // The change set: IWidget must be unmodified (it has the same content in v1 and v2).
          val unmodifiedNames = diff.changes.unmodified.collect { case u: TypeId.User => u.name.name }
          assert(
            unmodifiedNames.contains("IWidget"),
            s"Expected IWidget in unmodified set (same content across versions); got unmodified: $unmodifiedNames, " +
            s"changed: ${diff.changes.changed.collect { case u: TypeId.User => u.name.name }}",
          )
        }
    }
  }
}
