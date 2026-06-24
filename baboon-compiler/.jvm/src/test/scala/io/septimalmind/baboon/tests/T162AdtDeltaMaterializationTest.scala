package io.septimalmind.baboon.tests

import io.septimalmind.baboon.parser.BaboonParser
import io.septimalmind.baboon.parser.model.FSPath
import io.septimalmind.baboon.parser.model.issues.{BaboonIssue, TyperIssue}
import io.septimalmind.baboon.parser.model.issues.TyperIssue.AdtDeltaConflictDetail
import io.septimalmind.baboon.tests.BaboonTest.BaboonTestModule
import io.septimalmind.baboon.typer.BaboonFamilyManager
import io.septimalmind.baboon.typer.model.{Domain, DomainMember, Pkg, Typedef, Version}
import izumi.functional.bio.{Error2, F}
import izumi.fundamentals.collections.nonempty.{NEList, NEString}
import izumi.reflect.TagKK

/** T162 (G27): keep/drop pure-sugar materialization pre-pass + AdtDeltaConflict detection.
  *
  * Two acceptance parts:
  *   (1) MATERIALIZATION (IR-equality): a 2-version model whose v2 uses
  *       `root adt T4_A1 { keep B1, B2; drop B3; data B4 {…} }` (under `import "<v1>" { * }`) produces,
  *       after the pre-pass + typing, an ADT with EXACTLY branches {B1, B2, B4} — structurally
  *       identical (same branch set, same field shapes) to the explicit twin
  *       `root adt T4_A1 { B1; B2; B4 }`. No Keep/Drop node survives into `convertAdt` (the materialized
  *       branch list types cleanly; the guard never fires => no RuntimeException).
  *   (2) CONFLICTS (RED-then-GREEN per case): each conflict model yields the corresponding
  *       `AdtDeltaConflict` detail through the issue channel (NOT a thrown RuntimeException), and a valid
  *       delta still compiles clean.
  */
final class T162AdtDeltaMaterializationTest extends T162AdtDeltaMaterializationTestBase[Either]

abstract class T162AdtDeltaMaterializationTestBase[F[+_, +_]: Error2: TagKK: BaboonTestModule] extends BaboonTest[F] {

  private def makeInput(name: String, body: String): BaboonParser.Input =
    BaboonParser.Input(FSPath.parse(NEString.unsafeFrom(name)), body)

  private val pkg = Pkg(NEList("t162", "delta"))
  private val v1  = Version.parse("1.0.0")
  private val v2  = Version.parse("2.0.0")

  // v1: full ADT with four branches B1..B4, each a distinct field shape.
  private val v1Body =
    """model t162.delta
      |version "1.0.0"
      |root adt T4_A1 {
      |  data B1 { a: i32 }
      |  data B2 { b: str }
      |  data B3 { c: bit }
      |}
      |""".stripMargin

  // v2 delta form: keep B1, B2; drop B3; add B4 — materializes to {B1, B2, B4}.
  private val v2DeltaBody =
    """model t162.delta
      |version "2.0.0"
      |import "1.0.0" { * }
      |root adt T4_A1 {
      |  keep B1, B2
      |  drop B3
      |  data B4 { d: f64 }
      |}
      |""".stripMargin

  // v2 explicit twin: literal whole-ADT re-declaration of the SAME materialized branch set.
  private val v2ExplicitBody =
    """model t162.delta
      |version "2.0.0"
      |import "1.0.0" { * }
      |root adt T4_A1 {
      |  data B1 { a: i32 }
      |  data B2 { b: str }
      |  data B4 { d: f64 }
      |}
      |""".stripMargin

  /** Extract a typed domain's `T4_A1` ADT and its branches as a map: branchName -> sorted field shapes. */
  private def adtBranchShapes(domain: Domain, adtName: String): Map[String, List[(String, String)]] = {
    val nodes = domain.defs.meta.nodes
    val adt = nodes.values.collectFirst {
      case DomainMember.User(_, a: Typedef.Adt, _, _) if a.id.name.name == adtName => a
    }.getOrElse(throw new RuntimeException(s"ADT $adtName not found in domain ${domain.id} ${domain.version}"))

    adt.members.toList.map {
      branchId =>
        val branchName = branchId.name.name
        val fields = nodes.get(branchId) match {
          case Some(DomainMember.User(_, dto: Typedef.Dto, _, _)) =>
            dto.fields.map(f => (f.name.name, f.tpe.toString)).sortBy(_._1)
          case other =>
            throw new RuntimeException(s"branch $branchName not a Dto: $other")
        }
        branchName -> fields
    }.toMap
  }

  private def domainOf(family: io.septimalmind.baboon.typer.model.BaboonFamily, version: Version): Domain =
    family.domains.toMap(pkg).versions.toMap(version)

  /** Collect every `AdtDeltaConflict` detail present in a failed load's issue list. */
  private def conflictDetails(issues: NEList[BaboonIssue]): List[AdtDeltaConflictDetail] =
    issues.toList.collect {
      case BaboonIssue.Typer(c: TyperIssue.AdtDeltaConflict) => c.detail
    }

  "T162 keep/drop materialization" should {

    "materialize `keep B1,B2; drop B3; data B4` to EXACTLY {B1,B2,B4}, IR-identical to the explicit twin" in {
      (manager: BaboonFamilyManager[F]) =>
        for {
          deltaFamily <- manager.load(
            List(makeInput("v1.baboon", v1Body), makeInput("v2.baboon", v2DeltaBody))
          )
          explicitFamily <- manager.load(
            List(makeInput("v1e.baboon", v1Body), makeInput("v2e.baboon", v2ExplicitBody))
          )
        } yield {
          val deltaV2    = adtBranchShapes(domainOf(deltaFamily, v2), "T4_A1")
          val explicitV2 = adtBranchShapes(domainOf(explicitFamily, v2), "T4_A1")

          // Exact branch set {B1, B2, B4}; B3 dropped, no Keep/Drop node leaked.
          assert(
            deltaV2.keySet == Set("B1", "B2", "B4"),
            s"Expected materialized branch set {B1,B2,B4}, got ${deltaV2.keySet}",
          )
          // Structural IR-equality with the explicit twin: identical branch set AND field shapes.
          assert(
            deltaV2 == explicitV2,
            s"Materialized ADT must be IR-identical to explicit twin.\n  delta=$deltaV2\n  explicit=$explicitV2",
          )
          // Kept branches preserve their prior (v1) field shapes verbatim.
          assert(deltaV2("B1") == List(("a", "#i32")), s"B1 shape: ${deltaV2("B1")}")
          assert(deltaV2("B2") == List(("b", "#str")), s"B2 shape: ${deltaV2("B2")}")
          assert(deltaV2("B4") == List(("d", "#f64")), s"B4 shape: ${deltaV2("B4")}")
        }
    }

    // ---- CONFLICT cases: each must surface the matching detail through the issue channel ----

    "raise DropOfAbsent for `drop X` where X is not a prior branch" in {
      (manager: BaboonFamilyManager[F]) =>
        val v2 =
          """model t162.delta
            |version "2.0.0"
            |import "1.0.0" { * }
            |root adt T4_A1 {
            |  keep B1
            |  drop Nonexistent
            |}
            |""".stripMargin
        F.attempt(manager.load(List(makeInput("v1.baboon", v1Body), makeInput("v2.baboon", v2)))).map {
          case Left(issues) =>
            val details = conflictDetails(issues)
            assert(
              details.exists(_.isInstanceOf[AdtDeltaConflictDetail.DropOfAbsent]),
              s"Expected DropOfAbsent; got $details",
            )
          case Right(_) =>
            fail("Expected DropOfAbsent conflict, but the model compiled clean")
        }
    }

    "raise KeepDropSame when a branch is both kept and dropped" in {
      (manager: BaboonFamilyManager[F]) =>
        val v2 =
          """model t162.delta
            |version "2.0.0"
            |import "1.0.0" { * }
            |root adt T4_A1 {
            |  keep B1, B2
            |  drop B2
            |}
            |""".stripMargin
        F.attempt(manager.load(List(makeInput("v1.baboon", v1Body), makeInput("v2.baboon", v2)))).map {
          case Left(issues) =>
            val details = conflictDetails(issues)
            assert(
              details.exists(_.isInstanceOf[AdtDeltaConflictDetail.KeepDropSame]),
              s"Expected KeepDropSame; got $details",
            )
          case Right(_) =>
            fail("Expected KeepDropSame conflict, but the model compiled clean")
        }
    }

    "raise KeepRedefineSame when a selectively-kept branch is also redefined in-body" in {
      (manager: BaboonFamilyManager[F]) =>
        val v2 =
          """model t162.delta
            |version "2.0.0"
            |import "1.0.0" { * }
            |root adt T4_A1 {
            |  keep B1, B2
            |  data B1 { a: i32 }
            |}
            |""".stripMargin
        F.attempt(manager.load(List(makeInput("v1.baboon", v1Body), makeInput("v2.baboon", v2)))).map {
          case Left(issues) =>
            val details = conflictDetails(issues)
            assert(
              details.exists(_.isInstanceOf[AdtDeltaConflictDetail.KeepRedefineSame]),
              s"Expected KeepRedefineSame; got $details",
            )
          case Right(_) =>
            fail("Expected KeepRedefineSame conflict, but the model compiled clean")
        }
    }

    "raise DropRedefineSame when a dropped branch is also redefined in-body" in {
      (manager: BaboonFamilyManager[F]) =>
        val v2 =
          """model t162.delta
            |version "2.0.0"
            |import "1.0.0" { * }
            |root adt T4_A1 {
            |  keep B1
            |  drop B3
            |  data B3 { c: bit }
            |}
            |""".stripMargin
        F.attempt(manager.load(List(makeInput("v1.baboon", v1Body), makeInput("v2.baboon", v2)))).map {
          case Left(issues) =>
            val details = conflictDetails(issues)
            assert(
              details.exists(_.isInstanceOf[AdtDeltaConflictDetail.DropRedefineSame]),
              s"Expected DropRedefineSame; got $details",
            )
          case Right(_) =>
            fail("Expected DropRedefineSame conflict, but the model compiled clean")
        }
    }

    "raise MissingImportHeader for a delta body with no `import` header in scope" in {
      (manager: BaboonFamilyManager[F]) =>
        // A single-version model whose ADT uses keep/drop but imports nothing.
        val only =
          """model t162.delta
            |version "1.0.0"
            |root adt T4_A1 {
            |  keep *
            |  drop B3
            |}
            |""".stripMargin
        F.attempt(manager.load(List(makeInput("only.baboon", only)))).map {
          case Left(issues) =>
            val details = conflictDetails(issues)
            assert(
              details.contains(AdtDeltaConflictDetail.MissingImportHeader),
              s"Expected MissingImportHeader; got $details",
            )
          case Right(_) =>
            fail("Expected MissingImportHeader conflict, but the model compiled clean")
        }
    }

    "raise KeepOfAbsent for selective `keep A` where A is not a prior branch" in {
      (manager: BaboonFamilyManager[F]) =>
        val v2 =
          """model t162.delta
            |version "2.0.0"
            |import "1.0.0" { * }
            |root adt T4_A1 {
            |  keep B1, Nonexistent
            |}
            |""".stripMargin
        F.attempt(manager.load(List(makeInput("v1.baboon", v1Body), makeInput("v2.baboon", v2)))).map {
          case Left(issues) =>
            val details = conflictDetails(issues)
            assert(
              details.exists(_.isInstanceOf[AdtDeltaConflictDetail.KeepOfAbsent]),
              s"Expected KeepOfAbsent; got $details",
            )
          case Right(_) =>
            fail("Expected KeepOfAbsent conflict, but the model compiled clean")
        }
    }

    "compile a valid `keep *` wildcard delta clean (no conflict, all prior branches retained)" in {
      (manager: BaboonFamilyManager[F]) =>
        val v2WildcardBody =
          """model t162.delta
            |version "2.0.0"
            |import "1.0.0" { * }
            |root adt T4_A1 {
            |  keep *
            |  drop B3
            |}
            |""".stripMargin
        for {
          family <- manager.load(List(makeInput("v1.baboon", v1Body), makeInput("v2.baboon", v2WildcardBody)))
        } yield {
          val v2Shapes = adtBranchShapes(domainOf(family, v2), "T4_A1")
          assert(
            v2Shapes.keySet == Set("B1", "B2"),
            s"`keep *; drop B3` must retain {B1,B2}; got ${v2Shapes.keySet}",
          )
        }
    }
  }
}
