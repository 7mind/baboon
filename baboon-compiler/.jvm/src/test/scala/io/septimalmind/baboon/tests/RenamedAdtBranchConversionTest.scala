package io.septimalmind.baboon.tests

import io.septimalmind.baboon.parser.BaboonParser
import io.septimalmind.baboon.parser.model.FSPath
import io.septimalmind.baboon.tests.BaboonTest.BaboonTestModule
import io.septimalmind.baboon.typer.BaboonFamilyManager
import io.septimalmind.baboon.typer.model.{AdtOp, Conversion, EvolutionStep, Owner, Pkg, TypedefDiff, Version}
import io.septimalmind.baboon.typer.model.Conversion.FieldOp
import izumi.functional.bio.Error2
import izumi.fundamentals.collections.nonempty.{NEList, NEString}
import izumi.reflect.TagKK

final class RenamedAdtBranchConversionTest extends RenamedAdtBranchConversionTestBase[Either]

abstract class RenamedAdtBranchConversionTestBase[F[+_, +_]: Error2: TagKK: BaboonTestModule] extends BaboonTest[F] {
  private val before =
    """model review.renamed.branches
      |version "1.0.0"
      |root adt OldShape {
      |  data Same { value: str }
      |  data Widen { value: i32 }
      |  data Manual { value: i32 }
      |}
      |root adt Stable {
      |  data Original { value: str }
      |  data Untouched { value: str }
      |}
      |""".stripMargin

  private val after =
    """model review.renamed.branches
      |version "2.0.0"
      |root adt NewShape : was[OldShape] {
      |  data Same { value: str }
      |  data Widen {
      |    value: i64
      |    label: opt[str]
      |  }
      |  data Manual { value: str }
      |}
      |root adt Stable {
      |  data Renamed : was[Original] { value: str }
      |  data Untouched { value: str }
      |}
      |""".stripMargin

  "Renamed ADT branches" should {
    "derive branch conversions under the new owner and retain custom conversion requirements" in {
      (manager: BaboonFamilyManager[F]) =>
        for {
          family <- manager.load(
            List(
              BaboonParser.Input(FSPath.parse(NEString.unsafeFrom("v1.baboon")), before),
              BaboonParser.Input(FSPath.parse(NEString.unsafeFrom("v2.baboon")), after),
            )
          )
        } yield {
          val lineage = family.domains.toMap(Pkg(NEList("review", "renamed", "branches")))
          val step    = EvolutionStep(Version.parse("1.0.0"), Version.parse("2.0.0"))
          val rules   = lineage.evolution.rules(step)
          val parent = rules.conversions.collectFirst {
            case c: Conversion.CopyAdtBranchByName if c.sourceTpe.name.name == "OldShape" => c
          }.getOrElse(fail("Missing renamed ADT conversion"))

          def dto(name: String): Conversion.DtoConversion = rules.conversions.collectFirst {
            case c: Conversion.DtoConversion if c.sourceTpe.name.name == name && c.sourceTpe.owner == Owner.Adt(parent.sourceTpe) => c
          }.getOrElse(fail(s"Missing renamed branch conversion: $name"))

          val same = dto("Same")
          assert(same.targetTpe.owner == Owner.Adt(parent.targetTpe))
          assert(same.ops.forall(_.isInstanceOf[FieldOp.Transfer]))
          val widen = dto("Widen")
          assert(widen.targetTpe.owner == Owner.Adt(parent.targetTpe))
          assert(widen.ops.exists(_.isInstanceOf[FieldOp.ExpandPrecision]))
          assert(widen.ops.exists(_.isInstanceOf[FieldOp.InitializeWithDefault]))
          assert(rules.conversions.exists {
            case c: Conversion.CustomConversionRequired =>
              c.sourceTpe.name.name == "Manual" && c.sourceTpe.owner == Owner.Adt(parent.sourceTpe) && c.targetTpe.owner == Owner.Adt(parent.targetTpe)
            case _ => false
          })
          val explicit = rules.conversions.collectFirst {
            case c: Conversion.CopyAdtBranchByName if c.sourceTpe.name.name == "Stable" => c
          }.getOrElse(fail("Missing explicitly renamed branch owner conversion"))
          assert(explicit.branchMapping("Original").name.name == "Renamed")
          for (adt <- List(parent, explicit)) {
            val diff = lineage.evolution.diffs(step).diffs(adt.sourceTpe).asInstanceOf[TypedefDiff.AdtDiff]
            val kept = diff.ops.collect { case op: AdtOp.KeepBranch => op.id }
            assert(kept.distinct.size == kept.size, s"Duplicate retained branch operations: $kept")
            assert(kept.size == adt.oldDefn.members.size)
          }
        }
    }
  }
}
