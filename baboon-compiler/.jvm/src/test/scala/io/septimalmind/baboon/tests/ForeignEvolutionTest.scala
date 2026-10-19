package io.septimalmind.baboon.tests

import io.septimalmind.baboon.parser.BaboonParser
import io.septimalmind.baboon.parser.model.FSPath
import io.septimalmind.baboon.parser.model.issues.{BaboonIssue, EvolutionIssue}
import io.septimalmind.baboon.tests.BaboonTest.BaboonTestModule
import io.septimalmind.baboon.typer.BaboonFamilyManager
import io.septimalmind.baboon.typer.model.{BaboonFamily, BaboonLang, Conversion, EvolutionStep, ForeignOp, Pkg, TypedefDiff, Version}
import izumi.functional.bio.{Error2, F}
import izumi.fundamentals.collections.nonempty.{NEList, NEString}
import izumi.reflect.TagKK

/** A foreign type may change its `rt` and gain or lose language bindings between versions; a binding
  * changed in place, or a change of kind, is an error rather than a compiler crash.
  */
final class ForeignEvolutionTest extends ForeignEvolutionTestBase[Either]

abstract class ForeignEvolutionTestBase[F[+_, +_]: Error2: TagKK: BaboonTestModule] extends BaboonTest[F] {
  private val step = EvolutionStep(Version.parse("1.0.0"), Version.parse("2.0.0"))

  private def model(version: String, cents: String): String =
    s"""model fevo
       |version "$version"
       |foreign Cents {
       |$cents
       |}
       |root data Order {
       |  price: Cents
       |}
       |""".stripMargin

  private def load(manager: BaboonFamilyManager[F], v1: String, v2: String): F[NEList[BaboonIssue], BaboonFamily] =
    manager.load(List("v1.baboon" -> v1, "v2.baboon" -> v2).map { case (n, b) => BaboonParser.Input(FSPath.parse(NEString.unsafeFrom(n)), b) })

  private val v1Bindings = """  cs = "System.Int64"
                             |  scala = "scala.Long"""".stripMargin

  "Foreign type evolution" should {
    "accept an rt mapping added in a later version, with no conversion to write" in {
      (manager: BaboonFamilyManager[F]) =>
        load(manager, model("1.0.0", v1Bindings), model("2.0.0", s"  rt = i64\n$v1Bindings")).map {
          family =>
            val lineage = family.domains.toMap(Pkg(NEList("fevo")))
            lineage.evolution.diffs(step).diffs.toList.collect { case (id, TypedefDiff.ForeignDiff(ops)) if id.name.name == "Cents" => ops } match {
              case List(List(ForeignOp.ChangeRuntimeMapping(None, Some(to)))) => assert(to.toString == "#i64")
              case other                                                      => fail(other.toString)
            }
            val conversions = lineage.evolution.rules(step).conversions
            assert(conversions.exists {
              case c: Conversion.NonDataTypeTypeNoConversion => c.sourceTpe.name.name == "Cents"
              case _                                         => false
            })
            assert(!conversions.exists(_.isInstanceOf[Conversion.CustomConversionRequired]), conversions.toString)
        }
    }

    "accept a binding added in a later version" in {
      (manager: BaboonFamilyManager[F]) =>
        load(manager, model("1.0.0", v1Bindings), model("2.0.0", s"$v1Bindings\n  rust = \"i64\"")).map {
          family =>
            val ops = family.domains.toMap(Pkg(NEList("fevo"))).evolution.diffs(step).diffs.values.collect { case TypedefDiff.ForeignDiff(ops) => ops }.flatten.toList
            assert(ops.collect { case ForeignOp.AddBinding(entry) => entry.lang } == List(BaboonLang.Rust))
        }
    }

    "reject a binding changed in place" in {
      (manager: BaboonFamilyManager[F]) =>
        F.attempt(load(manager, model("1.0.0", v1Bindings), model("2.0.0", v1Bindings.replace("System.Int64", "System.Decimal")))).map {
          case Left(issues) =>
            val changed = issues.toList.collect { case BaboonIssue.Evolution(i: EvolutionIssue.ForeignBindingChanged) => (i.id.name.name, i.prev.lang, i.step) }
            assert(changed == List(("Cents", BaboonLang.Cs, step)), issues.toString)
          case Right(_) =>
            fail("expected ForeignBindingChanged")
        }
    }

    "reject a type that changes its kind" in {
      (manager: BaboonFamilyManager[F]) =>
        val v1 = "model fevo.kind\nversion \"1.0.0\"\nroot data X { a: i32 }\n"
        val v2 = "model fevo.kind\nversion \"2.0.0\"\nroot enum X { A B }\n"
        F.attempt(load(manager, v1, v2)).map {
          case Left(issues) =>
            assert(
              issues.toList.collect { case BaboonIssue.Evolution(i: EvolutionIssue.TypeKindChanged) => (i.prev.id.name.name, i.step) } == List(("X", step)),
              issues.toString,
            )
          case Right(_) =>
            fail("expected TypeKindChanged")
        }
    }
  }
}
