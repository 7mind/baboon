package io.septimalmind.baboon.tests

import io.septimalmind.baboon.parser.BaboonParser
import io.septimalmind.baboon.parser.model.FSPath
import io.septimalmind.baboon.tests.BaboonTest.BaboonTestModule
import io.septimalmind.baboon.translator.scl.{ScConversionTranslator, ScTreeTools, ScTypeTranslator}
import io.septimalmind.baboon.typer.BaboonFamilyManager
import io.septimalmind.baboon.typer.model.{BaboonEvolution, BaboonLineage, Conversion, Domain, EvolutionStep, Pkg, Version}
import izumi.functional.bio.Error2
import izumi.fundamentals.collections.nonempty.{NEList, NEString}
import izumi.reflect.TagKK

final class ScEnumConversionTest extends ScEnumConversionTestBase[Either]

// Regression test for BAB-S02-R: CopyEnumByName conversion map keys must be capitalized
// to match the Scala case-object .toString form emitted by ScDefnTranslator.
// Pre-fix: ScConversionTranslator used raw (lower/mixed-case) names as map keys, which
//          never matched from.toString (the case-object name), causing .parse to throw.
// Post-fix: both keys and values are capitalized to match case-object identity.
abstract class ScEnumConversionTestBase[F[+_, +_]: Error2: TagKK: BaboonTestModule] extends BaboonTest[F] {

  private def makeInput(name: String, body: String): BaboonParser.Input =
    BaboonParser.Input(FSPath.parse(NEString.unsafeFrom(name)), body)

  // Two-version in-memory fixture: v1 has non-Pascal enum members; v2 renames some of them.
  // - MyEnum has members 'cafe' and 'bar_pub' renamed to 'coffeeShop' and 'tapRoom'.
  // - Stable is a separate DTO that never changes, so 'missingDiffs' in the evolution
  //   validator is non-empty (preventing the MissingEvoDiff invariant check from firing).
  private val v1Body =
    """model test.enum.rename
      |version "1.0.0"
      |enum MyEnum { Alpha  cafe  bar_pub }
      |root data Holder { e: MyEnum }
      |root data Stable { x: i32 }
      |""".stripMargin

  private val v2Body =
    """model test.enum.rename
      |version "2.0.0"
      |import "1.0.0" { * }
      |enum MyEnum {
      |  Alpha
      |  coffeeShop : was[cafe]
      |  tapRoom : was[bar_pub]
      |}
      |root data Holder { e: MyEnum }
      |root data Stable { x: i32 }
      |""".stripMargin

  "ScConversionTranslator CopyEnumByName" should {

    // Core regression: the generated Map literal in the conversion object must use
    // capitalized keys/values matching the Scala case-object .toString form.
    // Pre-fix: Map("cafe" -> "coffeeShop", ...) — keys never match from.toString
    // Post-fix: Map("Cafe" -> "Coffeeshop", "Bar_pub" -> "Taproom") — keys match
    "emit capitalized keys and values in the enum rename mapping" in {
      (manager: BaboonFamilyManager[F]) =>
        for {
          family <- manager.load(
            List(
              makeInput("v1.baboon", v1Body),
              makeInput("v2.baboon", v2Body),
            )
          )
        } yield {
          val pkg  = Pkg(NEList("test", "enum", "rename"))
          val v1   = Version.parse("1.0.0")
          val v2   = Version.parse("2.0.0")
          val step = EvolutionStep(v1, v2)

          val lineage: BaboonLineage = family.domains.toMap(pkg)
          val evo: BaboonEvolution   = lineage.evolution
          val ruleset                = evo.rules(step)
          val srcDom: Domain         = lineage.versions.toMap(v1)
          val tgtDom: Domain         = lineage.versions.toMap(v2)

          // Extract the CopyEnumByName conversion for MyEnum
          val copyEnumConversion = ruleset.conversions.collectFirst {
            case c: Conversion.CopyEnumByName if c.sourceTpe.name.name == "MyEnum" => c
          }.getOrElse(fail("CopyEnumByName conversion for MyEnum not found in rules"))

          // memberMapping has raw names as produced by BaboonRules.computeEnumMapping
          val rawMapping = copyEnumConversion.memberMapping
          assert(rawMapping.nonEmpty, "memberMapping must not be empty — rename was not detected")
          assert(
            rawMapping == Map("cafe" -> "coffeeShop", "bar_pub" -> "tapRoom"),
            s"Expected raw rename mapping, got: $rawMapping",
          )

          // Simulate what ScConversionTranslator emits.
          // The generated map must use .capitalize on both sides so that
          // from.toString (which returns the case-object name) finds a match.
          val trans       = new ScTypeTranslator()
          val tools       = new ScTreeTools.ScTreeToolsImpl()
          val scPkg       = trans.toScPkg(pkg, v2, evo)
          val translator  = new ScConversionTranslator[Either](trans, scPkg, srcDom, tgtDom, ruleset, tools, evo)
          val convs       = translator.makeConvs.fold(errs => fail(s"makeConvs failed: $errs"), identity)

          // The rendered conversion source should contain capitalized keys matching case-object .toString.
          // Scala's .capitalize only uppercases the first character, leaving the rest unchanged.
          // So: "cafe".capitalize = "Cafe", "bar_pub".capitalize = "Bar_pub",
          //     "coffeeShop".capitalize = "CoffeeShop", "tapRoom".capitalize = "TapRoom".
          val rendered = convs.map(_.conv.toString).mkString("\n")

          // Post-fix: Map("Cafe" -> "CoffeeShop", "Bar_pub" -> "TapRoom")
          assert(
            rendered.contains("\"Cafe\""),
            s"Expected capitalized key 'Cafe' in generated Map, but got:\n$rendered",
          )
          assert(
            rendered.contains("\"Bar_pub\""),
            s"Expected capitalized key 'Bar_pub' in generated Map, but got:\n$rendered",
          )
          assert(
            rendered.contains("\"CoffeeShop\""),
            s"Expected capitalized value 'CoffeeShop' in generated Map, but got:\n$rendered",
          )
          assert(
            rendered.contains("\"TapRoom\""),
            s"Expected capitalized value 'TapRoom' in generated Map, but got:\n$rendered",
          )

          // Negative: the raw (uncapitalized) forms must NOT appear as map keys
          assert(
            !rendered.contains("\"cafe\""),
            s"Raw key 'cafe' must not appear in generated Map — it won't match from.toString:\n$rendered",
          )
          assert(
            !rendered.contains("\"bar_pub\""),
            s"Raw key 'bar_pub' must not appear in generated Map — it won't match from.toString:\n$rendered",
          )
        }
    }
  }
}
