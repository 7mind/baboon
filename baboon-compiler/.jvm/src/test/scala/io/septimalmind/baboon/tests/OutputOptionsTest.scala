package io.septimalmind.baboon.tests

import io.septimalmind.baboon.parser.model.FSPath
import io.septimalmind.baboon.{CompilerProduct, OutputOptions, RuntimeGenOpt}
import izumi.fundamentals.collections.nonempty.NEString
import org.scalatest.wordspec.AnyWordSpec

/** `--fixture-output` is usable without `--test-output`: the fixtures still need their runtime,
  * and the output that was not requested must not decide where the others are rooted.
  */
final class OutputOptionsTest extends AnyWordSpec {
  private def path(s: String): FSPath = FSPath.parse(NEString.unsafeFrom(s))

  private def options(fixtures: Option[String], tests: Option[String]): OutputOptions = OutputOptions(
    safeToRemoveExtensions = Set.empty,
    runtime                = RuntimeGenOpt.With,
    generateConversions    = true,
    output                 = path("out/src"),
    fixturesOutput         = fixtures.map(path),
    testsOutput            = tests.map(path),
  )

  "output options" should {
    "emit the fixture runtime whenever fixtures are emitted" in {
      assert(options(Some("out/fix"), None).products.contains(CompilerProduct.FixtureRuntime))
      assert(options(Some("out/fix"), Some("out/tests")).products.contains(CompilerProduct.FixtureRuntime))
      assert(!options(None, None).products.contains(CompilerProduct.FixtureRuntime))
    }

    "root every requested output at their common directory" in {
      assert(options(Some("out/fix"), Some("out/tests")).packageBases == OutputOptions.PackageBases(List("src"), List("fix"), List("tests")))
      assert(options(Some("out/fix"), None).packageBases == OutputOptions.PackageBases(List("src"), List("fix"), Nil))
      assert(options(Some("out/a/fix"), Some("out/a/tests")).packageBases == OutputOptions.PackageBases(List("src"), List("a", "fix"), List("a", "tests")))
    }

    "treat a lone output directory as a package of its own name" in {
      assert(options(None, None).packageBases == OutputOptions.PackageBases(List("src"), Nil, Nil))
    }
  }
}
