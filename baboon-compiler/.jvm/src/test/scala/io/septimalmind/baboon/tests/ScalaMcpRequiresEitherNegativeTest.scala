package io.septimalmind.baboon.tests

import distage.plugins.PluginBase
import io.septimalmind.baboon.*
import io.septimalmind.baboon.CompilerTarget.ScTarget
import io.septimalmind.baboon.parser.model.FSPath
import io.septimalmind.baboon.parser.model.issues.{BaboonIssue, TranslationIssue}
import io.septimalmind.baboon.tests.BaboonTest.BaboonTestModule
import io.septimalmind.baboon.translator.BaboonAbstractTranslator
import io.septimalmind.baboon.typer.model.BaboonFamily
import izumi.distage.plugins.PluginConfig
import izumi.distage.testkit.model.TestConfig
import izumi.functional.bio.{Error2, F}
import izumi.functional.bio.unsafe.UnsafeInstances
import izumi.fundamentals.collections.nonempty.{NEList, NEString}
import izumi.fundamentals.platform.files.IzFiles
import izumi.fundamentals.platform.resources.IzResources
import izumi.reflect.TagKK

/** T69 negative control for the Scala MCP server generator's Either-only
  * serviceResult requirement.
  *
  * The Scala MCP dispatch runtime (`AbstractBaboonMcpServer.handle`) is
  * synchronous and Either-shaped; it is NOT generic over the configurable
  * serviceResult container that [[io.septimalmind.baboon.translator.scl.ScServiceWiringTranslator]]
  * honors. Combining a non-Either (here: an HKT `F[+_, +_]`) serviceResult with
  * `--scala-generate-mcp-server=true` must be rejected up front with a clear,
  * actionable [[TranslationIssue.ScalaMcpRequiresEither]] error rather than
  * emitting code whose `invokeJson` delegate type mismatches the wiring
  * container. This drives the real `translate` path with the HKT serviceResult
  * (mirroring the `test-gen-sc-wiring-hkt` lane config) + MCP flag and asserts
  * that exact issue.
  */
final class ScalaMcpRequiresEitherNegativeTest extends ScalaMcpRequiresEitherNegativeTestBase[Either]

abstract class ScalaMcpRequiresEitherNegativeTestBase[F[+_, +_]: Error2: TagKK: BaboonTestModule] extends BaboonTest[F] {

  private def scTarget: ScTarget = ScTarget(
    id = "Scala",
    output = OutputOptions(
      safeToRemoveExtensions = Set.empty,
      runtime                = RuntimeGenOpt.With,
      generateConversions    = true,
      output                 = FSPath.parse(NEString.unsafeFrom("./target/baboon-scalatests-sc-mcp-hkt/")),
      fixturesOutput         = None,
      testsOutput            = None,
    ),
    generic = GenericOptions(
      codecTestIterations = 0
    ),
    language = ScOptions(
      writeEvolutionDict          = false,
      wrappedAdtBranchCodecs      = false,
      enableDeprecatedEncoders    = false,
      generateJsonCodecs          = true,
      generateUebaCodecs          = true,
      generateJsonCodecsByDefault = true,
      generateUebaCodecsByDefault = true,
      generateDomainFacade        = false,
      // Non-Either HKT serviceResult — mirrors the `test-gen-sc-wiring-hkt` lane.
      serviceResult               = ServiceResultConfig(
        noErrors   = false,
        resultType = Some("custom.MyBi"),
        pattern    = Some("[$error, $success]"),
        hkt        = Some(HktConfig(name = "F", signature = "[+_, +_]")),
      ),
      serviceContext  = ServiceContextConfig.default,
      pragmas         = Map.empty,
      generateMcpServer = true,
    ),
  )

  private def moduleFor(target: ScTarget): distage.Module = {
    val baseModule =
      new BaboonModuleJvm[Either](
        CompilerOptions(
          debug                    = false,
          individualInputs         = Set.empty,
          directoryInputs          = Set(FSPath.parse(NEString.unsafeFrom("./baboon-compiler/src/test/resources/mcp-stub-ok"))),
          metaWriteEvolutionJsonTo = None,
          lockFile                 = Some(FSPath.parse(NEString.unsafeFrom("./target/baboon.lock"))),
          emitOnly                 = None,
          targets                  = Seq(target),
        ),
        UnsafeInstances.Lawless_ParallelErrorAccumulatingOpsEither,
      )
    baseModule overriddenBy new BaboonJvmScModule[Either](target)
  }

  override protected def config: TestConfig = super.config.copy(
    pluginConfig = PluginConfig.const(moduleFor(scTarget).morph[PluginBase]),
    activation   = super.config.activation + BaboonModeAxis.Compiler,
  )

  private def loadStubFamily(loader: BaboonLoader[F]): F[NEList[BaboonIssue], BaboonFamily] = {
    val root = IzResources
      .getPath("mcp-stub-ok")
      .getOrElse(throw new AssertionError("mcp-stub-ok fixture not found"))
      .asInstanceOf[IzResources.LoadablePathReference]
      .path
    val baboons =
      if (root.toFile.isDirectory)
        IzFiles.walk(root.toFile).toList.filter(p => p.toFile.isFile && p.toFile.getName.endsWith(".baboon"))
      else List(root)
    loader.load(baboons)
  }

  "Scala MCP server generator (non-Either serviceResult)" should {

    "reject --scala-generate-mcp-server with an HKT serviceResult with an actionable ScalaMcpRequiresEither error" in {
      (loader: BaboonLoader[F], translator: BaboonAbstractTranslator[F]) =>
        for {
          family <- loadStubFamily(loader)
          // `catchAll` turns the expected failure into a success for assertion purposes.
          outcome <- translator
            .translate(family)
            .map(Right(_): Either[NEList[BaboonIssue], io.septimalmind.baboon.translator.Sources])
            .catchAll(errs => F.pure(Left(errs): Either[NEList[BaboonIssue], io.septimalmind.baboon.translator.Sources]))
        } yield {
          assert(outcome.isLeft, s"expected translate failure for HKT serviceResult + MCP, got success: $outcome")
          val issues = outcome.left.toOption.get.toList
          val guard = issues.collectFirst { case BaboonIssue.Translation(i: TranslationIssue.ScalaMcpRequiresEither) => i }
          assert(
            guard.isDefined,
            s"expected a TranslationIssue.ScalaMcpRequiresEither, got issues: $issues",
          )
          // The actionable error names the offending result container.
          assert(
            guard.get.resultType.contains("F[+_, +_]") || guard.get.resultType.contains("F"),
            s"ScalaMcpRequiresEither must describe the offending HKT result type, got: ${guard.get.resultType}",
          )
        }
    }
  }
}
