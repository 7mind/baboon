package io.septimalmind.baboon.tests

import distage.plugins.PluginBase
import io.septimalmind.baboon.*
import io.septimalmind.baboon.CompilerTarget.CSTarget
import io.septimalmind.baboon.tests.BaboonTest.BaboonTestModule
import izumi.distage.modules.DefaultModule2
import izumi.distage.modules.support.unsafe.EitherSupport
import izumi.distage.plugins.PluginConfig
import izumi.distage.testkit.model.TestConfig
import izumi.distage.testkit.scalatest.Spec2
import izumi.functional.bio.unsafe.UnsafeInstances
import izumi.reflect.TagKK

import java.nio.file.Paths

abstract class BaboonTest[F[+_, +_]: TagKK: BaboonTestModule] extends Spec2[F]()(using BaboonTestModule[F].defaultModule, implicitly[TagKK[F]]) {
  override protected def config: TestConfig = super.config.copy(
    pluginConfig = PluginConfig.const(
      new BaboonModule[Either](
        CompilerOptions(
          debug            = false,
          individualInputs = Set.empty,
          directoryInputs  = Set(Paths.get("./src/test/resources/baboon")),
          targets = Seq(
            CSTarget(
              id = "C#",
              output = OutputOptions(
                safeToRemoveExtensions = Set.empty,
                runtime                = RuntimeGenOpt.With,
                generateConversions    = true,
                // dummy path (should be unused)
                output         = Paths.get("./target/baboon-scalatests/"),
                fixturesOutput = None,
                testsOutput    = None,
              ),
              generic = GenericOptions(
                codecTestIterations      = 500,
                metaWriteEvolutionJsonTo = None,
              ),
              language = CSOptions(
                obsoleteErrors                            = false,
                writeEvolutionDict                        = true,
                useCompactAdtForm                         = true,
                wrappedAdtBranchCodecs                    = true,
                disregardImplicitUsings                   = true,
                omitMostRecentVersionSuffixFromPaths      = true,
                omitMostRecentVersionSuffixFromNamespaces = true,
                enableDeprecatedEncoders                  = false,
              ),
            )
          ),
        ),
        UnsafeInstances.Lawless_ParallelErrorAccumulatingOpsEither,
      ).morph[PluginBase]
    )
  )
}

object BaboonTest {
  // All this really does is, it adds `defaultModuleEither` instance into the implicit scope so that you don't have to import it in every test file
  final class BaboonTestModule[F[+_, +_]]()(implicit val defaultModule: DefaultModule2[F])

  object BaboonTestModule extends BaboonTestModuleLowPriorityInstances {
    @inline def apply[F[+_, +_]](implicit baboonTestModule: BaboonTestModule[F]): BaboonTestModule[F] = baboonTestModule

    implicit val eitherBaboonTestModule: BaboonTestModule[Either] = {
      new BaboonTestModule[Either]()(using EitherSupport.defaultModuleEither)
    }
  }
  sealed trait BaboonTestModuleLowPriorityInstances {
    implicit final def anyOtherDefaultModule[F[+_, +_]: DefaultModule2]: BaboonTestModule[F] = new BaboonTestModule[F]()
  }
}
