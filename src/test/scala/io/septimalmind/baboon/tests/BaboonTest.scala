package io.septimalmind.baboon.tests

import distage.plugins.PluginBase
import io.septimalmind.baboon.*
import io.septimalmind.baboon.CompilerTarget.CSTarget
import io.septimalmind.baboon.QuasiIOEither.BaboonTestModule
import io.septimalmind.baboon.util.functional.ParallelAccumulatingOpsInstances
import izumi.distage.plugins.PluginConfig
import izumi.distage.testkit.model.TestConfig
import izumi.distage.testkit.scalatest.Spec2
import izumi.reflect.TagKK

import java.nio.file.Paths

abstract class BaboonTest[F[+_, +_]: TagKK](implicit baboonTestModule: BaboonTestModule[F]) extends Spec2[F]()(baboonTestModule.defaultModule, implicitly[TagKK[F]]) {
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
        ParallelAccumulatingOpsInstances.Lawless_ParallelAccumulatingOpsEither,
      ).morph[PluginBase]
    )
  )
}
