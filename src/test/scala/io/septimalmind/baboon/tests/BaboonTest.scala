package io.septimalmind.baboon.tests

import distage.plugins.PluginBase
import io.septimalmind.baboon.*
import izumi.distage.plugins.PluginConfig
import izumi.distage.testkit.model.TestConfig
import izumi.distage.testkit.scalatest.Spec1
import izumi.fundamentals.platform.functional.Identity

import java.nio.file.Paths

abstract class BaboonTest extends Spec1[Identity] {
  override protected def config: TestConfig = super.config.copy(
    pluginConfig = PluginConfig.const(
      new BaboonModule(
        CompilerOptions(
          debug = false,
          target = TargetOptions(
            runtime             = RuntimeGenOpt.With,
            generateConversions = true,
            // dummy path (should be unused)
            output         = Paths.get("./src/test/resources/baboon"),
            fixturesOutput = None,
            testsOutput    = None,
          ),
          generic = GenericOptions(
            obsoleteErrors           = false,
            codecTestIterations      = 500,
            metaWriteEvolutionJsonTo = None,
          ),
          csOptions = CSOptions(
            writeEvolutionDict                        = true,
            useCompactAdtForm                         = true,
            wrappedAdtBranchCodecs                    = true,
            disregardImplicitUsings                   = true,
            omitMostRecentVersionSuffixFromPaths      = true,
            omitMostRecentVersionSuffixFromNamespaces = true,
          ),
        ),
        Seq(Paths.get("./src/test/resources/baboon")),
      ).morph[PluginBase]
    )
  )
}
