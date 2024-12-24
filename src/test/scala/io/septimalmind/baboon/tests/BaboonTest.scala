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
          csOptions = CSOptions(
            generic = GenericOptions(
              obsoleteErrors           = false,
              runtime                  = RuntimeGenOpt.With,
              generateConversions      = true,
              codecTestIterations      = 500,
              metaWriteEvolutionJsonTo = None,
            ),
            csWriteEvolutionDict                      = true,
            csUseCompactAdtForm                       = true,
            csWrappedAdtBranchCodecs                  = true,
            csDisregardImplicitUsings                 = true,
            omitMostRecentVersionSuffixFromPaths      = true,
            omitMostRecentVersionSuffixFromNamespaces = true,
          ),
        ),
        Seq(Paths.get("./src/test/resources/baboon")),
        None,
      ).morph[PluginBase]
    )
  )
}
