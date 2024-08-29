package io.septimalmind.baboon.tests

import distage.plugins.PluginBase
import io.septimalmind.baboon.{BaboonCompiler, BaboonModule, RuntimeGenOpt}
import izumi.distage.plugins.PluginConfig
import izumi.distage.testkit.model.TestConfig
import izumi.distage.testkit.scalatest.Spec1
import izumi.fundamentals.platform.functional.Identity

abstract class BaboonTest extends Spec1[Identity] {
  override protected def config: TestConfig = super.config.copy(
    pluginConfig = PluginConfig.const(
      new BaboonModule(
        BaboonCompiler.CompilerOptions(
          debug = false,
          obsoleteErrors = false,
          runtime = RuntimeGenOpt.With,
          generateConversions = true,
          disregardImplicitUsings = true,
          omitMostRecentVersionSuffixFromPaths = true,
          omitMostRecentVersionSuffixFromNamespaces = true,
          csUseCompactAdtForm = true,
          csWrappedAdtBranchCodecs = true,
        ),
        Seq.empty,
        None,
      ).morph[PluginBase]
    )
  )
}
