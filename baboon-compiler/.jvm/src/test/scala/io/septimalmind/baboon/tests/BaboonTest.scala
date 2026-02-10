package io.septimalmind.baboon.tests

import distage.plugins.PluginBase
import io.septimalmind.baboon.*
import io.septimalmind.baboon.CompilerTarget.CSTarget
import io.septimalmind.baboon.parser.model.FSPath
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.tests.BaboonTest.BaboonTestModule
import io.septimalmind.baboon.typer.model.BaboonFamily
import izumi.distage.modules.DefaultModule2
import izumi.distage.modules.support.unsafe.EitherSupport
import izumi.distage.plugins.PluginConfig
import izumi.distage.testkit.model.TestConfig
import izumi.distage.testkit.scalatest.Spec2
import izumi.functional.bio.unsafe.UnsafeInstances
import izumi.fundamentals.collections.nonempty.{NEList, NEString}
import izumi.fundamentals.platform.files.IzFiles
import izumi.fundamentals.platform.resources.IzResources
import izumi.reflect.TagKK

abstract class BaboonTest[F[+_, +_]: TagKK: BaboonTestModule] extends Spec2[F]()(using BaboonTestModule[F].defaultModule, implicitly[TagKK[F]]) {
  override protected def config: TestConfig = super.config.copy(
    pluginConfig = PluginConfig.const(
      new BaboonModuleJvm[Either](
        CompilerOptions(
          debug                    = false,
          individualInputs         = Set.empty,
          directoryInputs          = Set(FSPath.parse(NEString.unsafeFrom("./baboon-compiler/src/test/resources/baboon"))),
          metaWriteEvolutionJsonTo = None,
          lockFile                 = Some(FSPath.parse(NEString.unsafeFrom("./target/baboon.lock"))),
          targets = Seq(
            CSTarget(
              id = "C#",
              output = OutputOptions(
                safeToRemoveExtensions = Set.empty,
                runtime                = RuntimeGenOpt.With,
                generateConversions    = true,
                // dummy path (should be unused)
                output         = FSPath.parse(NEString.unsafeFrom("./target/baboon-scalatests/")),
                fixturesOutput = None,
                testsOutput    = None,
              ),
              generic = GenericOptions(
                codecTestIterations = 500
              ),
              language = CSOptions(
                obsoleteErrors                            = false,
                writeEvolutionDict                        = true,
                wrappedAdtBranchCodecs                    = true,
                disregardImplicitUsings                   = true,
                omitMostRecentVersionSuffixFromPaths      = true,
                omitMostRecentVersionSuffixFromNamespaces = true,
                enableDeprecatedEncoders                  = false,
                generateIndexWriters                      = true,
                generateJsonCodecs                        = true,
                generateUebaCodecs                        = true,
                generateJsonCodecsByDefault               = true,
                generateUebaCodecsByDefault               = true,
                deduplicate                               = true,
                serviceResult                             = ServiceResultConfig.csDefault,
                serviceContext                            = ServiceContextConfig.default,
                pragmas                                   = Map.empty,
              ),
            )
          ),
        ),
        UnsafeInstances.Lawless_ParallelErrorAccumulatingOpsEither,
      ).morph[PluginBase]
    ),
    activation = super.config.activation + BaboonModeAxis.Compiler
  )

  def loadPkg(loader: BaboonLoader[F]): F[NEList[BaboonIssue], BaboonFamily] = {
    val root = IzResources
      .getPath("baboon/pkg0")
      .get
      .asInstanceOf[IzResources.LoadablePathReference]
      .path
    val baboons = IzFiles
      .walk(root.toFile)
      .toList
      .filter(p => p.toFile.isFile && p.toFile.getName.endsWith(".baboon"))
    loader.load(baboons)
  }
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
