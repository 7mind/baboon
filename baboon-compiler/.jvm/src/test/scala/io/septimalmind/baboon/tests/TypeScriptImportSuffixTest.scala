package io.septimalmind.baboon.tests

import distage.plugins.PluginBase
import io.septimalmind.baboon.*
import io.septimalmind.baboon.CompilerTarget.TsTarget
import io.septimalmind.baboon.parser.model.FSPath
import io.septimalmind.baboon.translator.BaboonAbstractTranslator
import izumi.distage.plugins.PluginConfig
import izumi.distage.testkit.model.TestConfig
import izumi.functional.bio.unsafe.UnsafeInstances
import izumi.fundamentals.collections.nonempty.NEString
import izumi.fundamentals.platform.files.IzFiles
import izumi.fundamentals.platform.resources.IzResources

/** With `--ts-import-suffix`, every relative import of every emitted TypeScript file ends in the
  * suffix — the static runtime files included — so the output resolves under Node16/NodeNext
  * (https://github.com/7mind/baboon/issues/96).
  */
final class TypeScriptImportSuffixTest extends BaboonTest[Either] {
  private val Suffix = ".js"

  private val target: TsTarget = TsTarget(
    id = "TypeScript",
    output = OutputOptions(
      safeToRemoveExtensions = Set.empty,
      runtime                = RuntimeGenOpt.With,
      generateConversions    = true,
      output                 = FSPath.parse(NEString.unsafeFrom("./target/baboon-ts-import-suffix-test/")),
      fixturesOutput         = Some(FSPath.parse(NEString.unsafeFrom("./target/baboon-ts-import-suffix-test-fixtures/"))),
      testsOutput            = Some(FSPath.parse(NEString.unsafeFrom("./target/baboon-ts-import-suffix-test-tests/"))),
    ),
    generic = GenericOptions(codecTestIterations = 1),
    language = TsOptions(
      writeEvolutionDict          = false,
      wrappedAdtBranchCodecs      = false,
      importSuffix                = Suffix,
      generateJsonCodecs          = true,
      generateUebaCodecs          = true,
      generateJsonCodecsByDefault = true,
      generateUebaCodecsByDefault = true,
      serviceResult               = ServiceResultConfig.typescriptDefault,
      serviceContext              = ServiceContextConfig.default,
      pragmas                     = Map.empty,
      generateDomainFacade        = true,
      asyncServices               = false,
      bareServiceSymbols          = false,
      mapsAsRecords               = false,
      timestampsUtcMode           = "wrapper",
      timestampsOffsetMode        = "wrapper",
      enumLowercaseValues         = false,
      generateMcpServer           = true,
    ),
  )

  override protected def config: TestConfig = super.config.copy(
    pluginConfig = PluginConfig.const(
      (new BaboonModuleJvm[Either](
        CompilerOptions(
          debug                    = false,
          individualInputs         = Set.empty,
          directoryInputs          = Set(FSPath.parse(NEString.unsafeFrom("./baboon-compiler/src/test/resources/mcp-stub-ok"))),
          metaWriteEvolutionJsonTo = None,
          lockfile                 = None,
          emitOnly                 = None,
          targets                  = Seq(target),
        ),
        UnsafeInstances.Lawless_ParallelErrorAccumulatingOpsEither,
      ) overriddenBy new BaboonJvmTsModule[Either](target)).morph[PluginBase]
    ),
    activation = super.config.activation + BaboonModeAxis.Compiler,
  )

  /** Module specifiers of static imports/re-exports (`from "..."`) and dynamic or bare imports (`import("...")`, `import "..."`). */
  private val Specifier = """(?:\bfrom|\bimport)\s*\(?\s*["']([^"']+)["']""".r

  "TypeScript emission with an import suffix" should {
    "suffix every relative import, the runtime files' included" in {
      (loader: BaboonLoader[Either], translator: BaboonAbstractTranslator[Either]) =>
        val root  = IzResources.getPath("mcp-stub-ok").get.asInstanceOf[IzResources.LoadablePathReference].path
        val files = IzFiles.walk(root.toFile).toList.filter(p => p.toFile.isFile && p.toFile.getName.endsWith(".baboon"))
        for {
          family <- loader.load(files)
          srcs   <- translator.translate(family)
        } yield {
          val emitted = srcs.files.toList.filter(_._1.endsWith(".ts"))
          for (runtime <- List(
              "BaboonSharedRuntime.ts",
              "BaboonCodecsFacade.ts",
              "BaboonAnyOpaque.ts",
              "baboon-identifier-repr.ts",
              "BaboonSharedFixture.ts",
              "BaboonMcpRuntime.ts",
            )) {
            assert(emitted.exists(_._1.endsWith(runtime)), s"$runtime was not emitted: ${emitted.map(_._1)}")
          }
          val unsuffixed = for {
            (path, file) <- emitted
            specifier    <- Specifier.findAllMatchIn(file.content).map(_.group(1)).toList
            if (specifier.startsWith("./") || specifier.startsWith("../")) && !specifier.endsWith(Suffix)
          } yield s"$path: $specifier"
          assert(unsuffixed.isEmpty, unsuffixed.mkString("\n"))
        }
    }
  }
}
