package io.septimalmind.baboon.tests

import distage.plugins.PluginBase
import io.septimalmind.baboon.*
import io.septimalmind.baboon.CompilerTarget.TsTarget
import io.septimalmind.baboon.parser.model.FSPath
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.tests.BaboonTest.BaboonTestModule
import io.septimalmind.baboon.translator.BaboonAbstractTranslator
import io.septimalmind.baboon.typer.model.BaboonFamily
import izumi.distage.plugins.PluginConfig
import izumi.distage.testkit.model.TestConfig
import izumi.functional.bio.Error2
import izumi.functional.bio.unsafe.UnsafeInstances
import izumi.fundamentals.collections.nonempty.{NEList, NEString}
import izumi.fundamentals.platform.files.IzFiles
import izumi.fundamentals.platform.resources.IzResources
import izumi.reflect.TagKK

/** Regression test for the TS translator: every emitted import path segment
  * derived from the source `model X.Y.Z` declaration MUST be lowercased,
  * matching the on-disk lowercasing applied by `TsFileTools.basename`. If the
  * two disagree, `tsc`/`deno` reject the program on case-sensitive filesystems
  * with "differs from already included file name ... only in casing".
  */
final class TypeScriptImportCaseTest extends TypeScriptImportCaseTestBase[Either]

abstract class TypeScriptImportCaseTestBase[F[+_, +_]: Error2: TagKK: BaboonTestModule] extends BaboonTest[F] {

  private val tsTarget: TsTarget = TsTarget(
    id = "TypeScript",
    output = OutputOptions(
      safeToRemoveExtensions = Set.empty,
      runtime                = RuntimeGenOpt.With,
      generateConversions    = true,
      output                 = FSPath.parse(NEString.unsafeFrom("./target/baboon-scalatests-ts-case/")),
      fixturesOutput         = None,
      testsOutput            = None,
    ),
    generic = GenericOptions(
      codecTestIterations = 0
    ),
    language = TsOptions(
      writeEvolutionDict          = false,
      wrappedAdtBranchCodecs      = false,
      importSuffix                = "",
      generateJsonCodecs          = true,
      generateUebaCodecs          = true,
      generateJsonCodecsByDefault = true,
      generateUebaCodecsByDefault = true,
      serviceResult               = ServiceResultConfig.typescriptDefault,
      serviceContext              = ServiceContextConfig.default,
      pragmas                     = Map.empty,
      generateDomainFacade        = false,
      asyncServices               = true,
      mapsAsRecords               = false,
      timestampsUtcMode           = "wrapper",
      timestampsOffsetMode        = "wrapper",
      enumLowercaseValues         = false,
    ),
  )

  private val baseModule: distage.Module =
    new BaboonModuleJvm[Either](
      CompilerOptions(
        debug                    = false,
        individualInputs         = Set.empty,
        directoryInputs          = Set(FSPath.parse(NEString.unsafeFrom("./baboon-compiler/src/test/resources/baboon"))),
        metaWriteEvolutionJsonTo = None,
        lockFile                 = Some(FSPath.parse(NEString.unsafeFrom("./target/baboon.lock"))),
        emitOnly                 = None,
        targets                  = Seq(tsTarget),
      ),
      UnsafeInstances.Lawless_ParallelErrorAccumulatingOpsEither,
    )

  private val translatorModule: distage.Module = new BaboonJvmTsModule[Either](tsTarget)
  private val combinedModule: distage.Module   = baseModule overriddenBy translatorModule

  override protected def config: TestConfig = super.config.copy(
    pluginConfig = PluginConfig.const(combinedModule.morph[PluginBase]),
    activation   = super.config.activation + BaboonModeAxis.Compiler,
  )

  private def loadCaseFamily(loader: BaboonLoader[F]): F[NEList[BaboonIssue], BaboonFamily] = {
    val root = IzResources
      .getPath("baboon/case-ok")
      .getOrElse(throw new AssertionError("case-ok fixture not found"))
      .asInstanceOf[IzResources.LoadablePathReference]
      .path
    val baboons =
      if (root.toFile.isDirectory)
        IzFiles.walk(root.toFile).toList.filter(p => p.toFile.isFile && p.toFile.getName.endsWith(".baboon"))
      else List(root)
    loader.load(baboons)
  }

  // Captures the path part of an emitted ES-module import: `import ... from 'PATH'`
  // (single or double quoted). Used to assert that no emitted path segment
  // derived from the source `model` declaration leaks original (mixed) case.
  private val ImportPathRegex = """(?:from|import)\s*[\(]?\s*['\"]([^'\"]+)['\"]""".r

  "case-ok fixture, TypeScript target" should {

    "emit only lowercase package-path segments in all import paths" in {
      (loader: BaboonLoader[F], translator: BaboonAbstractTranslator[F]) =>
        for {
          family <- loadCaseFamily(loader)
          srcs   <- translator.translate(family)
        } yield {
          val tsFiles = srcs.files.iterator
            .collect { case (path, of) if path.endsWith(".ts") => (path, of.content) }
            .toList

          assert(tsFiles.nonEmpty, "no TS files emitted")

          // The package was declared `model MixedCase.Pkg`. Any import that
          // uses `MixedCase` or `Pkg` as a *directory segment* (rather than the
          // lowercased `mixedcase`/`pkg` used for the on-disk layout) is the
          // bug: file emission and import emission disagree. Filename basenames
          // (e.g. `DomainMixedCasePkgFacade`) are intentionally CamelCase and
          // are not checked.
          val PkgSegments: Set[String] = Set("MixedCase", "Pkg")
          val offenders = tsFiles.flatMap {
            case (path, content) =>
              ImportPathRegex
                .findAllMatchIn(content)
                .map(_.group(1))
                .filter { p =>
                  val segs = p.split('/').toList
                  // drop basename — only directory segments matter
                  val dirSegs = if (segs.nonEmpty) segs.init else Nil
                  dirSegs.exists(PkgSegments.contains)
                }
                .map(p => s"$path: import '$p'")
          }

          assert(
            offenders.isEmpty,
            s"Imports with non-lowercased package-path segments (file vs import case mismatch):\n${offenders.mkString("\n")}",
          )

          // Sanity: at least one emitted file lives under the lowercased path.
          assert(
            tsFiles.exists { case (p, _) => p.contains("mixedcase/pkg/") },
            s"Expected at least one file under mixedcase/pkg/. Paths: ${tsFiles.map(_._1)}",
          )
        }
    }
  }
}
