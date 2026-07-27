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

/** T202 / D45 reproduction (fail-first): `TsBaboonTranslator.generateBarrels`
  * selects a directory's re-exported files via `exportedNames(f).nonEmpty`,
  * which only sees exports embedded as structured `TsValue.TsType` values in
  * the file's `TextTree`. The domain facade (`generateDomainFacade`) is
  * emitted as `TextTree.verbatim` raw text — `export class Domain<...>Facade
  * extends BaboonCodecsFacade { ... }` is a plain string, contributing no
  * `TsValue.TsType` values to `output.tree.values` — so `exportedNames`
  * returns an empty map for it and the facade file is silently dropped from
  * its directory's `index.ts` barrel, even though the facade IS emitted on
  * disk and DOES export a class.
  *
  * Uses the `m19-ok` fixture's `direct-wrapper.baboon` (model
  * `my.ok.m19.direct`, single version, `root data Holder`) with
  * `generateDomainFacade = true`. The facade class is
  * `DomainMyOkM19DirectFacade`, emitted at
  * `my/ok/m19/direct/DomainMyOkM19DirectFacade.ts`, in the same directory as
  * `Holder.ts` / `ItemId.ts`. This test locates the generated
  * `my/ok/m19/direct/index.ts` barrel and asserts it re-exports the facade.
  *
  * Pre-fix: FAILS on the facade re-export assertion — the barrel omits any
  * `DomainMyOkM19DirectFacade` re-export line, while still containing the
  * sibling `export * from './Holder'`-style lines (proving the barrel entry
  * itself is produced, not merely absent as a whole).
  */
final class TypescriptDomainFacadeBarrelReexportTest extends TypescriptDomainFacadeBarrelReexportTestBase[Either]

abstract class TypescriptDomainFacadeBarrelReexportTestBase[F[+_, +_]: Error2: TagKK: BaboonTestModule] extends BaboonTest[F] {

  private val tsTarget: TsTarget = TsTarget(
    id = "TypeScript",
    output = OutputOptions(
      safeToRemoveExtensions = Set.empty,
      runtime                = RuntimeGenOpt.With,
      generateConversions    = true,
      output                 = FSPath.parse(NEString.unsafeFrom("./target/baboon-ts-domain-facade-barrel-test/")),
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
      generateDomainFacade        = true,
      asyncServices               = false,
      bareServiceSymbols          = false,
      mapsAsRecords               = false,
      timestampsUtcMode           = "wrapper",
      timestampsOffsetMode        = "wrapper",
      enumLowercaseValues         = false,
      generateMcpServer           = false,
    ),
  )

  private val baseModule: distage.Module =
    new BaboonModuleJvm[Either](
      CompilerOptions(
        debug                    = false,
        individualInputs         = Set.empty,
        directoryInputs          = Set(FSPath.parse(NEString.unsafeFrom("./baboon-compiler/src/test/resources/baboon"))),
        metaWriteEvolutionJsonTo = None,
        lockfile                 = Some(FSPath.parse(NEString.unsafeFrom("./target/baboon.lock"))),
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

  private def loadM19Family(loader: BaboonLoader[F]): F[NEList[BaboonIssue], BaboonFamily] = {
    val root = IzResources
      .getPath("baboon/m19-ok")
      .getOrElse(throw new AssertionError("m19-ok fixture not found"))
      .asInstanceOf[IzResources.LoadablePathReference]
      .path
    val baboons =
      if (root.toFile.isDirectory)
        IzFiles.walk(root.toFile).toList.filter(p => p.toFile.isFile && p.toFile.getName.endsWith(".baboon"))
      else List(root)
    loader.load(baboons)
  }

  "TypeScript domain facade barrel emission" should {

    "re-export the domain facade class from its directory's index.ts barrel (D45)" in {
      (loader: BaboonLoader[F], translator: BaboonAbstractTranslator[F]) =>
        for {
          family <- loadM19Family(loader)
          srcs   <- translator.translate(family)
        } yield {
          val all = srcs.files.iterator.map { case (path, of) => (path.toString, of.content) }.toList

          val facadePath = all.map(_._1).find(_.endsWith("my/ok/m19/direct/DomainMyOkM19DirectFacade.ts"))
          assert(
            facadePath.isDefined,
            s"Expected the domain facade file to be emitted at .../my/ok/m19/direct/DomainMyOkM19DirectFacade.ts. Paths: ${all.map(_._1)}",
          )

          val barrel = all.find { case (path, _) => path.endsWith("my/ok/m19/direct/index.ts") }
          assert(
            barrel.isDefined,
            s"Expected a barrel at .../my/ok/m19/direct/index.ts. Paths: ${all.map(_._1)}",
          )

          val (barrelPath, barrelContent) = barrel.get

          // The barrel IS produced and DOES re-export sibling model types (e.g.
          // `Holder`) — the defect is specific to the facade, not to the barrel
          // being absent entirely.
          assert(
            barrelContent.linesIterator.exists(l => l.contains("Holder")),
            s"Expected barrel at $barrelPath to contain a sibling re-export mentioning 'Holder'. Content:\n$barrelContent",
          )

          val facadeReexported = barrelContent.linesIterator.exists { l =>
            (l.contains("export *") && l.contains("./DomainMyOkM19DirectFacade")) ||
            (l.contains("export {") && l.contains("DomainMyOkM19DirectFacade"))
          }
          assert(
            facadeReexported,
            s"Expected barrel at $barrelPath to re-export DomainMyOkM19DirectFacade (D45), but it did not.\nBarrel content:\n$barrelContent",
          )
        }
    }
  }
}
