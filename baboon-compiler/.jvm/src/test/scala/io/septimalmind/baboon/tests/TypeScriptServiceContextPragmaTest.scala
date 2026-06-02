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

/** Reproduction for the bug report: a model-level
  * `pragma typescript.service.context = "abstract"` must change the emitted
  * service interface (parameterize it `<Ctx>` and prepend `ctx: Ctx` to every
  * method) even though the CLI/target [[ServiceContextConfig]] is left at the
  * default ("none"). The `case-ctx` fixture carries exactly that pragma.
  */
final class TypeScriptServiceContextPragmaTest extends TypeScriptServiceContextPragmaTestBase[Either]

abstract class TypeScriptServiceContextPragmaTestBase[F[+_, +_]: Error2: TagKK: BaboonTestModule] extends BaboonTest[F] {

  private val tsTarget: TsTarget = TsTarget(
    id = "TypeScript",
    output = OutputOptions(
      safeToRemoveExtensions = Set.empty,
      runtime                = RuntimeGenOpt.With,
      generateConversions    = true,
      output                 = FSPath.parse(NEString.unsafeFrom("./target/baboon-scalatests-ts-ctx/")),
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
      serviceContext              = ServiceContextConfig.default, // mode = "none" — the model pragma must override this
      pragmas                     = Map.empty,
      generateDomainFacade        = false,
      asyncServices               = true,
      bareServiceSymbols          = true,
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
      .getPath("baboon-ctx/case-ctx")
      .getOrElse(throw new AssertionError("case-ctx fixture not found"))
      .asInstanceOf[IzResources.LoadablePathReference]
      .path
    val baboons =
      if (root.toFile.isDirectory)
        IzFiles.walk(root.toFile).toList.filter(p => p.toFile.isFile && p.toFile.getName.endsWith(".baboon"))
      else List(root)
    loader.load(baboons)
  }

  "case-ctx fixture, TypeScript target" should {

    "honor model-level `typescript.service.context = abstract` pragma" in {
      (loader: BaboonLoader[F], translator: BaboonAbstractTranslator[F]) =>
        for {
          family <- loadCaseFamily(loader)
          srcs   <- translator.translate(family)
        } yield {
          val all = srcs.files.iterator.collect { case (p, of) if p.endsWith(".ts") => (p, of.content) }.toList
          def existsLine(pred: String => Boolean): Boolean = all.exists { case (_, c) => c.linesIterator.exists(pred) }

          assert(
            existsLine(l => l.trim.startsWith("export interface Service<") && l.contains("Ctx")),
            s"expected `export interface Service<Ctx>` from abstract-context pragma.\n${all.map(_._2).mkString("\n----\n")}",
          )
          assert(
            existsLine(l => l.contains("ctx: Ctx")),
            "expected method signatures to carry `ctx: Ctx` from abstract-context pragma",
          )

          // Regression guards for the abstract-context codegen defects (would
          // all FAIL before the fix): the client class must be generic, the
          // codec-context parameter must not collide with the abstract `ctx`,
          // and the JSON/UEBA wrappers must carry the context-aware contract.
          assert(
            existsLine(l => l.trim.startsWith("export class Client<") && l.contains("Ctx")),
            s"expected generic `export class Client<Ctx>` (client must declare the abstract-context type param).\n${all.map(_._2).mkString("\n----\n")}",
          )
          assert(
            !existsLine(l => l.contains("ctx: Ctx,") && l.contains("ctx: BaboonCodecContext")),
            s"abstract-context param `ctx` must not collide with the codec-context param on one line.\n${all.map(_._2).mkString("\n----\n")}",
          )
          assert(
            existsLine(l => l.contains("codecCtx: BaboonCodecContext")),
            "expected the codec-context parameter to be renamed (`codecCtx`) to avoid the abstract-context clash",
          )
          assert(
            existsLine(l => l.contains("implements IBaboonJsonServiceCtx<")) &&
              existsLine(l => l.contains("implements IBaboonUebaServiceCtx<")),
            s"expected JSON/UEBA wrappers to implement the context-aware `IBaboon*ServiceCtx<Ctx, R>` contract.\n${all.map(_._2).mkString("\n----\n")}",
          )
          assert(
            existsLine(l => l.trim.startsWith("invoke(") && l.contains("ctx: Ctx") && l.contains("codecCtx: BaboonCodecContext")),
            "expected wrapper `invoke` to receive the service context per-invocation (not via constructor)",
          )
        }
    }
  }
}
