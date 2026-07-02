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

/** D10 regression: under `enumLowercaseValues = true`, the enum member is DECLARED with the
  * (escaped) raw member name (`cafe`), but the `_values` array previously referenced it via the
  * PascalCase wire name (`T_NsPascal.Cafe`) — a reference to an undeclared member. The `_values`
  * array MUST reference the SAME identifier the member is declared with. Exercised against the
  * `pkg0` fixture's `enum T_NsPascal { cafe; bar_pub }` (lowercase-leading members).
  */
final class EnumLowercaseValuesTypeScriptEmissionTest extends EnumLowercaseValuesTypeScriptEmissionTestBase[Either]

abstract class EnumLowercaseValuesTypeScriptEmissionTestBase[F[+_, +_]: Error2: TagKK: BaboonTestModule] extends BaboonTest[F] {

  private val tsTarget: TsTarget = TsTarget(
    id = "TypeScript",
    output = OutputOptions(
      safeToRemoveExtensions = Set.empty,
      runtime                = RuntimeGenOpt.With,
      generateConversions    = true,
      output                 = FSPath.parse(NEString.unsafeFrom("./target/baboon-scalatests-ts-lc/")),
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
      asyncServices               = false,
      bareServiceSymbols          = false,
      mapsAsRecords               = false,
      timestampsUtcMode           = "wrapper",
      timestampsOffsetMode        = "wrapper",
      enumLowercaseValues         = true,
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

  private def loadPkg0Family(loader: BaboonLoader[F]): F[NEList[BaboonIssue], BaboonFamily] = {
    val root = IzResources
      .getPath("baboon/pkg0")
      .getOrElse(throw new AssertionError("pkg0 fixture not found"))
      .asInstanceOf[IzResources.LoadablePathReference]
      .path
    val baboons = IzFiles.walk(root.toFile).toList.filter(p => p.toFile.isFile && p.toFile.getName.endsWith(".baboon"))
    loader.load(baboons)
  }

  "pkg0 fixture, TypeScript target, enumLowercaseValues=true" should {

    "reference _values members via the declared (lowercase) identifier, not the PascalCase wire name (D10)" in {
      (loader: BaboonLoader[F], translator: BaboonAbstractTranslator[F]) =>
        for {
          family <- loadPkg0Family(loader)
          srcs   <- translator.translate(family)
        } yield {
          val all = srcs.files.iterator.map { case (path, of) => (path, of.content) }.toList

          val enumRendering = all.collectFirst {
            case (path, content) if path.endsWith("T_NsPascal.ts") => content
          }.getOrElse(fail(s"T_NsPascal.ts not found. Paths: ${all.map(_._1)}"))

          // Declaration is lowercase under the flag.
          assert(
            enumRendering.contains("cafe = \"cafe\"") && enumRendering.contains("bar_pub = \"bar_pub\""),
            s"Expected lowercase member declarations. Source:\n$enumRendering",
          )

          // The _values array must reference the SAME (declared) identifiers — D10: it previously
          // referenced the PascalCase wire name (T_NsPascal.Cafe), an undeclared member.
          assert(
            enumRendering.contains("T_NsPascal.cafe") && enumRendering.contains("T_NsPascal.bar_pub"),
            s"Expected _values to reference declared lowercase idents. Source:\n$enumRendering",
          )
          assert(
            !enumRendering.contains("T_NsPascal.Cafe") && !enumRendering.contains("T_NsPascal.Bar_pub"),
            s"Must NOT reference undeclared PascalCase members in _values (D10). Source:\n$enumRendering",
          )
        }
    }
  }
}
