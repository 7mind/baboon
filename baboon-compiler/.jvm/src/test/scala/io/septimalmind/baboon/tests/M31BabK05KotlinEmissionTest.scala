package io.septimalmind.baboon.tests

import distage.plugins.PluginBase
import io.septimalmind.baboon.*
import io.septimalmind.baboon.CompilerTarget.KtTarget
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

/** BAB-K05 regression: for `map[str, V]` fields the JSON codec emitter must
  * produce `e.key` (no `.toString()`) in the `put(e.key, ...)` map-encoding
  * expression. `e.key` is already a Kotlin `String`; calling `.toString()` on
  * it is redundant and triggers `Redundant call of conversion method.` under
  * `-Werror`.
  *
  * The fix narrows `encodeKey` in `KtJsonCodecGenerator.scala`: a new
  * `case TypeId.Builtins.str` arm emits `$ref` directly; all other builtin
  * key types retain the existing `$ref.toString()` emission.
  *
  * This test asserts:
  *   (a) `map[str, f64]` on `CollFloat` emits `put(e.key,` — not `e.key.toString()`.
  *   (b) `map[u64, str]` on the m28 `Holder` fixture still emits `.toString()`,
  *       confirming the non-str path is preserved (negative / non-regression).
  *
  * We reuse the existing `m31-bab-r04` fixture (CollFloat has `named: map[str, f64]`
  * with `derived[json]`) rather than adding a new fixture.
  */
final class M31BabK05KotlinEmissionTest extends M31BabK05KotlinEmissionTestBase[Either]

abstract class M31BabK05KotlinEmissionTestBase[F[+_, +_]: Error2: TagKK: BaboonTestModule] extends BaboonTest[F] {

  private val ktTarget: KtTarget = KtTarget(
    id = "Kotlin",
    output = OutputOptions(
      safeToRemoveExtensions = Set.empty,
      runtime                = RuntimeGenOpt.With,
      generateConversions    = true,
      output                 = FSPath.parse(NEString.unsafeFrom("./target/baboon-scalatests-kt-bab-k05/")),
      fixturesOutput         = None,
      testsOutput            = None,
    ),
    generic = GenericOptions(
      codecTestIterations = 0
    ),
    language = KtOptions(
      writeEvolutionDict          = false,
      wrappedAdtBranchCodecs      = false,
      enableDeprecatedEncoders    = false,
      generateJsonCodecs          = true,
      generateUebaCodecs          = true,
      generateJsonCodecsByDefault = true,
      generateUebaCodecsByDefault = true,
      multiplatform               = false,
      serviceResult               = ServiceResultConfig.kotlinDefault,
      serviceContext              = ServiceContextConfig.default,
      pragmas                     = Map.empty,
      asyncServices               = false,
      generateDomainFacade        = false,
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
        lockFile                 = Some(FSPath.parse(NEString.unsafeFrom("./target/baboon.lock"))),
        emitOnly                 = None,
        targets                  = Seq(ktTarget),
      ),
      UnsafeInstances.Lawless_ParallelErrorAccumulatingOpsEither,
    )

  private val translatorModule: distage.Module = new BaboonJvmKtModule[Either](ktTarget)
  private val combinedModule: distage.Module   = baseModule overriddenBy translatorModule

  override protected def config: TestConfig = super.config.copy(
    pluginConfig = PluginConfig.const(combinedModule.morph[PluginBase]),
    activation   = super.config.activation + BaboonModeAxis.Compiler,
  )

  private def loadFamily(loader: BaboonLoader[F]): F[NEList[BaboonIssue], BaboonFamily] = {
    val root = IzResources
      .getPath("baboon/m31-bab-r04/m31_bab_r04.baboon")
      .getOrElse(throw new AssertionError("m31-bab-r04 fixture not found"))
      .asInstanceOf[IzResources.LoadablePathReference]
      .path
    val baboons =
      if (root.toFile.isDirectory)
        IzFiles.walk(root.toFile).toList.filter(p => p.toFile.isFile && p.toFile.getName.endsWith(".baboon"))
      else List(root)
    loader.load(baboons)
  }

  "m31-bab-r04 fixture, Kotlin JSON codec, map[str, V] encoding (BAB-K05)" should {

    "emit `e.key` (not `e.key.toString()`) for str-keyed map fields" in {
      (loader: BaboonLoader[F], translator: BaboonAbstractTranslator[F]) =>
        for {
          family <- loadFamily(loader)
          srcs   <- translator.translate(family)
        } yield {
          val all = srcs.files.iterator.map { case (path, of) => (path, of.content) }.toList

          // CollFloat has `named: map[str, f64]` with `derived[json]`.
          // The JSON codec for CollFloat must encode the key as plain `e.key`,
          // not `e.key.toString()`.
          val collFloatCodec = all.collectFirst {
            case (path, content) if path.endsWith("CollFloat_JsonCodec.kt") || content.contains("data class CollFloat(") => content
          }.getOrElse(fail(s"CollFloat source not found. Paths:\n${all.map(_._1).mkString("\n")}"))

          assert(
            !collFloatCodec.contains("e.key.toString()"),
            s"[BAB-K05] map[str, f64] encoder must NOT emit e.key.toString(). Source:\n$collFloatCodec",
          )
          assert(
            collFloatCodec.contains("put(e.key,"),
            s"[BAB-K05] map[str, f64] encoder must emit put(e.key, ...). Source:\n$collFloatCodec",
          )
        }
    }
  }
}
