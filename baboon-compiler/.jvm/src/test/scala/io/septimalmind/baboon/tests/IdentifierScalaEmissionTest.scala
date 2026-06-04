package io.septimalmind.baboon.tests

import distage.plugins.PluginBase
import io.septimalmind.baboon.*
import io.septimalmind.baboon.CompilerTarget.ScTarget
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

/** PR-56 D03: end-to-end emission test. Runs the actual `ScDefnTranslator`
  * pipeline against the `identifier-ok` fixture and asserts the emitted
  * Scala source for the `id`-typed Dtos matches spec §6 canonical patterns.
  *
  * Closes the gap between the helper-level property test (which exercises
  * a hand-coded mirror of what the emitter is supposed to produce) and the
  * full Scala-stub gen pipeline (which compiles emitted code but doesn't
  * assert specific spec patterns). This test asserts the contract directly
  * on the emitter output without requiring stub-project compilation.
  */
final class IdentifierScalaEmissionTest extends IdentifierScalaEmissionTestBase[Either]

abstract class IdentifierScalaEmissionTestBase[F[+_, +_]: Error2: TagKK: BaboonTestModule] extends BaboonTest[F] {

  // Override the parent's pluginConfig to inject a Scala target instead of C#,
  // so the DI graph wires up `ScDefnTranslator` and its dependencies.
  private val scTarget: ScTarget = ScTarget(
    id = "Scala",
    output = OutputOptions(
      safeToRemoveExtensions = Set.empty,
      runtime                = RuntimeGenOpt.With,
      generateConversions    = true,
      output                 = FSPath.parse(NEString.unsafeFrom("./target/baboon-scalatests-sc/")),
      fixturesOutput         = None,
      testsOutput            = None,
    ),
    generic = GenericOptions(
      codecTestIterations = 0
    ),
    language = ScOptions(
      writeEvolutionDict          = false,
      wrappedAdtBranchCodecs      = false,
      enableDeprecatedEncoders    = false,
      generateJsonCodecs          = true,
      generateUebaCodecs          = true,
      generateJsonCodecsByDefault = true,
      generateUebaCodecsByDefault = true,
      serviceResult               = ServiceResultConfig.scalaDefault,
      serviceContext              = ServiceContextConfig.default,
      pragmas                     = Map.empty,
      generateDomainFacade        = false,
      generateMcpServer           = false,
    ),
  )

  // Combine the JVM compiler module (loader, logger, etc.) with the Scala-target
  // translator wiring (BaboonJvmScModule) so the Scala translator is reachable
  // by tests via DI. Use the distage Module `++` operator (overrides duplicates
  // from the right side) so transitive duplicate `BaboonSharedModule` includes
  // do not cause conflict-resolution failures.
  private val baseModule: distage.Module =
    new BaboonModuleJvm[Either](
      CompilerOptions(
        debug                    = false,
        individualInputs         = Set.empty,
        directoryInputs          = Set(FSPath.parse(NEString.unsafeFrom("./baboon-compiler/src/test/resources/baboon"))),
        metaWriteEvolutionJsonTo = None,
        lockFile                 = Some(FSPath.parse(NEString.unsafeFrom("./target/baboon.lock"))),
        emitOnly                 = None,
        targets                  = Seq(scTarget),
      ),
      UnsafeInstances.Lawless_ParallelErrorAccumulatingOpsEither,
    )

  private val translatorModule: distage.Module = new BaboonJvmScModule[Either](scTarget)

  private val combinedModule: distage.Module = baseModule overriddenBy translatorModule

  override protected def config: TestConfig = super.config.copy(
    pluginConfig = PluginConfig.const(combinedModule.morph[PluginBase]),
    activation   = super.config.activation + BaboonModeAxis.Compiler,
  )

  private def loadIdentifierFamily(loader: BaboonLoader[F]): F[NEList[BaboonIssue], BaboonFamily] = {
    val root = IzResources
      .getPath("baboon/identifier-ok/identifiers.baboon")
      .getOrElse(throw new AssertionError("identifier-ok fixture not found"))
      .asInstanceOf[IzResources.LoadablePathReference]
      .path
    val baboons =
      if (root.toFile.isDirectory)
        IzFiles.walk(root.toFile).toList.filter(p => p.toFile.isFile && p.toFile.getName.endsWith(".baboon"))
      else List(root)
    loader.load(baboons)
  }

  "identifier-ok fixture, Scala target" should {

    "emit toString and parseRepr that match spec §6 canonical patterns" in {
      (loader: BaboonLoader[F], translator: BaboonAbstractTranslator[F]) =>
        for {
          family <- loadIdentifierFamily(loader)
          srcs   <- translator.translate(family)
        } yield {
          val all = srcs.files.iterator.map { case (path, of) => (path, of.content) }.toList

          // Locate the rendered file containing PointId. The Scala emitter places
          // the type in a path determined by the Pkg; we don't pin the path
          // structure here, only that the type name appears once.
          val pointIdRendering = all.collectFirst {
            case (_, content) if content.contains("class PointId") || content.contains("case class PointId") => content
          }.getOrElse(fail(s"PointId not found in any emitted Scala file. Paths: ${all.map(_._1)}"))

          // Spec §2.1 / §6.9: header is `<simpleName>:<version>#`; field-pair
          // joiner is `:`. The emitted toString builds the string from the
          // fixed header literal plus per-field exprs.
          assert(
            pointIdRendering.contains("\"PointId:1.0.0#\""),
            s"Expected canonical header literal 'PointId:1.0.0#' in emitted toString. Source:\n$pointIdRendering",
          )

          // Spec §3 row str: emitter MUST escape via the runtime helper.
          assert(
            pointIdRendering.contains("escapeStr"),
            s"Expected escapeStr helper call for str field. Source:\n$pointIdRendering",
          )

          // Spec §5: parser entry point exists at PointIdCodec.parseRepr.
          assert(
            pointIdRendering.contains("object PointIdCodec"),
            s"Expected emitted PointIdCodec object. Source:\n$pointIdRendering",
          )
          assert(
            pointIdRendering.contains("def parseRepr"),
            s"Expected emitted parseRepr method. Source:\n$pointIdRendering",
          )

          // Spec §3 / §5.4 fixed-width tsu/tso consumption: parser uses readFixed.
          val mixedRendering = all.collectFirst {
            case (_, content) if content.contains("class Mixed") || content.contains("case class Mixed") => content
          }.getOrElse(fail(s"Mixed not found in any emitted Scala file. Paths: ${all.map(_._1)}"))

          assert(
            mixedRendering.contains("readFixed(24)"),
            s"Expected fixed-width 24-char consumption for tsu (spec §5.4). Source:\n$mixedRendering",
          )
          assert(
            mixedRendering.contains("readFixed(29)"),
            s"Expected fixed-width 29-char consumption for tso (spec §5.4). Source:\n$mixedRendering",
          )
          assert(
            mixedRendering.contains("tsuToString"),
            s"Expected tsuToString helper call. Source:\n$mixedRendering",
          )
          assert(
            mixedRendering.contains("tsoToString"),
            s"Expected tsoToString helper call. Source:\n$mixedRendering",
          )

          // PR-56 D04 fix: range-checked narrowing (out-of-range is a parse error).
          assert(
            pointIdRendering.contains("i32 out of range"),
            s"Expected i32 range check on parser path (D04 fix). Source:\n$pointIdRendering",
          )
          val uintsRendering = all.collectFirst {
            case (_, content) if content.contains("class UInts") || content.contains("case class UInts") => content
          }.getOrElse(fail(s"UInts not found. Paths: ${all.map(_._1)}"))
          assert(
            uintsRendering.contains("u08 out of range"),
            s"Expected u08 range check on parser path (D04 fix). Source:\n$uintsRendering",
          )

          // PR-56 D05 fix: uid lowercase regex enforcement.
          assert(
            mixedRendering.contains("uid not in canonical lowercase form"),
            s"Expected uid lowercase enforcement on parser path (D05 fix). Source:\n$mixedRendering",
          )
        }
    }
  }
}
