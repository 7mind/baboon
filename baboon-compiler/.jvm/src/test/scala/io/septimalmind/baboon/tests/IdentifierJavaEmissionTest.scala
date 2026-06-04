package io.septimalmind.baboon.tests

import distage.plugins.PluginBase
import io.septimalmind.baboon.*
import io.septimalmind.baboon.CompilerTarget.JvTarget
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

/** PR-57a emission test: runs the actual `JvDefnTranslator` pipeline against
  * the `identifier-ok` fixture and asserts the emitted Java source for the
  * `id`-typed Dtos matches spec §6 canonical patterns.
  */
final class IdentifierJavaEmissionTest extends IdentifierJavaEmissionTestBase[Either]

abstract class IdentifierJavaEmissionTestBase[F[+_, +_]: Error2: TagKK: BaboonTestModule] extends BaboonTest[F] {

  private val jvTarget: JvTarget = JvTarget(
    id = "Java",
    output = OutputOptions(
      safeToRemoveExtensions = Set.empty,
      runtime                = RuntimeGenOpt.With,
      generateConversions    = true,
      output                 = FSPath.parse(NEString.unsafeFrom("./target/baboon-scalatests-jv/")),
      fixturesOutput         = None,
      testsOutput            = None,
    ),
    generic = GenericOptions(
      codecTestIterations = 0
    ),
    language = JvOptions(
      writeEvolutionDict          = false,
      wrappedAdtBranchCodecs      = false,
      enableDeprecatedEncoders    = false,
      generateJsonCodecs          = true,
      generateUebaCodecs          = true,
      generateJsonCodecsByDefault = true,
      generateUebaCodecsByDefault = true,
      serviceResult               = ServiceResultConfig.javaDefault,
      serviceContext              = ServiceContextConfig.default,
      pragmas                     = Map.empty,
      generateDomainFacade        = false,
      asyncServices               = false,
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
        targets                  = Seq(jvTarget),
      ),
      UnsafeInstances.Lawless_ParallelErrorAccumulatingOpsEither,
    )

  private val translatorModule: distage.Module = new BaboonJvmJvModule[Either](jvTarget)

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

  "identifier-ok fixture, Java target" should {

    "emit toString and parseRepr that match spec §6 canonical patterns" in {
      (loader: BaboonLoader[F], translator: BaboonAbstractTranslator[F]) =>
        for {
          family <- loadIdentifierFamily(loader)
          srcs   <- translator.translate(family)
        } yield {
          val all = srcs.files.iterator.map { case (path, of) => (path, of.content) }.toList

          val pointIdRendering = all.collectFirst {
            case (_, content) if content.contains("public record PointId(") => content
          }.getOrElse(fail(s"PointId record not found in any emitted Java file. Paths: ${all.map(_._1)}"))

          // Spec §2.1 / §6.9: header is `<simpleName>:<version>#`.
          assert(
            pointIdRendering.contains("\"PointId:1.0.0#\""),
            s"Expected canonical header literal 'PointId:1.0.0#' in emitted toString. Source:\n$pointIdRendering",
          )

          // Spec §3 row str: emitter MUST escape via the runtime helper.
          assert(
            pointIdRendering.contains("escapeStr"),
            s"Expected escapeStr helper call for str field. Source:\n$pointIdRendering",
          )

          // Spec §5: parser entry point exists at PointIdCodec.parseRepr in a separate file.
          val pointIdCodec = all.collectFirst {
            case (path, content) if path.endsWith("PointIdCodec.java") => content
          }.getOrElse(fail(s"PointIdCodec.java not found. Paths: ${all.map(_._1)}"))
          assert(
            pointIdCodec.contains("public final class PointIdCodec"),
            s"Expected emitted PointIdCodec class. Source:\n$pointIdCodec",
          )
          assert(
            pointIdCodec.contains("public static") && pointIdCodec.contains("parseRepr"),
            s"Expected emitted parseRepr static method. Source:\n$pointIdCodec",
          )

          // Spec §3 / §5.4 fixed-width tsu/tso consumption: parser uses readFixed.
          val mixedRendering = all.collectFirst {
            case (_, content) if content.contains("public record Mixed(") => content
          }.getOrElse(fail(s"Mixed record not found. Paths: ${all.map(_._1)}"))
          val mixedCodec = all.collectFirst {
            case (path, content) if path.endsWith("MixedCodec.java") => content
          }.getOrElse(fail(s"MixedCodec.java not found. Paths: ${all.map(_._1)}"))

          assert(
            mixedCodec.contains("readFixed(24)"),
            s"Expected fixed-width 24-char consumption for tsu (spec §5.4). Source:\n$mixedCodec",
          )
          assert(
            mixedCodec.contains("readFixed(29)"),
            s"Expected fixed-width 29-char consumption for tso (spec §5.4). Source:\n$mixedCodec",
          )
          assert(
            mixedRendering.contains("tsuToString"),
            s"Expected tsuToString helper call. Source:\n$mixedRendering",
          )
          assert(
            mixedRendering.contains("tsoToString"),
            s"Expected tsoToString helper call. Source:\n$mixedRendering",
          )

          // Range-checked narrowing.
          assert(
            pointIdCodec.contains("i32 out of range"),
            s"Expected i32 range check on parser path. Source:\n$pointIdCodec",
          )
          val uintsCodec = all.collectFirst {
            case (path, content) if path.endsWith("UIntsCodec.java") => content
          }.getOrElse(fail(s"UIntsCodec.java not found. Paths: ${all.map(_._1)}"))
          assert(
            uintsCodec.contains("u08 out of range"),
            s"Expected u08 range check on parser path. Source:\n$uintsCodec",
          )

          // Uid lowercase enforcement.
          assert(
            mixedCodec.contains("uid not in canonical lowercase form"),
            s"Expected uid lowercase enforcement on parser path. Source:\n$mixedCodec",
          )

          // Empty-field id (Marker): repr is `Marker:1.0.0#` per spec §6.12.
          val markerRendering = all.collectFirst {
            case (_, content) if content.contains("public record Marker(") || content.contains("public record Marker()") => content
          }.getOrElse(fail(s"Marker record not found. Paths: ${all.map(_._1)}"))
          assert(
            markerRendering.contains("\"Marker:1.0.0#\""),
            s"Expected canonical header literal 'Marker:1.0.0#' for empty-field id. Source:\n$markerRendering",
          )

          // Nested id (Outer.ref → PointId): nested codec dispatch.
          val outerCodec = all.collectFirst {
            case (path, content) if path.endsWith("OuterCodec.java") => content
          }.getOrElse(fail(s"OuterCodec.java not found. Paths: ${all.map(_._1)}"))
          assert(
            outerCodec.contains("PointIdCodec.parseRepr(cursor)"),
            s"Expected nested PointIdCodec.parseRepr dispatch. Source:\n$outerCodec",
          )
          val outerRendering = all.collectFirst {
            case (_, content) if content.contains("public record Outer(") => content
          }.getOrElse(fail(s"Outer record not found. Paths: ${all.map(_._1)}"))
          assert(
            outerRendering.contains("\"{\" + this.ref().toString() + \"}\""),
            s"Expected `{...}` nested-id wrapper in toString. Source:\n$outerRendering",
          )

          // u64 unsigned formatter must be used.
          val uintsRendering = all.collectFirst {
            case (_, content) if content.contains("public record UInts(") => content
          }.getOrElse(fail(s"UInts record not found. Paths: ${all.map(_._1)}"))
          assert(
            uintsRendering.contains("u64ToString"),
            s"Expected u64ToString unsigned helper for u64 field. Source:\n$uintsRendering",
          )
        }
    }
  }
}
