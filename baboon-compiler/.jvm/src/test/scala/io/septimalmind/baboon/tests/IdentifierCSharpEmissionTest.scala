package io.septimalmind.baboon.tests

import distage.plugins.PluginBase
import io.septimalmind.baboon.*
import io.septimalmind.baboon.CompilerTarget.CSTarget
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

/** PR-57a emission test: runs the actual `CSDefnTranslator` pipeline against
  * the `identifier-ok` fixture and asserts the emitted C# source for the
  * `id`-typed Dtos matches spec §6 canonical patterns.
  *
  * Mirrors the Scala emission test (`IdentifierScalaEmissionTest`); the spec
  * (`docs/spec/identifier-repr.md`) is the contract — this test pins the
  * emitter conformance without requiring the C# stub to compile.
  */
final class IdentifierCSharpEmissionTest extends IdentifierCSharpEmissionTestBase[Either]

abstract class IdentifierCSharpEmissionTestBase[F[+_, +_]: Error2: TagKK: BaboonTestModule] extends BaboonTest[F] {

  private val csTarget: CSTarget = CSTarget(
    id = "C#",
    output = OutputOptions(
      safeToRemoveExtensions = Set.empty,
      runtime                = RuntimeGenOpt.With,
      generateConversions    = true,
      output                 = FSPath.parse(NEString.unsafeFrom("./target/baboon-scalatests-cs/")),
      fixturesOutput         = None,
      testsOutput            = None,
    ),
    generic = GenericOptions(
      codecTestIterations = 0
    ),
    language = CSOptions(
      obsoleteErrors                            = false,
      writeEvolutionDict                        = false,
      wrappedAdtBranchCodecs                    = false,
      disregardImplicitUsings                   = true,
      omitMostRecentVersionSuffixFromPaths      = true,
      omitMostRecentVersionSuffixFromNamespaces = true,
      enableDeprecatedEncoders                  = false,
      generateIndexWriters                      = true,
      generateJsonCodecs                        = true,
      generateUebaCodecs                        = true,
      generateJsonCodecsByDefault               = true,
      generateUebaCodecsByDefault               = true,
      deduplicate                               = false,
      serviceResult                             = ServiceResultConfig.csDefault,
      serviceContext                            = ServiceContextConfig.default,
      pragmas                                   = Map.empty,
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
        targets                  = Seq(csTarget),
      ),
      UnsafeInstances.Lawless_ParallelErrorAccumulatingOpsEither,
    )

  private val translatorModule: distage.Module = new BaboonJvmCSModule[Either](csTarget)

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

  "identifier-ok fixture, C# target" should {

    "emit ToString and ParseRepr that match spec §6 canonical patterns" in {
      (loader: BaboonLoader[F], translator: BaboonAbstractTranslator[F]) =>
        for {
          family <- loadIdentifierFamily(loader)
          srcs   <- translator.translate(family)
        } yield {
          val all = srcs.files.iterator.map { case (path, of) => (path, of.content) }.toList

          val pointIdRendering = all.collectFirst {
            case (_, content) if content.contains("public sealed record PointId(") => content
          }.getOrElse(fail(s"PointId not found in any emitted C# file. Paths: ${all.map(_._1)}"))

          // Spec §2.1 / §6.9: header is `<simpleName>:<version>#`.
          assert(
            pointIdRendering.contains("\"PointId:1.0.0#\""),
            s"Expected canonical header literal 'PointId:1.0.0#' in emitted ToString. Source:\n$pointIdRendering",
          )

          // Spec §3 row str: emitter MUST escape via the runtime helper.
          assert(
            pointIdRendering.contains("EscapeStr"),
            s"Expected EscapeStr helper call for str field. Source:\n$pointIdRendering",
          )

          // Spec §5: parser entry point exists at PointIdCodec.ParseRepr.
          assert(
            pointIdRendering.contains("public static class PointIdCodec"),
            s"Expected emitted PointIdCodec class. Source:\n$pointIdRendering",
          )
          assert(
            pointIdRendering.contains("ParseRepr"),
            s"Expected emitted ParseRepr method. Source:\n$pointIdRendering",
          )

          // Spec §3 / §5.4 fixed-width tsu/tso consumption: parser uses ReadFixed.
          val mixedRendering = all.collectFirst {
            case (_, content) if content.contains("public sealed record Mixed(") => content
          }.getOrElse(fail(s"Mixed not found in any emitted C# file. Paths: ${all.map(_._1)}"))

          assert(
            mixedRendering.contains("ReadFixed(24)"),
            s"Expected fixed-width 24-char consumption for tsu (spec §5.4). Source:\n$mixedRendering",
          )
          assert(
            mixedRendering.contains("ReadFixed(29)"),
            s"Expected fixed-width 29-char consumption for tso (spec §5.4). Source:\n$mixedRendering",
          )
          assert(
            mixedRendering.contains("TsuToString"),
            s"Expected TsuToString helper call. Source:\n$mixedRendering",
          )
          assert(
            mixedRendering.contains("TsoToString"),
            s"Expected TsoToString helper call. Source:\n$mixedRendering",
          )

          // Range checks on parser path (out-of-range is a parse error).
          assert(
            pointIdRendering.contains("i32 out of range"),
            s"Expected i32 range check on parser path. Source:\n$pointIdRendering",
          )
          val uintsRendering = all.collectFirst {
            case (_, content) if content.contains("public sealed record UInts(") => content
          }.getOrElse(fail(s"UInts not found. Paths: ${all.map(_._1)}"))
          assert(
            uintsRendering.contains("u08 out of range"),
            s"Expected u08 range check on parser path. Source:\n$uintsRendering",
          )

          // Uid lowercase enforcement.
          assert(
            mixedRendering.contains("uid not in canonical lowercase form"),
            s"Expected uid lowercase enforcement on parser path. Source:\n$mixedRendering",
          )

          // Empty-field id (Marker): repr is `Marker:1.0.0#` per spec §6.12.
          val markerRendering = all.collectFirst {
            case (_, content) if content.contains("public sealed record Marker(") => content
          }.getOrElse(fail(s"Marker not found. Paths: ${all.map(_._1)}"))
          assert(
            markerRendering.contains("\"Marker:1.0.0#\""),
            s"Expected canonical header literal 'Marker:1.0.0#' for empty-field id. Source:\n$markerRendering",
          )

          // Nested id (Outer.ref → PointId): nested codec dispatch.
          val outerRendering = all.collectFirst {
            case (_, content) if content.contains("public sealed record Outer(") => content
          }.getOrElse(fail(s"Outer not found. Paths: ${all.map(_._1)}"))
          assert(
            outerRendering.contains("PointIdCodec.ParseRepr(cursor)"),
            s"Expected nested PointIdCodec.ParseRepr dispatch. Source:\n$outerRendering",
          )
          assert(
            outerRendering.contains("\"{\" + this.Ref.ToString() + \"}\""),
            s"Expected `{...}` nested-id wrapper in ToString. Source:\n$outerRendering",
          )

          // u64 unsigned formatter must be used.
          assert(
            uintsRendering.contains("U64ToString"),
            s"Expected U64ToString unsigned helper for u64 field. Source:\n$uintsRendering",
          )
        }
    }
  }
}
