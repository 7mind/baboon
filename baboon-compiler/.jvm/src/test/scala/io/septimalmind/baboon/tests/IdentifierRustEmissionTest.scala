package io.septimalmind.baboon.tests

import distage.plugins.PluginBase
import io.septimalmind.baboon.*
import io.septimalmind.baboon.CompilerTarget.RsTarget
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

/** PR-57c emission test: runs the actual `RsDefnTranslator` pipeline against
  * the `identifier-ok` fixture and asserts the emitted Rust source for the
  * `id`-typed Dtos matches spec §6 canonical patterns.
  */
final class IdentifierRustEmissionTest extends IdentifierRustEmissionTestBase[Either]

abstract class IdentifierRustEmissionTestBase[F[+_, +_]: Error2: TagKK: BaboonTestModule] extends BaboonTest[F] {

  private val rsTarget: RsTarget = RsTarget(
    id = "Rust",
    output = OutputOptions(
      safeToRemoveExtensions = Set.empty,
      runtime                = RuntimeGenOpt.With,
      generateConversions    = true,
      output                 = FSPath.parse(NEString.unsafeFrom("./target/baboon-scalatests-rs/")),
      fixturesOutput         = None,
      testsOutput            = None,
    ),
    generic = GenericOptions(
      codecTestIterations = 0
    ),
    language = RsOptions(
      writeEvolutionDict          = false,
      wrappedAdtBranchCodecs      = false,
      generateJsonCodecs          = true,
      generateUebaCodecs          = true,
      generateJsonCodecsByDefault = true,
      generateUebaCodecsByDefault = true,
      serviceResult               = ServiceResultConfig.rustDefault,
      serviceContext              = ServiceContextConfig.default,
      pragmas                     = Map.empty,
      generateDomainFacade        = false,
      asyncServices               = false,
      cratePrefix                 = "crate",
      reexportMode                = "selective",
      edition                     = "2021",
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
        targets                  = Seq(rsTarget),
      ),
      UnsafeInstances.Lawless_ParallelErrorAccumulatingOpsEither,
    )

  private val translatorModule: distage.Module = new BaboonJvmRsModule[Either](rsTarget)
  private val combinedModule: distage.Module   = baseModule overriddenBy translatorModule

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

  "identifier-ok fixture, Rust target" should {

    "emit Display + parse_repr that match spec §6 canonical patterns" in {
      (loader: BaboonLoader[F], translator: BaboonAbstractTranslator[F]) =>
        for {
          family <- loadIdentifierFamily(loader)
          srcs   <- translator.translate(family)
        } yield {
          val all = srcs.files.iterator.map { case (path, of) => (path, of.content) }.toList

          val pointIdRendering = all.collectFirst {
            case (path, content) if path.endsWith("point_id.rs") => content
          }.getOrElse(fail(s"point_id.rs not found. Paths: ${all.map(_._1)}"))

          // Spec §2.1 / §6.9: header literal `<simpleName>:<version>#`.
          assert(
            pointIdRendering.contains("\"PointId:1.0.0#\""),
            s"Expected canonical header literal. Source:\n$pointIdRendering",
          )

          // Display impl shape.
          assert(
            pointIdRendering.contains("impl std::fmt::Display for PointId"),
            s"Expected `impl Display`. Source:\n$pointIdRendering",
          )

          // Spec §3 row str: emitter MUST escape via the runtime helper.
          assert(
            pointIdRendering.contains("escape_str"),
            s"Expected escape_str helper call. Source:\n$pointIdRendering",
          )

          // Spec §5: parser entry point exists on a free function in the same module.
          assert(
            pointIdRendering.contains("pub fn parse_repr(s: &str)"),
            s"Expected `pub fn parse_repr(s: &str)`. Source:\n$pointIdRendering",
          )
          assert(
            pointIdRendering.contains("pub fn parse_repr_cursor"),
            s"Expected `pub fn parse_repr_cursor`. Source:\n$pointIdRendering",
          )

          // Q-FU-4: NO `impl FromStr` (would advertise canonical .parse() — undesirable).
          assert(
            !pointIdRendering.contains("impl std::str::FromStr for PointId"),
            s"Must NOT emit `impl FromStr` for identifier types (Q-FU-4). Source:\n$pointIdRendering",
          )

          // Spec §3 / §5.4 fixed-width tsu/tso consumption.
          val mixedRendering = all.collectFirst {
            case (path, content) if path.endsWith("mixed.rs") => content
          }.getOrElse(fail(s"mixed.rs not found. Paths: ${all.map(_._1)}"))

          assert(
            mixedRendering.contains("read_fixed(24)"),
            s"Expected fixed-width 24-char consumption for tsu. Source:\n$mixedRendering",
          )
          assert(
            mixedRendering.contains("read_fixed(29)"),
            s"Expected fixed-width 29-char consumption for tso. Source:\n$mixedRendering",
          )
          assert(
            mixedRendering.contains("tsu_to_string"),
            s"Expected tsu_to_string helper call. Source:\n$mixedRendering",
          )
          assert(
            mixedRendering.contains("tso_to_string"),
            s"Expected tso_to_string helper call. Source:\n$mixedRendering",
          )

          // Range-checked narrowing on parse path.
          assert(
            pointIdRendering.contains("i32 out of range"),
            s"Expected i32 range check. Source:\n$pointIdRendering",
          )
          val uintsRendering = all.collectFirst {
            case (path, content) if path.endsWith("u_ints.rs") => content
          }.getOrElse(fail(s"u_ints.rs not found. Paths: ${all.map(_._1)}"))
          assert(
            uintsRendering.contains("u08 out of range"),
            s"Expected u08 range check. Source:\n$uintsRendering",
          )

          // Uid lowercase enforcement.
          assert(
            mixedRendering.contains("uid not in canonical lowercase form"),
            s"Expected uid lowercase enforcement. Source:\n$mixedRendering",
          )

          // Empty-field id (Marker): repr is `Marker:1.0.0#` per spec §6.12.
          val markerRendering = all.collectFirst {
            case (path, content) if path.endsWith("marker.rs") => content
          }.getOrElse(fail(s"marker.rs not found. Paths: ${all.map(_._1)}"))
          assert(
            markerRendering.contains("\"Marker:1.0.0#\""),
            s"Expected canonical empty-field header. Source:\n$markerRendering",
          )

          // Nested id (Outer.ref → PointId): nested codec dispatch.
          val outerRendering = all.collectFirst {
            case (path, content) if path.endsWith("outer.rs") => content
          }.getOrElse(fail(s"outer.rs not found. Paths: ${all.map(_._1)}"))
          assert(
            outerRendering.contains("point_id::point_id_repr_codec::parse_repr_cursor"),
            s"Expected nested PointId point_id_repr_codec::parse_repr_cursor dispatch. Source:\n$outerRendering",
          )
          assert(
            outerRendering.contains("\"{{{}}}\""),
            s"Expected `{...}` nested-id wrapper via format!. Source:\n$outerRendering",
          )

          // u64 unsigned formatter must be used.
          assert(
            uintsRendering.contains("u64_to_string"),
            s"Expected u64_to_string unsigned helper for u64 field. Source:\n$uintsRendering",
          )
        }
    }
  }
}
