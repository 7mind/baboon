package io.septimalmind.baboon.tests

import distage.plugins.PluginBase
import io.septimalmind.baboon.*
import io.septimalmind.baboon.CompilerTarget.DtTarget
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

/** PR-57d emission test: runs the actual `DtDefnTranslator` pipeline against
  * the `identifier-ok` fixture and asserts the emitted Dart source for the
  * `id`-typed Dtos matches spec §6 canonical patterns.
  */
final class IdentifierDartEmissionTest extends IdentifierDartEmissionTestBase[Either]

abstract class IdentifierDartEmissionTestBase[F[+_, +_]: Error2: TagKK: BaboonTestModule] extends BaboonTest[F] {

  private val dtTarget: DtTarget = DtTarget(
    id = "Dart",
    output = OutputOptions(
      safeToRemoveExtensions = Set.empty,
      runtime                = RuntimeGenOpt.With,
      generateConversions    = true,
      output                 = FSPath.parse(NEString.unsafeFrom("./target/baboon-scalatests-dt/")),
      fixturesOutput         = None,
      testsOutput            = None,
    ),
    generic = GenericOptions(
      codecTestIterations = 0
    ),
    language = DtOptions(
      writeEvolutionDict          = false,
      wrappedAdtBranchCodecs      = false,
      generateJsonCodecs          = true,
      generateUebaCodecs          = true,
      generateJsonCodecsByDefault = true,
      generateUebaCodecsByDefault = true,
      serviceResult               = ServiceResultConfig.dartDefault,
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
        targets                  = Seq(dtTarget),
      ),
      UnsafeInstances.Lawless_ParallelErrorAccumulatingOpsEither,
    )

  private val translatorModule: distage.Module = new BaboonJvmDtModule[Either](dtTarget)
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

  "identifier-ok fixture, Dart target" should {

    "emit toString and parseRepr that match spec §6 canonical patterns" in {
      (loader: BaboonLoader[F], translator: BaboonAbstractTranslator[F]) =>
        for {
          family <- loadIdentifierFamily(loader)
          srcs   <- translator.translate(family)
        } yield {
          val all = srcs.files.iterator.map { case (path, of) => (path, of.content) }.toList

          val pointIdRendering = all.collectFirst {
            case (path, content) if path.endsWith("point_id.dart") => content
          }.getOrElse(fail(s"point_id.dart not found. Paths: ${all.map(_._1)}"))

          // Spec §2.1 / §6.9: header literal `<simpleName>:<version>#`.
          assert(
            pointIdRendering.contains("\"PointId:1.0.0#\""),
            s"Expected canonical header literal. Source:\n$pointIdRendering",
          )

          // Spec §3 row str: emitter MUST escape via the runtime helper.
          assert(
            pointIdRendering.contains("escapeStr"),
            s"Expected escapeStr helper call. Source:\n$pointIdRendering",
          )

          // Q-FU-4: parseRepr lives on a sibling `<TypeName>Codec` class.
          assert(
            pointIdRendering.contains("class PointIdCodec"),
            s"Expected `class PointIdCodec`. Source:\n$pointIdRendering",
          )
          assert(
            pointIdRendering.contains("static") && pointIdRendering.contains("parseRepr(String s)"),
            s"Expected static parseRepr on PointIdCodec. Source:\n$pointIdRendering",
          )

          // Q-FU-4: NO parseRepr method on the data class itself (only via codec class).
          // Verify by counting "parseRepr" occurrences against codec class layout.
          val parseReprCount = "parseRepr".r.findAllIn(pointIdRendering).size
          assert(
            parseReprCount > 0,
            s"Expected parseRepr to appear in PointIdCodec. Source:\n$pointIdRendering",
          )

          // Spec §3 / §5.4 fixed-width tsu/tso consumption.
          val mixedRendering = all.collectFirst {
            case (path, content) if path.endsWith("mixed.dart") => content
          }.getOrElse(fail(s"mixed.dart not found. Paths: ${all.map(_._1)}"))

          assert(
            mixedRendering.contains("readFixed(24)"),
            s"Expected fixed-width 24-char consumption for tsu. Source:\n$mixedRendering",
          )
          assert(
            mixedRendering.contains("readFixed(29)"),
            s"Expected fixed-width 29-char consumption for tso. Source:\n$mixedRendering",
          )
          assert(
            mixedRendering.contains("tsuToString"),
            s"Expected tsuToString helper call. Source:\n$mixedRendering",
          )
          assert(
            mixedRendering.contains("tsoToString"),
            s"Expected tsoToString helper call. Source:\n$mixedRendering",
          )

          // Range-checked narrowing on parse path.
          assert(
            pointIdRendering.contains("i32 out of range"),
            s"Expected i32 range check. Source:\n$pointIdRendering",
          )
          val uintsRendering = all.collectFirst {
            case (path, content) if path.endsWith("u_ints.dart") => content
          }.getOrElse(fail(s"u_ints.dart not found. Paths: ${all.map(_._1)}"))
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
            case (path, content) if path.endsWith("marker.dart") => content
          }.getOrElse(fail(s"marker.dart not found. Paths: ${all.map(_._1)}"))
          assert(
            markerRendering.contains("\"Marker:1.0.0#\""),
            s"Expected canonical empty-field header. Source:\n$markerRendering",
          )

          // Nested id (Outer.ref → PointId): nested codec dispatch.
          val outerRendering = all.collectFirst {
            case (path, content) if path.endsWith("outer.dart") => content
          }.getOrElse(fail(s"outer.dart not found. Paths: ${all.map(_._1)}"))
          assert(
            outerRendering.contains("PointIdCodec.parseReprCursor"),
            s"Expected nested PointIdCodec.parseReprCursor dispatch. Source:\n$outerRendering",
          )

          // u64 unsigned formatter must be used.
          assert(
            uintsRendering.contains("u64ToString"),
            s"Expected u64ToString unsigned helper for u64 field. Source:\n$uintsRendering",
          )

          // PR-57a-D01 carryover: i64 always-true range check elided.
          val longIdRendering = all.collectFirst {
            case (path, content) if path.endsWith("long_id.dart") => content
          }.getOrElse(fail(s"long_id.dart not found. Paths: ${all.map(_._1)}"))
          assert(
            !longIdRendering.contains("i64 out of range"),
            s"Must NOT emit dead always-true range check for i64. Source:\n$longIdRendering",
          )
        }
    }
  }
}
