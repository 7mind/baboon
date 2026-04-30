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

/** PR-57d emission test: runs the actual `TsDefnTranslator` pipeline against
  * the `identifier-ok` fixture and asserts the emitted TypeScript source for
  * the `id`-typed Dtos matches spec §6 canonical patterns.
  */
final class IdentifierTypeScriptEmissionTest extends IdentifierTypeScriptEmissionTestBase[Either]

abstract class IdentifierTypeScriptEmissionTestBase[F[+_, +_]: Error2: TagKK: BaboonTestModule] extends BaboonTest[F] {

  private val tsTarget: TsTarget = TsTarget(
    id = "TypeScript",
    output = OutputOptions(
      safeToRemoveExtensions = Set.empty,
      runtime                = RuntimeGenOpt.With,
      generateConversions    = true,
      output                 = FSPath.parse(NEString.unsafeFrom("./target/baboon-scalatests-ts/")),
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
      asyncServices               = false,
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

  "identifier-ok fixture, TypeScript target" should {

    "emit toString and parseRepr that match spec §6 canonical patterns" in {
      (loader: BaboonLoader[F], translator: BaboonAbstractTranslator[F]) =>
        for {
          family <- loadIdentifierFamily(loader)
          srcs   <- translator.translate(family)
        } yield {
          val all = srcs.files.iterator.map { case (path, of) => (path, of.content) }.toList

          val pointIdRendering = all.collectFirst {
            case (path, content) if path.endsWith("PointId.ts") => content
          }.getOrElse(fail(s"PointId.ts not found. Paths: ${all.map(_._1)}"))

          // Spec §2.1 / §6.9: header literal `<simpleName>:<version>#`.
          assert(
            pointIdRendering.contains("\"PointId:1.0.0#\""),
            s"Expected canonical header literal. Source:\n$pointIdRendering",
          )

          // toString method on the class (Q-FU-4).
          assert(
            pointIdRendering.contains("public toString(): string"),
            s"Expected `public toString(): string`. Source:\n$pointIdRendering",
          )

          // Spec §3 row str: emitter MUST escape via the runtime helper.
          assert(
            pointIdRendering.contains("escapeStr"),
            s"Expected escapeStr helper call. Source:\n$pointIdRendering",
          )

          // Q-FU-4: parser entry point on exported `pointIdCodec` object — NOT static on class.
          assert(
            pointIdRendering.contains("export const pointIdCodec"),
            s"Expected `export const pointIdCodec`. Source:\n$pointIdRendering",
          )
          assert(
            pointIdRendering.contains("parseRepr(s: string)"),
            s"Expected `parseRepr(s: string)` method. Source:\n$pointIdRendering",
          )

          // Q-FU-4: NO static parseRepr on the class itself.
          assert(
            !pointIdRendering.contains("public static parseRepr") && !pointIdRendering.contains("static parseRepr(s: string)"),
            s"Must NOT emit class-static parseRepr (Q-FU-4). Source:\n$pointIdRendering",
          )

          // Spec §3 / §5.4 fixed-width tsu/tso consumption.
          val mixedRendering = all.collectFirst {
            case (path, content) if path.endsWith("Mixed.ts") => content
          }.getOrElse(fail(s"Mixed.ts not found. Paths: ${all.map(_._1)}"))

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
            case (path, content) if path.endsWith("UInts.ts") => content
          }.getOrElse(fail(s"UInts.ts not found. Paths: ${all.map(_._1)}"))
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
            case (path, content) if path.endsWith("Marker.ts") => content
          }.getOrElse(fail(s"Marker.ts not found. Paths: ${all.map(_._1)}"))
          assert(
            markerRendering.contains("\"Marker:1.0.0#\""),
            s"Expected canonical empty-field header. Source:\n$markerRendering",
          )

          // Nested id (Outer.ref → PointId): nested codec dispatch.
          val outerRendering = all.collectFirst {
            case (path, content) if path.endsWith("Outer.ts") => content
          }.getOrElse(fail(s"Outer.ts not found. Paths: ${all.map(_._1)}"))
          assert(
            outerRendering.contains("pointIdCodec.parseReprCursor"),
            s"Expected nested pointIdCodec.parseReprCursor dispatch. Source:\n$outerRendering",
          )
          assert(
            outerRendering.contains("toString()"),
            s"Expected nested toString() invocation. Source:\n$outerRendering",
          )

          // u64 unsigned formatter must be used.
          assert(
            uintsRendering.contains("u64ToString"),
            s"Expected u64ToString unsigned helper for u64 field. Source:\n$uintsRendering",
          )

          // PR-57a-D01 carryover: i64 always-true range check elided (no `if (!(true))`).
          val longIdRendering = all.collectFirst {
            case (path, content) if path.endsWith("LongId.ts") => content
          }.getOrElse(fail(s"LongId.ts not found. Paths: ${all.map(_._1)}"))
          assert(
            !longIdRendering.contains("if (!(true))"),
            s"Must NOT emit dead always-true range check for i64. Source:\n$longIdRendering",
          )
        }
    }
  }
}
