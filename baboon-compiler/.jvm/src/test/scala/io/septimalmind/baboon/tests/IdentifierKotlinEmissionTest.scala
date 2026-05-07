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

/** PR-57b emission test: runs the actual `KtDefnTranslator` pipeline against
  * the `identifier-ok` fixture and asserts the emitted Kotlin source for the
  * `id`-typed Dtos matches spec §6 canonical patterns.
  *
  * Covers BOTH JVM-Kotlin and KMP-Kotlin (multiplatform=true). The shape of
  * emitted code is byte-identical between the two backends for everything
  * except the uid parse call (`Uuid.parse` for KMP vs `UUID.fromString` for
  * JVM); we cover both in separate `should` blocks.
  */
final class IdentifierKotlinEmissionTest extends IdentifierKotlinEmissionTestBase[Either]

abstract class IdentifierKotlinEmissionTestBase[F[+_, +_]: Error2: TagKK: BaboonTestModule] extends BaboonTest[F] {

  private def ktTarget(multiplatform: Boolean): KtTarget = KtTarget(
    id = if (multiplatform) "Kotlin-KMP" else "Kotlin",
    output = OutputOptions(
      safeToRemoveExtensions = Set.empty,
      runtime                = RuntimeGenOpt.With,
      generateConversions    = true,
      output                 = FSPath.parse(NEString.unsafeFrom(s"./target/baboon-scalatests-kt${if (multiplatform) "-kmp" else ""}/")),
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
      multiplatform               = multiplatform,
      serviceResult               = ServiceResultConfig.kotlinDefault,
      serviceContext              = ServiceContextConfig.default,
      pragmas                     = Map.empty,
      generateDomainFacade        = false,
    ),
  )

  private def buildModule(target: KtTarget): distage.Module = {
    val baseModule: distage.Module =
      new BaboonModuleJvm[Either](
        CompilerOptions(
          debug                    = false,
          individualInputs         = Set.empty,
          directoryInputs          = Set(FSPath.parse(NEString.unsafeFrom("./baboon-compiler/src/test/resources/baboon"))),
          metaWriteEvolutionJsonTo = None,
          lockFile                 = Some(FSPath.parse(NEString.unsafeFrom("./target/baboon.lock"))),
          emitOnly                 = None,
          targets                  = Seq(target),
        ),
        UnsafeInstances.Lawless_ParallelErrorAccumulatingOpsEither,
      )
    val translatorModule: distage.Module = new BaboonJvmKtModule[Either](target)
    baseModule overriddenBy translatorModule
  }

  override protected def config: TestConfig = super.config.copy(
    pluginConfig = PluginConfig.const(buildModule(ktTarget(multiplatform = false)).morph[PluginBase]),
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

  private def assertCommonEmission(all: List[(String, String)], suffix: String): Unit = {
    val pointIdRendering = all.collectFirst {
      case (_, content) if content.contains("data class PointId(") => content
    }.getOrElse(fail(s"PointId data class not found in $suffix. Paths: ${all.map(_._1)}"))

    // Spec §2.1 / §6.9: header is `<simpleName>:<version>#`.
    assert(
      pointIdRendering.contains("\"PointId:1.0.0#\""),
      s"[$suffix] Expected canonical header literal 'PointId:1.0.0#'. Source:\n$pointIdRendering",
    )

    // Spec §3 row str: emitter MUST escape via the runtime helper.
    assert(
      pointIdRendering.contains("escapeStr"),
      s"[$suffix] Expected escapeStr helper call for str field. Source:\n$pointIdRendering",
    )

    // Spec §5: parser entry point exists at PointIdCodec.parseRepr in same file.
    assert(
      pointIdRendering.contains("object PointIdCodec"),
      s"[$suffix] Expected emitted PointIdCodec object. Source:\n$pointIdRendering",
    )
    assert(
      pointIdRendering.contains("fun parseRepr"),
      s"[$suffix] Expected emitted parseRepr method. Source:\n$pointIdRendering",
    )

    // Spec §3 / §5.4 fixed-width tsu/tso consumption.
    val mixedRendering = all.collectFirst {
      case (_, content) if content.contains("data class Mixed(") => content
    }.getOrElse(fail(s"[$suffix] Mixed not found. Paths: ${all.map(_._1)}"))
    assert(
      mixedRendering.contains("readFixed(24)"),
      s"[$suffix] Expected fixed-width 24-char consumption for tsu (spec §5.4). Source:\n$mixedRendering",
    )
    assert(
      mixedRendering.contains("readFixed(29)"),
      s"[$suffix] Expected fixed-width 29-char consumption for tso (spec §5.4). Source:\n$mixedRendering",
    )
    assert(
      mixedRendering.contains("tsuToString"),
      s"[$suffix] Expected tsuToString helper call. Source:\n$mixedRendering",
    )
    assert(
      mixedRendering.contains("tsoToString"),
      s"[$suffix] Expected tsoToString helper call. Source:\n$mixedRendering",
    )

    // Range-checked narrowing on parse path.
    assert(
      pointIdRendering.contains("i32 out of range"),
      s"[$suffix] Expected i32 range check on parser path. Source:\n$pointIdRendering",
    )
    val uintsRendering = all.collectFirst {
      case (_, content) if content.contains("data class UInts(") => content
    }.getOrElse(fail(s"[$suffix] UInts not found. Paths: ${all.map(_._1)}"))
    assert(
      uintsRendering.contains("u08 out of range"),
      s"[$suffix] Expected u08 range check on parser path. Source:\n$uintsRendering",
    )

    // Uid lowercase enforcement.
    assert(
      mixedRendering.contains("uid not in canonical lowercase form"),
      s"[$suffix] Expected uid lowercase enforcement. Source:\n$mixedRendering",
    )

    // Empty-field id (Marker): repr is `Marker:1.0.0#`.
    val markerRendering = all.collectFirst {
      case (_, content) if content.contains("class Marker") => content
    }.getOrElse(fail(s"[$suffix] Marker not found. Paths: ${all.map(_._1)}"))
    assert(
      markerRendering.contains("\"Marker:1.0.0#\""),
      s"[$suffix] Expected canonical header literal 'Marker:1.0.0#' for empty-field id. Source:\n$markerRendering",
    )

    // Nested id (Outer.ref → PointId): nested codec dispatch.
    val outerRendering = all.collectFirst {
      case (_, content) if content.contains("data class Outer(") => content
    }.getOrElse(fail(s"[$suffix] Outer not found. Paths: ${all.map(_._1)}"))
    assert(
      outerRendering.contains("PointIdCodec.parseRepr(cursor)"),
      s"[$suffix] Expected nested PointIdCodec.parseRepr dispatch. Source:\n$outerRendering",
    )
    assert(
      outerRendering.contains("\"{\" + this.ref.toString() + \"}\""),
      s"[$suffix] Expected `{...}` nested-id wrapper in toString. Source:\n$outerRendering",
    )

    // u64 unsigned formatter must be used.
    assert(
      uintsRendering.contains("u64ToString"),
      s"[$suffix] Expected u64ToString unsigned helper for u64 field. Source:\n$uintsRendering",
    )
  }

  "identifier-ok fixture, JVM-Kotlin target" should {
    "emit toString and parseRepr that match spec §6 canonical patterns" in {
      (loader: BaboonLoader[F], translator: BaboonAbstractTranslator[F]) =>
        for {
          family <- loadIdentifierFamily(loader)
          srcs   <- translator.translate(family)
        } yield {
          val all = srcs.files.iterator.map { case (path, of) => (path, of.content) }.toList
          assertCommonEmission(all, "JVM-Kotlin")

          // JVM-Kotlin specific: uid via java.util.UUID.fromString.
          val mixedRendering = all.collectFirst { case (_, c) if c.contains("data class Mixed(") => c }.get
          assert(
            mixedRendering.contains("UUID.fromString"),
            s"[JVM-Kotlin] Expected java.util.UUID.fromString for uid parse. Source:\n$mixedRendering",
          )
        }
    }
  }
}

final class IdentifierKotlinKmpEmissionTest extends IdentifierKotlinKmpEmissionTestBase[Either]

abstract class IdentifierKotlinKmpEmissionTestBase[F[+_, +_]: Error2: TagKK: BaboonTestModule] extends BaboonTest[F] {

  private val ktTarget: KtTarget = KtTarget(
    id = "Kotlin-KMP",
    output = OutputOptions(
      safeToRemoveExtensions = Set.empty,
      runtime                = RuntimeGenOpt.With,
      generateConversions    = true,
      output                 = FSPath.parse(NEString.unsafeFrom("./target/baboon-scalatests-kt-kmp/")),
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
      multiplatform               = true,
      serviceResult               = ServiceResultConfig.kotlinDefault,
      serviceContext              = ServiceContextConfig.default,
      pragmas                     = Map.empty,
      generateDomainFacade        = false,
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

  "identifier-ok fixture, KMP-Kotlin target" should {
    "emit toString and parseRepr that match spec §6 canonical patterns; uses Uuid.parse for KMP" in {
      (loader: BaboonLoader[F], translator: BaboonAbstractTranslator[F]) =>
        for {
          family <- loadIdentifierFamily(loader)
          srcs   <- translator.translate(family)
        } yield {
          val all = srcs.files.iterator.map { case (path, of) => (path, of.content) }.toList

          val mixedRendering = all.collectFirst {
            case (_, content) if content.contains("data class Mixed(") => content
          }.getOrElse(fail(s"[KMP] Mixed not found. Paths: ${all.map(_._1)}"))

          // KMP-specific: uid parses via kotlin.uuid.Uuid.parse, not java.util.UUID.fromString.
          assert(
            mixedRendering.contains("Uuid.parse"),
            s"[KMP] Expected kotlin.uuid.Uuid.parse for uid parse on KMP. Source:\n$mixedRendering",
          )
          assert(
            !mixedRendering.contains("UUID.fromString"),
            s"[KMP] Must NOT use java.util.UUID.fromString in KMP. Source:\n$mixedRendering",
          )

          // tsu uses kotlinx.datetime.Instant on KMP (vs OffsetDateTime on JVM).
          assert(
            mixedRendering.contains("Instant"),
            s"[KMP] Expected Instant type for tsu on KMP. Source:\n$mixedRendering",
          )

          // Sanity: header literal still correct.
          assert(
            mixedRendering.contains("\"Mixed:1.0.0#\""),
            s"[KMP] Expected canonical Mixed header. Source:\n$mixedRendering",
          )
        }
    }
  }
}
