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

/** PR-30.11 emission test: verifies that `DtDefnTranslator` emits Dart outer-line
  * `///` doc comments for doc-bearing types, fields, and service methods.
  *
  * Uses the `m30-sc-docs` fixture (shared with the Scala, C#, Python, Rust,
  * TypeScript, Kotlin, and Java emission tests — the model content is
  * language-agnostic) and asserts the emitted Dart source contains the
  * expected `///` lines per `docs/spec/docstrings.md` §7.8.
  */
final class DocCommentDartEmissionTest extends DocCommentDartEmissionTestBase[Either]

abstract class DocCommentDartEmissionTestBase[F[+_, +_]: Error2: TagKK: BaboonTestModule] extends BaboonTest[F] {

  private val dtTarget: DtTarget = DtTarget(
    id = "Dart",
    output = OutputOptions(
      safeToRemoveExtensions = Set.empty,
      runtime                = RuntimeGenOpt.With,
      generateConversions    = true,
      output                 = FSPath.parse(NEString.unsafeFrom("./target/baboon-dt-docs-test/")),
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

  private def loadDocsFamily(loader: BaboonLoader[F]): F[NEList[BaboonIssue], BaboonFamily] = {
    val root = IzResources
      .getPath("baboon/m30-sc-docs")
      .getOrElse(throw new AssertionError("m30-sc-docs fixture not found"))
      .asInstanceOf[IzResources.LoadablePathReference]
      .path
    val baboons =
      if (root.toFile.isDirectory)
        IzFiles.walk(root.toFile).toList.filter(p => p.toFile.isFile && p.toFile.getName.endsWith(".baboon"))
      else List(root)
    loader.load(baboons)
  }

  "m30-sc-docs fixture, Dart target" should {

    "emit /// type-level doc before class declaration (spec §7.8)" in {
      (loader: BaboonLoader[F], translator: BaboonAbstractTranslator[F]) =>
        for {
          family <- loadDocsFamily(loader)
          srcs   <- translator.translate(family)
        } yield {
          val all = srcs.files.iterator.map { case (path, of) => (path, of.content) }.toList

          val itemFile = all.collectFirst {
            case (_, c) if c.contains("class DocItem") => c
          }.getOrElse(fail(s"DocItem not found. Paths: ${all.map(_._1)}"))

          // Type-level doc before `class DocItem`
          assert(
            itemFile.contains("/// A simple item with field-level docs."),
            s"Expected type-level /// doc before DocItem.\n$itemFile",
          )

          // Field-level docs
          assert(
            itemFile.contains("/// Display name of the item."),
            s"Expected field /// doc for DocItem.name.\n$itemFile",
          )
          // price has both prefix doc and suffix `//!` — merged as sequential /// lines
          assert(
            itemFile.contains("Unit price in store currency."),
            s"Expected prefix part of merged /// doc for DocItem.price.\n$itemFile",
          )
          assert(
            itemFile.contains("never negative"),
            s"Expected suffix part of merged /// doc for DocItem.price.\n$itemFile",
          )
        }
    }

    "emit /// type-level doc before enum declaration (spec §7.8)" in {
      (loader: BaboonLoader[F], translator: BaboonAbstractTranslator[F]) =>
        for {
          family <- loadDocsFamily(loader)
          srcs   <- translator.translate(family)
        } yield {
          val all = srcs.files.iterator.map { case (path, of) => (path, of.content) }.toList

          val severityFile = all.collectFirst {
            case (_, c) if c.contains("enum DocSeverity") => c
          }.getOrElse(fail(s"DocSeverity not found. Paths: ${all.map(_._1)}"))

          assert(
            severityFile.contains("/// Severity levels."),
            s"Expected type-level /// doc before DocSeverity.\n$severityFile",
          )
        }
    }

    "emit /// type-level doc before sealed class for ADT (spec §7.8)" in {
      (loader: BaboonLoader[F], translator: BaboonAbstractTranslator[F]) =>
        for {
          family <- loadDocsFamily(loader)
          srcs   <- translator.translate(family)
        } yield {
          val all = srcs.files.iterator.map { case (path, of) => (path, of.content) }.toList

          val resultFile = all.collectFirst {
            case (_, c) if c.contains("sealed class DocResult") => c
          }.getOrElse(fail(s"DocResult ADT not found. Paths: ${all.map(_._1)}"))

          assert(
            resultFile.contains("/// Result or error."),
            s"Expected type-level /// doc before DocResult sealed class.\n$resultFile",
          )
        }
    }

    "emit /// type-level and method-level docs in service abstract class (spec §7.8)" in {
      (loader: BaboonLoader[F], translator: BaboonAbstractTranslator[F]) =>
        for {
          family <- loadDocsFamily(loader)
          srcs   <- translator.translate(family)
        } yield {
          val all = srcs.files.iterator.map { case (path, of) => (path, of.content) }.toList

          val crudFile = all.collectFirst {
            case (_, c) if c.contains("abstract class DocCrud") => c
          }.getOrElse(fail(s"DocCrud service not found. Paths: ${all.map(_._1)}"))

          assert(
            crudFile.contains("/// The CRUD service."),
            s"Expected type-level /// doc before DocCrud abstract class.\n$crudFile",
          )
          assert(
            crudFile.contains("/// Create an item."),
            s"Expected method /// doc for DocCrud.create.\n$crudFile",
          )
        }
    }

    "emit /// arm-level doc before ADT branch class (PR-30.11 / spec §7.8)" in {
      (loader: BaboonLoader[F], translator: BaboonAbstractTranslator[F]) =>
        for {
          family <- loadDocsFamily(loader)
          srcs   <- translator.translate(family)
        } yield {
          val all = srcs.files.iterator.map { case (path, of) => (path, of.content) }.toList

          val resultFile = all.collectFirst {
            case (_, c) if c.contains("class DocOk") => c
          }.getOrElse(fail(s"DocOk ADT arm not found. Paths: ${all.map(_._1)}"))

          // Arm-level doc on DocOk
          assert(
            resultFile.contains("/// Successful payload variant."),
            s"Expected arm-level /// doc before DocOk class.\n$resultFile",
          )
          // Field-level doc inside DocOk arm
          assert(
            resultFile.contains("/// the carried payload"),
            s"Expected field /// doc for DocOk.value.\n$resultFile",
          )
        }
    }

    "emit no spurious doc comments for types without docs (no churn)" in {
      (loader: BaboonLoader[F], translator: BaboonAbstractTranslator[F]) =>
        for {
          family <- loadDocsFamily(loader)
          srcs   <- translator.translate(family)
        } yield {
          val all = srcs.files.iterator.map { case (path, of) => (path, of.content) }.toList

          val pageFile = all.collectFirst {
            case (_, c) if c.contains("class DocPage") => c
          }.getOrElse(fail(s"DocPage not found. Paths: ${all.map(_._1)}"))

          // Page has a type-level doc but its fields (items, total) have none.
          assert(
            pageFile.contains("/// Paged doc results."),
            s"Expected type-level /// doc before DocPage.\n$pageFile",
          )
          // Fields `items` and `total` have no docs — must not have spurious /// lines
          assert(
            !pageFile.contains("/// \nfinal"),
            s"Unexpected empty /// doc line before a field in DocPage.\n$pageFile",
          )
        }
    }
  }
}
