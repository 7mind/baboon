package io.septimalmind.baboon.tests

import distage.plugins.PluginBase
import io.septimalmind.baboon.*
import io.septimalmind.baboon.CompilerTarget.PyTarget
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

/** PR-30.6 emission test: verifies that `PyDefnTranslator` emits PEP 257
  * `"""…"""` docstrings for doc-bearing types, with field docs folded into
  * the class docstring as a Sphinx/Google-style `Attributes:` section per
  * spec §7.6 / Q2 lock.
  *
  * Reuses the `m30-sc-docs` fixture (shared with the Scala and C# emission
  * tests — the model content is language-agnostic).
  */
final class DocCommentPythonEmissionTest extends DocCommentPythonEmissionTestBase[Either]

abstract class DocCommentPythonEmissionTestBase[F[+_, +_]: Error2: TagKK: BaboonTestModule] extends BaboonTest[F] {

  private val pyTarget: PyTarget = PyTarget(
    id = "Python",
    output = OutputOptions(
      safeToRemoveExtensions = Set.empty,
      runtime                = RuntimeGenOpt.With,
      generateConversions    = true,
      output                 = FSPath.parse(NEString.unsafeFrom("./target/baboon-py-docs-test/")),
      fixturesOutput         = None,
      testsOutput            = None,
    ),
    generic = GenericOptions(
      codecTestIterations = 0
    ),
    language = PyOptions(
      writeEvolutionDict          = false,
      wrappedAdtBranchCodecs      = false,
      generateJsonCodecs          = true,
      generateUebaCodecs          = true,
      generateJsonCodecsByDefault = true,
      generateUebaCodecsByDefault = true,
      enableDeprecatedEncoders    = false,
      serviceResult               = ServiceResultConfig.pythonDefault,
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
        lockfile                 = Some(FSPath.parse(NEString.unsafeFrom("./target/baboon.lock"))),
        emitOnly                 = None,
        targets                  = Seq(pyTarget),
      ),
      UnsafeInstances.Lawless_ParallelErrorAccumulatingOpsEither,
    )

  private val translatorModule: distage.Module = new BaboonJvmPyModule[Either](pyTarget)
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

  "m30-sc-docs fixture, Python target" should {

    "emit class docstring with Attributes: section for type with field docs (spec §7.6)" in {
      (loader: BaboonLoader[F], translator: BaboonAbstractTranslator[F]) =>
        for {
          family <- loadDocsFamily(loader)
          srcs   <- translator.translate(family)
        } yield {
          val all = srcs.files.iterator.map { case (path, of) => (path, of.content) }.toList

          val itemFile = all.collectFirst {
            case (_, c) if c.contains("class DocItem(") => c
          }.getOrElse(fail(s"DocItem not found. Paths: ${all.map(_._1)}"))

          // Type-level doc in the class docstring
          assert(
            itemFile.contains("A simple item with field-level docs."),
            s"Expected type-level doc in DocItem class docstring.\n$itemFile",
          )

          // Attributes: section must be present
          assert(
            itemFile.contains("Attributes:"),
            s"Expected Attributes: section in DocItem class docstring.\n$itemFile",
          )

          // Field docs folded into Attributes: section
          assert(
            itemFile.contains("name: Display name of the item."),
            s"Expected field doc for DocItem.name in Attributes: section.\n$itemFile",
          )

          // Suffix doc (`//! never negative`) merged into field entry
          assert(
            itemFile.contains("never negative"),
            s"Expected suffix doc for DocItem.price merged into class docstring.\n$itemFile",
          )

          // Field docs must NOT appear as standalone triple-quoted strings
          // outside the class docstring (Q2 lock).
          val docstringCount = itemFile.sliding(3).count(_ == "\"\"\"")
          assert(
            docstringCount <= 4,
            s"Unexpected extra triple-quote groups (field docs emitted as separate statements?). Count: $docstringCount.\n$itemFile",
          )
        }
    }

    "emit class docstring without Attributes: for type with no field docs (spec §7.6)" in {
      (loader: BaboonLoader[F], translator: BaboonAbstractTranslator[F]) =>
        for {
          family <- loadDocsFamily(loader)
          srcs   <- translator.translate(family)
        } yield {
          val all = srcs.files.iterator.map { case (path, of) => (path, of.content) }.toList

          val pageFile = all.collectFirst {
            case (_, c) if c.contains("class DocPage(") => c
          }.getOrElse(fail(s"DocPage not found. Paths: ${all.map(_._1)}"))

          // Type-level doc present
          assert(
            pageFile.contains("Paged doc results."),
            s"Expected type-level doc in DocPage class docstring.\n$pageFile",
          )

          // No Attributes: section — fields have no docs
          assert(
            !pageFile.contains("Attributes:"),
            s"Unexpected Attributes: section in DocPage (fields have no docs).\n$pageFile",
          )
        }
    }

    "emit class docstring with Attributes: only when type has no doc but fields do (spec §7.6)" in {
      (loader: BaboonLoader[F], translator: BaboonAbstractTranslator[F]) =>
        for {
          family <- loadDocsFamily(loader)
          srcs   <- translator.translate(family)
        } yield {
          // DocOk arm has arm-level doc ("Successful payload variant.") and a field doc ("the carried payload").
          val all = srcs.files.iterator.map { case (path, of) => (path, of.content) }.toList

          val docOkFile = all.collectFirst {
            case (_, c) if c.contains("class DocOk(") => c
          }.getOrElse(fail(s"DocOk ADT arm not found. Paths: ${all.map(_._1)}"))

          // Arm-level doc
          assert(
            docOkFile.contains("Successful payload variant."),
            s"Expected arm-level doc in DocOk class docstring.\n$docOkFile",
          )
          // Field doc for `value` in Attributes: section
          assert(
            docOkFile.contains("value: the carried payload"),
            s"Expected field doc for DocOk.value in Attributes: section.\n$docOkFile",
          )
        }
    }

    "emit no docstring for types without docs (no spurious output)" in {
      (loader: BaboonLoader[F], translator: BaboonAbstractTranslator[F]) =>
        for {
          family <- loadDocsFamily(loader)
          srcs   <- translator.translate(family)
        } yield {
          val all = srcs.files.iterator.map { case (path, of) => (path, of.content) }.toList

          val reqFile = all.collectFirst {
            case (_, c) if c.contains("class DocReq(") => c
          }.getOrElse(fail(s"DocReq not found. Paths: ${all.map(_._1)}"))

          // DocReq has no type-level doc and no field-level docs
          assert(
            !reqFile.contains("\"\"\""),
            s"Unexpected docstring in DocReq (no docs defined).\n$reqFile",
          )
        }
    }

    "emit enum class docstring (spec §7.6)" in {
      (loader: BaboonLoader[F], translator: BaboonAbstractTranslator[F]) =>
        for {
          family <- loadDocsFamily(loader)
          srcs   <- translator.translate(family)
        } yield {
          val all = srcs.files.iterator.map { case (path, of) => (path, of.content) }.toList

          val severityFile = all.collectFirst {
            case (_, c) if c.contains("class DocSeverity(") => c
          }.getOrElse(fail(s"DocSeverity not found. Paths: ${all.map(_._1)}"))

          assert(
            severityFile.contains("Severity levels."),
            s"Expected type-level doc in DocSeverity class docstring.\n$severityFile",
          )
        }
    }

    "emit service class and method docstrings (spec §7.6)" in {
      (loader: BaboonLoader[F], translator: BaboonAbstractTranslator[F]) =>
        for {
          family <- loadDocsFamily(loader)
          srcs   <- translator.translate(family)
        } yield {
          val all = srcs.files.iterator.map { case (path, of) => (path, of.content) }.toList

          val crudFile = all.collectFirst {
            case (_, c) if c.contains("class DocCrud(") => c
          }.getOrElse(fail(s"DocCrud service not found. Paths: ${all.map(_._1)}"))

          // Service-level class docstring
          assert(
            crudFile.contains("The CRUD service."),
            s"Expected service-level doc in DocCrud class docstring.\n$crudFile",
          )
          // Method-level docstring
          assert(
            crudFile.contains("Create an item."),
            s"Expected method doc for DocCrud.create.\n$crudFile",
          )
        }
    }
  }
}
