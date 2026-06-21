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

/** PR-30.5 emission test: verifies that `CSDefnTranslator` emits C# XML doc
  * comments (`/// <summary>…</summary>`) for doc-bearing types, fields, and
  * service methods.
  *
  * Uses the `m30-sc-docs` fixture (shared with the Scala emission test — the
  * model content is language-agnostic) and asserts the emitted C# source
  * contains the expected `/// <summary>…</summary>` blocks per
  * `docs/spec/docstrings.md` §7.5.
  */
final class DocCommentCsharpEmissionTest extends DocCommentCsharpEmissionTestBase[Either]

abstract class DocCommentCsharpEmissionTestBase[F[+_, +_]: Error2: TagKK: BaboonTestModule] extends BaboonTest[F] {

  private val csTarget: CSTarget = CSTarget(
    id = "C#",
    output = OutputOptions(
      safeToRemoveExtensions = Set.empty,
      runtime                = RuntimeGenOpt.With,
      generateConversions    = true,
      output                 = FSPath.parse(NEString.unsafeFrom("./target/baboon-cs-docs-test/")),
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
        targets                  = Seq(csTarget),
      ),
      UnsafeInstances.Lawless_ParallelErrorAccumulatingOpsEither,
    )

  private val translatorModule: distage.Module = new BaboonJvmCSModule[Either](csTarget)
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

  "m30-sc-docs fixture, C# target" should {

    "emit /// <summary> type-level doc before sealed record (spec §7.5)" in {
      (loader: BaboonLoader[F], translator: BaboonAbstractTranslator[F]) =>
        for {
          family <- loadDocsFamily(loader)
          srcs   <- translator.translate(family)
        } yield {
          val all = srcs.files.iterator.map { case (path, of) => (path, of.content) }.toList

          val itemFile = all.collectFirst {
            case (_, c) if c.contains("sealed record DocItem") => c
          }.getOrElse(fail(s"DocItem not found. Paths: ${all.map(_._1)}"))

          // Type-level doc before `public sealed record DocItem`
          assert(
            itemFile.contains("/// <summary>A simple item with field-level docs.</summary>"),
            s"Expected type-level XML doc before DocItem.\n$itemFile",
          )

          // Field-level docs — D38: emitted as <param> tags on the type doc block,
          // NOT as /// <summary> before the positional constructor parameters.
          assert(
            itemFile.contains("""/// <param name="Name">Display name of the item.</param>"""),
            s"Expected <param name=\"Name\"> field doc for DocItem.Name.\n$itemFile",
          )
          // price has both prefix doc and suffix `//!` — both appear inside the <param> body.
          assert(
            itemFile.contains("Unit price in store currency."),
            s"Expected prefix part of merged doc for DocItem.Price.\n$itemFile",
          )
          assert(
            itemFile.contains("never negative"),
            s"Expected suffix part of merged doc for DocItem.Price.\n$itemFile",
          )
        }
    }

    // D38 RED assertions — encode the CORRECT post-fix shape.
    // After the fix, CSDefnTranslator must emit per-field docs as
    // `/// <param name="X">...</param>` tags on the record TYPE doc block,
    // NOT as `/// <summary>` comments before positional constructor parameters
    // (which C# does not associate with positional record properties — CS1587).
    "emit field docs as <param> tags on the record type doc block, not as /// <summary> before positional params (D38)" in {
      (loader: BaboonLoader[F], translator: BaboonAbstractTranslator[F]) =>
        for {
          family <- loadDocsFamily(loader)
          srcs   <- translator.translate(family)
        } yield {
          val all = srcs.files.iterator.map { case (path, of) => (path, of.content) }.toList

          val itemFile = all.collectFirst {
            case (_, c) if c.contains("sealed record DocItem") => c
          }.getOrElse(fail(s"DocItem not found. Paths: ${all.map(_._1)}"))

          // (c) The record TYPE-level <summary> must still be present before `public sealed record`.
          assert(
            itemFile.contains("/// <summary>A simple item with field-level docs.</summary>"),
            s"Expected type-level <summary> still present before DocItem record.\n$itemFile",
          )

          // (a) Field docs must appear as <param> tags on the type doc block, not inside the ctor.
          // DocItem.name => C# property Name; doc text: "Display name of the item."
          assert(
            itemFile.contains("""/// <param name="Name">Display name of the item.</param>"""),
            s"""Expected <param name="Name"> tag for DocItem.name field doc, but not found.\n$itemFile""",
          )
          // DocItem.price => C# property Price; prefix doc "Unit price in store currency."
          // plus the `//!` suffix "never negative" form two paragraphs, so renderParamDocs
          // emits the multi-line wrapped <param> form (open tag, body lines, close tag).
          assert(
            itemFile.contains("""/// <param name="Price">""") &&
              itemFile.contains("/// Unit price in store currency.") &&
              itemFile.contains("/// never negative"),
            s"""Expected <param name="Price"> wrapped tag for DocItem.price field doc, but not found.\n$itemFile""",
          )

          // (b) NO `/// <summary>` block must appear immediately before a positional constructor
          // parameter inside the `public sealed record ...(` header (CS1587).
          // The current (broken) output has `/// <summary>Display name of the item.</summary>`
          // between `(` and `String Name,` — this must no longer be present in that position.
          val ctorRegion = {
            val open  = itemFile.indexOf("public sealed record DocItem(")
            val close = itemFile.indexOf(")", open)
            if (open < 0 || close < 0) ""
            else itemFile.substring(open, close + 1)
          }
          assert(
            !ctorRegion.contains("/// <summary>Display name of the item.</summary>"),
            s"Field doc must NOT appear as /// <summary> inside the positional constructor parameter list.\nCtor region:\n$ctorRegion",
          )
          assert(
            !ctorRegion.contains("/// <summary>Unit price in store currency.</summary>"),
            s"Field doc must NOT appear as /// <summary> inside the positional constructor parameter list.\nCtor region:\n$ctorRegion",
          )
        }
    }

    "emit /// <summary> type-level doc before enum (spec §7.5)" in {
      (loader: BaboonLoader[F], translator: BaboonAbstractTranslator[F]) =>
        for {
          family <- loadDocsFamily(loader)
          srcs   <- translator.translate(family)
        } yield {
          val all = srcs.files.iterator.map { case (path, of) => (path, of.content) }.toList

          val severityFile = all.collectFirst {
            case (_, c) if c.contains("public enum DocSeverity") => c
          }.getOrElse(fail(s"DocSeverity not found. Paths: ${all.map(_._1)}"))

          assert(
            severityFile.contains("/// <summary>Severity levels.</summary>"),
            s"Expected type-level XML doc before DocSeverity.\n$severityFile",
          )
        }
    }

    "emit /// <summary> type-level doc before abstract record for ADT (spec §7.5)" in {
      (loader: BaboonLoader[F], translator: BaboonAbstractTranslator[F]) =>
        for {
          family <- loadDocsFamily(loader)
          srcs   <- translator.translate(family)
        } yield {
          val all = srcs.files.iterator.map { case (path, of) => (path, of.content) }.toList

          val resultFile = all.collectFirst {
            case (_, c) if c.contains("abstract record DocResult") => c
          }.getOrElse(fail(s"DocResult ADT not found. Paths: ${all.map(_._1)}"))

          assert(
            resultFile.contains("/// <summary>Result or error.</summary>"),
            s"Expected type-level XML doc before DocResult.\n$resultFile",
          )
        }
    }

    "emit /// <summary> type-level and method-level docs in service interface (spec §7.5)" in {
      (loader: BaboonLoader[F], translator: BaboonAbstractTranslator[F]) =>
        for {
          family <- loadDocsFamily(loader)
          srcs   <- translator.translate(family)
        } yield {
          val all = srcs.files.iterator.map { case (path, of) => (path, of.content) }.toList

          val crudFile = all.collectFirst {
            case (_, c) if c.contains("interface IDocCrud") => c
          }.getOrElse(fail(s"DocCrud service not found. Paths: ${all.map(_._1)}"))

          assert(
            crudFile.contains("/// <summary>The CRUD service.</summary>"),
            s"Expected type-level XML doc before DocCrud.\n$crudFile",
          )
          assert(
            crudFile.contains("/// <summary>Create an item.</summary>"),
            s"Expected method doc for DocCrud.create.\n$crudFile",
          )
        }
    }

    "emit /// <summary> arm-level doc before ADT arm sealed record (PR-30.5 / spec §7.5)" in {
      (loader: BaboonLoader[F], translator: BaboonAbstractTranslator[F]) =>
        for {
          family <- loadDocsFamily(loader)
          srcs   <- translator.translate(family)
        } yield {
          val all = srcs.files.iterator.map { case (path, of) => (path, of.content) }.toList

          val resultFile = all.collectFirst {
            case (_, c) if c.contains("sealed record DocOk") => c
          }.getOrElse(fail(s"DocOk ADT arm not found. Paths: ${all.map(_._1)}"))

          // Arm-level doc on DocOk
          assert(
            resultFile.contains("/// <summary>Successful payload variant.</summary>"),
            s"Expected arm-level XML doc before DocOk.\n$resultFile",
          )
          // Field-level doc inside DocOk arm — D38: rendered as a <param> tag on the
          // arm record's type doc block, NOT as /// <summary> before the positional param.
          assert(
            resultFile.contains("""/// <param name="Value">the carried payload</param>"""),
            s"""Expected <param name="Value"> tag for DocOk.value field doc.\n$resultFile""",
          )
        }
    }

    "emit no spurious empty doc blocks for types without docs (no churn)" in {
      (loader: BaboonLoader[F], translator: BaboonAbstractTranslator[F]) =>
        for {
          family <- loadDocsFamily(loader)
          srcs   <- translator.translate(family)
        } yield {
          val all = srcs.files.iterator.map { case (path, of) => (path, of.content) }.toList

          val pageFile = all.collectFirst {
            case (_, c) if c.contains("sealed record DocPage") => c
          }.getOrElse(fail(s"DocPage not found. Paths: ${all.map(_._1)}"))

          // Page has a type-level doc but its fields have none.
          assert(
            pageFile.contains("/// <summary>Paged doc results.</summary>"),
            s"Expected type-level XML doc before DocPage.\n$pageFile",
          )
          // No empty summary tags must appear
          assert(
            !pageFile.contains("/// <summary></summary>"),
            s"Unexpected empty XML doc block in DocPage.\n$pageFile",
          )
        }
    }
  }
}
