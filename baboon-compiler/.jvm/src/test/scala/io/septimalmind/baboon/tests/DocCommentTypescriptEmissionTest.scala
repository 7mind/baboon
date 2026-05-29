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

/** PR-30.8 emission test: verifies that `TsDefnTranslator` emits Javadoc-style
  * `/** … */` doc comments for doc-bearing types, fields, and service methods.
  *
  * Uses the `m30-sc-docs` fixture (shared with the Scala, C#, Python, and Rust
  * emission tests — the model content is language-agnostic) and asserts the
  * emitted TypeScript source contains the expected `/** … */` blocks per
  * `docs/spec/docstrings.md` §7.4.
  */
final class DocCommentTypescriptEmissionTest extends DocCommentTypescriptEmissionTestBase[Either]

abstract class DocCommentTypescriptEmissionTestBase[F[+_, +_]: Error2: TagKK: BaboonTestModule] extends BaboonTest[F] {

  private val tsTarget: TsTarget = TsTarget(
    id = "TypeScript",
    output = OutputOptions(
      safeToRemoveExtensions = Set.empty,
      runtime                = RuntimeGenOpt.With,
      generateConversions    = true,
      output                 = FSPath.parse(NEString.unsafeFrom("./target/baboon-ts-docs-test/")),
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
      generateDomainFacade        = false,
      asyncServices               = false,
      bareServiceSymbols          = false,
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

  "m30-sc-docs fixture, TypeScript target" should {

    "emit /** type-level */ doc before exported class (spec §7.4)" in {
      (loader: BaboonLoader[F], translator: BaboonAbstractTranslator[F]) =>
        for {
          family <- loadDocsFamily(loader)
          srcs   <- translator.translate(family)
        } yield {
          val all = srcs.files.iterator.map { case (path, of) => (path, of.content) }.toList

          val itemFile = all.collectFirst {
            case (_, c) if c.contains("export class DocItem") => c
          }.getOrElse(fail(s"DocItem not found. Paths: ${all.map(_._1)}"))

          // Type-level doc before `export class DocItem`
          assert(
            itemFile.contains("/** A simple item with field-level docs. */"),
            s"Expected type-level /** */ doc before DocItem.\n$itemFile",
          )

          // Field-level docs on getters
          assert(
            itemFile.contains("/** Display name of the item. */"),
            s"Expected field doc for DocItem.name getter.\n$itemFile",
          )
          // price has both prefix doc and suffix `//!` — merged into one Javadoc block
          assert(
            itemFile.contains("Unit price in store currency."),
            s"Expected prefix part of merged doc for DocItem.price.\n$itemFile",
          )
          assert(
            itemFile.contains("never negative"),
            s"Expected suffix part of merged doc for DocItem.price.\n$itemFile",
          )
        }
    }

    "emit /** type-level */ doc before exported enum (spec §7.4)" in {
      (loader: BaboonLoader[F], translator: BaboonAbstractTranslator[F]) =>
        for {
          family <- loadDocsFamily(loader)
          srcs   <- translator.translate(family)
        } yield {
          val all = srcs.files.iterator.map { case (path, of) => (path, of.content) }.toList

          val severityFile = all.collectFirst {
            case (_, c) if c.contains("export enum DocSeverity") => c
          }.getOrElse(fail(s"DocSeverity not found. Paths: ${all.map(_._1)}"))

          assert(
            severityFile.contains("/** Severity levels. */"),
            s"Expected type-level /** */ doc before DocSeverity.\n$severityFile",
          )
        }
    }

    "emit /** type-level */ doc before exported ADT type union (spec §7.4)" in {
      (loader: BaboonLoader[F], translator: BaboonAbstractTranslator[F]) =>
        for {
          family <- loadDocsFamily(loader)
          srcs   <- translator.translate(family)
        } yield {
          val all = srcs.files.iterator.map { case (path, of) => (path, of.content) }.toList

          val resultFile = all.collectFirst {
            case (_, c) if c.contains("export type DocResult") => c
          }.getOrElse(fail(s"DocResult ADT not found. Paths: ${all.map(_._1)}"))

          assert(
            resultFile.contains("/** Result or error. */"),
            s"Expected type-level /** */ doc before DocResult type.\n$resultFile",
          )
        }
    }

    "emit /** type-level */ and /** method-level */ docs in service interface (spec §7.4)" in {
      (loader: BaboonLoader[F], translator: BaboonAbstractTranslator[F]) =>
        for {
          family <- loadDocsFamily(loader)
          srcs   <- translator.translate(family)
        } yield {
          val all = srcs.files.iterator.map { case (path, of) => (path, of.content) }.toList

          val crudFile = all.collectFirst {
            case (_, c) if c.contains("interface DocCrud") => c
          }.getOrElse(fail(s"DocCrud service not found. Paths: ${all.map(_._1)}"))

          assert(
            crudFile.contains("/** The CRUD service. */"),
            s"Expected type-level /** */ doc before DocCrud interface.\n$crudFile",
          )
          assert(
            crudFile.contains("/** Create an item. */"),
            s"Expected method /** */ doc for DocCrud.create.\n$crudFile",
          )
        }
    }

    "emit /** arm-level */ doc before ADT branch class (PR-30.8 / spec §7.4)" in {
      (loader: BaboonLoader[F], translator: BaboonAbstractTranslator[F]) =>
        for {
          family <- loadDocsFamily(loader)
          srcs   <- translator.translate(family)
        } yield {
          val all = srcs.files.iterator.map { case (path, of) => (path, of.content) }.toList

          // ADT-branch classes are prefixed with the owning ADT name (`DocResult_DocOk`).
          val resultFile = all.collectFirst {
            case (_, c) if c.contains("export class DocResult_DocOk") => c
          }.getOrElse(fail(s"DocResult_DocOk ADT arm not found. Paths: ${all.map(_._1)}"))

          // Arm-level doc on DocOk
          assert(
            resultFile.contains("/** Successful payload variant. */"),
            s"Expected arm-level /** */ doc before DocOk class.\n$resultFile",
          )
          // Field-level doc inside DocOk arm
          assert(
            resultFile.contains("/** the carried payload */"),
            s"Expected field /** */ doc for DocOk.value getter.\n$resultFile",
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
            case (_, c) if c.contains("export class DocPage") => c
          }.getOrElse(fail(s"DocPage not found. Paths: ${all.map(_._1)}"))

          // Page has a type-level doc but its fields (items, total) have none.
          assert(
            pageFile.contains("/** Paged doc results. */"),
            s"Expected type-level /** */ doc before DocPage.\n$pageFile",
          )
          // Fields `items` and `total` have no docs — must not have spurious /** */ lines
          assert(
            !pageFile.contains("/** */"),
            s"Unexpected empty /** */ doc block in DocPage.\n$pageFile",
          )
        }
    }

    // BAB-T03 regression: every type-only name in a mixed import must carry its own `type`
    // qualifier. Pre-fix the emitter prepended `type ` once before the comma-joined list,
    // producing `import { type BaboonAdtMemberMeta, BaboonGeneratedLatest }` — only the
    // first name got the qualifier, causing TS1484 under `verbatimModuleSyntax: true`.
    "emit per-name `type` qualifier for every type-only name in an ADT import (BAB-T03)" in {
      (loader: BaboonLoader[F], translator: BaboonAbstractTranslator[F]) =>
        for {
          family <- loadDocsFamily(loader)
          srcs   <- translator.translate(family)
        } yield {
          val all = srcs.files.iterator.map { case (path, of) => (path, of.content) }.toList

          // DocResult is an ADT with two members (DocOk, DocErr).  The emitted file for
          // DocResult imports BaboonAdtMemberMeta *and* BaboonGeneratedLatest, both
          // type-only, from the same BaboonSharedRuntime module.  That produces a
          // multi-name type-only import — the exact case where the pre-fix emitter
          // dropped `type` from every name after the first.
          val resultFile = all.collectFirst {
            case (_, c) if c.contains("export type DocResult") => c
          }.getOrElse(fail(s"DocResult ADT not found. Paths: ${all.map(_._1)}"))

          // Every `type X` qualifier in the import block must appear as a standalone
          // `type X` token, not as `X` without a qualifier. We verify that the two
          // known type-only names from BaboonSharedRuntime both carry their `type` prefix.
          assert(
            resultFile.contains("type BaboonAdtMemberMeta"),
            s"Expected 'type BaboonAdtMemberMeta' in import (BAB-T03).\n$resultFile",
          )
          assert(
            resultFile.contains("type BaboonGeneratedLatest"),
            s"Expected 'type BaboonGeneratedLatest' in import (BAB-T03).\n$resultFile",
          )
          // Negative check: the broken pattern has a bare (non-type-qualified) name
          // immediately after a comma in the type-only import.  We ensure there is no
          // import line where `BaboonGeneratedLatest` appears without a leading `type `.
          val importLines = resultFile.linesIterator.filter(_.startsWith("import ")).toList
          importLines.foreach { line =>
            if (line.contains("BaboonGeneratedLatest")) {
              assert(
                line.contains("type BaboonGeneratedLatest"),
                s"Found 'BaboonGeneratedLatest' without per-name 'type' qualifier (BAB-T03):\n  $line",
              )
            }
          }
        }
    }
  }
}
