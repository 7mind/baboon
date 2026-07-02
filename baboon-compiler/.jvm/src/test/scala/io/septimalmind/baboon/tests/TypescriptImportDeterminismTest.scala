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

/** T28 / D3 regression: the TypeScript import emitter must produce a total,
  * deterministic lexical ordering of import lines.
  *
  * Pre-fix `TsBaboonTranslator` ordered modules by the non-injective key
  * `moduleId.path.size + types.size` (ties fell back to hash-derived
  * immutable-HashMap iteration order) and rendered per-module symbols in
  * `groupBy` order with no sort. This test asserts:
  *
  *   1. module-line order within each generated file's import block is lexically
  *      sorted by the import source path;
  *   2. per-module symbol order inside each `import { ... }` brace list is
  *      lexically sorted by name;
  *   3. each emitted import block already equals its own canonical form — module
  *      lines sorted by the rendered `from '...'` source path, symbols sorted by
  *      base name within each brace list (a single-translation, non-vacuous
  *      replacement for the earlier two-run byte-identity check, which was
  *      vacuous because two same-JVM runs share HashMap iteration order).
  *
  * Reuses the `m30-sc-docs` fixture (model content is language-agnostic; it
  * yields files importing from several modules, exercising the module-ordering
  * codepath at `TsBaboonTranslator.scala` ~L461).
  */
final class TypescriptImportDeterminismTest extends TypescriptImportDeterminismTestBase[Either]

abstract class TypescriptImportDeterminismTestBase[F[+_, +_]: Error2: TagKK: BaboonTestModule] extends BaboonTest[F] {

  private val tsTarget: TsTarget = TsTarget(
    id = "TypeScript",
    output = OutputOptions(
      safeToRemoveExtensions = Set.empty,
      runtime                = RuntimeGenOpt.With,
      generateConversions    = true,
      output                 = FSPath.parse(NEString.unsafeFrom("./target/baboon-ts-import-determinism-test/")),
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

  /** The leading contiguous block of `import ...` lines at the top of a TS file. */
  private def importBlock(content: String): List[String] = {
    content.linesIterator.takeWhile(l => l.startsWith("import ") || l.isEmpty).filter(_.startsWith("import ")).toList
  }

  /** Extract the source-path string from an `... from 'PATH'` import line. */
  private def importSource(line: String): String = {
    val marker = "from '"
    val i      = line.indexOf(marker)
    if (i < 0) line
    else {
      val rest = line.substring(i + marker.length)
      val j    = rest.indexOf('\'')
      if (j < 0) rest else rest.substring(0, j)
    }
  }

  /** Extract the comma-separated symbol names from an `import { ... }` brace list,
    * stripping `type ` qualifiers and ` as Alias` suffixes so we compare base names.
    */
  private def importSymbols(line: String): List[String] = {
    val open  = line.indexOf('{')
    val close = line.indexOf('}')
    if (open < 0 || close < 0 || close < open) Nil
    else {
      line
        .substring(open + 1, close)
        .split(',')
        .toList
        .map(_.trim)
        .filter(_.nonEmpty)
        .map(_.stripPrefix("type "))
        .map(s => s.split(" as ").head.trim)
    }
  }

  /** Modules emitted as a hand-authored, fixed-order preamble (not part of the
    * model-deduced import set governed by the D3 fix). `generateDomainFacade`
    * prepends these three constant lines verbatim; their order is fixed and
    * deterministic by construction, so they are excluded from the lexical-order
    * assertion (the byte-identity test below still guards their determinism).
    */
  private val fixedPreambleSources: Set[String] = Set("BaboonSharedRuntime", "BaboonCodecsFacade")

  private def isPreamble(source: String): Boolean =
    fixedPreambleSources.exists(p => source == p || source.endsWith(s"/$p"))

  "TypeScript import emission" should {

    "produce lexically-sorted module-line and per-module symbol order (D3)" in {
      (loader: BaboonLoader[F], translator: BaboonAbstractTranslator[F]) =>
        for {
          family <- loadDocsFamily(loader)
          srcs   <- translator.translate(family)
        } yield {
          val all = srcs.files.iterator.map { case (path, of) => (path.toString, of.content) }.toList

          val filesWithImports = all.filter { case (_, c) => importBlock(c).nonEmpty }
          assert(
            filesWithImports.nonEmpty,
            s"Expected at least one generated TS file with an import block. Paths: ${all.map(_._1)}",
          )

          filesWithImports.foreach {
            case (path, content) =>
              val lines = importBlock(content)

              // (1) module-line order of the model-deduced imports is lexically
              // sorted by import source path. The fixed runtime preamble is excluded.
              val sources = lines.map(importSource).filterNot(isPreamble)
              assert(
                sources == sources.sorted,
                s"Import module-line order is not lexically sorted in $path:\n  actual: $sources\n  sorted: ${sources.sorted}\nFull block:\n${lines.mkString("\n")}",
              )

              // (2) per-module symbol order inside each brace list is lexically
              // sorted (again excluding the fixed runtime-preamble lines, whose
              // symbol order is hand-authored).
              lines.filterNot(l => isPreamble(importSource(l))).foreach { line =>
                val syms = importSymbols(line)
                assert(
                  syms == syms.sorted,
                  s"Per-module import symbol order is not lexically sorted in $path:\n  line: $line\n  actual: $syms\n  sorted: ${syms.sorted}",
                )
              }
          }
        }
    }

    // CRITICISM 2 fix: the previous "byte-identical across two translations"
    // sub-test was VACUOUS — two translations within one JVM share the same
    // deterministic immutable-HashMap iteration order, so they agree even on the
    // pre-fix source; it could not catch the cross-run/cross-JVM hash-derived
    // nondeterminism it claimed to guard. This replacement compares the emitted
    // import block against its OWN canonical form: module lines re-sorted by the
    // rendered `from '...'` source path and symbols re-sorted within each brace
    // list. It is LIVE — on the pre-fix source the module-line order
    // (`moduleId.path.size + types.size`, reversed) and the unsorted per-module
    // `groupBy` symbol order both diverge from this canonical form, so the
    // assertion fails; post-fix both match. NON-VACUOUS for BOTH the
    // module-order and the symbol-order properties.
    "emit each import block in canonical (rendered-path, then symbol-name) order (D3 determinism)" in {
      (loader: BaboonLoader[F], translator: BaboonAbstractTranslator[F]) =>
        for {
          family <- loadDocsFamily(loader)
          srcs   <- translator.translate(family)
        } yield {
          val filesWithImports = srcs.files.iterator
            .map { case (path, of) => (path.toString, of.content) }
            .filter { case (_, c) => importBlock(c).exists(l => !isPreamble(importSource(l))) }
            .toList

          assert(
            filesWithImports.nonEmpty,
            "Expected at least one generated TS file with a model-deduced (non-preamble) import block.",
          )

          // Canonical form of one model-deduced import line: ONLY the symbols
          // inside the brace list are re-sorted by base name. The surrounding text
          // (the `import {` prefix, the brace delimiters and exact spacing, and the
          // `} from '...'` suffix) is spliced back verbatim, so a correctly-ordered
          // line round-trips to itself byte-for-byte and the verbatim block
          // comparison below has no false positives from reformatting. Each
          // symbol's full `type `/` as Alias` text is preserved; only the sort key
          // strips those to the base name.
          def canonicalLine(line: String): String = {
            val open  = line.indexOf('{')
            val close = line.indexOf('}')
            if (open < 0 || close < 0 || close < open) line
            else {
              val inner = line.substring(open + 1, close)
              // preserve the brace's inner padding verbatim (the facade site emits
              // `{ a, b }` with surrounding spaces; the renderTree site emits
              // `{a, b}` with none) so a correctly-ordered line round-trips exactly.
              val leadPad  = inner.takeWhile(_.isWhitespace)
              val trailPad = inner.reverseIterator.takeWhile(_.isWhitespace).mkString
              val sortedRaw = inner
                .split(',')
                .toList
                .map(_.trim)
                .filter(_.nonEmpty)
                .sortBy(s => s.stripPrefix("type ").split(" as ").head.trim)
              line.substring(0, open + 1) + leadPad + sortedRaw.mkString(", ") + trailPad + line.substring(close)
            }
          }

          filesWithImports.foreach {
            case (path, content) =>
              // model-deduced lines only (drop the fixed runtime preamble, whose
              // order is hand-authored and out of scope for the D3 sort)
              val modelLines = importBlock(content).filterNot(l => isPreamble(importSource(l)))

              // canonical block: each line's symbols re-sorted by base name, then
              // the lines themselves re-sorted by the rendered `from '...'` source
              // path — the exact key the D3 fix sorts `typesByModule` by. Comparing
              // the ACTUAL emitted lines (verbatim) against this canonical block
              // surfaces BOTH a wrong module-line order AND an unsorted brace list,
              // because canonicalisation is applied only to the expected side.
              val canonical = modelLines
                .map(canonicalLine)
                .sortBy(importSource)

              assert(
                modelLines == canonical,
                s"Import block is not in canonical (rendered-path, then symbol-name) order in $path:\n  actual:\n    ${modelLines.mkString("\n    ")}\n  canonical:\n    ${canonical.mkString("\n    ")}",
              )
          }
        }
    }
  }
}
