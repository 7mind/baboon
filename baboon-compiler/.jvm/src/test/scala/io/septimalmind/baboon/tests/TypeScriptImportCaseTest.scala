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

/** Regression test for the TS translator covering several import-emission
  * defects that historically slipped through CI because all in-tree `.baboon`
  * fixtures used lowercase package names and shallow shapes:
  *
  *   1. Case mismatch between on-disk paths (lowercased by `TsFileTools.basename`)
  *      and emitted import paths (case-preserved). `tsc`/`deno` reject with
  *      "differs from already included file name ... only in casing".
  *   2. Generated domain facade omits the `Owner.Ns` namespace prefix from
  *      import paths and the local binding aliases — service-method
  *      `In`/`Out`/`Err` synthetic types from multiple services end up
  *      imported from non-existent `./In`/`./Out`/`./Err` modules and
  *      collide on the bare name (TS2307 + TS2300).
  *   3. Facade references `${T}.BaboonTypeIdentifier` for every type kind,
  *      but TS `enum`s cannot carry static properties — only the generated
  *      `${T}_JsonCodec`/`${T}_UEBACodec` classes do. TS2339 on any enum
  *      pulled into the facade.
  *
  * The fixture under `baboon/case-ok` is the smallest shape that exercises
  * all three: mixed-case package, two services with colliding `In`/`Out`/`Err`
  * synthetic types under different namespaces, and a reachable top-level enum.
  */
final class TypeScriptImportCaseTest extends TypeScriptImportCaseTestBase[Either]

abstract class TypeScriptImportCaseTestBase[F[+_, +_]: Error2: TagKK: BaboonTestModule] extends BaboonTest[F] {

  private val tsTarget: TsTarget = TsTarget(
    id = "TypeScript",
    output = OutputOptions(
      safeToRemoveExtensions = Set.empty,
      runtime                = RuntimeGenOpt.With,
      generateConversions    = true,
      output                 = FSPath.parse(NEString.unsafeFrom("./target/baboon-scalatests-ts-case/")),
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
      asyncServices               = true,
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

  private def loadCaseFamily(loader: BaboonLoader[F]): F[NEList[BaboonIssue], BaboonFamily] = {
    val root = IzResources
      .getPath("baboon/case-ok")
      .getOrElse(throw new AssertionError("case-ok fixture not found"))
      .asInstanceOf[IzResources.LoadablePathReference]
      .path
    val baboons =
      if (root.toFile.isDirectory)
        IzFiles.walk(root.toFile).toList.filter(p => p.toFile.isFile && p.toFile.getName.endsWith(".baboon"))
      else List(root)
    loader.load(baboons)
  }

  // Captures the path part of an emitted ES-module `from '...'` clause
  // (single or double quoted). Restricted to `from` to avoid matching string
  // literals that happen to follow the bare word `import` elsewhere.
  private val ImportFromRegex = """\bfrom\s*['\"]([^'\"]+)['\"]""".r

  // Resolve `relative` against `from` (an emitted-file path) using ES-module
  // semantics: split, walk `.`/`..`, then append `.ts` if not already there.
  // Returns None if the relative ref escapes the emission root (`..` past root).
  private def resolveRelative(from: String, relative: String): Option[String] = {
    if (!(relative.startsWith("./") || relative.startsWith("../") || relative == "." || relative == "..")) {
      None // non-relative — treat as external/bare-module ref, not our concern
    } else {
      val fromSegs = from.split('/').dropRight(1).toList
      val relSegs  = relative.split('/').toList
      val resolved = relSegs.foldLeft[Option[List[String]]](Some(fromSegs)) {
        case (None, _)            => None
        case (Some(acc), ".")     => Some(acc)
        case (Some(Nil), "..")    => None
        case (Some(acc), "..")    => Some(acc.init)
        case (Some(acc), seg)     => Some(acc :+ seg)
      }
      resolved.map { segs =>
        val joined = segs.mkString("/")
        if (joined.endsWith(".ts")) joined else s"$joined.ts"
      }
    }
  }

  "case-ok fixture, TypeScript target" should {

    "emit only lowercase package-path segments in all import paths" in {
      (loader: BaboonLoader[F], translator: BaboonAbstractTranslator[F]) =>
        for {
          family <- loadCaseFamily(loader)
          srcs   <- translator.translate(family)
        } yield {
          val tsFiles = srcs.files.iterator
            .collect { case (path, of) if path.endsWith(".ts") => (path, of.content) }
            .toList

          assert(tsFiles.nonEmpty, "no TS files emitted")

          // The package was declared `model MixedCase.Pkg`. Any import that
          // uses `MixedCase` or `Pkg` as a *directory segment* (rather than the
          // lowercased `mixedcase`/`pkg` used for the on-disk layout) is the
          // bug: file emission and import emission disagree. Filename basenames
          // (e.g. `DomainMixedCasePkgFacade`) are intentionally CamelCase and
          // are not checked.
          val PkgSegments: Set[String] = Set("MixedCase", "Pkg")
          val offenders = tsFiles.flatMap {
            case (path, content) =>
              ImportFromRegex
                .findAllMatchIn(content)
                .map(_.group(1))
                .filter { p =>
                  val segs = p.split('/').toList
                  // drop basename — only directory segments matter
                  val dirSegs = if (segs.nonEmpty) segs.init else Nil
                  dirSegs.exists(PkgSegments.contains)
                }
                .map(p => s"$path: import '$p'")
          }

          assert(
            offenders.isEmpty,
            s"Imports with non-lowercased package-path segments (file vs import case mismatch):\n${offenders.mkString("\n")}",
          )

          // Sanity: at least one emitted file lives under the lowercased path.
          assert(
            tsFiles.exists { case (p, _) => p.contains("mixedcase/pkg/") },
            s"Expected at least one file under mixedcase/pkg/. Paths: ${tsFiles.map(_._1)}",
          )
        }
    }

    "emit imports that resolve to an emitted file" in {
      (loader: BaboonLoader[F], translator: BaboonAbstractTranslator[F]) =>
        for {
          family <- loadCaseFamily(loader)
          srcs   <- translator.translate(family)
        } yield {
          val tsFiles = srcs.files.iterator
            .collect { case (path, of) if path.endsWith(".ts") => (path, of.content) }
            .toList
          val emittedPaths: Set[String] = tsFiles.map(_._1).toSet

          val unresolved = tsFiles.flatMap {
            case (path, content) =>
              ImportFromRegex
                .findAllMatchIn(content)
                .map(_.group(1))
                .flatMap(rel => resolveRelative(path, rel).map(target => (rel, target)))
                .filterNot { case (_, target) => emittedPaths.contains(target) }
                .map { case (rel, target) => s"$path: import from '$rel' resolves to '$target' which is not in the emitted set" }
          }

          assert(
            unresolved.isEmpty,
            s"Imports pointing at non-existent files:\n${unresolved.mkString("\n")}",
          )
        }
    }

    "facade registers codec identifiers via the codec class (not the type class)" in {
      (loader: BaboonLoader[F], translator: BaboonAbstractTranslator[F]) =>
        for {
          family <- loadCaseFamily(loader)
          srcs   <- translator.translate(family)
        } yield {
          val facade = srcs.files.iterator
            .collect { case (path, of) if path.endsWith("Facade.ts") && path.contains("Domain") => (path, of.content) }
            .toList

          assert(facade.nonEmpty, "no domain facade file emitted")

          // The facade must reference `${T}_JsonCodec.BaboonTypeIdentifier` /
          // `${T}_UEBACodec.BaboonTypeIdentifier`. A raw `${T}.BaboonTypeIdentifier`
          // call (without the `_JsonCodec`/`_UEBACodec` suffix preceding it) breaks
          // for enums, which cannot carry static properties.
          val RawIdRegex = """\b(\w+)\.BaboonTypeIdentifier\b""".r
          val offenders = facade.flatMap {
            case (path, content) =>
              RawIdRegex
                .findAllMatchIn(content)
                .map(_.group(1))
                .filterNot(sym => sym.endsWith("_JsonCodec") || sym.endsWith("_UEBACodec"))
                .map(sym => s"$path: $sym.BaboonTypeIdentifier (must go through *_JsonCodec or *_UEBACodec)")
                .toList
          }

          assert(
            offenders.isEmpty,
            s"Facade references BaboonTypeIdentifier on a non-codec symbol (breaks for enums):\n${offenders.mkString("\n")}",
          )

          // Sanity: the facade must register EnvSelector (the reachable top-level
          // enum) — if it doesn't, this test would silently pass even if the bug
          // returned, because no enum would exercise the codepath.
          assert(
            facade.exists { case (_, c) => c.contains("EnvSelector_JsonCodec.BaboonTypeIdentifier") || c.contains("EnvSelector_UEBACodec.BaboonTypeIdentifier") },
            "Facade does not register EnvSelector — fixture no longer exercises the enum path",
          )
        }
    }

    "facade emits no duplicate import bindings" in {
      (loader: BaboonLoader[F], translator: BaboonAbstractTranslator[F]) =>
        for {
          family <- loadCaseFamily(loader)
          srcs   <- translator.translate(family)
        } yield {
          val facade = srcs.files.iterator
            .collect { case (path, of) if path.endsWith("Facade.ts") && path.contains("Domain") => (path, of.content) }
            .toList

          // An `import { In, ... } from './X'` declares the local bindings
          // `In`, `...`. Two such imports introducing the same local binding
          // produces TS2300 "Duplicate identifier". Scan the import header of
          // every facade and reject duplicates of the locally-introduced names.
          val ImportBindingsRegex = """import\s*\{([^}]+)\}\s*from""".r
          val duplicates = facade.flatMap {
            case (path, content) =>
              val locals: List[String] = ImportBindingsRegex
                .findAllMatchIn(content)
                .flatMap(_.group(1).split(',').toList)
                .map { binding =>
                  // `Foo as Bar` → local binding is `Bar`; plain `Foo` → `Foo`.
                  val trimmed = binding.trim
                  val asIdx   = trimmed.indexOf(" as ")
                  if (asIdx >= 0) trimmed.substring(asIdx + 4).trim else trimmed
                }
                .toList
              locals.groupBy(identity).collect { case (k, vs) if vs.size > 1 => s"$path: '$k' (×${vs.size})" }
          }

          assert(
            duplicates.isEmpty,
            s"Facade emits duplicate local import bindings:\n${duplicates.mkString("\n")}",
          )
        }
    }
  }
}
