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

/** Covers `--ts-bare-service-symbols=true`: service symbols are emitted bare
  * (`Service`/`Client`/`invokeJson`/`invokeUeba`/`JsonService`/`UebaService`),
  * relying on the per-service directory + barrel for namespacing, while
  * cross-file references (notably the package-level dispatcher, which pulls in
  * every service) import the bare export aliased back to the service-name form
  * so they never collide.
  *
  * The `case-ok` fixture has two services (`AdminService`, `ReportService`) in a
  * mixed-case package — the smallest multi-service shape that would surface a
  * bare-name collision or a dangling import in the dispatcher.
  */
final class TypeScriptBareServiceSymbolsTest extends TypeScriptBareServiceSymbolsTestBase[Either]

abstract class TypeScriptBareServiceSymbolsTestBase[F[+_, +_]: Error2: TagKK: BaboonTestModule] extends BaboonTest[F] {

  private val tsTarget: TsTarget = TsTarget(
    id = "TypeScript",
    output = OutputOptions(
      safeToRemoveExtensions = Set.empty,
      runtime                = RuntimeGenOpt.With,
      generateConversions    = true,
      output                 = FSPath.parse(NEString.unsafeFrom("./target/baboon-scalatests-ts-bare/")),
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
      bareServiceSymbols          = true,
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

  private val ImportFromRegex = """\bfrom\s*['\"]([^'\"]+)['\"]""".r

  private def resolveRelative(from: String, relative: String): Option[String] = {
    if (!(relative.startsWith("./") || relative.startsWith("../") || relative == "." || relative == "..")) {
      None
    } else {
      val fromSegs = from.split('/').dropRight(1).toList
      val relSegs  = relative.split('/').toList
      val resolved = relSegs.foldLeft[Option[List[String]]](Some(fromSegs)) {
        case (None, _)        => None
        case (Some(acc), ".") => Some(acc)
        case (Some(Nil), "..") => None
        case (Some(acc), "..") => Some(acc.init)
        case (Some(acc), seg) => Some(acc :+ seg)
      }
      resolved.map { segs =>
        val joined = segs.mkString("/")
        if (joined.endsWith(".ts")) joined else s"$joined.ts"
      }
    }
  }

  "case-ok fixture, TypeScript target with bare service symbols" should {

    "emit bare service symbols (Service/Client/invokeJson/JsonService)" in {
      (loader: BaboonLoader[F], translator: BaboonAbstractTranslator[F]) =>
        for {
          family <- loadCaseFamily(loader)
          srcs   <- translator.translate(family)
        } yield {
          val all = srcs.files.iterator.collect { case (p, of) if p.endsWith(".ts") => (p, of.content) }.toList
          def existsLine(pred: String => Boolean): Boolean = all.exists { case (_, c) => c.linesIterator.exists(pred) }

          assert(existsLine(_.trim.startsWith("export interface Service")), "expected a bare `export interface Service`")
          assert(existsLine(_.trim.startsWith("export class Client")), "expected a bare `export class Client`")
          // `export [async] function invokeJson(` — async prefix depends on asyncServices.
          assert(existsLine(l => l.contains("function invokeJson(")), "expected a bare `function invokeJson(`")
          assert(existsLine(_.trim.startsWith("export class JsonService")), "expected a bare `export class JsonService`")
          // No service-name-prefixed wiring functions should remain in a declaration.
          assert(!existsLine(_.contains("function invokeJson_")), "did not expect prefixed `invokeJson_<Service>` declaration")
        }
    }

    "alias bare wiring symbols per-service in the dispatcher (no collision)" in {
      (loader: BaboonLoader[F], translator: BaboonAbstractTranslator[F]) =>
        for {
          family <- loadCaseFamily(loader)
          srcs   <- translator.translate(family)
        } yield {
          val dispatcher = srcs.files.iterator
            .collect { case (p, of) if p.endsWith("baboon-dispatcher.ts") => of.content }
            .toList
            .headOption
            .getOrElse(fail("baboon-dispatcher.ts not emitted"))
          // Each service's bare `invokeJson`/`invokeUeba` is imported aliased to the prefixed name.
          assert(
            dispatcher.contains("invokeJson as invokeJson_") || dispatcher.contains("invokeUeba as invokeUeba_"),
            s"dispatcher must alias bare wiring fns per service.\n$dispatcher",
          )
        }
    }

    "emit imports that resolve to an emitted file" in {
      (loader: BaboonLoader[F], translator: BaboonAbstractTranslator[F]) =>
        for {
          family <- loadCaseFamily(loader)
          srcs   <- translator.translate(family)
        } yield {
          val tsFiles                   = srcs.files.iterator.collect { case (p, of) if p.endsWith(".ts") => (p, of.content) }.toList
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
          assert(unresolved.isEmpty, s"Imports pointing at non-existent files:\n${unresolved.mkString("\n")}")
        }
    }
  }
}
