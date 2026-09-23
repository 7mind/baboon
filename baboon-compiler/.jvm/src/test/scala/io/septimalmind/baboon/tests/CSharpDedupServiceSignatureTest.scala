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

/** With `deduplicate` on, a type whose schema is unchanged across versions is emitted once,
  * in the latest namespace, and the older namespaces reference that surviving twin.
  * Services are never deduplicated, so an unchanged service is re-emitted per version and
  * its method signature mixes both worlds: it must name the surviving twin for EVERY
  * referenced type. The return type used to be flattened to a fully-qualified string inside
  * `CSDefnTranslator`/`CSServiceWiringTranslator` before the renderer could rewrite it, so
  * the old-version interface declared `…v1_0_0.Svc.M.Out` — a type no file declares (CS0234)
  * — while the parameter, which stayed a tree, correctly pointed at the latest namespace.
  */
final class CSharpDedupServiceSignatureTest extends CSharpDedupServiceSignatureTestBase[Either]

abstract class CSharpDedupServiceSignatureTestBase[F[+_, +_]: Error2: TagKK: BaboonTestModule] extends BaboonTest[F] {

  private val csTarget: CSTarget = CSTarget(
    id = "C#",
    output = OutputOptions(
      safeToRemoveExtensions = Set.empty,
      runtime                = RuntimeGenOpt.With,
      generateConversions    = true,
      output                 = FSPath.parse(NEString.unsafeFrom("./target/baboon-cs-dedup-service-test/")),
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
      deduplicate                               = true,
      // Errors mode: the return type becomes `Either<$error, $success>`, which is the shape
      // that used to be assembled from pre-rendered strings.
      serviceResult = ServiceResultConfig(
        noErrors   = false,
        resultType = Some("Either"),
        pattern    = Some("<$error,$success>"),
        hkt        = None,
      ),
      serviceContext       = ServiceContextConfig.default,
      pragmas              = Map.empty,
      generateDomainFacade = false,
      asyncServices        = true,
      generateMcpServer    = false,
    ),
  )

  private val baseModule: distage.Module =
    new BaboonModuleJvm[Either](
      CompilerOptions(
        debug                    = false,
        individualInputs         = Set.empty,
        directoryInputs          = Set(FSPath.parse(NEString.unsafeFrom("./baboon-compiler/src/test/resources/dedup-service-ok"))),
        metaWriteEvolutionJsonTo = None,
        lockfile                 = Some(FSPath.parse(NEString.unsafeFrom("./target/baboon-dedup-service.lock"))),
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

  private def loadFamily(loader: BaboonLoader[F]): F[NEList[BaboonIssue], BaboonFamily] = {
    val root = IzResources
      .getPath("dedup-service-ok")
      .getOrElse(throw new AssertionError("dedup-service-ok fixture not found"))
      .asInstanceOf[IzResources.LoadablePathReference]
      .path
    val baboons =
      if (root.toFile.isDirectory)
        IzFiles.walk(root.toFile).toList.filter(p => p.toFile.isFile && p.toFile.getName.endsWith(".baboon"))
      else List(root)
    loader.load(baboons)
  }

  private val obsoleteNs = "Dedup.Service.v1_0_0"

  "dedup-service-ok fixture, C# target with deduplication" should {

    "not emit the deduplicated method I/O types into the obsolete version's namespace" in {
      (loader: BaboonLoader[F], translator: BaboonAbstractTranslator[F]) =>
        for {
          family <- loadFamily(loader)
          srcs   <- translator.translate(family)
        } yield {
          val declarations = srcs.files.iterator.collect {
            case (path, of) if of.content.contains(s"namespace $obsoleteNs") && of.content.contains("record Out") => path
          }.toList

          // Premise of the test: the emitter really does drop them from 1.0.0.
          assert(
            declarations.isEmpty,
            s"Expected the 1.0.0 method I/O types to be deduplicated away, but they are declared in: $declarations",
          )
        }
    }

    "reference the surviving latest-namespace twin from the obsolete service interface" in {
      (loader: BaboonLoader[F], translator: BaboonAbstractTranslator[F]) =>
        for {
          family <- loadFamily(loader)
          srcs   <- translator.translate(family)
        } yield {
          val all = srcs.files.iterator.map { case (path, of) => (path, of.content) }.toList

          val interface = all.collectFirst {
            case (_, c) if c.contains(s"namespace $obsoleteNs") && c.contains("interface ISvc") => c
          }.getOrElse(fail(s"Obsolete ISvc interface not found. Paths: ${all.map(_._1)}"))

          val signature = interface.linesIterator.find(_.contains(" M(")).getOrElse(
            fail(s"Method M not found in the obsolete ISvc interface.\n$interface")
          )

          assert(
            !signature.contains(s"$obsoleteNs.Svc.M."),
            s"The obsolete interface must not name method I/O types that were deduplicated away.\n$signature",
          )
          assert(
            signature.contains("Dedup.Service.Svc.M.Out") &&
              signature.contains("Dedup.Service.Svc.M.Err") &&
              signature.contains("Dedup.Service.Svc.M.In"),
            s"The obsolete interface must reference the surviving latest-namespace twins.\n$signature",
          )
        }
    }

    "reference the surviving latest-namespace twin from the obsolete wiring" in {
      (loader: BaboonLoader[F], translator: BaboonAbstractTranslator[F]) =>
        for {
          family <- loadFamily(loader)
          srcs   <- translator.translate(family)
        } yield {
          val all = srcs.files.iterator.map { case (path, of) => (path, of.content) }.toList

          val wiring = all.collectFirst {
            case (_, c) if c.contains(s"namespace $obsoleteNs") && c.contains("class SvcWiring") => c
          }.getOrElse(fail(s"Obsolete SvcWiring not found. Paths: ${all.map(_._1)}"))

          val declarations = wiring.linesIterator.filter(_.contains("> output;")).toList
          assert(declarations.nonEmpty, s"No wiring result declaration found.\n$wiring")
          assert(
            declarations.forall(!_.contains(s"$obsoleteNs.Svc.M.")),
            s"The obsolete wiring must not name method I/O types that were deduplicated away.\n${declarations.mkString("\n")}",
          )
        }
    }
  }
}
