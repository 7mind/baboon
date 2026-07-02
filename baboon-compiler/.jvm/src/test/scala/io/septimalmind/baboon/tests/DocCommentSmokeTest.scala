package io.septimalmind.baboon.tests

import distage.plugins.PluginBase
import io.septimalmind.baboon.*
import io.septimalmind.baboon.CompilerTarget.ScTarget
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

/** PR-30.15 cross-language smoke test: loads the comprehensive `m30-ok`
  * fixture (which exercises every doc position from spec §3.1) against the
  * Scala backend and asserts that every expected doc text appears in the
  * generated output.
  *
  * This is the belt-and-braces check for the m30-ok fixture. The per-language
  * toolchain tests (`mdl :test-*-regular`) are the load-bearing gate that
  * verifies actual compilation; this test verifies doc-text presence via
  * the JVM translation pipeline without invoking external toolchains.
  *
  * Doc positions exercised by m30-ok:
  *   - type-level doc on `data` (M30Item, M30Page, M30Entry, M30Req, M30Resp, M30Holder)
  *   - type-level doc on `adt`  (M30Result)
  *   - type-level doc on `enum` (M30Severity)
  *   - type-level doc on `contract` (M30Header)
  *   - type-level doc on `foreign` (M30Bytes)
  *   - type-level doc on `service` (M30Svc)
  *   - ADT arm-level docs on M30Ok and M30Err
  *   - field-prefix doc on M30Item.name, M30Item.price, M30Entry.severity, M30Entry.message
  *   - field-suffix `//!` doc on M30Item.price (merged with prefix)
  *   - method-level docs on M30Svc.create and M30Svc.fetch
  */
final class DocCommentSmokeTest extends DocCommentSmokeTestBase[Either]

abstract class DocCommentSmokeTestBase[F[+_, +_]: Error2: TagKK: BaboonTestModule] extends BaboonTest[F] {

  private val scTarget: ScTarget = ScTarget(
    id = "Scala",
    output = OutputOptions(
      safeToRemoveExtensions = Set.empty,
      runtime                = RuntimeGenOpt.With,
      generateConversions    = true,
      output                 = FSPath.parse(NEString.unsafeFrom("./target/baboon-m30-smoke-test/")),
      fixturesOutput         = None,
      testsOutput            = None,
    ),
    generic = GenericOptions(
      codecTestIterations = 0
    ),
    language = ScOptions(
      writeEvolutionDict          = false,
      wrappedAdtBranchCodecs      = false,
      enableDeprecatedEncoders    = false,
      generateJsonCodecs          = true,
      generateUebaCodecs          = true,
      generateJsonCodecsByDefault = true,
      generateUebaCodecsByDefault = true,
      serviceResult               = ServiceResultConfig.scalaDefault,
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
        lockfile                 = Some(FSPath.parse(NEString.unsafeFrom("./target/baboon.lock"))),
        emitOnly                 = None,
        targets                  = Seq(scTarget),
      ),
      UnsafeInstances.Lawless_ParallelErrorAccumulatingOpsEither,
    )

  private val translatorModule: distage.Module = new BaboonJvmScModule[Either](scTarget)
  private val combinedModule: distage.Module   = baseModule overriddenBy translatorModule

  override protected def config: TestConfig = super.config.copy(
    pluginConfig = PluginConfig.const(combinedModule.morph[PluginBase]),
    activation   = super.config.activation + BaboonModeAxis.Compiler,
  )

  private def loadSmokeFamily(loader: BaboonLoader[F]): F[NEList[BaboonIssue], BaboonFamily] = {
    val root = IzResources
      .getPath("baboon/m30-ok")
      .getOrElse(throw new AssertionError("m30-ok fixture not found"))
      .asInstanceOf[IzResources.LoadablePathReference]
      .path
    val baboons =
      if (root.toFile.isDirectory)
        IzFiles.walk(root.toFile).toList.filter(p => p.toFile.isFile && p.toFile.getName.endsWith(".baboon"))
      else List(root)
    loader.load(baboons)
  }

  "m30-ok smoke fixture, Scala target" should {

    "emit type-level doc on data declaration (M30Item)" in {
      (loader: BaboonLoader[F], translator: BaboonAbstractTranslator[F]) =>
        for {
          family <- loadSmokeFamily(loader)
          srcs   <- translator.translate(family)
        } yield {
          val all = srcs.files.iterator.map { case (_, of) => of.content }.toList.mkString("\n")
          assert(
            all.contains("A simple item DTO with mixed field docs"),
            s"Expected type-level doc for M30Item in Scala output.",
          )
        }
    }

    "emit field-prefix and field-suffix docs on M30Item.price (merged)" in {
      (loader: BaboonLoader[F], translator: BaboonAbstractTranslator[F]) =>
        for {
          family <- loadSmokeFamily(loader)
          srcs   <- translator.translate(family)
        } yield {
          val all = srcs.files.iterator.map { case (_, of) => of.content }.toList.mkString("\n")
          // Prefix doc on price
          assert(
            all.contains("Unit price in store currency"),
            s"Expected prefix doc on M30Item.price in Scala output.",
          )
          // Suffix doc on price (//! never negative) must be merged
          assert(
            all.contains("never negative"),
            s"Expected suffix doc (//! never negative) on M30Item.price in Scala output.",
          )
        }
    }

    "emit type-level doc on enum declaration (M30Severity)" in {
      (loader: BaboonLoader[F], translator: BaboonAbstractTranslator[F]) =>
        for {
          family <- loadSmokeFamily(loader)
          srcs   <- translator.translate(family)
        } yield {
          val all = srcs.files.iterator.map { case (_, of) => of.content }.toList.mkString("\n")
          assert(
            all.contains("Severity of a diagnostic entry"),
            s"Expected type-level doc for M30Severity in Scala output.",
          )
        }
    }

    "emit type-level doc and arm-level docs on adt declaration (M30Result)" in {
      (loader: BaboonLoader[F], translator: BaboonAbstractTranslator[F]) =>
        for {
          family <- loadSmokeFamily(loader)
          srcs   <- translator.translate(family)
        } yield {
          val all = srcs.files.iterator.map { case (_, of) => of.content }.toList.mkString("\n")
          // Type-level doc on M30Result
          assert(
            all.contains("Result or error ADT"),
            s"Expected type-level doc for M30Result in Scala output.",
          )
          // Arm-level doc on M30Ok
          assert(
            all.contains("Successful result arm"),
            s"Expected arm-level doc for M30Ok in Scala output.",
          )
          // Arm-level doc on M30Err
          assert(
            all.contains("Error arm"),
            s"Expected arm-level doc for M30Err in Scala output.",
          )
          // Field-level doc inside M30Ok arm
          assert(
            all.contains("The returned payload"),
            s"Expected field doc for M30Ok.value in Scala output.",
          )
        }
    }

    "emit type-level and method-level docs on service declaration (M30Svc)" in {
      (loader: BaboonLoader[F], translator: BaboonAbstractTranslator[F]) =>
        for {
          family <- loadSmokeFamily(loader)
          srcs   <- translator.translate(family)
        } yield {
          val all = srcs.files.iterator.map { case (_, of) => of.content }.toList.mkString("\n")
          // Type-level doc on M30Svc
          assert(
            all.contains("CRUD service for M30Item management"),
            s"Expected type-level doc for M30Svc in Scala output.",
          )
          // Method-level doc on create
          assert(
            all.contains("Create a new item"),
            s"Expected method doc for M30Svc.create in Scala output.",
          )
          // Method-level doc on fetch
          assert(
            all.contains("Fetch an item by id"),
            s"Expected method doc for M30Svc.fetch in Scala output.",
          )
        }
    }

    "emit type-level doc on contract declaration (M30Header)" in {
      (loader: BaboonLoader[F], translator: BaboonAbstractTranslator[F]) =>
        for {
          family <- loadSmokeFamily(loader)
          srcs   <- translator.translate(family)
        } yield {
          val all = srcs.files.iterator.map { case (_, of) => of.content }.toList.mkString("\n")
          assert(
            all.contains("shared header contract"),
            s"Expected type-level doc for M30Header (contract) in Scala output.",
          )
          // Field-level doc inside M30Header
          assert(
            all.contains("Correlation id for tracing"),
            s"Expected field doc for M30Header.traceId in Scala output.",
          )
        }
    }
  }
}
