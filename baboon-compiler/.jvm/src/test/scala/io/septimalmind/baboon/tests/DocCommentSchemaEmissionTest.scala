package io.septimalmind.baboon.tests

import distage.plugins.PluginBase
import io.septimalmind.baboon.*
import io.septimalmind.baboon.CompilerTarget.{GqlTarget, OasTarget}
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

/** PR-30.13 emission test: verifies that `GqlBaboonTranslator` emits GraphQL
  * block-string `"""…"""` descriptions for doc-bearing types and fields, and
  * that `OasBaboonTranslator` emits `"description"` JSON Schema keys for
  * doc-bearing component schemas and properties.
  *
  * Both halves share the `m30-sc-docs` fixture (language-agnostic model) and
  * assert per `docs/spec/docstrings.md` §7.10 (GraphQL) and §7.11 (OpenAPI).
  */
final class DocCommentGraphqlEmissionTest extends DocCommentGraphqlEmissionTestBase[Either]

abstract class DocCommentGraphqlEmissionTestBase[F[+_, +_]: Error2: TagKK: BaboonTestModule] extends BaboonTest[F] {

  private val gqlTarget: GqlTarget = GqlTarget(
    id = "GraphQL",
    output = OutputOptions(
      safeToRemoveExtensions = Set.empty,
      runtime                = RuntimeGenOpt.With,
      generateConversions    = false,
      output                 = FSPath.parse(NEString.unsafeFrom("./target/baboon-gql-docs-test/")),
      fixturesOutput         = None,
      testsOutput            = None,
    ),
    generic = GenericOptions(
      codecTestIterations = 0
    ),
    language = GqlOptions(
      pragmas = Map.empty,
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
        targets                  = Seq(gqlTarget),
      ),
      UnsafeInstances.Lawless_ParallelErrorAccumulatingOpsEither,
    )

  private val translatorModule: distage.Module = new BaboonJvmGqlModule[Either](gqlTarget)
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

  "m30-sc-docs fixture, GraphQL target" should {

    "emit block-string description before type with type-level doc (spec §7.10)" in {
      (loader: BaboonLoader[F], translator: BaboonAbstractTranslator[F]) =>
        for {
          family <- loadDocsFamily(loader)
          srcs   <- translator.translate(family)
        } yield {
          val all = srcs.files.iterator.map { case (path, of) => (path, of.content) }.toList

          val schemaFile = all.collectFirst {
            case (_, c) if c.contains("type m30_sc_docs_DocItem") => c
          }.getOrElse(fail(s"DocItem not found in GraphQL output. Paths: ${all.map(_._1)}"))

          // Type-level block-string before `type m30_sc_docs_DocItem`
          assert(
            schemaFile.contains("\"A simple item with field-level docs.\""),
            s"Expected type-level block-string description before DocItem.\n$schemaFile",
          )
        }
    }

    "emit block-string description before field with field-level doc (spec §7.10)" in {
      (loader: BaboonLoader[F], translator: BaboonAbstractTranslator[F]) =>
        for {
          family <- loadDocsFamily(loader)
          srcs   <- translator.translate(family)
        } yield {
          val all = srcs.files.iterator.map { case (path, of) => (path, of.content) }.toList

          val schemaFile = all.collectFirst {
            case (_, c) if c.contains("type m30_sc_docs_DocItem") => c
          }.getOrElse(fail(s"DocItem not found in GraphQL output. Paths: ${all.map(_._1)}"))

          // Field-level block-string before `name`
          assert(
            schemaFile.contains("\"Display name of the item.\""),
            s"Expected field block-string description for DocItem.name.\n$schemaFile",
          )
          // price has both prefix and suffix docs — merged together
          assert(
            schemaFile.contains("Unit price in store currency."),
            s"Expected prefix part of merged description for DocItem.price.\n$schemaFile",
          )
          assert(
            schemaFile.contains("never negative"),
            s"Expected suffix part of merged description for DocItem.price.\n$schemaFile",
          )
        }
    }

    "emit block-string description before enum type (spec §7.10)" in {
      (loader: BaboonLoader[F], translator: BaboonAbstractTranslator[F]) =>
        for {
          family <- loadDocsFamily(loader)
          srcs   <- translator.translate(family)
        } yield {
          val all = srcs.files.iterator.map { case (path, of) => (path, of.content) }.toList

          val schemaFile = all.collectFirst {
            case (_, c) if c.contains("enum m30_sc_docs_DocSeverity") => c
          }.getOrElse(fail(s"DocSeverity not found in GraphQL output. Paths: ${all.map(_._1)}"))

          assert(
            schemaFile.contains("\"Severity levels.\""),
            s"Expected type-level block-string description before DocSeverity.\n$schemaFile",
          )
        }
    }

    "emit block-string description before ADT arm type (spec §7.10)" in {
      (loader: BaboonLoader[F], translator: BaboonAbstractTranslator[F]) =>
        for {
          family <- loadDocsFamily(loader)
          srcs   <- translator.translate(family)
        } yield {
          val all = srcs.files.iterator.map { case (path, of) => (path, of.content) }.toList

          val schemaFile = all.collectFirst {
            case (_, c) if c.contains("type m30_sc_docs_DocResult_DocOk") => c
          }.getOrElse(fail(s"DocOk ADT arm not found in GraphQL output. Paths: ${all.map(_._1)}"))

          assert(
            schemaFile.contains("\"Successful payload variant.\""),
            s"Expected arm-level block-string description before DocOk.\n$schemaFile",
          )
          assert(
            schemaFile.contains("\"the carried payload\""),
            s"Expected field block-string description for DocOk.value.\n$schemaFile",
          )
        }
    }

    "emit no spurious descriptions for types without docs (no churn)" in {
      (loader: BaboonLoader[F], translator: BaboonAbstractTranslator[F]) =>
        for {
          family <- loadDocsFamily(loader)
          srcs   <- translator.translate(family)
        } yield {
          val all = srcs.files.iterator.map { case (path, of) => (path, of.content) }.toList

          val schemaFile = all.collectFirst {
            case (_, c) if c.contains("type m30_sc_docs_DocPage") => c
          }.getOrElse(fail(s"DocPage not found in GraphQL output. Paths: ${all.map(_._1)}"))

          // DocPage has a type-level doc; its fields (items, total) have none.
          assert(
            schemaFile.contains("\"Paged doc results.\""),
            s"Expected type-level block-string description before DocPage.\n$schemaFile",
          )
          // Fields without docs must not have empty block-string descriptions.
          assert(
            !schemaFile.contains("\"\"\"\n  items:"),
            s"Unexpected empty block-string before items field in DocPage.\n$schemaFile",
          )
          assert(
            !schemaFile.contains("\"\"\"\"\"\""),
            s"Unexpected empty single/block-string description in DocPage.\n$schemaFile",
          )
        }
    }
  }
}

final class DocCommentOpenapiEmissionTest extends DocCommentOpenapiEmissionTestBase[Either]

abstract class DocCommentOpenapiEmissionTestBase[F[+_, +_]: Error2: TagKK: BaboonTestModule] extends BaboonTest[F] {

  private val oasTarget: OasTarget = OasTarget(
    id = "OpenAPI",
    output = OutputOptions(
      safeToRemoveExtensions = Set.empty,
      runtime                = RuntimeGenOpt.With,
      generateConversions    = false,
      output                 = FSPath.parse(NEString.unsafeFrom("./target/baboon-oas-docs-test/")),
      fixturesOutput         = None,
      testsOutput            = None,
    ),
    generic = GenericOptions(
      codecTestIterations = 0
    ),
    language = OasOptions(
      pragmas = Map.empty,
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
        targets                  = Seq(oasTarget),
      ),
      UnsafeInstances.Lawless_ParallelErrorAccumulatingOpsEither,
    )

  private val translatorModule: distage.Module = new BaboonJvmOasModule[Either](oasTarget)
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

  "m30-sc-docs fixture, OpenAPI target" should {

    "emit description key on component schema for type with type-level doc (spec §7.11)" in {
      (loader: BaboonLoader[F], translator: BaboonAbstractTranslator[F]) =>
        for {
          family <- loadDocsFamily(loader)
          srcs   <- translator.translate(family)
        } yield {
          val all = srcs.files.iterator.map { case (path, of) => (path, of.content) }.toList

          val schemaFile = all.collectFirst {
            case (_, c) if c.contains("m30_sc_docs_DocItem") => c
          }.getOrElse(fail(s"DocItem schema not found in OpenAPI output. Paths: ${all.map(_._1)}"))

          // Type-level description on DocItem component
          assert(
            schemaFile.contains("\"description\": \"A simple item with field-level docs.\""),
            s"Expected type-level description on DocItem schema.\n$schemaFile",
          )
        }
    }

    "emit description key on property schema for field with field-level doc (spec §7.11)" in {
      (loader: BaboonLoader[F], translator: BaboonAbstractTranslator[F]) =>
        for {
          family <- loadDocsFamily(loader)
          srcs   <- translator.translate(family)
        } yield {
          val all = srcs.files.iterator.map { case (path, of) => (path, of.content) }.toList

          val schemaFile = all.collectFirst {
            case (_, c) if c.contains("m30_sc_docs_DocItem") => c
          }.getOrElse(fail(s"DocItem schema not found in OpenAPI output. Paths: ${all.map(_._1)}"))

          // Field-level description on DocItem.name property
          assert(
            schemaFile.contains("\"description\": \"Display name of the item.\""),
            s"Expected field description on DocItem.name property.\n$schemaFile",
          )
          // price has both prefix and suffix docs
          assert(
            schemaFile.contains("Unit price in store currency."),
            s"Expected prefix part of merged description for DocItem.price.\n$schemaFile",
          )
          assert(
            schemaFile.contains("never negative"),
            s"Expected suffix part of merged description for DocItem.price.\n$schemaFile",
          )
        }
    }

    "emit description key on enum component schema (spec §7.11)" in {
      (loader: BaboonLoader[F], translator: BaboonAbstractTranslator[F]) =>
        for {
          family <- loadDocsFamily(loader)
          srcs   <- translator.translate(family)
        } yield {
          val all = srcs.files.iterator.map { case (path, of) => (path, of.content) }.toList

          val schemaFile = all.collectFirst {
            case (_, c) if c.contains("m30_sc_docs_DocSeverity") => c
          }.getOrElse(fail(s"DocSeverity schema not found in OpenAPI output. Paths: ${all.map(_._1)}"))

          assert(
            schemaFile.contains("\"description\": \"Severity levels.\""),
            s"Expected type-level description on DocSeverity schema.\n$schemaFile",
          )
        }
    }

    "emit description key on ADT branch component schema (spec §7.11)" in {
      (loader: BaboonLoader[F], translator: BaboonAbstractTranslator[F]) =>
        for {
          family <- loadDocsFamily(loader)
          srcs   <- translator.translate(family)
        } yield {
          val all = srcs.files.iterator.map { case (path, of) => (path, of.content) }.toList

          val schemaFile = all.collectFirst {
            case (_, c) if c.contains("m30_sc_docs_DocResult_DocOk") => c
          }.getOrElse(fail(s"DocOk ADT arm schema not found in OpenAPI output. Paths: ${all.map(_._1)}"))

          assert(
            schemaFile.contains("\"description\": \"Successful payload variant.\""),
            s"Expected arm-level description on DocOk schema.\n$schemaFile",
          )
          assert(
            schemaFile.contains("\"description\": \"the carried payload\""),
            s"Expected field description on DocOk.value property.\n$schemaFile",
          )
        }
    }

    "emit no spurious description keys for types without docs (no churn)" in {
      (loader: BaboonLoader[F], translator: BaboonAbstractTranslator[F]) =>
        for {
          family <- loadDocsFamily(loader)
          srcs   <- translator.translate(family)
        } yield {
          val all = srcs.files.iterator.map { case (path, of) => (path, of.content) }.toList

          val schemaFile = all.collectFirst {
            case (_, c) if c.contains("m30_sc_docs_DocPage") => c
          }.getOrElse(fail(s"DocPage schema not found in OpenAPI output. Paths: ${all.map(_._1)}"))

          // DocPage has a type-level doc; its fields (items, total) have none.
          assert(
            schemaFile.contains("\"description\": \"Paged doc results.\""),
            s"Expected type-level description on DocPage schema.\n$schemaFile",
          )
          // Fields without docs must not produce `"description": ""`
          assert(
            !schemaFile.contains("\"description\": \"\","),
            s"Unexpected empty description on a field in DocPage schema.\n$schemaFile",
          )
        }
    }
  }
}
