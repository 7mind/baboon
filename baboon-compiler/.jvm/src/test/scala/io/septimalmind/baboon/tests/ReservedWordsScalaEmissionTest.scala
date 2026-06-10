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

/** T13 codegen-shape regression guard for the per-language reserved-word
  * escaping fixes (D1 / T3-T12).
  *
  * Runs the actual `ScDefnTranslator` pipeline against the `reserved-words-ok`
  * fixture (which lives INSIDE the shared `baboon` model-dir, so it is also
  * generated+compiled by the `test-gen-{regular,wrapped}-adt` matrix lanes with
  * codecs on — see `.mdl/defs/tests.md`) and asserts the emitted Scala source
  * escapes keyword-named DTO fields via backtick quoting while preserving the
  * original wire names in the JSON codec keys.
  *
  * The assertions pin three contracts that the T3-T12 fixes established:
  *   1. keyword fields collide with Scala reserved words → backtick-quoted at
  *      the declaration and at every property access (`value.`type``);
  *   2. fields whose names are NOT Scala keywords (`default`, `void`, `none`)
  *      stay bare — escaping must be language-specific, not blanket;
  *   3. the JSON wire key is the ORIGINAL identifier (`"type"`, `"class"`),
  *      decoupled from the escaped Scala property name (the codec-capture site).
  */
final class ReservedWordsScalaEmissionTest extends ReservedWordsScalaEmissionTestBase[Either]

abstract class ReservedWordsScalaEmissionTestBase[F[+_, +_]: Error2: TagKK: BaboonTestModule] extends BaboonTest[F] {

  private val scTarget: ScTarget = ScTarget(
    id = "Scala",
    output = OutputOptions(
      safeToRemoveExtensions = Set.empty,
      runtime                = RuntimeGenOpt.With,
      generateConversions    = true,
      output                 = FSPath.parse(NEString.unsafeFrom("./target/baboon-scalatests-reserved-sc/")),
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
        lockFile                 = Some(FSPath.parse(NEString.unsafeFrom("./target/baboon.lock"))),
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

  private def loadReservedFamily(loader: BaboonLoader[F]): F[NEList[BaboonIssue], BaboonFamily] = {
    val root = IzResources
      .getPath("baboon/reserved-words-ok/reserved.baboon")
      .getOrElse(throw new AssertionError("reserved-words-ok fixture not found"))
      .asInstanceOf[IzResources.LoadablePathReference]
      .path
    val baboons =
      if (root.toFile.isDirectory)
        IzFiles.walk(root.toFile).toList.filter(p => p.toFile.isFile && p.toFile.getName.endsWith(".baboon"))
      else List(root)
    loader.load(baboons)
  }

  "reserved-words-ok fixture, Scala target" should {

    "backtick-escape keyword-named DTO fields while keeping non-keyword names bare" in {
      (loader: BaboonLoader[F], translator: BaboonAbstractTranslator[F]) =>
        for {
          family <- loadReservedFamily(loader)
          srcs   <- translator.translate(family)
        } yield {
          val all = srcs.files.iterator.map { case (path, of) => (path, of.content) }.toList

          val holder = all.collectFirst {
            case (_, content) if content.contains("case class Holder(") => content
          }.getOrElse(fail(s"Holder case class not found. Paths: ${all.map(_._1)}"))

          // (1) Scala-keyword field names MUST be backtick-quoted at the declaration.
          for (kw <- List("class", "final", "val", "object", "def", "super", "import", "type", "true", "false")) {
            assert(
              holder.contains(s"`$kw`: String"),
              s"Expected backtick-escaped Scala-keyword field `$kw` in Holder declaration. Source:\n$holder",
            )
          }

          // (2) Field names that are NOT Scala keywords MUST stay bare (no over-escaping).
          for (nonKw <- List("default", "void", "none")) {
            assert(
              holder.contains(s"$nonKw: String"),
              s"Expected bare non-keyword field '$nonKw' in Holder declaration. Source:\n$holder",
            )
            assert(
              !holder.contains(s"`$nonKw`: String"),
              s"Non-keyword field '$nonKw' must NOT be backtick-escaped. Source:\n$holder",
            )
          }

          // (3) JSON wire key is the ORIGINAL identifier, decoupled from the escaped
          //     property access (codec-capture site exercised by codecs-on gen).
          assert(
            holder.contains("\"type\" -> Json.fromString(value.`type`)"),
            s"Expected JSON key 'type' bound to escaped property access value.`type`. Source:\n$holder",
          )
          assert(
            holder.contains("\"class\" -> Json.fromString(value.`class`)"),
            s"Expected JSON key 'class' bound to escaped property access value.`class`. Source:\n$holder",
          )
        }
    }

    "emit per-branch codecs for keyword-named ADT branches" in {
      (loader: BaboonLoader[F], translator: BaboonAbstractTranslator[F]) =>
        for {
          family <- loadReservedFamily(loader)
          srcs   <- translator.translate(family)
        } yield {
          val all = srcs.files.iterator.map { case (path, of) => (path, of.content) }.toList

          val avatar = all.collectFirst {
            case (_, content) if content.contains("sealed trait AvatarItem") => content
          }.getOrElse(fail(s"AvatarItem ADT not found. Paths: ${all.map(_._1)}"))

          // Keyword-named ADT branches (Type/Object/Class/When/Match/Is/In/Default)
          // are emitted as case classes with their own JSON + UEBA codec objects.
          for (branch <- List("Default", "Type", "Object", "Class", "When", "Match", "Is", "In")) {
            assert(
              avatar.contains(s"final case class $branch("),
              s"Expected keyword-named ADT branch case class '$branch'. Source:\n$avatar",
            )
            assert(
              avatar.contains(s"object ${branch}_JsonCodec"),
              s"Expected JSON codec object for ADT branch '$branch'. Source:\n$avatar",
            )
          }
        }
    }
  }
}
