package io.septimalmind.baboon.tests

import distage.plugins.PluginBase
import distage.{Activation, Injector}
import io.septimalmind.baboon.*
import io.septimalmind.baboon.CompilerTarget.*
import io.septimalmind.baboon.parser.model.FSPath
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.tests.BaboonTest.BaboonTestModule
import io.septimalmind.baboon.translator.BaboonAbstractTranslator
import io.septimalmind.baboon.typer.model.BaboonFamily
import io.septimalmind.baboon.util.BaboonMetagen
import izumi.distage.modules.support.unsafe.EitherSupport
import izumi.distage.plugins.PluginConfig
import izumi.distage.testkit.model.TestConfig
import izumi.functional.bio.Error2
import izumi.functional.bio.unsafe.UnsafeInstances
import izumi.fundamentals.collections.nonempty.{NEList, NEString}
import izumi.fundamentals.platform.files.IzFiles
import izumi.fundamentals.platform.resources.IzResources
import izumi.reflect.TagKK

/** T165 (G27): the OPERATIONAL Q43 proof that keep/drop is pure sugar.
  *
  * Where T162 proves IR-equality at the raw/typed level, THIS acceptance test proves
  * delta-vs-explicit equivalence END-TO-END at the GENERATED-OUTPUT + EVOLUTION level,
  * across all 9 backends. It compiles a 2-version model in BOTH forms:
  *
  *   - DELTA form:    v2 = `import "1.0.0" { * }` + `root adt T4_A1 { keep B1, B2; drop B3; data B4 {…} }`
  *   - EXPLICIT twin: v2 = `import "1.0.0" { * }` + `root adt T4_A1 { data B1 {…}; data B2 {…}; data B4 {…} }`
  *
  * and asserts, per backend:
  *   (1) BYTE-IDENTICAL generated output trees (same file paths, same file bytes), and
  *   (2) IDENTICAL evolution metadata (the `--meta-write-evolution-json` payload — the auto-derived
  *       evolution rules/conversions), independent of backend.
  *
  * A WILDCARD variant additionally covers the motivating "add one field to one branch" case:
  * `keep *` retaining all prior branches with a single in-place redefinition, diffed against its
  * explicit whole-ADT re-declaration twin.
  *
  * Fixtures live OUTSIDE the shared `baboon/` model-dir (per the D9 isolation note in CLAUDE.md):
  * `baboon-compiler/src/test/resources/adt-delta-twin-ok/{delta,explicit,wildcard-delta,wildcard-explicit}/`.
  */
final class T165AdtDeltaTwinAcceptanceTest extends T165AdtDeltaTwinAcceptanceTestBase[Either]

abstract class T165AdtDeltaTwinAcceptanceTestBase[F[+_, +_]: Error2: TagKK: BaboonTestModule] extends BaboonTest[F] {

  // The 9 code-emitting backends (schema-only GraphQL/OpenAPI excluded — they emit no codecs/conversions
  // and are not part of the "9 backends" the task enumerates).
  private val backendLabels: Seq[String] = Seq("cs", "scala", "py", "rs", "ts", "kt", "jv", "dt", "sw")

  private val dummyOut: OutputOptions = OutputOptions(
    safeToRemoveExtensions = Set.empty,
    runtime                = RuntimeGenOpt.With,
    generateConversions    = true,
    output                 = FSPath.parse(NEString.unsafeFrom("./target/baboon-t165/")),
    fixturesOutput         = None,
    testsOutput            = None,
  )
  private val generic: GenericOptions = GenericOptions(codecTestIterations = 0)

  /** A (CompilerTarget, per-language JVM transpiler module) pair for the given backend label. */
  private def targetAndModule(label: String): (CompilerTarget, distage.Module) = label match {
    case "cs" =>
      val t = CSTarget(
        "C#", dummyOut, generic,
        CSOptions(
          obsoleteErrors = false, omitMostRecentVersionSuffixFromPaths = true, omitMostRecentVersionSuffixFromNamespaces = true,
          wrappedAdtBranchCodecs = false, writeEvolutionDict = false, disregardImplicitUsings = true, enableDeprecatedEncoders = false,
          generateIndexWriters = true, generateJsonCodecs = true, generateUebaCodecs = true, generateUebaCodecsByDefault = true,
          generateJsonCodecsByDefault = true, deduplicate = false, generateDomainFacade = false,
          serviceResult = ServiceResultConfig.csDefault, serviceContext = ServiceContextConfig.default, pragmas = Map.empty,
          asyncServices = false, generateMcpServer = false,
        ),
      )
      (t, new BaboonJvmCSModule[F](t))
    case "scala" =>
      val t = ScTarget(
        "Scala", dummyOut, generic,
        ScOptions(
          writeEvolutionDict = false, wrappedAdtBranchCodecs = false, enableDeprecatedEncoders = false,
          generateJsonCodecs = true, generateUebaCodecs = true, generateUebaCodecsByDefault = true, generateJsonCodecsByDefault = true,
          generateDomainFacade = false, serviceResult = ServiceResultConfig.scalaDefault, serviceContext = ServiceContextConfig.default,
          pragmas = Map.empty, generateMcpServer = false,
        ),
      )
      (t, new BaboonJvmScModule[F](t))
    case "py" =>
      val t = PyTarget(
        "Python", dummyOut, generic,
        PyOptions(
          writeEvolutionDict = true, wrappedAdtBranchCodecs = false, generateJsonCodecs = true, generateUebaCodecs = true,
          generateUebaCodecsByDefault = true, generateJsonCodecsByDefault = true, enableDeprecatedEncoders = false,
          generateDomainFacade = false, serviceResult = ServiceResultConfig.pythonDefault, serviceContext = ServiceContextConfig.default,
          pragmas = Map.empty, asyncServices = false, generateMcpServer = false,
        ),
      )
      (t, new BaboonJvmPyModule[F](t))
    case "rs" =>
      val t = RsTarget(
        "Rust", dummyOut, generic,
        RsOptions(
          writeEvolutionDict = false, wrappedAdtBranchCodecs = false, generateJsonCodecs = true, generateUebaCodecs = true,
          generateUebaCodecsByDefault = true, generateJsonCodecsByDefault = true, generateDomainFacade = false,
          serviceResult = ServiceResultConfig.rustDefault, serviceContext = ServiceContextConfig.default, pragmas = Map.empty,
          asyncServices = false, cratePrefix = "crate", reexportMode = "selective", edition = "2024", generateMcpServer = false,
        ),
      )
      (t, new BaboonJvmRsModule[F](t))
    case "ts" =>
      val t = TsTarget(
        "TypeScript", dummyOut, generic,
        TsOptions(
          writeEvolutionDict = false, wrappedAdtBranchCodecs = false, importSuffix = "", generateJsonCodecs = true, generateUebaCodecs = true,
          generateUebaCodecsByDefault = true, generateJsonCodecsByDefault = true, generateDomainFacade = false,
          serviceResult = ServiceResultConfig.typescriptDefault, serviceContext = ServiceContextConfig.default, pragmas = Map.empty,
          asyncServices = false, bareServiceSymbols = false, mapsAsRecords = false, timestampsUtcMode = "wrapper",
          timestampsOffsetMode = "wrapper", enumLowercaseValues = false, generateMcpServer = false,
        ),
      )
      (t, new BaboonJvmTsModule[F](t))
    case "kt" =>
      val t = KtTarget(
        "Kotlin", dummyOut, generic,
        KtOptions(
          writeEvolutionDict = false, wrappedAdtBranchCodecs = false, enableDeprecatedEncoders = false, generateJsonCodecs = true,
          generateUebaCodecs = true, generateUebaCodecsByDefault = true, generateJsonCodecsByDefault = true, generateDomainFacade = false,
          serviceResult = ServiceResultConfig.kotlinDefault, serviceContext = ServiceContextConfig.default, pragmas = Map.empty,
          asyncServices = false, multiplatform = false, generateMcpServer = false,
        ),
      )
      (t, new BaboonJvmKtModule[F](t))
    case "jv" =>
      val t = JvTarget(
        "Java", dummyOut, generic,
        JvOptions(
          writeEvolutionDict = false, wrappedAdtBranchCodecs = false, enableDeprecatedEncoders = false, generateJsonCodecs = true,
          generateUebaCodecs = true, generateUebaCodecsByDefault = true, generateJsonCodecsByDefault = true, generateDomainFacade = false,
          serviceResult = ServiceResultConfig.javaDefault, serviceContext = ServiceContextConfig.default, pragmas = Map.empty,
          asyncServices = false, generateMcpServer = false,
        ),
      )
      (t, new BaboonJvmJvModule[F](t))
    case "dt" =>
      val t = DtTarget(
        "Dart", dummyOut, generic,
        DtOptions(
          writeEvolutionDict = false, wrappedAdtBranchCodecs = false, generateJsonCodecs = true, generateUebaCodecs = true,
          generateUebaCodecsByDefault = true, generateJsonCodecsByDefault = true, generateDomainFacade = false,
          serviceResult = ServiceResultConfig.dartDefault, serviceContext = ServiceContextConfig.default, pragmas = Map.empty,
          asyncServices = false, generateMcpServer = false,
        ),
      )
      (t, new BaboonJvmDtModule[F](t))
    case "sw" =>
      val t = SwTarget(
        "Swift", dummyOut, generic,
        SwOptions(
          writeEvolutionDict = false, wrappedAdtBranchCodecs = false, generateJsonCodecs = true, generateUebaCodecs = true,
          generateUebaCodecsByDefault = true, generateJsonCodecsByDefault = true, generateDomainFacade = false,
          serviceResult = ServiceResultConfig.swiftDefault, serviceContext = ServiceContextConfig.default, pragmas = Map.empty,
          asyncServices = false, generateMcpServer = false,
        ),
      )
      (t, new BaboonJvmSwModule[F](t))
    case other =>
      throw new AssertionError(s"unknown backend label: $other")
  }

  private def baseModule(target: CompilerTarget): distage.Module =
    new BaboonModuleJvm[Either](
      CompilerOptions(
        debug                    = false,
        individualInputs         = Set.empty,
        directoryInputs          = Set(FSPath.parse(NEString.unsafeFrom("./baboon-compiler/src/test/resources/baboon"))),
        metaWriteEvolutionJsonTo = None,
        lockfile                 = None,
        emitOnly                 = None,
        targets                  = Seq(target),
      ),
      UnsafeInstances.Lawless_ParallelErrorAccumulatingOpsEither,
    )

  // We only need a valid Spec2 plugin to bootstrap; the actual per-backend wiring is built with
  // fresh Injectors inside the test body. Bind the CS target so `config` resolves cleanly.
  private val (bootstrapTarget, bootstrapModule): (CompilerTarget, distage.Module) = targetAndModule("cs")
  private val combinedBootstrap: distage.Module                                    = baseModule(bootstrapTarget) overriddenBy bootstrapModule

  override protected def config: TestConfig = super.config.copy(
    pluginConfig = PluginConfig.const(combinedBootstrap.morph[PluginBase]),
    activation   = super.config.activation + BaboonModeAxis.Compiler,
  )

  private def fixtureFiles(dir: String): List[java.nio.file.Path] = {
    val root = IzResources
      .getPath(dir)
      .getOrElse(throw new AssertionError(s"fixture dir not found on test classpath: $dir"))
      .asInstanceOf[IzResources.LoadablePathReference]
      .path
    IzFiles.walk(root.toFile).toList.filter(p => p.toFile.isFile && p.toFile.getName.endsWith(".baboon"))
  }

  import EitherSupport.{defaultModuleEither, quasiIOEither, quasiIORunnerEither}

  /** Load one form (a 2-version model under `dir`) to a typed family, failing the test on issues. */
  private def loadForm(dir: String): BaboonFamily = {
    val ran: Either[Throwable, Either[NEList[BaboonIssue], BaboonFamily]] =
      Injector
        .NoCycles[Either[Throwable, _]]()
        .produceRun(combinedBootstrap, Activation(BaboonModeAxis.Compiler)) {
          (loader: BaboonLoader[Either]) =>
            Right(loader.load(fixtureFiles(dir))): Either[Throwable, Either[NEList[BaboonIssue], BaboonFamily]]
        }
    ran.fold(t => throw t, identity) match {
      case Right(fam) => fam
      case Left(is)   => fail(s"loading $dir failed: ${is.toList.mkString("\n")}")
    }
  }

  /** Translate a family with the given backend, returning the per-path emitted bytes. */
  private def translateWith(label: String, fam: BaboonFamily): Map[String, String] = {
    val (target, perLang) = targetAndModule(label)
    val combined          = baseModule(target) overriddenBy perLang
    val ran: Either[Throwable, Either[NEList[BaboonIssue], io.septimalmind.baboon.translator.Sources]] =
      Injector
        .NoCycles[Either[Throwable, _]]()
        .produceRun(combined, Activation(BaboonModeAxis.Compiler)) {
          (translator: BaboonAbstractTranslator[Either]) =>
            Right(translator.translate(fam)): Either[Throwable, Either[NEList[BaboonIssue], io.septimalmind.baboon.translator.Sources]]
        }
    ran.fold(t => throw t, identity) match {
      case Right(srcs) => srcs.files.iterator.map { case (p, of) => (p, of.content) }.toMap
      case Left(is)    => fail(s"$label translate failed: ${is.toList.mkString("\n")}")
    }
  }

  private val metagen: BaboonMetagen = new BaboonMetagen.BaboonMetagenImpl

  /** Assert two emitted trees are byte-identical: same paths, same bytes per path. */
  private def assertTreesEqual(label: String, delta: Map[String, String], explicit: Map[String, String]): Unit = {
    val deltaPaths    = delta.keySet
    val explicitPaths = explicit.keySet
    assert(
      deltaPaths == explicitPaths,
      s"[$label] emitted file SET differs.\n  only-in-delta=${(deltaPaths -- explicitPaths).toList.sorted}\n  only-in-explicit=${(explicitPaths -- deltaPaths).toList.sorted}",
    )
    val byteDiffs = deltaPaths.toList.sorted.filter(p => delta(p) != explicit(p))
    assert(
      byteDiffs.isEmpty,
      s"[$label] ${byteDiffs.size} file(s) differ in BYTES between delta and explicit twin: ${byteDiffs.take(5)}",
    )
  }

  private def runForms(deltaDir: String, explicitDir: String, what: String): Unit = {
    val deltaFamily    = loadForm(deltaDir)
    val explicitFamily = loadForm(explicitDir)

    // (2) Evolution metadata MUST be identical between the two forms (backend-independent).
    val deltaMeta    = metagen.meta(deltaFamily).spaces2
    val explicitMeta = metagen.meta(explicitFamily).spaces2
    assert(
      deltaMeta == explicitMeta,
      s"[$what] evolution metadata differs between delta and explicit twin.\n--- delta ---\n$deltaMeta\n--- explicit ---\n$explicitMeta",
    )

    // (1) Byte-identical generated output for ALL 9 backends.
    backendLabels.foreach {
      label =>
        val deltaTree    = translateWith(label, deltaFamily)
        val explicitTree = translateWith(label, explicitFamily)
        assertTreesEqual(s"$what/$label", deltaTree, explicitTree)
    }
  }

  "T165 keep/drop end-to-end equivalence" should {

    "produce byte-identical 9-backend output AND identical evolution for `keep B1,B2; drop B3; data B4` vs its explicit twin" in {
      (_: BaboonLoader[F]) =>
        runForms("adt-delta-twin-ok/delta", "adt-delta-twin-ok/explicit", "keep-drop-add")
        // Spec2 requires the body to return F[_, _]; nothing to fail beyond the asserts above.
        izumi.functional.bio.F.unit
    }

    "produce byte-identical 9-backend output AND identical evolution for the `keep *` add-one-field wildcard vs its explicit twin" in {
      (_: BaboonLoader[F]) =>
        runForms("adt-delta-twin-ok/wildcard-delta", "adt-delta-twin-ok/wildcard-explicit", "wildcard-add-field")
        izumi.functional.bio.F.unit
    }
  }
}
