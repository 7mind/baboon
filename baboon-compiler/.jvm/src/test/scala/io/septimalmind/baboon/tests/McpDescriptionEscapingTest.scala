package io.septimalmind.baboon.tests

import distage.plugins.PluginBase
import io.septimalmind.baboon.*
import io.septimalmind.baboon.CompilerTarget.*
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

/** T127 codegen-shape RED-gate suite: asserts that the MCP description
  * string literal for the multi-line `describePricing` method (D34/T125
  * fixture) is properly escaped per backend.
  *
  * === What this suite pins ===
  *
  * The `describePricing` method carries a multi-line `/** */` doc comment
  * whose cleaned text (produced by [[io.septimalmind.baboon.translator.mcp.McpDocs.flatten]])
  * contains three hazard characters:
  *   - raw `\n` (newlines between doc-comment lines) — hazard for all 6 non-JSON backends
  *   - `$` (in "$5" and "$20") — hazard for Kotlin and Dart (string interpolation)
  *   - `"` (in `"premium"`) — hazard for backends using double-quoted literals
  *
  * === Assertion strategy ===
  *
  * McpDocs.flatten yields text beginning with the first doc-comment line:
  *   "Returns the fee schedule for the requested service tier."
  * followed by a real LF then:
  *   "Base cost is $5 per call; ..."
  *
  * If the generator does NOT escape the LF, the generated source will contain
  * the FIRST_LINE text immediately followed by a raw newline character (0x0A).
  * We assert that `FIRST_LINE + '\n'` does NOT appear in the generated server
  * file: a false assertion here means a raw LF was emitted and the test is RED.
  *
  * For the dollar-sign ($) assertion (Kotlin and Dart only): the description
  * also contains "$5" and "$20". If unescaped, the literal "$5" / "$20" will
  * appear verbatim in the generated source. We assert that NEITHER "$5" NOR
  * "$20" appears literally in the server file (after the T129/T130 fix they
  * must appear as "\$5" / "\$20").
  *
  * === RED / GREEN status (before T129/T130 fix) ===
  *
  * VULNERABLE backends (6 — no-raw-LF assertions RED until T129/T130):
  *   - C#    : csString — manual replace, no \n escape
  *   - Rust  : rustStr  — manual replace, no \n escape
  *   - Kotlin: ktString — manual replace, no \n escape; $ unescaped in descriptions
  *   - Java  : javaString — manual replace, no \n escape
  *   - Dart  : dartString — manual replace, no \n escape; $ unescaped in descriptions
  *   - Swift : swiftString — manual replace, no \n escape
  *
  * SAFE backends (3 — GREEN now, locking the invariant):
  *   - Scala      : scalaString  → Json.fromString(s).noSpaces (Circe) — correct
  *   - Python     : pyString     → Json.fromString(s).noSpaces (Circe) — correct
  *   - TypeScript : jsString     → Json.fromString(s).noSpaces (Circe) — correct
  *
  * All 9 backend classes live in this one file (M29 — one suite touch per file).
  */

// Known first line of the describePricing cleaned description text:
private object McpDescFixture {
  // McpDocs.flatten strips comment delimiters and normalises whitespace;
  // the cleaned text starts with this line (no leading spaces, no "* " prefix).
  val descFirstLine: String = "Returns the fee schedule for the requested service tier."

  // The raw LF hazard: if a generator does not escape '\n', the source will
  // contain descFirstLine immediately followed by a literal newline character.
  val rawLfHazard: String = descFirstLine + "\n"

  // The dollar-sign hazard: the doc contains "$5" and "$20".
  val dollarHazard5: String  = "$5"
  val dollarHazard20: String = "$20"
}

// ─── C# ──────────────────────────────────────────────────────────────────────

final class CSharpMcpDescriptionEscapingTest extends CSharpMcpDescriptionEscapingTestBase[Either]

abstract class CSharpMcpDescriptionEscapingTestBase[F[+_, +_]: Error2: TagKK: BaboonTestModule] extends BaboonTest[F] {

  private def csTarget: CSTarget = CSTarget(
    id = "C#",
    output = OutputOptions(
      safeToRemoveExtensions = Set.empty,
      runtime                = RuntimeGenOpt.With,
      generateConversions    = true,
      output                 = FSPath.parse(NEString.unsafeFrom("./target/baboon-t127-cs-mcp/")),
      fixturesOutput         = None,
      testsOutput            = None,
    ),
    generic = GenericOptions(codecTestIterations = 0),
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
      generateDomainFacade                      = false,
      asyncServices                             = false,
      generateMcpServer                         = true,
    ),
  )

  private def moduleFor: distage.Module = {
    val baseModule =
      new BaboonModuleJvm[Either](
        CompilerOptions(
          debug                    = false,
          individualInputs         = Set.empty,
          directoryInputs          = Set(FSPath.parse(NEString.unsafeFrom("./baboon-compiler/src/test/resources/mcp-stub-ok"))),
          metaWriteEvolutionJsonTo = None,
          lockFile                 = Some(FSPath.parse(NEString.unsafeFrom("./target/baboon.lock"))),
          emitOnly                 = None,
          targets                  = Seq(csTarget),
        ),
        UnsafeInstances.Lawless_ParallelErrorAccumulatingOpsEither,
      )
    baseModule overriddenBy new BaboonJvmCSModule[Either](csTarget)
  }

  override protected def config: TestConfig = super.config.copy(
    pluginConfig = PluginConfig.const(moduleFor.morph[PluginBase]),
    activation   = super.config.activation + BaboonModeAxis.Compiler,
  )

  private def loadStubFamily(loader: BaboonLoader[F]): F[NEList[BaboonIssue], BaboonFamily] = {
    val root = IzResources
      .getPath("mcp-stub-ok")
      .getOrElse(throw new AssertionError("mcp-stub-ok fixture not found"))
      .asInstanceOf[IzResources.LoadablePathReference]
      .path
    val baboons =
      if (root.toFile.isDirectory)
        IzFiles.walk(root.toFile).toList.filter(p => p.toFile.isFile && p.toFile.getName.endsWith(".baboon"))
      else List(root)
    loader.load(baboons)
  }

  "C# MCP description escaping (describePricing multi-line hazard chars)" should {

    // T127 RED gate (fails before T129/T130): csString does not escape raw \n.
    // Assertion: the server source must NOT contain the first description line
    // immediately followed by a raw LF — that would mean csString emitted the
    // multi-line doc as a broken C# string literal.
    "emit no raw LF in the describePricing description literal (RED before T129/T130 fix)" in {
      (loader: BaboonLoader[F], translator: BaboonAbstractTranslator[F]) =>
        for {
          family <- loadStubFamily(loader)
          srcs   <- translator.translate(family)
        } yield {
          val serverPaths = srcs.files.keys.filter(_.endsWith("McpServer.cs")).toList
          assert(serverPaths.size == 1, s"expected one McpServer.cs, got: ${serverPaths}")
          val server = srcs.files(serverPaths.head).content

          assert(
            !server.contains(McpDescFixture.rawLfHazard),
            s"[T127 RED] C# generated server contains raw LF in description — csString does not escape newlines. " +
              s"Hazard text (first-line + LF) found: '${McpDescFixture.rawLfHazard.take(60).replace("\n", "<LF>")}'",
          )
        }
    }
  }
}

// ─── Rust ─────────────────────────────────────────────────────────────────────

final class RustMcpDescriptionEscapingTest extends RustMcpDescriptionEscapingTestBase[Either]

abstract class RustMcpDescriptionEscapingTestBase[F[+_, +_]: Error2: TagKK: BaboonTestModule] extends BaboonTest[F] {

  private def rsTarget: RsTarget = RsTarget(
    id = "Rust",
    output = OutputOptions(
      safeToRemoveExtensions = Set.empty,
      runtime                = RuntimeGenOpt.With,
      generateConversions    = true,
      output                 = FSPath.parse(NEString.unsafeFrom("./target/baboon-t127-rs-mcp/")),
      fixturesOutput         = None,
      testsOutput            = None,
    ),
    generic = GenericOptions(codecTestIterations = 0),
    language = RsOptions(
      writeEvolutionDict          = false,
      wrappedAdtBranchCodecs      = false,
      generateJsonCodecs          = true,
      generateUebaCodecs          = true,
      generateJsonCodecsByDefault = true,
      generateUebaCodecsByDefault = true,
      serviceResult               = ServiceResultConfig.rustDefault,
      serviceContext              = ServiceContextConfig.default,
      pragmas                     = Map.empty,
      generateDomainFacade        = false,
      asyncServices               = false,
      cratePrefix                 = "crate",
      reexportMode                = "selective",
      edition                     = "2021",
      generateMcpServer           = true,
    ),
  )

  private def moduleFor: distage.Module = {
    val baseModule =
      new BaboonModuleJvm[Either](
        CompilerOptions(
          debug                    = false,
          individualInputs         = Set.empty,
          directoryInputs          = Set(FSPath.parse(NEString.unsafeFrom("./baboon-compiler/src/test/resources/mcp-stub-ok"))),
          metaWriteEvolutionJsonTo = None,
          lockFile                 = Some(FSPath.parse(NEString.unsafeFrom("./target/baboon.lock"))),
          emitOnly                 = None,
          targets                  = Seq(rsTarget),
        ),
        UnsafeInstances.Lawless_ParallelErrorAccumulatingOpsEither,
      )
    baseModule overriddenBy new BaboonJvmRsModule[Either](rsTarget)
  }

  override protected def config: TestConfig = super.config.copy(
    pluginConfig = PluginConfig.const(moduleFor.morph[PluginBase]),
    activation   = super.config.activation + BaboonModeAxis.Compiler,
  )

  private def loadStubFamily(loader: BaboonLoader[F]): F[NEList[BaboonIssue], BaboonFamily] = {
    val root = IzResources
      .getPath("mcp-stub-ok")
      .getOrElse(throw new AssertionError("mcp-stub-ok fixture not found"))
      .asInstanceOf[IzResources.LoadablePathReference]
      .path
    val baboons =
      if (root.toFile.isDirectory)
        IzFiles.walk(root.toFile).toList.filter(p => p.toFile.isFile && p.toFile.getName.endsWith(".baboon"))
      else List(root)
    loader.load(baboons)
  }

  "Rust MCP description escaping (describePricing multi-line hazard chars)" should {

    // T127 RED gate (fails before T129/T130): rustStr does not escape raw \n.
    "emit no raw LF in the describePricing description literal (RED before T129/T130 fix)" in {
      (loader: BaboonLoader[F], translator: BaboonAbstractTranslator[F]) =>
        for {
          family <- loadStubFamily(loader)
          srcs   <- translator.translate(family)
        } yield {
          val serverPaths = srcs.files.keys
            .filter(_.endsWith("_mcp_server.rs"))
            .filterNot(_.endsWith("baboon_mcp_server.rs"))
            .toList
          assert(serverPaths.size == 1, s"expected one *_mcp_server.rs, got: ${serverPaths}")
          val server = srcs.files(serverPaths.head).content

          assert(
            !server.contains(McpDescFixture.rawLfHazard),
            s"[T127 RED] Rust generated server contains raw LF in description — rustStr does not escape newlines.",
          )
        }
    }
  }
}

// ─── Kotlin ───────────────────────────────────────────────────────────────────

final class KotlinMcpDescriptionEscapingTest extends KotlinMcpDescriptionEscapingTestBase[Either]

abstract class KotlinMcpDescriptionEscapingTestBase[F[+_, +_]: Error2: TagKK: BaboonTestModule] extends BaboonTest[F] {

  private def ktTarget: KtTarget = KtTarget(
    id = "Kotlin",
    output = OutputOptions(
      safeToRemoveExtensions = Set.empty,
      runtime                = RuntimeGenOpt.With,
      generateConversions    = true,
      output                 = FSPath.parse(NEString.unsafeFrom("./target/baboon-t127-kt-mcp/")),
      fixturesOutput         = None,
      testsOutput            = None,
    ),
    generic = GenericOptions(codecTestIterations = 0),
    language = KtOptions(
      writeEvolutionDict          = false,
      wrappedAdtBranchCodecs      = false,
      enableDeprecatedEncoders    = false,
      generateJsonCodecs          = true,
      generateUebaCodecs          = true,
      generateJsonCodecsByDefault = true,
      generateUebaCodecsByDefault = true,
      generateDomainFacade        = false,
      serviceResult               = ServiceResultConfig.kotlinDefault,
      serviceContext              = ServiceContextConfig.default,
      pragmas                     = Map.empty,
      asyncServices               = false,
      multiplatform               = false,
      generateMcpServer           = true,
    ),
  )

  private def moduleFor: distage.Module = {
    val baseModule =
      new BaboonModuleJvm[Either](
        CompilerOptions(
          debug                    = false,
          individualInputs         = Set.empty,
          directoryInputs          = Set(FSPath.parse(NEString.unsafeFrom("./baboon-compiler/src/test/resources/mcp-stub-ok"))),
          metaWriteEvolutionJsonTo = None,
          lockFile                 = Some(FSPath.parse(NEString.unsafeFrom("./target/baboon.lock"))),
          emitOnly                 = None,
          targets                  = Seq(ktTarget),
        ),
        UnsafeInstances.Lawless_ParallelErrorAccumulatingOpsEither,
      )
    baseModule overriddenBy new BaboonJvmKtModule[Either](ktTarget)
  }

  override protected def config: TestConfig = super.config.copy(
    pluginConfig = PluginConfig.const(moduleFor.morph[PluginBase]),
    activation   = super.config.activation + BaboonModeAxis.Compiler,
  )

  private def loadStubFamily(loader: BaboonLoader[F]): F[NEList[BaboonIssue], BaboonFamily] = {
    val root = IzResources
      .getPath("mcp-stub-ok")
      .getOrElse(throw new AssertionError("mcp-stub-ok fixture not found"))
      .asInstanceOf[IzResources.LoadablePathReference]
      .path
    val baboons =
      if (root.toFile.isDirectory)
        IzFiles.walk(root.toFile).toList.filter(p => p.toFile.isFile && p.toFile.getName.endsWith(".baboon"))
      else List(root)
    loader.load(baboons)
  }

  "Kotlin MCP description escaping (describePricing multi-line hazard chars)" should {

    // T127 RED gate: ktString does not escape raw \n in description strings.
    "emit no raw LF in the describePricing description literal (RED before T129/T130 fix)" in {
      (loader: BaboonLoader[F], translator: BaboonAbstractTranslator[F]) =>
        for {
          family <- loadStubFamily(loader)
          srcs   <- translator.translate(family)
        } yield {
          val serverPaths = srcs.files.keys.filter(_.endsWith("McpToolsMcpServer.kt")).toList
          assert(serverPaths.size == 1, s"expected one McpToolsMcpServer.kt, got: ${serverPaths}")
          val server = srcs.files(serverPaths.head).content

          assert(
            !server.contains(McpDescFixture.rawLfHazard),
            s"[T127 RED] Kotlin generated server contains raw LF in description — ktString does not escape newlines.",
          )
        }
    }

    // T127 RED gate: ktString does not escape $ in description strings.
    // The describePricing doc contains "$5" and "$20" — in a Kotlin regular
    // string, bare $ triggers interpolation; must be escaped as \$.
    "escape $$ as \\$$ in the describePricing description literal (RED before T129/T130 fix)" in {
      (loader: BaboonLoader[F], translator: BaboonAbstractTranslator[F]) =>
        for {
          family <- loadStubFamily(loader)
          srcs   <- translator.translate(family)
        } yield {
          val serverPaths = srcs.files.keys.filter(_.endsWith("McpToolsMcpServer.kt")).toList
          assert(serverPaths.size == 1, s"expected one McpToolsMcpServer.kt, got: ${serverPaths}")
          val server = srcs.files(serverPaths.head).content

          // After the fix, "$5" and "$20" must appear as "\$5" / "\$20" in the source.
          // Before the fix they appear as bare "$5" / "$20" (interpolation hazard).
          assert(
            !server.contains(McpDescFixture.dollarHazard5),
            s"[T127 RED] Kotlin server contains unescaped '$$5' in description — ktString must escape $$ as \\$$",
          )
          assert(
            !server.contains(McpDescFixture.dollarHazard20),
            s"[T127 RED] Kotlin server contains unescaped '$$20' in description — ktString must escape $$ as \\$$",
          )
        }
    }
  }
}

// ─── Java ─────────────────────────────────────────────────────────────────────

final class JavaMcpDescriptionEscapingTest extends JavaMcpDescriptionEscapingTestBase[Either]

abstract class JavaMcpDescriptionEscapingTestBase[F[+_, +_]: Error2: TagKK: BaboonTestModule] extends BaboonTest[F] {

  private def jvTarget: JvTarget = JvTarget(
    id = "Java",
    output = OutputOptions(
      safeToRemoveExtensions = Set.empty,
      runtime                = RuntimeGenOpt.With,
      generateConversions    = true,
      output                 = FSPath.parse(NEString.unsafeFrom("./target/baboon-t127-jv-mcp/")),
      fixturesOutput         = None,
      testsOutput            = None,
    ),
    generic = GenericOptions(codecTestIterations = 0),
    language = JvOptions(
      writeEvolutionDict          = false,
      wrappedAdtBranchCodecs      = false,
      enableDeprecatedEncoders    = false,
      generateJsonCodecs          = true,
      generateUebaCodecs          = true,
      generateJsonCodecsByDefault = true,
      generateUebaCodecsByDefault = true,
      generateDomainFacade        = false,
      serviceResult               = ServiceResultConfig(
        noErrors   = false,
        resultType = Some("baboon.runtime.shared.BaboonEither"),
        pattern    = Some("<$error, $success>"),
        hkt        = None,
      ),
      serviceContext    = ServiceContextConfig.default,
      pragmas           = Map.empty,
      asyncServices     = false,
      generateMcpServer = true,
    ),
  )

  private def moduleFor: distage.Module = {
    val baseModule =
      new BaboonModuleJvm[Either](
        CompilerOptions(
          debug                    = false,
          individualInputs         = Set.empty,
          directoryInputs          = Set(FSPath.parse(NEString.unsafeFrom("./baboon-compiler/src/test/resources/mcp-stub-ok"))),
          metaWriteEvolutionJsonTo = None,
          lockFile                 = Some(FSPath.parse(NEString.unsafeFrom("./target/baboon.lock"))),
          emitOnly                 = None,
          targets                  = Seq(jvTarget),
        ),
        UnsafeInstances.Lawless_ParallelErrorAccumulatingOpsEither,
      )
    baseModule overriddenBy new BaboonJvmJvModule[Either](jvTarget)
  }

  override protected def config: TestConfig = super.config.copy(
    pluginConfig = PluginConfig.const(moduleFor.morph[PluginBase]),
    activation   = super.config.activation + BaboonModeAxis.Compiler,
  )

  private def loadStubFamily(loader: BaboonLoader[F]): F[NEList[BaboonIssue], BaboonFamily] = {
    val root = IzResources
      .getPath("mcp-stub-ok")
      .getOrElse(throw new AssertionError("mcp-stub-ok fixture not found"))
      .asInstanceOf[IzResources.LoadablePathReference]
      .path
    val baboons =
      if (root.toFile.isDirectory)
        IzFiles.walk(root.toFile).toList.filter(p => p.toFile.isFile && p.toFile.getName.endsWith(".baboon"))
      else List(root)
    loader.load(baboons)
  }

  "Java MCP description escaping (describePricing multi-line hazard chars)" should {

    // T127 RED gate (fails before T129/T130): javaString does not escape raw \n.
    "emit no raw LF in the describePricing description literal (RED before T129/T130 fix)" in {
      (loader: BaboonLoader[F], translator: BaboonAbstractTranslator[F]) =>
        for {
          family <- loadStubFamily(loader)
          srcs   <- translator.translate(family)
        } yield {
          val serverPaths = srcs.files.keys.filter(_.endsWith("McpToolsMcpServer.java")).toList
          assert(serverPaths.size == 1, s"expected one McpToolsMcpServer.java, got: ${serverPaths}")
          val server = srcs.files(serverPaths.head).content

          assert(
            !server.contains(McpDescFixture.rawLfHazard),
            s"[T127 RED] Java generated server contains raw LF in description — javaString does not escape newlines.",
          )
        }
    }
  }
}

// ─── Dart ─────────────────────────────────────────────────────────────────────

final class DartMcpDescriptionEscapingTest extends DartMcpDescriptionEscapingTestBase[Either]

abstract class DartMcpDescriptionEscapingTestBase[F[+_, +_]: Error2: TagKK: BaboonTestModule] extends BaboonTest[F] {

  private def dtTarget: DtTarget = DtTarget(
    id = "Dart",
    output = OutputOptions(
      safeToRemoveExtensions = Set.empty,
      runtime                = RuntimeGenOpt.With,
      generateConversions    = true,
      output                 = FSPath.parse(NEString.unsafeFrom("./target/baboon-t127-dt-mcp/")),
      fixturesOutput         = None,
      testsOutput            = None,
    ),
    generic = GenericOptions(codecTestIterations = 0),
    language = DtOptions(
      writeEvolutionDict          = false,
      wrappedAdtBranchCodecs      = false,
      generateJsonCodecs          = true,
      generateUebaCodecs          = true,
      generateJsonCodecsByDefault = true,
      generateUebaCodecsByDefault = true,
      generateDomainFacade        = false,
      serviceResult               = ServiceResultConfig.dartDefault,
      serviceContext              = ServiceContextConfig.default,
      pragmas                     = Map.empty,
      asyncServices               = false,
      generateMcpServer           = true,
    ),
  )

  private def moduleFor: distage.Module = {
    val baseModule =
      new BaboonModuleJvm[Either](
        CompilerOptions(
          debug                    = false,
          individualInputs         = Set.empty,
          directoryInputs          = Set(FSPath.parse(NEString.unsafeFrom("./baboon-compiler/src/test/resources/mcp-stub-ok"))),
          metaWriteEvolutionJsonTo = None,
          lockFile                 = Some(FSPath.parse(NEString.unsafeFrom("./target/baboon.lock"))),
          emitOnly                 = None,
          targets                  = Seq(dtTarget),
        ),
        UnsafeInstances.Lawless_ParallelErrorAccumulatingOpsEither,
      )
    baseModule overriddenBy new BaboonJvmDtModule[Either](dtTarget)
  }

  override protected def config: TestConfig = super.config.copy(
    pluginConfig = PluginConfig.const(moduleFor.morph[PluginBase]),
    activation   = super.config.activation + BaboonModeAxis.Compiler,
  )

  private def loadStubFamily(loader: BaboonLoader[F]): F[NEList[BaboonIssue], BaboonFamily] = {
    val root = IzResources
      .getPath("mcp-stub-ok")
      .getOrElse(throw new AssertionError("mcp-stub-ok fixture not found"))
      .asInstanceOf[IzResources.LoadablePathReference]
      .path
    val baboons =
      if (root.toFile.isDirectory)
        IzFiles.walk(root.toFile).toList.filter(p => p.toFile.isFile && p.toFile.getName.endsWith(".baboon"))
      else List(root)
    loader.load(baboons)
  }

  "Dart MCP description escaping (describePricing multi-line hazard chars)" should {

    // T127 RED gate (fails before T129/T130): dartString does not escape raw \n.
    "emit no raw LF in the describePricing description literal (RED before T129/T130 fix)" in {
      (loader: BaboonLoader[F], translator: BaboonAbstractTranslator[F]) =>
        for {
          family <- loadStubFamily(loader)
          srcs   <- translator.translate(family)
        } yield {
          val serverPaths = srcs.files.keys.filter(_.endsWith("mcp_tools_mcp_server.dart")).toList
          assert(serverPaths.size == 1, s"expected one mcp_tools_mcp_server.dart, got: ${serverPaths}")
          val server = srcs.files(serverPaths.head).content

          assert(
            !server.contains(McpDescFixture.rawLfHazard),
            s"[T127 RED] Dart generated server contains raw LF in description — dartString does not escape newlines.",
          )
        }
    }

    // T127 RED gate: dartString does not escape $ in description strings.
    // In Dart single-quoted strings, bare "$" triggers string interpolation.
    "escape $$ as \\$$ in the describePricing description literal (RED before T129/T130 fix)" in {
      (loader: BaboonLoader[F], translator: BaboonAbstractTranslator[F]) =>
        for {
          family <- loadStubFamily(loader)
          srcs   <- translator.translate(family)
        } yield {
          val serverPaths = srcs.files.keys.filter(_.endsWith("mcp_tools_mcp_server.dart")).toList
          assert(serverPaths.size == 1, s"expected one mcp_tools_mcp_server.dart, got: ${serverPaths}")
          val server = srcs.files(serverPaths.head).content

          // After T129/T130: "$5" must appear as "\$5" in the Dart source.
          assert(
            !server.contains(McpDescFixture.dollarHazard5),
            s"[T127 RED] Dart server contains unescaped '$$5' in description — dartString must escape $$ as \\$$",
          )
          assert(
            !server.contains(McpDescFixture.dollarHazard20),
            s"[T127 RED] Dart server contains unescaped '$$20' in description — dartString must escape $$ as \\$$",
          )
        }
    }
  }
}

// ─── Swift ────────────────────────────────────────────────────────────────────

final class SwiftMcpDescriptionEscapingTest extends SwiftMcpDescriptionEscapingTestBase[Either]

abstract class SwiftMcpDescriptionEscapingTestBase[F[+_, +_]: Error2: TagKK: BaboonTestModule] extends BaboonTest[F] {

  private def swTarget: SwTarget = SwTarget(
    id = "Swift",
    output = OutputOptions(
      safeToRemoveExtensions = Set.empty,
      runtime                = RuntimeGenOpt.With,
      generateConversions    = true,
      output                 = FSPath.parse(NEString.unsafeFrom("./target/baboon-t127-sw-mcp/")),
      fixturesOutput         = None,
      testsOutput            = None,
    ),
    generic = GenericOptions(codecTestIterations = 0),
    language = SwOptions(
      writeEvolutionDict          = false,
      wrappedAdtBranchCodecs      = false,
      generateJsonCodecs          = true,
      generateUebaCodecs          = true,
      generateJsonCodecsByDefault = true,
      generateUebaCodecsByDefault = true,
      serviceResult               = ServiceResultConfig.swiftDefault,
      serviceContext              = ServiceContextConfig.default,
      pragmas                     = Map.empty,
      generateDomainFacade        = false,
      asyncServices               = false,
      generateMcpServer           = true,
    ),
  )

  private def moduleFor: distage.Module = {
    val baseModule =
      new BaboonModuleJvm[Either](
        CompilerOptions(
          debug                    = false,
          individualInputs         = Set.empty,
          directoryInputs          = Set(FSPath.parse(NEString.unsafeFrom("./baboon-compiler/src/test/resources/mcp-stub-ok"))),
          metaWriteEvolutionJsonTo = None,
          lockFile                 = Some(FSPath.parse(NEString.unsafeFrom("./target/baboon.lock"))),
          emitOnly                 = None,
          targets                  = Seq(swTarget),
        ),
        UnsafeInstances.Lawless_ParallelErrorAccumulatingOpsEither,
      )
    baseModule overriddenBy new BaboonJvmSwModule[Either](swTarget)
  }

  override protected def config: TestConfig = super.config.copy(
    pluginConfig = PluginConfig.const(moduleFor.morph[PluginBase]),
    activation   = super.config.activation + BaboonModeAxis.Compiler,
  )

  private def loadStubFamily(loader: BaboonLoader[F]): F[NEList[BaboonIssue], BaboonFamily] = {
    val root = IzResources
      .getPath("mcp-stub-ok")
      .getOrElse(throw new AssertionError("mcp-stub-ok fixture not found"))
      .asInstanceOf[IzResources.LoadablePathReference]
      .path
    val baboons =
      if (root.toFile.isDirectory)
        IzFiles.walk(root.toFile).toList.filter(p => p.toFile.isFile && p.toFile.getName.endsWith(".baboon"))
      else List(root)
    loader.load(baboons)
  }

  "Swift MCP description escaping (describePricing multi-line hazard chars)" should {

    // T127 RED gate (fails before T129/T130): swiftString does not escape raw \n.
    // Swift double-quoted string literals must not span multiple lines
    // (raw newlines are a syntax error in Swift).
    "emit no raw LF in the describePricing description literal (RED before T129/T130 fix)" in {
      (loader: BaboonLoader[F], translator: BaboonAbstractTranslator[F]) =>
        for {
          family <- loadStubFamily(loader)
          srcs   <- translator.translate(family)
        } yield {
          val serverPaths = srcs.files.keys.filter(_.endsWith("mcp_tools_mcp_server.swift")).toList
          assert(serverPaths.size == 1, s"expected one mcp_tools_mcp_server.swift, got: ${serverPaths}")
          val server = srcs.files(serverPaths.head).content

          assert(
            !server.contains(McpDescFixture.rawLfHazard),
            s"[T127 RED] Swift generated server contains raw LF in description — swiftString does not escape newlines.",
          )
        }
    }
  }
}

// ─── Scala (SAFE — GREEN now) ─────────────────────────────────────────────────

final class ScalaMcpDescriptionEscapingTest extends ScalaMcpDescriptionEscapingTestBase[Either]

abstract class ScalaMcpDescriptionEscapingTestBase[F[+_, +_]: Error2: TagKK: BaboonTestModule] extends BaboonTest[F] {

  private def scTarget: ScTarget = ScTarget(
    id = "Scala",
    output = OutputOptions(
      safeToRemoveExtensions = Set.empty,
      runtime                = RuntimeGenOpt.With,
      generateConversions    = true,
      output                 = FSPath.parse(NEString.unsafeFrom("./target/baboon-t127-sc-mcp/")),
      fixturesOutput         = None,
      testsOutput            = None,
    ),
    generic = GenericOptions(codecTestIterations = 0),
    language = ScOptions(
      writeEvolutionDict          = false,
      wrappedAdtBranchCodecs      = false,
      enableDeprecatedEncoders    = false,
      generateJsonCodecs          = true,
      generateUebaCodecs          = true,
      generateJsonCodecsByDefault = true,
      generateUebaCodecsByDefault = true,
      generateDomainFacade        = false,
      serviceResult               = ServiceResultConfig(
        noErrors   = false,
        resultType = Some("Either"),
        pattern    = Some("[$error, $success]"),
        hkt        = None,
      ),
      serviceContext    = ServiceContextConfig.default,
      pragmas           = Map.empty,
      generateMcpServer = true,
    ),
  )

  private def moduleFor: distage.Module = {
    val baseModule =
      new BaboonModuleJvm[Either](
        CompilerOptions(
          debug                    = false,
          individualInputs         = Set.empty,
          directoryInputs          = Set(FSPath.parse(NEString.unsafeFrom("./baboon-compiler/src/test/resources/mcp-stub-ok"))),
          metaWriteEvolutionJsonTo = None,
          lockFile                 = Some(FSPath.parse(NEString.unsafeFrom("./target/baboon.lock"))),
          emitOnly                 = None,
          targets                  = Seq(scTarget),
        ),
        UnsafeInstances.Lawless_ParallelErrorAccumulatingOpsEither,
      )
    baseModule overriddenBy new BaboonJvmScModule[Either](scTarget)
  }

  override protected def config: TestConfig = super.config.copy(
    pluginConfig = PluginConfig.const(moduleFor.morph[PluginBase]),
    activation   = super.config.activation + BaboonModeAxis.Compiler,
  )

  private def loadStubFamily(loader: BaboonLoader[F]): F[NEList[BaboonIssue], BaboonFamily] = {
    val root = IzResources
      .getPath("mcp-stub-ok")
      .getOrElse(throw new AssertionError("mcp-stub-ok fixture not found"))
      .asInstanceOf[IzResources.LoadablePathReference]
      .path
    val baboons =
      if (root.toFile.isDirectory)
        IzFiles.walk(root.toFile).toList.filter(p => p.toFile.isFile && p.toFile.getName.endsWith(".baboon"))
      else List(root)
    loader.load(baboons)
  }

  // T127 GREEN invariant lock: scalaString uses Json.fromString(s).noSpaces (Circe),
  // which JSON-encodes '\n' as '\\n' and '$' is not special in Scala string literals.
  "Scala MCP description escaping (describePricing multi-line hazard chars) [SAFE — GREEN now]" should {

    "emit no raw LF in the describePricing description (Scala — scalaString uses Circe, GREEN)" in {
      (loader: BaboonLoader[F], translator: BaboonAbstractTranslator[F]) =>
        for {
          family <- loadStubFamily(loader)
          srcs   <- translator.translate(family)
        } yield {
          val serverPaths = srcs.files.keys.filter(_.endsWith("McpToolsMcpServer.scala")).toList
          assert(serverPaths.size == 1, s"expected one McpToolsMcpServer.scala, got: ${serverPaths}")
          val server = srcs.files(serverPaths.head).content

          // scalaString encodes '\n' as JSON '\\n' — the raw LF hazard must be absent.
          assert(
            !server.contains(McpDescFixture.rawLfHazard),
            s"Scala server must not contain raw LF in description (scalaString via Circe is safe).",
          )
        }
    }
  }
}

// ─── Python (SAFE — GREEN now) ────────────────────────────────────────────────

final class PythonMcpDescriptionEscapingTest extends PythonMcpDescriptionEscapingTestBase[Either]

abstract class PythonMcpDescriptionEscapingTestBase[F[+_, +_]: Error2: TagKK: BaboonTestModule] extends BaboonTest[F] {

  private def pyTarget: PyTarget = PyTarget(
    id = "Python",
    output = OutputOptions(
      safeToRemoveExtensions = Set.empty,
      runtime                = RuntimeGenOpt.With,
      generateConversions    = true,
      output                 = FSPath.parse(NEString.unsafeFrom("./target/baboon-t127-py-mcp/")),
      fixturesOutput         = None,
      testsOutput            = None,
    ),
    generic = GenericOptions(codecTestIterations = 0),
    language = PyOptions(
      writeEvolutionDict          = false,
      wrappedAdtBranchCodecs      = false,
      generateJsonCodecs          = true,
      generateUebaCodecs          = true,
      generateJsonCodecsByDefault = true,
      generateUebaCodecsByDefault = true,
      enableDeprecatedEncoders    = false,
      generateDomainFacade        = false,
      asyncServices               = false,
      serviceResult               = ServiceResultConfig(
        noErrors   = false,
        resultType = Some("BaboonEither"),
        pattern    = Some("<$error, $success>"),
        hkt        = None,
      ),
      serviceContext    = ServiceContextConfig.default,
      pragmas           = Map.empty,
      generateMcpServer = true,
    ),
  )

  private def moduleFor: distage.Module = {
    val baseModule =
      new BaboonModuleJvm[Either](
        CompilerOptions(
          debug                    = false,
          individualInputs         = Set.empty,
          directoryInputs          = Set(FSPath.parse(NEString.unsafeFrom("./baboon-compiler/src/test/resources/mcp-stub-ok"))),
          metaWriteEvolutionJsonTo = None,
          lockFile                 = Some(FSPath.parse(NEString.unsafeFrom("./target/baboon.lock"))),
          emitOnly                 = None,
          targets                  = Seq(pyTarget),
        ),
        UnsafeInstances.Lawless_ParallelErrorAccumulatingOpsEither,
      )
    baseModule overriddenBy new BaboonJvmPyModule[Either](pyTarget)
  }

  override protected def config: TestConfig = super.config.copy(
    pluginConfig = PluginConfig.const(moduleFor.morph[PluginBase]),
    activation   = super.config.activation + BaboonModeAxis.Compiler,
  )

  private def loadStubFamily(loader: BaboonLoader[F]): F[NEList[BaboonIssue], BaboonFamily] = {
    val root = IzResources
      .getPath("mcp-stub-ok")
      .getOrElse(throw new AssertionError("mcp-stub-ok fixture not found"))
      .asInstanceOf[IzResources.LoadablePathReference]
      .path
    val baboons =
      if (root.toFile.isDirectory)
        IzFiles.walk(root.toFile).toList.filter(p => p.toFile.isFile && p.toFile.getName.endsWith(".baboon"))
      else List(root)
    loader.load(baboons)
  }

  // T127 GREEN invariant lock: pyString uses Json.fromString(s).noSpaces (Circe),
  // which JSON-encodes '\n' as '\\n'; '$' is not special in Python strings.
  "Python MCP description escaping (describePricing multi-line hazard chars) [SAFE — GREEN now]" should {

    "emit no raw LF in the describePricing description (Python — pyString uses Circe, GREEN)" in {
      (loader: BaboonLoader[F], translator: BaboonAbstractTranslator[F]) =>
        for {
          family <- loadStubFamily(loader)
          srcs   <- translator.translate(family)
        } yield {
          val serverPaths = srcs.files.keys.filter(_.endsWith("McpToolsMcpServer.py")).toList
          assert(serverPaths.size == 1, s"expected one McpToolsMcpServer.py, got: ${serverPaths}")
          val server = srcs.files(serverPaths.head).content

          assert(
            !server.contains(McpDescFixture.rawLfHazard),
            s"Python server must not contain raw LF in description (pyString via Circe is safe).",
          )
        }
    }
  }
}

// ─── TypeScript (SAFE — GREEN now) ────────────────────────────────────────────

final class TypeScriptMcpDescriptionEscapingTest extends TypeScriptMcpDescriptionEscapingTestBase[Either]

abstract class TypeScriptMcpDescriptionEscapingTestBase[F[+_, +_]: Error2: TagKK: BaboonTestModule] extends BaboonTest[F] {

  private def tsTarget: TsTarget = TsTarget(
    id = "TypeScript",
    output = OutputOptions(
      safeToRemoveExtensions = Set.empty,
      runtime                = RuntimeGenOpt.With,
      generateConversions    = true,
      output                 = FSPath.parse(NEString.unsafeFrom("./target/baboon-t127-ts-mcp/")),
      fixturesOutput         = None,
      testsOutput            = None,
    ),
    generic = GenericOptions(codecTestIterations = 0),
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
      generateMcpServer           = true,
    ),
  )

  private def moduleFor: distage.Module = {
    val baseModule =
      new BaboonModuleJvm[Either](
        CompilerOptions(
          debug                    = false,
          individualInputs         = Set.empty,
          directoryInputs          = Set(FSPath.parse(NEString.unsafeFrom("./baboon-compiler/src/test/resources/mcp-stub-ok"))),
          metaWriteEvolutionJsonTo = None,
          lockFile                 = Some(FSPath.parse(NEString.unsafeFrom("./target/baboon.lock"))),
          emitOnly                 = None,
          targets                  = Seq(tsTarget),
        ),
        UnsafeInstances.Lawless_ParallelErrorAccumulatingOpsEither,
      )
    baseModule overriddenBy new BaboonJvmTsModule[Either](tsTarget)
  }

  override protected def config: TestConfig = super.config.copy(
    pluginConfig = PluginConfig.const(moduleFor.morph[PluginBase]),
    activation   = super.config.activation + BaboonModeAxis.Compiler,
  )

  private def loadStubFamily(loader: BaboonLoader[F]): F[NEList[BaboonIssue], BaboonFamily] = {
    val root = IzResources
      .getPath("mcp-stub-ok")
      .getOrElse(throw new AssertionError("mcp-stub-ok fixture not found"))
      .asInstanceOf[IzResources.LoadablePathReference]
      .path
    val baboons =
      if (root.toFile.isDirectory)
        IzFiles.walk(root.toFile).toList.filter(p => p.toFile.isFile && p.toFile.getName.endsWith(".baboon"))
      else List(root)
    loader.load(baboons)
  }

  // T127 GREEN invariant lock: jsString uses Json.fromString(s).noSpaces (Circe),
  // which JSON-encodes '\n' as '\\n'; '$' is not special in TS/JS double-quoted strings.
  "TypeScript MCP description escaping (describePricing multi-line hazard chars) [SAFE — GREEN now]" should {

    "emit no raw LF in the describePricing description (TypeScript — jsString uses Circe, GREEN)" in {
      (loader: BaboonLoader[F], translator: BaboonAbstractTranslator[F]) =>
        for {
          family <- loadStubFamily(loader)
          srcs   <- translator.translate(family)
        } yield {
          val serverPaths = srcs.files.keys.filter(_.endsWith("/mcp-server.ts")).toList
          assert(serverPaths.size == 1, s"expected one mcp-server.ts, got: ${serverPaths}")
          val server = srcs.files(serverPaths.head).content

          assert(
            !server.contains(McpDescFixture.rawLfHazard),
            s"TypeScript server must not contain raw LF in description (jsString via Circe is safe).",
          )
        }
    }
  }
}
