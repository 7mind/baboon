package io.septimalmind.baboon.tests

import distage.plugins.PluginBase
import io.septimalmind.baboon.*
import io.septimalmind.baboon.CompilerTarget.KtTarget
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

/** T14 codegen-shape test for the Kotlin MCP server generator.
  *
  * Drives the real `translate` path with `--kt-generate-mcp-server=true`, so the
  * T6 dispatch seam invokes [[io.septimalmind.baboon.translator.kotlin.KtMcpServerGenerator]],
  * and asserts the emitted Kotlin MCP server shape for the locked K6 stub model
  * (`mcp-stub-ok`): the additive runtime file, the per-service dispatch class,
  * the transport-abstract `handle` (inherited via `AbstractBaboonMcpServer`, no baked-in I/O),
  * the tool registry with the 5 `McpTools_*` tool names in declaration order, and the
  * inputSchema wired from the T5 emitter (embedded as `Json.parseToJsonElement` literals).
  *
  * Mirrors `CSharpMcpServerEmissionTest` (T10) and `TypeScriptMcpServerEmissionTest` (T8).
  */
final class KotlinMcpServerEmissionTest extends KotlinMcpServerEmissionTestBase[Either]

abstract class KotlinMcpServerEmissionTestBase[F[+_, +_]: Error2: TagKK: BaboonTestModule] extends BaboonTest[F] {

  private def ktTarget(mcp: Boolean): KtTarget = KtTarget(
    id = "Kotlin",
    output = OutputOptions(
      safeToRemoveExtensions = Set.empty,
      runtime                = RuntimeGenOpt.With,
      generateConversions    = true,
      output                 = FSPath.parse(NEString.unsafeFrom("./target/baboon-scalatests-kt-mcp/")),
      fixturesOutput         = None,
      testsOutput            = None,
    ),
    generic = GenericOptions(
      codecTestIterations = 0
    ),
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
      multiplatform               = false,
      generateMcpServer           = mcp,
    ),
  )

  private def moduleFor(target: KtTarget): distage.Module = {
    val baseModule =
      new BaboonModuleJvm[Either](
        CompilerOptions(
          debug                    = false,
          individualInputs         = Set.empty,
          directoryInputs          = Set(FSPath.parse(NEString.unsafeFrom("./baboon-compiler/src/test/resources/mcp-stub-ok"))),
          metaWriteEvolutionJsonTo = None,
          lockFile                 = Some(FSPath.parse(NEString.unsafeFrom("./target/baboon.lock"))),
          emitOnly                 = None,
          targets                  = Seq(target),
        ),
        UnsafeInstances.Lawless_ParallelErrorAccumulatingOpsEither,
      )
    baseModule overriddenBy new BaboonJvmKtModule[Either](target)
  }

  override protected def config: TestConfig = super.config.copy(
    pluginConfig = PluginConfig.const(moduleFor(ktTarget(mcp = true)).morph[PluginBase]),
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

  private val expectedToolNames = List(
    "McpTools_listCollections",
    "McpTools_submitComposite",
    "McpTools_processShape",
    "McpTools_pagePoints",
    "McpTools_ping",
  )

  "Kotlin MCP server generator (mcp-stub-ok fixture)" should {

    "emit a per-service MCP server + runtime with the contract shape when --kt-generate-mcp-server=true" in {
      (loader: BaboonLoader[F], translator: BaboonAbstractTranslator[F]) =>
        for {
          family <- loadStubFamily(loader)
          srcs   <- translator.translate(family)
        } yield {
          val files = srcs.files
          // The additive static runtime resource is emitted.
          assert(files.keys.exists(_.endsWith("BaboonMcpRuntime.kt")), s"BaboonMcpRuntime.kt not emitted; files=${files.keys}")

          // Exactly one per-service MCP server file (single service McpTools).
          val serverPaths = files.keys.filter(_.endsWith("McpToolsMcpServer.kt")).toList
          assert(serverPaths.size == 1, s"expected one McpToolsMcpServer.kt, got: $serverPaths")
          val server = files(serverPaths.head).content

          // Class is generic over Ctx and extends the transport-abstract base.
          assert(
            server.contains("class McpToolsMcpServer<Ctx>("),
            s"missing generic server class:\n$server",
          )
          // It implements the dispatch contract entrypoint through the runtime base
          // (inherited `handle`), supplying the injected JSON delegate — NO I/O loop.
          assert(
            server.contains("McpJsonInvoke<Ctx>"),
            s"missing McpJsonInvoke delegate type:\n$server",
          )
          assert(
            server.contains("AbstractBaboonMcpServer<Ctx>()"),
            s"missing AbstractBaboonMcpServer extension:\n$server",
          )
          assert(
            server.contains("override fun invokeJson("),
            s"missing invokeJson override:\n$server",
          )
          val lower = server.toLowerCase
          assert(
            !lower.contains("while (true)") && !lower.contains("readline") && !lower.contains("stdin"),
            s"generated server must not bake in an I/O loop:\n$server",
          )

          // serverInfo carries name + version.
          assert(
            server.contains("""McpServerInfo("McpTools", "1.0.0")"""),
            s"missing McpServerInfo:\n$server",
          )

          // Tool registry: all 5 tool names present, in declaration order, mapped to methodName.
          expectedToolNames.foreach(n => assert(server.contains(s""""$n""""), s"tool name $n missing:\n$server"))
          val nameOrder = expectedToolNames.map(n => server.indexOf(s""""$n""""))
          assert(nameOrder == nameOrder.sorted && nameOrder.forall(_ >= 0), s"tool names not in declaration order: $nameOrder")
          // Tool entries map to BaboonMethodId with verbatim lowercase wire names.
          assert(
            server.contains("""BaboonMethodId("McpTools", "ping")"""),
            s"ping method id missing:\n$server",
          )

          // inputSchema wired from the T5 emitter: self-contained Draft 2020-12 schemas
          // with the local $defs closure. Probe a couple of emitter-specific fragments.
          // NOTE: the schemas are embedded in Kotlin regular strings; `"` is escaped as `\"`
          // and `$` is escaped as `\$`, so check for the escaped forms in the source text.
          assert(server.contains("https://json-schema.org/draft/2020-12/schema"), s"inputSchema $$schema dialect missing:\n$server")
          assert(server.contains("mcp_stub_Tree"), s"recursive Tree local ref (T5 emitter) missing — inputSchema not wired:\n$server")
          // Check for the Color enum schema; in the Kotlin source the JSON `"enum"` appears
          // as `\"enum\"` (escaped within a regular string literal).
          assert(server.contains("""\"enum\":["""), s"Color enum schema (T5 emitter) missing:\n$server")
          // Schema is carried as a Json.parseToJsonElement literal, not computed at runtime.
          assert(server.contains("Json.parseToJsonElement("), s"inputSchema must be embedded as a Json.parseToJsonElement literal:\n$server")
        }
    }
  }
}
