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

/** T8 codegen-shape test for the TypeScript MCP server generator.
  *
  * Drives the real `translate` path with `--ts-generate-mcp-server=true`, so the
  * T6 dispatch seam invokes [[io.septimalmind.baboon.translator.typescript.TsMcpServerGenerator]],
  * and asserts the emitted TS MCP server shape for the locked K6 stub model
  * (`mcp-stub-ok`): the additive runtime file, the per-service dispatch class,
  * the transport-abstract `handle` (inherited, no baked-in I/O), the tool
  * registry with the 5 `McpTools_*` tool names in declaration order, and the
  * inputSchema wired from the T5 emitter.
  *
  * The flag-off byte-identity invariant (with `generateMcpServer=false` the TS
  * output is byte-identical to baseline) is verified operationally via a CLI
  * codegen diff (see the task's verification notes), not in this harness — the
  * distage test target binds a single TsTarget per test class.
  */
final class TypeScriptMcpServerEmissionTest extends TypeScriptMcpServerEmissionTestBase[Either]

abstract class TypeScriptMcpServerEmissionTestBase[F[+_, +_]: Error2: TagKK: BaboonTestModule] extends BaboonTest[F] {

  private def tsTarget(mcp: Boolean): TsTarget = TsTarget(
    id = "TypeScript",
    output = OutputOptions(
      safeToRemoveExtensions = Set.empty,
      runtime                = RuntimeGenOpt.With,
      generateConversions    = true,
      output                 = FSPath.parse(NEString.unsafeFrom("./target/baboon-scalatests-ts-mcp/")),
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
      generateMcpServer           = mcp,
    ),
  )

  private def moduleFor(target: TsTarget): distage.Module = {
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
    baseModule overriddenBy new BaboonJvmTsModule[Either](target)
  }

  override protected def config: TestConfig = super.config.copy(
    pluginConfig = PluginConfig.const(moduleFor(tsTarget(mcp = true)).morph[PluginBase]),
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

  "TypeScript MCP server generator (mcp-stub-ok fixture)" should {

    "emit a per-service MCP server + runtime with the contract shape when --ts-generate-mcp-server=true" in {
      (loader: BaboonLoader[F], translator: BaboonAbstractTranslator[F]) =>
        for {
          family <- loadStubFamily(loader)
          srcs   <- translator.translate(family)
        } yield {
          val files = srcs.files
          // The additive static runtime resource is emitted.
          assert(files.keys.exists(_.endsWith("BaboonMcpRuntime.ts")), s"BaboonMcpRuntime.ts not emitted; files=${files.keys}")

          // Exactly one per-service MCP server file (single service McpTools).
          val serverPaths = files.keys.filter(_.endsWith("/mcp-server.ts")).toList
          assert(serverPaths.size == 1, s"expected one mcp-server.ts, got: $serverPaths")
          val server = files(serverPaths.head).content

          // Class is generic over Ctx and extends the transport-abstract base.
          assert(server.contains("export class McpToolsMcpServer<Ctx> extends AbstractBaboonMcpServer<Ctx>"), s"missing generic server class:\n$server")
          // It implements the dispatch contract entrypoint through the runtime base
          // (inherited `handle`), supplying the injected JSON delegate — NO I/O loop.
          assert(server.contains("invokeJson: (method: BaboonMethodId, data: string, ctx: Ctx, codecCtx: BaboonCodecContext) => BaboonEitherResult"), s"missing invokeJson delegate signature:\n$server")
          assert(!server.contains("while (true)") && !server.toLowerCase.contains("stdin") && !server.toLowerCase.contains("createserver"), s"generated server must not bake in an I/O loop:\n$server")

          // serverInfo carries name + version.
          assert(server.contains("""serverInfo: McpServerInfo = { name: "McpTools", version: "1.0.0" }"""), s"missing serverInfo:\n$server")

          // Tool registry: all 5 tool names present, in declaration order, mapped to methodName.
          expectedToolNames.foreach(n => assert(server.contains(s"""name: "$n""""), s"tool name $n missing:\n$server"))
          val nameOrder = expectedToolNames.map(n => server.indexOf(s"""name: "$n""""))
          assert(nameOrder == nameOrder.sorted && nameOrder.forall(_ >= 0), s"tool names not in declaration order: $nameOrder")
          // Tool entries map to BaboonMethodId.
          assert(server.contains("""method: { serviceName: "McpTools", methodName: "ping" }"""), s"ping method id missing:\n$server")

          // inputSchema wired from the T5 emitter: self-contained Draft 2020-12 schemas
          // with the local $defs closure. Probe a couple of emitter-specific fragments.
          assert(server.contains("https://json-schema.org/draft/2020-12/schema"), s"inputSchema $$schema dialect missing:\n$server")
          assert(server.contains("#/$defs/mcp_stub_Tree"), s"recursive Tree local ref (T5 emitter) missing — inputSchema not wired:\n$server")
          assert(server.contains("\"enum\":[\"Red\",\"Green\",\"Blue\"]"), s"Color enum schema (T5 emitter) missing:\n$server")
        }
    }
  }
}
