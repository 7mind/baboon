package io.septimalmind.baboon.tests

import distage.plugins.PluginBase
import io.septimalmind.baboon.*
import io.septimalmind.baboon.CompilerTarget.SwTarget
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

/** T17 codegen-shape test for the Swift MCP server generator.
  *
  * Drives the real `translate` path with `--sw-generate-mcp-server=true`, so the
  * T6 dispatch seam invokes [[io.septimalmind.baboon.translator.swift.SwMcpServerGenerator]],
  * and asserts the emitted Swift MCP server shape for the locked K6 stub model
  * (`mcp-stub-ok`): the additive runtime file, the per-service dispatch class
  * (generic over `Ctx`, conforming to `IBaboonMcpServer`), the transport-abstract
  * `handle` (inherited via the protocol extension, no baked-in I/O loop), the
  * tool registry with the 5 `McpTools_*` tool names in declaration order, and the
  * inputSchema wired from the T5 emitter (embedded as JSONSerialization literals).
  *
  * Mirrors [[DartMcpServerEmissionTest]] (T16).
  */
final class SwiftMcpServerEmissionTest extends SwiftMcpServerEmissionTestBase[Either]

abstract class SwiftMcpServerEmissionTestBase[F[+_, +_]: Error2: TagKK: BaboonTestModule] extends BaboonTest[F] {

  private def swTarget(mcp: Boolean): SwTarget = SwTarget(
    id = "Swift",
    output = OutputOptions(
      safeToRemoveExtensions = Set.empty,
      runtime                = RuntimeGenOpt.With,
      generateConversions    = true,
      output                 = FSPath.parse(NEString.unsafeFrom("./target/baboon-scalatests-sw-mcp/")),
      fixturesOutput         = None,
      testsOutput            = None,
    ),
    generic = GenericOptions(
      codecTestIterations = 0
    ),
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
      generateMcpServer           = mcp,
    ),
  )

  private def moduleFor(target: SwTarget): distage.Module = {
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
    baseModule overriddenBy new BaboonJvmSwModule[Either](target)
  }

  override protected def config: TestConfig = super.config.copy(
    pluginConfig = PluginConfig.const(moduleFor(swTarget(mcp = true)).morph[PluginBase]),
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

  "Swift MCP server generator (mcp-stub-ok fixture)" should {

    "emit a per-service MCP server + runtime with the contract shape when --sw-generate-mcp-server=true" in {
      (loader: BaboonLoader[F], translator: BaboonAbstractTranslator[F]) =>
        for {
          family <- loadStubFamily(loader)
          srcs   <- translator.translate(family)
        } yield {
          val files = srcs.files
          // The additive static runtime resource is emitted.
          assert(
            files.keys.exists(_.endsWith("baboon_mcp_runtime.swift")),
            s"baboon_mcp_runtime.swift not emitted; files=${files.keys}",
          )

          // Exactly one per-service MCP server file (single service McpTools).
          val serverPaths = files.keys.filter(_.endsWith("mcp_tools_mcp_server.swift")).toList
          assert(serverPaths.size == 1, s"expected one mcp_tools_mcp_server.swift, got: $serverPaths")
          val server = files(serverPaths.head).content

          // Class is generic over Ctx and conforms to the transport-abstract protocol.
          assert(
            server.contains("public final class McpToolsMcpServer<Ctx>: IBaboonMcpServer"),
            s"missing generic server class conforming to IBaboonMcpServer:\n$server",
          )
          // It has an injected invokeJson delegate — NO I/O loop.
          assert(
            server.contains("(BaboonMethodId, String, Ctx, BaboonCodecContext) throws -> String"),
            s"missing invokeJson delegate signature:\n$server",
          )
          assert(
            server.contains("public func invokeJson("),
            s"missing invokeJson method:\n$server",
          )
          val lower = server.toLowerCase
          assert(
            !lower.contains("while true") && !lower.contains("readline") && !lower.contains("filehandle.standardinput"),
            s"generated server must not bake in an I/O loop:\n$server",
          )

          // serverInfo carries name + version.
          assert(
            server.contains("""McpServerInfo("McpTools", "1.0.0")"""),
            s"missing McpServerInfo:\n$server",
          )

          // Tool registry: all 5 tool names present, in declaration order, mapped to methodName.
          expectedToolNames.foreach(n => assert(server.contains(s"\"$n\""), s"tool name $n missing:\n$server"))
          val nameOrder = expectedToolNames.map(n => server.indexOf(s"\"$n\""))
          assert(
            nameOrder == nameOrder.sorted && nameOrder.forall(_ >= 0),
            s"tool names not in declaration order: $nameOrder",
          )
          // Tool entries map to BaboonMethodId with verbatim lowercase wire names.
          assert(
            server.contains("""BaboonMethodId(serviceId: "McpTools", methodName: "ping")"""),
            s"ping method id missing:\n$server",
          )

          // inputSchema wired from the T5 emitter: self-contained Draft 2020-12 schemas
          // with the local $defs closure. Probe a couple of emitter-specific fragments.
          // NOTE: in the Swift literal `$`/`"` are escaped (`\"`); probe escaped text.
          assert(
            server.contains("https://json-schema.org/draft/2020-12/schema"),
            s"inputSchema schema dialect missing:\n$server",
          )
          assert(
            server.contains("mcp_stub_Tree"),
            s"recursive Tree local ref (T5 emitter) missing — inputSchema not wired:\n$server",
          )
          // Color enum schema has "enum" key (escaped in the Swift literal).
          assert(
            server.contains("""\"enum\":"""),
            s"Color enum schema (T5 emitter) missing:\n$server",
          )
          // Schema is parsed via JSONSerialization.
          assert(
            server.contains("JSONSerialization.jsonObject"),
            s"inputSchema must be parsed via JSONSerialization:\n$server",
          )
          // The runtime module is imported.
          assert(
            server.contains("import BaboonRuntime"),
            s"BaboonRuntime import missing:\n$server",
          )
        }
    }
  }
}
