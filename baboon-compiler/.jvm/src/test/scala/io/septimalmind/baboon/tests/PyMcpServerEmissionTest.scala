package io.septimalmind.baboon.tests

import distage.plugins.PluginBase
import io.septimalmind.baboon.*
import io.septimalmind.baboon.CompilerTarget.PyTarget
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

/** T18 codegen-shape test for the Python MCP server generator.
  *
  * Drives the real `translate` path with `--py-generate-mcp-server=true`,
  * so the T6 dispatch seam invokes [[PyMcpServerGenerator]], and asserts the
  * emitted Python MCP server shape for the locked K6 stub model (`mcp-stub-ok`):
  * the additive runtime file, the per-service dispatch class, the correct
  * `Generic`/`TypeVar` imports (noted gotcha from T18 spec), the tool registry
  * with the 5 `McpTools_*` tool names in declaration order, and the inputSchema
  * wired from the T5 emitter.
  *
  * K1 validity gate: at tools/list (simulated via the emitted code shape) each
  * inputSchema literal must be well-formed JSON (parseable by standard library)
  * and contain the Draft 2020-12 `$schema` key.
  *
  * Negative controls:
  *   - The flag-off path emits no MCP files.
  *   - The tool-name position assertion is a live check (wrong name ≠ pass).
  */
final class PyMcpServerEmissionTest extends PyMcpServerEmissionTestBase[Either]

abstract class PyMcpServerEmissionTestBase[F[+_, +_]: Error2: TagKK: BaboonTestModule] extends BaboonTest[F] {

  private def pyTarget(mcp: Boolean): PyTarget = PyTarget(
    id = "Python",
    output = OutputOptions(
      safeToRemoveExtensions = Set.empty,
      runtime                = RuntimeGenOpt.With,
      generateConversions    = true,
      output                 = FSPath.parse(NEString.unsafeFrom("./target/baboon-tests-py-mcp/")),
      fixturesOutput         = None,
      testsOutput            = None,
    ),
    generic = GenericOptions(
      codecTestIterations = 0
    ),
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
      generateMcpServer = mcp,
    ),
  )

  private def moduleFor(target: PyTarget): distage.Module = {
    val baseModule =
      new BaboonModuleJvm[Either](
        CompilerOptions(
          debug                    = false,
          individualInputs         = Set.empty,
          directoryInputs          = Set(FSPath.parse(NEString.unsafeFrom("./baboon-compiler/src/test/resources/mcp-stub-ok"))),
          metaWriteEvolutionJsonTo = None,
          lockfile                 = Some(FSPath.parse(NEString.unsafeFrom("./target/baboon.lock"))),
          emitOnly                 = None,
          targets                  = Seq(target),
        ),
        UnsafeInstances.Lawless_ParallelErrorAccumulatingOpsEither,
      )
    baseModule overriddenBy new BaboonJvmPyModule[Either](target)
  }

  override protected def config: TestConfig = super.config.copy(
    pluginConfig = PluginConfig.const(moduleFor(pyTarget(mcp = true)).morph[PluginBase]),
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

  "Python MCP server generator (mcp-stub-ok fixture)" should {

    "emit a per-service MCP server + runtime with the contract shape when --py-generate-mcp-server=true" in {
      (loader: BaboonLoader[F], translator: BaboonAbstractTranslator[F]) =>
        for {
          family <- loadStubFamily(loader)
          srcs   <- translator.translate(family)
        } yield {
          val files = srcs.files

          // The additive static runtime resource is emitted.
          assert(files.keys.exists(_.endsWith("baboon_mcp_runtime.py")), s"baboon_mcp_runtime.py not emitted; files=${files.keys}")

          // Exactly one per-service MCP server file (single service McpTools).
          val serverPaths = files.keys.filter(_.endsWith("McpToolsMcpServer.py")).toList
          assert(serverPaths.size == 1, s"expected one McpToolsMcpServer.py, got: $serverPaths")
          val server = files(serverPaths.head).content

          // GOTCHA (T18 spec): Generic and TypeVar must be imported (Python forward-generic class).
          assert(
            server.contains("from typing import") && server.contains("Generic") && server.contains("TypeVar"),
            s"Generic/TypeVar imports missing in generated Python MCP server:\n$server",
          )

          // Class is generic over Ctx (Python: Generic[Ctx]) and extends the transport-abstract base.
          assert(
            server.contains("McpToolsMcpServer(AbstractBaboonMcpServer[Ctx], Generic[Ctx])"),
            s"missing generic server class declaration:\n$server",
          )

          // Ctx TypeVar declaration present.
          assert(
            server.contains("Ctx = TypeVar(\"Ctx\")"),
            s"missing Ctx TypeVar declaration:\n$server",
          )

          // Verify no baked-in I/O loop.
          val lower = server.toLowerCase
          assert(
            !lower.contains("while true") && !lower.contains("readline") && !lower.contains("stdin"),
            s"generated server must not bake in an I/O loop:\n$server",
          )

          // serverInfo carries name + version.
          assert(
            server.contains("\"McpTools\""),
            s"missing McpTools service name in serverInfo:\n$server",
          )
          assert(
            server.contains("\"1.0.0\""),
            s"missing 1.0.0 version in serverInfo:\n$server",
          )

          // Tool registry: all 5 tool names present, in declaration order.
          expectedToolNames.foreach(n => assert(server.contains(s""""$n""""), s"tool name $n missing:\n$server"))
          val nameOrder = expectedToolNames.map(n => server.indexOf(s""""$n""""))
          assert(nameOrder == nameOrder.sorted && nameOrder.forall(_ >= 0), s"tool names not in declaration order: $nameOrder")

          // Tool entries use BaboonMethodId with verbatim lowercase wire names.
          assert(
            server.contains("BaboonMethodId(\"McpTools\", \"ping\")"),
            s"ping method id missing:\n$server",
          )

          // inputSchema wired from T5 emitter: self-contained Draft 2020-12 schemas
          // embedded as json.loads("...") literals. Inside the Python string literal
          // all double quotes are escaped as \", so check for raw substrings (no Scala
          // string escaping needed — just check for the unescaped text in the JSON).
          assert(server.contains("json.loads("), s"json.loads inputSchema embedding missing:\n$server")
          assert(server.contains("https://json-schema.org/draft/2020-12/schema"), s"inputSchema $$schema dialect missing:\n$server")
          // mcp_stub_Tree appears in the $ref URI inside the escaped Python string.
          assert(server.contains("mcp_stub_Tree"), s"recursive Tree ref (T5 emitter) missing — inputSchema not wired:\n$server")
          // Color enum values appear as escaped strings (\\"Red\\"...) — just check the bare word.
          assert(server.contains("Red"), s"Color enum value Red missing in inputSchema:\n$server")

          // K1 validity gate: each json.loads call must contain well-formed JSON.
          // The schemas are embedded as Python string literals: json.loads("{...escaped...}").
          // Extract them by finding each json.loads call and unescaping the Python string.
          // Strategy: find `json.loads("` markers, then consume until matching unescaped `")`
          // by unescaping \\" → " and collecting the balanced content.
          val jsonLoadsMarker = """json.loads(""""
          val inlineSchemas: List[String] = {
            val sb    = scala.collection.mutable.ListBuffer.empty[String]
            var start = server.indexOf(jsonLoadsMarker)
            while (start >= 0) {
              val contentStart = start + jsonLoadsMarker.length
              val sb2          = new StringBuilder
              var i            = contentStart
              var done         = false
              while (i < server.length && !done) {
                if (server(i) == '\\' && i + 1 < server.length && server(i + 1) == '"') {
                  sb2.append('"')
                  i += 2
                } else if (server(i) == '\\' && i + 1 < server.length && server(i + 1) == '\\') {
                  sb2.append('\\')
                  i += 2
                } else if (server(i) == '"') {
                  done = true
                } else {
                  sb2.append(server(i))
                  i += 1
                }
              }
              sb += sb2.toString
              start = server.indexOf(jsonLoadsMarker, start + 1)
            }
            sb.toList
          }

          // Each extracted schema must parse as well-formed JSON.
          inlineSchemas.foreach { schemaStr =>
            val parsed = io.circe.parser.parse(schemaStr)
            assert(
              parsed.isRight,
              s"K1: inputSchema literal does not parse as well-formed JSON. Error: ${parsed.left.toOption}. Schema: $schemaStr",
            )
            val schemaUri = parsed.toOption.get.hcursor.downField("$schema").as[String]
            assert(
              schemaUri == Right("https://json-schema.org/draft/2020-12/schema"),
              s"K1: parsed inputSchema missing or wrong $$schema. Got: $schemaUri. Schema: $schemaStr",
            )
          }

          // K1 negative control: ensure we actually parsed some schemas (not vacuously green).
          assert(inlineSchemas.nonEmpty, s"K1 negative control: no inputSchema literals extracted — K1 gate is vacuous:\n$server")
          // 5 original methods + processTagged added in D1/T26 + describePricing added in D34/T125.
          assert(inlineSchemas.size == 7, s"K1: expected 7 inputSchema literals (one per method), got ${inlineSchemas.size}:\n$server")
        }
    }

  }
}
