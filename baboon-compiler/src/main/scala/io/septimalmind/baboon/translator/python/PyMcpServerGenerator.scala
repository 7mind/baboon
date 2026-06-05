package io.septimalmind.baboon.translator.python

import io.circe.Json
import io.septimalmind.baboon.CompilerProduct
import io.septimalmind.baboon.CompilerTarget.PyTarget
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.translator.mcp.McpInputSchemaEmitter
import io.septimalmind.baboon.translator.openapi.OasTypeTranslator
import io.septimalmind.baboon.translator.{BaboonRuntimeResources, McpServerGeneratorHook, OutputFile, Sources}
import io.septimalmind.baboon.typer.model.*
import izumi.functional.bio.{Error2, F}
import izumi.fundamentals.collections.nonempty.NEList

/** Python MCP server generator (T18 — mirrors the T8 TypeScript / T10 C# references).
  *
  * Fills the T6 dispatch seam ([[McpServerGeneratorHook]]): when
  * `--python-generate-mcp-server=true`, the Python translator already calls
  * [[generateMcpServer]]; this supplies the generator behind that hook. It does
  * NOT re-wire dispatch.
  *
  * Per Baboon `service` (in the latest version of each lineage) it emits a
  * transport-abstract `<Service>McpServer[Ctx]` per the dispatch runtime contract
  * (`docs/research/mcp-dispatch-runtime-contract.md`):
  *
  *   - a JSON-RPC `handle(request, session, ctx, codec_ctx)` state machine
  *     (initialize / tools/list / tools/call) inherited from the static
  *     `AbstractBaboonMcpServer` runtime base — NO baked-in I/O loop;
  *   - an immutable, declaration-ordered tool registry mapping each method to a
  *     `<serviceName>_<methodName>` tool name and its `input_schema` (produced by
  *     the shared T5 [[McpInputSchemaEmitter]] — NOT reimplemented here);
  *   - an `invoke_json` delegate that routes one tool into the generated service
  *     JSON dispatch and maps its `BaboonEither[BaboonWiringError, str]` result to
  *     MCP Channel-A / Channel-B.
  *
  * UEBA is not involved (MCP is JSON-only). The additive runtime types live in
  * the static resource `baboon-runtime/python/baboon_mcp_runtime.py`, emitted
  * alongside the per-service file. With the flag false the hook is never called,
  * so Python output is byte-identical to baseline.
  *
  * Note on imports: the generated per-service file imports `Generic` and
  * `TypeVar` from `typing` explicitly; these are required for the `[Ctx]`
  * generic class declaration in Python.
  */
class PyMcpServerGenerator[F[+_, +_]: Error2](
  target: PyTarget,
  oasTypeTranslator: OasTypeTranslator,
  pyFileTools: PyFileTools,
) extends McpServerGeneratorHook[F] {

  private val schemaEmitter = new McpInputSchemaEmitter(oasTypeTranslator)

  override def generateMcpServer(family: BaboonFamily): F[NEList[BaboonIssue], Sources] = {
    val perService: List[(String, OutputFile)] = family.domains.toMap.values.toList.flatMap {
      lineage =>
        val evo          = lineage.evolution
        val latestDomain = lineage.versions(evo.latest)
        servicesOf(latestDomain).map(svc => generateForService(svc, latestDomain, evo))
    }

    if (perService.isEmpty) {
      F.pure(Sources(Map.empty))
    } else {
      val runtimeFile =
        "baboon_mcp_runtime.py" -> OutputFile(
          BaboonRuntimeResources.read("baboon-runtime/python/baboon_mcp_runtime.py"),
          CompilerProduct.Runtime,
        )
      F.pure(Sources((runtimeFile :: perService).toMap))
    }
  }

  private def servicesOf(domain: Domain): List[Typedef.Service] =
    domain.defs.meta.nodes.values.collect {
      case DomainMember.User(_, s: Typedef.Service, _, _) => s
    }.toList.sortBy(_.id.name.name)

  /** A service's MCP server file path.
    * Places the file alongside the service-wiring files: `<basename>/<Service>McpServer.py`
    */
  private def serverPath(svc: Typedef.Service, domain: Domain, evo: BaboonEvolution): String = {
    val basename    = pyFileTools.basename(domain, evo)
    val serviceName = svc.id.name.name
    s"$basename/${serviceName}McpServer.py"
  }

  private def generateForService(svc: Typedef.Service, domain: Domain, evo: BaboonEvolution): (String, OutputFile) = {
    val path = serverPath(svc, domain, evo)

    val serviceName = svc.id.name.name
    val className   = s"${serviceName}McpServer"
    val modelVer    = domain.version.v.toString

    // The import prefix mirrors how PyBaboonTranslator.renderTree resolves
    // baboon runtime module imports: baboon runtime modules (baboon_mcp_runtime,
    // baboon_service_wiring, …) are prefixed with the definitions base package
    // so that e.g. `from Generated.baboon_mcp_runtime import …` matches the
    // test harness layout (test is run from the parent of the `Generated/`
    // directory, with `Generated` as the package root).
    val basePkg = pyFileTools.definitionsBasePkg.mkString(".")
    val rtPkg   = if (basePkg.nonEmpty) s"$basePkg." else ""

    // Declaration-ordered tool entries (K4 §2.3): one per method. The wire
    // tool name and BaboonMethodId service/method strings stay verbatim
    // lowercase model names. inputSchema literals are embedded as pre-parsed
    // Python dicts (json.loads of the JSON text) so they are constant values.
    val toolEntries: List[String] = svc.methods.toList.map {
      m =>
        val toolName   = s"${serviceName}_${m.name.name}"
        val schema     = schemaEmitter.emitInputSchema(m.sig, domain)
        val schemaJson = schema.noSpaces
          .replace("\\", "\\\\")
          .replace("\"", "\\\"")
        s"""            McpToolEntry(${pyString(toolName)}, BaboonMethodId(${pyString(serviceName)}, ${pyString(m.name.name)}), json.loads("$schemaJson")),"""
    }

    val invokeFnName = s"invoke_json_$serviceName"

    val content =
      s"""# Generated MCP server for service `$serviceName` (model `${domain.id.path.mkString(".")}` v$modelVer).
         |# Transport-abstract: `handle` is inherited from AbstractBaboonMcpServer and
         |# performs no I/O. The `_invoke_json` delegate routes `tools/call` into the
         |# generated service dispatch; the integrator supplies it (typically the
         |# errors-mode `$invokeFnName` bound to this service) plus the per-request `ctx`.
         |import json
         |from typing import Callable, Generic, List, TypeVar
         |
         |from ${rtPkg}baboon_mcp_runtime import AbstractBaboonMcpServer, McpServerInfo, McpSession, McpToolEntry
         |from ${rtPkg}baboon_service_wiring import BaboonLeft, BaboonMethodId, BaboonWiringError
         |
         |Ctx = TypeVar("Ctx")
         |
         |
         |class $className(AbstractBaboonMcpServer[Ctx], Generic[Ctx]):
         |    def __init__(self, invoke_json: Callable[[BaboonMethodId, str, Ctx, object], object]) -> None:
         |        self._invoke_json = invoke_json
         |
         |    @property
         |    def server_info(self) -> McpServerInfo:
         |        return McpServerInfo(${pyString(serviceName)}, ${pyString(modelVer)})
         |
         |    @property
         |    def tools(self) -> List[McpToolEntry]:
         |        return [
         |${toolEntries.mkString("\n")}
         |        ]
         |
         |    def invoke_json(self, method: BaboonMethodId, data: str, ctx: Ctx, codec_ctx: object) -> object:
         |        return self._invoke_json(method, data, ctx, codec_ctx)
         |""".stripMargin

    path -> OutputFile(content, CompilerProduct.Definition)
  }

  private def pyString(s: String): String = Json.fromString(s).noSpaces
}
