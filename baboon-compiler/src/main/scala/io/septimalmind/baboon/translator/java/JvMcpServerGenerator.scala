package io.septimalmind.baboon.translator.java

import io.circe.Json
import io.septimalmind.baboon.CompilerTarget.JvTarget
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.translator.mcp.{McpDocs, McpInputSchemaEmitter}
import io.septimalmind.baboon.translator.openapi.OasTypeTranslator
import io.septimalmind.baboon.translator.{BaboonRuntimeResources, McpServerGeneratorHook, OutputFile, Sources}
import io.septimalmind.baboon.typer.model.*
import izumi.functional.bio.{Error2, F}
import izumi.fundamentals.collections.nonempty.NEList

/** Java MCP server generator (T15 — mirrors the T14 Kotlin / T10 C# references).
  *
  * Fills the T6 dispatch seam ([[McpServerGeneratorHook]]): when
  * `--java-generate-mcp-server=true`, the Java translator already calls
  * [[generateMcpServer]]; this supplies the generator behind that hook. It does
  * NOT re-wire dispatch.
  *
  * Per Baboon `service` (in the latest version of each lineage) it emits a
  * transport-abstract `<Service>McpServer<Ctx>` per the dispatch runtime contract
  * (`docs/research/mcp-dispatch-runtime-contract.md`):
  *
  *   - a JSON-RPC `handle(request, session, ctx, codecCtx)` state machine
  *     (initialize / tools/list / tools/call) inherited from the static
  *     `AbstractBaboonMcpServer` runtime base — NO baked-in I/O loop;
  *   - an immutable, declaration-ordered tool registry mapping each method to a
  *     `<serviceName>_<methodName>` tool name and its `inputSchema` (produced by
  *     the shared T5 [[McpInputSchemaEmitter]] — NOT reimplemented here);
  *   - a `tools/call` delegate that routes one tool into the generated service
  *     JSON dispatch and maps its `BaboonEither<BaboonWiringError, String>` result
  *     to MCP Channel-A / Channel-B.
  *
  * Jackson JSON reuse: the MCP runtime uses Jackson `JsonNode` for in-memory JSON
  * (same as the service-wiring layer). The inputSchema JSON literals are embedded
  * as `MAPPER.readTree("...")` calls at construction so the literal text is parsed
  * exactly once and carried as a constant.
  *
  * UEBA is not involved (MCP is JSON-only). The additive runtime types live in
  * the static resource `baboon-runtime/java/BaboonMcpRuntime.java`, emitted
  * alongside the per-service file. With the flag false the hook is never called,
  * so Java output is byte-identical to baseline.
  */
class JvMcpServerGenerator[F[+_, +_]: Error2](
  target: JvTarget,
  typeTranslator: JvTypeTranslator,
  oasTypeTranslator: OasTypeTranslator,
  jvFileTools: JvFileTools,
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
      // Additive MCP runtime files (one class per file, all in baboon.runtime.shared).
      val mcpRuntimeFiles: List[(String, OutputFile)] = List(
        "JsonRpcRequest.java",
        "JsonRpcError.java",
        "JsonRpcResponse.java",
        "McpProtocol.java",
        "McpSession.java",
        "McpToolEntry.java",
        "McpServerInfo.java",
        "IBaboonMcpServer.java",
        "IBaboonRoutableMcpServer.java",
        "McpJsonInvoke.java",
        "AbstractBaboonMcpServer.java",
        "BaboonMcpWiringError.java",
        "BaboonMcpWiringException.java",
        "AbstractMcpMuxer.java",
      ).map { fname =>
        fname -> OutputFile(
          BaboonRuntimeResources.read(s"baboon-runtime/java/$fname"),
          io.septimalmind.baboon.CompilerProduct.Runtime,
        )
      }
      F.pure(Sources((mcpRuntimeFiles ++ perService).toMap))
    }
  }

  private def servicesOf(domain: Domain): List[Typedef.Service] =
    domain.defs.meta.nodes.values.collect {
      case DomainMember.User(_, s: Typedef.Service, _, _) => s
    }.toList.sortBy(_.id.name.name)

  /** A service's MCP server file path, mirroring the per-service directory the
    * service-wiring generator uses (`<basename>/<ServiceName>McpServer.java`).
    */
  private def serverPath(svc: Typedef.Service, domain: Domain, evo: BaboonEvolution): String = {
    val basename = jvFileTools.basename(domain, evo)
    val fname    = s"${svc.id.name.name.capitalize}McpServer.java"
    s"$basename/$fname"
  }

  private def generateForService(svc: Typedef.Service, domain: Domain, evo: BaboonEvolution): (String, OutputFile) = {
    val path = serverPath(svc, domain, evo)

    val serviceName = svc.id.name.name
    val className   = s"${serviceName.capitalize}McpServer"
    val modelVer    = domain.version.v.toString
    val pkg         = typeTranslator.toJvPkg(domain.id, domain.version, evo)
    val pkgStr      = pkg.parts.mkString(".")

    // Declaration-ordered tool entries (K4 §2.3): one per method. The wire
    // tool.name and the BaboonMethodId service/method strings stay verbatim
    // lowercase model names; Java class symbols are capitalized only where
    // appropriate (className). inputSchema literals are embedded as
    // MAPPER.readTree calls so they are parsed once at construction
    // and carried as constants.
    //
    // NOTE: The schema JSON text contains `$` characters (`$schema`, `$ref`,
    // `$defs`). In a Java string literal, `$` is not special and needs no escaping.
    // Backslashes and double-quotes are escaped as `\\` and `\"` respectively.
    val toolEntries: List[String] = svc.methods.toList.map {
      m =>
        val toolName    = s"${serviceName}_${m.name.name}"
        val schema      = schemaEmitter.emitInputSchema(m.sig, domain)
        // Embed schema as a Java string literal via javaString which handles
        // all escaping (`\` → `\\`, `"` → `\"`). `$` is literal in Java.
        val schemaJson   = schema.noSpaces
        val descArg      = McpDocs.flatten(m.docs).map(d => s", ${javaString(d)}").getOrElse("")
        s"""            new McpToolEntry(${javaString(toolName)}, new BaboonMethodId(${javaString(serviceName)}, ${javaString(m.name.name)}), parseSchema(${javaString(schemaJson)})$descArg)"""
    }

    val content =
      s"""package $pkgStr;
         |
         |import baboon.runtime.shared.AbstractBaboonMcpServer;
         |import baboon.runtime.shared.BaboonCodecContext;
         |import baboon.runtime.shared.BaboonEither;
         |import baboon.runtime.shared.BaboonMethodId;
         |import baboon.runtime.shared.BaboonWiringError;
         |import baboon.runtime.shared.McpJsonInvoke;
         |import baboon.runtime.shared.McpServerInfo;
         |import baboon.runtime.shared.McpSession;
         |import baboon.runtime.shared.McpToolEntry;
         |import com.fasterxml.jackson.databind.JsonNode;
         |
         |import java.util.List;
         |
         |// Generated MCP server for service `$serviceName` (model `${domain.id.path.mkString(".")}` v$modelVer).
         |// Transport-abstract: `handle` is inherited from AbstractBaboonMcpServer and
         |// performs no I/O. The `invokeJson` delegate routes `tools/call` into the
         |// generated service dispatch; the integrator supplies it (typically the
         |// errors-mode `${serviceName}Wiring.invokeJson` bound to this service) plus the
         |// per-request `Ctx`.
         |public final class $className<Ctx> extends AbstractBaboonMcpServer<Ctx> {
         |    private final McpJsonInvoke<Ctx> _invokeJson;
         |
         |    public $className(McpJsonInvoke<Ctx> invokeJson) {
         |        this._invokeJson = invokeJson;
         |    }
         |
         |    private static JsonNode parseSchema(String json) {
         |        try {
         |            return MAPPER.readTree(json);
         |        } catch (Exception e) {
         |            throw new RuntimeException("BUG: failed to parse embedded MCP inputSchema literal", e);
         |        }
         |    }
         |
         |    @Override
         |    public McpServerInfo serverInfo() {
         |        return new McpServerInfo(${javaString(serviceName)}, ${javaString(modelVer)});
         |    }
         |
         |    @Override
         |    public List<McpToolEntry> tools() {
         |        return List.of(
         |${toolEntries.mkString(",\n")}
         |        );
         |    }
         |
         |    @Override
         |    protected BaboonEither<BaboonWiringError, String> invokeJson(
         |            BaboonMethodId method, String data, Ctx ctx, BaboonCodecContext codecCtx) {
         |        return _invokeJson.invoke(method, data, ctx, codecCtx);
         |    }
         |}
         |""".stripMargin

    path -> OutputFile(content, io.septimalmind.baboon.CompilerProduct.Definition)
  }

  private def javaString(s: String): String =
    "\"" + s.replace("\\", "\\\\").replace("\"", "\\\"") + "\""
}
