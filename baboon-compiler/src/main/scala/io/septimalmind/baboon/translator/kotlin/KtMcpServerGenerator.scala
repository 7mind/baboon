package io.septimalmind.baboon.translator.kotlin

import io.circe.Json
import io.septimalmind.baboon.CompilerTarget.KtTarget
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.translator.mcp.{McpDocs, McpInputSchemaEmitter}
import io.septimalmind.baboon.translator.openapi.OasTypeTranslator
import io.septimalmind.baboon.translator.{BaboonRuntimeResources, McpServerGeneratorHook, OutputFile, Sources}
import io.septimalmind.baboon.typer.model.*
import izumi.functional.bio.{Error2, F}
import izumi.fundamentals.collections.nonempty.NEList

/** Kotlin MCP server generator (T14 — mirrors the T8 TypeScript / T10 C# references).
  *
  * Fills the T6 dispatch seam ([[McpServerGeneratorHook]]): when
  * `--kt-generate-mcp-server=true`, the Kotlin translator already calls
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
  *     JSON dispatch and maps its `Either<BaboonWiringError, String>` result to
  *     MCP Channel-A / Channel-B.
  *
  * Jackson / kotlinx-serialization JSON reuse: the MCP runtime uses kotlinx-serialization
  * `JsonElement` for in-memory JSON (same as the service-wiring layer). The inputSchema
  * JSON literals are embedded as `Json.parseToJsonElement("""...""")` calls at
  * construction so the literal text is parsed exactly once and carried as a constant.
  *
  * UEBA is not involved (MCP is JSON-only). The additive runtime types live in
  * the static resource `baboon-runtime/kotlin/BaboonMcpRuntime.kt`, emitted
  * alongside the per-service file. With the flag false the hook is never called,
  * so Kotlin output is byte-identical to baseline.
  */
class KtMcpServerGenerator[F[+_, +_]: Error2](
  target: KtTarget,
  typeTranslator: KtTypeTranslator,
  oasTypeTranslator: OasTypeTranslator,
  ktFileTools: KtFileTools,
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
        "BaboonMcpRuntime.kt" -> OutputFile(
          BaboonRuntimeResources.read("baboon-runtime/kotlin/BaboonMcpRuntime.kt"),
          io.septimalmind.baboon.CompilerProduct.Runtime,
        )
      F.pure(Sources((runtimeFile :: perService).toMap))
    }
  }

  private def servicesOf(domain: Domain): List[Typedef.Service] =
    domain.defs.meta.nodes.values.collect {
      case DomainMember.User(_, s: Typedef.Service, _, _) => s
    }.toList.sortBy(_.id.name.name)

  /** A service's MCP server file path, mirroring the per-service directory the
    * service-wiring generator uses (`<basename>/<pkgPath>/<ServiceName>McpServer.kt`).
    */
  private def serverPath(svc: Typedef.Service, domain: Domain, evo: BaboonEvolution): String = {
    val basename = ktFileTools.basename(domain, evo)
    val fname    = s"${svc.id.name.name.capitalize}McpServer.kt"
    s"$basename/$fname"
  }

  private def generateForService(svc: Typedef.Service, domain: Domain, evo: BaboonEvolution): (String, OutputFile) = {
    val path = serverPath(svc, domain, evo)

    val serviceName = svc.id.name.name
    val className   = s"${serviceName.capitalize}McpServer"
    val modelVer    = domain.version.v.toString
    val pkg         = typeTranslator.toKtPkg(domain.id, domain.version, evo)
    val pkgStr      = pkg.parts.mkString(".")

    // Declaration-ordered tool entries (K4 §2.3): one per method. The wire
    // tool.name and the BaboonMethodId service/method strings stay verbatim
    // lowercase model names; Kotlin class symbols are capitalized only where
    // appropriate (className). inputSchema literals are embedded as
    // Json.parseToJsonElement calls so they are parsed once at construction
    // and carried as constants.
    //
    // NOTE: We use a regular Kotlin string (not triple-quoted) because the JSON
    // schema text contains `$` characters (`$schema`, `$ref`, `$defs`) that
    // Kotlin string templates would interpret as interpolation starts in raw
    // strings. In a regular Kotlin string, `\$` is a literal `$` (Kotlin allows
    // `\$` as an escape in regular strings, but not in raw strings).
    val toolEntries: List[String] = svc.methods.toList.map {
      m =>
        val toolName = s"${serviceName}_${m.name.name}"
        val schema   = schemaEmitter.emitInputSchema(m.sig, domain)
        // Embed schema as a regular Kotlin string literal. Escape rules for
        // a Kotlin non-raw string: `\` → `\\`, `"` → `\"`, `$` → `\$`.
        val schemaLiteral = schema.noSpaces
          .replace("\\", "\\\\")
          .replace("\"", "\\\"")
          .replace("$", "\\$")
        val descArg = McpDocs.flatten(m.docs).map(d => s", description = ${ktDescString(d)}").getOrElse("")
        s"""        McpToolEntry(${ktString(toolName)}, BaboonMethodId(${ktString(serviceName)}, ${ktString(m.name.name)}), Json.parseToJsonElement("$schemaLiteral")$descArg),"""
    }

    val content =
      s"""package $pkgStr
         |
         |import baboon.runtime.shared.AbstractBaboonMcpServer
         |import baboon.runtime.shared.BaboonCodecContext
         |import baboon.runtime.shared.BaboonMethodId
         |import baboon.runtime.shared.BaboonWiringError
         |import baboon.runtime.shared.Either
         |import baboon.runtime.shared.McpJsonInvoke
         |import baboon.runtime.shared.McpServerInfo
         |import baboon.runtime.shared.McpSession
         |import baboon.runtime.shared.McpToolEntry
         |import kotlinx.serialization.json.Json
         |
         |// Generated MCP server for service `$serviceName` (model `${domain.id.path.mkString(".")}` v$modelVer).
         |// Transport-abstract: `handle` is inherited from AbstractBaboonMcpServer and
         |// performs no I/O. The `invokeJson` delegate routes `tools/call` into the
         |// generated service dispatch; the integrator supplies it (typically the
         |// errors-mode `${serviceName}Wiring.invokeJson` bound to this service) plus the
         |// per-request `Ctx`.
         |class $className<Ctx>(
         |    private val _invokeJson: McpJsonInvoke<Ctx>,
         |) : AbstractBaboonMcpServer<Ctx>() {
         |    override val serverInfo: McpServerInfo = McpServerInfo(${ktString(serviceName)}, ${ktString(modelVer)})
         |
         |    override val tools: List<McpToolEntry> = listOf(
         |${toolEntries.mkString("\n")}
         |    )
         |
         |    override fun invokeJson(method: BaboonMethodId, data: String, ctx: Ctx, codecCtx: BaboonCodecContext): Either<BaboonWiringError, String> {
         |        return _invokeJson(method, data, ctx, codecCtx)
         |    }
         |}
         |""".stripMargin

    path -> OutputFile(content, io.septimalmind.baboon.CompilerProduct.Definition)
  }

  /** Embed a description string as a Kotlin non-raw double-quoted literal.
    * Uses Circe's JSON encoding to handle control characters (including `\n`),
    * then additionally escapes `$` → `\$` because Kotlin non-raw `"..."` literals
    * interpret bare `$` as string-template interpolation starts.
    * JSON encoding does not escape `$`, so the extra replace is required.
    */
  private def ktDescString(s: String): String =
    Json.fromString(s).noSpaces.replace("$", "\\$")

  private def ktString(s: String): String =
    "\"" + s.replace("\\", "\\\\").replace("\"", "\\\"") + "\""
}
