package io.septimalmind.baboon.translator.typescript

import io.circe.Json
import io.septimalmind.baboon.CompilerTarget.TsTarget
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.translator.mcp.McpInputSchemaEmitter
import io.septimalmind.baboon.translator.openapi.OasTypeTranslator
import io.septimalmind.baboon.translator.{BaboonRuntimeResources, McpServerGeneratorHook, OutputFile, Sources}
import io.septimalmind.baboon.typer.model.*
import izumi.functional.bio.{Error2, F}
import izumi.fundamentals.collections.nonempty.NEList

/** TypeScript MCP server generator (T8 — the reference implementation T10/C# and
  * T12–T18 replicate).
  *
  * Fills the T6 dispatch seam ([[McpServerGeneratorHook]]): when
  * `--ts-generate-mcp-server=true`, the TS translator already calls
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
  *     JSON dispatch and maps its `BaboonEither<BaboonWiringError, string>`
  *     result to MCP Channel-A / Channel-B.
  *
  * UEBA is not involved (MCP is JSON-only). The additive runtime types live in
  * the static resource `baboon-runtime/typescript/BaboonMcpRuntime.ts`, emitted
  * alongside the per-service file. With the flag false the hook is never called,
  * so TS output is byte-identical to baseline.
  */
class TsMcpServerGenerator[F[+_, +_]: Error2](
  target: TsTarget,
  typeTranslator: TsTypeTranslator,
  oasTypeTranslator: OasTypeTranslator,
  tsFileTools: TsFileTools,
) extends McpServerGeneratorHook[F] {

  private val schemaEmitter = new McpInputSchemaEmitter(oasTypeTranslator)
  private val sfx: String   = target.language.importSuffix

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
        "BaboonMcpRuntime.ts" -> OutputFile(
          BaboonRuntimeResources.read("baboon-runtime/typescript/BaboonMcpRuntime.ts"),
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
    * service-wiring generator uses (`<basename>/<serviceDir>/mcp-server.ts`).
    */
  private def serverPath(svc: Typedef.Service, basename: String): String = {
    val serviceDir = typeTranslator.serviceDirSegments(svc).mkString("/")
    s"$basename/$serviceDir/mcp-server.ts"
  }

  private def generateForService(svc: Typedef.Service, domain: Domain, evo: BaboonEvolution): (String, OutputFile) = {
    val basename = tsFileTools.basename(domain, evo)
    val path     = serverPath(svc, basename)
    // Relative prefix from the server file's directory back to the generated
    // root where the runtime files (BaboonSharedRuntime, BaboonMcpRuntime) live.
    val depth   = path.split('/').length - 1
    val rootRel = "../" * depth

    val serviceName = svc.id.name.name
    val className   = s"${serviceName.capitalize}McpServer"
    val modelVer    = domain.version.v.toString

    // Declaration-ordered tool entries (K4 §2.3): one per method.
    val toolEntries: List[String] = svc.methods.toList.map {
      m =>
        val toolName = s"${serviceName}_${m.name.name}"
        val schema   = schemaEmitter.emitInputSchema(m.sig, domain)
        // The self-contained JSON Schema is carried as a constant literal value.
        s"""        { name: ${jsString(toolName)}, method: { serviceName: ${jsString(serviceName)}, methodName: ${jsString(m.name.name)} }, inputSchema: ${jsonLiteral(schema)} },"""
    }

    // Async axis (`--ts-async-services=true`): the errors-mode wiring entry
    // `dispatchJson` is `async` and returns `Promise<BaboonEither<…>>`
    // (TsServiceWiringTranslator.scala:484/490), so the MCP server must extend
    // the async runtime base, take a `Promise`-returning delegate, and `await`
    // it in its `invokeJson` override (which is itself `async`; the inherited
    // async `handle` awaits the `tools/call` dispatch). When OFF the sync branch
    // is emitted verbatim, keeping the generated file byte-identical to the
    // pre-change baseline.
    val isAsync = target.language.asyncServices

    val baseClass     = if (isAsync) "AbstractAsyncBaboonMcpServer" else "AbstractBaboonMcpServer"
    val delegateRet   = if (isAsync) "Promise<BaboonEitherResult>" else "BaboonEitherResult"
    val invokeJsonRet = if (isAsync) "Promise<BaboonEitherResult>" else "BaboonEitherResult"
    val asyncPrefix   = if (isAsync) "async " else ""

    val content =
      s"""import { $baseClass, McpServerInfo, McpToolEntry, BaboonEitherResult } from '${rootRel}BaboonMcpRuntime$sfx';
         |import { BaboonCodecContext, BaboonMethodId } from '${rootRel}BaboonSharedRuntime$sfx';
         |
         |// Generated MCP server for service `$serviceName` (model `${domain.id.path.mkString(".")}` v$modelVer).
         |// Transport-abstract: `handle` is inherited from $baseClass and
         |// performs no I/O. The `invokeJson` delegate routes `tools/call` into the
         |// generated service dispatch; the integrator supplies it (typically the
         |// errors-mode `dispatchJson` bound to this service) plus the per-request `Ctx`.
         |export class $className<Ctx> extends $baseClass<Ctx> {
         |    public readonly serverInfo: McpServerInfo = { name: ${jsString(serviceName)}, version: ${jsString(modelVer)} };
         |
         |    public readonly tools: readonly McpToolEntry[] = [
         |${toolEntries.mkString("\n")}
         |    ];
         |
         |    private readonly _invokeJson: (method: BaboonMethodId, data: string, ctx: Ctx, codecCtx: BaboonCodecContext) => $delegateRet;
         |
         |    constructor(invokeJson: (method: BaboonMethodId, data: string, ctx: Ctx, codecCtx: BaboonCodecContext) => $delegateRet) {
         |        super();
         |        this._invokeJson = invokeJson;
         |    }
         |
         |    protected ${asyncPrefix}invokeJson(method: BaboonMethodId, data: string, ctx: Ctx, codecCtx: BaboonCodecContext): $invokeJsonRet {
         |        return this._invokeJson(method, data, ctx, codecCtx);
         |    }
         |}
         |""".stripMargin

    path -> OutputFile(content, io.septimalmind.baboon.CompilerProduct.Definition)
  }

  /** Render a circe `Json` as a TypeScript object literal. `Json.noSpaces`
    * already produces valid JSON, which is also a valid TS expression; the
    * schema contains only objects, arrays, strings, numbers and booleans (no
    * `undefined`/functions), so the JSON text is a sound TS literal verbatim.
    */
  private def jsonLiteral(j: Json): String = j.noSpaces

  private def jsString(s: String): String = Json.fromString(s).noSpaces
}
