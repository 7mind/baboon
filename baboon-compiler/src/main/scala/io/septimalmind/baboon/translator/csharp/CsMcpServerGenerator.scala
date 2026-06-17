package io.septimalmind.baboon.translator.csharp

import io.circe.Json
import io.septimalmind.baboon.CompilerTarget.CSTarget
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.translator.mcp.{McpDocs, McpInputSchemaEmitter}
import io.septimalmind.baboon.translator.openapi.OasTypeTranslator
import io.septimalmind.baboon.translator.{BaboonRuntimeResources, McpServerGeneratorHook, OutputFile, Sources}
import io.septimalmind.baboon.typer.model.*
import izumi.functional.bio.{Error2, F}
import izumi.fundamentals.collections.nonempty.NEList

/** C# MCP server generator (T10 — mirrors the T8 TypeScript reference).
  *
  * Fills the T6 dispatch seam ([[McpServerGeneratorHook]]): when
  * `--cs-generate-mcp-server=true`, the C# translator already calls
  * [[generateMcpServer]]; this supplies the generator behind that hook. It does
  * NOT re-wire dispatch.
  *
  * Per Baboon `service` (in the latest version of each lineage) it emits a
  * transport-abstract `<Service>McpServer<Ctx>` per the dispatch runtime contract
  * (`docs/research/mcp-dispatch-runtime-contract.md`):
  *
  *   - a JSON-RPC `Handle(request, session, ctx, codecCtx)` state machine
  *     (initialize / tools/list / tools/call) inherited from the static
  *     `AbstractBaboonMcpServer` runtime base — NO baked-in I/O loop;
  *   - an immutable, declaration-ordered tool registry mapping each method to a
  *     `<serviceName>_<methodName>` tool name and its `inputSchema` (produced by
  *     the shared T5 [[McpInputSchemaEmitter]] — NOT reimplemented here);
  *   - a `tools/call` delegate that routes one tool into the generated service
  *     JSON dispatch and maps its `Either<BaboonWiringError, string>` result to
  *     MCP Channel-A / Channel-B.
  *
  * C#-only PascalCase convention (abstract-context service work): internal C#
  * symbols (`Handle`, `InputSchema`, `ServerInfo`) are PascalCase, but the wire
  * `tool.name`, the JSON-RPC method strings, and the JSON result keys
  * (`"inputSchema"`, `"protocolVersion"` …) stay lowercase literals so
  * tool-name matching is byte-identical across all backends.
  *
  * UEBA is not involved (MCP is JSON-only). The additive runtime types live in
  * the static resource `baboon-runtime/cs/BaboonMcpRuntime.cs`, emitted
  * alongside the per-service file. With the flag false the hook is never called,
  * so C# output is byte-identical to baseline.
  */
class CsMcpServerGenerator[F[+_, +_]: Error2](
  target: CSTarget,
  typeTranslator: CSTypeTranslator,
  oasTypeTranslator: OasTypeTranslator,
  csFileTools: CSFileTools,
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
        "BaboonMcpRuntime.cs" -> OutputFile(
          BaboonRuntimeResources.read("baboon-runtime/cs/BaboonMcpRuntime.cs"),
          io.septimalmind.baboon.CompilerProduct.Runtime,
        )
      F.pure(Sources((runtimeFile :: perService).toMap))
    }
  }

  private def servicesOf(domain: Domain): List[Typedef.Service] =
    domain.defs.meta.nodes.values.collect {
      case DomainMember.User(_, s: Typedef.Service, _, _) => s
    }.toList.sortBy(_.id.name.name)

  /** A service's MCP server file path, mirroring the C# `getOutputPath`
    * convention (`<basename>/<ns>.<Type>.cs`).
    */
  private def serverPath(svc: Typedef.Service, domain: Domain, evo: BaboonEvolution): String = {
    val fbase = csFileTools.basename(domain, evo)
    val fname = s"${svc.id.name.name.capitalize}McpServer.cs"
    svc.id.owner match {
      case Owner.Toplevel => s"$fbase/$fname"
      case Owner.Ns(path) => s"$fbase/${path.map(_.name.toLowerCase).mkString(".")}.$fname"
      case Owner.Adt(id)  => s"$fbase/${id.name.name.toLowerCase}.$fname"
    }
  }

  private def generateForService(svc: Typedef.Service, domain: Domain, evo: BaboonEvolution): (String, OutputFile) = {
    val path = serverPath(svc, domain, evo)

    val serviceName = svc.id.name.name
    val className    = s"${serviceName.capitalize}McpServer"
    val modelVer     = domain.version.v.toString
    val ns           = typeTranslator.toCsPkg(domain.id, domain.version, evo).parts.mkString(".")

    // Declaration-ordered tool entries (K4 §2.3): one per method. The wire
    // tool.name and the BaboonMethodId service/method strings stay verbatim
    // lowercase model names; only C# symbols are PascalCase.
    val toolEntries: List[String] = svc.methods.toList.map {
      m =>
        val toolName    = s"${serviceName}_${m.name.name}"
        val schema      = schemaEmitter.emitInputSchema(m.sig, domain)
        val descArg     = McpDocs.flatten(m.docs).map(d => s", ${csString(d)}").getOrElse("")
        s"""        new Baboon.Runtime.Shared.McpToolEntry(${csString(toolName)}, new Baboon.Runtime.Shared.BaboonMethodId(${csString(serviceName)}, ${csString(m.name.name)}), Newtonsoft.Json.Linq.JToken.Parse(${csVerbatimJson(schema)})$descArg),"""
    }

    // Async axis (`--cs-async-services=true`): the errors-mode wiring entry
    // `${serviceName}Wiring.InvokeJson` returns `Task<Either<..>>`, so the MCP
    // server must take the async `McpJsonInvokeAsync<Ctx>` delegate, extend the
    // async runtime base, and `await` the delegate in its `InvokeJson` override.
    // When OFF the sync branch is emitted verbatim, keeping the generated file
    // byte-identical to the pre-change baseline.
    val isAsync = target.language.asyncServices

    val baseClass    = if (isAsync) "AbstractBaboonMcpServerAsync" else "AbstractBaboonMcpServer"
    val delegateType = if (isAsync) "McpJsonInvokeAsync" else "McpJsonInvoke"
    val invokeRetType =
      if (isAsync)
        "async System.Threading.Tasks.Task<Baboon.Runtime.Shared.Either<Baboon.Runtime.Shared.BaboonWiringError, string>>"
      else
        "Baboon.Runtime.Shared.Either<Baboon.Runtime.Shared.BaboonWiringError, string>"
    val invokeReturn =
      if (isAsync) "return await _invokeJson(method, data, ctx, codecCtx);"
      else "return _invokeJson(method, data, ctx, codecCtx);"

    val body =
      s"""// Generated MCP server for service `$serviceName` (model `${domain.id.path.mkString(".")}` v$modelVer).
         |// Transport-abstract: `Handle` is inherited from $baseClass and
         |// performs no I/O. The `InvokeJson` delegate routes `tools/call` into the
         |// generated service dispatch; the integrator supplies it (typically the
         |// errors-mode `${serviceName}Wiring.InvokeJson` bound to this service) plus the
         |// per-request `Ctx`.
         |public sealed class $className<Ctx> : Baboon.Runtime.Shared.$baseClass<Ctx>
         |{
         |    public override Baboon.Runtime.Shared.McpServerInfo ServerInfo { get; } = new Baboon.Runtime.Shared.McpServerInfo(${csString(serviceName)}, ${csString(modelVer)});
         |
         |    public override System.Collections.Generic.IReadOnlyList<Baboon.Runtime.Shared.McpToolEntry> Tools { get; } = new System.Collections.Generic.List<Baboon.Runtime.Shared.McpToolEntry>
         |    {
         |${toolEntries.mkString("\n")}
         |    };
         |
         |    private readonly Baboon.Runtime.Shared.$delegateType<Ctx> _invokeJson;
         |
         |    public $className(Baboon.Runtime.Shared.$delegateType<Ctx> invokeJson)
         |    {
         |        _invokeJson = invokeJson;
         |    }
         |
         |    protected override $invokeRetType InvokeJson(Baboon.Runtime.Shared.BaboonMethodId method, string data, Ctx ctx, Baboon.Runtime.Shared.BaboonCodecContext codecCtx)
         |    {
         |        $invokeReturn
         |    }
         |}
         |""".stripMargin

    val wrapped =
      if (ns.isEmpty) body
      else
        s"""namespace $ns {
           |${body.split("\n", -1).map(l => if (l.isEmpty) l else "    " + l).mkString("\n")}
           |}
           |""".stripMargin

    val content =
      s"""#nullable enable
         |
         |// ReSharper disable CheckNamespace
         |// ReSharper disable InconsistentNaming
         |// ReSharper disable RedundantNameQualifier
         |
         |$wrapped""".stripMargin

    path -> OutputFile(content, io.septimalmind.baboon.CompilerProduct.Definition)
  }

  /** Embed a circe `Json` schema as a C# verbatim-string literal carrying the
    * JSON text, parsed back into a `JToken` at construction. Verbatim strings
    * only need `"` doubled; the schema text contains no other escape hazards
    * for that form.
    */
  private def csVerbatimJson(j: Json): String = {
    val text = j.noSpaces
    "@\"" + text.replace("\"", "\"\"") + "\""
  }

  private def csString(s: String): String =
    "\"" + s.replace("\\", "\\\\").replace("\"", "\\\"") + "\""
}
