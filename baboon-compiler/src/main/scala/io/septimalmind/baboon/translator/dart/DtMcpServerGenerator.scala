package io.septimalmind.baboon.translator.dart

import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.translator.mcp.{McpDocs, McpInputSchemaEmitter}
import io.septimalmind.baboon.translator.openapi.OasTypeTranslator
import io.septimalmind.baboon.translator.{BaboonRuntimeResources, McpServerGeneratorHook, OutputFile, Sources}
import io.septimalmind.baboon.typer.model.*
import izumi.functional.bio.{Error2, F}
import izumi.fundamentals.collections.nonempty.NEList

/** Dart MCP server generator (T16 — mirrors the T8 TypeScript / T10 C# / T14 Kotlin references).
  *
  * Fills the T6 dispatch seam ([[McpServerGeneratorHook]]): when
  * `--dart-generate-mcp-server=true`, the Dart translator already calls
  * [[generateMcpServer]]; this supplies the generator behind that hook. It does
  * NOT re-wire dispatch.
  *
  * Per Baboon `service` (in the latest version of each lineage) it emits a
  * transport-abstract `<Service>McpServer<Ctx>` per the dispatch runtime contract
  * (`docs/research/mcp-dispatch-runtime-contract.md`):
  *
  *   - a JSON-RPC `handle(request, session, ctx, codecCtx)` state machine
  *     (initialize / tools/list / tools/call) inherited from the static
  *     `AbstractBaboonMcpServer<Ctx>` runtime base — NO baked-in I/O loop;
  *   - an immutable, declaration-ordered tool registry mapping each method to a
  *     `<serviceName>_<methodName>` tool name and its `inputSchema` (produced by
  *     the shared T5 [[McpInputSchemaEmitter]] — NOT reimplemented here);
  *   - a `tools/call` delegate that routes one tool into the generated service
  *     JSON dispatch and maps its result to MCP Channel-A / Channel-B.
  *
  * dart:convert JSON reuse: inputSchema literals are embedded as inline JSON
  * strings (constant string literals in Dart) parsed via `jsonDecode` once at
  * construction time and carried as constants.
  *
  * UEBA is not involved (MCP is JSON-only). The additive runtime types live in
  * the static resource `baboon-runtime/dart/baboon_mcp_runtime.dart`, emitted
  * alongside the per-service file. With the flag false the hook is never called,
  * so Dart output is byte-identical to baseline.
  */
class DtMcpServerGenerator[F[+_, +_]: Error2](
  dtFileTools: DtFileTools,
  oasTypeTranslator: OasTypeTranslator,
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
        "baboon_mcp_runtime.dart" -> OutputFile(
          BaboonRuntimeResources.read("baboon-runtime/dart/baboon_mcp_runtime.dart"),
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
    * service-wiring generator uses (`<basename>/<ServiceName>_mcp_server.dart`).
    */
  private def serverPath(svc: Typedef.Service, domain: Domain, evo: BaboonEvolution): String = {
    val basename = dtFileTools.basename(domain, evo)
    val svcName  = svc.id.name.name
    val fname    = s"${toSnakeCase(svcName)}_mcp_server.dart"
    s"$basename/$fname"
  }

  private def generateForService(svc: Typedef.Service, domain: Domain, evo: BaboonEvolution): (String, OutputFile) = {
    val path = serverPath(svc, domain, evo)

    val serviceName = svc.id.name.name
    val className   = s"${serviceName}McpServer"
    val modelVer    = domain.version.v.toString

    // Declaration-ordered tool entries (K4 §2.3): one per method. The wire
    // tool.name and the BaboonMethodId service/method strings stay verbatim
    // lowercase model names.
    // inputSchema literals are embedded as dart:convert `jsonDecode` calls on
    // constant strings so they are parsed once at construction and carried as constants.
    val toolEntries: List[String] = svc.methods.toList.map {
      m =>
        val toolName = s"${serviceName}_${m.name.name}"
        val schema   = schemaEmitter.emitInputSchema(m.sig, domain)
        // Embed schema as a Dart single-quoted string literal. Escape rules:
        // `\` → `\\`, `'` → `\'`, `$` → `\$` (Dart interpolation guard).
        val schemaLiteral = schema.noSpaces
          .replace("\\", "\\\\")
          .replace("'", "\\'")
          .replace("$", "\\$")
        val descArg   = McpDocs.flatten(m.docs).map(d => s", ${dartDescString(d)}").getOrElse("")
        s"    McpToolEntry(${dartString(toolName)}, const BaboonMethodId(${dartString(serviceName)}, ${dartString(m.name.name)}), jsonDecode('$schemaLiteral') as Map<String, dynamic>$descArg),"
    }

    val content =
      s"""import 'dart:convert';
         |import 'package:baboon_runtime/baboon_runtime.dart';
         |import 'package:baboon_runtime/baboon_mcp_runtime.dart';
         |
         |// Generated MCP server for service `$serviceName` (model `${domain.id.path.mkString(".")}` v$modelVer).
         |// Transport-abstract: `handle` is inherited from AbstractBaboonMcpServer and
         |// performs no I/O. The `invokeJson` delegate routes `tools/call` into the
         |// generated service dispatch; the integrator supplies it (typically the
         |// errors-mode `${serviceName}Wiring.invokeJson` bound to this service) plus the
         |// per-request `Ctx`.
         |class $className<Ctx> extends AbstractBaboonMcpServer<Ctx> {
         |  final String Function(BaboonMethodId, String, Ctx, BaboonCodecContext) _invokeJson;
         |
         |  $className(this._invokeJson);
         |
         |  @override
         |  McpServerInfo get serverInfo => const McpServerInfo(${dartString(serviceName)}, ${dartString(modelVer)});
         |
         |  // inputSchema values are parsed once via jsonDecode (dart:convert).
         |  // jsonDecode is not a const expression so we use a late final field.
         |  late final List<McpToolEntry> _tools = [
         |${toolEntries.mkString("\n")}
         |  ];
         |
         |  @override
         |  List<McpToolEntry> get tools => _tools;
         |
         |  @override
         |  String Function(BaboonMethodId, String, Ctx, BaboonCodecContext) get invokeJsonFn => _invokeJson;
         |}
         |""".stripMargin

    path -> OutputFile(content, io.septimalmind.baboon.CompilerProduct.Definition)
  }

  private def toSnakeCase(s: String): String = {
    val sb = new StringBuilder
    s.foreach {
      c =>
        if (c.isUpper && sb.nonEmpty) { sb.append('_'); sb.append(c.toLower) }
        else sb.append(c.toLower)
    }
    sb.toString()
  }

  /** Embed a description string as a Dart single-quoted literal.
    * Escapes all characters that are hazardous in a Dart non-raw single-quoted string:
    *   `\`  → `\\`  (must be first to avoid double-escaping)
    *   `'`  → `\'`  (string terminator)
    *   `$`  → `\$`  (Dart string interpolation start)
    *   LF   → `\n`  (raw newline is a syntax error in a single-quoted literal)
    *   CR   → `\r`
    *   TAB  → `\t`
    */
  private def dartDescString(s: String): String =
    "'" + s
      .replace("\\", "\\\\")
      .replace("'", "\\'")
      .replace("$", "\\$")
      .replace("\n", "\\n")
      .replace("\r", "\\r")
      .replace("\t", "\\t")
      + "'"

  private def dartString(s: String): String =
    "'" + s.replace("\\", "\\\\").replace("'", "\\'") + "'"
}
