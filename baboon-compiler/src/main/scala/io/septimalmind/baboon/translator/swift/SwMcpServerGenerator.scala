package io.septimalmind.baboon.translator.swift

import io.septimalmind.baboon.CompilerTarget.SwTarget
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.translator.mcp.{McpDocs, McpInputSchemaEmitter}
import io.septimalmind.baboon.translator.openapi.OasTypeTranslator
import io.septimalmind.baboon.translator.{BaboonRuntimeResources, McpServerGeneratorHook, OutputFile, Sources}
import io.septimalmind.baboon.typer.model.*
import izumi.functional.bio.{Error2, F}
import izumi.fundamentals.collections.nonempty.NEList

/** Swift MCP server generator (T17 — mirrors the T8 TypeScript / T10 C# /
  * T12 Scala / T14 Kotlin / T15 Java / T16 Dart / T18 Python references).
  *
  * Fills the T6 dispatch seam ([[McpServerGeneratorHook]]): when
  * `--swift-generate-mcp-server=true`, the Swift translator already calls
  * [[generateMcpServer]]; this supplies the generator behind that hook. It does
  * NOT re-wire dispatch.
  *
  * Per Baboon `service` (in the latest version of each lineage) it emits a
  * transport-abstract `<Service>McpServer<Ctx>` per the dispatch runtime
  * contract (`docs/research/mcp-dispatch-runtime-contract.md`):
  *
  *   - a JSON-RPC `handle(request, session, ctx, codecCtx)` state machine
  *     (initialize / tools/list / tools/call) inherited from the static
  *     `IBaboonMcpServer` protocol extension — NO baked-in I/O loop;
  *   - an immutable, declaration-ordered tool registry mapping each method to a
  *     `<serviceName>_<methodName>` tool name and its `inputSchema` (produced by
  *     the shared T5 [[McpInputSchemaEmitter]] — NOT reimplemented here);
  *   - a `tools/call` delegate that routes one tool into the generated service
  *     JSON dispatch and maps its result to MCP Channel-A / Channel-B.
  *
  * Swift is the hardest generic case: a `protocol` with an `associatedtype Ctx`
  * cannot be a bare existential, so the runtime follows the verbatim
  * abstract-context service precedent (`IBaboonJsonServiceCtx` +
  * `AnyJsonServiceCtx<Ctx, R>` type-eraser) with `IBaboonMcpServer` (`Ctx`
  * associated type) + `AnyMcpServer<Ctx>`. The generated class is generic over
  * `Ctx` and conforms to the protocol; the shared `handle` lives in the
  * protocol extension.
  *
  * JSONSerialization JSON reuse: inputSchema literals are embedded as Swift
  * string literals parsed once via `JSONSerialization.jsonObject` at the
  * `_tools` accessor and carried as `[String: Any]` constants.
  *
  * UEBA is not involved (MCP is JSON-only). The additive runtime types live in
  * the static resource `baboon-runtime/swift/baboon_mcp_runtime.swift`, emitted
  * alongside the per-service file. With the flag false the hook is never called,
  * so Swift output is byte-identical to baseline.
  */
class SwMcpServerGenerator[F[+_, +_]: Error2](
  target: SwTarget,
  trans: SwTypeTranslator,
  swFiles: SwFileTools,
  oasTypeTranslator: OasTypeTranslator,
) extends McpServerGeneratorHook[F] {

  private val schemaEmitter = new McpInputSchemaEmitter(oasTypeTranslator)

  // Async axis (D24/T67). When enabled the generated no-errors service-wiring
  // dispatcher `<Svc>Wiring.invokeJson` is `async throws -> String`
  // (SwServiceWiringTranslator), so the MCP server must hold an `async throws`
  // delegate and conform to the async dispatch surface `IBaboonAsyncMcpServer`.
  // When disabled the generated output is byte-identical to the sync baseline.
  private val isAsync: Boolean = target.language.asyncServices

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
        "BaboonRuntime/baboon_mcp_runtime.swift" -> OutputFile(
          BaboonRuntimeResources.read("baboon-runtime/swift/baboon_mcp_runtime.swift"),
          io.septimalmind.baboon.CompilerProduct.Runtime,
        )
      F.pure(Sources((runtimeFile :: perService).toMap))
    }
  }

  private def servicesOf(domain: Domain): List[Typedef.Service] =
    domain.defs.meta.nodes.values.collect {
      case DomainMember.User(_, s: Typedef.Service, _, _) => s
    }.toList.sortBy(_.id.name.name)

  /** A service's MCP server file path, mirroring the per-service file the
    * Swift codegen uses (`<basename>/<service_name>_mcp_server.swift`).
    */
  private def serverPath(svc: Typedef.Service, domain: Domain, evo: BaboonEvolution): String = {
    val basename = swFiles.basename(domain, evo)
    val svcName  = svc.id.name.name
    val fname    = s"${trans.toSnakeCase(svcName)}_mcp_server.swift"
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
    //
    // inputSchema literals are embedded as Swift string literals parsed once
    // via `JSONSerialization.jsonObject` into `[String: Any]` — reusing the
    // existing Swift JSON path rather than introducing a new schema model.
    val toolEntries: List[String] = svc.methods.toList.map {
      m =>
        val toolName      = s"${serviceName}_${m.name.name}"
        val schema        = schemaEmitter.emitInputSchema(m.sig, domain)
        val schemaLiteral = swiftStringLiteral(schema.noSpaces)
        val descArg       = McpDocs.flatten(m.docs).map(d => s", ${swiftString(d)}").getOrElse("")
        s"""        McpToolEntry(${swiftString(toolName)}, BaboonMethodId(serviceId: ${swiftString(serviceName)}, methodName: ${swiftString(m.name.name)}), $className._parseSchema($schemaLiteral)$descArg),"""
    }

    // Async axis (D24/T67): the delegate type, the conformed protocol, and the
    // `invokeJson` declaration flip to `async throws` / `IBaboonAsyncMcpServer`
    // when the Swift target's `asyncServices` is on. The inherited `handle` state
    // machine is then GENUINELY `async` (it `await`s the delegate directly in the
    // caller's task — no synchronous semaphore bridge, deadlock-free from an
    // actor-isolated caller); the integrator calls `await server.handle(...)`.
    // With async off, every interpolated fragment below is empty, so the rendered
    // file is byte-identical to the sync baseline.
    val mcpProtocol     = if (isAsync) "IBaboonAsyncMcpServer" else "IBaboonMcpServer"
    val delegateEffects = if (isAsync) "async throws" else "throws"
    val methodEffects   = if (isAsync) "async throws" else "throws"
    val invokeCallPfx   = if (isAsync) "try await" else "try"

    val content =
      s"""import Foundation
         |import BaboonRuntime
         |
         |// Generated MCP server for service `$serviceName` (model `${domain.id.path.mkString(".")}` v$modelVer).
         |// Transport-abstract: `handle` is inherited from the $mcpProtocol
         |// protocol extension and performs no I/O. The `invokeJson` delegate routes
         |// `tools/call` into the generated service dispatch; the integrator supplies
         |// it (typically the errors-mode `${serviceName}Wiring.invokeJson` bound to this
         |// service) plus the per-request `Ctx`.
         |public final class $className<Ctx>: $mcpProtocol {
         |    private let _invokeJson: (BaboonMethodId, String, Ctx, BaboonCodecContext) $delegateEffects -> String
         |
         |    public init(_ invokeJson: @escaping (BaboonMethodId, String, Ctx, BaboonCodecContext) $delegateEffects -> String) {
         |        self._invokeJson = invokeJson
         |    }
         |
         |    public var serverInfo: McpServerInfo {
         |        return McpServerInfo(${swiftString(serviceName)}, ${swiftString(modelVer)})
         |    }
         |
         |    // inputSchema values are parsed once via JSONSerialization. `jsonObject`
         |    // is not a constant expression so the registry is a lazily-computed
         |    // stored property carried as a constant for the server's lifetime.
         |    private lazy var _tools: [McpToolEntry] = [
         |${toolEntries.mkString("\n")}
         |    ]
         |
         |    public var tools: [McpToolEntry] {
         |        return _tools
         |    }
         |
         |    public func invokeJson(_ method: BaboonMethodId, _ data: String, _ ctx: Ctx, _ codecCtx: BaboonCodecContext) $methodEffects -> String {
         |        return $invokeCallPfx _invokeJson(method, data, ctx, codecCtx)
         |    }
         |
         |    private static func _parseSchema(_ json: String) -> [String: Any] {
         |        let data = json.data(using: .utf8)!
         |        return (try! JSONSerialization.jsonObject(with: data, options: [])) as! [String: Any]
         |    }
         |}
         |""".stripMargin

    path -> OutputFile(content, io.septimalmind.baboon.CompilerProduct.Definition)
  }

  /** A Swift double-quoted string literal for a wire name (no embedded JSON). */
  private def swiftString(s: String): String =
    "\"" + s.replace("\\", "\\\\").replace("\"", "\\\"") + "\""

  /** A Swift double-quoted string literal carrying the schema JSON text.
    * Escapes backslash, double-quote, and control characters; `noSpaces` JSON
    * contains no raw newlines, so only `\\`, `\"` need handling beyond the
    * defensive control-char pass.
    */
  private def swiftStringLiteral(s: String): String = {
    val sb = new StringBuilder("\"")
    s.foreach {
      case '\\' => sb.append("\\\\")
      case '"'  => sb.append("\\\"")
      case '\n' => sb.append("\\n")
      case '\r' => sb.append("\\r")
      case '\t' => sb.append("\\t")
      case c    => sb.append(c)
    }
    sb.append("\"")
    sb.toString()
  }
}
