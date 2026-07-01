package io.septimalmind.baboon.translator.scl

import io.circe.Json
import io.septimalmind.baboon.CompilerTarget.ScTarget
import io.septimalmind.baboon.parser.model.issues.{BaboonIssue, TranslationIssue}
import io.septimalmind.baboon.translator.mcp.{McpDocs, McpInputSchemaEmitter}
import io.septimalmind.baboon.translator.openapi.OasTypeTranslator
import io.septimalmind.baboon.translator.{BaboonRuntimeResources, McpServerGeneratorHook, OutputFile, ServiceResultResolver, Sources}
import io.septimalmind.baboon.typer.model.*
import izumi.functional.bio.{Error2, F}
import izumi.fundamentals.collections.nonempty.NEList

/** Scala MCP server generator (T12 — mirrors the T8/T10 reference shape).
  *
  * Fills the T6 dispatch seam ([[McpServerGeneratorHook]]): when
  * `--scala-generate-mcp-server=true`, the Scala translator already calls
  * [[generateMcpServer]]; this supplies the generator behind that hook. It does
  * NOT re-wire dispatch.
  *
  * Per Baboon `service` (in the latest version of each lineage) it emits a
  * transport-abstract `<Service>McpServer[Ctx]` per the dispatch runtime contract
  * (`docs/research/mcp-dispatch-runtime-contract.md`):
  *
  *   - a JSON-RPC `handle(request, session, ctx, codecCtx)` state machine
  *     (initialize / tools/list / tools/call) inherited from the static
  *     `AbstractBaboonMcpServer` runtime base — NO baked-in I/O loop;
  *   - an immutable, declaration-ordered tool registry mapping each method to a
  *     `<serviceName>_<methodName>` tool name and its `inputSchema` (produced by
  *     the shared T5 [[McpInputSchemaEmitter]] — NOT reimplemented here);
  *   - a `tools/call` delegate that routes one tool into the generated service
  *     JSON dispatch and maps its `Either[BaboonWiringError, String]` result to
  *     MCP Channel-A / Channel-B.
  *
  * UEBA is not involved (MCP is JSON-only). The additive runtime types live in
  * the static resource `baboon-runtime/scala/BaboonMcpRuntime.scala`, emitted
  * alongside the per-service file. With the flag false the hook is never called,
  * so Scala output is byte-identical to baseline.
  *
  * '''serviceResult requirement (Either-only).''' The MCP delegate type and the
  * runtime base `AbstractBaboonMcpServer.handle` are synchronous and
  * Either-shaped — `handle` pattern-matches `Right`/`Left` on the wiring's
  * `invokeJson` result to drive MCP Channel-A / Channel-B. This is the SAME
  * synchronous axis as the broader async-service work (D24); it is NOT generic
  * over the configurable serviceResult container that
  * [[ScServiceWiringTranslator]] honors. Consequently MCP generation is supported
  * ONLY in Either errors-mode serviceResult. Combining a non-Either serviceResult
  * (an HKT `F[_, _]`, a custom result type, or `no-errors` mode) with
  * `--scala-generate-mcp-server=true` is rejected up front with
  * [[io.septimalmind.baboon.parser.model.issues.TranslationIssue.ScalaMcpRequiresEither]]
  * rather than emitting code whose delegate type mismatches the wiring container.
  */
class ScMcpServerGenerator[F[+_, +_]: Error2](
  target: ScTarget,
  trans: ScTypeTranslator,
  oasTypeTranslator: OasTypeTranslator,
  scFiles: ScFileTools,
) extends McpServerGeneratorHook[F] {

  private val schemaEmitter = new McpInputSchemaEmitter(oasTypeTranslator)

  override def generateMcpServer(family: BaboonFamily): F[NEList[BaboonIssue], Sources] = {
    // The Scala MCP dispatch runtime (AbstractBaboonMcpServer.handle) is synchronous
    // and Either-shaped: it matches Right/Left on the wiring's `invokeJson` result to
    // map MCP Channel-A / Channel-B. The hardcoded `Either[BaboonWiringError, String]`
    // delegate type below mirrors that. A non-Either (HKT / custom) serviceResult
    // honored by ScServiceWiringTranslator would make the wiring's invokeJson return a
    // different container, producing a latent type mismatch. Guard against that
    // unsupported combination with a clear, actionable compiler error.
    val mcpIncompatibleResult: Option[String] = family.domains.toMap.values.toList.flatMap {
      lineage =>
        val latestDomain = lineage.versions(lineage.evolution.latest)
        if (servicesOf(latestDomain).isEmpty) None
        else {
          val resolved = ServiceResultResolver.resolve(latestDomain, "scala", target.language.serviceResult, target.language.pragmas)
          if (isEitherErrorsMode(resolved)) None
          else Some(describeResult(resolved))
        }
    }.headOption

    mcpIncompatibleResult match {
      case Some(rt) => F.fail(NEList(TranslationIssue.ScalaMcpRequiresEither(rt): BaboonIssue))
      case None     => emitSources(family)
    }
  }

  private def isEitherErrorsMode(resolved: io.septimalmind.baboon.translator.ResolvedServiceResult): Boolean =
    !resolved.noErrors &&
      resolved.hkt.isEmpty &&
      resolved.resultType.exists(t => t == "Either" || t == "scala.util.Either")

  private def describeResult(resolved: io.septimalmind.baboon.translator.ResolvedServiceResult): String = {
    if (resolved.noErrors) "no-errors mode (no error channel)"
    else
      resolved.hkt match {
        case Some(h) => s"HKT ${h.name}${h.signature}${resolved.resultType.fold("")(t => s" ($t)")}"
        case None    => resolved.resultType.getOrElse("<unspecified>")
      }
  }

  private def emitSources(family: BaboonFamily): F[NEList[BaboonIssue], Sources] = {
    val perService: List[(String, OutputFile)] = family.domains.toMap.values.toList.flatMap {
      lineage =>
        val evo          = lineage.evolution
        val latestDomain = lineage.versions(evo.latest)
        servicesOf(latestDomain).map(svc => generateForService(svc, latestDomain, evo))
    }

    val runtimeFile =
      "BaboonMcpRuntime.scala" -> OutputFile(
        BaboonRuntimeResources.read("baboon-runtime/scala/BaboonMcpRuntime.scala"),
        io.septimalmind.baboon.CompilerProduct.Runtime,
      )
    F.pure(Sources((runtimeFile :: perService).toMap))
  }

  private def servicesOf(domain: Domain): List[Typedef.Service] =
    domain.defs.meta.nodes.values.collect {
      case DomainMember.User(_, s: Typedef.Service, _, _) => s
    }.toList.sortBy(_.id.name.name)

  private def serverPath(svc: Typedef.Service, domain: Domain, evo: BaboonEvolution): String = {
    val basename    = scFiles.basename(domain, evo)
    val serviceName = svc.id.name.name.toLowerCase
    svc.id.owner match {
      case Owner.Toplevel => s"$basename/$serviceName/${svc.id.name.name}McpServer.scala"
      case Owner.Ns(path) => s"$basename/${path.map(_.name.toLowerCase).mkString("/")}/$serviceName/${svc.id.name.name}McpServer.scala"
      case Owner.Adt(id)  => s"$basename/${id.name.name.toLowerCase}/$serviceName/${svc.id.name.name}McpServer.scala"
    }
  }

  private def generateForService(svc: Typedef.Service, domain: Domain, evo: BaboonEvolution): (String, OutputFile) = {
    val path = serverPath(svc, domain, evo)

    val serviceName = svc.id.name.name
    val className   = s"${serviceName}McpServer"
    val modelVer    = domain.version.v.toString
    val pkg         = trans.toScPkg(domain.id, domain.version, evo)
    val pkgStr      = pkg.parts.mkString(".")

    // Declaration-ordered tool entries (K4 §2.3): one per method.
    val toolEntries: List[String] = svc.methods.toList.map {
      m =>
        val toolName = s"${serviceName}_${m.name.name}"
        val schema   = schemaEmitter.emitInputSchema(m.sig, domain)
        // The self-contained JSON Schema is carried as a constant literal value.
        // io.circe.Json literals are produced by io.circe.parser.parse.
        val descArg = McpDocs.flatten(m.docs)
          .map(d => s"\n      description = Some(${scalaString(d)}),")
          .getOrElse("")
        s"""    _root_.baboon.runtime.shared.McpToolEntry(
           |      name = ${scalaString(toolName)},
           |      method = _root_.baboon.runtime.shared.BaboonMethodId(${scalaString(serviceName)}, ${scalaString(m.name.name)}),
           |      inputSchema = io.circe.parser.parse(${scalaMultilineString(schema.noSpaces)}).fold(throw _, identity),$descArg
           |    ),""".stripMargin
    }

    val invokeJsonType = s"(_root_.baboon.runtime.shared.BaboonMethodId, String, Ctx, _root_.baboon.runtime.shared.BaboonCodecContext) => Either[_root_.baboon.runtime.shared.BaboonWiringError, String]"

    val body =
      s"""// Generated MCP server for service `$serviceName` (model `${domain.id.path.mkString(".")}` v$modelVer).
         |// Transport-abstract: `handle` is inherited from AbstractBaboonMcpServer and
         |// performs no I/O. The `invokeJson` delegate routes `tools/call` into the
         |// generated service dispatch; the integrator supplies it (typically the
         |// errors-mode `${serviceName}Wiring.invokeJson` bound to this service) plus the
         |// per-request `Ctx`.
         |class $className[Ctx](
         |  private val _invokeJson: $invokeJsonType,
         |) extends _root_.baboon.runtime.shared.AbstractBaboonMcpServer[Ctx] {
         |
         |  override val serverInfo: _root_.baboon.runtime.shared.McpServerInfo =
         |    _root_.baboon.runtime.shared.McpServerInfo(name = ${scalaString(serviceName)}, version = ${scalaString(modelVer)})
         |
         |  override val tools: Seq[_root_.baboon.runtime.shared.McpToolEntry] = Seq(
         |${toolEntries.mkString("\n")}
         |  )
         |
         |  protected override def invokeJson(method: _root_.baboon.runtime.shared.BaboonMethodId, data: String, ctx: Ctx, codecCtx: _root_.baboon.runtime.shared.BaboonCodecContext): Either[_root_.baboon.runtime.shared.BaboonWiringError, String] =
         |    _invokeJson(method, data, ctx, codecCtx)
         |}
         |""".stripMargin

    val content =
      if (pkgStr.isEmpty) body
      else s"package $pkgStr\n\n$body"

    path -> OutputFile(content, io.septimalmind.baboon.CompilerProduct.Definition)
  }

  private def scalaString(s: String): String =
    Json.fromString(s).noSpaces

  /** Embed a JSON string as a Scala string literal using triple-quote form to
    * avoid escaping issues with the schema text.
    */
  private def scalaMultilineString(s: String): String = {
    // Triple-quoted strings in Scala cannot contain """  — use string concatenation.
    if (s.contains("\"\"\"")) {
      // Fall back to escaped string.
      "\"" + s.replace("\\", "\\\\").replace("\"", "\\\"") + "\""
    } else {
      "\"\"\"" + s + "\"\"\""
    }
  }
}
