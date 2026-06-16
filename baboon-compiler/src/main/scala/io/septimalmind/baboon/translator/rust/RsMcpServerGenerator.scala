package io.septimalmind.baboon.translator.rust

import io.circe.Json
import io.septimalmind.baboon.CompilerTarget.RsTarget
import io.septimalmind.baboon.parser.model.issues.BaboonIssue
import io.septimalmind.baboon.translator.mcp.McpInputSchemaEmitter
import io.septimalmind.baboon.translator.openapi.OasTypeTranslator
import io.septimalmind.baboon.translator.rust.RsDefnTranslator.{toSnakeCaseFileName, toSnakeCaseRaw}
import io.septimalmind.baboon.translator.{BaboonRuntimeResources, McpServerGeneratorHook, OutputFile, Sources}
import io.septimalmind.baboon.typer.model.*
import izumi.functional.bio.{Error2, F}
import izumi.fundamentals.collections.nonempty.NEList

/** Rust MCP server generator (T13 — mirrors the T8 TypeScript and T10 C# references).
  *
  * Fills the T6 dispatch seam ([[McpServerGeneratorHook]]): when
  * `--rust-generate-mcp-server=true`, the Rust translator already calls
  * [[generateMcpServer]]; this supplies the generator behind that hook. It does
  * NOT re-wire dispatch.
  *
  * Per Baboon `service` (in the latest version of each lineage) it emits a
  * transport-abstract `<ServiceName>McpServer<Ctx>` per the dispatch runtime contract
  * (`docs/research/mcp-dispatch-runtime-contract.md`):
  *
  *   - a JSON-RPC `handle(request, session, ctx, codec_ctx)` state machine
  *     (initialize / tools/list / tools/call) delegated to the static
  *     `BaboonMcpServerBase` runtime helper — NO baked-in I/O loop;
  *   - an immutable, declaration-ordered tool registry mapping each method to a
  *     `<serviceName>_<methodName>` tool name and its `input_schema` (produced by
  *     the shared T5 [[McpInputSchemaEmitter]] — NOT reimplemented here);
  *   - a `tools/call` delegate that routes one tool into the generated service
  *     JSON dispatch and maps its `Result<String, BaboonWiringError>`
  *     result to MCP Channel-A / Channel-B.
  *
  * UEBA is not involved (MCP is JSON-only). The additive runtime types live in
  * the static resource `baboon-runtime/rust/baboon_mcp_server.rs`, emitted
  * alongside the per-service file. With the flag false the hook is never called,
  * so Rust output is byte-identical to baseline.
  */
class RsMcpServerGenerator[F[+_, +_]: Error2](
  target: RsTarget,
  rsFiles: RsFileTools,
  oasTypeTranslator: OasTypeTranslator,
) extends McpServerGeneratorHook[F] {

  private val schemaEmitter = new McpInputSchemaEmitter(oasTypeTranslator)

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
        "baboon_mcp_server.rs" -> OutputFile(
          mcpServerRuntime,
          io.septimalmind.baboon.CompilerProduct.Runtime,
        )
      F.pure(Sources((runtimeFile :: perService).toMap))
    }
  }

  /** The static MCP server runtime resource. In SYNC mode it is emitted
    * byte-for-byte. In ASYNC mode (`--rs-async-services=true`) the single
    * `McpJsonInvoke<Ctx>` type alias is swapped for a future-returning form so
    * the generated MCP server can bind the async `invoke_json_<svc>` (which is
    * an `async fn` whose call evaluates to a future, not a `Result`). The
    * future is intentionally NOT `+ Send`: the generated service trait methods
    * are bare `async fn`s (AFIT, `?Send`) and `BaboonWiringError` is non-`Send`,
    * matching T97's wiring future shape. No other byte of the resource changes,
    * so the `BaboonMcpServerBase` state machine is reused as-is — the generated
    * async server adapts the future to that sync helper by blocking on it.
    */
  private def mcpServerRuntime: String = {
    val resource = BaboonRuntimeResources.read("baboon-runtime/rust/baboon_mcp_server.rs")
    if (!isAsync) resource
    else {
      val swapped = resource.replace(syncInvokeAlias, asyncInvokeAlias)
      if (swapped == resource)
        throw new IllegalStateException(
          "RsMcpServerGenerator: failed to locate the sync McpJsonInvoke alias in " +
            "baboon-runtime/rust/baboon_mcp_server.rs for the async swap. The resource " +
            "alias text drifted from RsMcpServerGenerator.syncInvokeAlias.",
        )
      swapped
    }
  }

  /** Verbatim sync alias block as it appears in the resource (used as the swap
    * anchor). Kept literally in sync with `baboon_mcp_server.rs`.
    */
  private val syncInvokeAlias: String =
    """pub type McpJsonInvoke<Ctx> =
      |    Box<dyn Fn(&BaboonMethodId, &str, Ctx, &BaboonCodecContext) -> Result<String, BaboonWiringError>>;""".stripMargin

  /** Future-returning replacement emitted only under `--rs-async-services=true`. */
  private val asyncInvokeAlias: String =
    """pub type McpJsonInvoke<Ctx> =
      |    Box<dyn Fn(&BaboonMethodId, &str, Ctx, &BaboonCodecContext) -> std::pin::Pin<Box<dyn std::future::Future<Output = Result<String, BaboonWiringError>>>>>;""".stripMargin

  private def servicesOf(domain: Domain): List[Typedef.Service] =
    domain.defs.meta.nodes.values.collect {
      case DomainMember.User(_, s: Typedef.Service, _, _) => s
    }.toList.sortBy(_.id.name.name)

  /** A service's MCP server file path. Mirrors the `_wiring.rs` path convention:
    * `<basename>/<path_prefix><snakeName>_mcp_server.rs`.
    */
  private def serverPath(svc: Typedef.Service, domain: Domain, evo: BaboonEvolution): String = {
    val fbase    = rsFiles.basename(domain, evo)
    val baseName = escapeRustModuleName(toSnakeCaseFileName(svc.id.name.name))
    val fname    = s"${baseName}_mcp_server.rs"
    svc.id.owner match {
      case Owner.Toplevel => s"$fbase/$fname"
      case Owner.Ns(path) => s"$fbase/${path.map(n => escapeRustModuleName(n.name.toLowerCase)).mkString("/")}/$fname"
      case Owner.Adt(id)  => s"$fbase/${escapeRustModuleName(toSnakeCaseFileName(id.name.name))}/$fname"
    }
  }

  private def generateForService(svc: Typedef.Service, domain: Domain, evo: BaboonEvolution): (String, OutputFile) = {
    val path = serverPath(svc, domain, evo)

    val serviceName   = svc.id.name.name
    val snakeName     = toSnakeCaseRaw(serviceName)
    val structName    = s"${serviceName}McpServer"
    val modelVer      = domain.version.v.toString
    val modelId       = domain.id.path.mkString(".")

    // Declaration-ordered tool entries (K4 §2.3): one per method.
    // The schema is carried as a parsed serde_json::Value constant.
    val toolEntries: List[String] = svc.methods.toList.map {
      m =>
        val toolName = s"${serviceName}_${m.name.name}"
        val schema   = schemaEmitter.emitInputSchema(m.sig, domain)
        val schemaLiteral = rustJsonLiteral(schema)
        s"""        crate::baboon_mcp_server::McpToolEntry {
           |            name: ${rustStr(toolName)},
           |            method: crate::baboon_service_wiring::BaboonMethodId {
           |                service_name: ${rustStr(serviceName)}.to_string(),
           |                method_name: ${rustStr(m.name.name)}.to_string(),
           |            },
           |            description: None,
           |            input_schema: serde_json::json!($schemaLiteral),
           |        },""".stripMargin
    }

    // In ASYNC mode `McpJsonInvoke<Ctx>` is future-returning (see
    // `asyncInvokeAlias`): `self.invoke_json(...)` evaluates to a
    // `Pin<Box<dyn Future<Output = Result<String, BaboonWiringError>>>>`, not a
    // `Result`. `BaboonMcpServerBase::handle_request` is transport-abstract and
    // synchronous (it returns a `JsonRpcResponse` value, performing no I/O); to
    // reuse that byte-identical state machine the generated server adapts the
    // async invoke into the sync `Fn(..) -> Result<..>` it expects by driving
    // the future to completion with a self-contained, dependency-free executor
    // (`block_on`). SYNC mode passes `&*self.invoke_json` directly, byte-identical.
    val handleBody =
      if (isAsync)
        s"""        let invoke = &*self.invoke_json;
           |        let sync_invoke = move |method: &crate::baboon_service_wiring::BaboonMethodId,
           |                                data: &str,
           |                                ctx: Ctx,
           |                                codec_ctx: &crate::baboon_runtime::BaboonCodecContext|
           |              -> Result<String, crate::baboon_service_wiring::BaboonWiringError> {
           |            block_on(invoke(method, data, ctx, codec_ctx))
           |        };
           |        self.base.handle_request(request, session, ctx, codec_ctx, &sync_invoke)""".stripMargin
      else
        "        self.base.handle_request(request, session, ctx, codec_ctx, &*self.invoke_json)"

    // Self-contained, dependency-free `block_on` for the async server (no tokio
    // / futures crate is on the generated crate's dependency list). The generated
    // service futures are not `Send`, so a single-threaded park-loop executor is
    // appropriate. Emitted only under `--rs-async-services=true`.
    val blockOnHelper =
      if (isAsync)
        s"""
           |/// Minimal single-threaded executor: drives a future to completion on the
           |/// current thread. The generated async MCP dispatch is transport-abstract and
           |/// synchronous at the `handle` boundary (it returns a `JsonRpcResponse` value),
           |/// so the future-returning `invoke_json` is blocked on here. The future is not
           |/// required to be `Send` (matching the `?Send` service-wiring futures).
           |///
           |/// **Precondition:** The future MUST NOT park on an external waker. If it does,
           |/// `Poll::Pending` will trigger `std::thread::yield_now()`, which busy-spins the
           |/// CPU core instead of blocking. The generated service futures are not `Send` and
           |/// do not use external wakers, so this precondition is satisfied by construction.
           |fn block_on<F: std::future::Future>(future: F) -> F::Output {
           |    use std::task::{Context, Poll, RawWaker, RawWakerVTable, Waker};
           |
           |    fn raw_waker() -> RawWaker {
           |        fn no_op(_: *const ()) {}
           |        fn clone(_: *const ()) -> RawWaker { raw_waker() }
           |        static VTABLE: RawWakerVTable = RawWakerVTable::new(clone, no_op, no_op, no_op);
           |        RawWaker::new(std::ptr::null(), &VTABLE)
           |    }
           |
           |    let waker = unsafe { Waker::from_raw(raw_waker()) };
           |    let mut cx = Context::from_waker(&waker);
           |    let mut future = Box::pin(future);
           |    loop {
           |        match future.as_mut().poll(&mut cx) {
           |            Poll::Ready(value) => return value,
           |            Poll::Pending => std::thread::yield_now(),
           |        }
           |    }
           |}
           |""".stripMargin
      else ""

    val content =
      s"""// Generated MCP server for service `$serviceName` (model `$modelId` v$modelVer).
         |// Transport-abstract: `handle` delegates to BaboonMcpServerBase and
         |// performs no I/O. The `invoke_json` closure routes `tools/call` into the
         |// generated service dispatch; the integrator supplies it (typically the
         |// errors-mode `invoke_json_${snakeName}` bound to this service) plus the
         |// per-request `Ctx`.
         |$blockOnHelper
         |pub struct $structName<Ctx: Clone> {
         |    base: crate::baboon_mcp_server::BaboonMcpServerBase,
         |    invoke_json: crate::baboon_mcp_server::McpJsonInvoke<Ctx>,
         |}
         |
         |impl<Ctx: Clone> $structName<Ctx> {
         |    pub fn new(
         |        invoke_json: crate::baboon_mcp_server::McpJsonInvoke<Ctx>,
         |    ) -> Self {
         |        let base = crate::baboon_mcp_server::BaboonMcpServerBase {
         |            server_info: crate::baboon_mcp_server::McpServerInfo {
         |                name: ${rustStr(serviceName)},
         |                version: ${rustStr(modelVer)},
         |            },
         |            tools: vec![
         |${toolEntries.mkString("\n")}
         |            ],
         |        };
         |        $structName { base, invoke_json }
         |    }
         |}
         |
         |impl<Ctx: Clone> crate::baboon_mcp_server::IBaboonMcpServer<Ctx> for $structName<Ctx> {
         |    fn handle(
         |        &self,
         |        request: &crate::baboon_mcp_server::JsonRpcRequest,
         |        session: &mut crate::baboon_mcp_server::McpSession,
         |        ctx: Ctx,
         |        codec_ctx: &crate::baboon_runtime::BaboonCodecContext,
         |    ) -> Option<crate::baboon_mcp_server::JsonRpcResponse> {
         |$handleBody
         |    }
         |}
         |""".stripMargin

    path -> OutputFile(content, io.septimalmind.baboon.CompilerProduct.Definition)
  }

  /** Render a circe `Json` as a Rust `serde_json::json!()` macro argument.
    * `Json.noSpaces` produces valid JSON; the `serde_json::json!` macro accepts
    * JSON literals verbatim, so we can embed the schema text directly.
    */
  private def rustJsonLiteral(j: Json): String = j.noSpaces

  private def rustStr(s: String): String =
    "\"" + s.replace("\\", "\\\\").replace("\"", "\\\"") + "\""

  private def escapeRustModuleName(name: String): String =
    RsDefnTranslator.escapeRustModuleName(name)
}
