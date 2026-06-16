package baboon.runtime.shared {

  // Additive MCP server runtime (decisions ledger M1; contract:
  // docs/research/mcp-dispatch-runtime-contract.md). Emitted only when
  // `--scala-generate-mcp-server=true`; the service-wiring runtime in
  // BaboonServiceWiring.scala is unchanged. These types are STATIC (no per-model
  // templating) — the only per-model code is the generated `<Service>McpServer`
  // and its tool-registry literals.
  //
  // The MCP dispatch surface is transport-abstract: it implements the JSON-RPC
  // method state machine + tool dispatch ONLY. Bytes-in/bytes-out (stdio framing,
  // Streamable-HTTP bodies) is an injected adapter the generated surface never
  // contains — mirroring the abstract-context service contract, which supplies
  // `Ctx` per invocation rather than baking an I/O loop into the wrapper.

  // --- JSON-RPC value types (already parsed from bytes by the adapter) ---
  //
  // `JsonRpcId` is `String | Long` per JSON-RPC 2.0. A notification carries no
  // `id`; the server produces no response for it. `params`/`result`/`error.data`
  // are represented as Circe `io.circe.Json` — the MCP runtime reuses what the
  // JSON codecs already speak rather than introducing a new model.

  sealed trait JsonRpcId
  object JsonRpcId {
    final case class StringId(value: String) extends JsonRpcId
    final case class LongId(value: Long)     extends JsonRpcId
  }

  final case class JsonRpcRequest(
    id: Option[JsonRpcId],
    method: String,
    params: Option[io.circe.Json],
  )

  final case class JsonRpcError(code: Int, message: String, data: Option[io.circe.Json] = None)

  final case class JsonRpcResponse(
    id: Option[JsonRpcId],
    result: Option[io.circe.Json] = None,
    error: Option[JsonRpcError] = None,
  )

  // JSON-RPC / MCP protocol constants (wire contract K4).
  object McpProtocol {
    val Version: String = "2025-06-18"
  }

  object JsonRpcErrorCodes {
    val ParseError: Int     = -32700
    val InvalidRequest: Int = -32600
    val MethodNotFound: Int = -32601
    val InvalidParams: Int  = -32602
    val InternalError: Int  = -32603
  }

  // --- Per-connection state (adapter-owned) ---
  //
  // The "initialized" precondition (reject `tools/*` before a successful
  // `initialize`) is per-connection state; a connection is a transport concept.
  // The latch therefore lives in this tiny value the adapter creates per
  // connection, NOT as ambient mutable state inside the server object (which stays
  // immutable and shareable across concurrent connections).
  final class McpSession {
    var initialized: Boolean = false
  }

  // --- Tool registry ---
  //
  // One entry per Baboon method bound to a server. `inputSchema` is the
  // precomputed, self-contained JSON Schema (from the shared T5 emitter), carried
  // as a constant value — the runtime does not compute schemas.
  final case class McpToolEntry(
    name: String,
    method: BaboonMethodId,
    inputSchema: io.circe.Json,
    description: Option[String] = None,
  )

  final case class McpServerInfo(name: String, version: String)

  // --- Dispatch interface ---
  //
  // The single generated entrypoint, analogous to `IBaboonJsonServiceCtx[Ctx,R]`.
  // It is NOT R-parametric (an MCP response is always a `JsonRpcResponse` value);
  // the only free type parameter is the caller's `Ctx` — the SAME `Ctx` the
  // service-wiring contract threads. `handle` is synchronous and performs no I/O.
  // It returns `None` for an accepted notification (no reply).
  trait IBaboonMcpServer[Ctx] {
    def handle(request: JsonRpcRequest, session: McpSession, ctx: Ctx, codecCtx: BaboonCodecContext): Option[JsonRpcResponse]
  }

  // The JSON `tools/call` delegate the generated server supplies: it routes one
  // tool invocation into the already-generated service dispatch (the errors-mode
  // `invokeJson`, which returns `Either[BaboonWiringError, String]`). The MCP layer
  // turns that `Either` into Channel-A / Channel-B per the wire contract (K4 §3).
  // The codecs are reached exclusively through this delegate; the MCP runtime holds
  // no codec logic itself.
  trait McpJsonInvoke[Ctx] {
    def apply(method: BaboonMethodId, data: String, ctx: Ctx, codecCtx: BaboonCodecContext): Either[BaboonWiringError, String]
  }

  // --- PUBLIC routable-server surface (tasks:T114) ---
  //
  // The composition seam the cross-service MCP muxer (AbstractMcpMuxer) depends
  // on. A sibling muxer reads each server's identity (`serverInfo`) and its
  // declaration-ordered registry (`tools`) to build the union tools/list and the
  // tool-name -> owner table, and routes a single tools/call into the owning
  // server via `routeToolCall` — reusing its existing Channel-A/Channel-B mapping
  // unchanged. Those inputs were `protected`/`private` on the base, so a sibling
  // could not compose them; this trait promotes exactly them to a stable PUBLIC
  // surface. The muxer depends on the trait, NEVER on `handle`.
  //
  // Scala MCP is Either-only (D24/T69): `routeToolCall` returns
  // `Either[BaboonWiringError, String]`. No async/HKT variant is emitted.
  trait IBaboonRoutableMcpServer[Ctx] {
    def serverInfo: McpServerInfo
    def tools: Seq[McpToolEntry]
    def routeToolCall(method: BaboonMethodId, data: String, ctx: Ctx, codecCtx: BaboonCodecContext): Either[BaboonWiringError, String]
  }

  // --- Transport-abstract dispatch base ---
  //
  // Shared `handle` state machine. The generated `<Service>McpServer` extends this
  // with its fixed `serverInfo`, ordered tool registry, and `invokeJson` delegate.
  // All JSON-RPC method strings ("tools/list" …) and result keys ("protocolVersion",
  // "inputSchema" …) are literal lowercase strings, NOT subject to any per-language
  // symbol casing.
  abstract class AbstractBaboonMcpServer[Ctx] extends IBaboonMcpServer[Ctx] with IBaboonRoutableMcpServer[Ctx] {
    // PUBLIC routable-server surface (tasks:T114): the muxer reads `serverInfo` /
    // `tools` and routes via `routeToolCall`, never via the private `byName()`
    // and never via `handle`.
    def serverInfo: McpServerInfo
    def tools: Seq[McpToolEntry]
    protected def invokeJson(method: BaboonMethodId, data: String, ctx: Ctx, codecCtx: BaboonCodecContext): Either[BaboonWiringError, String]

    // PUBLIC dispatch entry (tasks:T114): the same path `handle` drives for its
    // own tools/call arm, exposed for the muxer to reuse Channel-A/B unchanged.
    final def routeToolCall(method: BaboonMethodId, data: String, ctx: Ctx, codecCtx: BaboonCodecContext): Either[BaboonWiringError, String] =
      invokeJson(method, data, ctx, codecCtx)

    private def byName(): Map[String, McpToolEntry] =
      tools.map(t => t.name -> t).toMap

    override final def handle(request: JsonRpcRequest, session: McpSession, ctx: Ctx, codecCtx: BaboonCodecContext): Option[JsonRpcResponse] = {
      val id = request.id
      request.method match {
        case "initialize" =>
          val pv = request.params.flatMap(_.hcursor.downField("protocolVersion").as[String].toOption)
          if (request.params.isEmpty || pv.isEmpty) {
            Some(errorResponse(id, JsonRpcErrorCodes.InvalidParams, "initialize: missing protocolVersion"))
          } else {
            session.initialized = true
            val result = io.circe.Json.obj(
              "protocolVersion" -> io.circe.Json.fromString(McpProtocol.Version),
              "capabilities"    -> io.circe.Json.obj("tools" -> io.circe.Json.obj()),
              "serverInfo"      -> io.circe.Json.obj(
                "name"    -> io.circe.Json.fromString(serverInfo.name),
                "version" -> io.circe.Json.fromString(serverInfo.version),
              ),
            )
            Some(JsonRpcResponse(id, result = Some(result)))
          }

        case "notifications/initialized" =>
          None

        case "tools/list" =>
          if (!session.initialized) {
            Some(errorResponse(id, JsonRpcErrorCodes.InvalidRequest, "tools/list before initialize"))
          } else {
            val toolsArray = tools.map {
              t =>
                val base = Seq(
                  "name"        -> io.circe.Json.fromString(t.name),
                  "inputSchema" -> t.inputSchema,
                )
                val withDesc = t.description.fold(base)(d => base :+ ("description" -> io.circe.Json.fromString(d)))
                io.circe.Json.obj(withDesc: _*)
            }
            val result = io.circe.Json.obj("tools" -> io.circe.Json.arr(toolsArray: _*))
            Some(JsonRpcResponse(id, result = Some(result)))
          }

        case "tools/call" =>
          if (!session.initialized) {
            Some(errorResponse(id, JsonRpcErrorCodes.InvalidRequest, "tools/call before initialize"))
          } else {
            val nameOpt = request.params.flatMap(_.hcursor.downField("name").as[String].toOption)
            nameOpt match {
              case None =>
                Some(errorResponse(id, JsonRpcErrorCodes.InvalidParams, "tools/call: missing tool name"))
              case Some(name) =>
                byName().get(name) match {
                  case None =>
                    Some(errorResponse(id, JsonRpcErrorCodes.InvalidParams, s"tools/call: unknown tool '$name'"))
                  case Some(entry) =>
                    val argsJson = request.params
                      .flatMap(_.hcursor.downField("arguments").as[io.circe.Json].toOption)
                      .getOrElse(io.circe.Json.obj())
                      .noSpaces
                    invokeJson(entry.method, argsJson, ctx, codecCtx) match {
                      case Right(text) =>
                        val content = io.circe.Json.arr(io.circe.Json.obj("type" -> io.circe.Json.fromString("text"), "text" -> io.circe.Json.fromString(text)))
                        val result  = io.circe.Json.obj("content" -> content, "isError" -> io.circe.Json.fromBoolean(false))
                        Some(JsonRpcResponse(id, result = Some(result)))
                      case Left(err) =>
                        // Channel B: a valid protocol call whose domain payload failed.
                        val content = io.circe.Json.arr(io.circe.Json.obj("type" -> io.circe.Json.fromString("text"), "text" -> io.circe.Json.fromString(describeWiringError(err))))
                        val result  = io.circe.Json.obj("content" -> content, "isError" -> io.circe.Json.fromBoolean(true))
                        Some(JsonRpcResponse(id, result = Some(result)))
                    }
                }
            }
          }

        case other =>
          Some(errorResponse(id, JsonRpcErrorCodes.MethodNotFound, s"Method not found: $other"))
      }
    }

    protected def errorResponse(id: Option[JsonRpcId], code: Int, message: String): JsonRpcResponse =
      JsonRpcResponse(id, error = Some(JsonRpcError(code, message)))

    protected def describeWiringError(e: BaboonWiringError): String = e.toString
  }

  // --- MCP-muxer error taxonomy (tasks:T107; contract §6) ---
  //
  // The cross-service MCP muxer composes several <Service>McpServer instances
  // behind one endpoint. `DuplicateTool` is the MCP-tier analogue of the
  // service-wiring `DuplicateService` (a registration-time tool-name collision);
  // `NoMatchingTool` is the analogue of `NoMatchingService` (a dispatch-time
  // unknown tool name). They mirror that taxonomy exactly, but live in this
  // MCP-only runtime file rather than the always-shipped service-wiring runtime
  // (BaboonServiceWiring.scala) so that with `--scala-generate-mcp-server`
  // absent the generated output is byte-identical to the pre-MCP baseline (no
  // MCP types leak into the unconditional runtime).
  // `BaboonMcpWiringException` is the MCP-tier carrier — the structural twin of
  // `BaboonWiringException` (a thrown programmer error carrying a tagged value),
  // so `DuplicateTool` propagates to the integrator exactly as `DuplicateService`
  // does, just from the MCP-only runtime.
  sealed trait BaboonMcpWiringError
  object BaboonMcpWiringError {
    final case class DuplicateTool(toolName: String) extends BaboonMcpWiringError
    final case class NoMatchingTool(toolName: String) extends BaboonMcpWiringError
  }

  class BaboonMcpWiringException(val error: BaboonMcpWiringError) extends RuntimeException(error.toString)

  // --- Cross-service MCP muxer (tasks:T107; contract:
  // docs/research/mcp-muxer-runtime-contract.md) ---
  //
  // `AbstractMcpMuxer[Ctx]` composes several `<Service>McpServer[Ctx]` instances
  // behind ONE MCP endpoint so a single connection serves the union of their
  // tools. It is the MCP-tier sibling of `JsonMuxer`: where `JsonMuxer` keys
  // `LinkedHashMap<serviceName, IBaboonJsonService>` and routes by
  // `method.serviceName`, the muxer keys a name→owning-server table and routes
  // by the inbound flat MCP tool name (contract §1).
  //
  // Composition seam: it depends ONLY on the PUBLIC `IBaboonRoutableMcpServer[Ctx]`
  // surface (tasks:T114) — it reads each server's `serverInfo` and `tools`, and
  // routes each `tools/call` via the public `routeToolCall`. It NEVER reads
  // protected members and NEVER calls a server's `handle()` (a member's `handle`
  // resolves only its OWN tools and returns "unknown tool" for any cross-service
  // name — contract §4). The muxer owns the JSON-RPC envelope; each member owns
  // its domain dispatch.
  //
  // Scala MCP is Either-only (D24/T69): this muxer is synchronous and
  // Either-shaped to match `AbstractBaboonMcpServer`. No async variant is
  // emitted for Scala.
  //
  // The `handle` state machine is the SAME shape as the per-service base, with
  // three arms differing only in operating over the union: `tools/list` returns
  // the union, `tools/call` routes by tool name to the owning server, and
  // `initialize` returns a single merged `serverInfo` supplied to the ctor.
  class AbstractMcpMuxer[Ctx](
    mergedServerInfo: McpServerInfo,
    initServers: IBaboonRoutableMcpServer[Ctx]*,
  ) extends IBaboonMcpServer[Ctx] {
    // Registration-order-preserving table: tool name -> owning server.
    // Built at registration time (contract §2), never per request.
    private val route  = scala.collection.mutable.LinkedHashMap.empty[String, IBaboonRoutableMcpServer[Ctx]]
    private val entries = scala.collection.mutable.LinkedHashMap.empty[String, McpToolEntry]

    initServers.foreach(register)

    // Folds the server's declaration-ordered `tools` into the union table;
    // throws DuplicateTool on a tool-name collision across servers (the exact
    // MCP-tier analogue of JsonMuxer.register throwing DuplicateService).
    def register(server: IBaboonRoutableMcpServer[Ctx]): Unit = {
      server.tools.foreach {
        t =>
          if (route.contains(t.name)) {
            throw new BaboonMcpWiringException(BaboonMcpWiringError.DuplicateTool(t.name))
          }
          route.update(t.name, server)
          entries.update(t.name, t)
      }
    }

    override final def handle(request: JsonRpcRequest, session: McpSession, ctx: Ctx, codecCtx: BaboonCodecContext): Option[JsonRpcResponse] = {
      val id = request.id
      request.method match {
        case "initialize" =>
          val pv = request.params.flatMap(_.hcursor.downField("protocolVersion").as[String].toOption)
          if (request.params.isEmpty || pv.isEmpty) {
            Some(errorResponse(id, JsonRpcErrorCodes.InvalidParams, "initialize: missing protocolVersion"))
          } else {
            session.initialized = true
            val result = io.circe.Json.obj(
              "protocolVersion" -> io.circe.Json.fromString(McpProtocol.Version),
              "capabilities"    -> io.circe.Json.obj("tools" -> io.circe.Json.obj()),
              "serverInfo"      -> io.circe.Json.obj(
                "name"    -> io.circe.Json.fromString(mergedServerInfo.name),
                "version" -> io.circe.Json.fromString(mergedServerInfo.version),
              ),
            )
            Some(JsonRpcResponse(id, result = Some(result)))
          }

        case "notifications/initialized" =>
          None

        case "tools/list" =>
          if (!session.initialized) {
            Some(errorResponse(id, JsonRpcErrorCodes.InvalidRequest, "tools/list before initialize"))
          } else {
            val toolsArray = toolsListUnion()
            val result = io.circe.Json.obj("tools" -> io.circe.Json.arr(toolsArray: _*))
            Some(JsonRpcResponse(id, result = Some(result)))
          }

        case "tools/call" =>
          if (!session.initialized) {
            Some(errorResponse(id, JsonRpcErrorCodes.InvalidRequest, "tools/call before initialize"))
          } else {
            val nameOpt = request.params.flatMap(_.hcursor.downField("name").as[String].toOption)
            nameOpt match {
              case None =>
                Some(errorResponse(id, JsonRpcErrorCodes.InvalidParams, "tools/call: missing tool name"))
              case Some(name) =>
                route.get(name) match {
                  case None =>
                    // NoMatchingTool: surfaced as the SAME wire response the per-service
                    // base uses for an unknown tool (-32602, "unknown tool '<name>'"),
                    // so the bytes are identical whether one server or the muxer rejects.
                    Some(errorResponse(id, JsonRpcErrorCodes.InvalidParams, s"tools/call: unknown tool '$name'"))
                  case Some(server) =>
                    val entry    = entries(name)
                    val argsJson = request.params
                      .flatMap(_.hcursor.downField("arguments").as[io.circe.Json].toOption)
                      .getOrElse(io.circe.Json.obj())
                      .noSpaces
                    server.routeToolCall(entry.method, argsJson, ctx, codecCtx) match {
                      case Right(text) =>
                        val content = io.circe.Json.arr(io.circe.Json.obj("type" -> io.circe.Json.fromString("text"), "text" -> io.circe.Json.fromString(text)))
                        val result  = io.circe.Json.obj("content" -> content, "isError" -> io.circe.Json.fromBoolean(false))
                        Some(JsonRpcResponse(id, result = Some(result)))
                      case Left(err) =>
                        // Channel B: a valid protocol call whose domain payload failed.
                        val content = io.circe.Json.arr(io.circe.Json.obj("type" -> io.circe.Json.fromString("text"), "text" -> io.circe.Json.fromString(describeWiringError(err))))
                        val result  = io.circe.Json.obj("content" -> content, "isError" -> io.circe.Json.fromBoolean(true))
                        Some(JsonRpcResponse(id, result = Some(result)))
                    }
                }
            }
          }

        case other =>
          Some(errorResponse(id, JsonRpcErrorCodes.MethodNotFound, s"Method not found: $other"))
      }
    }

    // Backs tools/list (§3.2): the union of all registered servers' tool entries
    // in registration-then-declaration order (the insertion order of `entries`),
    // each in the same shape the per-service base emits.
    private def toolsListUnion(): Seq[io.circe.Json] =
      entries.values.map {
        t =>
          val base = Seq(
            "name"        -> io.circe.Json.fromString(t.name),
            "inputSchema" -> t.inputSchema,
          )
          val withDesc = t.description.fold(base)(d => base :+ ("description" -> io.circe.Json.fromString(d)))
          io.circe.Json.obj(withDesc: _*)
      }.toSeq

    protected def errorResponse(id: Option[JsonRpcId], code: Int, message: String): JsonRpcResponse =
      JsonRpcResponse(id, error = Some(JsonRpcError(code, message)))

    protected def describeWiringError(e: BaboonWiringError): String = e.toString
  }
}
