import Foundation

// Additive MCP server runtime (decisions ledger M1; contract:
// docs/research/mcp-dispatch-runtime-contract.md). Emitted only when
// `--swift-generate-mcp-server=true`; the service-wiring runtime in
// baboon_service_wiring.swift is unchanged. These types are STATIC (no
// per-model templating) — the only per-model code is the generated
// `<Service>McpServer<Ctx>` and its tool-registry literals.
//
// The MCP dispatch surface is transport-abstract: it implements the JSON-RPC
// method state machine + tool dispatch ONLY. Bytes-in/bytes-out (stdio framing,
// Streamable-HTTP bodies) is an injected adapter the generated surface never
// contains — mirroring the abstract-context service contract, which supplies
// `Ctx` per invocation rather than baking an I/O loop into the wrapper.

// --- JSON-RPC value types (already parsed from bytes by the adapter) ---
//
// A notification carries no `id`; the server produces no response for it.
// `params`/`result`/`error.data` use Swift `Any?` (the JSONSerialization
// object graph: `[String: Any]`, `[Any]`, `NSNumber`, `String`, `NSNull`) —
// the MCP runtime reuses what JSONSerialization already speaks rather than
// introducing a new model. This mirrors the Swift service-wiring JSON path,
// which also threads `Any` from `JSONSerialization.jsonObject`.

public struct JsonRpcRequest {
    public let id: Any?
    public let method: String
    public let params: Any?

    public init(_ id: Any?, _ method: String, _ params: Any?) {
        self.id = id
        self.method = method
        self.params = params
    }
}

public struct JsonRpcError {
    public let code: Int
    public let message: String
    public let data: Any?

    public init(_ code: Int, _ message: String, _ data: Any? = nil) {
        self.code = code
        self.message = message
        self.data = data
    }
}

public struct JsonRpcResponse {
    public let id: Any?
    public let result: Any?
    public let error: JsonRpcError?

    public init(_ id: Any?, result: Any? = nil, error: JsonRpcError? = nil) {
        self.id = id
        self.result = result
        self.error = error
    }
}

// JSON-RPC / MCP protocol constants (wire contract K4).
public let mcpProtocolVersion = "2025-06-18"

public let jsonRpcErrorParseError = -32700
public let jsonRpcErrorInvalidRequest = -32600
public let jsonRpcErrorMethodNotFound = -32601
public let jsonRpcErrorInvalidParams = -32602
public let jsonRpcErrorInternalError = -32603

// --- Per-connection state (adapter-owned) ---
//
// The "initialized" precondition (reject `tools/*` before a successful
// `initialize`) is per-connection state; a connection is a transport concept.
// The latch therefore lives in this tiny value the adapter creates per
// connection, NOT as ambient mutable state inside the server object (which
// stays immutable and shareable across concurrent connections). It is a
// reference type so the shared `handle` extension can flip the latch in place.
public final class McpSession {
    public var initialized: Bool = false
    public init() {}
}

// --- Tool registry ---
//
// One entry per Baboon method bound to a server. `inputSchema` is the
// precomputed, self-contained JSON Schema (from the shared T5 emitter), carried
// as a parsed `[String: Any]` value — the runtime does not compute schemas.
public struct McpToolEntry {
    public let name: String
    public let method: BaboonMethodId
    public let inputSchema: [String: Any]
    public let description: String?

    public init(_ name: String, _ method: BaboonMethodId, _ inputSchema: [String: Any], _ description: String? = nil) {
        self.name = name
        self.method = method
        self.inputSchema = inputSchema
        self.description = description
    }
}

public struct McpServerInfo {
    public let name: String
    public let version: String

    public init(_ name: String, _ version: String) {
        self.name = name
        self.version = version
    }
}

// --- Dispatch interface ---
//
// The single generated entrypoint, analogous to `IBaboonJsonServiceCtx`. It is
// NOT result-parametric (an MCP response is always a `JsonRpcResponse` value);
// the only free type parameter is the caller's `Ctx` — the SAME `Ctx` the
// service-wiring contract threads. Swift `protocol` with `associatedtype`
// cannot be used directly as an existential when the muxer-style flat storage
// needs erasure, so this follows the verbatim `IBaboonJsonServiceCtx` ->
// `AnyJsonServiceCtx<Ctx, R>` precedent: an `associatedtype Ctx` protocol plus
// an `AnyMcpServer<Ctx>` type-eraser below.
//
// `handle` is synchronous and performs no I/O; it returns `nil` for an accepted
// notification (no reply). It carries a default implementation (the shared
// state machine) in the protocol extension, so a conforming generated server
// supplies only `serverInfo`, `tools`, and the `invokeJson` delegate.
public protocol IBaboonMcpServer {
    associatedtype Ctx
    var serverInfo: McpServerInfo { get }
    var tools: [McpToolEntry] { get }

    // The JSON `tools/call` delegate the generated server supplies: routes one
    // tool invocation into the already-generated service dispatch (the
    // errors-mode `invokeJson`, which throws `BaboonWiringException` on a
    // domain-payload failure). Reuses the existing JSONSerialization codecs via
    // the generated `<Svc>Wiring.invokeJson`.
    func invokeJson(_ method: BaboonMethodId, _ data: String, _ ctx: Ctx, _ codecCtx: BaboonCodecContext) throws -> String

    func handle(_ request: JsonRpcRequest, _ session: McpSession, _ ctx: Ctx, _ codecCtx: BaboonCodecContext) -> JsonRpcResponse?
}

// --- Transport-abstract dispatch state machine ---
//
// Shared `handle` logic as a protocol-extension default. The generated
// `<Service>McpServer<Ctx>` supplies its fixed `serverInfo`, ordered tool
// registry, and `invokeJson` delegate; this drives the JSON-RPC method state
// machine over them. All JSON-RPC method strings ("tools/list" …) and result
// keys ("protocolVersion", "inputSchema" …) are literal lowercase strings, NOT
// subject to any per-language symbol casing.
extension IBaboonMcpServer {
    private func byName() -> [String: McpToolEntry] {
        var m: [String: McpToolEntry] = [:]
        for t in tools { m[t.name] = t }
        return m
    }

    // --- PUBLIC routable-server surface (tasks:T114) ---
    //
    // `serverInfo`/`tools` are already public protocol requirements; this adds
    // `routeToolCall`, the public name for the per-service dispatch entry that
    // `handle` drives for its own `tools/call` arm (`invokeJson`). The
    // cross-service MCP muxer (AbstractMcpMuxer) composes registered servers
    // through `serverInfo`/`tools`/`routeToolCall` ONLY — never via the private
    // `byName()` and never via `handle`. It reuses Channel-A/Channel-B mapping
    // unchanged.
    public func routeToolCall(_ method: BaboonMethodId, _ data: String, _ ctx: Ctx, _ codecCtx: BaboonCodecContext) throws -> String {
        return try invokeJson(method, data, ctx, codecCtx)
    }

    private func errorResponse(_ id: Any?, _ code: Int, _ message: String) -> JsonRpcResponse {
        return JsonRpcResponse(id, error: JsonRpcError(code, message))
    }

    public func handle(_ request: JsonRpcRequest, _ session: McpSession, _ ctx: Ctx, _ codecCtx: BaboonCodecContext) -> JsonRpcResponse? {
        let id = request.id
        switch request.method {
        case "initialize":
            guard let params = request.params as? [String: Any] else {
                return errorResponse(id, jsonRpcErrorInvalidParams, "initialize: missing params")
            }
            if params["protocolVersion"] == nil {
                return errorResponse(id, jsonRpcErrorInvalidParams, "initialize: missing protocolVersion")
            }
            session.initialized = true
            let result: [String: Any] = [
                "protocolVersion": mcpProtocolVersion,
                "capabilities": ["tools": [String: Any]()],
                "serverInfo": ["name": serverInfo.name, "version": serverInfo.version],
            ]
            return JsonRpcResponse(id, result: result)

        case "notifications/initialized":
            return nil

        case "tools/list":
            if !session.initialized {
                return errorResponse(id, jsonRpcErrorInvalidRequest, "tools/list before initialize")
            }
            var toolsArr: [[String: Any]] = []
            for t in tools {
                var entry: [String: Any] = [
                    "name": t.name,
                    "inputSchema": t.inputSchema,
                ]
                if let d = t.description {
                    entry["description"] = d
                }
                toolsArr.append(entry)
            }
            return JsonRpcResponse(id, result: ["tools": toolsArr])

        case "tools/call":
            if !session.initialized {
                return errorResponse(id, jsonRpcErrorInvalidRequest, "tools/call before initialize")
            }
            guard let params = request.params as? [String: Any] else {
                return errorResponse(id, jsonRpcErrorInvalidParams, "tools/call: missing params")
            }
            guard let toolName = params["name"] as? String else {
                return errorResponse(id, jsonRpcErrorInvalidParams, "tools/call: missing tool name")
            }
            guard let entry = byName()[toolName] else {
                return errorResponse(id, jsonRpcErrorInvalidParams, "tools/call: unknown tool '\(toolName)'")
            }
            let argsRaw: Any = params["arguments"] ?? [String: Any]()
            let argsJson: String
            do {
                let data = try JSONSerialization.data(withJSONObject: argsRaw, options: [.sortedKeys, .fragmentsAllowed])
                argsJson = String(data: data, encoding: .utf8)!
            } catch {
                return errorResponse(id, jsonRpcErrorInternalError, "tools/call: failed to serialize arguments: \(error)")
            }
            do {
                let resultStr = try invokeJson(entry.method, argsJson, ctx, codecCtx)
                let result: [String: Any] = [
                    "content": [["type": "text", "text": resultStr]],
                    "isError": false,
                ]
                return JsonRpcResponse(id, result: result)
            } catch let e as BaboonWiringException {
                // Channel B: a valid protocol call whose domain payload failed.
                let result: [String: Any] = [
                    "content": [["type": "text", "text": describeWiringError(e.error)]],
                    "isError": true,
                ]
                return JsonRpcResponse(id, result: result)
            } catch {
                // Channel B: unexpected error during dispatch.
                let result: [String: Any] = [
                    "content": [["type": "text", "text": "\(error)"]],
                    "isError": true,
                ]
                return JsonRpcResponse(id, result: result)
            }

        default:
            return errorResponse(id, jsonRpcErrorMethodNotFound, "Method not found: \(request.method)")
        }
    }

    private func describeWiringError(_ e: BaboonWiringError) -> String {
        return "\(e)"
    }
}

// --- Type-erasing box ---
//
// `IBaboonMcpServer` carries an `associatedtype Ctx`, so it cannot be used as a
// bare existential when an adapter needs to hold a server behind a single
// `Ctx`. `AnyMcpServer<Ctx>` erases the concrete conformer to a closure over
// `handle`, mirroring the verbatim `AnyJsonServiceCtx<Ctx, R>` precedent in the
// service-wiring runtime. The adapter constructs `Ctx`, builds the concrete
// `<Svc>McpServer<Ctx>`, optionally wraps it here, then calls `handle`.
public struct AnyMcpServer<Ctx> {
    private let _handle: (JsonRpcRequest, McpSession, Ctx, BaboonCodecContext) -> JsonRpcResponse?

    public init<S: IBaboonMcpServer>(_ s: S) where S.Ctx == Ctx {
        self._handle = s.handle
    }

    public func handle(_ request: JsonRpcRequest, _ session: McpSession, _ ctx: Ctx, _ codecCtx: BaboonCodecContext) -> JsonRpcResponse? {
        return _handle(request, session, ctx, codecCtx)
    }
}

// ===========================================================================
// Async MCP dispatch surface (D24/T67).
//
// Emitted ONLY when the Swift target has `--sw-async-services=true`. Under that
// axis the generated no-errors service-wiring dispatcher `<Svc>Wiring.invokeJson`
// is `async throws -> String` (SwServiceWiringTranslator `dispatcherEffects`),
// so its result requires `await`. The generated MCP server therefore binds an
// `async throws` delegate rather than the sync `throws -> String` one above, and
// conforms to `IBaboonAsyncMcpServer` instead of `IBaboonMcpServer`.
//
// `handle` is GENUINELY `async` on this surface (the async analogue of the sync
// `IBaboonMcpServer.handle`): the adapter calls `await server.handle(...)`. The
// single async hop — the `tools/call` delegate — is `await`ed DIRECTLY in the
// caller's task; there is NO synchronous bridge (no `DispatchSemaphore`, no
// spawned `Task`). This is deadlock-free from any execution context, including
// an actor-isolated (`@MainActor` or custom-actor) caller: a sync semaphore
// bridge that parks the calling thread while a context-inheriting `Task` waits
// on that same actor's executor would deadlock (the inherited task can never run
// because the blocked thread holds the actor). Awaiting directly suspends the
// caller's task cooperatively and resumes it when `invokeJson` completes —
// structurally immune to that deadlock class and to cooperative-pool starvation.
// All other JSON-RPC arms (initialize / tools/list / errors) are pure (no
// suspension points) and identical to the sync state machine; only the
// `invokeJson` call is awaited.
//
// The sync `IBaboonMcpServer` protocol + extension above are UNTOUCHED, so with
// `--sw-async-services=false` the generated output is byte-identical to baseline.
// ===========================================================================
public protocol IBaboonAsyncMcpServer {
    associatedtype Ctx
    var serverInfo: McpServerInfo { get }
    var tools: [McpToolEntry] { get }

    // The async `tools/call` delegate the generated server supplies: routes one
    // tool invocation into the generated async service dispatch (the
    // errors-mode/no-errors `invokeJson`, which is `async throws` under the async
    // axis). The async `handle` awaits it directly in the caller's task.
    func invokeJson(_ method: BaboonMethodId, _ data: String, _ ctx: Ctx, _ codecCtx: BaboonCodecContext) async throws -> String

    func handle(_ request: JsonRpcRequest, _ session: McpSession, _ ctx: Ctx, _ codecCtx: BaboonCodecContext) async -> JsonRpcResponse?
}

extension IBaboonAsyncMcpServer {
    private func byName() -> [String: McpToolEntry] {
        var m: [String: McpToolEntry] = [:]
        for t in tools { m[t.name] = t }
        return m
    }

    // PUBLIC routable-server surface (tasks:T114), async flavour: `routeToolCall`
    // is `async throws`; the muxer awaits it before applying Channel-A/B.
    public func routeToolCall(_ method: BaboonMethodId, _ data: String, _ ctx: Ctx, _ codecCtx: BaboonCodecContext) async throws -> String {
        return try await invokeJson(method, data, ctx, codecCtx)
    }

    private func errorResponse(_ id: Any?, _ code: Int, _ message: String) -> JsonRpcResponse {
        return JsonRpcResponse(id, error: JsonRpcError(code, message))
    }

    public func handle(_ request: JsonRpcRequest, _ session: McpSession, _ ctx: Ctx, _ codecCtx: BaboonCodecContext) async -> JsonRpcResponse? {
        let id = request.id
        switch request.method {
        case "initialize":
            guard let params = request.params as? [String: Any] else {
                return errorResponse(id, jsonRpcErrorInvalidParams, "initialize: missing params")
            }
            if params["protocolVersion"] == nil {
                return errorResponse(id, jsonRpcErrorInvalidParams, "initialize: missing protocolVersion")
            }
            session.initialized = true
            let result: [String: Any] = [
                "protocolVersion": mcpProtocolVersion,
                "capabilities": ["tools": [String: Any]()],
                "serverInfo": ["name": serverInfo.name, "version": serverInfo.version],
            ]
            return JsonRpcResponse(id, result: result)

        case "notifications/initialized":
            return nil

        case "tools/list":
            if !session.initialized {
                return errorResponse(id, jsonRpcErrorInvalidRequest, "tools/list before initialize")
            }
            var toolsArr: [[String: Any]] = []
            for t in tools {
                var entry: [String: Any] = [
                    "name": t.name,
                    "inputSchema": t.inputSchema,
                ]
                if let d = t.description {
                    entry["description"] = d
                }
                toolsArr.append(entry)
            }
            return JsonRpcResponse(id, result: ["tools": toolsArr])

        case "tools/call":
            if !session.initialized {
                return errorResponse(id, jsonRpcErrorInvalidRequest, "tools/call before initialize")
            }
            guard let params = request.params as? [String: Any] else {
                return errorResponse(id, jsonRpcErrorInvalidParams, "tools/call: missing params")
            }
            guard let toolName = params["name"] as? String else {
                return errorResponse(id, jsonRpcErrorInvalidParams, "tools/call: missing tool name")
            }
            guard let entry = byName()[toolName] else {
                return errorResponse(id, jsonRpcErrorInvalidParams, "tools/call: unknown tool '\(toolName)'")
            }
            let argsRaw: Any = params["arguments"] ?? [String: Any]()
            let argsJson: String
            do {
                let data = try JSONSerialization.data(withJSONObject: argsRaw, options: [.sortedKeys, .fragmentsAllowed])
                argsJson = String(data: data, encoding: .utf8)!
            } catch {
                return errorResponse(id, jsonRpcErrorInternalError, "tools/call: failed to serialize arguments: \(error)")
            }
            do {
                let resultStr = try await invokeJson(entry.method, argsJson, ctx, codecCtx)
                let result: [String: Any] = [
                    "content": [["type": "text", "text": resultStr]],
                    "isError": false,
                ]
                return JsonRpcResponse(id, result: result)
            } catch let e as BaboonWiringException {
                // Channel B: a valid protocol call whose domain payload failed.
                let result: [String: Any] = [
                    "content": [["type": "text", "text": describeWiringError(e.error)]],
                    "isError": true,
                ]
                return JsonRpcResponse(id, result: result)
            } catch {
                // Channel B: unexpected error during dispatch.
                let result: [String: Any] = [
                    "content": [["type": "text", "text": "\(error)"]],
                    "isError": true,
                ]
                return JsonRpcResponse(id, result: result)
            }

        default:
            return errorResponse(id, jsonRpcErrorMethodNotFound, "Method not found: \(request.method)")
        }
    }

    private func describeWiringError(_ e: BaboonWiringError) -> String {
        return "\(e)"
    }
}

// Type-erasing box for the async dispatch surface, the async analogue of
// `AnyMcpServer<Ctx>`. `handle` is `async` (it awaits the async delegate
// directly), so the erased closure type carries the `async` effect.
public struct AnyAsyncMcpServer<Ctx> {
    private let _handle: (JsonRpcRequest, McpSession, Ctx, BaboonCodecContext) async -> JsonRpcResponse?

    public init<S: IBaboonAsyncMcpServer>(_ s: S) where S.Ctx == Ctx {
        self._handle = s.handle
    }

    public func handle(_ request: JsonRpcRequest, _ session: McpSession, _ ctx: Ctx, _ codecCtx: BaboonCodecContext) async -> JsonRpcResponse? {
        return await _handle(request, session, ctx, codecCtx)
    }
}

// ===========================================================================
// MCP-muxer error taxonomy (tasks:T113; contract §6).
//
// The cross-service MCP muxer composes several <Service>McpServer instances
// behind one endpoint. `duplicateTool` is the MCP-tier analogue of the
// service-wiring `BaboonWiringError.duplicateService` (a registration-time
// tool-name collision); `noMatchingTool` is the analogue of `noMatchingService`
// (a dispatch-time unknown tool name). They mirror that taxonomy and carrier
// exactly, but live in this MCP-only runtime file rather than the always-shipped
// service-wiring runtime (baboon_service_wiring.swift) so that with
// `--sw-generate-mcp-server` absent the generated output is byte-identical to the
// pre-MCP baseline (no MCP types leak into the unconditional runtime).
// `BaboonMcpWiringException` is the MCP-tier carrier — the structural twin of the
// service-wiring `BaboonWiringException` (a thrown programmer error carrying a
// tagged value), so `duplicateTool` propagates to the integrator exactly as
// `duplicateService` does, just from the MCP-only runtime.
public enum BaboonMcpWiringError: Error {
    case duplicateTool(String)
    case noMatchingTool(String)
}

public struct BaboonMcpWiringException: Error {
    public let error: BaboonMcpWiringError
    public init(_ error: BaboonMcpWiringError) { self.error = error }
}

// --- Type-erasing routable-server box (sync) ---
//
// `IBaboonMcpServer` carries an `associatedtype Ctx`, so a homogeneous muxer
// store cannot hold `[any IBaboonMcpServer]` keyed by a single `Ctx`. This box
// captures exactly the T114 PUBLIC routable surface — `serverInfo`, `tools`, and
// the `routeToolCall` dispatch entry — from any conforming server, mirroring the
// `AnyJsonService<R>` precedent in the service-wiring runtime. The muxer's flat
// storage stays `[AnyRoutableMcpServer<Ctx>]`. It NEVER captures `handle()`.
public struct AnyRoutableMcpServer<Ctx> {
    public let serverInfo: McpServerInfo
    public let tools: [McpToolEntry]
    private let _routeToolCall: (BaboonMethodId, String, Ctx, BaboonCodecContext) throws -> String

    public init<S: IBaboonMcpServer>(_ s: S) where S.Ctx == Ctx {
        self.serverInfo = s.serverInfo
        self.tools = s.tools
        self._routeToolCall = s.routeToolCall
    }

    public func routeToolCall(_ method: BaboonMethodId, _ data: String, _ ctx: Ctx, _ codecCtx: BaboonCodecContext) throws -> String {
        return try _routeToolCall(method, data, ctx, codecCtx)
    }
}

// --- Cross-service MCP muxer (tasks:T113; contract:
// docs/research/mcp-muxer-runtime-contract.md) ---
//
// `AbstractMcpMuxer<Ctx>` composes several `<Service>McpServer<Ctx>` instances
// behind ONE MCP endpoint so a single connection serves the union of their
// tools. It is the MCP-tier sibling of the service `JsonMuxer`: where `JsonMuxer`
// keys `[serviceName: AnyJsonService]` and routes by `method.serviceId`, the
// muxer keys `[toolName: owningServer]` and routes by the inbound flat MCP tool
// name (contract §1).
//
// Composition seam: it depends ONLY on the PUBLIC routable surface (tasks:T114)
// — it reads each server's `serverInfo` and `tools`, and routes each `tools/call`
// via the public `routeToolCall`. It NEVER reads private members and NEVER calls
// a server's `handle()` (a member's `handle` resolves only its OWN tools and
// returns "unknown tool" for any cross-service name — contract §4). The muxer
// owns the JSON-RPC envelope; each member owns its domain dispatch.
//
// The `handle` state machine is the SAME shape as the per-service base, with
// three arms differing only in operating over the union: `tools/list` returns the
// union, `tools/call` routes by tool name to the owning server, and `initialize`
// returns a single merged `serverInfo` supplied to the ctor.
public final class AbstractMcpMuxer<Ctx> {
    // Registration order preserved (an order-preserving `_toolOrder` array
    // alongside the routing dictionaries — JsonMuxer LinkedHashMap precedent).
    // `_route`/`_entries`/`_toolOrder` are built at registration (contract §2),
    // never per request.
    private var _servers: [AnyRoutableMcpServer<Ctx>] = []
    private var _route: [String: AnyRoutableMcpServer<Ctx>] = [:]
    private var _entries: [String: McpToolEntry] = [:]
    private var _toolOrder: [String] = []
    private let _mergedServerInfo: McpServerInfo

    // varargs ctor mirrors `JsonMuxer(_ services: ...)`. `mergedServerInfo` is the
    // composed endpoint's single identity returned by `initialize` (§3.1).
    public init(_ mergedServerInfo: McpServerInfo, _ servers: AnyRoutableMcpServer<Ctx>...) throws {
        self._mergedServerInfo = mergedServerInfo
        for s in servers { try register(s) }
    }

    public init(_ mergedServerInfo: McpServerInfo, _ servers: [AnyRoutableMcpServer<Ctx>]) throws {
        self._mergedServerInfo = mergedServerInfo
        for s in servers { try register(s) }
    }

    // Folds the server's declaration-ordered `tools` into the union table; throws
    // duplicateTool on a tool-name collision across servers (the exact MCP-tier
    // analogue of JsonMuxer.register throwing duplicateService).
    public func register(_ server: AnyRoutableMcpServer<Ctx>) throws {
        for t in server.tools {
            if _route[t.name] != nil {
                throw BaboonMcpWiringException(BaboonMcpWiringError.duplicateTool(t.name))
            }
            _route[t.name] = server
            _entries[t.name] = t
            _toolOrder.append(t.name)
        }
        _servers.append(server)
    }

    private func errorResponse(_ id: Any?, _ code: Int, _ message: String) -> JsonRpcResponse {
        return JsonRpcResponse(id, error: JsonRpcError(code, message))
    }

    public func handle(_ request: JsonRpcRequest, _ session: McpSession, _ ctx: Ctx, _ codecCtx: BaboonCodecContext) -> JsonRpcResponse? {
        let id = request.id
        switch request.method {
        case "initialize":
            guard let params = request.params as? [String: Any] else {
                return errorResponse(id, jsonRpcErrorInvalidParams, "initialize: missing params")
            }
            if params["protocolVersion"] == nil {
                return errorResponse(id, jsonRpcErrorInvalidParams, "initialize: missing protocolVersion")
            }
            session.initialized = true
            let result: [String: Any] = [
                "protocolVersion": mcpProtocolVersion,
                "capabilities": ["tools": [String: Any]()],
                "serverInfo": ["name": _mergedServerInfo.name, "version": _mergedServerInfo.version],
            ]
            return JsonRpcResponse(id, result: result)

        case "notifications/initialized":
            return nil

        case "tools/list":
            if !session.initialized {
                return errorResponse(id, jsonRpcErrorInvalidRequest, "tools/list before initialize")
            }
            return JsonRpcResponse(id, result: ["tools": toolsListUnion()])

        case "tools/call":
            if !session.initialized {
                return errorResponse(id, jsonRpcErrorInvalidRequest, "tools/call before initialize")
            }
            guard let params = request.params as? [String: Any] else {
                return errorResponse(id, jsonRpcErrorInvalidParams, "tools/call: missing params")
            }
            guard let toolName = params["name"] as? String else {
                return errorResponse(id, jsonRpcErrorInvalidParams, "tools/call: missing tool name")
            }
            guard let server = _route[toolName], let entry = _entries[toolName] else {
                // NoMatchingTool: surfaced as the SAME wire response the per-service
                // base uses for an unknown tool (-32602, "unknown tool '<name>'"),
                // so the bytes are identical whether one server or the muxer rejects.
                return errorResponse(id, jsonRpcErrorInvalidParams, "tools/call: unknown tool '\(toolName)'")
            }
            let argsRaw: Any = params["arguments"] ?? [String: Any]()
            let argsJson: String
            do {
                let data = try JSONSerialization.data(withJSONObject: argsRaw, options: [.sortedKeys, .fragmentsAllowed])
                argsJson = String(data: data, encoding: .utf8)!
            } catch {
                return errorResponse(id, jsonRpcErrorInternalError, "tools/call: failed to serialize arguments: \(error)")
            }
            do {
                let resultStr = try server.routeToolCall(entry.method, argsJson, ctx, codecCtx)
                let result: [String: Any] = [
                    "content": [["type": "text", "text": resultStr]],
                    "isError": false,
                ]
                return JsonRpcResponse(id, result: result)
            } catch let e as BaboonWiringException {
                // Channel B: a valid protocol call whose domain payload failed.
                let result: [String: Any] = [
                    "content": [["type": "text", "text": describeWiringError(e.error)]],
                    "isError": true,
                ]
                return JsonRpcResponse(id, result: result)
            } catch {
                // Channel B: unexpected error during dispatch.
                let result: [String: Any] = [
                    "content": [["type": "text", "text": "\(error)"]],
                    "isError": true,
                ]
                return JsonRpcResponse(id, result: result)
            }

        default:
            return errorResponse(id, jsonRpcErrorMethodNotFound, "Method not found: \(request.method)")
        }
    }

    // Backs tools/list (§3.2): the union of all registered servers' tool entries
    // in registration-then-declaration order (the insertion order of `_toolOrder`),
    // each in the same shape the per-service base emits.
    private func toolsListUnion() -> [[String: Any]] {
        var toolsArr: [[String: Any]] = []
        for name in _toolOrder {
            guard let t = _entries[name] else { continue }
            var entry: [String: Any] = [
                "name": t.name,
                "inputSchema": t.inputSchema,
            ]
            if let d = t.description {
                entry["description"] = d
            }
            toolsArr.append(entry)
        }
        return toolsArr
    }

    private func describeWiringError(_ e: BaboonWiringError) -> String {
        return "\(e)"
    }
}

// --- Type-erasing routable-server box (async) ---
//
// Async analogue of `AnyRoutableMcpServer`: captures the T114 PUBLIC async
// routable surface from any `IBaboonAsyncMcpServer` (whose `routeToolCall` is
// `async throws`). The async muxer's flat storage is
// `[AnyAsyncRoutableMcpServer<Ctx>]`.
public struct AnyAsyncRoutableMcpServer<Ctx> {
    public let serverInfo: McpServerInfo
    public let tools: [McpToolEntry]
    private let _routeToolCall: (BaboonMethodId, String, Ctx, BaboonCodecContext) async throws -> String

    public init<S: IBaboonAsyncMcpServer>(_ s: S) where S.Ctx == Ctx {
        self.serverInfo = s.serverInfo
        self.tools = s.tools
        self._routeToolCall = s.routeToolCall
    }

    public func routeToolCall(_ method: BaboonMethodId, _ data: String, _ ctx: Ctx, _ codecCtx: BaboonCodecContext) async throws -> String {
        return try await _routeToolCall(method, data, ctx, codecCtx)
    }
}

// --- Async cross-service MCP muxer (tasks:T113; contract §7) ---
//
// Async sibling of `AbstractMcpMuxer`, for backends generated with
// `--sw-async-services=true`: it composes `AnyAsyncRoutableMcpServer<Ctx>` members
// (whose `routeToolCall` is `async throws`), so the single `tools/call` dispatch
// hop is awaited directly in the caller's task (no synchronous bridge) and
// `handle` is itself `async`. The registration / union-table build (duplicateTool
// on collision), the merged `initialize`, the ordering rule, and the
// noMatchingTool wire mapping are identical to the sync muxer; only the
// `tools/call` hop awaits.
public final class AbstractAsyncMcpMuxer<Ctx> {
    private var _servers: [AnyAsyncRoutableMcpServer<Ctx>] = []
    private var _route: [String: AnyAsyncRoutableMcpServer<Ctx>] = [:]
    private var _entries: [String: McpToolEntry] = [:]
    private var _toolOrder: [String] = []
    private let _mergedServerInfo: McpServerInfo

    public init(_ mergedServerInfo: McpServerInfo, _ servers: AnyAsyncRoutableMcpServer<Ctx>...) throws {
        self._mergedServerInfo = mergedServerInfo
        for s in servers { try register(s) }
    }

    public init(_ mergedServerInfo: McpServerInfo, _ servers: [AnyAsyncRoutableMcpServer<Ctx>]) throws {
        self._mergedServerInfo = mergedServerInfo
        for s in servers { try register(s) }
    }

    public func register(_ server: AnyAsyncRoutableMcpServer<Ctx>) throws {
        for t in server.tools {
            if _route[t.name] != nil {
                throw BaboonMcpWiringException(BaboonMcpWiringError.duplicateTool(t.name))
            }
            _route[t.name] = server
            _entries[t.name] = t
            _toolOrder.append(t.name)
        }
        _servers.append(server)
    }

    private func errorResponse(_ id: Any?, _ code: Int, _ message: String) -> JsonRpcResponse {
        return JsonRpcResponse(id, error: JsonRpcError(code, message))
    }

    public func handle(_ request: JsonRpcRequest, _ session: McpSession, _ ctx: Ctx, _ codecCtx: BaboonCodecContext) async -> JsonRpcResponse? {
        let id = request.id
        switch request.method {
        case "initialize":
            guard let params = request.params as? [String: Any] else {
                return errorResponse(id, jsonRpcErrorInvalidParams, "initialize: missing params")
            }
            if params["protocolVersion"] == nil {
                return errorResponse(id, jsonRpcErrorInvalidParams, "initialize: missing protocolVersion")
            }
            session.initialized = true
            let result: [String: Any] = [
                "protocolVersion": mcpProtocolVersion,
                "capabilities": ["tools": [String: Any]()],
                "serverInfo": ["name": _mergedServerInfo.name, "version": _mergedServerInfo.version],
            ]
            return JsonRpcResponse(id, result: result)

        case "notifications/initialized":
            return nil

        case "tools/list":
            if !session.initialized {
                return errorResponse(id, jsonRpcErrorInvalidRequest, "tools/list before initialize")
            }
            return JsonRpcResponse(id, result: ["tools": toolsListUnion()])

        case "tools/call":
            if !session.initialized {
                return errorResponse(id, jsonRpcErrorInvalidRequest, "tools/call before initialize")
            }
            guard let params = request.params as? [String: Any] else {
                return errorResponse(id, jsonRpcErrorInvalidParams, "tools/call: missing params")
            }
            guard let toolName = params["name"] as? String else {
                return errorResponse(id, jsonRpcErrorInvalidParams, "tools/call: missing tool name")
            }
            guard let server = _route[toolName], let entry = _entries[toolName] else {
                return errorResponse(id, jsonRpcErrorInvalidParams, "tools/call: unknown tool '\(toolName)'")
            }
            let argsRaw: Any = params["arguments"] ?? [String: Any]()
            let argsJson: String
            do {
                let data = try JSONSerialization.data(withJSONObject: argsRaw, options: [.sortedKeys, .fragmentsAllowed])
                argsJson = String(data: data, encoding: .utf8)!
            } catch {
                return errorResponse(id, jsonRpcErrorInternalError, "tools/call: failed to serialize arguments: \(error)")
            }
            do {
                let resultStr = try await server.routeToolCall(entry.method, argsJson, ctx, codecCtx)
                let result: [String: Any] = [
                    "content": [["type": "text", "text": resultStr]],
                    "isError": false,
                ]
                return JsonRpcResponse(id, result: result)
            } catch let e as BaboonWiringException {
                let result: [String: Any] = [
                    "content": [["type": "text", "text": describeWiringError(e.error)]],
                    "isError": true,
                ]
                return JsonRpcResponse(id, result: result)
            } catch {
                let result: [String: Any] = [
                    "content": [["type": "text", "text": "\(error)"]],
                    "isError": true,
                ]
                return JsonRpcResponse(id, result: result)
            }

        default:
            return errorResponse(id, jsonRpcErrorMethodNotFound, "Method not found: \(request.method)")
        }
    }

    private func toolsListUnion() -> [[String: Any]] {
        var toolsArr: [[String: Any]] = []
        for name in _toolOrder {
            guard let t = _entries[name] else { continue }
            var entry: [String: Any] = [
                "name": t.name,
                "inputSchema": t.inputSchema,
            ]
            if let d = t.description {
                entry["description"] = d
            }
            toolsArr.append(entry)
        }
        return toolsArr
    }

    private func describeWiringError(_ e: BaboonWiringError) -> String {
        return "\(e)"
    }
}
