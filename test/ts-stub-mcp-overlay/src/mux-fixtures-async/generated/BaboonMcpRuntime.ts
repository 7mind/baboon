// Additive MCP server runtime (decisions ledger M1; contract:
// docs/research/mcp-dispatch-runtime-contract.md). Emitted only when
// `--ts-generate-mcp-server=true`; the service-wiring runtime in
// BaboonSharedRuntime.ts is unchanged. These types are STATIC (no per-model
// templating) — the only per-model code is the generated `<Service>McpServer`
// and its tool-registry literals.
//
// The MCP dispatch surface is transport-abstract: it implements the JSON-RPC
// method state machine + tool dispatch ONLY. Bytes-in/bytes-out (stdio framing,
// Streamable-HTTP bodies) is an injected adapter the generated surface never
// contains — mirroring the abstract-context service contract, which supplies
// `Ctx` per invocation rather than baking an I/O loop into the wrapper.

import type { BaboonCodecContext, BaboonMethodId, BaboonWiringError } from "./BaboonSharedRuntime";

// --- MCP-muxer error taxonomy (tasks:T104; contract §6) ---
//
// The cross-service MCP muxer composes several <Service>McpServer instances
// behind one endpoint. `DuplicateTool` is the MCP-tier analogue of the
// service-wiring `DuplicateService` (a registration-time tool-name collision);
// `NoMatchingTool` is the analogue of `NoMatchingService` (a dispatch-time
// unknown tool name). They mirror that taxonomy and carrier exactly, but live in
// this MCP-only runtime file rather than the always-shipped service-wiring
// runtime (BaboonSharedRuntime.ts) so that with `--ts-generate-mcp-server`
// absent the generated output is byte-identical to the pre-MCP baseline (no MCP
// types leak into the unconditional runtime). `BaboonMcpWiringException` is the
// MCP-tier carrier — the structural twin of the service-wiring
// `BaboonWiringException` (a thrown programmer error carrying a tagged value),
// so `DuplicateTool` propagates to the integrator exactly as `DuplicateService`
// does, just from the MCP-only runtime.
export type BaboonMcpWiringError =
    | { readonly tag: 'DuplicateTool'; readonly toolName: string }
    | { readonly tag: 'NoMatchingTool'; readonly toolName: string };

export class BaboonMcpWiringException extends Error {
    readonly error: BaboonMcpWiringError;
    constructor(error: BaboonMcpWiringError) {
        super(JSON.stringify(error));
        this.error = error;
    }
}

// --- JSON-RPC value types (already parsed from bytes by the adapter) ---
//
// `JsonRpcId` is `string | number` per JSON-RPC 2.0. A notification carries no
// `id`; the server produces no response for it. `params`/`result`/`error.data`
// are the language's existing JSON value type (TS: `unknown`) — the MCP runtime
// reuses what the JSON codecs already speak rather than introducing a new model.

export type JsonRpcId = string | number;

export interface JsonRpcRequest {
    readonly id?: JsonRpcId;
    readonly method: string;
    readonly params?: unknown;
}

export interface JsonRpcError {
    readonly code: number;
    readonly message: string;
    readonly data?: unknown;
}

export interface JsonRpcResponse {
    readonly id: JsonRpcId | null;
    readonly result?: unknown;
    readonly error?: JsonRpcError;
}

// JSON-RPC / MCP protocol constants (wire contract K4).
export const McpProtocolVersion = '2025-06-18';

export const JsonRpcErrorCodes = {
    ParseError: -32700,
    InvalidRequest: -32600,
    MethodNotFound: -32601,
    InvalidParams: -32602,
    InternalError: -32603,
} as const;

// --- Per-connection state (adapter-owned) ---
//
// The "initialized" precondition (reject `tools/*` before a successful
// `initialize`) is per-connection state; a connection is a transport concept.
// The latch therefore lives in this tiny value the adapter creates per
// connection, NOT as ambient mutable state inside the server object (which stays
// immutable and shareable across concurrent connections).
export class McpSession {
    initialized: boolean = false;
}

// --- Tool registry ---
//
// One entry per Baboon method bound to a server. `inputSchema` is the
// precomputed, self-contained JSON Schema (from the shared T5 emitter), carried
// as a constant value — the runtime does not compute schemas.
export interface McpToolEntry {
    readonly name: string;
    readonly method: BaboonMethodId;
    readonly description?: string;
    readonly inputSchema: unknown;
}

export interface McpServerInfo {
    readonly name: string;
    readonly version: string;
}

// --- Dispatch interface ---
//
// The single generated entrypoint, analogous to `IBaboonJsonServiceCtx<Ctx,R>`.
// It is NOT R-parametric (an MCP response is always a `JsonRpcResponse` value);
// the only free type parameter is the caller's `Ctx` — the SAME `Ctx` the
// service-wiring contract threads. `handle` is synchronous and performs no I/O.
// It returns `undefined` for an accepted notification (no reply).
export interface IBaboonMcpServer<Ctx> {
    handle(request: JsonRpcRequest, session: McpSession, ctx: Ctx, codecCtx: BaboonCodecContext): JsonRpcResponse | undefined;
}

// --- PUBLIC routable-server surface (tasks:T114) ---
//
// The composition seam the cross-service MCP muxer (AbstractMcpMuxer) depends
// on. A sibling muxer needs to (1) read each server's identity, (2) read its
// declaration-ordered tool registry to build the union tools/list and the
// tool-name -> owner table, and (3) route a single tools/call into the owning
// server reusing its existing Channel-A/Channel-B mapping unchanged. Those
// inputs were `protected`/`private` on the base, so a sibling could not compose
// them portably; this interface promotes exactly them to a stable PUBLIC
// surface. The muxer depends on the interface, not on concrete
// `<Service>McpServer` subclasses, and NEVER calls a server's `handle()`.
//
// `routeToolCall` is the public name for the per-service routable dispatch
// entry: the SAME code path `handle()` drives for its own `tools/call` arm
// (`invokeJson`), exposed so the muxer reuses Channel-A/Channel-B verbatim.
// The sync variant returns `BaboonEitherResult`; the async variant returns a
// `Promise<BaboonEitherResult>` (see `IBaboonRoutableAsyncMcpServer`).
export interface IBaboonRoutableMcpServer<Ctx> {
    readonly serverInfo: McpServerInfo;
    readonly tools: readonly McpToolEntry[];
    routeToolCall(method: BaboonMethodId, data: string, ctx: Ctx, codecCtx: BaboonCodecContext): BaboonEitherResult;
}

// Async sibling of `IBaboonRoutableMcpServer`, for backends generated with
// `--ts-async-services=true`: `routeToolCall` returns a `Promise`.
export interface IBaboonRoutableAsyncMcpServer<Ctx> {
    readonly serverInfo: McpServerInfo;
    readonly tools: readonly McpToolEntry[];
    routeToolCall(method: BaboonMethodId, data: string, ctx: Ctx, codecCtx: BaboonCodecContext): Promise<BaboonEitherResult>;
}

// The JSON `tools/call` delegate the generated server supplies: it routes one
// tool invocation into the already-generated service dispatch (the errors-mode
// `dispatchJson`, which returns the service-result container). The MCP layer
// turns that `BaboonEither<BaboonWiringError, string>` into Channel-A / Channel-B
// per the wire contract (K4 §3). The codecs are reached exclusively through this
// delegate; the MCP runtime holds no codec logic itself.
export type McpJsonInvoke<Ctx> = (
    method: BaboonMethodId,
    data: string,
    ctx: Ctx,
    codecCtx: BaboonCodecContext,
) => BaboonEitherResult;

export type BaboonEitherResult =
    | { readonly tag: 'Left'; readonly value: BaboonWiringError }
    | { readonly tag: 'Right'; readonly value: string };

// --- Transport-abstract dispatch base ---
//
// Shared `handle` state machine. The generated `<Service>McpServer` extends this
// with its fixed `serverInfo`, ordered tool registry, and `invokeJson` delegate.
// All JSON-RPC method strings ("tools/list" …) and result keys ("protocolVersion",
// "inputSchema" …) are literal lowercase strings, NOT subject to any per-language
// symbol casing.
export abstract class AbstractBaboonMcpServer<Ctx> implements IBaboonMcpServer<Ctx>, IBaboonRoutableMcpServer<Ctx> {
    // PUBLIC routable-server surface (tasks:T114): the muxer reads serverInfo /
    // tools and routes via routeToolCall, never via the (private) byName() and
    // never via handle().
    public abstract readonly serverInfo: McpServerInfo;
    public abstract readonly tools: readonly McpToolEntry[];
    protected abstract invokeJson(method: BaboonMethodId, data: string, ctx: Ctx, codecCtx: BaboonCodecContext): BaboonEitherResult;

    // PUBLIC dispatch entry (tasks:T114): the same path handle() drives for its
    // own tools/call arm, exposed for the muxer to reuse Channel-A/B unchanged.
    routeToolCall(method: BaboonMethodId, data: string, ctx: Ctx, codecCtx: BaboonCodecContext): BaboonEitherResult {
        return this.invokeJson(method, data, ctx, codecCtx);
    }

    private byName(): Map<string, McpToolEntry> {
        const m = new Map<string, McpToolEntry>();
        for (const t of this.tools) m.set(t.name, t);
        return m;
    }

    handle(request: JsonRpcRequest, session: McpSession, ctx: Ctx, codecCtx: BaboonCodecContext): JsonRpcResponse | undefined {
        const id: JsonRpcId | null = request.id ?? null;
        switch (request.method) {
            case 'initialize': {
                const params = request.params as { protocolVersion?: unknown } | undefined;
                if (params === undefined || params === null || params.protocolVersion === undefined) {
                    return this.errorResponse(id, JsonRpcErrorCodes.InvalidParams, 'initialize: missing protocolVersion');
                }
                session.initialized = true;
                return {
                    id,
                    result: {
                        protocolVersion: McpProtocolVersion,
                        capabilities: { tools: {} },
                        serverInfo: { name: this.serverInfo.name, version: this.serverInfo.version },
                    },
                };
            }
            case 'notifications/initialized':
                return undefined;
            case 'tools/list': {
                if (!session.initialized) {
                    return this.errorResponse(id, JsonRpcErrorCodes.InvalidRequest, 'tools/list before initialize');
                }
                const tools = this.tools.map(t => {
                    const entry: { name: string; inputSchema: unknown; description?: string } = { name: t.name, inputSchema: t.inputSchema };
                    if (t.description !== undefined) entry.description = t.description;
                    return entry;
                });
                return { id, result: { tools } };
            }
            case 'tools/call': {
                if (!session.initialized) {
                    return this.errorResponse(id, JsonRpcErrorCodes.InvalidRequest, 'tools/call before initialize');
                }
                const params = request.params as { name?: unknown; arguments?: unknown } | undefined;
                const name = params?.name;
                if (typeof name !== 'string') {
                    return this.errorResponse(id, JsonRpcErrorCodes.InvalidParams, 'tools/call: missing tool name');
                }
                const entry = this.byName().get(name);
                if (entry === undefined) {
                    return this.errorResponse(id, JsonRpcErrorCodes.InvalidParams, `tools/call: unknown tool '${name}'`);
                }
                const argsJson = JSON.stringify(params?.arguments ?? {});
                const result = this.invokeJson(entry.method, argsJson, ctx, codecCtx);
                if (result.tag === 'Right') {
                    return { id, result: { content: [{ type: 'text', text: result.value }], isError: false } };
                } else {
                    // Channel B: a valid protocol call whose domain payload failed.
                    return { id, result: { content: [{ type: 'text', text: this.describeWiringError(result.value) }], isError: true } };
                }
            }
            default:
                return this.errorResponse(id, JsonRpcErrorCodes.MethodNotFound, `Method not found: ${request.method}`);
        }
    }

    protected errorResponse(id: JsonRpcId | null, code: number, message: string): JsonRpcResponse {
        return { id, error: { code, message } };
    }

    protected describeWiringError(e: BaboonWiringError): string {
        return JSON.stringify(e);
    }
}

// --- Cross-service MCP muxer (tasks:T104; contract:
// docs/research/mcp-muxer-runtime-contract.md) ---
//
// `AbstractMcpMuxer<Ctx>` composes several `<Service>McpServer<Ctx>` instances
// behind ONE MCP endpoint so a single connection serves the union of their
// tools. It is the MCP-tier sibling of `JsonMuxer`: where `JsonMuxer` keys
// `Map<serviceName, IBaboonJsonService>` and routes by `method.serviceName`, the
// muxer keys `Map<toolName, owningServer>` and routes by the inbound flat MCP
// tool name (contract §1).
//
// Composition seam: it depends ONLY on the PUBLIC `IBaboonRoutableMcpServer<Ctx>`
// surface (tasks:T114) — it reads each server's `serverInfo` and `tools`, and
// routes each `tools/call` via the public `routeToolCall`. It NEVER reads
// protected members and NEVER calls a server's `handle()` (a member's `handle`
// resolves only its OWN tools and returns "unknown tool" for any cross-service
// name — contract §4). The muxer owns the JSON-RPC envelope; each member owns
// its domain dispatch.
//
// The `handle` state machine is the SAME shape as the per-service base, with
// three arms differing only in operating over the union: `tools/list` returns
// the union, `tools/call` routes by tool name to the owning server, and
// `initialize` returns a single merged `serverInfo` supplied to the ctor.
export class AbstractMcpMuxer<Ctx> implements IBaboonMcpServer<Ctx> {
    // Registration order preserved (insertion-ordered Map / array — JsonMuxer
    // LinkedHashMap precedent). `route`/`entries` are built at registration
    // (contract §2), never per request.
    private readonly servers: IBaboonRoutableMcpServer<Ctx>[] = [];
    private readonly route = new Map<string, IBaboonRoutableMcpServer<Ctx>>();
    private readonly entries = new Map<string, McpToolEntry>();
    private readonly mergedServerInfo: McpServerInfo;

    // varargs ctor mirrors `JsonMuxer(...services)`. `mergedServerInfo` is the
    // composed endpoint's single identity returned by `initialize` (§3.1).
    constructor(mergedServerInfo: McpServerInfo, ...servers: IBaboonRoutableMcpServer<Ctx>[]) {
        this.mergedServerInfo = mergedServerInfo;
        for (const s of servers) this.register(s);
    }

    // Folds the server's declaration-ordered `tools()` into the union table;
    // throws DuplicateTool on a tool-name collision across servers (the exact
    // MCP-tier analogue of JsonMuxer.register throwing DuplicateService).
    register(server: IBaboonRoutableMcpServer<Ctx>): void {
        for (const t of server.tools) {
            if (this.route.has(t.name)) {
                throw new BaboonMcpWiringException({ tag: 'DuplicateTool', toolName: t.name });
            }
            this.route.set(t.name, server);
            this.entries.set(t.name, t);
        }
        this.servers.push(server);
    }

    handle(request: JsonRpcRequest, session: McpSession, ctx: Ctx, codecCtx: BaboonCodecContext): JsonRpcResponse | undefined {
        const id: JsonRpcId | null = request.id ?? null;
        switch (request.method) {
            case 'initialize': {
                const params = request.params as { protocolVersion?: unknown } | undefined;
                if (params === undefined || params === null || params.protocolVersion === undefined) {
                    return this.errorResponse(id, JsonRpcErrorCodes.InvalidParams, 'initialize: missing protocolVersion');
                }
                session.initialized = true;
                return {
                    id,
                    result: {
                        protocolVersion: McpProtocolVersion,
                        capabilities: { tools: {} },
                        serverInfo: { name: this.mergedServerInfo.name, version: this.mergedServerInfo.version },
                    },
                };
            }
            case 'notifications/initialized':
                return undefined;
            case 'tools/list': {
                if (!session.initialized) {
                    return this.errorResponse(id, JsonRpcErrorCodes.InvalidRequest, 'tools/list before initialize');
                }
                return { id, result: { tools: this.toolsListUnion() } };
            }
            case 'tools/call': {
                if (!session.initialized) {
                    return this.errorResponse(id, JsonRpcErrorCodes.InvalidRequest, 'tools/call before initialize');
                }
                const params = request.params as { name?: unknown; arguments?: unknown } | undefined;
                const name = params?.name;
                if (typeof name !== 'string') {
                    return this.errorResponse(id, JsonRpcErrorCodes.InvalidParams, 'tools/call: missing tool name');
                }
                const server = this.route.get(name);
                if (server === undefined) {
                    // NoMatchingTool: surfaced as the SAME wire response the per-service
                    // base uses for an unknown tool (-32602, "unknown tool '<name>'"),
                    // so the bytes are identical whether one server or the muxer rejects.
                    return this.errorResponse(id, JsonRpcErrorCodes.InvalidParams, `tools/call: unknown tool '${name}'`);
                }
                const entry = this.entries.get(name)!;
                const argsJson = JSON.stringify(params?.arguments ?? {});
                const result = server.routeToolCall(entry.method, argsJson, ctx, codecCtx);
                if (result.tag === 'Right') {
                    return { id, result: { content: [{ type: 'text', text: result.value }], isError: false } };
                } else {
                    // Channel B: a valid protocol call whose domain payload failed.
                    return { id, result: { content: [{ type: 'text', text: this.describeWiringError(result.value) }], isError: true } };
                }
            }
            default:
                return this.errorResponse(id, JsonRpcErrorCodes.MethodNotFound, `Method not found: ${request.method}`);
        }
    }

    // Backs tools/list (§3.2): the union of all registered servers' tool entries
    // in registration-then-declaration order (the insertion order of `entries`),
    // each in the same shape the per-service base emits.
    private toolsListUnion(): Array<{ name: string; inputSchema: unknown; description?: string }> {
        const out: Array<{ name: string; inputSchema: unknown; description?: string }> = [];
        for (const t of this.entries.values()) {
            const entry: { name: string; inputSchema: unknown; description?: string } = { name: t.name, inputSchema: t.inputSchema };
            if (t.description !== undefined) entry.description = t.description;
            out.push(entry);
        }
        return out;
    }

    protected errorResponse(id: JsonRpcId | null, code: number, message: string): JsonRpcResponse {
        return { id, error: { code, message } };
    }

    protected describeWiringError(e: BaboonWiringError): string {
        return JSON.stringify(e);
    }
}

// --- Async dispatch interface ---
//
// Async sibling of `IBaboonMcpServer`, selected when the backend is generated
// with `--ts-async-services=true`. The errors-mode wiring entry
// `invokeJson_<svc>` is then `async` and returns
// `Promise<BaboonEither<BaboonWiringError, string>>`, so the `tools/call`
// dispatch must `await` the delegate; `handle` is therefore itself `async` and
// returns a `Promise`. The integrator awaits the returned promise.
export interface IBaboonAsyncMcpServer<Ctx> {
    handle(request: JsonRpcRequest, session: McpSession, ctx: Ctx, codecCtx: BaboonCodecContext): Promise<JsonRpcResponse | undefined>;
}

// The async JSON `tools/call` delegate the generated async server supplies: it
// routes one tool invocation into the async errors-mode `dispatchJson`, which
// returns `Promise<BaboonEither<…>>`. Async sibling of `McpJsonInvoke`.
export type McpJsonInvokeAsync<Ctx> = (
    method: BaboonMethodId,
    data: string,
    ctx: Ctx,
    codecCtx: BaboonCodecContext,
) => Promise<BaboonEitherResult>;

// --- Async transport-abstract dispatch base ---
//
// Async sibling of `AbstractBaboonMcpServer`. Selected when the backend is
// generated with `--ts-async-services=true`: the `tools/call` dispatch awaits
// the async `invokeJson` (whose result is the async wiring's
// `Promise<BaboonEither<…>>`), so `handle` itself is `async`. The synchronous
// method state machine (initialize / tools/list, the error mapping, the wire
// constants) is identical to the sync base; only the single `tools/call` hop
// awaits.
export abstract class AbstractAsyncBaboonMcpServer<Ctx> implements IBaboonAsyncMcpServer<Ctx>, IBaboonRoutableAsyncMcpServer<Ctx> {
    // PUBLIC routable-server surface (tasks:T114), async flavour: routeToolCall
    // returns a Promise; the muxer awaits it before applying Channel-A/B.
    public abstract readonly serverInfo: McpServerInfo;
    public abstract readonly tools: readonly McpToolEntry[];
    protected abstract invokeJson(method: BaboonMethodId, data: string, ctx: Ctx, codecCtx: BaboonCodecContext): Promise<BaboonEitherResult>;

    routeToolCall(method: BaboonMethodId, data: string, ctx: Ctx, codecCtx: BaboonCodecContext): Promise<BaboonEitherResult> {
        return this.invokeJson(method, data, ctx, codecCtx);
    }

    private byName(): Map<string, McpToolEntry> {
        const m = new Map<string, McpToolEntry>();
        for (const t of this.tools) m.set(t.name, t);
        return m;
    }

    async handle(request: JsonRpcRequest, session: McpSession, ctx: Ctx, codecCtx: BaboonCodecContext): Promise<JsonRpcResponse | undefined> {
        const id: JsonRpcId | null = request.id ?? null;
        switch (request.method) {
            case 'initialize': {
                const params = request.params as { protocolVersion?: unknown } | undefined;
                if (params === undefined || params === null || params.protocolVersion === undefined) {
                    return this.errorResponse(id, JsonRpcErrorCodes.InvalidParams, 'initialize: missing protocolVersion');
                }
                session.initialized = true;
                return {
                    id,
                    result: {
                        protocolVersion: McpProtocolVersion,
                        capabilities: { tools: {} },
                        serverInfo: { name: this.serverInfo.name, version: this.serverInfo.version },
                    },
                };
            }
            case 'notifications/initialized':
                return undefined;
            case 'tools/list': {
                if (!session.initialized) {
                    return this.errorResponse(id, JsonRpcErrorCodes.InvalidRequest, 'tools/list before initialize');
                }
                const tools = this.tools.map(t => {
                    const entry: { name: string; inputSchema: unknown; description?: string } = { name: t.name, inputSchema: t.inputSchema };
                    if (t.description !== undefined) entry.description = t.description;
                    return entry;
                });
                return { id, result: { tools } };
            }
            case 'tools/call': {
                if (!session.initialized) {
                    return this.errorResponse(id, JsonRpcErrorCodes.InvalidRequest, 'tools/call before initialize');
                }
                const params = request.params as { name?: unknown; arguments?: unknown } | undefined;
                const name = params?.name;
                if (typeof name !== 'string') {
                    return this.errorResponse(id, JsonRpcErrorCodes.InvalidParams, 'tools/call: missing tool name');
                }
                const entry = this.byName().get(name);
                if (entry === undefined) {
                    return this.errorResponse(id, JsonRpcErrorCodes.InvalidParams, `tools/call: unknown tool '${name}'`);
                }
                const argsJson = JSON.stringify(params?.arguments ?? {});
                const result = await this.invokeJson(entry.method, argsJson, ctx, codecCtx);
                if (result.tag === 'Right') {
                    return { id, result: { content: [{ type: 'text', text: result.value }], isError: false } };
                } else {
                    // Channel B: a valid protocol call whose domain payload failed.
                    return { id, result: { content: [{ type: 'text', text: this.describeWiringError(result.value) }], isError: true } };
                }
            }
            default:
                return this.errorResponse(id, JsonRpcErrorCodes.MethodNotFound, `Method not found: ${request.method}`);
        }
    }

    protected errorResponse(id: JsonRpcId | null, code: number, message: string): JsonRpcResponse {
        return { id, error: { code, message } };
    }

    protected describeWiringError(e: BaboonWiringError): string {
        return JSON.stringify(e);
    }
}

// --- Async cross-service MCP muxer (tasks:T104; contract §7) ---
//
// Async sibling of `AbstractMcpMuxer`, for backends generated with
// `--ts-async-services=true`: it composes `IBaboonRoutableAsyncMcpServer<Ctx>`
// members (whose `routeToolCall` returns a `Promise<BaboonEitherResult>`), so the
// single `tools/call` dispatch hop is awaited and `handle` is itself `async`. The
// registration / union-table build (`DuplicateTool` on collision), the merged
// `initialize`, the ordering rule, and the `NoMatchingTool` wire mapping are
// identical to the sync muxer; only the `tools/call` hop awaits.
export class AbstractAsyncMcpMuxer<Ctx> implements IBaboonAsyncMcpServer<Ctx> {
    private readonly servers: IBaboonRoutableAsyncMcpServer<Ctx>[] = [];
    private readonly route = new Map<string, IBaboonRoutableAsyncMcpServer<Ctx>>();
    private readonly entries = new Map<string, McpToolEntry>();
    private readonly mergedServerInfo: McpServerInfo;

    constructor(mergedServerInfo: McpServerInfo, ...servers: IBaboonRoutableAsyncMcpServer<Ctx>[]) {
        this.mergedServerInfo = mergedServerInfo;
        for (const s of servers) this.register(s);
    }

    register(server: IBaboonRoutableAsyncMcpServer<Ctx>): void {
        for (const t of server.tools) {
            if (this.route.has(t.name)) {
                throw new BaboonMcpWiringException({ tag: 'DuplicateTool', toolName: t.name });
            }
            this.route.set(t.name, server);
            this.entries.set(t.name, t);
        }
        this.servers.push(server);
    }

    async handle(request: JsonRpcRequest, session: McpSession, ctx: Ctx, codecCtx: BaboonCodecContext): Promise<JsonRpcResponse | undefined> {
        const id: JsonRpcId | null = request.id ?? null;
        switch (request.method) {
            case 'initialize': {
                const params = request.params as { protocolVersion?: unknown } | undefined;
                if (params === undefined || params === null || params.protocolVersion === undefined) {
                    return this.errorResponse(id, JsonRpcErrorCodes.InvalidParams, 'initialize: missing protocolVersion');
                }
                session.initialized = true;
                return {
                    id,
                    result: {
                        protocolVersion: McpProtocolVersion,
                        capabilities: { tools: {} },
                        serverInfo: { name: this.mergedServerInfo.name, version: this.mergedServerInfo.version },
                    },
                };
            }
            case 'notifications/initialized':
                return undefined;
            case 'tools/list': {
                if (!session.initialized) {
                    return this.errorResponse(id, JsonRpcErrorCodes.InvalidRequest, 'tools/list before initialize');
                }
                return { id, result: { tools: this.toolsListUnion() } };
            }
            case 'tools/call': {
                if (!session.initialized) {
                    return this.errorResponse(id, JsonRpcErrorCodes.InvalidRequest, 'tools/call before initialize');
                }
                const params = request.params as { name?: unknown; arguments?: unknown } | undefined;
                const name = params?.name;
                if (typeof name !== 'string') {
                    return this.errorResponse(id, JsonRpcErrorCodes.InvalidParams, 'tools/call: missing tool name');
                }
                const server = this.route.get(name);
                if (server === undefined) {
                    return this.errorResponse(id, JsonRpcErrorCodes.InvalidParams, `tools/call: unknown tool '${name}'`);
                }
                const entry = this.entries.get(name)!;
                const argsJson = JSON.stringify(params?.arguments ?? {});
                const result = await server.routeToolCall(entry.method, argsJson, ctx, codecCtx);
                if (result.tag === 'Right') {
                    return { id, result: { content: [{ type: 'text', text: result.value }], isError: false } };
                } else {
                    return { id, result: { content: [{ type: 'text', text: this.describeWiringError(result.value) }], isError: true } };
                }
            }
            default:
                return this.errorResponse(id, JsonRpcErrorCodes.MethodNotFound, `Method not found: ${request.method}`);
        }
    }

    private toolsListUnion(): Array<{ name: string; inputSchema: unknown; description?: string }> {
        const out: Array<{ name: string; inputSchema: unknown; description?: string }> = [];
        for (const t of this.entries.values()) {
            const entry: { name: string; inputSchema: unknown; description?: string } = { name: t.name, inputSchema: t.inputSchema };
            if (t.description !== undefined) entry.description = t.description;
            out.push(entry);
        }
        return out;
    }

    protected errorResponse(id: JsonRpcId | null, code: number, message: string): JsonRpcResponse {
        return { id, error: { code, message } };
    }

    protected describeWiringError(e: BaboonWiringError): string {
        return JSON.stringify(e);
    }
}