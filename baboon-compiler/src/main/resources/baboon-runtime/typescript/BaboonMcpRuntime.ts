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
export abstract class AbstractBaboonMcpServer<Ctx> implements IBaboonMcpServer<Ctx> {
    protected abstract readonly serverInfo: McpServerInfo;
    protected abstract readonly tools: readonly McpToolEntry[];
    protected abstract invokeJson(method: BaboonMethodId, data: string, ctx: Ctx, codecCtx: BaboonCodecContext): BaboonEitherResult;

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
