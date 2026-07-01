// T178 / D40 — TypeScript zero-service MCP overlay test (RED baseline).
//
// Purpose: prove the ZERO-SERVICE MCP runtime contract. The zero-service model
// (mcp-stub-zero-services-ok) declares @root types but NO RPC block, so
// servicesOf(domain) is empty and NO per-service `<Service>McpServer` module is
// generated. With zero services the ONLY source of the MCP runtime symbols
// (AbstractMcpMuxer / IBaboonRoutableMcpServer / McpServerInfo / McpSession /
// JsonRpcRequest / JsonRpcResponse / JsonRpcErrorCodes) is the STATIC runtime
// module `BaboonMcpRuntime.ts`.
//
// RED (pre-fix): the current generator emits NO `BaboonMcpRuntime.ts` for a
// zero-service model, so the import below resolves to a non-existent module and
// vitest/tsc FAILS with "Cannot find module './baboondefinitions/generated/
// BaboonMcpRuntime'". That failure IS the D40 reproduction.
//
// GREEN (post-fix, later task): once the generator emits the static MCP runtime
// even when servicesOf(domain) is empty, this file resolves/compiles and the
// runtime assertions below pass — an empty muxer lists zero tools and rejects
// any tools/call with JSON-RPC -32602.
//
// Assertion discipline: explicit `throw` on failure — unconditional.

import { describe, test } from "vitest";
import { BaboonCodecContext } from "./baboondefinitions/generated/BaboonSharedRuntime";
import {
    JsonRpcRequest,
    JsonRpcResponse,
    McpSession,
    McpServerInfo,
    JsonRpcErrorCodes,
    AbstractMcpMuxer,
} from "./baboondefinitions/generated/BaboonMcpRuntime";

function require_(cond: boolean, message: string): void {
    if (!cond) {
        throw new Error(message);
    }
}

const codecCtx = BaboonCodecContext.Default;
const MERGED_INFO: McpServerInfo = { name: "ZeroEndpoint", version: "1.0.0" };

// COMPILE-TIME contract: constructing AbstractMcpMuxer<Ctx> with ZERO registered
// servers requires the static runtime module to exist. With zero services there
// is no generated <Service>McpServer to import — these symbols resolve ONLY from
// BaboonMcpRuntime.ts.
function makeEmptyMuxer(): AbstractMcpMuxer<null> {
    return new AbstractMcpMuxer<null>(MERGED_INFO);
}

function initedSession(mux: AbstractMcpMuxer<null>): McpSession {
    const session = new McpSession();
    mux.handle(
        { id: 0, method: "initialize", params: { protocolVersion: "2025-06-18", capabilities: {}, clientInfo: { name: "t", version: "0" } } },
        session, null, codecCtx,
    );
    mux.handle({ method: "notifications/initialized" }, session, null, codecCtx);
    return session;
}

describe("MCP zero-service (T178/D40): empty AbstractMcpMuxer", () => {
    // RUNTIME contract (a): tools/list on an empty muxer returns an empty array.
    test("tools/list is empty", () => {
        const mux = makeEmptyMuxer();
        const session = initedSession(mux);
        const resp: JsonRpcResponse | undefined = mux.handle(
            { id: 1, method: "tools/list" }, session, null, codecCtx);

        require_(resp !== undefined, "tools/list must return a response");
        require_(resp!.error === undefined, "tools/list must not return an error");
        const result = resp!.result as { tools: Array<unknown> };
        require_(Array.isArray(result.tools), "result must carry a tools array");
        require_(result.tools.length === 0, "empty muxer MUST list zero tools");
    });

    // RUNTIME contract (b): an unknown tool call yields JSON-RPC -32602.
    test("unknown tool call yields -32602", () => {
        const mux = makeEmptyMuxer();
        const session = initedSession(mux);
        const req: JsonRpcRequest = { id: 2, method: "tools/call", params: { name: "anything_at_all", arguments: {} } };
        const resp = mux.handle(req, session, null, codecCtx);

        require_(resp !== undefined, "tools/call must return a response");
        require_(resp!.error !== undefined, "unknown tool on empty muxer MUST produce a Channel-A error");
        require_(resp!.result === undefined, "no result expected for unknown tool");
        require_(resp!.error!.code === JsonRpcErrorCodes.InvalidParams,
            "unknown-tool error code MUST be -32602 (InvalidParams)");
        require_(typeof resp!.error!.message === "string" && resp!.error!.message.length > 0,
            "error.message must be non-empty");
    });
});
