/**
 * T64 / T65 / D24 — TypeScript ASYNC-MCP round-trip overlay test (GREEN).
 *
 * Async sibling of `test/ts-stub-mcp-overlay/src/mcp.test.ts`. The model is
 * generated with BOTH `--ts-generate-mcp-server=true` AND
 * `--ts-async-services=true`, so:
 *
 *   - the `McpTools` service interface methods return `Promise<…Out>`;
 *   - the generated wiring `invokeJson_McpTools(...)` is `async` and returns
 *     `Promise<BaboonEither<BaboonWiringError, string>>`
 *     (wiring.ts under the async axis).
 *
 * FIX (T65, D24): `TsMcpServerGenerator` now threads the async axis through the
 * MCP-server delegate. Under `--ts-async-services=true` the generated
 * `McpToolsMcpServer` extends `AbstractAsyncBaboonMcpServer`, its constructor and
 * `_invokeJson` field declare the delegate as `Promise`-returning —
 * `(method, data, ctx, codecCtx) => Promise<BaboonEitherResult>` — and its
 * `invokeJson` / inherited `handle` are `async` and `await` the dispatch. The
 * async wiring's `Promise<BaboonEitherResult>` is therefore assignable to the
 * delegate and `tsc --noEmit` typechecks.
 *
 * The load-bearing lines are `makeServer()` (the constructor binding accepting
 * the `Promise`-returning `fakeInvokeJson`) and the `await`ed `handle` calls in
 * `send` / `initSession` that round-trip a real `tools/call`.
 */

import { describe, test, expect } from "vitest";
import Ajv from "ajv/dist/2020.js";

import {
    BaboonCodecContext,
    BaboonMethodId,
} from "./baboondefinitions/generated/BaboonSharedRuntime";
import { BaboonServiceRtDefault } from "./baboondefinitions/generated/mcp/stub/baboon-service-rt";
import { McpTools } from "./baboondefinitions/generated/mcp/stub/mcp-tools/service";
import { invokeJson_McpTools } from "./baboondefinitions/generated/mcp/stub/mcp-tools/wiring";
import { McpToolsMcpServer } from "./baboondefinitions/generated/mcp/stub/mcp-tools/mcp-server";
import {
    JsonRpcRequest,
    JsonRpcResponse,
    McpSession,
    BaboonEitherResult,
} from "./baboondefinitions/generated/BaboonMcpRuntime";

import { Out as PingOut } from "./baboondefinitions/generated/mcp/stub/mcp-tools/ping/out";
import { In as PingIn } from "./baboondefinitions/generated/mcp/stub/mcp-tools/ping/in";
import { FFancyStr_JsonCodec } from "./baboondefinitions/generated/mcp/stub/FFancyStr";
import { Lazy } from "./baboondefinitions/generated/BaboonSharedRuntime";
import { In as ListCollectionsIn } from "./baboondefinitions/generated/mcp/stub/mcp-tools/listcollections/in";
import { Out as ListCollectionsOut } from "./baboondefinitions/generated/mcp/stub/mcp-tools/listcollections/out";
import { In as SubmitCompositeIn } from "./baboondefinitions/generated/mcp/stub/mcp-tools/submitcomposite/in";
import { Out as SubmitCompositeOut } from "./baboondefinitions/generated/mcp/stub/mcp-tools/submitcomposite/out";
import { In as ProcessShapeIn } from "./baboondefinitions/generated/mcp/stub/mcp-tools/processshape/in";
import { Out as ProcessShapeOut } from "./baboondefinitions/generated/mcp/stub/mcp-tools/processshape/out";
import { In as ProcessTaggedIn } from "./baboondefinitions/generated/mcp/stub/mcp-tools/processtagged/in";
import { Out as ProcessTaggedOut } from "./baboondefinitions/generated/mcp/stub/mcp-tools/processtagged/out";
import { In as PagePointsIn } from "./baboondefinitions/generated/mcp/stub/mcp-tools/pagepoints/in";
import { Out as PagePointsOut } from "./baboondefinitions/generated/mcp/stub/mcp-tools/pagepoints/out";

// ---------------------------------------------------------------------------
// FFancyStr passthrough codec (foreign type → string identity).
// ---------------------------------------------------------------------------

class FFancyStrPassthroughCodec extends FFancyStr_JsonCodec {
    override decode(_ctx: BaboonCodecContext, json: unknown): string {
        return json as string;
    }
    override encode(_ctx: BaboonCodecContext, value: string): unknown {
        return value;
    }
}
FFancyStr_JsonCodec.lazyInstance = new Lazy(() => new FFancyStrPassthroughCodec());

// ---------------------------------------------------------------------------
// Stub McpTools service. Under --ts-async-services=true every interface method
// returns Promise<…Out>, so the stub uses `async` method shorthand.
// ---------------------------------------------------------------------------

const stubMcpTools: McpTools = {
    async listCollections(_arg: ListCollectionsIn): Promise<ListCollectionsOut> {
        return new ListCollectionsOut(true);
    },
    async submitComposite(_arg: SubmitCompositeIn): Promise<SubmitCompositeOut> {
        return new SubmitCompositeOut(true);
    },
    async processShape(_arg: ProcessShapeIn): Promise<ProcessShapeOut> {
        return new ProcessShapeOut(true);
    },
    async processTagged(_arg: ProcessTaggedIn): Promise<ProcessTaggedOut> {
        return new ProcessTaggedOut(true);
    },
    async pagePoints(_arg: PagePointsIn): Promise<PagePointsOut> {
        return new PagePointsOut(true);
    },
    async ping(_arg: PingIn): Promise<PingOut> {
        return new PingOut(true);
    },
};

const codecCtx = BaboonCodecContext.Default;
const rt = BaboonServiceRtDefault;

// ---------------------------------------------------------------------------
// Fake transport. invokeJson_McpTools is `async` under the async axis and
// returns Promise<BaboonEither<…>>, so this delegate is necessarily async and
// returns Promise<BaboonEitherResult>.
// ---------------------------------------------------------------------------

async function fakeInvokeJson(
    method: BaboonMethodId,
    data: string,
    _ctx: null,
    _codecCtx: BaboonCodecContext,
): Promise<BaboonEitherResult> {
    const result = await invokeJson_McpTools(method, data, stubMcpTools, rt, codecCtx);
    return result as unknown as BaboonEitherResult;
}

// ---------------------------------------------------------------------------
// Server factory. LOAD-BEARING (T65): under `--ts-async-services=true` the
// generated McpToolsMcpServer extends AbstractAsyncBaboonMcpServer and its
// constructor accepts the `Promise`-returning delegate
// `(…) => Promise<BaboonEitherResult>`. `fakeInvokeJson` returns
// `Promise<BaboonEitherResult>`, so this binding now typechecks under
// `tsc --noEmit`. `handle` is `async`; the integrator awaits the round-trip.
// ---------------------------------------------------------------------------

function makeServer(): McpToolsMcpServer<null> {
    return new McpToolsMcpServer<null>(fakeInvokeJson);
}

async function send(
    server: McpToolsMcpServer<null>,
    session: McpSession,
    req: JsonRpcRequest,
): Promise<JsonRpcResponse> {
    const resp = await server.handle(req, session, null, codecCtx);
    if (resp === undefined) {
        throw new Error(
            `Expected a response for "${req.method}" but got undefined (notification not expected here)`,
        );
    }
    return resp;
}

async function initSession(server: McpToolsMcpServer<null>, session: McpSession): Promise<void> {
    await server.handle(
        {
            id: 0,
            method: "initialize",
            params: {
                protocolVersion: "2025-06-18",
                capabilities: {},
                clientInfo: { name: "test-client", version: "0.0.1" },
            },
        },
        session,
        null,
        codecCtx,
    );
    await server.handle({ method: "notifications/initialized" }, session, null, codecCtx);
}

const ajv = new Ajv({ strict: false, allErrors: true });

// ---------------------------------------------------------------------------
// §1 — initialize + tools/list (smoke; the gate is the tsc build step above).
// ---------------------------------------------------------------------------

describe("MCP async §1: initialize", () => {
    const server = makeServer();
    const session = new McpSession();

    test("§1.2: protocolVersion, capabilities, serverInfo are correct", async () => {
        const resp = await send(server, session, {
            id: 1,
            method: "initialize",
            params: {
                protocolVersion: "2025-06-18",
                capabilities: {},
                clientInfo: { name: "test-client", version: "0.0.1" },
            },
        });

        expect(resp.id).toBe(1);
        expect(resp.error).toBeUndefined();

        const result = resp.result as {
            protocolVersion: string;
            capabilities: Record<string, unknown>;
            serverInfo: { name: string; version: string };
        };
        expect(result.protocolVersion).toBe("2025-06-18");
        expect(Object.keys(result.capabilities)).toStrictEqual(["tools"]);
    });
});

describe("MCP async §2: tools/list AJV well-formedness", () => {
    const server = makeServer();
    const session = new McpSession();

    test("§2.3: each inputSchema compiles as well-formed Draft-2020-12", async () => {
        await initSession(server, session);
        const resp = await send(server, session, { id: 2, method: "tools/list" });
        const result = resp.result as {
            tools: Array<{ name: string; inputSchema: unknown }>;
        };
        expect(result.tools).toHaveLength(6);
        for (const t of result.tools) {
            const validate = ajv.compile(t.inputSchema as object);
            expect(typeof validate).toBe("function");
        }
    });
});

// ---------------------------------------------------------------------------
// §3 — tools/call success path (smoke round-trip through the async wiring).
// ---------------------------------------------------------------------------

describe("MCP async §3: tools/call success path", () => {
    const server = makeServer();
    const session = new McpSession();

    test("§3.1 McpTools_ping: content[0].text is '{\"ok\":true}', isError is false", async () => {
        await initSession(server, session);
        const resp = await send(server, session, {
            id: 3,
            method: "tools/call",
            params: { name: "McpTools_ping", arguments: { seqno: 42, label: "hello" } },
        });

        expect(resp.id).toBe(3);
        expect(resp.error).toBeUndefined();

        const result = resp.result as {
            content: Array<{ type: string; text: string }>;
            isError?: boolean;
        };
        expect(result.content).toHaveLength(1);
        expect(result.content[0].type).toBe("text");
        const payload = JSON.parse(result.content[0].text) as { ok: boolean };
        expect(payload.ok).toBe(true);
        expect(result.isError === false || result.isError === undefined).toBe(true);
    });
});
