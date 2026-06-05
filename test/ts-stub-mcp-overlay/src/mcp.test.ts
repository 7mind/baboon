/**
 * T9 — TypeScript MCP round-trip overlay test.
 *
 * Drives the generated McpToolsMcpServer<Ctx> through the canonical T7 scenario
 * (docs/research/mcp-roundtrip-scenario.md) using an entirely in-process fake
 * transport. No stdio or HTTP is involved.
 *
 * Assertion discipline (T7 §5.1):
 *   - All assertions are Vitest `expect(...).toBe(...)` / `.toStrictEqual(...)` etc.
 *     which throw unconditionally on failure.
 *   - No `if` guards around assertions — a conditional check that only runs when
 *     it can pass is not a live assertion.
 *
 * AJV tier (T7 §5.3 — T9 is the full-validator tier):
 *   - Each returned inputSchema is compiled with AJV Draft-2020-12. Compile errors
 *     throw immediately, proving the schema is well-formed. Conforming instances are
 *     validated after compilation.
 *
 * Negative controls (T7 §5.2):
 *   - §4.1 (unknown tool → -32602): if the server returned success for
 *     McpTools_nonexistent the assertions below would fail.
 *   - §4.2 (malformed decode → Channel-B isError=true): if isError were false this
 *     test would fail.
 *   - AJV conforming/non-conforming: live — see "enum negative control" test.
 *   - DELIBERATE-NEGATIVE-CONTROL (documented, not left active):
 *     Replacing `expect(tools[4].name).toBe("McpTools_ping")` with
 *     `expect(tools[4].name).toBe("McpTools_WRONG")` makes the test fail, proving
 *     the position-4 name assertion is live.
 *
 * Channel-B (§4.2) trigger strategy:
 *   The TypeScript baboon codecs use `as` casts and do NOT throw on missing scalar
 *   fields (e.g. missing `seqno` from ping). To reliably trigger a real `DecoderFailed`
 *   we use `McpTools_submitComposite` with `nested: null` — the Nested decoder does
 *   `(null as Record<string,unknown>)["point"]` which throws `TypeError` at runtime
 *   and is caught by the wiring `try/catch` as `DecoderFailed`.
 */

import { describe, test, expect } from "vitest";
import Ajv from "ajv/dist/2020.js";

import {
    BaboonCodecContext,
    BaboonMethodId,
    BaboonWiringError,
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

// Generated DTOs — imported only to construct stub responses.
import { Out as PingOut } from "./baboondefinitions/generated/mcp/stub/mcp-tools/ping/out";
import { In as PingIn } from "./baboondefinitions/generated/mcp/stub/mcp-tools/ping/in";
// FFancyStr codec registration — foreign types have no built-in codec.
import { FFancyStr_JsonCodec } from "./baboondefinitions/generated/mcp/stub/FFancyStr";
import { Lazy } from "./baboondefinitions/generated/BaboonSharedRuntime";
import { In as ListCollectionsIn } from "./baboondefinitions/generated/mcp/stub/mcp-tools/listcollections/in";
import { Out as ListCollectionsOut } from "./baboondefinitions/generated/mcp/stub/mcp-tools/listcollections/out";
import { In as SubmitCompositeIn } from "./baboondefinitions/generated/mcp/stub/mcp-tools/submitcomposite/in";
import { Out as SubmitCompositeOut } from "./baboondefinitions/generated/mcp/stub/mcp-tools/submitcomposite/out";
import { In as ProcessShapeIn } from "./baboondefinitions/generated/mcp/stub/mcp-tools/processshape/in";
import { Out as ProcessShapeOut } from "./baboondefinitions/generated/mcp/stub/mcp-tools/processshape/out";
import { In as PagePointsIn } from "./baboondefinitions/generated/mcp/stub/mcp-tools/pagepoints/in";
import { Out as PagePointsOut } from "./baboondefinitions/generated/mcp/stub/mcp-tools/pagepoints/out";

// ---------------------------------------------------------------------------
// Register a passthrough codec for FFancyStr (foreign type → string identity).
//
// FFancyStr is a foreign type with no built-in codec: the generated
// FFancyStr_JsonCodec.decode throws BaboonDecoderFailure by design, requiring
// the integrator to provide a real codec via the lazy-instance override. In
// this test harness a passthrough (string → string) is sufficient.
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
// Stub McpTools service: every method returns ok=true by convention (T7 §3).
// ---------------------------------------------------------------------------

const stubMcpTools: McpTools = {
    listCollections(_arg: ListCollectionsIn): ListCollectionsOut {
        return new ListCollectionsOut(true);
    },
    submitComposite(_arg: SubmitCompositeIn): SubmitCompositeOut {
        return new SubmitCompositeOut(true);
    },
    processShape(_arg: ProcessShapeIn): ProcessShapeOut {
        return new ProcessShapeOut(true);
    },
    pagePoints(_arg: PagePointsIn): PagePointsOut {
        return new PagePointsOut(true);
    },
    ping(_arg: PingIn): PingOut {
        return new PingOut(true);
    },
};

// ---------------------------------------------------------------------------
// Fake transport: routes through the generated invokeJson_McpTools dispatch.
// This is transport-abstract — no HTTP, no stdio, no network.
// ---------------------------------------------------------------------------

const codecCtx = BaboonCodecContext.Default;
const rt = BaboonServiceRtDefault;

function fakeInvokeJson(
    method: BaboonMethodId,
    data: string,
    _ctx: null,
    _codecCtx: BaboonCodecContext,
): BaboonEitherResult {
    // invokeJson_McpTools returns BaboonEither<BaboonWiringError, string>.
    // BaboonEitherResult has the same {tag, value} shape — safe cast.
    const result = invokeJson_McpTools(method, data, stubMcpTools, rt, codecCtx);
    return result as unknown as BaboonEitherResult;
}

// ---------------------------------------------------------------------------
// Server factory: a new McpToolsMcpServer<null> per test sequence.
// McpSession is per-connection state; one new session per describe block.
// ---------------------------------------------------------------------------

function makeServer(): McpToolsMcpServer<null> {
    return new McpToolsMcpServer<null>(fakeInvokeJson);
}

// Helper: send one JSON-RPC request and assert a response was returned.
function send(
    server: McpToolsMcpServer<null>,
    session: McpSession,
    req: JsonRpcRequest,
): JsonRpcResponse {
    const resp = server.handle(req, session, null, codecCtx);
    if (resp === undefined) {
        throw new Error(
            `Expected a response for "${req.method}" but got undefined (notification not expected here)`,
        );
    }
    return resp;
}

// ---------------------------------------------------------------------------
// AJV Draft 2020-12 validator (shared across all tests in this file).
// ---------------------------------------------------------------------------

const ajv = new Ajv({ strict: false, allErrors: true });

// ---------------------------------------------------------------------------
// Helpers for the tools/list describe block (run once, shared across tests).
// ---------------------------------------------------------------------------

function initAndList(): { tools: Array<{ name: string; inputSchema: unknown; description?: string }>; resp: JsonRpcResponse } {
    const server = makeServer();
    const session = new McpSession();

    server.handle(
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
    server.handle({ method: "notifications/initialized" }, session, null, codecCtx);

    const resp = send(server, session, { id: 2, method: "tools/list" });
    const result = resp.result as {
        tools: Array<{ name: string; inputSchema: unknown; description?: string }>;
    };
    return { tools: result.tools, resp };
}

// ---------------------------------------------------------------------------
// §1 — initialize
// ---------------------------------------------------------------------------

describe("MCP §1: initialize", () => {
    const server = makeServer();
    const session = new McpSession();

    test("§1.2: protocolVersion, capabilities, serverInfo are correct", () => {
        const resp = send(server, session, {
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

        // protocolVersion MUST be "2025-06-18"
        expect(result.protocolVersion).toBe("2025-06-18");

        // capabilities MUST contain exactly one key "tools" with value {}
        const capKeys = Object.keys(result.capabilities);
        expect(capKeys).toHaveLength(1);
        expect(capKeys[0]).toBe("tools");
        expect(result.capabilities["tools"]).toStrictEqual({});

        // serverInfo: name and version must be non-empty strings
        expect(typeof result.serverInfo.name).toBe("string");
        expect(result.serverInfo.name.length).toBeGreaterThan(0);
        expect(typeof result.serverInfo.version).toBe("string");
        expect(result.serverInfo.version.length).toBeGreaterThan(0);
    });

    test("§1.2 post-initialize: notifications/initialized produces no response", () => {
        const notifResp = server.handle(
            { method: "notifications/initialized" },
            session,
            null,
            codecCtx,
        );
        // MUST return undefined (no JSON-RPC reply for a notification).
        expect(notifResp).toBeUndefined();
    });
});

// ---------------------------------------------------------------------------
// §2 — tools/list + AJV inputSchema validation
// ---------------------------------------------------------------------------

describe("MCP §2: tools/list and AJV inputSchema validation", () => {
    // Shared result — computed once for all tests in this describe block.
    const { tools, resp } = initAndList();

    test("§2.2: exactly 5 tools in declaration order (positions 0–4)", () => {
        expect(resp.id).toBe(2);
        expect(resp.error).toBeUndefined();
        expect(Array.isArray(tools)).toBe(true);
        expect(tools).toHaveLength(5);

        // Exact position assertions per §0 (model declaration order).
        // DELIBERATE-NEGATIVE-CONTROL: changing "McpTools_ping" → "McpTools_WRONG"
        // on the next line makes this test fail, proving position[4] check is live.
        expect(tools[0].name).toBe("McpTools_listCollections");
        expect(tools[1].name).toBe("McpTools_submitComposite");
        expect(tools[2].name).toBe("McpTools_processShape");
        expect(tools[3].name).toBe("McpTools_pagePoints");
        expect(tools[4].name).toBe("McpTools_ping");

        // No "nextCursor" key (§2.2)
        expect((resp.result as Record<string, unknown>)["nextCursor"]).toBeUndefined();

        // No "description" key for any tool (stub model has no doc comments)
        for (const t of tools) {
            expect(t.description).toBeUndefined();
        }
    });

    test("§2.3: each inputSchema.$schema is the Draft-2020-12 URI", () => {
        for (const t of tools) {
            const schema = t.inputSchema as Record<string, unknown>;
            expect(schema["$schema"]).toBe("https://json-schema.org/draft/2020-12/schema");
        }
    });

    test("§2.3 AJV: each inputSchema compiles as well-formed Draft-2020-12 (K1 tier)", () => {
        // ajv.compile() throws if the schema is malformed or has unresolved $refs.
        // This is the K1 full-validator check required for T9.
        for (const t of tools) {
            // Must not throw — proves the schema is well-formed.
            const validate = ajv.compile(t.inputSchema as object);
            expect(typeof validate).toBe("function");
        }
    });

    test("§2.3 AJV tool[0] McpTools_listCollections: conforming instance is valid", () => {
        const validate = ajv.compile(tools[0].inputSchema as object);
        // D6/T30: byColor is map[Color,str] — a string-keyed object with enum wire-name
        // keys (matches the wire the codecs emit and the reconciled inputSchema).
        const valid = validate({
            tags: ["a", "b"],
            uniqueIds: [1, 2],
            labels: { k: "v" },
            byColor: { Green: "ok", Red: "stop" },
        });
        expect(valid).toBe(true);
    });

    test("§2.3 AJV tool[1] McpTools_submitComposite: conforming instance is valid", () => {
        const validate = ajv.compile(tools[1].inputSchema as object);
        const valid = validate({
            nested: { point: { x: 1, y: 2 }, color: "Red" },
            maybePoint: null,
            color: "Blue",
            fancy: "anything",
        });
        expect(valid).toBe(true);
    });

    test("§2.3 AJV tool[1] McpTools_submitComposite: enum negative control — 'Purple' is rejected", () => {
        // Negative control (K1): proves the enum constraint ["Red","Green","Blue"] is live.
        // If this assertion were removed and "Purple" were accepted, K1 would fail silently.
        const validate = ajv.compile(tools[1].inputSchema as object);
        const valid = validate({
            nested: { point: { x: 1, y: 2 }, color: "Red" },
            maybePoint: null,
            color: "Purple",   // not in ["Red", "Green", "Blue"]
            fancy: "anything",
        });
        expect(valid).toBe(false);  // MUST be rejected
    });

    test("§2.3 AJV tool[2] McpTools_processShape: conforming instance is valid", () => {
        const validate = ajv.compile(tools[2].inputSchema as object);
        const valid = validate({
            shape: { radius: 1.5 },
            tree: { value: 0, children: [{ value: 1, children: [] }] },
        });
        expect(valid).toBe(true);
    });

    test("§2.3 AJV tool[3] McpTools_pagePoints: conforming instance is valid", () => {
        const validate = ajv.compile(tools[3].inputSchema as object);
        const valid = validate({ page: { items: [{ x: 1, y: 2 }], total: 1 } });
        expect(valid).toBe(true);
    });

    test("§2.3 AJV tool[4] McpTools_ping: conforming instance is valid", () => {
        const validate = ajv.compile(tools[4].inputSchema as object);
        const valid = validate({ seqno: 7, label: "hi" });
        expect(valid).toBe(true);
    });

    test("§2.3 AJV tool[4] McpTools_ping: required-field negative control — missing 'label' is rejected", () => {
        // Negative control: proves the `required: ["seqno","label"]` constraint is live.
        const validate = ajv.compile(tools[4].inputSchema as object);
        const valid = validate({ seqno: 7 });   // missing required "label"
        expect(valid).toBe(false);  // MUST be rejected
    });
});

// ---------------------------------------------------------------------------
// §3 — tools/call (success paths)
// ---------------------------------------------------------------------------

describe("MCP §3: tools/call success paths", () => {
    const server = makeServer();
    const session = new McpSession();

    // Precondition: initialize + notifications/initialized.
    server.handle(
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
    server.handle({ method: "notifications/initialized" }, session, null, codecCtx);

    test("§3.1 McpTools_ping: content[0].text is '{\"ok\":true}', isError is false", () => {
        const resp = send(server, session, {
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

        // isError MUST be false or absent (K4 §2.4 permits omission when false).
        expect(result.isError === false || result.isError === undefined).toBe(true);
    });

    test("§3.2 McpTools_submitComposite: content[0].text is '{\"ok\":true}', isError is false", () => {
        const resp = send(server, session, {
            id: 4,
            method: "tools/call",
            params: {
                name: "McpTools_submitComposite",
                arguments: {
                    nested: { point: { x: 10, y: 20 }, color: "Red" },
                    maybePoint: null,
                    color: "Blue",
                    fancy: "test-value",
                },
            },
        });

        expect(resp.id).toBe(4);
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

// ---------------------------------------------------------------------------
// §4 — tools/call (error paths) — primary negative controls
// ---------------------------------------------------------------------------

describe("MCP §4: tools/call error paths (negative controls)", () => {
    const server = makeServer();
    const session = new McpSession();

    server.handle(
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
    server.handle({ method: "notifications/initialized" }, session, null, codecCtx);

    test("§4.1 unknown tool → Channel-A error, code exactly -32602 (dispatch liveness)", () => {
        // NEGATIVE CONTROL (T7 §5.2): if the server returned a success result for
        // McpTools_nonexistent, `expect(resp.error).toBeDefined()` would fail.
        // If error.code were -32601 instead of -32602, the next assertion fails.
        const resp = send(server, session, {
            id: 5,
            method: "tools/call",
            params: { name: "McpTools_nonexistent", arguments: {} },
        });

        expect(resp.id).toBe(5);
        // MUST be a Channel-A error, not a result.
        expect(resp.error).toBeDefined();
        expect(resp.result).toBeUndefined();
        // §4.1: code MUST be -32602 (InvalidParams — unknown tool).
        expect(resp.error!.code).toBe(-32602);
        expect(typeof resp.error!.message).toBe("string");
        expect(resp.error!.message.length).toBeGreaterThan(0);
    });

    test("§4.2 decode failure → Channel-B isError=true (codec throws on null nested)", () => {
        // NEGATIVE CONTROL: if isError were false this test would fail.
        //
        // Channel-B trigger: `nested: null` causes Nested_JsonCodec.decode to execute
        // `(null as Record<string,unknown>)["point"]` which throws TypeError at runtime.
        // The wiring catch-block wraps it as { tag: 'DecoderFailed', ... } and
        // invokeJson_McpTools returns Left. The MCP server then produces Channel-B.
        const resp = send(server, session, {
            id: 6,
            method: "tools/call",
            params: {
                name: "McpTools_submitComposite",
                arguments: {
                    nested: null,   // deliberately null to trigger decoder TypeError
                    color: "Blue",
                    fancy: "test-value",
                },
            },
        });

        expect(resp.id).toBe(6);
        // Channel B: MUST be a result (not error) with isError=true.
        expect(resp.result).toBeDefined();
        expect(resp.error).toBeUndefined();

        const result = resp.result as {
            content: Array<{ type: string; text: string }>;
            isError: boolean;
        };
        expect(result.isError).toBe(true);
        expect(Array.isArray(result.content)).toBe(true);
        expect(result.content.length).toBeGreaterThan(0);
        expect(result.content[0].type).toBe("text");
        expect(result.content[0].text.length).toBeGreaterThan(0);
    });
});
