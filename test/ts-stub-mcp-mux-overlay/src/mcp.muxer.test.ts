/**
 * T105 — TypeScript MCP muxer lane test.
 *
 * Exercises `AbstractMcpMuxer<Ctx>` by composing two FRESHLY GENERATED
 * `<Service>McpServer` instances produced from the T102 `mcp-mux-stub-ok/`
 * model (UserService + OrderService). Composition is done strictly through
 * the public T114 routable surface:
 *   - the muxer ctor takes `IBaboonRoutableMcpServer<Ctx>` members,
 *   - the test NEVER subclasses a `<Service>McpServer`,
 *   - the test NEVER calls a member server's own `handle()`.
 *
 * Generated code lands in `src/mux-generated/` (the isolated dir set by
 * the `test-gen-ts-mcp-mux` mdl action). No committed generated fixtures.
 *
 * Four asserted muxer behaviours (T105 acceptance):
 *   1. tools/list  -> UNION of both services' tools in registration-then-
 *                    declaration order;
 *   2. tools/call  -> routes the flat tool name to the correct owning
 *                    service — proven for a tool of EACH service:
 *                    UserService_getUser (Channel-A Right, isError:false),
 *                    OrderService_cancelOrder (Channel-A Right, isError:false);
 *   3. register a server with a colliding tool name -> throws
 *      BaboonMcpWiringException{tag:'DuplicateTool'};
 *   4. unknown tool name -> -32602 "unknown tool" response (NoMatchingTool).
 *
 * Assertion discipline: every assertion is an unconditional vitest
 * expect(...) or expect(() => ...).toThrow. No `if`-guarded checks.
 */

import { describe, test, expect } from "vitest";

import { BaboonCodecContext, BaboonMethodId } from "./mux-generated/BaboonSharedRuntime";
import {
    JsonRpcRequest,
    JsonRpcResponse,
    McpSession,
    McpServerInfo,
    BaboonEitherResult,
    IBaboonRoutableMcpServer,
    AbstractMcpMuxer,
    BaboonMcpWiringException,
} from "./mux-generated/BaboonMcpRuntime";

import { BaboonServiceRtDefault as SyncRt } from "./mux-generated/mcp/mux/stub/baboon-service-rt";
import { UserService } from "./mux-generated/mcp/mux/stub/user-service/service";
import { invokeJson_UserService } from "./mux-generated/mcp/mux/stub/user-service/wiring";
import { UserServiceMcpServer } from "./mux-generated/mcp/mux/stub/user-service/mcp-server";
import { OrderService } from "./mux-generated/mcp/mux/stub/order-service/service";
import { invokeJson_OrderService } from "./mux-generated/mcp/mux/stub/order-service/wiring";
import { OrderServiceMcpServer } from "./mux-generated/mcp/mux/stub/order-service/mcp-server";

import { UserProfile } from "./mux-generated/mcp/mux/stub/UserProfile";
import { UserStatus } from "./mux-generated/mcp/mux/stub/UserStatus";
import { Out as GetUserOut } from "./mux-generated/mcp/mux/stub/user-service/getuser/out";
import { Out as CreateUserOut } from "./mux-generated/mcp/mux/stub/user-service/createuser/out";
import { Out as PlaceOrderOut } from "./mux-generated/mcp/mux/stub/order-service/placeorder/out";
import { Out as CancelOrderOut } from "./mux-generated/mcp/mux/stub/order-service/cancelorder/out";
import { OrderStatus } from "./mux-generated/mcp/mux/stub/OrderStatus";
import { OrderSummary } from "./mux-generated/mcp/mux/stub/OrderSummary";

const codecCtx = BaboonCodecContext.Default;

const MERGED_INFO: McpServerInfo = { name: "MergedEndpoint", version: "1.0.0" };

// The union of both services' tools in registration-then-declaration order:
// UserService registered first (createUser, getUser declared in that order),
// OrderService second (placeOrder, cancelOrder).
const EXPECTED_UNION = [
    "UserService_createUser",
    "UserService_getUser",
    "OrderService_placeOrder",
    "OrderService_cancelOrder",
];

// ---------------------------------------------------------------------------
// Stub implementations
// ---------------------------------------------------------------------------

const stubUser: UserService = {
    createUser: () => new CreateUserOut(new UserProfile("u1", "a@b.c", UserStatus.Active)),
    getUser: () => new GetUserOut(new UserProfile("u1", "a@b.c", UserStatus.Active)),
};

const stubOrder: OrderService = {
    placeOrder: () => new PlaceOrderOut(new OrderSummary("o1", OrderStatus.Confirmed, 10.0)),
    cancelOrder: () => new CancelOrderOut(true),
};

// ---------------------------------------------------------------------------
// Delegate factories: the SAME errors-mode invokeJson the integrator would bind.
// ---------------------------------------------------------------------------

function userDelegate(m: BaboonMethodId, d: string, _ctx: null, cc: BaboonCodecContext): BaboonEitherResult {
    return invokeJson_UserService(m, d, stubUser, SyncRt, cc) as unknown as BaboonEitherResult;
}

function orderDelegate(m: BaboonMethodId, d: string, _ctx: null, cc: BaboonCodecContext): BaboonEitherResult {
    return invokeJson_OrderService(m, d, stubOrder, SyncRt, cc) as unknown as BaboonEitherResult;
}

// ---------------------------------------------------------------------------
// Server factories: bound through the PUBLIC routable interface only.
// ---------------------------------------------------------------------------

function makeUserServer(): IBaboonRoutableMcpServer<null> {
    return new UserServiceMcpServer<null>(userDelegate);
}

function makeOrderServer(): IBaboonRoutableMcpServer<null> {
    return new OrderServiceMcpServer<null>(orderDelegate);
}

function makeMuxer(): AbstractMcpMuxer<null> {
    return new AbstractMcpMuxer<null>(MERGED_INFO, makeUserServer(), makeOrderServer());
}

// ---------------------------------------------------------------------------
// Test helpers
// ---------------------------------------------------------------------------

function initAndList(mux: AbstractMcpMuxer<null>): JsonRpcResponse {
    const session = new McpSession();
    mux.handle(
        { id: 0, method: "initialize", params: { protocolVersion: "2025-06-18", capabilities: {}, clientInfo: { name: "t", version: "0" } } },
        session, null, codecCtx,
    );
    mux.handle({ method: "notifications/initialized" }, session, null, codecCtx);
    const resp = mux.handle({ id: 1, method: "tools/list" }, session, null, codecCtx);
    expect(resp).toBeDefined();
    return resp!;
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

function call(mux: AbstractMcpMuxer<null>, session: McpSession, name: string, args: unknown): JsonRpcResponse {
    const req: JsonRpcRequest = { id: 99, method: "tools/call", params: { name, arguments: args } };
    const resp = mux.handle(req, session, null, codecCtx);
    expect(resp).toBeDefined();
    return resp!;
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

describe("MCP muxer (T105): AbstractMcpMuxer over two freshly-generated servers", () => {

    test("1: tools/list returns the UNION in registration-then-declaration order", () => {
        const resp = initAndList(makeMuxer());
        expect(resp.error).toBeUndefined();
        const result = resp.result as { tools: Array<{ name: string }> };
        const names = result.tools.map(t => t.name);
        expect(names).toStrictEqual(EXPECTED_UNION);
    });

    test("1 negative control: union is NOT in interleaved order", () => {
        // Proves the ordering assertion is live: a wrong order fails.
        const resp = initAndList(makeMuxer());
        const result = resp.result as { tools: Array<{ name: string }> };
        const names = result.tools.map(t => t.name);
        expect(names).not.toStrictEqual([
            "UserService_createUser",
            "OrderService_placeOrder",
            "UserService_getUser",
            "OrderService_cancelOrder",
        ]);
    });

    test("2 routing -- UserService_getUser -> owning service, Channel-A Right (isError:false)", () => {
        const mux = makeMuxer();
        const session = initedSession(mux);
        const resp = call(mux, session, "UserService_getUser", { userId: "u1" });
        expect(resp.id).toBe(99);
        expect(resp.error).toBeUndefined();
        const result = resp.result as { content: Array<{ type: string; text: string }>; isError: boolean };
        expect(result.isError).toBe(false);
        expect(result.content).toHaveLength(1);
        expect(result.content[0].type).toBe("text");
        // The JSON payload proves UserService handled it (profile.userId present).
        const payload = JSON.parse(result.content[0].text) as { profile: { userId: string } };
        expect(payload.profile.userId).toBe("u1");
    });

    test("2 routing -- OrderService_cancelOrder -> owning service, Channel-A Right (isError:false)", () => {
        const mux = makeMuxer();
        const session = initedSession(mux);
        const resp = call(mux, session, "OrderService_cancelOrder", { orderId: "o1", reason: null });
        expect(resp.error).toBeUndefined();
        const result = resp.result as { content: Array<{ type: string; text: string }>; isError: boolean };
        expect(result.isError).toBe(false);
        // The JSON payload proves OrderService handled it (ok field present).
        const payload = JSON.parse(result.content[0].text) as { ok: boolean };
        expect(payload.ok).toBe(true);
    });

    test("2 routing -- OrderService_placeOrder decoder failure -> Channel-B Left (isError:true)", () => {
        // items:null makes placeorder_In_JsonCodec.decode do (null as unknown[]).map -> TypeError,
        // caught by the wiring as { tag:'DecoderFailed' } -> Left -> Channel-B (isError:true).
        const mux = makeMuxer();
        const session = initedSession(mux);
        const resp = call(mux, session, "OrderService_placeOrder", { userId: "u1", items: null });
        expect(resp.error).toBeUndefined();
        const result = resp.result as { content: Array<{ type: string; text: string }>; isError: boolean };
        expect(result.isError).toBe(true);
        expect(result.content.length).toBeGreaterThan(0);
        expect(result.content[0].text.length).toBeGreaterThan(0);
    });

    test("3: registering a colliding tool name throws BaboonMcpWiringException{tag:'DuplicateTool'}", () => {
        // Second UserServiceMcpServer re-declares UserService_createUser / _getUser.
        let thrown: unknown;
        try {
            new AbstractMcpMuxer<null>(MERGED_INFO, makeUserServer(), makeUserServer());
        } catch (e) {
            thrown = e;
        }
        expect(thrown).toBeInstanceOf(BaboonMcpWiringException);
        expect((thrown as BaboonMcpWiringException).error.tag).toBe("DuplicateTool");
        expect((thrown as BaboonMcpWiringException).error.toolName).toBe("UserService_createUser");
    });

    test("3 also via register(): collision on register throws DuplicateTool", () => {
        const mux = new AbstractMcpMuxer<null>(MERGED_INFO, makeUserServer());
        expect(() => mux.register(makeUserServer())).toThrowError(BaboonMcpWiringException);
    });

    test("4: unknown tool name -> -32602 'unknown tool' response (NoMatchingTool)", () => {
        const mux = makeMuxer();
        const session = initedSession(mux);
        const resp = call(mux, session, "UserService_nope", {});
        // -32602 surfaces as a JSON-RPC error (NOT a result).
        expect(resp.error).toBeDefined();
        expect(resp.result).toBeUndefined();
        expect(resp.error!.code).toBe(-32602);
        expect(resp.error!.message.length).toBeGreaterThan(0);
    });
});
