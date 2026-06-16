/**
 * T105 (async) — TypeScript ASYNC MCP muxer lane test.
 *
 * Async sibling of `mcp.muxer.test.ts`. Exercises `AbstractAsyncMcpMuxer<Ctx>`
 * by composing two FRESHLY GENERATED async `<Service>McpServer` instances
 * produced from the T102 `mcp-mux-stub-ok/` model (UserService + OrderService)
 * compiled with `--ts-async-services=true`. Composition is done strictly through
 * the public T114 routable surface:
 *   - the muxer ctor takes `IBaboonRoutableAsyncMcpServer<Ctx>` members,
 *   - the test NEVER subclasses a `<Service>McpServer`,
 *   - the test NEVER calls a member server's own `handle()`.
 *
 * Generated code lands in `src/mux-async-generated/` (the isolated dir set by
 * the `test-gen-ts-mcp-mux-async` mdl action). No committed generated fixtures.
 *
 * Under `--ts-async-services=true`:
 *   - service interface methods return `Promise<...Out>`,
 *   - the generated wiring `invokeJson_<Service>` is `async` and returns
 *     `Promise<BaboonEither<BaboonWiringError, string>>`,
 *   - the generated `<Service>McpServer` extends `AbstractAsyncBaboonMcpServer`
 *     and implements `IBaboonRoutableAsyncMcpServer<Ctx>`,
 *   - `AbstractAsyncMcpMuxer.handle()` is `async` and awaits each `routeToolCall`.
 *
 * Four asserted muxer behaviours (T105 acceptance, async axis):
 *   1. tools/list  -> UNION of both services' tools in registration-then-
 *                    declaration order (awaited);
 *   2. tools/call  -> routes the flat tool name to the correct owning service,
 *                    awaited, proven for a tool of EACH service:
 *                    UserService_getUser (Channel-A Right, isError:false),
 *                    OrderService_cancelOrder (Channel-A Right, isError:false);
 *   3. register a server with a colliding tool name -> throws
 *      BaboonMcpWiringException{tag:'DuplicateTool'};
 *   4. unknown tool name -> -32602 "unknown tool" response (NoMatchingTool),
 *                          awaited.
 *
 * Assertion discipline: every assertion is an unconditional vitest
 * expect(...) or expect(() => ...).toThrow. No `if`-guarded checks.
 */

import { describe, test, expect } from "vitest";

import { BaboonCodecContext, BaboonMethodId } from "./mux-async-generated/BaboonSharedRuntime";
import {
    JsonRpcRequest,
    JsonRpcResponse,
    McpSession,
    McpServerInfo,
    BaboonEitherResult,
    IBaboonRoutableAsyncMcpServer,
    AbstractAsyncMcpMuxer,
    BaboonMcpWiringException,
} from "./mux-async-generated/BaboonMcpRuntime";

import { BaboonServiceRtDefault as AsyncRt } from "./mux-async-generated/mcp/mux/stub/baboon-service-rt";
import { UserService } from "./mux-async-generated/mcp/mux/stub/user-service/service";
import { invokeJson_UserService } from "./mux-async-generated/mcp/mux/stub/user-service/wiring";
import { UserServiceMcpServer } from "./mux-async-generated/mcp/mux/stub/user-service/mcp-server";
import { OrderService } from "./mux-async-generated/mcp/mux/stub/order-service/service";
import { invokeJson_OrderService } from "./mux-async-generated/mcp/mux/stub/order-service/wiring";
import { OrderServiceMcpServer } from "./mux-async-generated/mcp/mux/stub/order-service/mcp-server";

import { UserProfile } from "./mux-async-generated/mcp/mux/stub/UserProfile";
import { UserStatus } from "./mux-async-generated/mcp/mux/stub/UserStatus";
import { Out as GetUserOut } from "./mux-async-generated/mcp/mux/stub/user-service/getuser/out";
import { Out as CreateUserOut } from "./mux-async-generated/mcp/mux/stub/user-service/createuser/out";
import { Out as PlaceOrderOut } from "./mux-async-generated/mcp/mux/stub/order-service/placeorder/out";
import { Out as CancelOrderOut } from "./mux-async-generated/mcp/mux/stub/order-service/cancelorder/out";
import { OrderStatus } from "./mux-async-generated/mcp/mux/stub/OrderStatus";
import { OrderSummary } from "./mux-async-generated/mcp/mux/stub/OrderSummary";

const codecCtx = BaboonCodecContext.Default;

const MERGED_INFO: McpServerInfo = { name: "MergedAsyncEndpoint", version: "1.0.0" };

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
// Stub implementations -- under --ts-async-services=true, every method
// returns a Promise.
// ---------------------------------------------------------------------------

const stubUser: UserService = {
    async createUser() { return new CreateUserOut(new UserProfile("u1", "a@b.c", UserStatus.Active)); },
    async getUser()    { return new GetUserOut(new UserProfile("u1", "a@b.c", UserStatus.Active)); },
};

const stubOrder: OrderService = {
    async placeOrder()  { return new PlaceOrderOut(new OrderSummary("o1", OrderStatus.Confirmed, 10.0)); },
    async cancelOrder() { return new CancelOrderOut(true); },
};

// ---------------------------------------------------------------------------
// Delegate factories: async invokeJson delegates (the generated wiring returns
// Promise<BaboonEither<...>> under --ts-async-services=true).
// ---------------------------------------------------------------------------

async function userDelegate(
    m: BaboonMethodId, d: string, _ctx: null, cc: BaboonCodecContext,
): Promise<BaboonEitherResult> {
    const result = await invokeJson_UserService(m, d, stubUser, AsyncRt, cc);
    return result as unknown as BaboonEitherResult;
}

async function orderDelegate(
    m: BaboonMethodId, d: string, _ctx: null, cc: BaboonCodecContext,
): Promise<BaboonEitherResult> {
    const result = await invokeJson_OrderService(m, d, stubOrder, AsyncRt, cc);
    return result as unknown as BaboonEitherResult;
}

// ---------------------------------------------------------------------------
// Server factories: bound through the PUBLIC routable async interface only.
// ---------------------------------------------------------------------------

function makeUserServer(): IBaboonRoutableAsyncMcpServer<null> {
    return new UserServiceMcpServer<null>(userDelegate);
}

function makeOrderServer(): IBaboonRoutableAsyncMcpServer<null> {
    return new OrderServiceMcpServer<null>(orderDelegate);
}

function makeMuxer(): AbstractAsyncMcpMuxer<null> {
    return new AbstractAsyncMcpMuxer<null>(MERGED_INFO, makeUserServer(), makeOrderServer());
}

// ---------------------------------------------------------------------------
// Test helpers (all async -- handle() returns Promise<JsonRpcResponse|undefined>)
// ---------------------------------------------------------------------------

async function initAndList(mux: AbstractAsyncMcpMuxer<null>): Promise<JsonRpcResponse> {
    const session = new McpSession();
    await mux.handle(
        { id: 0, method: "initialize", params: { protocolVersion: "2025-06-18", capabilities: {}, clientInfo: { name: "t", version: "0" } } },
        session, null, codecCtx,
    );
    await mux.handle({ method: "notifications/initialized" }, session, null, codecCtx);
    const resp = await mux.handle({ id: 1, method: "tools/list" }, session, null, codecCtx);
    expect(resp).toBeDefined();
    return resp!;
}

async function initedSession(mux: AbstractAsyncMcpMuxer<null>): Promise<McpSession> {
    const session = new McpSession();
    await mux.handle(
        { id: 0, method: "initialize", params: { protocolVersion: "2025-06-18", capabilities: {}, clientInfo: { name: "t", version: "0" } } },
        session, null, codecCtx,
    );
    await mux.handle({ method: "notifications/initialized" }, session, null, codecCtx);
    return session;
}

async function call(
    mux: AbstractAsyncMcpMuxer<null>,
    session: McpSession,
    name: string,
    args: unknown,
): Promise<JsonRpcResponse> {
    const req: JsonRpcRequest = { id: 99, method: "tools/call", params: { name, arguments: args } };
    const resp = await mux.handle(req, session, null, codecCtx);
    expect(resp).toBeDefined();
    return resp!;
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

describe("MCP async muxer (T105): AbstractAsyncMcpMuxer over two freshly-generated servers", () => {

    test("1: tools/list returns the UNION in registration-then-declaration order", async () => {
        const resp = await initAndList(makeMuxer());
        expect(resp.error).toBeUndefined();
        const result = resp.result as { tools: Array<{ name: string }> };
        const names = result.tools.map(t => t.name);
        expect(names).toStrictEqual(EXPECTED_UNION);
    });

    test("1 negative control: union is NOT in interleaved order", async () => {
        const resp = await initAndList(makeMuxer());
        const result = resp.result as { tools: Array<{ name: string }> };
        const names = result.tools.map(t => t.name);
        expect(names).not.toStrictEqual([
            "UserService_createUser",
            "OrderService_placeOrder",
            "UserService_getUser",
            "OrderService_cancelOrder",
        ]);
    });

    test("2 routing -- UserService_getUser -> owning service, Channel-A Right (isError:false)", async () => {
        const mux = makeMuxer();
        const session = await initedSession(mux);
        const resp = await call(mux, session, "UserService_getUser", { userId: "u1" });
        expect(resp.id).toBe(99);
        expect(resp.error).toBeUndefined();
        const result = resp.result as { content: Array<{ type: string; text: string }>; isError: boolean };
        expect(result.isError).toBe(false);
        expect(result.content).toHaveLength(1);
        expect(result.content[0].type).toBe("text");
        const payload = JSON.parse(result.content[0].text) as { profile: { userId: string } };
        expect(payload.profile.userId).toBe("u1");
    });

    test("2 routing -- OrderService_cancelOrder -> owning service, Channel-A Right (isError:false)", async () => {
        const mux = makeMuxer();
        const session = await initedSession(mux);
        const resp = await call(mux, session, "OrderService_cancelOrder", { orderId: "o1", reason: null });
        expect(resp.error).toBeUndefined();
        const result = resp.result as { content: Array<{ type: string; text: string }>; isError: boolean };
        expect(result.isError).toBe(false);
        const payload = JSON.parse(result.content[0].text) as { ok: boolean };
        expect(payload.ok).toBe(true);
    });

    test("2 routing -- OrderService_placeOrder decoder failure -> Channel-B Left (isError:true)", async () => {
        // items:null makes placeorder_In_JsonCodec.decode do (null as unknown[]).map -> TypeError,
        // caught by the wiring as { tag:'DecoderFailed' } -> Left -> Channel-B (isError:true).
        const mux = makeMuxer();
        const session = await initedSession(mux);
        const resp = await call(mux, session, "OrderService_placeOrder", { userId: "u1", items: null });
        expect(resp.error).toBeUndefined();
        const result = resp.result as { content: Array<{ type: string; text: string }>; isError: boolean };
        expect(result.isError).toBe(true);
        expect(result.content.length).toBeGreaterThan(0);
        expect(result.content[0].text.length).toBeGreaterThan(0);
    });

    test("3: registering a colliding tool name throws BaboonMcpWiringException{tag:'DuplicateTool'}", () => {
        let thrown: unknown;
        try {
            new AbstractAsyncMcpMuxer<null>(MERGED_INFO, makeUserServer(), makeUserServer());
        } catch (e) {
            thrown = e;
        }
        expect(thrown).toBeInstanceOf(BaboonMcpWiringException);
        expect((thrown as BaboonMcpWiringException).error.tag).toBe("DuplicateTool");
        expect((thrown as BaboonMcpWiringException).error.toolName).toBe("UserService_createUser");
    });

    test("3 also via register(): collision on register throws DuplicateTool", () => {
        const mux = new AbstractAsyncMcpMuxer<null>(MERGED_INFO, makeUserServer());
        expect(() => mux.register(makeUserServer())).toThrowError(BaboonMcpWiringException);
    });

    test("4: unknown tool name -> -32602 'unknown tool' response (NoMatchingTool)", async () => {
        const mux = makeMuxer();
        const session = await initedSession(mux);
        const resp = await call(mux, session, "UserService_nope", {});
        expect(resp.error).toBeDefined();
        expect(resp.result).toBeUndefined();
        expect(resp.error!.code).toBe(-32602);
        expect(resp.error!.message.length).toBeGreaterThan(0);
    });
});
