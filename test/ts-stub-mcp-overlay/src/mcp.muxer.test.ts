/**
 * T104 — TypeScript MCP muxer behavioural test (sync + async).
 *
 * Exercises BOTH `AbstractMcpMuxer<Ctx>` and `AbstractAsyncMcpMuxer<Ctx>` by
 * composing two GENERATED `<Service>McpServer` instances produced from the T102
 * `mcp-mux-stub-ok/` model (UserService + OrderService). Composition is done
 * STRICTLY through the public T114 routable surface:
 *   - the muxer ctor takes `IBaboonRoutableMcpServer<Ctx>` (sync) /
 *     `IBaboonRoutableAsyncMcpServer<Ctx>` (async) members,
 *   - the test NEVER subclasses a `<Service>McpServer`,
 *   - the test NEVER calls a member server's own `handle()`.
 *
 * Generated fixtures committed alongside this test:
 *   - sync : ./mux-fixtures/generated/...        (--ts-generate-mcp-server=true)
 *   - async: ./mux-fixtures-async/generated/...  (... + --ts-async-services=true)
 * both from `baboon-compiler/src/test/resources/mcp-mux-stub-ok/`. So this test
 * is self-contained and runnable with the overlay's existing vitest tooling
 * (no on-the-fly codegen, no native binary needed).
 *
 * Four asserted muxer behaviours (T104 acceptance):
 *   1. tools/list  -> UNION of both services' tools in registration-then-declaration order;
 *   2. tools/call  -> routes the flat tool name to the correct owning service --
 *                    proven for a tool of EACH service: Channel-A Right->isError:false
 *                    (UserService_getUser and OrderService_cancelOrder), and a
 *                    Channel-B Left->isError:true (OrderService_placeOrder, decoder fails);
 *   3. register a server with a colliding tool name -> throws
 *      BaboonMcpWiringException{tag:'DuplicateTool'};
 *   4. unknown tool name -> -32602 "unknown tool" response (NoMatchingTool).
 *
 * Assertion discipline: every assertion is an unconditional vitest expect(...)
 * (or an expect(() => ...).toThrow). No `if`-guarded checks.
 */

import { describe, test, expect } from "vitest";

// NB: the sync and async fixtures are TWO independently-generated trees, each
// with its OWN copy of BaboonSharedRuntime / BaboonMcpRuntime. The nominal
// (class-based) types from one tree are NOT assignable to the other (TS sees
// distinct private fields), so the async block imports its runtime/value types
// from the async tree under `Async`-prefixed aliases and never crosses trees.
import { BaboonCodecContext, BaboonMethodId } from "./mux-fixtures/generated/BaboonSharedRuntime";
import {
    JsonRpcRequest,
    JsonRpcResponse,
    McpSession,
    McpServerInfo,
    BaboonEitherResult,
    IBaboonRoutableMcpServer,
    AbstractMcpMuxer,
    BaboonMcpWiringException,
} from "./mux-fixtures/generated/BaboonMcpRuntime";

import {
    BaboonCodecContext as AsyncBaboonCodecContext,
    BaboonMethodId as AsyncBaboonMethodId,
} from "./mux-fixtures-async/generated/BaboonSharedRuntime";
import {
    JsonRpcResponse as AsyncJsonRpcResponse,
    McpSession as AsyncMcpSession,
    McpServerInfo as AsyncMcpServerInfo,
    BaboonEitherResult as AsyncBaboonEitherResult,
    IBaboonRoutableAsyncMcpServer,
    AbstractAsyncMcpMuxer,
    BaboonMcpWiringException as AsyncBaboonMcpWiringException,
} from "./mux-fixtures-async/generated/BaboonMcpRuntime";

// -- Sync generated artefacts --------------------------------------------------
import { BaboonServiceRtDefault as SyncRt } from "./mux-fixtures/generated/mcp/mux/stub/baboon-service-rt";
import { UserService as SyncUserService } from "./mux-fixtures/generated/mcp/mux/stub/user-service/service";
import { invokeJson_UserService as syncInvokeUser } from "./mux-fixtures/generated/mcp/mux/stub/user-service/wiring";
import { UserServiceMcpServer as SyncUserMcpServer } from "./mux-fixtures/generated/mcp/mux/stub/user-service/mcp-server";
import { OrderService as SyncOrderService } from "./mux-fixtures/generated/mcp/mux/stub/order-service/service";
import { invokeJson_OrderService as syncInvokeOrder } from "./mux-fixtures/generated/mcp/mux/stub/order-service/wiring";
import { OrderServiceMcpServer as SyncOrderMcpServer } from "./mux-fixtures/generated/mcp/mux/stub/order-service/mcp-server";

import { UserProfile as SyncUserProfile } from "./mux-fixtures/generated/mcp/mux/stub/UserProfile";
import { UserStatus as SyncUserStatus } from "./mux-fixtures/generated/mcp/mux/stub/UserStatus";
import { Out as SyncGetUserOut } from "./mux-fixtures/generated/mcp/mux/stub/user-service/getuser/out";
import { Out as SyncCreateUserOut } from "./mux-fixtures/generated/mcp/mux/stub/user-service/createuser/out";
import { Out as SyncPlaceOrderOut } from "./mux-fixtures/generated/mcp/mux/stub/order-service/placeorder/out";
import { Out as SyncCancelOrderOut } from "./mux-fixtures/generated/mcp/mux/stub/order-service/cancelorder/out";
import { OrderStatus as SyncOrderStatus } from "./mux-fixtures/generated/mcp/mux/stub/OrderStatus";
import { OrderSummary as SyncOrderSummary } from "./mux-fixtures/generated/mcp/mux/stub/OrderSummary";

// -- Async generated artefacts -------------------------------------------------
import { BaboonServiceRtDefault as AsyncRt } from "./mux-fixtures-async/generated/mcp/mux/stub/baboon-service-rt";
import { UserService as AsyncUserService } from "./mux-fixtures-async/generated/mcp/mux/stub/user-service/service";
import { invokeJson_UserService as asyncInvokeUser } from "./mux-fixtures-async/generated/mcp/mux/stub/user-service/wiring";
import { UserServiceMcpServer as AsyncUserMcpServer } from "./mux-fixtures-async/generated/mcp/mux/stub/user-service/mcp-server";
import { OrderService as AsyncOrderService } from "./mux-fixtures-async/generated/mcp/mux/stub/order-service/service";
import { invokeJson_OrderService as asyncInvokeOrder } from "./mux-fixtures-async/generated/mcp/mux/stub/order-service/wiring";
import { OrderServiceMcpServer as AsyncOrderMcpServer } from "./mux-fixtures-async/generated/mcp/mux/stub/order-service/mcp-server";

import { UserProfile as AsyncUserProfile } from "./mux-fixtures-async/generated/mcp/mux/stub/UserProfile";
import { UserStatus as AsyncUserStatus } from "./mux-fixtures-async/generated/mcp/mux/stub/UserStatus";
import { Out as AsyncGetUserOut } from "./mux-fixtures-async/generated/mcp/mux/stub/user-service/getuser/out";
import { Out as AsyncCreateUserOut } from "./mux-fixtures-async/generated/mcp/mux/stub/user-service/createuser/out";
import { Out as AsyncPlaceOrderOut } from "./mux-fixtures-async/generated/mcp/mux/stub/order-service/placeorder/out";
import { Out as AsyncCancelOrderOut } from "./mux-fixtures-async/generated/mcp/mux/stub/order-service/cancelorder/out";
import { OrderStatus as AsyncOrderStatus } from "./mux-fixtures-async/generated/mcp/mux/stub/OrderStatus";
import { OrderSummary as AsyncOrderSummary } from "./mux-fixtures-async/generated/mcp/mux/stub/OrderSummary";

const codecCtx = BaboonCodecContext.Default;
const asyncCodecCtx = AsyncBaboonCodecContext.Default;

const MERGED_INFO: McpServerInfo = { name: "MergedEndpoint", version: "9.9.9" };
const ASYNC_MERGED_INFO: AsyncMcpServerInfo = { name: "MergedEndpoint", version: "9.9.9" };

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
// A -- SYNC muxer (AbstractMcpMuxer<Ctx>)
// ---------------------------------------------------------------------------

describe("MCP muxer (sync): AbstractMcpMuxer over two generated servers", () => {
    // Stub UserService: getUser returns a profile (Channel-A success path).
    const stubUser: SyncUserService = {
        createUser: () => new SyncCreateUserOut(new SyncUserProfile("u1", "a@b.c", SyncUserStatus.Active)),
        getUser: () => new SyncGetUserOut(new SyncUserProfile("u1", "a@b.c", SyncUserStatus.Active)),
    };
    // Stub OrderService: cancelOrder returns ok=true (Channel-A); placeOrder is
    // never reached because the decoder fails first (Channel-B trigger).
    const stubOrder: SyncOrderService = {
        placeOrder: () => new SyncPlaceOrderOut(new SyncOrderSummary("o1", SyncOrderStatus.Confirmed, 10.0)),
        cancelOrder: () => new SyncCancelOrderOut(true),
    };

    // Delegates: the SAME errors-mode invokeJson the integrator would bind.
    const userDelegate = (m: BaboonMethodId, d: string, _ctx: null, cc: BaboonCodecContext): BaboonEitherResult =>
        syncInvokeUser(m, d, stubUser, SyncRt, cc) as unknown as BaboonEitherResult;
    const orderDelegate = (m: BaboonMethodId, d: string, _ctx: null, cc: BaboonCodecContext): BaboonEitherResult =>
        syncInvokeOrder(m, d, stubOrder, SyncRt, cc) as unknown as BaboonEitherResult;

    // Bind the concrete servers ONLY through the public routable interface --
    // exactly the static type the muxer ctor accepts (no handle(), no subclass).
    function makeUserServer(): IBaboonRoutableMcpServer<null> {
        return new SyncUserMcpServer<null>(userDelegate);
    }
    function makeOrderServer(): IBaboonRoutableMcpServer<null> {
        return new SyncOrderMcpServer<null>(orderDelegate);
    }

    function makeMuxer(): AbstractMcpMuxer<null> {
        return new AbstractMcpMuxer<null>(MERGED_INFO, makeUserServer(), makeOrderServer());
    }

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

    test("1: tools/list returns the UNION in registration-then-declaration order", () => {
        const resp = initAndList(makeMuxer());
        expect(resp.error).toBeUndefined();
        const result = resp.result as { tools: Array<{ name: string }> };
        const names = result.tools.map(t => t.name);
        expect(names).toStrictEqual(EXPECTED_UNION);
    });

    test("1 negative control: union is NOT in interleaved/declaration-only order", () => {
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
        // The text is the JSON-encoded getUser Out -- proving UserService handled it.
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
        // Second UserService server re-declares UserService_createUser / _getUser.
        const dup = makeUserServer();
        let thrown: unknown;
        try {
            new AbstractMcpMuxer<null>(MERGED_INFO, makeUserServer(), dup);
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
        // -32602 surfaces as a JSON-RPC error (NOT a result), same wire bytes a
        // single server would emit for an unknown tool.
        expect(resp.error).toBeDefined();
        expect(resp.result).toBeUndefined();
        expect(resp.error!.code).toBe(-32602);
        expect(resp.error!.message.length).toBeGreaterThan(0);
    });
});

// ---------------------------------------------------------------------------
// B -- ASYNC muxer (AbstractAsyncMcpMuxer<Ctx>)
// ---------------------------------------------------------------------------

describe("MCP muxer (async): AbstractAsyncMcpMuxer over two generated servers", () => {
    const stubUser: AsyncUserService = {
        createUser: async () => new AsyncCreateUserOut(new AsyncUserProfile("u1", "a@b.c", AsyncUserStatus.Active)),
        getUser: async () => new AsyncGetUserOut(new AsyncUserProfile("u1", "a@b.c", AsyncUserStatus.Active)),
    };
    const stubOrder: AsyncOrderService = {
        placeOrder: async () => new AsyncPlaceOrderOut(new AsyncOrderSummary("o1", AsyncOrderStatus.Confirmed, 10.0)),
        cancelOrder: async () => new AsyncCancelOrderOut(true),
    };

    const userDelegate = (m: AsyncBaboonMethodId, d: string, _ctx: null, cc: AsyncBaboonCodecContext): Promise<AsyncBaboonEitherResult> =>
        asyncInvokeUser(m, d, stubUser, AsyncRt, cc) as unknown as Promise<AsyncBaboonEitherResult>;
    const orderDelegate = (m: AsyncBaboonMethodId, d: string, _ctx: null, cc: AsyncBaboonCodecContext): Promise<AsyncBaboonEitherResult> =>
        asyncInvokeOrder(m, d, stubOrder, AsyncRt, cc) as unknown as Promise<AsyncBaboonEitherResult>;

    function makeUserServer(): IBaboonRoutableAsyncMcpServer<null> {
        return new AsyncUserMcpServer<null>(userDelegate);
    }
    function makeOrderServer(): IBaboonRoutableAsyncMcpServer<null> {
        return new AsyncOrderMcpServer<null>(orderDelegate);
    }
    function makeMuxer(): AbstractAsyncMcpMuxer<null> {
        return new AbstractAsyncMcpMuxer<null>(ASYNC_MERGED_INFO, makeUserServer(), makeOrderServer());
    }

    async function initedSession(mux: AbstractAsyncMcpMuxer<null>): Promise<AsyncMcpSession> {
        const session = new AsyncMcpSession();
        await mux.handle(
            { id: 0, method: "initialize", params: { protocolVersion: "2025-06-18", capabilities: {}, clientInfo: { name: "t", version: "0" } } },
            session, null, asyncCodecCtx,
        );
        await mux.handle({ method: "notifications/initialized" }, session, null, asyncCodecCtx);
        return session;
    }

    async function call(mux: AbstractAsyncMcpMuxer<null>, session: AsyncMcpSession, name: string, args: unknown): Promise<AsyncJsonRpcResponse> {
        const resp = await mux.handle({ id: 99, method: "tools/call", params: { name, arguments: args } }, session, null, asyncCodecCtx);
        expect(resp).toBeDefined();
        return resp!;
    }

    test("1: tools/list returns the UNION in registration-then-declaration order", async () => {
        const mux = makeMuxer();
        const session = await initedSession(mux);
        const resp = await mux.handle({ id: 1, method: "tools/list" }, session, null, asyncCodecCtx);
        expect(resp).toBeDefined();
        expect(resp!.error).toBeUndefined();
        const names = (resp!.result as { tools: Array<{ name: string }> }).tools.map(t => t.name);
        expect(names).toStrictEqual(EXPECTED_UNION);
    });

    test("2 routing -- UserService_getUser -> Channel-A Right (isError:false)", async () => {
        const mux = makeMuxer();
        const session = await initedSession(mux);
        const resp = await call(mux, session, "UserService_getUser", { userId: "u1" });
        expect(resp.error).toBeUndefined();
        const result = resp.result as { content: Array<{ type: string; text: string }>; isError: boolean };
        expect(result.isError).toBe(false);
        const payload = JSON.parse(result.content[0].text) as { profile: { userId: string } };
        expect(payload.profile.userId).toBe("u1");
    });

    test("2 routing -- OrderService_cancelOrder -> Channel-A Right (isError:false)", async () => {
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
        const mux = makeMuxer();
        const session = await initedSession(mux);
        const resp = await call(mux, session, "OrderService_placeOrder", { userId: "u1", items: null });
        expect(resp.error).toBeUndefined();
        const result = resp.result as { content: Array<{ type: string; text: string }>; isError: boolean };
        expect(result.isError).toBe(true);
        expect(result.content[0].text.length).toBeGreaterThan(0);
    });

    test("3: registering a colliding tool name throws BaboonMcpWiringException{tag:'DuplicateTool'}", () => {
        let thrown: unknown;
        try {
            new AbstractAsyncMcpMuxer<null>(ASYNC_MERGED_INFO, makeUserServer(), makeUserServer());
        } catch (e) {
            thrown = e;
        }
        expect(thrown).toBeInstanceOf(AsyncBaboonMcpWiringException);
        expect((thrown as AsyncBaboonMcpWiringException).error.tag).toBe("DuplicateTool");
        expect((thrown as AsyncBaboonMcpWiringException).error.toolName).toBe("UserService_createUser");
    });

    test("4: unknown tool name -> -32602 'unknown tool' response (NoMatchingTool)", async () => {
        const mux = makeMuxer();
        const session = await initedSession(mux);
        const resp = await call(mux, session, "OrderService_nope", {});
        expect(resp.error).toBeDefined();
        expect(resp.result).toBeUndefined();
        expect(resp.error!.code).toBe(-32602);
    });
});
