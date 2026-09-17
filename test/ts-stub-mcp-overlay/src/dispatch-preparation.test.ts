import { describe, expect, test } from "vitest";
import { BaboonCodecContext, type BaboonMethodId } from "./baboondefinitions/generated/BaboonSharedRuntime";
import {
    AbstractBaboonMcpServer, AbstractAsyncBaboonMcpServer, AbstractMcpMuxer, AbstractAsyncMcpMuxer,
    McpSession, type BaboonEitherResult, type McpToolEntry,
} from "./baboondefinitions/generated/BaboonMcpRuntime";

function tool(name: string, methodName: string): McpToolEntry {
    return { name, method: { serviceName: "Test", methodName }, inputSchema: { type: "object" } };
}

class SyncServer extends AbstractBaboonMcpServer<null> {
    readonly serverInfo = { name: "test", version: "1" };
    tools = [tool("first", "first"), tool("second", "second")];
    result: BaboonEitherResult = { tag: "Right", value: "ok" };
    readonly calls: string[] = [];
    failure: Error | undefined;
    protected invokeJson(method: BaboonMethodId, data: string): BaboonEitherResult {
        this.calls.push(method.methodName + ":" + data);
        if (this.failure !== undefined) throw this.failure;
        return this.result;
    }
    protected describeWiringError(): string { return "custom description"; }
}

class AsyncServer extends AbstractAsyncBaboonMcpServer<null> {
    constructor(readonly core: SyncServer) { super(); }
    readonly serverInfo = { name: "test", version: "1" };
    get tools(): readonly McpToolEntry[] { return this.core.tools; }
    protected async invokeJson(method: BaboonMethodId, data: string): Promise<BaboonEitherResult> {
        return this.core.routeToolCall(method, data, null, BaboonCodecContext.Default);
    }
    protected describeWiringError(): string { return "custom description"; }
}

describe.each([false, true])("MCP async=%s", asyncMode => {
    test.each([false, true])("preparation, sessions and channels; mux=%s", async mux => {
        const core = new SyncServer();
        const service = asyncMode ? new AsyncServer(core) : core;
        const endpoint = mux
            ? (asyncMode ? new AbstractAsyncMcpMuxer(core.serverInfo, new AsyncServer(core)) : new AbstractMcpMuxer(core.serverInfo, core))
            : service;
        const session = new McpSession();
        const invoke = (method: string, params: unknown, current = session) => endpoint.handle({ id: 1, method, params }, current, null, BaboonCodecContext.Default);
        expect(await invoke("tools/list", {})).toMatchObject({ error: { code: -32600 } });
        expect(await invoke("initialize", {})).toMatchObject({ error: { code: -32602 } });
        expect(session.initialized).toBe(false);
        expect(await invoke("initialize", { protocolVersion: "test" })).toMatchObject({ result: { protocolVersion: "2025-06-18" } });
        expect(await invoke("tools/list", {}, new McpSession())).toMatchObject({ error: { code: -32600 } });
        expect(await invoke("notifications/initialized", {})).toBeUndefined();
        expect(await invoke("tools/list", {})).toMatchObject({ result: { tools: [{ name: "first" }, { name: "second" }] } });
        const circular: { self?: unknown } = {};
        circular.self = circular;
        expect(await invoke("tools/call", { name: "missing", arguments: circular })).toMatchObject({ error: { code: -32602 } });
        expect(await invoke("tools/call", { name: "first", arguments: null })).toMatchObject({ result: { isError: false } });
        expect(core.calls).toEqual(["first:{}"]);
        core.result = { tag: "Left", value: { tag: "NoMatchingMethod", method: { serviceName: "Test", methodName: "first" } } };
        expect(await invoke("tools/call", { name: "first" })).toMatchObject({ result: { isError: true } });
        expect(await invoke("missing", {})).toMatchObject({ error: { code: -32601 } });
    });

    test("external tools remain dynamic and duplicate names retain last-entry routing", async () => {
        const core = new SyncServer();
        const server = asyncMode ? new AsyncServer(core) : core;
        const session = new McpSession(); session.initialized = true;
        core.tools.push(tool("first", "last"));
        await server.handle({ method: "tools/call", params: { name: "first" } }, session, null, BaboonCodecContext.Default);
        expect(core.calls).toEqual(["last:{}"]);
        core.tools = [];
        expect(await server.handle({ method: "tools/call", params: { name: "first" } }, session, null, BaboonCodecContext.Default))
            .toMatchObject({ error: { code: -32602 } });
    });

    test("duplicate registration preserves the existing partial-registration contract", async () => {
        const core = new SyncServer();
        const extra = new SyncServer(); extra.tools = [tool("third", "third"), tool("first", "duplicate")];
        const mux = asyncMode ? new AbstractAsyncMcpMuxer(core.serverInfo, new AsyncServer(core)) : new AbstractMcpMuxer(core.serverInfo, core);
        if (mux instanceof AbstractAsyncMcpMuxer) expect(() => mux.register(new AsyncServer(extra))).toThrow();
        else expect(() => mux.register(extra)).toThrow();
        const session = new McpSession(); session.initialized = true;
        await mux.handle({ method: "tools/call", params: { name: "third" } }, session, null, BaboonCodecContext.Default);
        expect(extra.calls).toEqual(["third:{}"]);
    });
});

test("invocation exceptions stay synchronous or become Promise rejections according to the public interface", async () => {
    const core = new SyncServer(); core.failure = new Error("delegate failure");
    const session = new McpSession(); session.initialized = true;
    const request = { method: "tools/call", params: { name: "first" } };
    expect(() => core.handle(request, session, null, BaboonCodecContext.Default)).toThrow("delegate failure");
    await expect(new AsyncServer(core).handle(request, session, null, BaboonCodecContext.Default)).rejects.toThrow("delegate failure");
});
