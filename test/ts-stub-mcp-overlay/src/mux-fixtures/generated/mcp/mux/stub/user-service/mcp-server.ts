import { AbstractBaboonMcpServer, McpServerInfo, McpToolEntry, BaboonEitherResult } from '../../../../BaboonMcpRuntime';
import { BaboonCodecContext, BaboonMethodId } from '../../../../BaboonSharedRuntime';

// Generated MCP server for service `UserService` (model `mcp.mux.stub` v1.0.0).
// Transport-abstract: `handle` is inherited from AbstractBaboonMcpServer and
// performs no I/O. The `invokeJson` delegate routes `tools/call` into the
// generated service dispatch; the integrator supplies it (typically the
// errors-mode `dispatchJson` bound to this service) plus the per-request `Ctx`.
export class UserServiceMcpServer<Ctx> extends AbstractBaboonMcpServer<Ctx> {
    public readonly serverInfo: McpServerInfo = { name: "UserService", version: "1.0.0" };

    public readonly tools: readonly McpToolEntry[] = [
        { name: "UserService_createUser", method: { serviceName: "UserService", methodName: "createUser" }, inputSchema: {"$schema":"https://json-schema.org/draft/2020-12/schema","type":"object","properties":{"email":{"type":"string"},"name":{"type":"string"}},"required":["email","name"]} },
        { name: "UserService_getUser", method: { serviceName: "UserService", methodName: "getUser" }, inputSchema: {"$schema":"https://json-schema.org/draft/2020-12/schema","type":"object","properties":{"userId":{"type":"string"}},"required":["userId"]} },
    ];

    private readonly _invokeJson: (method: BaboonMethodId, data: string, ctx: Ctx, codecCtx: BaboonCodecContext) => BaboonEitherResult;

    constructor(invokeJson: (method: BaboonMethodId, data: string, ctx: Ctx, codecCtx: BaboonCodecContext) => BaboonEitherResult) {
        super();
        this._invokeJson = invokeJson;
    }

    protected invokeJson(method: BaboonMethodId, data: string, ctx: Ctx, codecCtx: BaboonCodecContext): BaboonEitherResult {
        return this._invokeJson(method, data, ctx, codecCtx);
    }
}
