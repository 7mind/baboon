import { AbstractBaboonMcpServer, McpServerInfo, McpToolEntry, BaboonEitherResult } from '../../../../BaboonMcpRuntime';
import { BaboonCodecContext, BaboonMethodId } from '../../../../BaboonSharedRuntime';

// Generated MCP server for service `OrderService` (model `mcp.mux.stub` v1.0.0).
// Transport-abstract: `handle` is inherited from AbstractBaboonMcpServer and
// performs no I/O. The `invokeJson` delegate routes `tools/call` into the
// generated service dispatch; the integrator supplies it (typically the
// errors-mode `dispatchJson` bound to this service) plus the per-request `Ctx`.
export class OrderServiceMcpServer<Ctx> extends AbstractBaboonMcpServer<Ctx> {
    public readonly serverInfo: McpServerInfo = { name: "OrderService", version: "1.0.0" };

    public readonly tools: readonly McpToolEntry[] = [
        { name: "OrderService_placeOrder", method: { serviceName: "OrderService", methodName: "placeOrder" }, inputSchema: {"$schema":"https://json-schema.org/draft/2020-12/schema","type":"object","properties":{"userId":{"type":"string"},"items":{"type":"array","items":{"$ref":"#/$defs/mcp_mux_stub_OrderItem"}}},"required":["userId","items"],"$defs":{"mcp_mux_stub_OrderItem":{"type":"object","properties":{"productId":{"type":"string"},"quantity":{"type":"integer","format":"int32"},"unitPrice":{"type":"number","format":"double"}},"required":["productId","quantity","unitPrice"]}}} },
        { name: "OrderService_cancelOrder", method: { serviceName: "OrderService", methodName: "cancelOrder" }, inputSchema: {"$schema":"https://json-schema.org/draft/2020-12/schema","type":"object","properties":{"orderId":{"type":"string"},"reason":{"oneOf":[{"type":"string"},{"type":"null"}]}},"required":["orderId"]} },
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
