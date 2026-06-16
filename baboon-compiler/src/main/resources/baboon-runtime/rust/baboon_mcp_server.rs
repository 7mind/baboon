// Additive MCP server runtime (decisions ledger M1; contract:
// docs/research/mcp-dispatch-runtime-contract.md). Emitted only when
// `--rust-generate-mcp-server=true`; the service-wiring runtime in
// baboon_service_wiring.rs is unchanged. These types are STATIC (no per-model
// templating) — the only per-model code is the generated `<Service>McpServer<Ctx>`
// and its tool-registry literals.
//
// The MCP dispatch surface is transport-abstract: it implements the JSON-RPC
// method state machine + tool dispatch ONLY. Bytes-in/bytes-out (stdio framing,
// Streamable-HTTP bodies) is an injected adapter the generated surface never
// contains — mirroring the abstract-context service contract, which supplies
// `Ctx` per invocation rather than baking an I/O loop into the wrapper.

use crate::baboon_runtime::BaboonCodecContext;
use crate::baboon_service_wiring::{BaboonMethodId, BaboonWiringError};

// --- JSON-RPC value types (already parsed from bytes by the adapter) ---
//
// `JsonRpcId` is `serde_json::Value` (null / string / number per JSON-RPC 2.0).
// A notification carries no `id`; the server produces no response for it.
// `params`/`result`/`error.data` are `serde_json::Value` — the MCP runtime
// reuses what the JSON codecs already speak rather than introducing a new model.

pub type JsonRpcId = serde_json::Value;

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct JsonRpcRequest {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub id: Option<JsonRpcId>,
    pub method: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub params: Option<serde_json::Value>,
}

#[derive(Debug, Clone, serde::Serialize)]
pub struct JsonRpcError {
    pub code: i32,
    pub message: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub data: Option<serde_json::Value>,
}

#[derive(Debug, Clone, serde::Serialize)]
pub struct JsonRpcResponse {
    pub id: Option<JsonRpcId>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub result: Option<serde_json::Value>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub error: Option<JsonRpcError>,
}

// JSON-RPC / MCP protocol constants (wire contract K4).
pub const MCP_PROTOCOL_VERSION: &str = "2025-06-18";

pub mod json_rpc_error_codes {
    pub const PARSE_ERROR: i32      = -32700;
    pub const INVALID_REQUEST: i32  = -32600;
    pub const METHOD_NOT_FOUND: i32 = -32601;
    pub const INVALID_PARAMS: i32   = -32602;
    pub const INTERNAL_ERROR: i32   = -32603;
}

// --- Per-connection state (adapter-owned) ---
//
// The "initialized" precondition (reject `tools/*` before a successful
// `initialize`) is per-connection state; a connection is a transport concept.
// The latch therefore lives in this tiny value the adapter creates per
// connection, NOT as ambient mutable state inside the server object (which stays
// immutable and shareable across concurrent connections).
pub struct McpSession {
    pub initialized: bool,
}

impl McpSession {
    pub fn new() -> Self {
        McpSession { initialized: false }
    }
}

impl Default for McpSession {
    fn default() -> Self { Self::new() }
}

// --- Tool registry ---
//
// One entry per Baboon method bound to a server. `input_schema` is the
// precomputed, self-contained JSON Schema (from the shared T5 emitter), carried
// as a constant value — the runtime does not compute schemas.
pub struct McpToolEntry {
    pub name: &'static str,
    pub method: BaboonMethodId,
    pub description: Option<&'static str>,
    pub input_schema: serde_json::Value,
}

pub struct McpServerInfo {
    pub name: &'static str,
    pub version: &'static str,
}

// --- Dispatch interface ---
//
// The single generated entrypoint. It is NOT R-parametric (an MCP response
// is always a `JsonRpcResponse` value); the only free type parameter is the
// caller's `Ctx` — the SAME `Ctx` the service-wiring contract threads. `handle`
// is synchronous and performs no I/O. It returns `None` for an accepted
// notification (no reply).
pub trait IBaboonMcpServer<Ctx> {
    fn handle(
        &self,
        request: &JsonRpcRequest,
        session: &mut McpSession,
        ctx: Ctx,
        codec_ctx: &BaboonCodecContext,
    ) -> Option<JsonRpcResponse>;
}

// --- PUBLIC routable-server surface (tasks:T114) ---
//
// The composition seam the cross-service MCP muxer (AbstractMcpMuxer) depends on.
// `BaboonMcpServerBase` already exposes `server_info`/`tools` as public fields,
// but the per-server routable dispatch (the `invoke_json` closure the generated
// struct owns) was not reachable behind a trait. This trait promotes the three
// composition inputs to a stable PUBLIC surface implemented BY THE GENERATED
// `<Service>McpServer<Ctx>` struct (which holds both the base and the closure),
// so the muxer depends on the trait, never on the concrete struct, and never on
// `handle`. `route_tool_call` is the public name for the per-service dispatch
// entry `handle` drives for its own `tools/call` arm; in async mode the
// generated impl drives the future to completion (the `block_on` bridge already
// used by `handle`), returning the same sync result container so Channel-A/B
// mapping is reused unchanged.
pub trait IBaboonRoutableMcpServer<Ctx> {
    fn server_info(&self) -> &McpServerInfo;
    fn tools(&self) -> &[McpToolEntry];
    fn route_tool_call(
        &self,
        method: &BaboonMethodId,
        data: &str,
        ctx: Ctx,
        codec_ctx: &BaboonCodecContext,
    ) -> Result<String, BaboonWiringError>;
}

// The JSON `tools/call` delegate the generated server supplies: it routes one
// tool invocation into the already-generated service dispatch (the errors-mode
// `invoke_json_*`, which returns `Result<String, BaboonWiringError>`). The MCP
// layer turns that result into Channel-A / Channel-B per the wire contract (K4
// §3). The codecs are reached exclusively through this delegate; the MCP
// runtime holds no codec logic itself.
pub type McpJsonInvoke<Ctx> =
    Box<dyn Fn(&BaboonMethodId, &str, Ctx, &BaboonCodecContext) -> Result<String, BaboonWiringError>>;

// --- Transport-abstract dispatch base ---
//
// Shared `handle` state machine. The generated `<Service>McpServer<Ctx>` struct
// embeds this helper and delegates to it, supplying its fixed `server_info`,
// ordered tool registry, and `invoke_json` callback. All JSON-RPC method strings
// ("tools/list" …) and result keys ("protocolVersion", "inputSchema" …) are
// literal lowercase strings, NOT subject to any per-language symbol casing.
pub struct BaboonMcpServerBase {
    pub server_info: McpServerInfo,
    pub tools: Vec<McpToolEntry>,
}

impl BaboonMcpServerBase {
    fn by_name(&self, name: &str) -> Option<&McpToolEntry> {
        self.tools.iter().find(|t| t.name == name)
    }

    pub fn handle_request<Ctx: Clone>(
        &self,
        request: &JsonRpcRequest,
        session: &mut McpSession,
        ctx: Ctx,
        codec_ctx: &BaboonCodecContext,
        invoke_json: &dyn Fn(&BaboonMethodId, &str, Ctx, &BaboonCodecContext) -> Result<String, BaboonWiringError>,
    ) -> Option<JsonRpcResponse> {
        let id = request.id.clone();
        match request.method.as_str() {
            "initialize" => {
                let has_pv = request.params.as_ref()
                    .and_then(|p| p.get("protocolVersion"))
                    .is_some();
                if !has_pv {
                    return Some(self.error_response(id, json_rpc_error_codes::INVALID_PARAMS, "initialize: missing protocolVersion"));
                }
                session.initialized = true;
                Some(JsonRpcResponse {
                    id,
                    result: Some(serde_json::json!({
                        "protocolVersion": MCP_PROTOCOL_VERSION,
                        "capabilities": { "tools": {} },
                        "serverInfo": { "name": self.server_info.name, "version": self.server_info.version }
                    })),
                    error: None,
                })
            }
            "notifications/initialized" => None,
            "tools/list" => {
                if !session.initialized {
                    return Some(self.error_response(id, json_rpc_error_codes::INVALID_REQUEST, "tools/list before initialize"));
                }
                let tools: Vec<serde_json::Value> = self.tools.iter().map(|t| {
                    let mut entry = serde_json::json!({
                        "name": t.name,
                        "inputSchema": t.input_schema.clone(),
                    });
                    if let Some(desc) = t.description {
                        entry["description"] = serde_json::Value::String(desc.to_string());
                    }
                    entry
                }).collect();
                Some(JsonRpcResponse {
                    id,
                    result: Some(serde_json::json!({ "tools": tools })),
                    error: None,
                })
            }
            "tools/call" => {
                if !session.initialized {
                    return Some(self.error_response(id, json_rpc_error_codes::INVALID_REQUEST, "tools/call before initialize"));
                }
                let name = request.params.as_ref()
                    .and_then(|p| p.get("name"))
                    .and_then(|v| v.as_str());
                let name = match name {
                    Some(n) => n,
                    None => {
                        return Some(self.error_response(id, json_rpc_error_codes::INVALID_PARAMS, "tools/call: missing tool name"));
                    }
                };
                let entry = match self.by_name(name) {
                    Some(e) => e,
                    None => {
                        return Some(self.error_response(
                            id,
                            json_rpc_error_codes::INVALID_PARAMS,
                            &format!("tools/call: unknown tool '{}'", name),
                        ));
                    }
                };
                let args = request.params.as_ref()
                    .and_then(|p| p.get("arguments"))
                    .cloned()
                    .unwrap_or_else(|| serde_json::json!({}));
                let args_json = match serde_json::to_string(&args) {
                    Ok(s) => s,
                    Err(e) => {
                        return Some(self.error_response(
                            id,
                            json_rpc_error_codes::INTERNAL_ERROR,
                            &format!("tools/call: failed to serialize arguments: {}", e),
                        ));
                    }
                };
                let result = invoke_json(&entry.method, &args_json, ctx, codec_ctx);
                match result {
                    Ok(json_str) => {
                        Some(JsonRpcResponse {
                            id,
                            result: Some(serde_json::json!({
                                "content": [{ "type": "text", "text": json_str }],
                                "isError": false
                            })),
                            error: None,
                        })
                    }
                    Err(wiring_err) => {
                        // Channel B: a valid protocol call whose domain payload failed.
                        Some(JsonRpcResponse {
                            id,
                            result: Some(serde_json::json!({
                                "content": [{ "type": "text", "text": wiring_err.to_string() }],
                                "isError": true
                            })),
                            error: None,
                        })
                    }
                }
            }
            _ => Some(self.error_response(
                id,
                json_rpc_error_codes::METHOD_NOT_FOUND,
                &format!("Method not found: {}", request.method),
            )),
        }
    }

    fn error_response(&self, id: Option<JsonRpcId>, code: i32, message: &str) -> JsonRpcResponse {
        JsonRpcResponse {
            id,
            result: None,
            error: Some(JsonRpcError {
                code,
                message: message.to_string(),
                data: None,
            }),
        }
    }
}
