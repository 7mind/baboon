// T178 / D40 — Rust zero-service MCP overlay test (RED baseline).
//
// Purpose: prove the ZERO-SERVICE MCP runtime contract. The zero-service model
// (mcp-stub-zero-services-ok) declares @root types but NO RPC block, so
// servicesOf(domain) is empty and NO per-service `<Service>McpServer` is
// generated. With zero services the ONLY source of the MCP runtime types
// (AbstractMcpMuxer / IBaboonRoutableMcpServer / McpServerInfo / McpSession /
// JsonRpcRequest / JsonRpcResponse / json_rpc_error_codes) is the STATIC runtime
// file `baboon_mcp_server.rs` (and its `pub mod baboon_mcp_server;` declaration).
//
// RED (pre-fix): the current generator emits NO `baboon_mcp_server.rs` for a
// zero-service model, so `baboon_rs_stub::baboon_mcp_server` is an unresolved
// module and THIS FILE FAILS TO COMPILE (E0432 unresolved import / E0433 failed
// to resolve). That compile failure IS the D40 reproduction.
//
// GREEN (post-fix, later task): once the generator emits the static MCP runtime
// even when servicesOf(domain) is empty, this file compiles and the runtime
// assertions below pass — an empty muxer lists zero tools and rejects any
// tools/call with JSON-RPC -32602.
//
// Assertion discipline: `assert!`/`assert_eq!` panic unconditionally on failure.

use baboon_rs_stub::baboon_mcp_server::{
    AbstractMcpMuxer, JsonRpcRequest, McpServerInfo, McpSession, json_rpc_error_codes,
};
use baboon_rs_stub::baboon_runtime::BaboonCodecContext;

// COMPILE-TIME contract: constructing AbstractMcpMuxer<Ctx> with ZERO registered
// servers requires the static runtime module to exist. With zero services there
// is no generated <Service>McpServer to import — these types resolve ONLY from
// baboon_mcp_server.rs.
fn make_empty_muxer() -> AbstractMcpMuxer<()> {
    AbstractMcpMuxer::<()>::new(McpServerInfo {
        name: "ZeroEndpoint",
        version: "1.0.0",
    })
}

fn inited_session(mux: &AbstractMcpMuxer<()>) -> McpSession {
    let ctx = BaboonCodecContext::Default;
    let mut session = McpSession::new();
    mux.handle(
        &JsonRpcRequest {
            id: Some(serde_json::json!(0)),
            method: "initialize".to_string(),
            params: Some(serde_json::json!({
                "protocolVersion": "2025-06-18",
                "capabilities": {},
                "clientInfo": {"name": "t", "version": "0"}
            })),
        },
        &mut session,
        (),
        &ctx,
    );
    mux.handle(
        &JsonRpcRequest {
            id: None,
            method: "notifications/initialized".to_string(),
            params: None,
        },
        &mut session,
        (),
        &ctx,
    );
    session
}

// RUNTIME contract (a): tools/list on an empty muxer returns an empty array.
#[test]
fn zero_services_tools_list_is_empty() {
    let ctx = BaboonCodecContext::Default;
    let mux = make_empty_muxer();
    let mut session = inited_session(&mux);

    let resp = mux
        .handle(
            &JsonRpcRequest {
                id: Some(serde_json::json!(1)),
                method: "tools/list".to_string(),
                params: None,
            },
            &mut session,
            (),
            &ctx,
        )
        .expect("tools/list must return a response");

    assert!(resp.error.is_none(), "tools/list must not return an error");
    let result = resp.result.expect("tools/list must carry a result");
    let tools = result.get("tools").expect("result must carry a tools array");
    let tools = tools.as_array().expect("tools must be an array");
    assert_eq!(tools.len(), 0, "empty muxer MUST list zero tools");
}

// RUNTIME contract (b): an unknown tool call yields JSON-RPC -32602.
#[test]
fn zero_services_unknown_tool_call_code_32602() {
    let ctx = BaboonCodecContext::Default;
    let mux = make_empty_muxer();
    let mut session = inited_session(&mux);

    let resp = mux
        .handle(
            &JsonRpcRequest {
                id: Some(serde_json::json!(2)),
                method: "tools/call".to_string(),
                params: Some(serde_json::json!({
                    "name": "anything_at_all",
                    "arguments": {}
                })),
            },
            &mut session,
            (),
            &ctx,
        )
        .expect("tools/call must return a response");

    assert!(resp.result.is_none(), "no result expected for unknown tool");
    let error = resp
        .error
        .expect("unknown tool on empty muxer MUST produce a Channel-A error");
    assert_eq!(
        error.code,
        json_rpc_error_codes::INVALID_PARAMS,
        "unknown-tool error code MUST be -32602 (InvalidParams)"
    );
    assert!(!error.message.is_empty(), "error.message must be non-empty");
}
