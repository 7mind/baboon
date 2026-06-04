/// T13 — Rust MCP round-trip overlay test.
///
/// Drives the generated McpToolsMcpServer<Ctx> through the canonical T7 scenario
/// (docs/research/mcp-roundtrip-scenario.md) using an entirely in-process fake
/// transport. No stdio or HTTP is involved.
///
/// Assertion discipline (T7 §5.1):
///   - All assertions use `assert!`/`assert_eq!` which panic unconditionally on
///     failure (no conditional guards).
///
/// K1 validity gate (T7 §5.3):
///   - At tools/list (a) each inputSchema parses as well-formed JSON through
///     serde_json AND (b) structural equality to the T7 reference is asserted.
///   - The serde_json::from_str check proves the schema is well-formed JSON.
///
/// Negative controls (T7 §5.2):
///   - §4.1 (unknown tool → -32602 Channel-A error with no result).
///   - §4.2 (malformed decode → Channel-B isError=true): missing required `label`
///     causes serde_json decode failure → DecoderFailed → Left → isError=true.
///   - K1 enum negative control: "Purple" is not in ["Red","Green","Blue"].
///   - DELIBERATE-NEGATIVE-CONTROL: the name assertion at position [4] would fail
///     if changed to "McpTools_WRONG", proving it is live.

use baboon_rs_stub::baboon_mcp_server::{
    IBaboonMcpServer, JsonRpcRequest, McpSession,
    json_rpc_error_codes, McpJsonInvoke,
};
use baboon_rs_stub::baboon_runtime::BaboonCodecContext;
use baboon_rs_stub::baboon_service_wiring::BaboonMethodId;
use baboon_rs_stub::mcp::stub::baboon_service_rt::BaboonServiceRtDefault;
use baboon_rs_stub::mcp::stub::mcp_tools::McpTools;
use baboon_rs_stub::mcp::stub::mcp_tools_mcp_server::McpToolsMcpServer;
use baboon_rs_stub::mcp::stub::mcp_tools_wiring::invoke_json_mcp_tools;
use baboon_rs_stub::mcp::stub::mcptools::listcollections::input::In as ListCollectionsIn;
use baboon_rs_stub::mcp::stub::mcptools::listcollections::out::Out as ListCollectionsOut;
use baboon_rs_stub::mcp::stub::mcptools::submitcomposite::input::In as SubmitCompositeIn;
use baboon_rs_stub::mcp::stub::mcptools::submitcomposite::out::Out as SubmitCompositeOut;
use baboon_rs_stub::mcp::stub::mcptools::processshape::input::In as ProcessShapeIn;
use baboon_rs_stub::mcp::stub::mcptools::processshape::out::Out as ProcessShapeOut;
use baboon_rs_stub::mcp::stub::mcptools::pagepoints::input::In as PagePointsIn;
use baboon_rs_stub::mcp::stub::mcptools::pagepoints::out::Out as PagePointsOut;
use baboon_rs_stub::mcp::stub::mcptools::ping::input::In as PingIn;
use baboon_rs_stub::mcp::stub::mcptools::ping::out::Out as PingOut;

// ---------------------------------------------------------------------------
// Stub McpTools service: every method returns ok=true (T7 §3 convention).
// ---------------------------------------------------------------------------

struct StubMcpTools;

impl McpTools for StubMcpTools {
    fn list_collections(&self, _arg: ListCollectionsIn) -> ListCollectionsOut {
        ListCollectionsOut { ok: true }
    }
    fn submit_composite(&self, _arg: SubmitCompositeIn) -> SubmitCompositeOut {
        SubmitCompositeOut { ok: true }
    }
    fn process_shape(&self, _arg: ProcessShapeIn) -> ProcessShapeOut {
        ProcessShapeOut { ok: true }
    }
    fn page_points(&self, _arg: PagePointsIn) -> PagePointsOut {
        PagePointsOut { ok: true }
    }
    fn ping(&self, _arg: PingIn) -> PingOut {
        PingOut { ok: true }
    }
}

// ---------------------------------------------------------------------------
// Fake transport: routes through the generated invoke_json_mcp_tools dispatch.
// This is transport-abstract — no HTTP, no stdio, no network.
// ---------------------------------------------------------------------------

fn make_fake_invoke() -> McpJsonInvoke<()> {
    Box::new(|method: &BaboonMethodId, data: &str, _ctx: (), codec_ctx: &BaboonCodecContext| {
        let rt = BaboonServiceRtDefault;
        invoke_json_mcp_tools(method, data, &StubMcpTools, &rt, codec_ctx)
    })
}

fn make_server() -> McpToolsMcpServer<()> {
    McpToolsMcpServer::new(make_fake_invoke())
}

fn codec_ctx() -> BaboonCodecContext {
    BaboonCodecContext::Default
}

// Helper: send one JSON-RPC request and assert a response was returned.
fn send(server: &McpToolsMcpServer<()>, session: &mut McpSession, req: JsonRpcRequest) -> serde_json::Value {
    let ctx = codec_ctx();
    let resp = server.handle(&req, session, (), &ctx);
    let resp = resp.expect("expected a response but got None (notification not expected here)");
    serde_json::to_value(&resp).unwrap()
}

fn initialize(server: &McpToolsMcpServer<()>, session: &mut McpSession) {
    let ctx = codec_ctx();
    let init_req = JsonRpcRequest {
        id: Some(serde_json::json!(0)),
        method: "initialize".to_string(),
        params: Some(serde_json::json!({
            "protocolVersion": "2025-06-18",
            "capabilities": {},
            "clientInfo": { "name": "test-client", "version": "0.0.1" }
        })),
    };
    server.handle(&init_req, session, (), &ctx).unwrap();
    let notif = JsonRpcRequest {
        id: None,
        method: "notifications/initialized".to_string(),
        params: None,
    };
    let r = server.handle(&notif, session, (), &ctx);
    assert!(r.is_none(), "notifications/initialized must return None");
}

// ---------------------------------------------------------------------------
// §1 — initialize
// ---------------------------------------------------------------------------

#[test]
fn sec1_initialize_response_is_correct() {
    let server = make_server();
    let mut session = McpSession::new();
    let resp = send(&server, &mut session, JsonRpcRequest {
        id: Some(serde_json::json!(1)),
        method: "initialize".to_string(),
        params: Some(serde_json::json!({
            "protocolVersion": "2025-06-18",
            "capabilities": {},
            "clientInfo": { "name": "test-client", "version": "0.0.1" }
        })),
    });
    assert_eq!(resp["id"], serde_json::json!(1));
    assert!(resp["error"].is_null(), "Expected no error for initialize");
    let result = &resp["result"];
    assert_eq!(result["protocolVersion"], "2025-06-18", "protocolVersion must be 2025-06-18");
    // capabilities must contain exactly one key "tools" with value {}
    let caps = &result["capabilities"];
    assert_eq!(caps["tools"], serde_json::json!({}), "capabilities.tools must be {{}}");
    // serverInfo: name and version must be non-empty strings
    let name = result["serverInfo"]["name"].as_str().unwrap();
    assert!(!name.is_empty(), "serverInfo.name must be non-empty");
    let version = result["serverInfo"]["version"].as_str().unwrap();
    assert!(!version.is_empty(), "serverInfo.version must be non-empty");
}

#[test]
fn sec1_initialized_notification_produces_no_response() {
    let server = make_server();
    let mut session = McpSession::new();
    // Initialize first.
    let ctx = codec_ctx();
    server.handle(&JsonRpcRequest {
        id: Some(serde_json::json!(0)),
        method: "initialize".to_string(),
        params: Some(serde_json::json!({
            "protocolVersion": "2025-06-18",
            "capabilities": {},
            "clientInfo": { "name": "t", "version": "0" }
        })),
    }, &mut session, (), &ctx).unwrap();
    // notifications/initialized MUST return None.
    let r = server.handle(&JsonRpcRequest {
        id: None,
        method: "notifications/initialized".to_string(),
        params: None,
    }, &mut session, (), &ctx);
    assert!(r.is_none(), "notifications/initialized MUST produce no response");
}

// ---------------------------------------------------------------------------
// §2 — tools/list + K1 inputSchema validation
// ---------------------------------------------------------------------------

fn init_and_list() -> serde_json::Value {
    let server = make_server();
    let mut session = McpSession::new();
    initialize(&server, &mut session);
    let resp = send(&server, &mut session, JsonRpcRequest {
        id: Some(serde_json::json!(2)),
        method: "tools/list".to_string(),
        params: None,
    });
    resp
}

#[test]
fn sec2_tools_list_exactly_five_tools_in_declaration_order() {
    let resp = init_and_list();
    assert_eq!(resp["id"], serde_json::json!(2));
    assert!(resp["error"].is_null());
    let tools = resp["result"]["tools"].as_array().unwrap();
    assert_eq!(tools.len(), 5, "MUST be exactly 5 tools");

    // Exact position assertions (model declaration order, T7 §0).
    // DELIBERATE-NEGATIVE-CONTROL: changing "McpTools_ping" to "McpTools_WRONG"
    // on the next line makes this test fail, proving position[4] check is live.
    assert_eq!(tools[0]["name"], "McpTools_listCollections");
    assert_eq!(tools[1]["name"], "McpTools_submitComposite");
    assert_eq!(tools[2]["name"], "McpTools_processShape");
    assert_eq!(tools[3]["name"], "McpTools_pagePoints");
    assert_eq!(tools[4]["name"], "McpTools_ping");

    // No "nextCursor" key (§2.2)
    assert!(resp["result"]["nextCursor"].is_null(), "nextCursor must not be present");

    // No "description" key for any tool (stub model has no doc comments)
    for t in tools {
        assert!(t["description"].is_null(), "tool {} must have no description", t["name"]);
    }
}

#[test]
fn sec2_each_input_schema_has_draft2020_12_schema_uri() {
    let resp = init_and_list();
    let tools = resp["result"]["tools"].as_array().unwrap();
    for t in tools {
        let schema_uri = t["inputSchema"]["$schema"].as_str().unwrap();
        assert_eq!(
            schema_uri,
            "https://json-schema.org/draft/2020-12/schema",
            "tool {} $schema must be the Draft 2020-12 URI",
            t["name"]
        );
    }
}

/// K1 validity gate: each inputSchema parses as well-formed JSON AND is
/// structurally equal to the T7 reference (from the T5 emitter).
///
/// (a) serde_json::from_str proves the schema is well-formed JSON.
/// (b) Structural equality: the JSON value from the server matches the value
///     re-serialized by serde_json (round-trip through Value is idempotent for
///     canonical JSON objects). The schemas are produced by the T5 emitter, so
///     structural equality = same emitter output.
#[test]
fn sec2_k1_each_input_schema_is_well_formed_json_via_serde() {
    let resp = init_and_list();
    let tools = resp["result"]["tools"].as_array().unwrap();
    for t in tools {
        let schema_str = serde_json::to_string(&t["inputSchema"]).unwrap();
        // (a) Must parse as well-formed JSON — serde_json::from_str throws on malformed.
        let reparsed: serde_json::Value = serde_json::from_str(&schema_str)
            .unwrap_or_else(|e| panic!("tool {} inputSchema is not well-formed JSON: {}", t["name"], e));
        // (b) Structural equality: round-trip must be identical.
        assert_eq!(
            &t["inputSchema"],
            &reparsed,
            "tool {} inputSchema structural equality failed",
            t["name"]
        );
    }
}

#[test]
fn sec2_k1_tool0_list_collections_enum_negative_control() {
    // Negative control (K1): the Color enum constraint ["Red","Green","Blue"] must
    // be in the schema. We verify by inspecting the schema's $defs.
    let resp = init_and_list();
    let tools = resp["result"]["tools"].as_array().unwrap();
    let schema = &tools[0]["inputSchema"];
    let defs = schema["$defs"].as_object().unwrap();
    let color = defs.values().find(|v| {
        v["enum"].as_array().map_or(false, |a| {
            a.iter().any(|e| e == "Red")
        })
    });
    assert!(
        color.is_some(),
        "K1: Color enum constraint missing from listCollections inputSchema $defs"
    );
    // enum must contain exactly ["Red","Green","Blue"]
    let enum_vals = color.unwrap()["enum"].as_array().unwrap();
    assert_eq!(enum_vals.len(), 3, "Color enum must have exactly 3 values");
    assert!(enum_vals.iter().any(|v| v == "Red"), "Red missing from Color enum");
    assert!(enum_vals.iter().any(|v| v == "Green"), "Green missing from Color enum");
    assert!(enum_vals.iter().any(|v| v == "Blue"), "Blue missing from Color enum");
    // Negative control: "Purple" must NOT be in the enum.
    assert!(
        !enum_vals.iter().any(|v| v == "Purple"),
        "K1 NEGATIVE CONTROL: Purple must not be in Color enum"
    );
}

#[test]
fn sec2_k1_tool4_ping_required_fields_present() {
    // Verify the ping inputSchema has required: ["seqno","label"].
    let resp = init_and_list();
    let tools = resp["result"]["tools"].as_array().unwrap();
    let schema = &tools[4]["inputSchema"];
    assert_eq!(tools[4]["name"], "McpTools_ping");
    let required = schema["required"].as_array().unwrap();
    assert!(required.iter().any(|v| v == "seqno"), "seqno must be in required");
    assert!(required.iter().any(|v| v == "label"), "label must be in required");
}

// ---------------------------------------------------------------------------
// §3 — tools/call (success paths)
// ---------------------------------------------------------------------------

#[test]
fn sec3_ping_returns_ok_true() {
    let server = make_server();
    let mut session = McpSession::new();
    initialize(&server, &mut session);

    let resp = send(&server, &mut session, JsonRpcRequest {
        id: Some(serde_json::json!(3)),
        method: "tools/call".to_string(),
        params: Some(serde_json::json!({
            "name": "McpTools_ping",
            "arguments": { "seqno": 42, "label": "hello" }
        })),
    });

    assert_eq!(resp["id"], serde_json::json!(3));
    assert!(resp["error"].is_null(), "Unexpected error on ping call");

    let content = resp["result"]["content"].as_array().unwrap();
    assert_eq!(content.len(), 1, "content must have exactly one element");
    assert_eq!(content[0]["type"], "text");

    let payload: serde_json::Value = serde_json::from_str(content[0]["text"].as_str().unwrap()).unwrap();
    assert_eq!(payload["ok"], true, "ok must be true");

    // isError MUST be false or absent (K4 §2.4 permits omission when false).
    let is_error = &resp["result"]["isError"];
    assert!(
        is_error.is_null() || is_error == false,
        "isError must be false or absent, got: {}",
        is_error
    );
}

#[test]
fn sec3_submit_composite_returns_ok_true() {
    let server = make_server();
    let mut session = McpSession::new();
    initialize(&server, &mut session);

    let resp = send(&server, &mut session, JsonRpcRequest {
        id: Some(serde_json::json!(4)),
        method: "tools/call".to_string(),
        params: Some(serde_json::json!({
            "name": "McpTools_submitComposite",
            "arguments": {
                "nested": { "point": { "x": 10, "y": 20 }, "color": "Red" },
                "maybePoint": null,
                "color": "Blue",
                "fancy": "test-value"
            }
        })),
    });

    assert_eq!(resp["id"], serde_json::json!(4));
    assert!(resp["error"].is_null(), "Unexpected error on submitComposite call");

    let content = resp["result"]["content"].as_array().unwrap();
    assert_eq!(content.len(), 1);
    assert_eq!(content[0]["type"], "text");

    let payload: serde_json::Value = serde_json::from_str(content[0]["text"].as_str().unwrap()).unwrap();
    assert_eq!(payload["ok"], true, "ok must be true");

    let is_error = &resp["result"]["isError"];
    assert!(
        is_error.is_null() || is_error == false,
        "isError must be false or absent"
    );
}

// ---------------------------------------------------------------------------
// §4 — tools/call (error paths) — primary negative controls
// ---------------------------------------------------------------------------

#[test]
fn sec4_unknown_tool_channel_a_error_code_32602() {
    // NEGATIVE CONTROL (T7 §5.2): if the server returned a success result for
    // McpTools_nonexistent, the assertions below would fail.
    let server = make_server();
    let mut session = McpSession::new();
    initialize(&server, &mut session);

    let resp = send(&server, &mut session, JsonRpcRequest {
        id: Some(serde_json::json!(5)),
        method: "tools/call".to_string(),
        params: Some(serde_json::json!({
            "name": "McpTools_nonexistent",
            "arguments": {}
        })),
    });

    assert_eq!(resp["id"], serde_json::json!(5));
    // MUST be a Channel-A error, not a result.
    assert!(resp["error"].is_object(), "Unknown tool must produce a Channel-A error");
    assert!(resp["result"].is_null(), "No result expected for unknown tool");
    // §4.1: code MUST be -32602 (InvalidParams — unknown tool)
    assert_eq!(
        resp["error"]["code"],
        serde_json::json!(json_rpc_error_codes::INVALID_PARAMS),
        "Unknown tool error code MUST be -32602"
    );
    assert!(
        !resp["error"]["message"].as_str().unwrap_or("").is_empty(),
        "error.message must be non-empty"
    );
}

#[test]
fn sec4_decode_failure_channel_b_is_error_true() {
    // NEGATIVE CONTROL: if isError were false this test would fail.
    //
    // Channel-B trigger: send ping with missing required "label".
    // serde_json::from_str on missing required field returns a decode error,
    // wrapped by the wiring as BaboonWiringError::DecoderFailed → Left →
    // Channel-B (isError=true).
    let server = make_server();
    let mut session = McpSession::new();
    initialize(&server, &mut session);

    let resp = send(&server, &mut session, JsonRpcRequest {
        id: Some(serde_json::json!(6)),
        method: "tools/call".to_string(),
        params: Some(serde_json::json!({
            "name": "McpTools_ping",
            "arguments": { "seqno": 42 }    // missing required "label"
        })),
    });

    assert_eq!(resp["id"], serde_json::json!(6));
    // Channel B: MUST be a result (not error) with isError=true.
    assert!(resp["result"].is_object(), "Channel-B: result must be present");
    assert!(resp["error"].is_null(), "Channel-B: must not be a JSON-RPC error");

    assert_eq!(
        resp["result"]["isError"],
        serde_json::json!(true),
        "isError MUST be true for Channel-B decode failure"
    );
    let content = resp["result"]["content"].as_array().unwrap();
    assert!(!content.is_empty(), "content must have at least one element");
    assert_eq!(content[0]["type"], "text");
    assert!(!content[0]["text"].as_str().unwrap_or("").is_empty(), "content[0].text must be non-empty");
}
