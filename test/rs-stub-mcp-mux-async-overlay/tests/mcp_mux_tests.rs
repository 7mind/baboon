/// T109 — Rust MCP muxer round-trip overlay test.
///
/// Exercises `AbstractMcpMuxer<Ctx>` by composing two FRESHLY GENERATED
/// `<Service>McpServer` instances produced from the T102 `mcp-mux-stub-ok/`
/// model (UserService + OrderService). Composition is done strictly through
/// the public T114 routable surface:
///   - the muxer's `register`/`with` take `Box<dyn IBaboonRoutableMcpServer<Ctx>>`,
///   - the test NEVER reaches into a `<Service>McpServer`'s private members,
///   - the test NEVER calls a member server's own `handle()`.
///
/// Generated code lands in `src/` (the isolated dir set by the
/// `test-gen-rs-mcp-mux` mdl action). No committed generated fixtures.
///
/// This is the ASYNC sibling of `rs-stub-mcp-mux-overlay`. In async mode
/// (`--rs-async-services=true`) the generated service trait methods are
/// `async fn` (AFIT, `?Send`) and `invoke_json_<svc>` is itself `async`, so the
/// `McpJsonInvoke<Ctx>` delegate alias is the FUTURE-RETURNING form — the
/// delegate factory therefore `Box::pin`s the invoke future. The muxer itself is
/// byte-identical to the sync lane: the generated `route_tool_call` drives its
/// invoke future to completion via the `block_on` bridge (D30/T98) and returns
/// the SAME sync `Result<String, BaboonWiringError>` container, so the muxer
/// surface and every assertion below are identical to the sync lane.
///
/// Four asserted muxer behaviours (T109 acceptance):
///   1. tools/list -> UNION of both services' tools in registration-then-
///      declaration order;
///   2. tools/call -> routes the flat tool name to the correct owning service —
///      proven for a tool of EACH service (UserService_getUser,
///      OrderService_cancelOrder), Channel-A Right (isError:false);
///   3. registering a server with a colliding tool name -> Err(DuplicateTool);
///   4. unknown tool name -> -32602 "unknown tool" response (NoMatchingTool).
///
/// Assertion discipline: every assertion uses `assert!`/`assert_eq!`, which
/// panic unconditionally on failure — no `if`-guarded checks.

use baboon_rs_stub::baboon_mcp_server::{
    AbstractMcpMuxer, BaboonMcpWiringError, JsonRpcRequest, McpSession, McpServerInfo,
    IBaboonRoutableMcpServer, McpJsonInvoke, json_rpc_error_codes,
};
use baboon_rs_stub::baboon_runtime::BaboonCodecContext;
use baboon_rs_stub::baboon_service_wiring::BaboonMethodId;

use baboon_rs_stub::mcp::mux::stub::baboon_service_rt::BaboonServiceRtDefault;

use baboon_rs_stub::mcp::mux::stub::user_service::UserService;
use baboon_rs_stub::mcp::mux::stub::user_service_wiring::invoke_json_user_service;
use baboon_rs_stub::mcp::mux::stub::user_service_mcp_server::UserServiceMcpServer;
use baboon_rs_stub::mcp::mux::stub::userservice::createuser::input::In as CreateUserIn;
use baboon_rs_stub::mcp::mux::stub::userservice::createuser::out::Out as CreateUserOut;
use baboon_rs_stub::mcp::mux::stub::userservice::getuser::input::In as GetUserIn;
use baboon_rs_stub::mcp::mux::stub::userservice::getuser::out::Out as GetUserOut;

use baboon_rs_stub::mcp::mux::stub::order_service::OrderService;
use baboon_rs_stub::mcp::mux::stub::order_service_wiring::invoke_json_order_service;
use baboon_rs_stub::mcp::mux::stub::order_service_mcp_server::OrderServiceMcpServer;
use baboon_rs_stub::mcp::mux::stub::orderservice::placeorder::input::In as PlaceOrderIn;
use baboon_rs_stub::mcp::mux::stub::orderservice::placeorder::out::Out as PlaceOrderOut;
use baboon_rs_stub::mcp::mux::stub::orderservice::cancelorder::input::In as CancelOrderIn;
use baboon_rs_stub::mcp::mux::stub::orderservice::cancelorder::out::Out as CancelOrderOut;

use baboon_rs_stub::mcp::mux::stub::user_profile::UserProfile;
use baboon_rs_stub::mcp::mux::stub::user_status::UserStatus;
use baboon_rs_stub::mcp::mux::stub::order_summary::OrderSummary;
use baboon_rs_stub::mcp::mux::stub::order_status::OrderStatus;

const MERGED_NAME: &str = "MergedEndpoint";
const MERGED_VERSION: &str = "1.0.0";

// The union of both services' tools in registration-then-declaration order:
// UserService registered first (createUser, getUser declared in that order),
// OrderService second (placeOrder, cancelOrder).
const EXPECTED_UNION: [&str; 4] = [
    "UserService_createUser",
    "UserService_getUser",
    "OrderService_placeOrder",
    "OrderService_cancelOrder",
];

// ---------------------------------------------------------------------------
// Stub service impls: every method returns a fixed value (no I/O).
// ---------------------------------------------------------------------------

struct StubUserService;

impl UserService for StubUserService {
    async fn create_user(&self, _arg: CreateUserIn) -> CreateUserOut {
        CreateUserOut { profile: profile() }
    }
    async fn get_user(&self, _arg: GetUserIn) -> GetUserOut {
        GetUserOut { profile: Some(profile()) }
    }
}

struct StubOrderService;

impl OrderService for StubOrderService {
    async fn place_order(&self, _arg: PlaceOrderIn) -> PlaceOrderOut {
        PlaceOrderOut { summary: OrderSummary { order_id: "o1".to_string(), status: OrderStatus::Confirmed, total: 10.0 } }
    }
    async fn cancel_order(&self, _arg: CancelOrderIn) -> CancelOrderOut {
        CancelOrderOut { ok: true }
    }
}

fn profile() -> UserProfile {
    UserProfile { user_id: "u1".to_string(), email: "a@b.c".to_string(), status: UserStatus::Active }
}

fn codec_ctx() -> BaboonCodecContext {
    BaboonCodecContext::Default
}

// ---------------------------------------------------------------------------
// Delegate factories (ASYNC mode): under `--rs-async-services=true`,
// `McpJsonInvoke<Ctx>` is the FUTURE-RETURNING alias
//   `Box<dyn Fn(..) -> Pin<Box<dyn Future<Output = Result<String, BaboonWiringError>>>>>`
// (baboon_mcp_server.rs, swapped by RsMcpServerGenerator). `invoke_json_<svc>(..)`
// is an `async fn` whose call evaluates to a future borrowing its inputs; to hand
// that future across the `Fn` boundary it must be `'static`, so the borrowed
// inputs are cloned into owned bindings moved into the `async move` block and `rt`
// is constructed inside the future. The generated `<Service>McpServer` /
// `AbstractMcpMuxer` drive this future to completion synchronously (the same
// `block_on` bridge the per-service async server uses, D30/T98).
// ---------------------------------------------------------------------------

fn user_invoke() -> McpJsonInvoke<()> {
    Box::new(|method: &BaboonMethodId, data: &str, _ctx: (), cc: &BaboonCodecContext| {
        let method = method.clone();
        let data = data.to_string();
        let cc = cc.clone();
        Box::pin(async move {
            let rt = BaboonServiceRtDefault;
            invoke_json_user_service(&method, &data, &StubUserService, &rt, &cc).await
        })
    })
}

fn order_invoke() -> McpJsonInvoke<()> {
    Box::new(|method: &BaboonMethodId, data: &str, _ctx: (), cc: &BaboonCodecContext| {
        let method = method.clone();
        let data = data.to_string();
        let cc = cc.clone();
        Box::pin(async move {
            let rt = BaboonServiceRtDefault;
            invoke_json_order_service(&method, &data, &StubOrderService, &rt, &cc).await
        })
    })
}

// ---------------------------------------------------------------------------
// Server / muxer factories: composed through the PUBLIC routable interface only.
// ---------------------------------------------------------------------------

fn make_user_server() -> Box<dyn IBaboonRoutableMcpServer<()>> {
    Box::new(UserServiceMcpServer::<()>::new(user_invoke()))
}

fn make_order_server() -> Box<dyn IBaboonRoutableMcpServer<()>> {
    Box::new(OrderServiceMcpServer::<()>::new(order_invoke()))
}

fn merged_info() -> McpServerInfo {
    McpServerInfo { name: MERGED_NAME, version: MERGED_VERSION }
}

fn make_muxer() -> AbstractMcpMuxer<()> {
    AbstractMcpMuxer::<()>::new(merged_info())
        .with(make_user_server())
        .expect("register UserService")
        .with(make_order_server())
        .expect("register OrderService")
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

fn inited_session(mux: &AbstractMcpMuxer<()>) -> McpSession {
    let ctx = codec_ctx();
    let mut session = McpSession::new();
    mux.handle(&JsonRpcRequest {
        id: Some(serde_json::json!(0)),
        method: "initialize".to_string(),
        params: Some(serde_json::json!({
            "protocolVersion": "2025-06-18",
            "capabilities": {},
            "clientInfo": { "name": "t", "version": "0" }
        })),
    }, &mut session, (), &ctx).unwrap();
    let r = mux.handle(&JsonRpcRequest {
        id: None,
        method: "notifications/initialized".to_string(),
        params: None,
    }, &mut session, (), &ctx);
    assert!(r.is_none(), "notifications/initialized must return None");
    session
}

fn list(mux: &AbstractMcpMuxer<()>) -> serde_json::Value {
    let ctx = codec_ctx();
    let mut session = inited_session(mux);
    let resp = mux.handle(&JsonRpcRequest {
        id: Some(serde_json::json!(1)),
        method: "tools/list".to_string(),
        params: None,
    }, &mut session, (), &ctx).expect("tools/list response");
    serde_json::to_value(&resp).unwrap()
}

fn call(mux: &AbstractMcpMuxer<()>, session: &mut McpSession, name: &str, args: serde_json::Value) -> serde_json::Value {
    let ctx = codec_ctx();
    let resp = mux.handle(&JsonRpcRequest {
        id: Some(serde_json::json!(99)),
        method: "tools/call".to_string(),
        params: Some(serde_json::json!({ "name": name, "arguments": args })),
    }, session, (), &ctx).expect("tools/call response");
    serde_json::to_value(&resp).unwrap()
}

// ---------------------------------------------------------------------------
// §1 — tools/list union ordering
// ---------------------------------------------------------------------------

#[test]
fn mux_tools_list_returns_union_in_registration_then_declaration_order() {
    let resp = list(&make_muxer());
    assert_eq!(resp["id"], serde_json::json!(1));
    assert!(resp["error"].is_null(), "tools/list must not error");
    let tools = resp["result"]["tools"].as_array().unwrap();
    assert_eq!(tools.len(), 4, "union MUST contain exactly 4 tools");
    let names: Vec<&str> = tools.iter().map(|t| t["name"].as_str().unwrap()).collect();
    assert_eq!(names.as_slice(), &EXPECTED_UNION, "union order must be registration-then-declaration");
}

#[test]
fn mux_union_is_not_interleaved_negative_control() {
    // DELIBERATE-NEGATIVE-CONTROL: proves the ordering assertion is live.
    let resp = list(&make_muxer());
    let tools = resp["result"]["tools"].as_array().unwrap();
    let names: Vec<&str> = tools.iter().map(|t| t["name"].as_str().unwrap()).collect();
    let interleaved = [
        "UserService_createUser",
        "OrderService_placeOrder",
        "UserService_getUser",
        "OrderService_cancelOrder",
    ];
    assert_ne!(names.as_slice(), &interleaved, "union must NOT be interleaved");
}

#[test]
fn mux_union_carries_each_tool_input_schema() {
    // Each union entry must keep the inputSchema the per-service server emitted.
    let resp = list(&make_muxer());
    let tools = resp["result"]["tools"].as_array().unwrap();
    for t in tools {
        let schema_uri = t["inputSchema"]["$schema"].as_str().unwrap();
        assert_eq!(
            schema_uri,
            "https://json-schema.org/draft/2020-12/schema",
            "tool {} inputSchema must carry the Draft 2020-12 $schema URI",
            t["name"]
        );
    }
}

// ---------------------------------------------------------------------------
// §2 — tools/call routes the flat name to the owning server
// ---------------------------------------------------------------------------

#[test]
fn mux_routes_user_service_get_user_to_owning_server() {
    let mux = make_muxer();
    let mut session = inited_session(&mux);
    let resp = call(&mux, &mut session, "UserService_getUser", serde_json::json!({ "userId": "u1" }));
    assert_eq!(resp["id"], serde_json::json!(99));
    assert!(resp["error"].is_null(), "routed call must not be a Channel-A error");
    assert_eq!(resp["result"]["isError"], serde_json::json!(false), "isError must be false");
    let content = resp["result"]["content"].as_array().unwrap();
    assert_eq!(content.len(), 1);
    assert_eq!(content[0]["type"], "text");
    // Payload proves UserService handled it (profile.userId present).
    let payload: serde_json::Value = serde_json::from_str(content[0]["text"].as_str().unwrap()).unwrap();
    assert_eq!(payload["profile"]["userId"], "u1", "UserService must produce the profile");
}

#[test]
fn mux_routes_order_service_cancel_order_to_owning_server() {
    let mux = make_muxer();
    let mut session = inited_session(&mux);
    let resp = call(&mux, &mut session, "OrderService_cancelOrder", serde_json::json!({ "orderId": "o1", "reason": null }));
    assert!(resp["error"].is_null(), "routed call must not be a Channel-A error");
    assert_eq!(resp["result"]["isError"], serde_json::json!(false), "isError must be false");
    let content = resp["result"]["content"].as_array().unwrap();
    // Payload proves OrderService handled it (ok field present).
    let payload: serde_json::Value = serde_json::from_str(content[0]["text"].as_str().unwrap()).unwrap();
    assert_eq!(payload["ok"], true, "OrderService must produce ok=true");
}

#[test]
fn mux_order_service_decode_failure_routes_to_channel_b() {
    // Channel-B trigger: cancelOrder missing required "orderId" -> serde decode
    // failure -> DecoderFailed -> Err -> isError=true. Proves routing reaches the
    // owning server's codec path, not a muxer-level short-circuit.
    let mux = make_muxer();
    let mut session = inited_session(&mux);
    let resp = call(&mux, &mut session, "OrderService_cancelOrder", serde_json::json!({ "reason": null }));
    assert!(resp["result"].is_object(), "Channel-B: result must be present");
    assert!(resp["error"].is_null(), "Channel-B: must not be a JSON-RPC error");
    assert_eq!(resp["result"]["isError"], serde_json::json!(true), "isError MUST be true for decode failure");
    let content = resp["result"]["content"].as_array().unwrap();
    assert!(!content.is_empty());
    assert!(!content[0]["text"].as_str().unwrap_or("").is_empty(), "Channel-B content text must be non-empty");
}

// ---------------------------------------------------------------------------
// §3 — DuplicateTool on registration collision
// ---------------------------------------------------------------------------

#[test]
fn mux_register_collision_returns_duplicate_tool() {
    // A second UserServiceMcpServer re-declares UserService_createUser / _getUser.
    let mut mux = AbstractMcpMuxer::<()>::new(merged_info());
    mux.register(make_user_server()).expect("first UserService registers cleanly");
    let err = mux.register(make_user_server()).expect_err("second UserService MUST collide");
    match err {
        BaboonMcpWiringError::DuplicateTool(name) => {
            assert_eq!(name, "UserService_createUser", "DuplicateTool must carry the colliding tool name");
        }
        other => panic!("expected DuplicateTool, got {:?}", other),
    }
}

#[test]
fn mux_with_collision_returns_duplicate_tool() {
    // The fluent `with` builder must also surface the collision as Err(DuplicateTool).
    let result = AbstractMcpMuxer::<()>::new(merged_info())
        .with(make_user_server())
        .and_then(|m| m.with(make_user_server()));
    assert!(result.is_err(), "colliding `with` chain MUST be Err");
    match result.err().unwrap() {
        BaboonMcpWiringError::DuplicateTool(_) => {}
        other => panic!("expected DuplicateTool, got {:?}", other),
    }
}

// ---------------------------------------------------------------------------
// §4 — NoMatchingTool: unknown tool name -> -32602
// ---------------------------------------------------------------------------

#[test]
fn mux_unknown_tool_name_returns_32602() {
    // NEGATIVE CONTROL: a success result for an unknown tool would fail these.
    let mux = make_muxer();
    let mut session = inited_session(&mux);
    let resp = call(&mux, &mut session, "UserService_nope", serde_json::json!({}));
    assert!(resp["error"].is_object(), "unknown tool MUST be a Channel-A error");
    assert!(resp["result"].is_null(), "no result for unknown tool");
    assert_eq!(
        resp["error"]["code"],
        serde_json::json!(json_rpc_error_codes::INVALID_PARAMS),
        "unknown tool error code MUST be -32602"
    );
    assert!(!resp["error"]["message"].as_str().unwrap_or("").is_empty(), "error.message must be non-empty");
}
