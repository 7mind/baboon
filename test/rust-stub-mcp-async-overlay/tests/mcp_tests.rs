/// T62 — Rust ASYNC-MCP round-trip overlay test (D24 / G11).
///
/// Async sibling of `test/rust-stub-mcp-overlay/tests/mcp_tests.rs`.
///
/// This lane is generated with BOTH `--rs-generate-mcp-server=true` AND
/// `--rs-async-services=true`. It is a DELIBERATE RED reproduction of the
/// async-MCP wiring mismatch (D24):
///
///   - Under `--rs-async-services=true`, the generated service-wiring entry
///     point `invoke_json_mcp_tools(...)` is emitted as an `async fn`
///     (RsServiceWiringTranslator.scala:778, `pub ${asyncKw}fn invoke_json_*`).
///     An `async fn` returns an opaque `impl Future<Output = Result<String,
///     BaboonWiringError>>`, NOT `Result<String, BaboonWiringError>` directly,
///     and the per-call `impl_.<method>(...).await` body only type-checks
///     inside an async context.
///
///   - The generated MCP server, however, still binds `invoke_json` to the
///     SYNC closure alias `McpJsonInvoke<Ctx>` (RsMcpServerGenerator.scala:124
///     / :152), which the runtime defines as
///         `Box<dyn Fn(&BaboonMethodId, &str, Ctx, &BaboonCodecContext)
///              -> Result<String, BaboonWiringError>>`
///     (baboon_mcp_server.rs:124). That is a SYNCHRONOUS closure: its body must
///     evaluate to a `Result`, and `.await` is illegal in it.
///
/// Therefore the `make_fake_invoke()` binding below — the async analogue of the
/// sync overlay's binding, which there compiles cleanly — cannot type-check:
/// the async `invoke_json_mcp_tools(...)` call evaluates to a future where a
/// `Result<String, BaboonWiringError>` is required (E0308 / `expected Result,
/// found future`, or "`await` is only allowed inside `async` ...").
///
/// EXPECTED RED: `cargo build` / `cargo test --test mcp_tests` MUST FAIL with a
/// rustc error tying the async wiring fn to the sync `McpJsonInvoke` closure
/// bound. The sync `test-rust-mcp` lane is untouched and still passes.
///
/// The async-MCP backend fix (T63) makes the generator emit an async-capable
/// invoke alias so this binding compiles; only then does this lane go green.

use baboon_rs_stub::baboon_mcp_server::{
    IBaboonMcpServer, JsonRpcRequest, McpSession, McpJsonInvoke,
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
use baboon_rs_stub::mcp::stub::mcptools::processtagged::input::In as ProcessTaggedIn;
use baboon_rs_stub::mcp::stub::mcptools::processtagged::out::Out as ProcessTaggedOut;
use baboon_rs_stub::mcp::stub::mcptools::pagepoints::input::In as PagePointsIn;
use baboon_rs_stub::mcp::stub::mcptools::pagepoints::out::Out as PagePointsOut;
use baboon_rs_stub::mcp::stub::mcptools::ping::input::In as PingIn;
use baboon_rs_stub::mcp::stub::mcptools::ping::out::Out as PingOut;
use baboon_rs_stub::mcp::stub::mcptools::describepricing::input::In as DescribePricingIn;
use baboon_rs_stub::mcp::stub::mcptools::describepricing::out::Out as DescribePricingOut;

// ---------------------------------------------------------------------------
// Stub McpTools service. Under `--rs-async-services=true` the generated
// `McpTools` trait declares each method as `async fn` (RsDefnTranslator.scala
// :1542), so the stub implements them as `async fn`; every method returns
// ok=true (T7 §3 convention).
// ---------------------------------------------------------------------------

struct StubMcpTools;

impl McpTools for StubMcpTools {
    async fn list_collections(&self, _arg: ListCollectionsIn) -> ListCollectionsOut {
        ListCollectionsOut { ok: true }
    }
    async fn submit_composite(&self, _arg: SubmitCompositeIn) -> SubmitCompositeOut {
        SubmitCompositeOut { ok: true }
    }
    async fn process_shape(&self, _arg: ProcessShapeIn) -> ProcessShapeOut {
        ProcessShapeOut { ok: true }
    }
    async fn process_tagged(&self, _arg: ProcessTaggedIn) -> ProcessTaggedOut {
        ProcessTaggedOut { ok: true }
    }
    async fn page_points(&self, _arg: PagePointsIn) -> PagePointsOut {
        PagePointsOut { ok: true }
    }
    async fn ping(&self, _arg: PingIn) -> PingOut {
        PingOut { ok: true }
    }
    async fn describe_pricing(&self, _arg: DescribePricingIn) -> DescribePricingOut {
        DescribePricingOut { ok: true }
    }
}

// ---------------------------------------------------------------------------
// Fake transport: routes through the generated invoke_json_mcp_tools dispatch.
//
// DELIBERATE RED (D24): `McpJsonInvoke<()>` is a SYNC closure alias
//   `Box<dyn Fn(...) -> Result<String, BaboonWiringError>>`,
// but under `--rs-async-services=true` `invoke_json_mcp_tools(...)` is an
// `async fn` whose call evaluates to a future, not a `Result`. The closure
// body below therefore does NOT type-check against the sync bound — this is
// the exact mismatch the lane is built to capture. (The sync overlay's
// identical binding compiles because there `invoke_json_mcp_tools` is sync.)
// ---------------------------------------------------------------------------

fn make_fake_invoke() -> McpJsonInvoke<()> {
    Box::new(|method: &BaboonMethodId, data: &str, _ctx: (), codec_ctx: &BaboonCodecContext| {
        // Under `--rs-async-services=true`, `McpJsonInvoke<Ctx>` is the
        // future-returning alias `Box<dyn Fn(..) -> Pin<Box<dyn Future<Output =
        // Result<String, BaboonWiringError>>>>>` (baboon_mcp_server.rs, swapped
        // by RsMcpServerGenerator). `invoke_json_mcp_tools(..)` is an `async fn`
        // whose call evaluates to a future borrowing `method`/`data`/`rt`/
        // `codec_ctx`. To hand that future across the `Fn` boundary it must be
        // `'static`, so the borrowed inputs are cloned into owned bindings moved
        // into the `async move` block (mirroring the generated muxer wrappers),
        // and `rt` is constructed inside the future so it is owned by it. The
        // generated `McpToolsMcpServer::handle` drives this future to completion
        // synchronously (its `block_on`).
        let method = method.clone();
        let data = data.to_string();
        let codec_ctx = codec_ctx.clone();
        Box::pin(async move {
            let rt = BaboonServiceRtDefault;
            invoke_json_mcp_tools(&method, &data, &StubMcpTools, &rt, &codec_ctx).await
        })
    })
}

fn make_server() -> McpToolsMcpServer<()> {
    McpToolsMcpServer::new(make_fake_invoke())
}

fn codec_ctx() -> BaboonCodecContext {
    BaboonCodecContext::Default
}

// ---------------------------------------------------------------------------
// §1 — initialize + tools/call round-trip.
//
// If the async-MCP fix (T63) lands, this drives the canonical round-trip; until
// then the crate does not compile (the binding above is rejected) and this test
// never runs. The lane's RED state is asserted at compile time, not here.
// ---------------------------------------------------------------------------

#[test]
fn async_mcp_initialize_and_tools_call_roundtrip() {
    let server = make_server();
    let mut session = McpSession::default();
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
    server.handle(&init_req, &mut session, (), &ctx).unwrap();

    let call_req = JsonRpcRequest {
        id: Some(serde_json::json!(1)),
        method: "tools/call".to_string(),
        params: Some(serde_json::json!({
            "name": "McpTools_ping",
            "arguments": { "seqno": 42, "label": "hello" }
        })),
    };
    let resp = server.handle(&call_req, &mut session, (), &ctx);
    let resp = resp.expect("expected a response for tools/call but got None");
    let value = serde_json::to_value(&resp).unwrap();
    let is_error = value
        .get("result")
        .and_then(|r| r.get("isError"))
        .and_then(|b| b.as_bool())
        .unwrap_or(true);
    assert!(!is_error, "tools/call McpTools_ping must succeed (isError=false)");
}
