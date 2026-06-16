# rust-stub-mcp-async-overlay

Async-MCP test-lane overlay for the Rust backend (D24/G11).

This directory is the async sibling of `test/rust-stub-mcp-overlay/`. It is layered
on top of a `rs-stub` copy by the `test-gen-rust-mcp-async` mdl action, which
generates the `mcp-stub-ok` model with BOTH `--rs-generate-mcp-server=true` AND
`--rs-async-services=true`. The `test-rust-mcp-async` action then builds and
round-trips the generated async MCP server against the test target in `tests/`.

Convention (shared by cs/py/rs/ts/sw):
- sync lane  : `test-gen-<lang>-mcp`       + `test-<lang>-mcp`       (overlay `test/<lang>-stub-mcp-overlay/`)
- async lane : `test-gen-<lang>-mcp-async` + `test-<lang>-mcp-async` (overlay `test/<lang>-stub-mcp-async-overlay/`)

## Status: GREEN (fix landed — T98/D28)

`tests/mcp_tests.rs` is the async analogue of the sync overlay's round-trip test:
its `make_fake_invoke()` binds the generated `invoke_json_mcp_tools` into the
generated MCP server's `McpJsonInvoke<()>` callback.

Under `--rs-async-services=true` the generator (RsMcpServerGenerator) now swaps
the runtime `McpJsonInvoke<Ctx>` alias for its future-returning form
(`Box<dyn Fn(..) -> Pin<Box<dyn Future<Output = Result<String, BaboonWiringError>>>>>`,
intentionally `?Send` to match the wiring futures), and the generated
`McpToolsMcpServer::handle` drives that future to completion synchronously with a
self-contained `block_on` before delegating to the (byte-identical) sync
`BaboonMcpServerBase` state machine. The `async fn`-in-trait AFIT warning that
`--rs-async-services=true` produces is allowed crate-wide
(`#![allow(async_fn_in_trait)]`) only when async is enabled.

`make_fake_invoke()` returns a `'static` boxed future: the async service future
borrows `method`/`data`/`rt`/`codec_ctx`, so those inputs are cloned into owned
bindings moved into the `async move` block (mirroring the generated muxer
wrappers) and `rt` is constructed inside the future. `cargo test --test mcp_tests`
now builds clean under `-D warnings` and the round-trip passes.

### Historical RED (pre-T98)

Before the fix this lane was an EXPECTED RED reproduction (T62, gating T63/T98):
the sync `McpJsonInvoke` closure alias could not hold the async `invoke_json_*`
future, so `cargo test --test mcp_tests` failed at `cargo build` (lib) — never
reaching the test body.

The RED was reproduced with the JVM-classpath compiler (`sbt baboonJVM/runMain
io.septimalmind.baboon.Baboon … --rs-generate-mcp-server=true
--rs-async-services=true`) + `cargo 1.91`. The sync `test-rust-mcp` lane,
generated and built the same way without `--rs-async-services`, still passes
(13 tests, exit 0).

### Captured rustc errors (all in GENERATED `src/`, none in this overlay)

The async/sync mismatch surfaces as a family of errors rooted in one cause: the
generated service-wiring abstractions assume SYNCHRONOUS execution, so async
service calls cannot be threaded through them.

1. `error[E0728]: 'await' is only allowed inside 'async' functions and blocks`
   — `src/mcp/stub/mcp_tools_wiring.rs` (12×, one per method × json/ueba). The
   errors-mode async `invoke_json_mcp_tools` body emits
   `rt.flat_map(input, |v| { … impl_.method(v).await … })`, but
   `IBaboonServiceRt::flat_map<F: FnOnce(B) -> Result<C,A>>` takes a SYNC closure
   returning a `Result`, not a future. `.await` inside that sync closure is
   illegal.

2. `error[E0599]: no method named 'clone' found for type parameter 'Impl'/'Rt'`
   — `src/mcp/stub/mcp_tools_wiring.rs` muxer wrappers (`McpToolsJsonService` /
   `McpToolsUebaService`): the async `invoke` clones `impl_`/`rt` into a
   `Box::pin(async move { … })` but the generic bounds omit `Clone`.

3. `error[E0277]: '?' couldn't convert the error …` — `src/mcp/stub/mcp_tools_client.rs`
   async client methods.

The `McpJsonInvoke` sync-closure bound that RsMcpServerGenerator.scala:124/152
emits (`Box<dyn Fn(…) -> Result<String, BaboonWiringError>>`) is the same
async/sync defect downstream: an `async fn invoke_json_*` returns a future, which
that sync closure type cannot hold. It is currently MASKED — the lib fails to
compile at (1)–(3) before the integration-test binding site is type-checked.

The fix T63 must make the async service path compile end-to-end — including an
async-capable invoke alias for the generated MCP server — which turns this lane
green.
