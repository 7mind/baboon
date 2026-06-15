# rust-stub-mcp-async-overlay (scaffold)

Async-MCP test-lane overlay for the Rust backend (D24/G11).

This directory is the async sibling of `test/rust-stub-mcp-overlay/`. It is layered
on top of a `rs-stub` copy by the `test-gen-rust-mcp-async` mdl action, which
generates the `mcp-stub-ok` model with BOTH `--rs-generate-mcp-server=true` AND
`--rs-async-services=true`. The `test-rust-mcp-async` action then builds and
round-trips the generated async MCP server against the test target placed here.

Convention (shared by cs/py/rs/ts/sw):
- sync lane  : `test-gen-<lang>-mcp`       + `test-<lang>-mcp`       (overlay `test/<lang>-stub-mcp-overlay/`)
- async lane : `test-gen-<lang>-mcp-async` + `test-<lang>-mcp-async` (overlay `test/<lang>-stub-mcp-async-overlay/`)

Status: scaffold only. The test target (mirroring
`test/rust-stub-mcp-overlay/tests/mcp_tests.rs`) lands with the Rust async-MCP
backend fix. Until then this lane is expected RED.
