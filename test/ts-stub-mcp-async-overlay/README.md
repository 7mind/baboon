# ts-stub-mcp-async-overlay (scaffold)

Async-MCP test-lane overlay for the TypeScript backend (D24/G11).

This directory is the async sibling of `test/ts-stub-mcp-overlay/`. It is layered
on top of a `ts-stub` copy by the `test-gen-ts-mcp-async` mdl action, which
generates the `mcp-stub-ok` model with BOTH `--ts-generate-mcp-server=true` AND
`--ts-async-services=true`. The `test-ts-mcp-async` action then builds and
round-trips the generated async MCP server against the test target placed here.

Convention (shared by cs/py/rs/ts/sw):
- sync lane  : `test-gen-<lang>-mcp`       + `test-<lang>-mcp`       (overlay `test/<lang>-stub-mcp-overlay/`)
- async lane : `test-gen-<lang>-mcp-async` + `test-<lang>-mcp-async` (overlay `test/<lang>-stub-mcp-async-overlay/`)

Status: scaffold only. The test target (mirroring
`test/ts-stub-mcp-overlay/src/mcp.test.ts`) lands with the TypeScript async-MCP
backend fix. Until then this lane is expected RED.
