# cs-stub-mcp-async-overlay (scaffold)

Async-MCP test-lane overlay for the C# backend (D24/G11).

This directory is the async sibling of `test/cs-stub-mcp-overlay/`. It is layered
on top of a `cs-stub` copy by the `test-gen-cs-mcp-async` mdl action, which
generates the `mcp-stub-ok` model with BOTH `--cs-generate-mcp-server=true` AND
`--cs-async-services=true`. The `test-cs-mcp-async` action then builds and
round-trips the generated async MCP server against the test target placed here.

Convention (shared by cs/py/rs/ts/sw):
- sync lane  : `test-gen-<lang>-mcp`       + `test-<lang>-mcp`       (overlay `test/<lang>-stub-mcp-overlay/`)
- async lane : `test-gen-<lang>-mcp-async` + `test-<lang>-mcp-async` (overlay `test/<lang>-stub-mcp-async-overlay/`)

Status: GREEN (T59). `McpTests/` mirrors `test/cs-stub-mcp-overlay/McpTests/` but
binds the async-typed wiring against the ASYNC generated MCP server. Under
`--cs-async-services=true` `McpToolsWiring.InvokeJson` returns
`Task<Either<BaboonWiringError, string>>`; T59 makes the generated
`McpToolsMcpServer<Ctx>` extend the async runtime base
`AbstractBaboonMcpServerAsync<Ctx>`, take the async `McpJsonInvokeAsync<Ctx>`
delegate (returns `Task<Either<..>>`), `await` it in `InvokeJson`, and expose
`Task<JsonRpcResponse?> Handle(...)`. The async wiring result binds directly to
that delegate, so the overlay builds and round-trips a `tools/call`. With the
flag OFF the generated sync MCP server is byte-identical to the pre-change
baseline.
