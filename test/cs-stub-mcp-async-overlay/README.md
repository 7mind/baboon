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

Status: RED reproduction (T58). `McpTests/` mirrors
`test/cs-stub-mcp-overlay/McpTests/` but binds the async-typed wiring against the
SYNC generated MCP server. `dotnet build` is EXPECTED to FAIL: under
`--cs-async-services=true` `McpToolsWiring.InvokeJson` returns
`Task<Either<BaboonWiringError, string>>`, but `McpToolsMcpServer<Ctx>` still
requires a SYNC `McpJsonInvoke<Ctx>` delegate (returns `Either<..>` directly) —
a return-type mismatch (CS0029 / CS4016). This gates the C# async-MCP fix (T59);
do NOT wire it green here. Once T59 lands an async-capable MCP server this
overlay is updated to compile and the lane turns green.
