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

Status: RED repro (T64/D24). The test target (`src/mcp.test.ts`, async sibling of
`test/ts-stub-mcp-overlay/src/mcp.test.ts`) binds the async wiring
`invokeJson_McpTools` (which returns `Promise<BaboonEither<…>>` under
`--ts-async-services=true`) into the generated `McpToolsMcpServer` constructor,
whose delegate `TsMcpServerGenerator` still declares SYNCHRONOUS
(`=> BaboonEitherResult`). The `npm run build` (`tsc --noEmit`) gate in the
`test-ts-mcp-async` action therefore FAILS with:

```
src/mcp.test.ts: error TS2345: Argument of type
  '(…) => Promise<BaboonEitherResult>' is not assignable to parameter of type
  '(…) => BaboonEitherResult'.
  Type 'Promise<BaboonEitherResult>' is not assignable to type 'BaboonEitherResult'.
```

This lane is EXPECTED RED until the generator fix (T65) threads the async axis
through the MCP-server delegate. The vitest run after the build is the eventual
green check once T65 lands.
