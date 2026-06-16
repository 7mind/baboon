# swift-stub-mcp-async-overlay

Async-MCP test-lane overlay for the Swift backend (D24/G11).

This directory is the async sibling of `test/swift-stub-mcp-overlay/`. It is layered
on top of a `sw-stub` copy by the `test-gen-swift-mcp-async` mdl action, which
generates the `mcp-stub-ok` model with BOTH `--sw-generate-mcp-server=true` AND
`--sw-async-services=true` (no-errors mode). The `test-swift-mcp-async` action then
builds and round-trips the generated async MCP server against the test target in
`Tests/McpTests/`.

Convention (shared by cs/py/rs/ts/sw):
- sync lane  : `test-gen-<lang>-mcp`       + `test-<lang>-mcp`       (overlay `test/<lang>-stub-mcp-overlay/`)
- async lane : `test-gen-<lang>-mcp-async` + `test-<lang>-mcp-async` (overlay `test/<lang>-stub-mcp-async-overlay/`)

## Status: RED (EXPECTED — gates the Swift async-MCP backend fix T67)

`Tests/McpTests/McpTests.swift` is the async analogue of the sync overlay's
round-trip test. Its `makeServer(...)` binds the generated
`McpToolsWiring.invokeJson` into the generated `McpToolsMcpServer`'s delegate —
exactly as the sync overlay does — but here that binding does NOT type-check.

### The async/sync mismatch (D24)

- Under `--sw-async-services=true` the generated no-errors wiring dispatcher
  `McpToolsWiring.invokeJson(...)` is `async throws -> String`
  (`SwServiceWiringTranslator.scala:43` `dispatcherEffects`, applied at the
  `invokeJson` declaration around `:508-513`). Its body is
  `try await impl.<method>(arg:)`, valid only in an async context, and a call to
  it requires `await`.

- The generated MCP server still declares its delegate as the SYNC closure type
  `(BaboonMethodId, String, Ctx, BaboonCodecContext) throws -> String`
  (`SwMcpServerGenerator.scala:125` stored property / `:127` init parameter) — a
  non-async closure whose body must produce a `String` without `await`.

Supplying the `async throws` wiring call (`try await McpToolsWiring.invokeJson(...)`)
where a non-async `throws -> String` closure is expected is rejected by swiftc.

### Captured swiftc error (in this overlay's `Tests/McpTests/McpTests.swift`)

`swift build --build-tests` / `swift test` FAILS at the `makeServer` binding
with the async/sync mismatch. Captured verbatim with Swift 5.10.1 (Linux
x86_64) via `scripts/swift-xcode.sh <pkg> build --build-tests`:

```
Tests/McpTests/McpTests.swift:85:44: error: cannot pass function of type
  '(BaboonMethodId, String, any McpTools, BaboonCodecContext) async throws -> String'
  to parameter expecting synchronous function type
Tests/McpTests/McpTests.swift:86:24: note: 'async' inferred from asynchronous operation used here
```

Line 85 is the `McpToolsMcpServer<McpTools> { ... }` delegate binding; line 86 is
the `try await McpToolsWiring.invokeJson(...)` call inside it. The closure is
inferred `async` (from the `await`), and that `async` closure cannot be passed
where the server's synchronous `throws -> String` delegate parameter is expected.
This is the async/sync mismatch — NOT an unrelated error.

This is the SAME async/sync defect the Rust lane reproduced (T62→T98): the sync
MCP-server delegate cannot hold the async wiring dispatcher. The fix T67 must give
the generated Swift MCP server an async-capable delegate path (whose `handle`
drives the async to completion, mirroring the Rust `block_on` path); only then
does this binding compile and this lane go green.

The sync `test-swift-mcp` lane — generated and built the same way WITHOUT
`--sw-async-services=true` — is untouched and still passes.
