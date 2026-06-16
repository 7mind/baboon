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

## Status: GREEN (T67 fix landed)

`Tests/McpTests/McpTests.swift` is the async analogue of the sync overlay's
round-trip test. Its `makeServer(...)` binds the generated
`McpToolsWiring.invokeJson` (`async throws -> String`) into the generated
`McpToolsMcpServer`'s delegate, then `await server.handle(...)` drives a real
`tools/call` round-trip.

### The async/sync mismatch (D24) and the T67 fix

- Under `--sw-async-services=true` the generated no-errors wiring dispatcher
  `McpToolsWiring.invokeJson(...)` is `async throws -> String`
  (`SwServiceWiringTranslator.scala:43` `dispatcherEffects`, applied at the
  `invokeJson` declaration around `:508-513`). Its body is
  `try await impl.<method>(arg:)`, valid only in an async context.

- T66 reproduced the defect RED: the generated MCP server declared its delegate
  as the SYNC closure type `throws -> String`, so the `async throws` wiring call
  could not inhabit it (`cannot pass function of type … async throws -> String to
  parameter expecting synchronous function type`).

- T67 threads `asyncServices` into `SwMcpServerGenerator` + the runtime: under
  the async axis the generated server holds an `async throws -> String` delegate
  and conforms to `IBaboonAsyncMcpServer`, whose `handle` is GENUINELY `async`
  and `await`s the delegate DIRECTLY in the caller's task.

### Why a genuinely-async `handle`, not a synchronous semaphore bridge

A round-1 attempt kept `handle` synchronous and drove the async delegate to
completion via a `DispatchSemaphore` joined on a spawned `Task`. That DEADLOCKS
when `handle` is called from an actor-isolated context (`@MainActor` or a custom
actor): the (context-inheriting) `Task` is scheduled on the caller's actor
executor, which the blocked thread is holding, so `signal()` never runs. It can
also starve the bounded cooperative thread pool. The genuinely-async `handle`
suspends the caller's task cooperatively rather than blocking a thread, so it is
deadlock-free from ANY context. `McpTests.swift` §2 exercises a `@MainActor`
caller as the regression for that deadlock class.

Unlike the Rust `block_on` path (a current-thread no-op-waker poll loop with a
documented external-waker precondition), Swift structured concurrency lets the
async effect propagate to the integrator, so no blocking bridge is needed at all.

The sync `test-swift-mcp` lane — generated and built the same way WITHOUT
`--sw-async-services=true` — is untouched and still passes; its output is
byte-identical to baseline.
