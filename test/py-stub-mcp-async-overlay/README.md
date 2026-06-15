# py-stub-mcp-async-overlay (GREEN — D24/G11)

Async-MCP RUNTIME test-lane overlay for the Python backend (D24/G11).

This directory is the async sibling of `test/py-stub-mcp-overlay/`. It is layered
on top of a `py-stub` copy by the `test-gen-python-mcp-async` mdl action, which
generates the `mcp-stub-ok` model with BOTH `--py-generate-mcp-server=true` AND
`--py-async-services=true`. The `test-python-mcp-async` action then round-trips the
generated async MCP server against the test target placed here.

Convention (shared by cs/py/rs/ts/sw):
- sync lane  : `test-gen-<lang>-mcp`       + `test-<lang>-mcp`       (overlay `test/<lang>-stub-mcp-overlay/`)
- async lane : `test-gen-<lang>-mcp-async` + `test-<lang>-mcp-async` (overlay `test/<lang>-stub-mcp-async-overlay/`)

## Status: GREEN — async server awaits the delegate (T61)

`BaboonTests/mcp/test_mcp.py` is a RUNTIME round-trip for the async MCP server.
Under `--py-async-services=true` the generated errors-mode wiring entry
`invoke_json_McpTools` is an `async def` (PyServiceWiringTranslator.scala:36-39,
:793). T61 threads `asyncServices` into `PyMcpServerGenerator`: under the flag
the generated `McpToolsMcpServer` extends `AbstractAsyncBaboonMcpServer`, its
`invoke_json` is an `async def` that `await`s the delegate, and the inherited
async `handle` is a coroutine that awaits the dispatch. The integrator awaits
the coroutine returned by `handle`.

`test_ping_returns_ok_true_async` awaits a real `tools/call` and asserts on the
AWAITED JSON result (`ok == True`) — no coroutine object, no
`RuntimeWarning: coroutine ... was never awaited`. Control tests pin that the
wiring entry and the server's `handle` are both coroutine functions
(`inspect.iscoroutinefunction`) and that a directly-awaited dispatch yields
`{"ok": true}`.

The defect (D24) was: pre-T61 the generated server called the `async def`
delegate SYNCHRONOUSLY (PyMcpServerGenerator.scala:147 → baboon_mcp_runtime.py
sync `handle`), silently returning an un-awaited coroutine object (neither
`BaboonRight` nor `BaboonLeft`). The sync `test-py-mcp` lane is unaffected: with
the flag OFF the generated server is byte-identical to baseline.
