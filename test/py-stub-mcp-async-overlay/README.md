# py-stub-mcp-async-overlay (RED red-repro — D24/G11)

Async-MCP RUNTIME test-lane overlay for the Python backend (D24/G11).

This directory is the async sibling of `test/py-stub-mcp-overlay/`. It is layered
on top of a `py-stub` copy by the `test-gen-python-mcp-async` mdl action, which
generates the `mcp-stub-ok` model with BOTH `--py-generate-mcp-server=true` AND
`--py-async-services=true`. The `test-python-mcp-async` action then round-trips the
generated async MCP server against the test target placed here.

Convention (shared by cs/py/rs/ts/sw):
- sync lane  : `test-gen-<lang>-mcp`       + `test-<lang>-mcp`       (overlay `test/<lang>-stub-mcp-overlay/`)
- async lane : `test-gen-<lang>-mcp-async` + `test-<lang>-mcp-async` (overlay `test/<lang>-stub-mcp-async-overlay/`)

## Status: RED — un-awaited-coroutine repro (T60), gates the fix (T61)

`BaboonTests/mcp/test_mcp.py` is a RUNTIME red-repro for the silent un-awaited
coroutine defect (D24): Python's MCP delegate type is dynamic
(`Callable[..., object]`), so the mismatch does NOT fail to compile. Under
`--py-async-services=true` the generated errors-mode wiring entry
`invoke_json_McpTools` is an `async def` (PyServiceWiringTranslator.scala:36-39,
:793), but the generated `McpToolsMcpServer` calls the delegate SYNCHRONOUSLY
(PyMcpServerGenerator.scala:147 → baboon_mcp_runtime.py `handle`). Calling the
`async def` without `await` SILENTLY returns a coroutine object that is never
awaited.

`test_ping_returns_ok_true_async` performs a real `tools/call` and asserts on the
AWAITED JSON result. It FAILS today: the coroutine is neither `BaboonRight` nor
`BaboonLeft`, so the sync dispatch raises on the coroutine (or emits a coroutine
repr as a non-JSON Channel-B body) and Python logs
`RuntimeWarning: coroutine 'invoke_json_McpTools' was never awaited`. The
assertion pinpoints the symptom (NOT an import/harness error). Two control tests
(`inspect.iscoroutinefunction` on the wiring entry; an `asyncio.run`-awaited
dispatch yielding `{"ok": true}`) confirm the stub/codec are correct and isolate
the defect to the server's missing `await`.

This lane stays RED until the Python async-MCP backend fix (T61) threads
`asyncServices` into the MCP server generator (async server base that awaits the
delegate). The sync `test-py-mcp` lane is unaffected.
