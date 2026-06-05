---
ledger: questions
counters:
  milestone: 0
  item: 5
archives: []
---

# questions

## M1

### Q1 — answered

- createdAt: 2026-06-04T17:48:22.726Z
- updatedAt: 2026-06-04T17:56:47.079Z
- author: user
- session: 9fa3b56d-80fc-4604-bf8e-7daadaf5957b
- question: "What exactly is the unit of MCP-server generation, and what subset of the Model Context Protocol must the generated server implement? Concretely: (a) does an MCP server get generated per Baboon service (the same `service Foo { def bar(...) }` declarations that drive the existing servers/clients generators), with each service method surfaced as an MCP `tool`? (b) Which MCP capabilities are in scope for v1 — tools only, or also resources / prompts / completions? (c) Which protocol surface — `initialize` handshake + `tools/list` + `tools/call` (the minimal tool-server), or more?"
- context: The existing servers/clients generators are driven by Baboon `service` declarations and emit per-service wiring (`<Svc>JsonService`/`<Svc>UebaService`) plus a cross-domain muxer. MCP (Model Context Protocol) is a JSON-RPC 2.0 protocol whose primary primitives are tools, resources, and prompts. Mapping Baboon service methods → MCP tools is the obvious correspondence, but the capability scope and which JSON-RPC methods must be generated are design decisions that determine the entire task breakdown and cannot be inferred from the repo.
- suggestions: ["Per-service MCP server, each method = one MCP tool; v1 = tools only (initialize + tools/list + tools/call)","Per-service, tools + resources + prompts (full MCP surface)","Per-domain single MCP server aggregating all services' methods as tools"]
- recommendation: Per-service MCP server, each method = one MCP tool; v1 scope = tools only (initialize + tools/list + tools/call), mirroring how the existing server generators map one service to one wiring unit. Resources/prompts can be a follow-up goal.
- ledgerRefs: ["goals:G1"]
- answer: as recommended

### Q2 — answered

- createdAt: 2026-06-04T17:48:33.154Z
- updatedAt: 2026-06-04T17:57:02.281Z
- author: user
- session: 9fa3b56d-80fc-4604-bf8e-7daadaf5957b
- question: "What does \"transport-abstract\" mean concretely for the MCP generator, given MCP's own transport model (stdio and Streamable HTTP) and JSON-RPC framing? Specifically: should the generated MCP server expose a transport-agnostic dispatch entrypoint (analogous to today's per-invocation `invoke(method, data, ctx, codecCtx)` / muxer contract) and leave the concrete stdio/HTTP transport to a hand-written or runtime-provided adapter — i.e. we generate the JSON-RPC method routing + tool dispatch, NOT the socket/stdio loop?"
- context: "The existing servers/clients generators were made transport-abstract via the `service.context=abstract` work: generic `<Ctx>` threaded through client/wrapper/runtime, with `IBaboon{Json,Ueba}ServiceCtx<Ctx,R>` runtime contracts and a per-invocation `invoke(...)` rather than a baked-in transport. MCP, however, defines its own transports (stdio, Streamable HTTP). I need to know where the abstraction boundary sits: generate only the protocol/dispatch layer (transport injected), or also generate concrete transport loops. This determines the runtime-contract surface and what the acceptance tests can exercise."
- suggestions: ["Generate protocol + tool-dispatch only; transport (stdio/HTTP) is an injected adapter, exactly mirroring the existing abstract-context server contract","Generate dispatch + a concrete stdio transport per language as the default adapter","Generate dispatch + both stdio and Streamable HTTP transports per language"]
- recommendation: Generate the protocol + tool-dispatch layer only, with the transport injected as an adapter — directly mirroring the existing transport-abstract server contract (per-invocation dispatch, no baked-in I/O loop). This keeps the new generator consistent with the established abstraction and keeps acceptance tests driving the dispatch entrypoint directly.
- ledgerRefs: ["goals:G1"]
- answer: as recommended

### Q3 — answered

- createdAt: 2026-06-04T17:48:42.556Z
- updatedAt: 2026-06-04T17:57:55.137Z
- author: user
- session: 9fa3b56d-80fc-4604-bf8e-7daadaf5957b
- question: How should the per-backend enabling flags be named and shaped, and should they be exposed in the playground? The repo's convention is a per-language `*Options` field (e.g. `TsOptions`, `KtOptions`) threaded through 4 sites (CompilerOptions.scala, Baboon.scala/CLIOptions.scala, BaboonJS.scala, tests) plus playground options.ts/compiler.ts. Do you want one boolean per language (e.g. `--ts-generate-mcp-server=false`, `--cs-generate-mcp-server=false`, ... for all 9), off by default, mirroring the existing per-language service flags?
- context: The goal mandates "backend-specific flags, off by default." The established mechanism is a per-language boolean option field added in 3 mandatory Scala sites (the BaboonJS.scala site is the cross-build trap) + a 4th playground site. I need the exact flag naming convention and confirmation that the per-language-boolean shape (not a single global flag) is what you want, plus whether to surface it in the playground (your stated preference has been to surface new flags there).
- suggestions: ["One boolean per language `--<lang>-generate-mcp-server`, default false, exposed in playground via the langs[] array","A single global `--generate-mcp-server` flag applying to all enabled language targets","Per-language boolean, CLI-only (not surfaced in the playground for v1)"]
- recommendation: "One boolean per language, named `--<lang>-generate-mcp-server` (e.g. `--ts-generate-mcp-server`), default false, added across all 4 option sites and surfaced in the playground via the options.ts `langs[]` array — consistent with the existing per-language service flag pattern and your standing preference to expose new flags in the playground."
- ledgerRefs: ["goals:G1"]
- answer: as recommended

### Q4 — answered

- createdAt: 2026-06-04T17:48:52.511Z
- updatedAt: 2026-06-04T17:59:16.400Z
- author: user
- session: 9fa3b56d-80fc-4604-bf8e-7daadaf5957b
- question: How should an MCP tool's input/output JSON Schema be derived, and which codec is authoritative? Each MCP tool advertises an `inputSchema` (JSON Schema) in `tools/list` and exchanges JSON arguments in `tools/call`. Baboon already has a JSON codec per language AND an OpenAPI/JSON-Schema-emitting backend (`translator/openapi`, GraphQL SDL too). Should the generated MCP server reuse the existing JSON codecs for argument/result (de)serialization, and reuse the OpenAPI/JSON-Schema machinery to produce each tool's `inputSchema` from the method's request type?
- context: MCP `tools/list` requires a JSON Schema for each tool's input, and `tools/call` is JSON-only (no binary). The repo already emits JSON Schema via the `openapi` backend and has per-language JSON codecs used by the existing JSON service wiring. Reusing both is the natural path and avoids a parallel schema generator, but whether the MCP generator should depend on / reuse the OpenAPI schema component and whether UEBA is entirely out of scope (MCP being JSON-RPC) are decisions that shape the task DAG and cross-backend dependencies.
- suggestions: ["Reuse existing JSON codecs for (de)serialization + reuse OpenAPI/JSON-Schema generator to derive each tool inputSchema; UEBA out of scope","Generate a fresh, MCP-specific JSON Schema emitter independent of the openapi backend","JSON only for transport, but hand-author tool schemas rather than deriving them from request types"]
- recommendation: Reuse the existing per-language JSON codecs for argument/result (de)serialization and reuse the OpenAPI/JSON-Schema machinery to derive each tool's `inputSchema` from the method's request DTO. Treat UEBA as out of scope (MCP is JSON-RPC). This avoids a second schema generator and keeps the feature consistent with the existing JSON wiring path.
- ledgerRefs: ["goals:G1"]
- answer: as recommended but our openapi/json-schema machinery is far from perfect, it was implemented for demo purposes mostly and most likely won't be enough - you might need to review it and decide to extend it or write some else, etc

### Q5 — answered

- createdAt: 2026-06-04T17:49:05.816Z
- updatedAt: 2026-06-04T17:59:42.378Z
- author: user
- session: 9fa3b56d-80fc-4604-bf8e-7daadaf5957b
- question: What should the test-matrix coverage for the MCP generators look like, and at what level must it verify behavior? Options range from (a) codegen-shape only — JVM unit tests asserting the generated MCP source compiles and has the right shape per language, mirroring the wiring overlay/`test-gen-<lang>` actions; (b) per-language compile + a local round-trip exercising `initialize`/`tools/list`/`tools/call` against the generated dispatch entrypoint (no real MCP client); (c) a full cross-language acceptance matrix analogous to the 9×9×2 service-acceptance harness, e.g. driving each generated server with a real MCP client/inspector over a transport.
- context: "The goal mandates inclusion \"into test matrices,\" but the existing matrices differ in rigor: per-language wiring overlays do compile+run a wiring test class, while `test/acceptance/run_service_acceptance.py` runs a 9x9x2 real-HTTP cross-language matrix. A real cross-language MCP matrix would require an MCP client/inspector and a transport per language (heavy, given MCP's stdio/HTTP transports), whereas a per-language dispatch round-trip is far cheaper and matches the transport-abstract dispatch boundary. The chosen rigor level materially changes the number and size of tasks and whether new harness infrastructure is needed."
- suggestions: ["Per-language: generated MCP source must compile + a local round-trip test drives the dispatch entrypoint through initialize/tools-list/tools-call (no real transport), wired as `test-<lang>-mcp` actions in the test aggregator","Codegen-shape only: JVM unit tests assert generated MCP output compiles and matches expected shape per language","Full cross-language acceptance matrix with a real MCP client over a transport, analogous to run_service_acceptance.py"]
- recommendation: Per-language compile + a local round-trip test that drives the generated transport-abstract dispatch entrypoint through `initialize` / `tools/list` / `tools/call` (no real transport, no cross-language NxN), wired as `test-gen-<lang>-mcp` + `test-<lang>-mcp` actions registered in the `test` aggregator — matching the wiring-overlay harness pattern. A full MCP-client cross-language matrix can be a follow-up if you want interop guarantees beyond per-language dispatch correctness.
- ledgerRefs: ["goals:G1"]
- answer: ""
