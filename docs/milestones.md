---
ledger: milestones
counters:
  milestone: 0
  item: 5
archives: []
---

# milestones

## active

### M-AMBIENT — open

- createdAt: 2026-06-04T17:28:06.627Z
- updatedAt: 2026-06-04T17:28:06.627Z
- title: ambient

### M1 — open

- createdAt: 2026-06-04T17:47:08.530Z
- updatedAt: 2026-06-04T17:47:08.530Z
- title: "Plan: MCP server generators across all backends"
- description: "Coordination milestone for the plan-flow goal: add transport-abstract MCP server generators across all 9 target languages, gated behind backend-specific flags (off by default), wired into the test matrices."

### M2 — open

- createdAt: 2026-06-04T18:01:48.098Z
- updatedAt: 2026-06-04T18:01:48.098Z
- title: MCP gen W1 — design, dispatch contract & inputSchema decision
- description: "Language-agnostic foundation for the MCP server generators: re-verify the repo grounding (exact *Options 4-site file:line, transport-abstract server contract, openapi JSON-Schema emitter), lock the MCP-scope/protocol-mapping decision (tools-only: initialize/tools-list/tools-call; service method -> MCP tool), design the transport-abstract MCP dispatch runtime contract (mirror IBaboon{Json,Ueba}ServiceCtx per-invocation invoke), and decide+lock how each tool inputSchema is derived (extend translator/openapi vs. dedicated MCP schema emitter -- the Q4 caveat). Gates all per-language work."

### M3 — open

- createdAt: 2026-06-04T18:01:53.669Z
- updatedAt: 2026-06-04T18:01:53.669Z
- title: MCP gen W2 — flags (all 9) + reference backends TS & C#
- description: Add the per-language --<lang>-generate-mcp-server boolean flag (default false) across all 9 languages via the established 4-site mechanism, then implement the MCP server generator for the two reference backends TypeScript and C# (per-service MCP server, tools-only protocol, transport-abstract dispatch entrypoint per the W1 contract, tool inputSchema per the W1 decision, JSON codec reuse). Includes the TS and C# local round-trip overlays/tests. Establishes the reviewable reference shape the other 7 languages replicate.
- dependsOn: ["M2"]

### M4 — open

- createdAt: 2026-06-04T18:01:58.328Z
- updatedAt: 2026-06-04T18:01:58.328Z
- title: MCP gen W3 — replicate generator to remaining 7 backends
- description: "Replicate the MCP server generator (matching the TS/C# reference shape from W2) to the other 7 backends: Scala, Rust, Kotlin, Java, Dart, Swift, Python. Each carries its own transport-abstract dispatch entrypoint, tool inputSchema emission, JSON codec reuse, and a local round-trip overlay/test. Per-language codegen gotchas apply (e.g. Java --jv-write-evolution-dict, Dart runtime-file relocation, Swift type-erasers / fixture fan-out)."
- dependsOn: ["M3"]

### M5 — open

- createdAt: 2026-06-04T18:02:04.937Z
- updatedAt: 2026-06-04T18:02:04.937Z
- title: MCP gen W4 — test-matrix integration, playground & CI
- description: "Wire the MCP feature into the test matrices and CI: register all 9 test-gen-<lang>-mcp + test-<lang>-mcp actions in the `test` aggregator in .mdl/defs/tests.md, expose the --<lang>-generate-mcp-server flag in the playground (options.ts langs[] + compiler.ts bridge for all 9), and prove the whole feature green under `mdl :build :test` and `mdl :ci` (incl. sbt +compile cross-build). Final closeout: feature off-by-default verified (no MCP output without the flag; NoMcp path byte-stable)."
- dependsOn: ["M4"]
