---
ledger: goals
counters:
  milestone: 0
  item: 1
archives: []
---

# goals

## M1

### G1 — planned

- createdAt: 2026-06-04T17:47:13.305Z
- updatedAt: 2026-06-04T18:21:12.767Z
- author: "opus-4.8[1m]"
- session: 9fa3b56d-80fc-4604-bf8e-7daadaf5957b
- title: MCP server generators across all backends
- description: "We need to support a new feature across all backends: MCP server generators. It should be hidden behind backend-specific flags, off by default. It should be included into test matrices. As our existing servers/clients generators, it should be transport-abstract."
- sessionLogs: ["docs/logs/20260604-174700-a43c3b428add53db3.md","docs/logs/20260604-180703-a44d25727418caa31.md","docs/logs/20260604-181234-a1eec76d19a4fe609.md","docs/logs/20260604-181711-a001fc5e3f837c2d5.md","docs/logs/20260604-182059-ae245961e0ce9b7e5.md"]
- grounding: |
    Plan shaped by Q1-Q5 answers + repo mechanisms (verified-but-dated project memory; implementers MUST re-verify exact file:line sites in W1/T1 before editing).
    
    SCOPE (Q1): per-Baboon-service MCP server; each service method = one MCP tool; v1 protocol surface = initialize + tools/list + tools/call (tools-only; resources/prompts deferred).
    
    TRANSPORT-ABSTRACT (Q2): generate protocol + tool-dispatch ONLY; transport (stdio/Streamable HTTP) is an injected adapter. Mirror the existing abstract-context server contract: per-invocation dispatch entrypoint (analogous to invoke(method,data,ctx,codecCtx) and IBaboon{Json,Ueba}ServiceCtx<Ctx,R>), additive runtime types, no baked-in I/O loop. JSON-RPC 2.0 framing.
    
    FLAGS (Q3): one boolean per language --<lang>-generate-mcp-server, default false. Threaded through the established 4 sites: (1) CompilerOptions.scala per-lang *Options case classes; (2) .jvm Baboon.scala + CLIOptions.scala; (3) .js BaboonJS.scala (the cross-build trap -- sbt +compile / test-sbt-basic catches a missed update here); (4) tests + playground options.ts langs[] array + compiler.ts bridge.
    
    CODEC/SCHEMA (Q4): reuse per-language JSON codecs for tool arg/result (de)serialization; UEBA out of scope (MCP is JSON-RPC, JSON-only). Derive each tool inputSchema from the method request DTO. CAVEAT from user: the existing translator/openapi JSON-Schema machinery was demo-grade and 'most likely won't be enough' -- W1 includes an explicit review-and-decide task (extend openapi schema emitter vs. write a dedicated MCP inputSchema emitter), recorded as a locked decision before per-language work begins.
    
    TEST RIGOR (Q5): per-language compile + LOCAL round-trip exercising the generated transport-abstract dispatch entrypoint through initialize/tools-list/tools-call (no real transport, no cross-language NxN). Wire as test-gen-<lang>-mcp (rsync stub+overlay, codegen with that lang's regular-adt flags verbatim incl. jv --jv-write-evolution-dict) + test-<lang>-mcp (build+run the MCP round-trip class) actions in .mdl/defs/tests.md, dep-registered in the `test` aggregator. Mirror the wiring-overlay harness pattern (test/<lang>-stub-mcp-overlay/), no-errors mode. Full cross-language MCP-client matrix is an explicit follow-up, NOT v1.
    
    LANGUAGES (9): scala, cs, python, rust, typescript, kotlin, java, dart, swift. Reference-first order: TS + C# first (established pattern), then replicate to the other 7.
    
    ASSERTION HYGIENE: round-trip value checks must be UNCONDITIONAL throws (asserts are vacuous in C#/Dart/Swift/JVM defaults); prove liveness with a negative control.
- milestones: ["M2","M3","M4","M5"]
