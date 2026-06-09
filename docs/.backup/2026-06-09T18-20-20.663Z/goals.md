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
- updatedAt: 2026-06-05T21:33:35.973Z
- author: "opus-4.8[1m]"
- session: 91173875-8554-45c0-a418-1837a6648f6f
- title: MCP server generators across all backends
- description: |
    MCP server generators across all 9 backends. FEATURE COMPLETE + merged (main 7f2e7f38). Defects D1-D10 all RESOLVED. `mdl :build` (GraalVM native-image) + sbt +compile (JVM+Scala.js) GREEN on combined main.
    
    === FOLLOW-UP SCOPE 5 (2026-06-05, defect-seeded from D11; reopened) ===
    The final CI-closeout T33 (full `mdl --seq :build :test`) surfaced regression D11 (root-caused): T26 added the `processTagged` tool to the SHARED mcp_stub.baboon (to make a contract-bearing ADT reachable for the D1 verify assertion) but did NOT update the 9 hand-written per-language MCP round-trip OVERLAY stubs (test/<lang>-stub-mcp-overlay/), each of which implements the full generated service interface. So on combined main test-cs-mcp fails CS0535 (StubMcpTools missing ProcessTagged) and the other statically-typed overlay stubs (scala/rust/kotlin/java/dart/swift/ts) are broken too; python (duck-typed) may fail only if the tool is dispatched. `mdl :build` is green; this is a test-overlay-stub gap, not feature code.
    
    REQUIRED PLAN ACTIONS: (1) add ONE new task under M6: 'Fix D11: add processTagged tool stub to all 9 MCP overlay stubs' — add a trivial processTagged handler (returns out ok:true, mirroring existing tool stubs) to test/{cs,scala,rust,kotlin,java,dart,swift,ts}-stub-mcp-overlay/ (statically-typed) + test/py-stub-mcp-overlay/ (if needed); verify all 9 test-<lang>-mcp lanes compile+pass; links defects:D11 + goals:G1; set D11.dependsOn=[it]. (2) Rewire T33 (final CI closeout, currently blocked) dependsOn += the D11-fix task and move T33 blocked->planned so it re-runs the full `mdl :build :test`+`:ci` after D11 lands. (3) Leave all done tasks (T1-T32) untouched.
    
    GLOBAL ACCEPTANCE: D11 fix makes all 9 test-<lang>-mcp lanes green; T33 re-run achieves `mdl :build :test` + `mdl :ci` green on the combined main (off-by-default byte-identity except the Q7-approved Python future-import). After T33 lands + the user closes G1, the next /advance sweep archives the now-eligible milestones.
- sessionLogs: ["docs/logs/20260605-201951-a29694c0986aee0e7.md","docs/logs/20260605-202447-a7b89405ca6693325.md","docs/logs/20260605-212409-a01305099bb5e8c55.md","docs/logs/20260605-212911-aeb067ff3d048c5ab.md","docs/logs/20260605-213221-a9156397801dd87d3.md"]
- grounding: "Plan shaped by Q1-Q5 answers + repo mechanisms. SCOPE (Q1): per-Baboon-service MCP server; method=tool; v1 = initialize+tools/list+tools/call. TRANSPORT-ABSTRACT (Q2): protocol+dispatch only, injected transport, no I/O loop. FLAGS (Q3): --<lang>-generate-mcp-server, default false, 4 sites. CODEC/SCHEMA (Q4): reuse JSON codecs; inputSchema from request DTO; dedicated emitter (K3). TEST RIGOR (Q5): per-lang compile + local round-trip; test-gen-<lang>-mcp + test-<lang>-mcp. 9 langs done. ASSERTION HYGIENE: unconditional throws + negative control. [D9 follow-up: fix is test-infra only; generator codec/fixture import bugs D4/D5 stay parked on Q7.]"
- milestones: ["M2","M3","M4","M5","M6"]
