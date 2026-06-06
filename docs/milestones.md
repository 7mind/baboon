---
ledger: milestones
counters:
  milestone: 0
  item: 6
archives:
  - id: M5
    path: ./archive/milestones/M5.md
    summary: "MCP gen W4 closeout COMPLETE: all 9 test-gen-<lang>-mcp + test-<lang>-mcp lanes registered in the `test` aggregator (T19); --<lang>-generate-mcp-server exposed in the playground for all 9 langs (T20, also closed a latent BaboonJS hardcoded-false gap); D9 regression fixed by relocating mcp-stub-ok out of the shared model-dir + a minimal recursive Tree model (T24, D9 resolved); full CI closeout green — `mdl --seq :build :test` (114 actions) + the :ci-only delta (smoke/test-acceptance 200/200/test-editors 80/80/test-service-acceptance/jv-client-roundtrip) all GREEN, GraalVM native-image green, off-by-default byte-identity verified (T21). All items terminal (T19/T20/T21/T24 done; D9 resolved; H2 confirmed; reviews R25/R26/R28/R29 go-ahead)."
    title: MCP gen W4 — test-matrix integration, playground & CI
    status: done
  - id: M2
    path: ./archive/milestones/M2.md
    summary: MCP gen W1 (design/dispatch-contract/inputSchema-decision) COMPLETE. Tasks T1-T5,T22 + D1/D2 fixes (T26,T27) done; decisions K1/K3/K4/K5/K6 locked; defects D1 (refuted→verified, T26) + D2 (contract-field validator guard, T27) resolved; hypotheses H3(wrong)/H4(confirmed) terminal; reviews terminal. All items terminal.
    title: MCP gen W1 — design, dispatch contract & inputSchema decision
    status: done
  - id: M3
    path: ./archive/milestones/M3.md
    summary: MCP gen W2 (flags all 9 + reference backends TS & C#) COMPLETE. Tasks T6-T11 + D3 fix (T28) done; defect D3 (TS import determinism, T28) resolved; hypothesis H5 confirmed; reviews terminal. All items terminal.
    title: MCP gen W2 — flags (all 9) + reference backends TS & C#
    status: done
  - id: M4
    path: ./archive/milestones/M4.md
    summary: MCP gen W3 (replicate to remaining 7 backends) COMPLETE. Tasks T12-T18,T23 + D4/D5/D6/D7/D10 fixes (T29,T30,T31,T32) done; defects D4,D5,D6,D7,D8,D10 resolved; hypotheses H1,H2,H6,H7,H8,H9,H10 terminal; reviews terminal. All items terminal.
    title: MCP gen W3 — replicate generator to remaining 7 backends
    status: done
  - id: M6
    path: ./archive/milestones/M6.md
    summary: MCP gen W5 (D1-D11 fix closeout) COMPLETE. Tasks T33 (final CI closeout — mdl :build :test + :ci GREEN on combined main) + T34 (D11 overlay-stub fix) done; defect D11 resolved; reviews R37-R42 terminal. All items terminal.
    title: MCP gen W5 — D1-D10 fix closeout
    status: done
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
