---
ledger: handoffs
counters:
  milestone: 0
  item: 3
archives: []
---

# handoffs

## M1

### HO1 — answers-required

- createdAt: 2026-06-04T17:50:09.725Z
- updatedAt: 2026-06-04T17:50:09.725Z
- author: "opus-4.8[1m]"
- session: 9fa3b56d-80fc-4604-bf8e-7daadaf5957b
- summary: "Bootstrapped plan-flow goal G1 (MCP server generators across all 9 backends — transport-abstract, behind per-backend flags off by default, wired into test matrices) under coordination milestone M1. Planner filed clarifying questions Q1–Q5 covering: generation unit + MCP capability/protocol scope (Q1), the transport-abstraction boundary given MCP's own stdio/HTTP transports (Q2), per-backend flag naming/shape + playground exposure (Q3), tool inputSchema derivation + authoritative codec / UEBA scope (Q4), and required test-matrix rigor (Q5). Goal left in `clarifying`; awaiting user answers before /plan:advance can emit a plan."
- flow: plan
- ledgerRefs: ["goals:G1"]
- blockingQuestions: ["Q1","Q2","Q3","Q4","Q5"]
- handoffReasons: ["First clarifying round filed; goal blocked on user answers to Q1–Q5"]
- sessionLogs: ["docs/logs/20260604-174700-a43c3b428add53db3.md"]

### HO2 — mixed

- createdAt: 2026-06-05T14:39:21.641Z
- updatedAt: 2026-06-05T14:39:21.641Z
- author: "opus-4.8[1m]"
- session: 91173875-8554-45c0-a418-1837a6648f6f
- summary: |
    MIXED stop. This /advance run drove the MCP-server feature (goal G1) to functional completion across all 9 backends AND landed two confirmed-defect fixes, then stopped because the 7 pre-existing/out-of-scope defects D1-D7 are blocked solely on the unanswered disposition question Q7.
    
    WHAT LANDED THIS RUN (all merged to main, HEAD f215f8de):
    - D8 (Swift recursive value-type DTO codegen) root-caused (H1 confirmed) + FIXED via T23 (@BaboonIndirect reference-box boxing of recursive DTO fields) -> D8 resolved; unblocked T17.
    - T17 (Swift MCP server) rebased onto the D8 fix + merged -> all 9 MCP backends now complete (ts, cs, scala, rust, kotlin, java, dart, swift, python).
    - T18 (Python MCP) ledger-reconciled to done (its work was already on main).
    - T19 (test-aggregator registration) verified; T20 (playground --<lang>-generate-mcp-server checkboxes for all 9 langs, + closed a latent BaboonJS hardcoded-false gap).
    - T21 (CI closeout) first run SURFACED regression D9 (mcp-stub-ok in the shared test model-dir broke 8 wiring lanes' fixture compilation); D9 root-caused (H2 confirmed) + FIXED via T24 (relocated the stub out of the shared dir + added a minimal recursive Tree model to preserve coverage) -> D9 resolved. T21 re-ran GREEN: full `mdl --seq :build :test` (114 actions) + the :ci-only delta (smoke / test-acceptance 200/200 / test-editors 80/80 / test-service-acceptance / jv-client-roundtrip) all green; GraalVM native-image green; off-by-default byte-identity verified; CLAUDE.md documented the new flag-family/harness patterns.
    - Milestone M5 (W4 closeout) archived.
    
    WHAT REMAINS (blocked on the user): D1-D7 are pre-existing/out-of-scope codegen defects (D1/D2 MCP inputSchema latent gaps; D3 TS nondeterministic import order; D4/D5 Scala unqualified/unresolved imports; D6 Python enum-keyed-map codec; D7 Python recursive future-import). Each fix changes versioned external-API generated output, and each was reviewer-flagged 'candidate for a separate fix or wontfix'. Their disposition is a user decision (mirroring D8's Q6), filed as question Q7. They are off the CI path (CI is green) so they do not block shipping the MCP feature.
    
    NEXT ACTION: answer Q7 in the TUI/web (per-defect fix-vs-wontfix), then re-run /advance to fold the answers in (fixes would be planned + implemented; wontfixes marked terminal). GOAL G1 is `planned` with all tasks done and ready for the user to close (building->done is the user's action).
- flow: advance
- ledgerRefs: ["goals:G1","defects:D1","defects:D2","defects:D3","defects:D4","defects:D5","defects:D6","defects:D7"]
- blockingQuestions: ["Q7"]
- handoffReasons: ["drained","answers-required"]
- sessionLogs: ["docs/logs/20260605-090917-aca6d3712dc5065b7.md","docs/logs/20260605-091623-ad4928753fb7373a7.md","docs/logs/20260605-091840-aa2dc3ee3919a4d9d.md","docs/logs/20260605-091840-a4a6bae4444a8889b.md","docs/logs/20260605-110151-a8b454bdb3a4908d2.md","docs/logs/20260605-111100-a4882ff98e5f4468b.md","docs/logs/20260605-112814-a165d4ba692ec7ac8.md","docs/logs/20260605-114251-a5fed3343bbfbb782.md","docs/logs/20260605-115000-a15ee35d0e612937e.md","docs/logs/20260605-115000-ab3a49328d35da2d9.md","docs/logs/20260605-115425-a0c324bfc41e5df72.md","docs/logs/20260605-122341-a4defd6a9945d84f7.md","docs/logs/20260605-122639-ae96d339e63d9d1af.md","docs/logs/20260605-123113-a05180c725462cb80.md","docs/logs/20260605-123113-a017ccc4179e814da.md","docs/logs/20260605-125628-a010d5e3c94b7964b.md","docs/logs/20260605-130755-a0c6977e67acf7fd0.md","docs/logs/20260605-140303-a98df87ffa3dd9d8f.md","docs/logs/20260605-143450-a22d5f65a6292fb4f.md"]

### HO3 — drained

- createdAt: 2026-06-05T23:57:25.196Z
- updatedAt: 2026-06-05T23:57:25.196Z
- author: "opus-4.8[1m]"
- session: 91173875-8554-45c0-a418-1837a6648f6f
- summary: |
    DRAINED. This /advance run (resumed after the user answered Q7 = 'fix all 7') fixed every remaining defect D1-D7, plus the two regressions the closeout surfaced (D10, D11), to full green CI. Nothing actionable remains: all defects terminal, all tasks done, no open questions, G1 locked for planning (planned).
    
    WHAT LANDED THIS RUN (all merged to main, HEAD 56096096):
    - D1 (MCP ADT common fields) — investigation REFUTED the claim (contract fields already merged into branch DTOs at typing); T26 added a contract-bearing ADT + a non-vacuous McpInputSchemaEmissionTest assertion empirically locking the correct behavior (no generator change). RESOLVED.
    - D2 (contract-as-field dangling $defs ref) — T27 added a BaboonValidator data-type guard rejecting a NonDataTypedef as a plain field/collection/any-underlying + a fail-fast throw in the MCP emitter + the 3 exhaustive-match sites (incl. Scala.js) + negative test. RESOLVED.
    - D3 (TS nondeterministic import order) — T28 made TsBaboonTranslator import emission totally lexically ordered at BOTH sites (renderTree + facade), keyed on the rendered import source, + a live determinism regression (2 criticism rounds). RESOLVED.
    - D4+D5 (Scala foreign/enum-key codec imports unresolved) — T29 routed all 3 ScJsonCodecGenerator decoder sites through codecName + removed the FFancyStrShim & listcollections package-object workarounds. RESOLVED.
    - D6 (Python enum-keyed-map codec) — T30 added the enum map-key arm (string-keyed object, mirroring Scala) + reconciled the MCP/OAS inputSchema for enum-keyed maps to the string-keyed-object form across all 9 overlays + golden (validated by cs NJsonSchema + ts ajv gates). RESOLVED.
    - D7 (Python recursive pydantic NameError) — T31 emitted `from __future__ import annotations` unconditionally in PyBaboonTranslator.renderTree + removed the harness shim (Q7-approved flag-off Python baseline change). RESOLVED.
    - D10 (OAS enum-keyed-map parity) — investigation REFUTED the divergence (OAS pre-resolves foreign-rt map keys at renderDto:165); T32 added OasEnumKeyMapSchemaTest pinning the string-keyed-object branch + the pre-resolution invariant (mutation-confirmed, 1 criticism round). RESOLVED.
    - D11 (combined-state regression: T26's processTagged tool broke the 9 MCP overlay stubs) — surfaced by the T33 closeout; T34 added the processTagged stub + updated tool-count (5->6)/tools-list-order/tools-call assertions across all 9 overlays (1 criticism round). RESOLVED.
    - FINAL CI CLOSEOUT (T33) GREEN on combined main: `mdl --seq :build :test` (114/114, exit 0) + `mdl --seq :ci` (120/120, exit 0) — GraalVM native-image, sbt +compile JVM+Scala.js, all 9 test-<lang>-mcp lanes, test-acceptance 200/200, test-service-acceptance 162/162 (9x9x2), test-editors 80/80. Off-by-default byte-identity holds; the ONLY off-by-default delta is the Q7-approved Python future-import (D7).
    - Milestones M2/M3/M4/M6 archived (M5 archived in the prior run) — all 5 of G1's work milestones now archived.
    
    FLOW PROCESS NOTE (per the user's feedback this session): NO disposition questions were filed; every confirmed defect was driven investigate->plan->implement to a fix (D1/D10 fixes were test-coverage/verification where investigation refuted the bug). A bug report for the prompt loopholes that previously caused a disposition-question stop is at docs/drafts/20260605-1747-advance-disposition-stop-bugreport.md.
    
    NEXT ACTION (user, not the flow): GOAL G1 is `planned` with all 34 tasks done and all 11 defects resolved — close it in the TUI/web (set status building->done). Goals never auto-close. Once you do, the next /advance sweep archives G1's now-eligible coordination milestone M1 automatically. No questions to answer; the run is complete.
- flow: advance
- ledgerRefs: ["goals:G1"]
- sessionLogs: ["docs/logs/20260605-180334-a4b97789b18f7398f.md","docs/logs/20260605-180334-a5a4414aaf1340e2c.md","docs/logs/20260605-180334-a70ee77bb43014030.md","docs/logs/20260605-180334-a90489e269db1b75f.md","docs/logs/20260605-180334-a2d685b9d26f843b9.md","docs/logs/20260605-180334-a0ec413510e4eb3f5.md","docs/logs/20260605-180334-acb345f878fd62012.md","docs/logs/20260605-190539-a3e3187638022cd76.md","docs/logs/20260605-190539-a0520f280b97e5b31.md","docs/logs/20260605-190539-aad62e89463bc09be.md","docs/logs/20260605-190539-a910752312cc9761b.md","docs/logs/20260605-190539-a8e4fc2c9b8094a16.md","docs/logs/20260605-190539-a24401ae895125d77.md","docs/logs/20260605-201632-acc5506e1d98acebd.md","docs/logs/20260605-204502-a38bdcc823454fcbf.md","docs/logs/20260605-212055-af72332c7b913602c.md","docs/logs/20260605-220537-ad7a09c8c879278d6.md","docs/logs/20260605-234830-a7e0e5511478a5a39.md","docs/logs/20260605-234830-a7870f9e7b0234afb.md"]
