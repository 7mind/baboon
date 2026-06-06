---
ledger: reviews
counters:
  milestone: 0
  item: 42
archives:
  - id: M5
    path: ./archive/reviews/M5.md
    summary: "MCP gen W4 closeout COMPLETE: all 9 test-gen-<lang>-mcp + test-<lang>-mcp lanes registered in the `test` aggregator (T19); --<lang>-generate-mcp-server exposed in the playground for all 9 langs (T20, also closed a latent BaboonJS hardcoded-false gap); D9 regression fixed by relocating mcp-stub-ok out of the shared model-dir + a minimal recursive Tree model (T24, D9 resolved); full CI closeout green — `mdl --seq :build :test` (114 actions) + the :ci-only delta (smoke/test-acceptance 200/200/test-editors 80/80/test-service-acceptance/jv-client-roundtrip) all GREEN, GraalVM native-image green, off-by-default byte-identity verified (T21). All items terminal (T19/T20/T21/T24 done; D9 resolved; H2 confirmed; reviews R25/R26/R28/R29 go-ahead)."
    title: MCP gen W4 — test-matrix integration, playground & CI
    status: done
  - id: M2
    path: ./archive/reviews/M2.md
    summary: MCP gen W1 (design/dispatch-contract/inputSchema-decision) COMPLETE. Tasks T1-T5,T22 + D1/D2 fixes (T26,T27) done; decisions K1/K3/K4/K5/K6 locked; defects D1 (refuted→verified, T26) + D2 (contract-field validator guard, T27) resolved; hypotheses H3(wrong)/H4(confirmed) terminal; reviews terminal. All items terminal.
    title: MCP gen W1 — design, dispatch contract & inputSchema decision
    status: done
  - id: M3
    path: ./archive/reviews/M3.md
    summary: MCP gen W2 (flags all 9 + reference backends TS & C#) COMPLETE. Tasks T6-T11 + D3 fix (T28) done; defect D3 (TS import determinism, T28) resolved; hypothesis H5 confirmed; reviews terminal. All items terminal.
    title: MCP gen W2 — flags (all 9) + reference backends TS & C#
    status: done
  - id: M4
    path: ./archive/reviews/M4.md
    summary: MCP gen W3 (replicate to remaining 7 backends) COMPLETE. Tasks T12-T18,T23 + D4/D5/D6/D7/D10 fixes (T29,T30,T31,T32) done; defects D4,D5,D6,D7,D8,D10 resolved; hypotheses H1,H2,H6,H7,H8,H9,H10 terminal; reviews terminal. All items terminal.
    title: MCP gen W3 — replicate generator to remaining 7 backends
    status: done
  - id: M6
    path: ./archive/reviews/M6.md
    summary: MCP gen W5 (D1-D11 fix closeout) COMPLETE. Tasks T33 (final CI closeout — mdl :build :test + :ci GREEN on combined main) + T34 (D11 overlay-stub fix) done; defect D11 resolved; reviews R37-R42 terminal. All items terminal.
    title: MCP gen W5 — D1-D10 fix closeout
    status: done
---

# reviews

## M1

### R1 — revise

- createdAt: 2026-06-04T18:10:10.737Z
- updatedAt: 2026-06-04T18:10:39.692Z
- author: "opus-4.8[1m]"
- session: 9fa3b56d-80fc-4604-bf8e-7daadaf5957b
- summary: "Plan is well-grounded, DAG-correct, and complete on Q1-Q3/Q5; three planner-fixable defects: T5<->T7 fixture circularity, an unowned flag->generator dispatch seam, and a missing JSON-Schema-validity gate for the Q4 inputSchema caveat."
- new_questions: []
- criticism: ["T5<->T7 circular intent: T5 (implement the inputSchema emitter) has acceptance 'produces valid JSON Schema for every Baboon construct used by the MCP test stub model', but the stub model is authored in T7 which dependsOn [T3, T5] -- i.e. AFTER T5. The fixture T5 is graded against does not exist when T5 runs. Re-sequence: author the MCP stub model (or at least lock its type inventory covering DTOs/ADTs/enums/collections/options/foreign/aliases/recursive) as a dependency of T5, or split the stub-model authoring out of T7 to land before T5.","No task owns the seam that reads the --<lang>-generate-mcp-server flag and dispatches to the MCP generator in the per-language BaboonAbstractTranslator invocation path. T6's acceptance explicitly says 'the flag only gates whether MCP output is emitted; no generator logic yet ... emits nothing or a clearly-stubbed nothing', and T8 says 'with --ts-generate-mcp-server=true the compiler emits a per-service TS MCP server' -- but the flag-read -> generator-call wiring is implied and unassigned. Clarify whether the dispatch seam is part of T6 (per-lang translator entrypoint), part of each per-lang generator task (T8/T10/T12-T18), or a distinct task; pick one so it is not dropped.","Test rigor gap vs Q4's explicit 'schema machinery is demo-grade' caveat: no task validates that each emitted tool inputSchema is WELL-FORMED JSON Schema against a real validator. The per-language round-trips (T9/T11/T12-T18) assert tool names + dispatch + results; T5 asserts a JVM golden/shape; T7 asserts inputSchemas 'match expectations'. None runs the emitted inputSchema through a JSON-Schema validator the way the existing test-openapi (swagger-parser) and test-graphql (graphql-js buildSchema) lanes validate their output. Add an inputSchema-validity assertion (validator-backed) to T5 or the round-trip, since MCP clients validate these schemas at tools/list time."]
- ledgerRefs: ["goals:G1"]
- sessionLogs: ["docs/logs/20260604-181028-a017b827abba65462.md"]

### R2 — revise

- createdAt: 2026-06-04T18:14:42.341Z
- updatedAt: 2026-06-04T18:15:11.912Z
- author: "opus-4.8[1m]"
- session: 9fa3b56d-80fc-4604-bf8e-7daadaf5957b
- summary: "All three R1 criticisms genuinely resolved (T22 breaks the T5<->T7 cycle -> DAG acyclic; T6 owns the flag->generator dispatch seam; validity gate now validator-backed at T5/T9/T11). One residual planner-fixable defect introduced by the asymmetric fix: the inputSchema-validity gate covers only the two reference backends (T9 ts/ajv, T11 cs) plus the JVM emitter (T5), but none of the 7 replication tasks T12-T18."
- new_questions: []
- criticism: ["Validity-gate asymmetry across the 7 replication backends. R1 criticism 3 was fixed by adding a validator-backed inputSchema check to T5 (JVM emitter), T9 (ts/ajv) and T11 (cs/NJsonSchema|JsonSchema.Net) -- mirroring the existing test-openapi (swagger-parser) and test-graphql (graphql-js buildSchema) lanes, both confirmed present in .mdl/defs/tests.md and registered in the `test` aggregator. But T12-T18 (scala, rust, kotlin, java, dart, swift, python) carry NO per-language inputSchema-validity assertion -- their acceptance only asserts initialize/tools-list/tools-call + error + negative control. R1's stated rationale was 'MCP clients validate these schemas at tools/list time': the per-language round-trip is the analog of a real client, and serializing the shared-emitter schema through each language's own JSON codec (key escaping, number/$ref/additionalProperties/nested-map rendering) is exactly the per-language failure mode T5's JVM validator cannot catch and the round-trip is positioned to. Either extend the per-language round-trip acceptance for T12-T18 to validate each returned tool inputSchema against a real JSON-Schema validator in that toolchain (parity with T9/T11), or record a locked decision that the shared-emitter JVM validation in T5 plus the two reference round-trips is sufficient coverage and the 7 replicas deliberately assert dispatch/result only -- so the omission is a documented choice rather than a silent gap."]
- ledgerRefs: ["goals:G1"]
- sessionLogs: ["docs/logs/20260604-181459-aa305eb8dea28d846.md"]

### R3 — go-ahead

- createdAt: 2026-06-04T18:18:25.551Z
- updatedAt: 2026-06-04T18:18:51.927Z
- author: "opus-4.8[1m]"
- session: 9fa3b56d-80fc-4604-bf8e-7daadaf5957b
- summary: "R2's sole criticism genuinely resolved: K1 (locked) documents the validator-scope decision AND T12-T18 now each assert per-language codec well-formed parse + structural equality to the T7 reference (validator-gated at T5/T9/T11), closing the codec-rendering-divergence gap without a full validator in Swift/Dart-class toolchains. DAG acyclic, T7 reference reachable on every replica's dep chain, revision surgical, no new defect. Plan is fine-grained, sequenced, testable, grounded, complete -> executable."
- new_questions: []
- criticism: []
- ledgerRefs: ["goals:G1"]
- sessionLogs: ["docs/logs/20260604-181841-ae7ac3159bebd0f7e.md"]

### R22 — go-ahead

- createdAt: 2026-06-05T09:18:22.104Z
- updatedAt: 2026-06-05T09:18:58.505Z
- author: "opus-4.8[1m]"
- session: 91173875-8554-45c0-a418-1837a6648f6f
- summary: "Follow-up delta approved: T23 faithfully reifies D8's source-verified root cause + suggestedFix with 5 concrete/testable acceptance criteria (swift build clean, FULL swift matrix incl. wrapped+compat/manual, sbt +compile cross-build, recursive-DTO round-trip with unconditional throw + negative control); DAG acyclic and correctly sequenced (T23->T17->{T19,T20,T21}); rewiring surgical (T17 blocked->planned, dependsOn [T11,T23]; D8.dependsOn [T23]; all done/approved tasks T1-T16/T18/T22 untouched); regression scope adequate for a change altering generated Swift for ALL recursive-DTO users."
- new_questions: []
- criticism: []
- ledgerRefs: ["goals:G1"]
- sessionLogs: ["docs/logs/20260605-091840-aa2dc3ee3919a4d9d.md"]

### R27 — go-ahead

- createdAt: 2026-06-05T12:30:55.463Z
- updatedAt: 2026-06-05T12:31:35.412Z
- author: "opus-4.8[1m]"
- session: 91173875-8554-45c0-a418-1837a6648f6f
- summary: "D9 follow-up delta approved: T24 faithfully reifies D9's confirmed root cause (H2). Relocation provably removes mcp-stub-ok from the wiring/regular/wrapped (+graphql/openapi) --model-dir scan (rs-result tests.md:1928, rs-outcome:1978, swift:2374 all use the shared root + --fixture-output; the 8 failing lanes generate the stub's unqualified cross-namespace fixtures). The minimal recursive-only Tree{value;left;children} carries NO cross-namespace ref so its fixture compiles clean; T23's recursive coverage uses ONLY Tree (RecursiveDtoRoundTripTests.swift L31-35, never Color/Page/FFancyStr), so a Tree-only module preserves it. The 7 named lane line-numbers (ts:1611,cs:1665,rust:1722,kotlin:1775,java:1829,scala:2633,python:2687) all verified exact; dart/swift mcp lanes covered by set ('the 9 lanes'); Package.swift McpStub target + RuntimeTests dep rewire covers all swift lanes (single checked-in test/sw-stub source rsync'd by every swift lane). Test-infra-only scoping sound: D4/D5 generator bugs stay parked on Q7, relocation removes them from the CI path without a generator fix. DAG acyclic and correctly sequenced (T23 done -> T24 [dependsOn T23] -> T21 [dependsOn T19,T20,T24], T21 blocked->planned; D9.dependsOn [T24]). Rewiring surgical: done tasks T1-T20/T22/T23 untouched. Acceptance concrete/testable (8 named wiring lanes compile+pass, 9 mcp lanes pass against relocated dir, swift recursive round-trip green, full mdl :test/:ci green, off-by-default byte-identity, diff confined to test resources + tests.md + sw-stub). Plan executable."
- new_questions: []
- criticism: []
- ledgerRefs: ["goals:G1"]
- sessionLogs: ["docs/logs/20260605-123113-a05180c725462cb80.md"]

### R30 — go-ahead

- createdAt: 2026-06-05T18:14:03.157Z
- updatedAt: 2026-06-05T18:14:54.589Z
- author: "opus-4.8[1m]"
- session: 91173875-8554-45c0-a418-1837a6648f6f
- summary: "D1-D7 fix delta (T26-T31) approved: each task faithfully reifies its defect's confirmed root cause + suggestedFix with concrete testable acceptance (named-workaround removal + mdl :build :test/:ci green); spot-verified against source (McpInputSchemaEmitter :169 silent-empty + :209-212 oneOf-only; Py mkJsonKey{En,De}coder :317-345/:396-439 no Enum arm). T26 correctly VERIFY-not-allOf per the D1 refutation; D4+D5 soundly folded into T29 (shared codecName idiom, both linked); T30 addresses the schema-vs-codec reconciliation not just the missing arm; T31 carries the Q7-approved Python baseline-change acknowledgment. dependsOn:[] correct (feature fully built T1-T24; fixes touch independent files/generators, parallelizable, no write conflict on the two Python tasks). Q20 bidirectional links verified (task->defect+G1; defect.dependsOn->task); T1-T24 untouched. Fine-grained, sequenced, testable, grounded, complete."
- new_questions: []
- criticism: []
- ledgerRefs: ["goals:G1"]
- sessionLogs: ["docs/logs/20260605-181428-aa71710eba6e730ce.md"]

### R37 — go-ahead

- createdAt: 2026-06-05T20:24:28.642Z
- updatedAt: 2026-06-05T20:25:04.501Z
- author: "opus-4.8[1m]"
- session: 91173875-8554-45c0-a418-1837a6648f6f
- summary: "FOLLOW-UP SCOPE 4 delta (T32 D10 fix + T33 final CI closeout) approved. T32 correctly scoped per the source-verified D10 refutation: NO behavior bug exists (OasTypeTranslator.mapSchema:149-151 matches enum keys via enumKeys.contains(id); resolveTypeRef:33-43 recurses into Constructor args at 40-41; OasBaboonTranslator.renderDto:165 pre-resolves every field tpe; line 177 is the SOLE typeRefSchema call site, operating on the pre-resolved fields) -> a foreign-rt-to-enum map key already reaches mapSchema resolved to the enum, same string-keyed-object form as MCP. So T32 is coverage + OPTIONAL hardening, NOT a blind divergence fix. Coverage gap VERIFIED real: test-gen-openapi (tests.md:2589) generates ONLY from --model-dir resources/baboon/, NOT mcp-stub-ok/; the sole enum-keyed map map[Color,str] lives in mcp-stub-ok/mcp_stub.baboon; zero propertyNames in the OAS golden -> OasTypeTranslator:149-151 branch genuinely unexercised by OAS. T32 acceptance is concrete/testable (asserts exact type:object+additionalProperties+propertyNames{enum:[wire-values]} shape, parity with MCP/T30; ideally also a foreign-rt-to-enum key pinning the renderDto:165 pre-resolution invariant). T33 is the right combined-state gate (analogous to T21), dependsOn [T32] correct, under valid open milestone M6 (M5 archived). mdl :ci (actions.md:392-409 = build+test+smoke+test-editors+test-acceptance+test-service-acceptance+test-jv-client-roundtrip) + mdl :build :test in T33 acceptance DO encompass the cross-language acceptance matrices + python recursive coverage on the merged HEAD that the per-task /tmp-clone runs did not -> the raised gap is already covered. DAG acyclic + sequenced (T32->T33); bidirectional D10<->T32 link present (D10.dependsOn=[T32]; T32.ledgerRefs=[defects:D10,goals:G1]); T1-T31 untouched; both milestones open/valid. Fine-grained, sequenced, testable, grounded, complete."
- new_questions: []
- criticism: []
- ledgerRefs: ["goals:G1"]
- sessionLogs: ["docs/logs/20260605-202447-ae7e7b1f3c135dd76.md"]

### R39 — revise

- createdAt: 2026-06-05T21:27:01.365Z
- updatedAt: 2026-06-05T21:27:42.453Z
- author: "opus-4.8[1m]"
- session: 91173875-8554-45c0-a418-1837a6648f6f
- summary: "T34 faithfully reifies D11's root cause and the T33 rewire/DAG/links are correct, but T34's scope+acceptance are incomplete: adding the processTagged interface impl fixes the compile error yet leaves the overlay round-trip ASSERTIONS (hard-coded tool-count=5 + exact 5-name tools/list order) stale, so the lanes would still fail at runtime — one planner-fixable criticism."
- new_questions: []
- criticism: ["T34 under-scopes the fix: it instructs only 'add a trivial processTagged handler' to each overlay stub, but the per-language MCP round-trip tests assert a HARD-CODED tool count and exact tools/list order for the pre-processTagged 5-tool set. Verified in test/cs-stub-mcp-overlay/McpTests/McpTests.cs: StubMcpTools (L74-90) implements exactly 5 IMcpTools methods (the CS0535 D11 confirms) AND Sec2_ToolsList_ExactlyFiveToolsInDeclarationOrder asserts Assert.AreEqual(5, tools.Count) (L242) plus exact position names tools[0..4] (L247-251, name 'McpTools_ping' at index 4). Since T26 added processTagged to the SHARED mcp_stub.baboon, the generated server now advertises 6 tools. Merely adding the ProcessTagged interface method clears the compile error but the round-trip then FAILS at runtime on the count==5 / 5-name-order assertions. Per the R3/K1 design every statically-typed replica (scala/rust/kotlin/java/dart/swift/ts) mirrors this T7 scenario, so all carry analogous count/order assertions, and the python overlay (if it asserts tool count) fails the same way — contradicting T34's framing of python as failing 'only if dispatched'. FIX: extend T34's scope+acceptance to ALSO update, in every overlay round-trip test, the tool-count assertion (5->6) and the tools/list name/order assertions (insert McpTools_processTagged at its model-declaration-order position) and add the matching tools/call success assertion if the scenario exercises per-tool calls — so 'all 9 test-<lang>-mcp lanes compile AND pass' is actually achievable, not just compilable."]
- ledgerRefs: ["goals:G1"]
- sessionLogs: ["docs/logs/20260605-212409-a37405d946f4f55d7.md"]

### R40 — go-ahead

- createdAt: 2026-06-05T21:31:46.698Z
- updatedAt: 2026-06-05T21:32:21.783Z
- author: "opus-4.8[1m]"
- session: 91173875-8554-45c0-a418-1837a6648f6f
- summary: "R39's sole criticism genuinely resolved: revised T34 now scopes BOTH the compile fix (add processTagged handler ok:true) AND the runtime-assertion fix (count 5->6; tools/list name/order with McpTools_processTagged at its model-declaration-order position; per-tool tools/call success assertion) across all 9 named overlays, with acceptance mandating COMPILE AND PASS. Order caveat verified against repo: mcp_stub.baboon declares processTagged BETWEEN processShape and pagePoints (L145), so it lands at index 3 (0-based) per K4 model-declaration-order, shifting pagePoints->4 and ping->5 — T34 correctly warns 'do NOT assume appended last'. Links (defects:D11+goals:G1), D11.dependsOn=[T34], T33.dependsOn=[T32,T34]+planned all intact; DAG acyclic; T1-T32 and mcp_stub.baboon untouched. Plan executable."
- new_questions: []
- criticism: []
- ledgerRefs: ["goals:G1"]
- sessionLogs: ["docs/logs/20260605-212911-af344bc80212127f4.md"]
