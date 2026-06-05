---
ledger: reviews
counters:
  milestone: 0
  item: 21
archives: []
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

## M2

### R4 — go-ahead

- createdAt: 2026-06-04T18:39:34.017Z
- updatedAt: 2026-06-04T18:39:34.017Z
- author: "opus-4.8[1m]"
- session: 9fa3b56d-80fc-4604-bf8e-7daadaf5957b
- summary: "T1 grounding note approved: 4 flag sites + OpenAPI emitter citations exact, discrepancies real; minor non-blocking 1-2 line drift in a few runtime/JS rows."
- criticism: []
- new_questions: []
- ledgerRefs: ["tasks:T1","goals:G1"]
- sessionLogs: ["docs/logs/20260604-183921-a3a486c7fedf930fb.md"]

### R5 — go-ahead

- createdAt: 2026-06-04T18:51:00.677Z
- updatedAt: 2026-06-04T18:51:00.677Z
- author: "opus-4.8[1m]"
- session: 9fa3b56d-80fc-4604-bf8e-7daadaf5957b
- summary: "T2 approved: MCP v1 protocol contract is spec-accurate (MCP 2025-06-18 + JSON-RPC 2.0), internally consistent, complete, and unambiguous for the 9 backends; K4 locked under M1 / goals:G1."
- criticism: []
- new_questions: []
- ledgerRefs: ["tasks:T2","goals:G1"]
- sessionLogs: ["docs/logs/20260604-185042-accf9595268f73098.md"]

### R6 — go-ahead

- createdAt: 2026-06-04T18:51:02.951Z
- updatedAt: 2026-06-04T18:51:02.951Z
- author: "opus-4.8[1m]"
- session: 9fa3b56d-80fc-4604-bf8e-7daadaf5957b
- summary: "T4 approved: evidence-backed MCP inputSchema gap analysis; locked decisions:K3 dedicated-emitter strategy grounded in reproduced + source-verified emitter behavior (dangling $ref scope mismatch)."
- criticism: []
- new_questions: []
- ledgerRefs: ["tasks:T4","goals:G1"]
- sessionLogs: ["docs/logs/20260604-185042-addb8396550df9d03.md"]

### R7 — go-ahead

- createdAt: 2026-06-04T19:04:28.561Z
- updatedAt: 2026-06-04T19:04:28.561Z
- author: "opus-4.8[1m]"
- session: 9fa3b56d-80fc-4604-bf8e-7daadaf5957b
- summary: "T3 approved: MCP dispatch runtime contract complete, internally consistent, and feasible across all 9 languages (Swift type-eraser/Python Generic/Rust/C# verified vs real source); K5 locked under M1/G1; entrypoint refines K4, consumes K3."
- criticism: []
- new_questions: []
- ledgerRefs: ["tasks:T3","goals:G1"]
- sessionLogs: ["docs/logs/20260604-190416-aec5b0bdad463cbe5.md"]

### R8 — go-ahead

- createdAt: 2026-06-04T19:15:16.416Z
- updatedAt: 2026-06-04T19:15:16.416Z
- author: "opus-4.8[1m]"
- session: 9fa3b56d-80fc-4604-bf8e-7daadaf5957b
- summary: "T22 approved: stub model compiles green under regular-adt flags (all 9 backends, independently re-run); all 11 K6 constructs reachable via McpTools tool requests; legal map keys. One out-of-scope cosmetic K6 headline count (8 vs 11) corrected directly by orchestrator."
- criticism: []
- new_questions: []
- ledgerRefs: ["tasks:T22","goals:G1"]
- sessionLogs: ["docs/logs/20260604-191459-a6da1814ad437ab91.md"]

### R9 — go-ahead

- createdAt: 2026-06-04T19:45:21.647Z
- updatedAt: 2026-06-04T19:45:21.647Z
- author: "opus-4.8[1m]"
- session: 9fa3b56d-80fc-4604-bf8e-7daadaf5957b
- summary: "T5 approved: real networknt Draft-2020-12 validity gate + golden assertions green (6/6, independently re-run); shared emitter fixes all T4 gaps with self-contained local $defs; cross-compiles to Scala.js; no scope creep. Two low-severity out-of-scope latent gaps filed as defects D1 (ADT common fields) + D2 (contract dangling ref)."
- criticism: []
- new_questions: []
- ledgerRefs: ["tasks:T5","goals:G1"]
- sessionLogs: ["docs/logs/20260604-194449-a76ca749b5bafa422.md"]

## M3

### R10 — go-ahead

- createdAt: 2026-06-04T20:16:28.892Z
- updatedAt: 2026-06-04T20:16:28.892Z
- author: "opus-4.8[1m]"
- session: 9fa3b56d-80fc-4604-bf8e-7daadaf5957b
- summary: "T6 approved: 9 MCP-server flags wired at all 4 sites (Options/CLI+Baboon/BaboonJS x9/tests x26), default false; per-language flag-read->stubbed-hook dispatch seam in each *BaboonTranslator.translate(); inert when off (byte-identical); cross-build sbt +compile independently re-verified green (JVM+Scala.js)."
- criticism: []
- new_questions: []
- ledgerRefs: ["tasks:T6","goals:G1"]
- sessionLogs: ["docs/logs/20260604-201605-a662e9f88829c4f5b.md"]

### R11 — go-ahead

- createdAt: 2026-06-04T20:19:33.317Z
- updatedAt: 2026-06-04T20:19:33.317Z
- author: "opus-4.8[1m]"
- session: 9fa3b56d-80fc-4604-bf8e-7daadaf5957b
- summary: "T7 approved after 1 criticism round: round-trip scenario doc is concrete + conformant (all 5 inputSchemas verified byte-for-byte vs live emitter, K4 error channels correct, §5.3 tiers correct); round-0's sole criticism (§7 task->language mapping mislabel) fixed and re-verified against the ledger."
- criticism: []
- new_questions: []
- ledgerRefs: ["tasks:T7","goals:G1"]
- sessionLogs: ["docs/logs/20260604-201605-a905954a2dc8c687b.md","docs/logs/20260604-201921-a753a27edceb3fdf3.md"]

### R12 — go-ahead

- createdAt: 2026-06-04T20:46:57.903Z
- updatedAt: 2026-06-04T20:46:57.903Z
- author: "opus-4.8[1m]"
- session: 9fa3b56d-80fc-4604-bf8e-7daadaf5957b
- summary: "T8 approved: TS MCP generator + runtime meet acceptance (tsc EXIT=0, sbt +compile green, JVM shape test green/non-vacuous, T7-conformant names+schemas, transport-abstract handle/no I/O loop per K5); flag-off non-MCP delta is pre-existing import-order nondeterminism filed as D3."
- criticism: []
- new_questions: []
- ledgerRefs: ["tasks:T8","goals:G1"]
- sessionLogs: ["docs/logs/20260604-204636-a65b17c5c2f913ef2.md"]

### R13 — go-ahead

- createdAt: 2026-06-04T21:18:03.422Z
- updatedAt: 2026-06-04T21:18:03.422Z
- author: "opus-4.8[1m]"
- session: 9fa3b56d-80fc-4604-bf8e-7daadaf5957b
- summary: "T9 approved: TS MCP overlay drives the GENERATED dispatch through the full T7 scenario (8 steps); ajv Draft-2020-12 validity gate non-vacuous (compile throws on dangling $ref); 4 negative controls verified live by flipping each assertion; test-gen-ts-mcp mirrors wiring-either flags + MCP flag, test-ts-mcp registered in :test aggregator; 16/16 reproduced independently."
- criticism: []
- new_questions: []
- ledgerRefs: ["tasks:T9","goals:G1"]
- sessionLogs: ["docs/logs/20260604-211741-a916dee3c59c87ede.md"]

### R14 — go-ahead

- createdAt: 2026-06-04T21:18:07.234Z
- updatedAt: 2026-06-04T21:18:07.234Z
- author: "opus-4.8[1m]"
- session: 9fa3b56d-80fc-4604-bf8e-7daadaf5957b
- summary: "T10 approved: C# MCP server generator mirrors T8/K4/K5 (per-service <Service>McpServer<Ctx>, lowercase wire names, T5 inputSchema, injected JSON delegate, transport-abstract Handle/no I/O loop); dotnet build 0/0 at cs-stub settings; flag-off byte-identical; PascalCase confined to C# symbols; JVM shape test non-vacuous; sbt +compile green."
- criticism: []
- new_questions: []
- ledgerRefs: ["tasks:T10","goals:G1"]
- sessionLogs: ["docs/logs/20260604-211741-ae97c4aee2c9e7443.md"]

### R15 — go-ahead

- createdAt: 2026-06-04T21:46:31.937Z
- updatedAt: 2026-06-04T21:46:31.937Z
- author: "opus-4.8[1m]"
- session: 9fa3b56d-80fc-4604-bf8e-7daadaf5957b
- summary: "T11 approved: C# MCP overlay drives generated McpToolsMcpServer.Handle via McpToolsWiring.InvokeJson through full T7 scenario; NJsonSchema validity gate non-vacuous + 3 live negative controls; $defs->definitions normalization touches only the validator copy (generated schema is correct Draft-2020-12); independent dotnet test -c Release 16/16 green; test-cs-mcp registered in :test with wiring-either flags. Closes the two-reference-backend milestone."
- criticism: []
- new_questions: []
- ledgerRefs: ["tasks:T11","goals:G1"]
- sessionLogs: ["docs/logs/20260604-214617-a1be70cc457fb5df1.md"]

## M4

### R16 — go-ahead

- createdAt: 2026-06-04T22:59:42.697Z
- updatedAt: 2026-06-04T22:59:42.697Z
- author: "opus-4.8[1m]"
- session: 9fa3b56d-80fc-4604-bf8e-7daadaf5957b
- summary: "T12 (Scala MCP replica) approved after 1 criticism round: generator/runtime mirror T8/T10/K5 (transport-abstract, no I/O loop, shared T5 emitter, lowercase wire names, flag-off byte-identical, sbt +compile green); round-0's vacuous K1 self-round-trip replaced by real recursive Circe structural equality vs embedded T7 §2.3 references (all 5 tools), liveness proven by defect injection + negative control; sbt test 10/10. Pre-existing Scala codegen issues filed as D4/D5."
- criticism: []
- new_questions: []
- ledgerRefs: ["tasks:T12","goals:G1"]
- sessionLogs: ["docs/logs/20260604-224034-afc74e884b46a7584.md","docs/logs/20260604-225919-a7eddde5e8a8b4783.md"]

### R17 — go-ahead

- createdAt: 2026-06-04T22:59:46.404Z
- updatedAt: 2026-06-04T22:59:46.404Z
- author: "opus-4.8[1m]"
- session: 9fa3b56d-80fc-4604-bf8e-7daadaf5957b
- summary: "T13 (Rust MCP replica) approved after 1 criticism round: generator/runtime mirror T8/T10/K5 (transport-abstract, no I/O loop, shared T5 emitter, flag-off inert, RsBaboonTranslator module-pass change MCP-only, sbt +compile green); round-0's vacuous serde self-round-trip replaced by real serde_json::Value deep equality vs embedded T7 §2.3 references (all 5 tools) + required-as-set, liveness proven by mutation; cargo test 12/12 with -D warnings; test-rust-mcp registered in :test."
- criticism: []
- new_questions: []
- ledgerRefs: ["tasks:T13","goals:G1"]
- sessionLogs: ["docs/logs/20260604-224034-aed725b66bef6d1e4.md","docs/logs/20260604-225919-a91a6e21f143ab4ea.md"]

### R18 — go-ahead

- createdAt: 2026-06-04T22:59:52.824Z
- updatedAt: 2026-06-04T22:59:52.824Z
- author: "opus-4.8[1m]"
- session: 9fa3b56d-80fc-4604-bf8e-7daadaf5957b
- summary: "T14 (Kotlin MCP replica) approved after 1 criticism round: generator/runtime mirror T8/T10/K5 (transport-abstract, no I/O loop, shared T5 emitter, $-escaping correct, flag-off byte-identical, JVM+JS compile green); round-0's vacuous self-round-trip replaced by real kotlinx JsonElement recursive structural equality vs embedded T7 §2.3 references (all 5 tools) + required-as-set, sound negative control; gradle 11/11; test-kotlin-mcp registered in :test."
- criticism: []
- new_questions: []
- ledgerRefs: ["tasks:T14","goals:G1"]
- sessionLogs: ["docs/logs/20260604-224034-a37afe2ff57235d12.md","docs/logs/20260604-225919-a5b6dba22ba511da9.md"]

### R19 — go-ahead

- createdAt: 2026-06-05T00:17:37.030Z
- updatedAt: 2026-06-05T00:17:37.030Z
- author: "opus-4.8[1m]"
- session: 9fa3b56d-80fc-4604-bf8e-7daadaf5957b
- summary: "T15 (Java MCP replica) approved: JvMcpServerGenerator + 10 Java runtime files mirror T8/T10/Kt/K5 (transport-abstract handle/no I/O loop, lowercase wire names, shared T5 inputSchema, injected delegate -> K4 Channel-A/B); sbt +compile green (585 JVM); Maven McpTests 11/11; real K1 structural equality vs embedded T7 §2.3 refs (Jackson JsonNode, required-as-set, live negative control); flag-off byte-identical (only MCP files added, --jv-write-evolution-dict in action); no 64KB violation; test-java-mcp in :test."
- criticism: []
- new_questions: []
- ledgerRefs: ["tasks:T15","goals:G1"]
- sessionLogs: ["docs/logs/20260605-001643-a524651f2843048c7.md"]

### R20 — go-ahead

- createdAt: 2026-06-05T00:17:39.652Z
- updatedAt: 2026-06-05T00:17:39.652Z
- author: "opus-4.8[1m]"
- session: 9fa3b56d-80fc-4604-bf8e-7daadaf5957b
- summary: "T16 (Dart MCP replica) approved: DtMcpServerGenerator + baboon_mcp_runtime.dart mirror reference/K5 (transport-abstract handle/no I/O loop, lowercase wire names, shared T5 inputSchema, dart:convert reuse, injected delegate -> K4 Channel-A/B); sbt +compile green, DartMcpServerEmissionTest 1/1, dart analyze clean, dart test 11/11; real recursive K1 structural equality vs T7 §2.3 refs (SetEquality for required, verified non-vacuous by required-set corruption); flag-off byte-identical (only 2 MCP files, relocation confined to action); test-dart-mcp in :test."
- criticism: []
- new_questions: []
- ledgerRefs: ["tasks:T16","goals:G1"]
- sessionLogs: ["docs/logs/20260605-001643-a77ec6e66cd4bc090.md"]

### R21 — go-ahead

- createdAt: 2026-06-05T00:37:33.656Z
- updatedAt: 2026-06-05T00:37:33.656Z
- author: "opus-4.8[1m]"
- session: 9fa3b56d-80fc-4604-bf8e-7daadaf5957b
- summary: "T18 (Python MCP replica) approved after 1 criticism round: PyMcpServerGenerator + baboon_mcp_runtime.py mirror reference/K5 (Generic[Ctx], transport-abstract handle/no I/O loop, shared T5 inputSchema, injected delegate -> K4 Channel-A/B); K1 real recursive dict structural equality vs T7 §2.3 refs (unconditional raise, live negative control). Round-0 criticisms fixed: (C1) reverted out-of-scope renderTree future-import -> flag-off byte-identical (generator diff empty), recursive-Tree compile moved to test-gen-python-mcp harness (verified live: py_compile OK + round-trip 11/11); (C2) added real PyMcpServerFlagOffEmissionTest (mcp=false -> no MCP files). sbt +compile green. Pre-existing python issues filed D6 (map[Color,str] codec) + D7 (recursive future-import generator fix)."
- criticism: []
- new_questions: []
- ledgerRefs: ["tasks:T18","goals:G1"]
- sessionLogs: ["docs/logs/20260605-001643-ae3dfe34ed8a59808.md","docs/logs/20260605-003713-a2883af1f72851494.md"]
