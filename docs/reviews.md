---
ledger: reviews
counters:
  milestone: 0
  item: 22
archives: []
---

# reviews

## M2

### R1 — revise

- createdAt: 2026-06-09T20:52:02.978Z
- updatedAt: 2026-06-09T20:52:38.565Z
- author: "opus-4.8[1m]"
- session: 9ef20a09-ca98-4884-9e65-b5b7a852c035
- summary: Plan is fine-grained, correctly sequenced, testable on both compile+wire-key dimensions, grounded (5/9 backend sites re-verified against source), and complete across all 9 backends + disciplines; one missing task-level dependency edge keeps it on revise.
- new_questions: []
- criticism: ["Per-language fix tasks T3, T5, T6, T7, T8, T9, T10, T11, T12 each consume the T2 reserved-word test model in their acceptance criteria ('Generating <lang> for the reserved-word test model (T2)...') but none list T2 in dependsOn. Add T2 to the dependsOn of T3 and T5-T12 (and T4 transitively via T3) so the DAG enforces that the shared test model exists before any per-language fix is verified, rather than relying solely on M3-before-M4 milestone ordering. T13 depends on T3-T12 but not on T2 either - add T2 there as well, or to each consumer."]
- ledgerRefs: ["goals:G1"]
- sessionLogs: ["docs/logs/20260609-205223-ad140c399de6a3a69.md"]

### R2 — go-ahead

- createdAt: 2026-06-09T20:54:32.280Z
- updatedAt: 2026-06-09T20:54:51.423Z
- author: "opus-4.8[1m]"
- session: 9ef20a09-ca98-4884-9e65-b5b7a852c035
- summary: "R1's sole criticism resolved: T2 now in dependsOn of every test-model consumer (T3, T5-T12 directly; T4 transitively via T3; T13 directly). DAG acyclic. Plan remains fine-grained, sequenced, testable on compile+wire-key, grounded, complete across all 9 backends."
- new_questions: []
- criticism: []
- ledgerRefs: ["goals:G1"]
- sessionLogs: ["docs/logs/20260609-205443-a77960edd25ac9517.md"]

## M3

### R3 — go-ahead

- createdAt: 2026-06-09T21:12:14.712Z
- updatedAt: 2026-06-09T21:12:14.712Z
- author: "opus-4.8[1m]"
- session: 9ef20a09-ca98-4884-9e65-b5b7a852c035
- summary: "approve: wire-key contract for all 4 RENAME backends concrete + correct, UEBA-neutrality grounded, 9-backend site inventory citations spot-checked accurate."
- criticism: []
- new_questions: []
- ledgerRefs: ["tasks:T1","goals:G1"]
- sessionLogs: ["docs/logs/20260609-210918-afce4af53bd336852.md"]

### R4 — go-ahead

- createdAt: 2026-06-09T21:18:46.804Z
- updatedAt: 2026-06-09T21:18:46.804Z
- author: "opus-4.8[1m]"
- session: 9ef20a09-ca98-4884-9e65-b5b7a852c035
- summary: "approve (round 2): reserved-words model complete — true/false fields + True/False enum members added; all prior keyword coverage intact; parses + typechecks."
- criticism: []
- new_questions: []
- ledgerRefs: ["tasks:T2","goals:G1"]
- sessionLogs: ["docs/logs/20260609-210918-abc7909e0fe01a7d1.md","docs/logs/20260609-211657-a6ee12345aa4afd7b.md"]

## M4

### R5 — go-ahead

- createdAt: 2026-06-09T21:42:08.176Z
- updatedAt: 2026-06-09T21:42:08.176Z
- author: "opus-4.8[1m]"
- session: 9ef20a09-ca98-4884-9e65-b5b7a852c035
- summary: "approve T3 — escapeCsKeyword (full 77-keyword set) applied at both JSON (:158) and UEBA (:196) capture sites across both branch-codec arms; wire-neutral, no regression for non-keyword branches."
- criticism: []
- new_questions: []
- ledgerRefs: ["tasks:T3","goals:G1"]
- sessionLogs: ["docs/logs/20260609-214140-af3a9491b3c67864c.md"]

### R6 — go-ahead

- createdAt: 2026-06-09T21:42:11.031Z
- updatedAt: 2026-06-09T21:42:11.031Z
- author: "opus-4.8[1m]"
- session: 9ef20a09-ca98-4884-9e65-b5b7a852c035
- summary: approve T9 — keyword rename (full JLS set) applied at all model-field/accessor/capture sites; JSON/UEBA wire keys provably preserved as original model names; class names safe via capitalization. One low out-of-scope defect filed (renderOwner ADT package segment).
- criticism: []
- new_questions: []
- ledgerRefs: ["tasks:T9","goals:G1"]
- sessionLogs: ["docs/logs/20260609-214140-a2cea0e21e7bdf37d.md"]

### R7 — go-ahead

- createdAt: 2026-06-09T22:06:56.703Z
- updatedAt: 2026-06-09T22:06:56.703Z
- author: "opus-4.8[1m]"
- session: 9ef20a09-ca98-4884-9e65-b5b7a852c035
- summary: approve T5 — escapeScKeyword (full Scala keyword set) applied at all identifier-emission sites, wire-key strings unescaped (wire-neutral); surgical, 602/602. D3 Class-shadowing out of scope.
- criticism: []
- new_questions: []
- ledgerRefs: ["tasks:T5","goals:G1"]
- sessionLogs: ["docs/logs/20260609-220620-a45bf9d85e30b139e.md"]

### R8 — go-ahead

- createdAt: 2026-06-09T22:06:59.482Z
- updatedAt: 2026-06-09T22:06:59.482Z
- author: "opus-4.8[1m]"
- session: 9ef20a09-ca98-4884-9e65-b5b7a852c035
- summary: approve T6 — escapeKtKeyword (Kotlin hard-keyword set) applied at field/codec/capture sites (kt-stub + kmp), wire-neutral; type-preserving, 602/602. One low out-of-scope defect filed (D4, service-wiring asymmetry). D3 Class-shadowing out of scope.
- criticism: []
- new_questions: []
- ledgerRefs: ["tasks:T6","goals:G1"]
- sessionLogs: ["docs/logs/20260609-220620-aef8c4956d229a5b8.md"]

### R16 — go-ahead

- createdAt: 2026-06-10T00:11:03.611Z
- updatedAt: 2026-06-10T00:11:03.611Z
- author: "opus-4.8[1m]"
- session: 9ef20a09-ca98-4884-9e65-b5b7a852c035
- summary: approve T8 — self/super/crate rename + serde wire-key preservation verified in generated Rust (reviewer built native + cargo test -D warnings green); type/variant escape byte-identical for existing fixtures.
- criticism: []
- new_questions: []
- ledgerRefs: ["tasks:T8","goals:G1"]
- sessionLogs: ["docs/logs/20260610-001024-T19-T21-T8-T4-T7-batch.md"]

### R17 — go-ahead

- createdAt: 2026-06-10T00:11:06.440Z
- updatedAt: 2026-06-10T00:11:06.440Z
- author: "opus-4.8[1m]"
- session: 9ef20a09-ca98-4884-9e65-b5b7a852c035
- summary: approve T4 — C# general keyword-escaping pass correct/complete/wire-safe/byte-identical across 8 csharp files. dotnet-build-green is blocked by the separate filed D7 (System.Type shadowing), out of T4's keyword scope.
- criticism: []
- new_questions: []
- ledgerRefs: ["tasks:T4","goals:G1"]
- sessionLogs: ["docs/logs/20260610-001024-T19-T21-T8-T4-T7-batch.md"]

### R18 — go-ahead

- createdAt: 2026-06-10T00:11:09.825Z
- updatedAt: 2026-06-10T00:11:09.825Z
- author: "opus-4.8[1m]"
- session: 9ef20a09-ca98-4884-9e65-b5b7a852c035
- summary: approve T7 — Swift ADT-branch/enum-case identifiers escaped at all cited sites, JSON wire strings + UEBA ordinals preserved; 2 supporting fixes (Type metatype dot-shorthand, self-field _self init param) sound + guarded; swift build green. Missing reserved-words CI lane noted (covered by planned T13).
- criticism: []
- new_questions: []
- ledgerRefs: ["tasks:T7","goals:G1"]
- sessionLogs: ["docs/logs/20260610-001024-T19-T21-T8-T4-T7-batch.md"]

### R20 — go-ahead

- createdAt: 2026-06-10T10:31:36.684Z
- updatedAt: 2026-06-10T10:31:36.684Z
- author: "opus-4.8[1m]"
- session: 9ef20a09-ca98-4884-9e65-b5b7a852c035
- summary: approve T10 — Python keyword rename + wire-key preservation verified end-to-end (JSON wire keys provably original via pydantic alias, UEBA rename-neutral, enum True_/False_); 602 green; non-keyword identity.
- criticism: []
- new_questions: []
- ledgerRefs: ["tasks:T10","goals:G1"]
- sessionLogs: ["docs/logs/20260610-103112-T10-T11-T12-batch.md"]

### R21 — go-ahead

- createdAt: 2026-06-10T10:31:39.846Z
- updatedAt: 2026-06-10T10:31:39.846Z
- author: "opus-4.8[1m]"
- session: 9ef20a09-ca98-4884-9e65-b5b7a852c035
- summary: approve T11 — dead escapeTsKeyword wired at binding positions, wire keys original in all serialization paths, identity for non-keywords; tsc green. Low latent hardening gap filed (D9, TS type/enum-name escape).
- criticism: []
- new_questions: []
- ledgerRefs: ["tasks:T11","goals:G1"]
- sessionLogs: ["docs/logs/20260610-103112-T10-T11-T12-batch.md"]

### R22 — go-ahead

- createdAt: 2026-06-10T10:31:43.129Z
- updatedAt: 2026-06-10T10:31:43.129Z
- author: "opus-4.8[1m]"
- session: 9ef20a09-ca98-4884-9e65-b5b7a852c035
- summary: approve T12 — escapeDartKeyword wired through member emission, wire keys/discriminators original; +2 sound fixes (UEBA castedName, Object/Type stdlib-shadow rename); dart analyze clean; non-keyword byte-identical (A/B-verified).
- criticism: []
- new_questions: []
- ledgerRefs: ["tasks:T12","goals:G1"]
- sessionLogs: ["docs/logs/20260610-103112-T10-T11-T12-batch.md"]

## M6

### R9 — go-ahead

- createdAt: 2026-06-09T22:28:20.180Z
- updatedAt: 2026-06-09T22:28:45.695Z
- author: "opus-4.8[1m]"
- session: 9ef20a09-ca98-4884-9e65-b5b7a852c035
- summary: "approve G2 plan (T15-T18): fine-grained, correctly sequenced (T15/T16/T17 parallel, T18 dependsOn all three), testable, grounded, complete. All cited sites source-verified: ScDomainTreeTools.scala:67 def baboonAdtType: $javaClass[?] (translator/scl pkg), KtDomainTreeTools:70 + KtTypes:66 JVM arm short Class (multiplatform :64 already FQ KClass), JvTypes:104, JvTypeTranslator:193 Owner.Adt unescaped vs :192 Ns escaped, KtServiceWiringTranslator impl.<m> raw at 447/448/489/490/669/677/699/707/767/775/798/806, reserved-words-ok adt AvatarItem data Class branch. D3 covers all 3 JVM backends and the correct Kotlin JVM arm; T18 asserts D3 closure unblocks G1 T14. Defect-seeded, root causes confirmed; no clarification needed."
- new_questions: []
- criticism: []
- ledgerRefs: ["goals:G2"]
- sourceRefs: ["defects:D3","defects:D2","defects:D4"]
- sessionLogs: ["docs/logs/20260609-222834-ada7374e3219f56b9.md"]

## M7

### R10 — go-ahead

- createdAt: 2026-06-09T22:46:14.193Z
- updatedAt: 2026-06-09T22:46:14.193Z
- author: "opus-4.8[1m]"
- session: 9ef20a09-ca98-4884-9e65-b5b7a852c035
- summary: approve T15 — baboonAdtType rendered FQ java.lang.Class across 3 JVM backends via .fullyQualified; predefs unchanged, C#/non-JVM untouched, Java 8→0 errors, 602 green. Out-of-scope Object-shadowing filed (D5).
- criticism: []
- new_questions: []
- ledgerRefs: ["tasks:T15","goals:G2"]
- sessionLogs: ["docs/logs/20260609-224538-T15-worker-reviewer.md"]

### R11 — go-ahead

- createdAt: 2026-06-09T22:46:17.033Z
- updatedAt: 2026-06-09T22:46:17.033Z
- author: "opus-4.8[1m]"
- session: 9ef20a09-ca98-4884-9e65-b5b7a852c035
- summary: approve T16 — Owner.Adt arm routes through escapeJvKeyword (escape-only, casing preserved); byte-identical for existing fixtures; 602 green.
- criticism: []
- new_questions: []
- ledgerRefs: ["tasks:T16","goals:G2"]
- sessionLogs: ["docs/logs/20260609-224538-T16-worker-reviewer.md"]

### R12 — go-ahead

- createdAt: 2026-06-09T22:46:19.841Z
- updatedAt: 2026-06-09T22:46:19.841Z
- author: "opus-4.8[1m]"
- session: 9ef20a09-ca98-4884-9e65-b5b7a852c035
- summary: approve T17 — all 12 kt wiring impl call-site receivers escaped via escapeKtKeyword, wire-name strings preserved, 602 green (clean-clone verified). Client-stub declaration escape filed (D6).
- criticism: []
- new_questions: []
- ledgerRefs: ["tasks:T17","goals:G2"]
- sessionLogs: ["docs/logs/20260609-224538-T17-worker-reviewer.md"]

## M8

### R13 — go-ahead

- createdAt: 2026-06-09T22:56:56.403Z
- updatedAt: 2026-06-09T22:57:23.377Z
- author: "opus-4.8[1m]"
- session: 9ef20a09-ca98-4884-9e65-b5b7a852c035
- summary: "G3 plan (M9: T19 Java-D5 FQ / T20 Scala+Kotlin-D5 analogues / T21 kt-D6 escape; M10: T22 verify) is fine-grained, correctly sequenced (T22 dependsOn T19/T20/T21), testable, source-grounded, and complete; no gaps."
- new_questions: []
- criticism: []
- ledgerRefs: ["goals:G3"]
- sessionLogs: ["docs/logs/20260609-225505-abf943028b0ba5ff7.md"]

## M9

### R14 — go-ahead

- createdAt: 2026-06-10T00:10:57.653Z
- updatedAt: 2026-06-10T00:10:57.653Z
- author: "opus-4.8[1m]"
- session: 9ef20a09-ca98-4884-9e65-b5b7a852c035
- summary: "approve T19 — Object/String FQ fix correct + scoped to in-record-body sites; reserved-words-ok AvatarItem compiles (15→0 javac errors, test-java-regular green). Low follow-up filed: enum parse(String) shadow (D8)."
- criticism: []
- new_questions: []
- ledgerRefs: ["tasks:T19","goals:G3"]
- sessionLogs: ["docs/logs/20260610-001024-T19-T21-T8-T4-T7-batch.md"]

### R15 — go-ahead

- createdAt: 2026-06-10T00:11:00.390Z
- updatedAt: 2026-06-10T00:11:00.390Z
- author: "opus-4.8[1m]"
- session: 9ef20a09-ca98-4884-9e65-b5b7a852c035
- summary: approve T21 — Kotlin client-stub declarations escaped at both sites with valid Json-variant whole-token backticking; transport wire strings raw; 602 green; byte-identical.
- criticism: []
- new_questions: []
- ledgerRefs: ["tasks:T21","goals:G3"]
- sessionLogs: ["docs/logs/20260610-001024-T19-T21-T8-T4-T7-batch.md"]

## M11

### R19 — go-ahead

- createdAt: 2026-06-10T10:19:48.102Z
- updatedAt: 2026-06-10T10:19:48.102Z
- author: "opus-4.8[1m]"
- session: 9ef20a09-ca98-4884-9e65-b5b7a852c035
- summary: G4 plan (T23/T24/T25) is fine-grained, correctly sequenced, testable, grounded, and complete — D7 FQ-csTpe fix verified mechanically sound in-repo; approve.
- new_questions: []
- criticism: []
- ledgerRefs: ["goals:G4"]
