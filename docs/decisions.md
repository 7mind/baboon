---
ledger: decisions
counters:
  milestone: 0
  item: 6
archives: []
---

# decisions

## M2

### K1 — locked

- createdAt: 2026-06-09T20:55:23.034Z
- updatedAt: 2026-06-09T20:55:23.034Z
- author: "opus-4.8[1m]"
- session: 9ef20a09-ca98-4884-9e65-b5b7a852c035
- headline: "plan review: approved"
- rationale: "Reviewer go-ahead on R2 (0 criticisms, 0 new questions): R1's sole DAG criticism resolved — T2 reserved-word test model now in dependsOn of every consumer (T3, T5-T12 direct; T4 via T3; T13 direct), DAG acyclic; plan fine-grained, sequenced, testable on compile+wire-key, grounded, complete across all 9 backends."
- ledgerRefs: ["goals:G1"]

## M3

### K2 — locked

- createdAt: 2026-06-09T21:12:49.949Z
- updatedAt: 2026-06-09T21:12:49.949Z
- author: "opus-4.8[1m]"
- session: 9ef20a09-ca98-4884-9e65-b5b7a852c035
- headline: Wire-key-preservation contract for RENAME backends + per-backend identifier-emission site inventory (G1/D1)
- rationale: "T1 deliverable, locked from docs/drafts/20260609-g1-wire-key-contract-and-site-inventory.md (committed aa324e53). CONTRACT: (1) Wire-key invariant — when a backend with no escape syntax (Java/Python/TypeScript/Dart) RENAMES a model-derived identifier to dodge a target keyword, the emitted JSON wire key and UEBA byte layout MUST remain a function of the model name + field/branch order, never the emitted target identifier. Per-backend mechanism: Java = Jackson @JsonProperty(\"<modelName>\") (hand-rolled codec already emits the model-name string literal, so codec is wire-safe; annotation covers non-codec Jackson consumers); Python = pydantic Field(alias=\"<modelName>\", serialization_alias=\"<modelName>\") for the transparent model_dump_json path (explicit-walker path already emits a literal); TypeScript = bracket-string wire key already literal, rename only the TS member identifier + UEBA local; Dart = literal map key already model-name, rename only the getter + named-ctor param. UEBA is positional/index-based => rename-neutral (confirmed CSUEBA:196/201/218/285). All mirror Rust's existing #[serde(rename)] discipline (RsDefnTranslator:1066-1073). (2) Site inventory: per-backend table of identifier-emission sites with file:line citations across all 9 backends, classifying centralized render passes (Scala ScBaboonTranslator:283-290 [no-op], Dart DtBaboonTranslator:270-303 [type names], Swift SwBaboonTranslator:331-340 [type/field]) vs scattered raw emissions (codec capture locals, ADT-branch/enum case names). Per-language fix tasks T3-T12 implement against this contract."
- sourceRefs: ["docs/drafts/20260609-g1-wire-key-contract-and-site-inventory.md"]
- ledgerRefs: ["goals:G1","defects:D1","tasks:T1"]

## M6

### K3 — locked

- createdAt: 2026-06-09T22:29:08.021Z
- updatedAt: 2026-06-09T22:29:08.021Z
- author: "opus-4.8[1m]"
- session: 9ef20a09-ca98-4884-9e65-b5b7a852c035
- headline: "plan review: approved"
- rationale: "Reviewer go-ahead (R9): G2 plan T15-T18 judged fine-grained, correctly sequenced (T15/T16/T17 parallel, T18 dependsOn all three), testable, and grounded with all cited sites source-verified. 0 criticisms, 0 new questions, no out-of-scope defects."
- ledgerRefs: ["goals:G2"]

## M8

### K4 — locked

- createdAt: 2026-06-09T22:57:48.898Z
- updatedAt: 2026-06-09T22:57:48.898Z
- author: "opus-4.8[1m]"
- session: 9ef20a09-ca98-4884-9e65-b5b7a852c035
- headline: "plan review: approved"
- rationale: "Reviewer go-ahead (ref review R13): G3 plan fine-grained, correctly sequenced (T22 dependsOn T19/T20/T21), testable, source-grounded, complete; 0 criticisms, 0 new questions, no out-of-scope defects."
- ledgerRefs: ["goals:G3"]

## M11

### K5 — locked

- createdAt: 2026-06-10T10:28:16.765Z
- updatedAt: 2026-06-10T10:28:16.765Z
- author: "opus-4.8[1m]"
- session: 9ef20a09-ca98-4884-9e65-b5b7a852c035
- headline: "plan review: approved"
- rationale: "Reviewer go-ahead (ref review R19): G4 plan (T23/T24/T25) is fine-grained, sequenced, testable, grounded, complete; D7 FQ-csTpe fix verified mechanically sound in-repo. 0 criticisms, 0 new questions."
- ledgerRefs: ["goals:G4"]

## M13

### K6 — locked

- createdAt: 2026-06-10T11:10:53.932Z
- updatedAt: 2026-06-10T11:10:53.932Z
- author: "opus-4.8[1m]"
- session: 9ef20a09-ca98-4884-9e65-b5b7a852c035
- headline: "plan review: approved"
- rationale: "Reviewer go-ahead (ref review R26): G5 plan (M14 T26->T27->T28) is fine-grained, correctly sequenced, testable, grounded against the repo, and complete; 0 criticisms, 0 new questions."
- ledgerRefs: ["goals:G5"]
