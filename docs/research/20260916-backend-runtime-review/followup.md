# Shared planning follow-up

Baseline: `4aef4b50`. The user authorized proposals 1–3 as a validated, separate commit, followed by a Java/Kotlin pilot of proposal 4. Proposal 5 (UEBA field-layout planning) is not included.

## Batch 1: product traversal, service facts and evolution metadata

- Reuse `DomainProductTranslator` in Python, Rust, TypeScript, Dart and Swift. Preserve member order, accumulated errors and disabled-product laziness.
- Replace the four backend-local service-method plan classes with `ServiceMethodPlan[A]`. Keep naming/escaping in each backend and retain independently lazy, once-per-plan type resolution.
- Prepare ordered evolution metadata through `EvolutionMetadataPlan` in all nine backends. Keep builtin rows in the shared plan; TypeScript's user-only projection and Rust's existing fallback/min-reader behavior stay local. Preserve version-list order and wire tier names.

### Verification

- Before edits: all 8 existing traversal/service-plan tests passed.
- Captured baseline generated output for 51 configurations: regular, wrapped, JSON-only and binary-only across all nine languages plus Kotlin KMP; no-errors and abstract-context services in C#/Scala/Kotlin/Java; async services in C#/Kotlin/Java.
- Added metadata-plan checks for stable type ordering, numerical version order, builtin retention, sparse tables and absent-version failure.
- Post-change JVM/Scala.js compilation and all 11 focused tests passed.
- All 42,180 generated files across the 51 configurations are byte-identical to the baseline, with no added or removed files.
- Cross-language serialization acceptance passed all 200 checks.
- Translator/runtime production code decreased by 103 non-comment, nonblank lines (`cloc` 2.10: 69,445 → 69,342). Runtime resources are unchanged; this is policy consolidation, not a measured performance improvement.
- Fresh `mdl --seq --keep-run-dir --simple-log :build :test :ci` run `20260917-184854-024193517` passed all 205 actions, including 846 compiler tests (no failures/cancellations), all backend matrices, native portability, editor grammars, 200 serialization checks and 162 RPC checks.

## Batch 2: recursive conversion planning

Not started. The pilot will share source/target structural decisions beneath the existing `FieldOp` model, while Java/Kotlin retain their numeric conversions, collection constructors/traversal and custom conversion hooks. No wire-format or runtime-contract changes are intended.
