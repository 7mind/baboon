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

Completed as the Java/Kotlin pilot, after committing batch 1 as `bb2c7827`.

- Added `ValueConversionPlan` beneath the existing `FieldOp` model, used only by Java and Kotlin in this pilot.
- Centralized recursive scalar/container decisions, map key/value planning and opaque-`any` invariants. Scalar leaves retain both type identifiers, even when equal, so renderers still invoke user-defined conversion hooks.
- Kept numeric conversions, foreign-type handling, collection constructors/traversal and field-operation-specific wrapping/swapping in the backend renderers. Removed optional source-type overrides in favor of explicit source types and a same-type overload.
- Moved the duplicated opaque-value rationale and invariant checks into the shared planner. No runtime resources, wire formats or runtime contracts changed.

### Verification

- Baseline: batch 1's complete CI and generated corpus.
- JVM/Scala.js compilation and all 8 new planner tests passed. Tests cover scalar source/target retention, hooks for equal identifiers, wrapping, nested lists/options/sets, independent map keys/values, opaque identity and rejected invariant violations.
- Repeated output parity passed: all 42,180 files across the same 51 configurations are byte-identical to batch 1, with no added or removed files.
- The pilot removes another 21 non-comment, nonblank production lines (translator total: 40,859 → 40,838); no runtime resources changed.
- Fresh full CI passed all 205 actions: serial Kotlin/serialization run `20260917-194510-533675770` passed 33 actions, then `mdl --par --continue --keep-run-dir --simple-log :build :test :ci` run `20260917-195957-573111324` passed the remaining 172 and restored only those same-source results. No production source changed between phases.
- Final results: 854 compiler tests across 138 suites (no failures/cancellations), all backend matrices, 200 serialization checks and 162 RPC checks passed.
- Commit closeout: the three production source files exactly matched their entries in the source JAR packaged for that validated build. Reran JVM/Scala.js compilation and all 19 traversal/service/metadata/value-plan tests successfully. Pre-commit `mdl --seq --continue --keep-run-dir --simple-log :build :test :ci` run `20260917-213004-450743262` restored the 205 passing actions for the unchanged implementation; it did not rerun the full matrix.

## Outcomes and measured savings

Counts below use `cloc` 2.10 with `--skip-uniqueness`, excluding blank lines and comments but counting identical files separately. Baselines were exported from Git; the final row counts the completed working tree. Scope is production translators plus embedded runtimes, not the whole repository or generated fixture output.

| State | Translator code | Runtime code | Total |
|---|---:|---:|---:|
| Before the earlier backend/runtime cleanup (`69bbb4c4`) | 43,352 | 28,130 | 71,482 |
| Start of proposals 1–4 (`4aef4b50`) | 40,962 | 28,483 | 69,445 |
| Proposals 1–3 committed (`bb2c7827`) | 40,859 | 28,483 | 69,342 |
| Proposal 4 complete | 40,838 | 28,483 | 69,321 |

- This round removes 124 production lines: 103 from proposals 1–3 and 21 from proposal 4. Across the earlier cleanup and this follow-up, the net reduction is 2,161 lines (3.0%). Tests and documentation are excluded from these savings.
- Product traversal is shared by eight backends; service-method facts by four; evolution metadata preparation by all nine; recursive value-conversion planning by Java and Kotlin in this pilot.
- Added 11 behavior-level planner tests in this round (three metadata and eight conversion-plan tests), while retaining the existing traversal/service checks.
- Generated output is byte-identical across the 51 compared configurations, so this round provides no generated-code size reduction. Runtime contracts and wire formats are unchanged. Execution time, memory use and maintenance effort were not benchmarked; the structural gain is fewer independent implementations of shared policy.
