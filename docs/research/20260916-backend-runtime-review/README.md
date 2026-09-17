# Backend and embedded-runtime maintainability review

Date: 2026-09-16. Baseline: `69bbb4c48a519d9a4148f370b76bde19af4bac6a`.
Review model: GPT-6 Astra; three concurrent read-only reviewers, reused for bounded backend assignments. The parent reviewed shared code and consolidated findings. This is an audit, not an implemented refactor.

Implementation was subsequently authorized. See the [implementation ledger](implementation.md) for current changes, verification results, and proposals deliberately narrowed to preserve observable behavior. The audit findings and measurements below describe the original baseline.

## Recommendation

There are concrete cleanup and generalization opportunities. The strongest boundary is **shared model decisions, backend-specific rendering, and runtime-owned protocol helpers**. Start with small extractions and observable allocation removal; use those results to decide whether larger service/evolution redesigns pay for their added abstractions.

Do not pursue a target such as “remove 30% of lines.” A shorter universal renderer can make nine language implementations harder to understand, and moving repeated generated code into a runtime may leave maintained LoC approximately unchanged while materially reducing generated volume.

## Measured scope and coverage

The requested translator/runtime trees contain **316 files and 71,482 nonblank, noncomment code lines**: 43,352 generator lines and 28,130 embedded-runtime lines. Exact duplicate files are deliberately counted separately because both copies are maintained. Whole-repository LoC is a different scope, including compiler frontend, LSP, tests, tooling and documentation; it is not the denominator for these recommendations.

| Review | Generator files | Generator code lines | Runtime files | Runtime code lines |
|---|---:|---:|---:|---:|
| [C#](csharp.md) | 17 | 5,359 | 15 | 4,046 |
| [Scala](scala.md) | 16 | 4,324 | 12 | 2,492 |
| [Python](python.md) | 17 | 4,207 | 10 | 2,489 |
| [Rust](rust.md) | 16 | 4,467 | 9 | 3,544 |
| [TypeScript](typescript.md) | 16 | 4,649 | 7 | 2,657 |
| [Kotlin JVM + KMP](kotlin.md) | 16 | 4,554 | 29 | 4,155 |
| [Java](java.md) | 16 | 4,880 | 59 | 2,688 |
| [Dart](dart.md) | 16 | 4,682 | 7 | 2,661 |
| [Swift](swift.md) | 16 | 4,997 | 9 | 3,398 |
| [GraphQL](graphql.md) | 2 | 372 | — | — |
| [OpenAPI](openapi.md) | 2 | 345 | — | — |
| [Shared translator/MCP code](cross-cutting.md) | 9 | 516 | — | — |
| **Total** | **159** | **43,352** | **157** | **28,130** |

Each report records full-content coverage of its assigned files and distinguishes supporting files that were only sampled. Kotlin coverage includes complete counterpart-plus-diff/hash inspection for some paired runtime resources. No claim is made that the entire compiler, all tests, or all generated outputs were exhaustively reviewed.

Reproduce the code census with:

```sh
cloc baboon-compiler/src/main/scala/io/septimalmind/baboon/translator \\
  baboon-compiler/src/main/resources/baboon-runtime \\
  --skip-uniqueness --by-file --json --quiet
```

## Candidate work packages

| Order | Bounded change | Evidence | Benefit | Effort / risk |
|---|---|---|---|---|
| 1 | Share identical doc renderers and three identical Kotlin JVM/KMP resources; remove proven compiler-internal dead scaffolding | X-02, X-06; KT-01; RS-08; TS-08; DT-03 | Fewer maintained copies, clearer active paths | S / low; public API checks still apply |
| 2 | Centralize identifier classification while retaining target renderers | X-01; JV-08 | One model-semantic classification across nine backends | S–M / low–medium |
| 3 | Pilot a backend-local scalar emitter shared by DTO and service codecs, then repeat where proven | CS-01, KT-02, JV-03, SC-02, SW-03, TS-03, DT-04, PY-07 | Removes independently maintained wire-policy tables | M / medium; characterize divergent cases first |
| 4 | Move schema-independent `any` envelope implementations into each language's runtime; preserve public forwarding methods and share static fallback planning | CS-03, KT-04, RS-01, JV-05, SC-04, SW-02, TS-04, DT-05 | Less repeated generated code; one framing implementation per runtime/format | M / medium–high |
| 5 | Remove known unnecessary work in isolated changes | CS-05, RS-07, SC-08, JV-06, SW-07, TS-05/06, DT-01/02, PY-06 | Sorting, repeated scans, unused materialization/copies disappear from inspected paths | S–M / low–medium; throughput benefit unmeasured |
| 6 | Share endpoint plans and MCP request/response policy within each backend/runtime | CS-02/07, KT-03/06, RS-02/03, JV-01/04, SC-03/07, SW-04/05, TS-07, DT-08, PY-04/05 | Fewer independent decisions across mode combinations | M–L / medium–high |
| 7 | Make shared JSON Schema fragments structured and prepare schema references once per domain | X-04/05; GQL-01/02; OAS-01/02/03 | Removes string/JSON round trips and repeated full-domain scans | S–M / medium; distinct projections remain explicit |
| 8 | Separately design larger language-specific changes, measuring performance claims where relevant | RS-05; TS-01/02; SW-01; KT-08; SC-05; JV-07; PY-01/02/08 | Typed conversions/resolution, explicit output/serialization contracts, and stable metadata ownership | M–L / medium–high |

These are grouped work packages, not additive savings estimates or permission to implement everything. Findings that recur in several reports describe one family of work. Effort: S = localized change; M = several collaborating components; L = architectural or contract-sensitive redesign. Risk concerns compatibility, not implementation difficulty.

Start with the small shared helpers/resources and a single backend-local scalar-emission pilot. Keep larger service/evolution changes in separate proposals. Typed Rust/TypeScript conversions may increase generated source while removing JSON intermediates; that tradeoff can be worthwhile but is not a LoC reduction claim. TypeScript already uses consume-only index reading and switch-based service dispatch: those particular Rust/Swift findings do not transfer to it.

Work package 4 has a specific compatibility gate: establish supported compiler/runtime version combinations, including consumers that omit runtime output. New generated helper calls will not exist in previously distributed runtimes. Require an appropriate runtime version or an explicit compatibility strategy, verify helper-file registration, and compile JSON-disabled/binary-only distributions. Matching new compiler/new runtime tests alone are insufficient.

Work package 5's collection optimizations require a separate proof from its exact-equivalence candidates. Direct population can interleave decoding with foreign hash/equality calls; fused iteration can change when user hooks run. Check failures, cursor position and side effects, or restrict optimization to domains whose equivalence is established. Preserve staged behavior elsewhere. Split buffer transfer, index consumption, collection construction and byte ownership into independent changes.

For work package 6, separate protocol factoring from caching. Prove metadata ownership and mutation semantics before adding a cache; do not infer immutability from a read-only accessor. Reuse existing `ResolvedServiceContext`, `ResolvedServiceResult`, `TypeRef` and `Conversion.FieldOp`. A service plan should centralize derived facts consumed by multiple sites; a bag of pre-rendered strings merely relocates the existing branching. Pilot it in one backend before wider adoption.

## Correctness follow-ups are separate from cleanup

The [Python reproduction record](python-reproductions.md) executes unchanged extracted production method bodies: `write_str` succeeds for 127 ASCII bytes and fails for 128 because a continuation byte is sent to signed byte packing; `Lazy.is_value_created` accesses a missing attribute. The active generated string writer path was traced. These are helper-level reproductions; the full Pydantic runtime and generated application were not executed because the Python interpreter used lacked Pydantic. Resolve their intended contracts and verify the production environment before combining any correction with an optimization.

Python conversion/metadata/JSON-boundary discrepancies and TypeScript's JSON-mediated conversion hazards also need supported-model reproductions. The TypeScript Node probe demonstrates that the expression fails on bigint and loses Map/Set contents; it does not prove that a specific accepted model reaches every expression. C# scalar service/DTO boolean divergence and schema projection alignment remain unconfirmed integration hypotheses. These findings must not be silently “fixed” during mechanical extraction.

## Boundaries to preserve

- Keep target syntax, imports, ownership/boxing, collection representation, effect/result types, and public runtime APIs local to each language.
- Keep JSON versus UEBA framing, map-key representations, and canonical identifier parsing distinct where their contracts differ.
- Keep Kotlin JVM/KMP platform mechanisms distinct even when pure policy or exact resource files can be shared.
- Preserve Swift recursive boxing and explicit package targets; Rust ownership and async contracts; Scala's deliberate MCP Either restriction; C# deduplication; TypeScript type/value imports and output modes.
- Do not merge resource files for aesthetics: embedded strings face the JVM constant-size limit. New runtime helpers need suitable files and output registration.
- Do not cache public mutable/dynamic tool metadata without an ownership/invalidation contract. Scope any compiler cache to a translation/domain, not a global registry.
- Retain Dart's public generated `encodeAnyField`/`decodeAnyField` signatures when relocating their bodies. A read-only collection type, including Kotlin `List`, does not alone prove safe immutable metadata ownership.
- Do not replace all backends/runtimes with a template framework. Share narrow typed decisions only where consumers and equivalent semantics are identified.

## Acceptance criteria for later implementation

For output-preserving generator extractions, compare file sets, product tags, and generated bytes before/after across the relevant flags. For runtime-helper relocation, generated text deliberately changes: compare public APIs, compiled behavior, JSON/UEBA bytes, schema evolution, and error categories instead.

Exercise concrete service axes (sync/async, error/result/container modes, void/value outputs, contexts, wire formats) and any-envelope kinds. Retain generated-language compilation and cross-language fixtures; hand-written mirrors alone do not verify emitters. Test public behavior and keep performance measurements separate, following constructive-test-taxonomy. Izumi guidance informs explicit effect constraints and DI/resource lifetime preservation.

Existing green lanes alone do not discharge these proofs. The Dart review identifies generated tests that return/catch on decoding failures, and the Scala review identifies skipped recursive/foreign types. Use mandatory fixtures with visible failures for the changed behavior, including async rejection/suspension and error ordering. These observations describe coverage limits, not demonstrated codec defects.

Run focused lanes during development, then the project's `mdl :build :test` before committing and `mdl :ci` before pushing/nontrivial refactors. Compiler checks must cover JVM and Scala.js. Runtime resource edits require `sbt clean` before recompilation because the embedding macro caches resources.

## What was actually checked

Complete assigned source inspections; file inventories/line counts; exact-file hashes; normalized exact-block comparisons; tracked-file reference searches; selected test/build/spec reads. A C# report's XOR check is an algebra sanity check in JavaScript, not C# execution. Python extracted-method probes and the TypeScript expression probe have captured outputs and explicit limitations. Reports list actual diagnostics separately from proposed tests.

A separate skeptical synthesis pass checked the major recommendations against selected source. Its material corrections are incorporated above and in the affected reports: Dart public forwarders, collection evaluation order, cache ownership, compiler/runtime compatibility, explicit schema projection policies, and removal of unmeasured executable-size claims. Report inventory and explicit file/line references were mechanically checked; that check does not itself prove the recommendations' semantics.

No compiler/runtime source was modified. No full build, backend test suite, generated compilation, or performance benchmark was run. Consequently this audit establishes source-level duplication and candidate costs, not a measured speedup, defect-free behavior, or a guaranteed LoC reduction. Suspected defects remain explicitly unconfirmed unless a report records a reproduction.
