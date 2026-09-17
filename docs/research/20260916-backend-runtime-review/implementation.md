# Implementation ledger

User authorized the full review backlog on 2026-09-16. Baseline: `69bbb4c48a519d9a4148f370b76bde19af4bac6a`. All 84 findings are addressed within the preservation decisions below; conditional optimizations without behavioral equivalence were narrowed or withheld. Integrated CI passed on 2026-09-17, resuming after a test-only dependency correction. Public APIs, wire representations, error behavior, ownership and supported output modes remain compatibility constraints.

## Sequence and checks

1. Shared helpers, exact resources and dead internal scaffolding → generated-output characterization and JVM/JS compilation.
2. Structured schema preparation and Python defect reproductions → schema behavior, actual runtime tests and supported generated fixtures.
3. Backend-local scalar/evolution/output plans and runtime allocation changes → generated-language compilation, public behavior and wire goldens.
4. Any-helper relocation and service/MCP factoring → runtime version compatibility, output mode matrix, sync/async/error ordering and metadata ownership.
5. Integrated verification → clean embedding rebuild and combined `mdl :build :test :ci`; no publication actions.

Concurrent editors use isolated worktrees. Root integrates and validates changes. Workers have progressed from shared/schema/Python batches to Scala/Kotlin, Rust/Swift and TypeScript/Dart. Root owns C# and then Java. No concurrent editors share a worktree.

## Findings

| Finding | Proposal | State |
|---|---|---|
| [RS-01](rust.md) | Move invariant any-field framing from emitted modules into the runtime | Integrated; generated any/split-runtime checks and full matrix pass |
| [RS-02](rust.md) | Share service invocation/error policy across wire formats | Integrated; sync/async generated source byte-identical; focused muxer tests pass |
| [RS-03](rust.md) | Unify MCP protocol dispatch and use its existing routing index | Integrated; reproduced failed-registration corruption fixed; sync 11/async 9 MCP checks pass |
| [RS-04](rust.md) | Centralize Rust field representation decisions | Integrated; cycle-safe float/any capability analysis; reproduced ADT compile failure fixed |
| [RS-05](rust.md) | Replace JSON-mediated evolution selectively with typed conversions | Integrated selectively; all 6 generated regressions pass, including ADT branch evolution; intentional foreign/custom/float fallbacks retained |
| [RS-06](rust.md) | Reduce facade lookup allocations behind existing APIs | Integrated; reproduced LazyCodec publication race fixed; concurrency and 14 surrounding checks pass |
| [RS-07](rust.md) | Build module parent/child relationships once | Integrated; full regular generated corpus byte-identical |
| [RS-08](rust.md) | Remove demonstrably unused internal scaffolding | Integrated; full regular generated corpus byte-identical |
| [PY-01](python.md) | Give native JSON values and serialized JSON text distinct contracts | Integrated; explicit native-value API plus legacy text compatibility; focused tests pass |
| [PY-02](python.md) | Compute one Python field serialization/naming plan | Integrated immutable field feature analysis; 484/485 full-corpus files byte-identical (only intentional facade runtime differs); regular/wrapped fixtures pass |
| [PY-03](python.md) | Unify collection traversal in evolution rendering | Integrated; focused generated/runtime tests and full matrix pass |
| [PY-04](python.md) | Share MCP protocol decisions, not sync/async execution machinery | Integrated; 17 sync and 5 async MCP tests and full matrix pass |
| [PY-05](python.md) | Build immutable MCP metadata once per generated server | Integrated; generated registry ownership and dynamic-subclass checks pass |
| [PY-06](python.md) | Consume/validate UEBA indices without materializing discarded entries | Integrated; focused generated/runtime tests and full matrix pass |
| [PY-07](python.md) | Centralize scalar codec policy for DTO and RPC consumers | Integrated JSON/binary scalar policy; 10 failing native-RPC checks reproduced before correction; fresh regular/wrapped and async checks pass |
| [PY-08](python.md) | Establish an executable generated/runtime contract boundary before facade cleanup | Integrated generated-contract corrections; conversion and masked factory-error regressions reproduced; fresh 11-case review suites, runtime 172 and generated self-codec 52 pass |
| [X-01](cross-cutting.md) | Share identifier classification before sharing identifier rendering | Integrated; seven unchanged backend corpus outputs byte-identical, Python separately verified |
| [X-02](cross-cutting.md) | Consolidate truly identical documentation renderers | Integrated; Scala/Kotlin/TypeScript corpus outputs byte-identical |
| [X-03](cross-cutting.md) | Normalize domain product planning without erasing backend output structure | Integrated Scala/Kotlin/Java product traversal; Java four-mode full-corpus comparison passes |
| [X-04](cross-cutting.md) | Make the shared schema fragment boundary structured | Integrated; schema tests pass; all 44 OpenAPI documents structurally unchanged |
| [X-05](cross-cutting.md) | Prepare MCP semantic context once per domain | Integrated across nine MCP generators; all generated MCP lanes pass |
| [X-06](cross-cutting.md) | Retire compiler-internal scaffolding with evidence | Integrated; compiler tests and generated corpus checks pass |
| [KT-01](kotlin.md) | Share exact platform-neutral runtime resources | Integrated; 442 generated Kotlin files byte-identical before later helper changes |
| [KT-02](kotlin.md) | Give primitive wire emission one Kotlin owner | Integrated; generated corpus byte-identical |
| [KT-03](kotlin.md) | Separate service planning from dispatch rendering | Integrated; focused method-plan tests and full compiler suite pass |
| [KT-04](kotlin.md) | Move schema-independent any-envelope helpers into runtime support | Integrated; actual JVM/KMP any checks 18 each pass; split runtime and binary-only compile pass |
| [KT-05](kotlin.md) | Remove avoidable intermediate collection and buffer materializations | KMP direct buffer transfer integrated and tested; overridable JVM stream ownership and risky collection staging retained |
| [KT-06](kotlin.md) | Reuse MCP protocol handling between server and muxer | Integrated; actual standalone 14 and mux 8 MCP checks pass |
| [KT-07](kotlin.md) | Centralize KMP civil-date decomposition | Integrated; calendar characterization passes before/after; existing negative-fraction wire discrepancy preserved |
| [KT-08](kotlin.md) | Share pure evolution policy only after isolating runtime mechanisms | Integrated; pure version-selection extraction passes JVM/KMP checks; platform mechanisms retained |
| [SC-01](scala.md) | Give identifier runtime helpers one maintained source | Exact-object mirror parity guard integrated and passing; source/resource surfaces remain separate |
| [SC-02](scala.md) | Share Scala scalar codec emission between DTO and RPC generators | Integrated; generated corpus byte-identical |
| [SC-03](scala.md) | Separate service planning from wire/result rendering | Integrated; focused method-plan tests and full compiler suite pass |
| [SC-04](scala.md) | Move schema-independent any-envelope operations into runtime | Integrated; 18 generated any checks and split runtime/binary-only checks pass |
| [SC-05](scala.md) | Hoist immutable generated metadata and singleton wrappers | Integrated; generated wrapper/metadata reuse 2 checks pass |
| [SC-06](scala.md) | Remove intermediate binary buffers/collections where ownership permits | Validated count-only index path integrated; 7 cursor/error vectors pass; extensible JVM stream copies retained |
| [SC-07](scala.md) | Share MCP protocol handling, retain separate routing ownership | Integrated; actual standalone 14 and mux 8 MCP checks pass |
| [SC-08](scala.md) | Replace repeated import-collision scans with a name index | Integrated; full regular generated corpus byte-identical |
| [DT-01](dart.md) | Write primitives directly into the growable binary buffer | Integrated; zero-capacity hang reproduced before correction; 4 native checks pass; clean JVM/JS embedding passes |
| [DT-02](dart.md) | Avoid per-call service handler maps and repeated JSON parsing | Integrated typed endpoint plan, switch dispatch and single response parse; fresh sync 9 and actual Future JSON dispatch pass |
| [DT-03](dart.md) | Remove the disconnected private errors-mode renderer island | Integrated; controlled 476-file comparison and fresh native generated checks pass |
| [DT-04](dart.md) | Consolidate scalar and foreign wire decisions before rendering | Integrated scalar/foreign plans; controlled output comparison, 158 runtime checks and four codec-mode checks pass |
| [DT-05](dart.md) | Move repeated any-field helpers behind a typed runtime boundary | Integrated runtime helpers preserving public forwarders; fresh runtime and all four split-runtime codec-mode checks pass |
| [DT-06](dart.md) | Unify import planning without flattening Dart library semantics | Integrated typed physical-file/library routing; controlled generated output unchanged; differing acronym naming intentionally retained |
| [DT-07](dart.md) | Build decoded collections directly; remove tiny hashing containers | Integrated fixed-pair hashing; 49-pair SDK characterization passes; callback/error-sensitive collection staging retained |
| [DT-08](dart.md) | Share synchronous MCP protocol handling, not transport contracts | Integrated shared synchronous protocol; fresh MCP 15 tests pass; mutable registry and string transport contracts retained |
| [CS-01](csharp.md) | Centralize C# scalar wire emission | Integrated; existing boolean RPC/DTO divergence explicitly preserved; generated definitions and runtime checks pass |
| [CS-02](csharp.md) | Give service dispatch a typed method plan | Integrated typed method plan and three shared invocation fragments; 8 generated-output comparisons byte-identical; full CI mode matrix passes |
| [CS-03](csharp.md) | Move schema-independent any framing into the runtime | Integrated; all six any kinds/golden/extensions/errors pass; generated C# checks pass |
| [CS-04](csharp.md) | Compute upgrade decisions once per translation | Candidate-independent traversal hoisted; broader caching conditional on profiling, not introduced speculatively |
| [CS-05](csharp.md) | Remove sorting before commutative hash accumulation | Integrated; 3 C# characterization tests passed before/after |
| [CS-06](csharp.md) | Avoid materializing discarded UEBA index entries | Integrated; compact/indexed/nonseekable/truncated parity passes; public materializing API retained |
| [CS-07](csharp.md) | Factor MCP protocol helpers and registry ownership | Integrated; private owned generated registry and shared protocol helpers; actual generated MCP 22 checks pass; external dynamic registry preserved |
| [CS-08](csharp.md) | Parse registered versions once inside facade bookkeeping | Integrated immutable-parser-context cache guard; 4 culture/subclass regressions fail before and pass after; 86 focused tests, netstandard2.1 compile and clean embedding pass |
| [TS-01](typescript.md) | Make output symbols explicit instead of inferring exports from references | Explicit output metadata integrated; strict generated build passes; NodeNext extensionless runtime imports confirmed pre-existing and retained |
| [TS-02](typescript.md) | Lower evolution operations to typed value transformations | Integrated typed conversions; user-approved shared renamed-ADT branch planning correction passes rule test and regular/wrapped 10-case TS suites; affected runtime source/target validation corrections are integrated |
| [TS-03](typescript.md) | Share codec activation and scalar rendering, not wire containers | Integrated; shared activation, per-version entry plan and scalar emission; strict generated build passes |
| [TS-04](typescript.md) | Move invariant any helpers into runtime modules | Integrated in existing any runtime module; fresh runtime suite passes |
| [TS-05](typescript.md) | Remove collection intermediates from generated codecs | Safe identity primitive JSON collection mapping removal integrated; callback/failure-sensitive fusion retained; collection characterization passes |
| [TS-06](typescript.md) | Write fixed-width primitives directly into writer storage | Integrated; 6 storage tests passed before/after; 5 reader checks passed |
| [TS-07](typescript.md) | Factor MCP protocol preparation and response construction | Integrated; shared protocol preparation/results and owned generated index; fresh sync MCP 34 and async 3 checks pass |
| [TS-08](typescript.md) | Remove demonstrably discarded generator scaffolding | Integrated; generated corpus byte-identical before typed conversion changes |
| [OAS-01](openapi.md) | Keep schemas structured until final rendering | Integrated; 44 parsed documents structurally unchanged; compiler schema/docs tests pass |
| [OAS-02](openapi.md) | Share prepared schema references with GraphQL/MCP | Integrated; GraphQL byte-identical and OpenAPI structurally equal |
| [OAS-03](openapi.md) | Share JSON Schema combinators only behind explicit projection decisions | Integrated; explicit projection characterization passes |
| [JV-01](java.md) | Give fixed MCP metadata a construction-time lifetime | Integrated; worker generated MCP 18 and mux 8 checks pass; clean JVM/JS embedding passes |
| [JV-02](java.md) | Stop constructing JSON parsers per RPC invocation | Integrated; context-owned lazy immutable reader; empty/malformed/null/duplicate/trailing-input characterization passes |
| [JV-03](java.md) | Centralize scalar wire expressions within the Java backend | Integrated scalar emitter; compiler characterization, 132 generated runtime checks and four codec-mode compilations pass |
| [JV-04](java.md) | Represent service invocation stages explicitly | Integrated typed facts and shared sync/async invocation policy; four full-corpus result/async output comparisons byte-identical outside intended runtime changes |
| [JV-05](java.md) | Move schema-independent `any` framing out of each generated codec | Integrated runtime any helpers; 132 generated runtime checks and four split-runtime/codec-mode compilations pass |
| [JV-06](java.md) | Eliminate discarded UEBA materialization | Count-only index path integrated and tested; stream-copy optimization rejected after public-overload characterization demonstrates observable difference |
| [JV-07](java.md) | Consolidate reflective metadata discovery | Integrated typed reflective access without caching; 5 before/after checks and 132 integrated generated runtime checks pass |
| [JV-08](java.md) | Share identifier classification, not target-language parsers | Integrated through shared identifier classifier; full Java regular corpus byte-identical |
| [GQL-01](graphql.md) | Share runtime-mapping resolution and schema naming, not target projections | Integrated; entire GraphQL generated corpus byte-identical |
| [GQL-02](graphql.md) | Resolve field references once and derive dependencies from that result | Integrated; entire GraphQL generated corpus byte-identical |
| [GQL-03](graphql.md) | Isolate SDL description literal encoding from documentation composition | Integrated; description characterization and byte parity pass |
| [SW-01](swift.md) | Replace structural foreign-binding reflection with typed resolution | Integrated; fresh generated compilation and normalized corpus comparison pass |
| [SW-02](swift.md) | Ship invariant `any` field helpers once | Integrated; fresh runtime/any checks and runtime-only/without assembly pass |
| [SW-03](swift.md) | Share scalar codec lowering with service wiring | Integrated; normalized generated corpus unchanged; fresh generated checks pass |
| [SW-04](swift.md) | Avoid rebuilding a closure dictionary for each service invocation | Integrated switch dispatch; fresh async and errors-mode service smoke programs pass |
| [SW-05](swift.md) | Factor MCP's pure protocol work without erasing async semantics | Integrated protocol factoring and generated index; fresh sync 13/async 9 MCP checks pass, including actor isolation |
| [SW-06](swift.md) | Encode binary UUIDs as bytes, not text | Integrated direct UUID tuple bytes; primitive characterization and fresh runtime checks pass |
| [SW-07](swift.md) | Consume ignored UEBA index entries without materializing them | Integrated count-only index consumption; cursor/error characterization and fresh runtime checks pass |
| [SW-08](swift.md) | Make identifier fixed-width reads proportional to consumed text | Integrated bounded scalar cursor; identifier characterization and fresh runtime checks pass |

## Verification record

- Baseline `sbt -batch '+compile' 'baboonJVM/test'`: passed both platforms and all 818 tests.
- Four schema characterization cases passed before schema changes.
- First integrated Python batch: clean JVM/JS compile passed; regenerated collection evolution (2) and domain facade (4) tests passed after recorded failures.
- Python: actual regenerated runtime suite 172, generated self-codec checks 52, field-plan fixtures 2, sync MCP 17 and async MCP 5 passed; split runtime-only/without checks passed. Cross-language fixture matrix remains separate.
- C# collection hashing: 3 tests passed before and after sort removal, including callback order and exceptions.
- TypeScript storage: 6 characterization tests passed before and after fixed-width write allocation changes; 5 existing reader checks passed.
- Baseline generated corpus captured for all nine executable backends, GraphQL/OpenAPI, and MCP before corresponding generator refactors.
- Eleventh integrated clean JVM/Scala.js build and compiler suite passed: 839 tests, 135 suites; 3 pre-existing canceled tests.
- Twelfth integrated clean JVM/Scala.js build and compiler suite passed: 840 tests, 136 suites; 3 pre-existing canceled tests. This build includes shared ADT-branch rename planning and the final C# culture guard.
- Fourteenth integrated clean JVM/Scala.js build passed the same 840 tests and 136 suites, with the same 3 pre-existing cancellations. It includes all runtime source/target identity corrections, Scala target qualification and the reviewed Python custom-validator correction.
- Actual generated C# netstandard2.1 definitions compiled; 144 hand-written runtime/facade/identifier checks passed. An initial broad filter also selected generated peer-fixture tests, which failed because this isolated directory lacks cross-language fixtures; they are not counted as semantic regressions or passes.
- Current isolated C# runtime checks: 24 passed, including four MCP dispatch variants and owned-registry mutation isolation; actual freshly generated MCP suite also passes 22 checks.
- Rust: typed evolution 6 cases, regular/wrapped focused 36 each, split-runtime any 18, and fresh MCP sync 11/async 9 passed. The sixth case covers nested ADT branch evolution.
- Swift: fresh runtime/characterization 85, MCP sync 13/async 9 and split-runtime 81 checks passed; async and errors-mode service smoke programs passed. Comparison of 507 generated files found no changes beyond the intended helper/index changes and indentation.
- TypeScript: fresh strict generated build and 188 runtime checks passed; sync MCP 34/async 3 passed.
- Java: direct runtime suite passes 12 checks; actual generated runtime suite passes 132. Four split-runtime JSON/binary enablement combinations compile. Four sync/async × errors/no-errors full-corpus comparisons are byte-identical outside the intended runtime changes.
- C#: all four codec combinations compile as netstandard2.1 with warnings-as-errors; split runtime/model assembly passes 82 focused tests; async Either JSON/UEBA smoke passes. Runtime assembly preserves model-owned generated domain files.
- Dart: fresh 158 runtime, 9 sync wiring and 15 MCP tests pass; Future-service dispatch passes. All four independently generated runtime-only/without and codec-toggle combinations analyze and execute successfully; 476 unaffected generated files compare equal after the explicit writer-call normalization.
- `mdl :test-editors` passed. The tenth integrated build exposed an unannotated Python field-plan map inference error; corrected before the next clean build.
- Fresh Python follow-up: regular/wrapped review suites 11 each, async scalar checks 15, runtime 172 and self-codec 52 pass. Two facade failures and ten scalar subcase failures were recorded before correction.
- First `mdl :build :test`: native build passed; `test-gen-manual` stopped at the TypeScript renamed-ADT conversion regression. Full C# regular/wrapped lanes subsequently passed. Java and Dart regular/wrapped lanes also passed. Parallel language lanes stopped on Kotlin compiler heap exhaustion; KMP regular passed on an isolated serial retry. The final serial CI run below supersedes these incomplete results.
- Shared ADT rename follow-up: TypeScript regular/wrapped evolution suites pass 10 cases each with strict type-checking, and the full manual corpus generates and type-checks. Absolute/relative namespace runtime probes pass 2 cases. Actual Rust, Java, Dart and Swift renamed-parent conversions pass; Java/Dart/Swift also pass JSON and compact/indexed UEBA roundtrips. C#/Scala/Kotlin/Python initially emitted the missing converters but rejected renamed results in runtime validation; the authorized source/target contract correction below resolves this.
- Final integrated verification: `mdl --seq --keep-run-dir --simple-log :build :test :ci`, run `20260917-130210-567957856`, followed by `mdl --continue --seq --keep-run-dir --simple-log :build :test :ci`, run `20260917-135417-702846955`, passed all 205 planned actions cumulatively. The resume reused successful actions after adding the missing test-only Node types dependency; compiler/runtime sources were unchanged between the two runs.
- Final matrix evidence: clean JVM/Scala.js and native builds, native smoke, all regular/wrapped and manual backend lanes, MCP and service-wiring variants, GraphQL/OpenAPI, editor tests, binary compatibility and diff checks passed. Compiler tests: 843 passed, 136 suites, no cancellations. Serialization acceptance: 200/200; service RPC acceptance: 162/162; neither matrix had build, execution or unexpected failures. Kotlin KMP execution is JVM-backed.
- The new TypeScript Buffer ownership test exposed an undeclared `node:buffer` type dependency in the clean stub (`TS2307`). Added pinned `@types/node` 24.13.5, compatible with the project's Node 24 and TypeScript 5.7, without changing test semantics. Clean install, strict build and all six storage tests passed; both full TypeScript lanes then passed in the resumed run. Existing npm audit warnings were not remediated by unrelated dependency upgrades.
- Commit preparation removed only trailing whitespace/blank lines from the new Swift any-field runtime resource. A further clean JVM/Scala.js compile, compiler suite (840 passed, 3 fixture-dependent cancellations) and native build passed; the earlier complete CI run exercised all 843 compiler tests. Pre-commit `mdl --continue --seq --keep-run-dir --simple-log :build :test` reused the successful backend matrix; no semantic source changes followed the full matrix.
- No application-level throughput improvement is claimed.

## Explicit preservation decisions

- Foreign conversion hooks, floating-point-to-decimal behavior and custom-required conversions are not bypassed merely to remove serialization code.
- Collection fusion is withheld where it changes iterator, hashing, foreign-hook or failure/cursor ordering without a proof over the supported input domain.
- Generated C# MCP `Tools` now returns detached, read-only snapshots rather than exposing its stable backing list. This is intentional metadata-ownership strengthening, not object-identity or cast-and-mutate equivalence; external dynamic subclasses retain their lookup contract.
- C# public positional records retain equality and `with` behavior. Private entries cache only under the same immutable standard CultureInfo parser context; mutable, switched and subclass-defined formats use per-access parsing. Registration remains lazy for malformed version strings.
- Existing Kotlin offset-timestamp negative-fraction formatting differs from the UTC identifier formatter. Civil-date extraction preserves both policies; it is not a wire-format correction.
- C# muxer server-retention lists remain: removing them would change the lifetime of registered zero-tool servers.
- Java indexed encoding retains its copied bulk write: `IndexConsumptionTest.streamCopyOverloadsAreObservableToPublicSubclasses` demonstrates that `writeTo` selects a different overridable public overload.
- TypeScript NodeNext extensionless embedded-runtime imports predate this work; strict supported-mode builds pass, but this refactor does not claim to repair that separate output-mode limitation.
- Dart retains existing acronym-specific naming, callback-sensitive collection staging, mutable tool registries and string dispatch. Only equivalent physical-library routing, fixed-pair hashing and shared protocol decisions are consolidated.
- Python conversion registries remain caller-supplied because generated RequiredConversions may require custom hooks; versioned-facade regression tests register them through the existing public API.
- Conversion identifiers retain their source meaning in existing public APIs. C#/Scala/Kotlin generated renamed converters override a protected target identifier; legacy custom converters retain the same-identifier default. Validation timing and foreign-value exemptions remain unchanged.
- Python's overridable validator remains active on both source and result, including custom subclasses of regenerated renamed conversion bases. For renamed converters the public helper recognizes either endpoint, while `convert` checks each direction separately. The legacy no-target path preserves overridden validators without forcing metadata reads. The existing non-ADT validation omission is not changed by this correction.
- Runtime helper extraction requires matched generator/runtime versions; runtime-only and runtime-without are independently generated and assembled in focused checks.

## Integration closeout — 2026-09-17

The user authorized the shared ADT rename planning correction. The comparator now derives same-name branch mappings when their owning ADT is renamed, allowing ordinary DTO evolution planning to emit branch conversions. Explicit rename sources/targets take precedence. Rule-level coverage checks transfers, widening/default initialization, retained custom requirements and unique retained-branch operations; the TypeScript regression exercises the actual generated parent conversion. The integrated rule test and TypeScript regular/wrapped probes pass.

Baseline probes independently reproduced missing renamed-branch conversions in C#, Scala, Kotlin, Python, Java, Dart and Swift; TypeScript failed during generation. Rust's serialization fallback completed the same probe. A separate frontend limitation remains outside this correction: an explicit old-branch reference under a renamed parent can resolve its missing old owner as a namespace rather than an ADT. The explicit-rename regression uses the supported unchanged-parent form.

The twelfth-build combined serial `mdl :build :test :ci` run passed native build and portability smoke, then failed Scala acceptance compilation: newly emitted namespace-renamed ADT branch conversions used unqualified target ADT names. Standalone Scala compilation reproduced six unresolved-symbol errors. The other nine acceptance builds and 162 serialization triplets passed; 38 triplets could not run because of the Scala build failure. After qualifying both target-reference sites, the final fourteenth-build matrix passes all 200 serialization triplets.

Cross-backend verification exposed a second defect in the C#, Scala, Kotlin and Python conversion runtimes: their single source identifier is also used to validate the target value, rejecting legitimate renamed results. This is reproduced after successful conversion registration. The user authorized the runtime/generator contract correction on 2026-09-17. The correction and persistent runtime/manual regressions are integrated for all four languages, including Kotlin JVM/KMP. Scala conversion target references are fully qualified at both construction and field-transfer sites.

Fresh thirteenth-build outputs pass 10 C# runtime cases and 3 manual rename cases, plus minimal and custom-required conversion probes. Scala/Kotlin JVM/KMP fresh generation, manual compilation and custom-required concrete-subclass execution pass; KMP execution is JVM-backed. Independent review reproduced a Python application-validator bypass in the first target-aware draft; the corrected path preserves both custom validator callbacks. Fresh fourteenth-build Python outputs pass 183 runtime tests, 5 manual conversion tests, renamed-ADT JSON/compact/indexed UEBA probes and generated custom-required subclass checks with and without `super()` in the validator. All focused checks are against embedded runtime output, not source overlays. Final integrated CI also passes as recorded above. Verification completed before committing; no publication actions were performed.
