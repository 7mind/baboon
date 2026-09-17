# Rust backend/runtime audit

Reviewed HEAD `69bbb4c48a519d9a4148f370b76bde19af4bac6a`. All 16 Rust generator files (5,588 lines) and all nine embedded runtime files (4,571 lines) were read completely. This is a static audit: allocation sites and algorithm structure are observed; elapsed-time improvements are unmeasured. No confirmed behavioral defects, source edits, or test executions are claimed.

In references below, `G/` expands to the exact repository-relative directory `baboon-compiler/src/main/scala/io/septimalmind/baboon/translator/rust/`; `R/` expands to `baboon-compiler/src/main/resources/baboon-runtime/rust/`.

## Findings

### RS-01 — Move invariant any-field framing from emitted modules into the runtime

Observed: `G/RsUEBACodecGenerator.scala:35` emits the same `anyFieldHelpers` block for every qualifying DTO; the block at `:68` accepts all model-specific information as arguments. Its call sites at `:164` and `:175` already separate framing from declared kind/static fallback information. The native UEBA branch at `:88` clones the entire payload before writing it.

Bounded change: place these helpers in a dedicated runtime module (or suitably sized existing module), retaining generated thin calls. Use a borrowed/owned payload distinction for native bytes versus converted JSON. Keep validation order, error messages/types, length framing, extension-byte skipping and facade resolution unchanged. This reduces generated source volume and duplicated framing maintenance; borrowed bytes also remove an observed payload-sized copy. It does not eliminate the necessary conversion buffer.

Effort M; risk M; confidence high. Verify all six any variants, nested positions, malformed lengths, compact/indexed goldens and cross-format facade tests. Check generated ADT modules containing multiple any-bearing branches as well as standalone DTOs.

### RS-02 — Share service invocation/error policy across wire formats

Observed: `G/RsServiceWiringTranslator.scala:770` and `:868` repeat service call construction across output/no-output, declared error/no-error and sync/async branches. JSON/UEBA no-errors dispatch at `:665` and `:698` repeats a smaller version. The serializer and decoder differ; the service invocation and `CallFailed` mapping largely do not. Runtime muxers likewise repeat registration/lookup at `R/baboon_service_wiring.rs:63`, `:102`, `:163`, `:202`.

Bounded change: extract a Rust-specific method-call plan and renderer, with typed wire-format operations for decode, encode and void value. Separately share the private runtime registry implementation behind unchanged public muxer types. Generator extraction reduces maintained complexity while emitted dispatch can remain identical. Runtime sharing reduces maintained repetition, not necessarily compiled size.

Effort M; risk M; confidence high. Verify errors/no-errors, custom result containers, abstract/concrete/no context and sync/async combinations. Preserve synchronous panic capture, existing asynchronous behavior, context ownership, `?Send` futures and public signatures; do not unify these axes by adding stronger bounds.

### RS-03 — Unify MCP protocol dispatch and use its existing routing index

Observed: `R/baboon_mcp_server.rs:170` and `:424` independently implement initialize, notifications, tools/list, tools/call, protocol errors and Channel-B mapping. Error-response constructors repeat at `:289` and `:538`. The muxer builds a hash route table at `:388`, but `entry` at `:403` scans every entry; `tools_list_union` at `:410` additionally searches the owning server's tools for every entry. This produces linear lookup and potentially quadratic listing work within a large service.

Bounded change: share one protocol dispatcher parameterized by server metadata, ordered tool view and routable invocation. Store entry indices in the route table and tool indices alongside the owning server. Retain declaration/registration order and public routable interfaces. This removes runtime policy duplication and repeated searches; performance impact depends on tool counts.

Effort M; risk M; confidence high. Verify byte-equivalent single-server/muxer responses, initialization state, missing/unknown tool handling, duplicate registration and sync/async overlays. Keep execution adaptation separate: `G/RsMcpServerGenerator.scala:195` contains a generated blocking executor; changing its scheduling contract is a distinct redesign requiring dedicated reproduction and API analysis.

### RS-04 — Centralize Rust field representation decisions

Observed: identical recursive boxing decisions live in `G/RsDefnTranslator.scala:1130` and `G/RsUEBACodecGenerator.scala:202`. Representation uses that decision for stored fields, borrowed wrapped-ADT serialization and contract accessors (`RsDefnTranslator.scala:658`, `:679`, `:800`); codecs independently dereference and reconstruct boxes (`RsUEBACodecGenerator.scala:248`, `:284`). Definition generation also recomputes float reachability in derives and ordering (`RsDefnTranslator.scala:992`, `:1025`).

Bounded change: introduce a per-domain Rust representation analysis returning named field information: surface type, stored type and indirection requirement. First consolidate boxing without changing its policy; only subsequently consider caching comparison capability with cycle-aware analysis. Keep Rust ownership/layout decisions local even if semantic reachability can be shared across backends.

Effort M; risk M; confidence high. Benefit is one authoritative layout decision and fewer repeated graph walks, not an asserted reduction in output. Verify recursive DTOs/ADTs through options and collections, contract references, wrapped/unwrapped branches, and float-bearing nesting.

### RS-05 — Replace JSON-mediated evolution selectively with typed conversions

Observed: `G/RsConversionTranslator.scala:52` lowers conversion to `serde_json::to_value` followed by `from_value`, both unwrapped. Used for transfers containing user types (`:159`), collection wrapping (`:186`), precision expansion (`:194`), collection swapping (`:196`), renames (`:200`) and redefinitions (`:206`); ADT branch conversion independently embeds the same round trip at `:122`.

Bounded redesign: lower existing `FieldOp` semantics into typed Rust construction, nested generated conversion calls and collection iteration. Start with provably equivalent primitive widening and collection transformations. This removes observed intermediate JSON trees and serialization coupling; it may increase generated source while reducing execution work. No claim is made that every current JSON-mediated conversion is incorrect.

Effort L; risk H; confidence high on opportunity, medium on full replacement. Preserve borrowing/cloning behavior, collection ordering and conversion customization. Compare values, panic/error behavior and wire output for historical conversions, enum/branch renames, foreign mappings, timestamps/decimal/bytes and nested collections before expanding the replacement domain.

### RS-06 — Reduce facade lookup allocations behind existing APIs

Observed: `R/baboon_codecs_facade.rs:728` clones the complete registered version vector per resolution although that path uses its first/last entries. `:890` allocates the complete same-in list to select its first element, and `:874` allocates all reader bounds to find one tier. Generated implementations allocate those vectors/strings at `G/RsBaboonTranslator.scala:266`. JSON and UEBA registry implementations at runtime `:278` and `:310` are structural duplicates; `LazyCodec<Arc<dyn Codec>>` adds an outer Arc whose value is cloned again at `:305`/`:333`.

Bounded change: snapshot only min/max for resolution; add compatible metadata accessors with default implementations and allocation-free generated overrides. A private typed registry can share storage/lookup mechanics while public JSON/UEBA interfaces stay distinct. Treat changing `LazyCodec` storage/public return types separately.

Effort M; risk M; confidence high. Verify lookup precedence, same-in ordering, v1/v2 and tolerant/lossless envelopes, re-registration and concurrency. Existing preload test (`test/rs-stub/tests/baboon_codecs_facade_latest_tests.rs:227`) is a smoke test; it is not evidence of initialization race freedom. Benchmark allocations before claiming throughput gains.

### RS-07 — Build module parent/child relationships once

Observed: `G/RsBaboonTranslator.scala:427` iterates all directories and at `:432` filters the entire directory set again to identify each node's direct children. Directory comparisons are quadratic in directory count, independently of output size.

Bounded change: construct a parent-to-direct-children index during the existing path pass, then sort/render each directory's children. Preserve conflict absorption, ADT non-reexport policy, reexport modes and MCP/facade inclusion. This is compiler execution optimization; generated source and runtime complexity need not change.

Effort S; risk L; confidence high. Verify byte-for-byte generation on deep namespaces, type/directory collisions, multiple versions and MCP/no-service fixtures; measure wide/deep synthetic families to establish materiality.

### RS-08 — Remove demonstrably unused internal scaffolding

Observed: `G/RsConversionTranslator.scala:225` has an unreachable `if (false)` error branch. `RsRenderedConversion` at `:26` carries `reg` and `missing`, but its only generator consumer (`G/RsBaboonTranslator.scala:786`) reads `fname` and `conv`. `G/RsDefnTranslator.scala:49` initializes a private `baboonIdReprCursor` value that has no other reference in the file; emitted cursor references are literal strings.

Bounded cleanup: remove the unreachable branch and unused private binding. Remove descriptor fields only after repository-wide usage confirmation; preserve custom-conversion comments and explicitly assess whether the unused `missing` field represents deferred functionality. These are maintained-code savings, not a reason to delete public runtime helpers simply because generated output does not call them.

Effort S; risk L for branch/binding, M for descriptor; confidence high. Verify JVM/JS compilation and unchanged generated conversion output. No runtime API deletion is recommended.

## Sequence, boundaries, and validation

Sequence: RS-08/RS-07 first; RS-04 then RS-02; RS-01 and RS-03 independently; RS-06 after concurrency-focused characterization; RS-05 last as a semantic migration. Keep serde ownership/lifetimes, context/result/async axes, foreign key hooks, canonical identifier parsing versus lenient JSON parsing, and envelope v1/v2 semantics distinct. Do not merge embedded files merely to reduce file count: the JVM 64KB constant limit requires the existing facade/type-meta separation.

Checks actually run: `git rev-parse HEAD` (matched requested commit), `git status --short` (clean), `rg --files` inventories, `wc -l`, complete `cat`/`sed` reads, and targeted `rg -n` reference searches. An initial combined read was truncated; its omitted small files were reread completely. A guessed `test/rs-stub-mcp-overlay` path was absent; inventory located `test/rust-stub-mcp-overlay` and the separate Rust mux overlays. Test files were sampled by symbol search, with the preload body read; no tests/builds/benchmarks ran.

Proposed verification: focused existing Rust regular/wrapped, manual evolution, wiring and MCP/mux async lanes, then `mdl :build :test` before commit and `mdl :ci` before push. Runtime changes require `sbt clean` before recompilation due to embedded-resource caching. No numeric speedup, binary-size or line-reduction estimate is supported by this audit.

## Complete coverage inventory

All entries below are **full-read**, including comments and implementation. No assigned file was sampled only.

Generator (`G/`):

| File | Lines |
|---|---:|
| RsBaboonTranslator.scala | 794 |
| RsCodecFixtureTranslator.scala | 253 |
| RsCodecTestsTranslator.scala | 156 |
| RsCodecTranslator.scala | 11 |
| RsConversionTranslator.scala | 232 |
| RsDefnTranslator.scala | 1691 |
| RsDomainTreeTools.scala | 55 |
| RsFileTools.scala | 26 |
| RsJsonCodecGenerator.scala | 70 |
| RsMcpServerGenerator.scala | 317 |
| RsServiceWiringTranslator.scala | 970 |
| RsTreeTools.scala | 60 |
| RsTypeTranslator.scala | 212 |
| RsTypes.scala | 118 |
| RsUEBACodecGenerator.scala | 587 |
| RsValue.scala | 36 |

Runtime (`R/`):

| File | Lines |
|---|---:|
| any_opaque.rs | 548 |
| baboon_codecs_facade.rs | 1284 |
| baboon_fixture.rs | 142 |
| baboon_identifier_repr.rs | 363 |
| baboon_mcp_server.rs | 549 |
| baboon_runtime.rs | 991 |
| baboon_service_wiring.rs | 237 |
| baboon_type_meta.rs | 338 |
| cross_language_fixture_path.rs | 119 |

Supporting coverage: full `CLAUDE.md`; sampled test symbols in `test/rs-stub/tests/{any_round_trip_tests,baboon_codecs_facade_latest_tests,domain_facade_tests}.rs`; sampled `.mdl/defs/tests.md` action names; repository Scala reference-search matches only. Other backends were not fully reviewed here.

Cross-backend questions: can existing typed evolution operations provide a shared conversion plan without sharing renderers? Do other runtimes duplicate single-server/muxer MCP protocol dispatch? Can fixed any framing be consistently runtime-owned while declared fallback metadata stays generator-owned?
