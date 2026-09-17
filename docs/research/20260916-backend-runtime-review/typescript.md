# TypeScript backend and embedded runtime review

Baseline `69bbb4c48a519d9a4148f370b76bde19af4bac6a`. Full read: 16 generator files, 5,662 lines; seven runtime files, 3,734 lines; 9,396 total. Truncated translator output was recovered with a narrower read. Repository unchanged. Performance effects below are hypotheses from explicit allocations, not benchmark results.

Citation prefixes: **G/** = `baboon-compiler/src/main/scala/io/septimalmind/baboon/translator/typescript/`; **R/** = `baboon-compiler/src/main/resources/baboon-runtime/typescript/`.

## Generator

### TS-01 — Make output symbols explicit instead of inferring exports from references

Observed: G/TsBaboonTranslator.scala:602 derives exports from `TsType` occurrences whose module matches the output. String-rendered declarations require `hasExports` text scanning (:615), and barrel generation has a separate verbatim-file path (:795). Conversely, service signatures rendered as strings require phantom aliases solely to trigger imports (G/TsServiceWiringTranslator.scala:293, :554, :839). MCP and facade generators bypass the normal symbolic import machinery with hand-written import text (G/TsMcpServerGenerator.scala:116; G/TsBaboonTranslator.scala:228).

Proposal: enrich existing `Output` with explicit exported symbols and required imports, carrying type/value usage; populate these at declaration sites and consume them in import/barrel rendering. First migrate service/facade outputs, then delete textual fallbacks when exhaustive. Retain symbolic `TextTree` references for expression aliasing. Benefit: fewer hidden coupling rules and phantom generated declarations, not a universal AST renderer. Effort M; risk H; confidence high. Verify strict ESM/NodeNext, type-only imports, suffix modes, duplicate names, bare services, service-only domains, foreign-empty files and JSON-disabled output; compare existing public export names before/after.

### TS-02 — Lower evolution operations to typed value transformations

Observed: G/TsConversionTranslator.scala:49 emits `JSON.parse(JSON.stringify(...))`; ADT copies repeat it (:119), and DTO transfer/wrapping/precision/collection/redefinition paths use it (:148–195). This is independent of the generated JSON codec, whose bigint/bytes rules differ (G/TsJsonCodecGenerator.scala:201). The facade conversion API remains an explicit failure stub (R/BaboonCodecsFacade.ts:503), while generated facade conversion registries contain only version accessors (G/TsBaboonTranslator.scala:310).

Proposal: recursively lower the existing typed `FieldOp`/`TypeRef` to constructors, collection mapping and numeric conversion; preserve registered custom-conversion requirements. Treat connecting facade dispatch as a separate public-API feature, not cleanup hidden in this refactor. Benefit: removes serialization work and represents migration semantics explicitly. Effort L; risk H; confidence high on mechanism, generated-domain impact requires reproduction. A small Node probe confirmed this expression throws on `1n` and changes `Set([1,2])`/`Map([["x",1]])` to `{}`; it did not execute compiler-generated conversions. Verify compiled multi-version models with bigint, Map/Record, Set, nested DTO/ADT prototypes, rename/redef and lowercase enum modes before claiming a model-specific defect.

### TS-03 — Share codec activation and scalar rendering, not wire containers

Observed: facade activation logic (G/TsBaboonTranslator.scala:133/:140) duplicates JSON/UEBA `isActive` (:622 in G/TsJsonCodecGenerator.scala; :632 in G/TsUEBACodecGenerator.scala). Its six-element type tuple is recomputed for imports and registries (:240/:269). Service primitive mappings (:119/:151/:176/:215 in G/TsServiceWiringTranslator.scala) repeat codec mappings (JSON :200/:403; UEBA :309/:405).

Proposal: derive a named per-version codec-entry plan once, sharing an activation predicate, and use TS-local scalar expression builders from codec and service emitters. Include timestamp representation and foreign binding decisions explicitly; keep key parsing separate from value decoding. This reduces maintained policy copies and repeat compiler traversal, with unchanged generated code as initial acceptance. Effort M; risk M; confidence high. Verify all scalar modes, derivation opt-ins, JSON-only/UEBA-only output, foreign hooks and context propagation; test bigint wire strings separately from numeric JSON values.

### TS-04 — Move invariant any helpers into runtime modules

Observed: complete parameterized helper methods are emitted per any-bearing codec (G/TsJsonCodecGenerator.scala:85/:543; G/TsUEBACodecGenerator.scala:84/:538). They depend on runtime types and explicit kind/fallback arguments, not DTO identity. Detection/fallback policy is separately repeated in both generators.

Proposal: dedicated runtime functions with generated call sites, plus a shared typed compiler decision for kind/static fallbacks. Benefit: generated-source reduction scales with affected codecs and runtime logic has one maintained implementation; bundle-size gain is unmeasured. Preserve kind checks before conversion, exception messages, undefined-versus-null, trailer consumption and facade requirements. Avoid a runtime value-import cycle: preserve existing type-only facade imports and keep helper resources below JVM constant limits. Effort M; risk M; confidence high. Verify all six variants, nested fields, native/cross-format paths and runtime emission with JSON codecs disabled.

### TS-05 — Remove collection intermediates from generated codecs

Observed: UEBA list encoding copies an array merely to read its length (G/TsUEBACodecGenerator.scala:370). JSON list/set encoding uses `Array.from(...).map(...)` (G/TsJsonCodecGenerator.scala:248/:250); map encoding materializes entry arrays and mapped arrays (:256–268). UEBA Set/Map decoding materializes complete intermediate arrays (:459–466).

Proposal: use array length for supported list values, fuse `Array.from(iterable, mapper)` where equivalent, and use small loops to construct map/set results directly. Benefit: fewer temporary collections and traversals, not fundamentally different asymptotics. Preserve iteration order, sparse-array semantics where relevant, duplicate-key behavior, bigint decoding and Map-versus-Record representation; do not casually replace `Object.fromEntries` with unsafe property assignment. Effort M; risk M; confidence high. Verify nested collections, empty/sparse arrays, `__proto__` keys and both representation modes; benchmark large collections separately.

Collection/iterator fusion changes when mapper calls and hashing/equality occur relative to iteration or decoding. Characterize mutation, throwing foreign hooks, malformed later elements and cursor state; retain staged behavior where equivalence cannot be established.

## Embedded runtime

### TS-06 — Write fixed-width primitives directly into writer storage

Observed: R/BaboonSharedRuntime.ts:384/:390/:396/:402/:412/:418/:424/:430/:436 allocate an ArrayBuffer, DataView and Uint8Array per primitive before copying into `BaboonBinWriter`. Reader `readBytes` (:160) performs `slice`, then constructs another Uint8Array from that copy.

Proposal: writer-owned DataView, refreshed after growth, with fixed-width write methods and existing BinTools wrappers retained. For reads, eliminate redundant copying while explicitly preserving ownership; Buffer subclasses and overridden slice behavior need checking before returning the first slice directly. Benefit: removes concrete temporary allocations; speedup unknown. Effort M; risk M; confidence high. Verify little-endian golden bytes, bigint extrema, float bit patterns, growth boundaries, Buffer/nonzero-offset inputs, output independence and malformed-input cursor/error behavior. Do not silently change current truncation policy as part of optimization.

### TS-07 — Factor MCP protocol preparation and response construction

Observed: R/BaboonMcpRuntime.ts repeats the state machine at :195/:314/:441/:539. Single-server `byName` recreates a Map per call (:189/:435); muxers already construct routing tables at registration. Shared service runtime independently repeats registry mechanics across four muxers (R/BaboonSharedRuntime.ts:759/:782/:821/:844).

Proposal: pure MCP preparation returning immediate response/notification or a typed pending call; thin sync/Promise invokers and shared response formatting. A small generic registration/lookup component can serve service muxers while public wrappers remain. Cache generated immutable tool indexes, but do not assume arbitrary subclass getters are immutable. Benefit: less maintained protocol duplication and request-local indexing. Effort M; risk H; confidence high. Verify session isolation, tool order, duplicate registration, exceptions versus Left results, notifications and Promise rejection timing. Keep MCP optional and transport-abstract.

### TS-08 — Remove demonstrably discarded generator scaffolding

Observed: G/TsUEBACodecGenerator.scala:287 constructs an export-function TextTree and discards it before returning the actual codec bodies. G/TsConversionTranslator.scala:213 has an unreachable `if (false)` error arm. These are local expression/control-flow observations, not repository-wide claims that public symbols are unused.

Proposal: remove the unused expression and constant-false branch; then remove imports made unused by that edit. Benefit: less maintained misleading source and needless tree construction; emitted source should be byte-identical. Effort S; risk L; confidence high. Verify compiler cross-build and representative generated-output identity; no runtime API removal.

## Sequence, verification and boundaries

Start TS-08, then activation/scalar sharing and explicit output metadata; separate runtime allocation work from any-helper relocation; leave typed evolution and MCP restructuring for independently reviewable changes. TS already consumes UEBA indices without allocating entry objects (G/TsUEBACodecGenerator.scala:149), and service dispatch already uses switches: Rust/Swift recommendations to change those structures do not apply here. Preserve native bigint, enum-lowercase choices, public toJSON behavior, type/value namespace distinctions, barrels, timestamp modes and always-Promise clients even for synchronous servers. Do not merge runtime resources for file-count reduction.

Actual checks: complete `cat`/`sed` reads, `rg --files`, `wc -l`, targeted `rg -n`, Node JSON-clone probe and clean `git status --short`. No builds/installations or generated-code executions. Probe command: `node -e 'for (const [name,value] of [["bigint",1n],["set",new Set([1,2])],["map",new Map([["x",1]])]]) {try {console.log(name,JSON.stringify(JSON.parse(JSON.stringify(value))))} catch(e){console.log(name,e.name+": "+e.message)}}'`. Output: bigint TypeError “Do not know how to serialize a BigInt”; set `{}`; map `{}`.

Proposed checks follow constructive-test-taxonomy: behavioral checks through public interfaces plus generated compile/wire conformance; performance measurement separate. Future changes require clean runtime embedding rebuild and regular/wrapped/manual/service/MCP plus cross-language acceptance lanes. No test bodies were audited this assignment.

## Complete inventory

Full-read generator files (lines): TsBaboonTranslator.scala 891; TsCodecFixtureTranslator.scala 241; TsCodecTestsTranslator.scala 180; TsCodecTranslator.scala 12; TsConversionTranslator.scala 220; TsDefnTranslator.scala 910; TsDomainTreeTools.scala 111; TsFileTools.scala 68; TsJsonCodecGenerator.scala 629; TsMcpServerGenerator.scala 157; TsServiceWiringTranslator.scala 1070; TsTreeTools.scala 65; TsTypeTranslator.scala 347; TsTypes.scala 93; TsUEBACodecGenerator.scala 639; TsValue.scala 29.

Full-read runtime files (lines): BaboonAnyOpaque.ts 304; BaboonCodecsFacade.ts 706; BaboonMcpRuntime.ts 609; BaboonSharedFixture.ts 112; BaboonSharedRuntime.ts 1536; CrossLanguageFixturePath.ts 118; baboon-identifier-repr.ts 349. CLAUDE.md and review brief fully read. No sampled-only assigned files. Cross-backend questions: reuse typed evolution lowering decisions and any metadata plans? Share MCP conformance fixtures while leaving target-language effect execution distinct?
