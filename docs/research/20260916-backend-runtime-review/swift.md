# Swift backend and embedded runtime review

Baseline: `69bbb4c48a519d9a4148f370b76bde19af4bac6a`. Repository unchanged. All 16 generator files (6,094 lines) and all nine embedded-runtime files (4,583 lines) were read completely, including recovered truncated output. Findings below are observations and bounded refactoring opportunities, not reproduced defects or measured performance claims.

Citation prefixes expand to repository-relative paths: **G/** = `baboon-compiler/src/main/scala/io/septimalmind/baboon/translator/swift/`; **R/** = `baboon-compiler/src/main/resources/baboon-runtime/swift/`.

## Generator findings

### SW-01 — Replace structural foreign-binding reflection with typed resolution

Observed: G/SwTypeTranslator.scala:12 and :129 define identical `Product`→`Map[String, Any]` reflection helpers. Alias extraction is repeated at :16 and :158; declaration extraction at :133 contains compatibility paths for historical product layouts. The current sealed model has only `Custom` and `BaboonRef`, with a typed `ForeignEntry` (`baboon-compiler/src/main/scala/io/septimalmind/baboon/typer/model/Typedef.scala:52`). `BaboonEnquiries.resolveForeignBinding` already matches those alternatives (:101 in the adjacent `typer/BaboonEnquiries.scala`).

Proposal: use typed matching and `BaboonLang.Swift`, then share the Swift-specific fallback-to-another-language's-Baboon-alias policy between reference/type rendering. Do not blindly substitute the enquiries method: it deliberately does not implement that fallback. Preserve collection aliases versus scalar references and failure messages. This removes internal compatibility scaffolding and map allocation, principally improving maintained type safety rather than generated size. Effort M; risk M; confidence high. Verify foreign custom bindings, Swift aliases, absent Swift bindings, and nested collection aliases through generated-source comparison and Swift compilation.

### SW-02 — Ship invariant `any` field helpers once

Observed: JSON and UEBA inject complete helper bodies into every any-bearing codec (G/SwJsonCodecGenerator.scala:63, :550; G/SwUEBACodecGenerator.scala:102, :563). Bodies accept context, kind and static fallback arguments; they do not need the enclosing DTO. Detection and fallback tables are also duplicated (JSON :472/:513; UEBA :486/:526).

Proposal: move helper bodies to a dedicated runtime file and generate calls; share the compiler's typed any-variant/fallback decision separately from Swift rendering. This reduces generated source volume proportional to affected codecs and centralizes runtime policy; no executable-size reduction is assumed because compiler deduplication is unknown. Preserve NSNull/nil distinctions, wire-over-static precedence, metadata trailer handling and nonthrowing encode traps versus throwing decode. Keep JSON and UEBA framing separate. Effort M; risk M; confidence high. Verify all six kinds, nested collections, cross-format conversion, missing facade and malformed metadata; compare JSON/UEBA bytes.

### SW-03 — Share scalar codec lowering with service wiring

Observed: G/SwServiceWiringTranslator.scala:146/:172 independently enumerate JSON decode/encode primitives already rendered in G/SwJsonCodecGenerator.scala:304/:210; UEBA tables at wiring :194/:220 duplicate G/SwUEBACodecGenerator.scala:360/:418. All four mappings encode wire policy, not service dispatch policy.

Proposal: introduce Swift-local scalar expression builders with explicit context/reader/value inputs and a named throwing-effect result instead of an unlabelled Boolean. Have both DTO and service emitters consume them. Keep JSON dictionary-key representation distinct. Benefit: fewer maintained policy copies; generated code need not change. Effort M; risk M; confidence high. Verify every primitive as DTO field and direct RPC payload, with decode exceptions and async effect insertion checked by compilation and public-interface round trips.

### SW-04 — Avoid rebuilding a closure dictionary for each service invocation

Observed: generated dispatchers allocate a complete method→capturing-closure dictionary inside each call (G/SwServiceWiringTranslator.scala:514, :560, :661, :865) and immediately select one entry. This includes synchronous/asynchronous and errors/no-errors variants.

Proposal: emit a direct `switch` on method name, retaining each existing body and default error branch. This removes source-level per-request table construction and unused closures; actual allocation/latency savings require measurement. Separately extract small repeated decode/call/encode fragments from sync/async bodies (:676/:765 and :876/:957), not a universal service renderer. Effort M; risk M; confidence high. Verify method-not-found, service-id validation, each error channel, context modes and direct async suspension; benchmark method-count scaling independently of correctness tests.

## Runtime and generated-runtime interaction

### SW-05 — Factor MCP's pure protocol work without erasing async semantics

Observed: four state machines repeat initialize validation, session gating, tool-list formatting, argument serialization and response construction (R/baboon_mcp_runtime.swift:174, :345, :565, :733). Single-server `byName` recreates an index per call (:151/:329), whereas muxers build routing tables at registration. Tool-list formatting is repeated inline and at :647/:807.

Proposal: share pure request preparation and response formatting, producing either an immediate reply/notification or a typed pending tool call. Retain thin synchronous and genuinely asynchronous invokers. For generated immutable tool lists, construct a reusable index; do not impose caching on arbitrary protocol conformers whose `tools` may change. Benefit: less maintained protocol duplication and avoidable indexing; no generated per-service bulk renderer needed. Effort M; risk H; confidence high. Verify identical channel-A/channel-B payloads, notification behavior, declaration/registration ordering and per-connection state across all four surfaces. Async must remain direct `await`, without semaphore or detached-task bridging.

### SW-06 — Encode binary UUIDs as bytes, not text

Observed: R/baboon_runtime.swift:479 converts UUID to string, removes separators and parses hexadecimal back to bytes. `readUuid` (:742) reverses the process by formatting bytes and reparsing a string. These transformations serve the fixed .NET mixed-endian permutation.

Proposal: use UUID's byte tuple and an explicit fixed permutation, preserving the 16-byte wire layout and truncation behavior. This removes observed intermediate string/array work; throughput benefit is inferred, unmeasured. Effort S; risk M; confidence high. Verify known GUID golden bytes, all-zero/all-ones and randomized UUID round trips, including nonzero Data start indices and truncation. Retain the public methods and their throwing shape.

### SW-07 — Consume ignored UEBA index entries without materializing them

Observed: G/SwUEBACodecGenerator.scala:299 obtains `readIndex`, checks only its count when indices are requested, then discards entries. R/baboon_runtime.swift:285 allocates and appends every offset/length pair. Index presence is determined by the wire header, not the context flag.

Proposal: add an internal consume-and-count path for generated sequential decoders while retaining public `readIndex`. Read both integers exactly as today rather than unchecked cursor arithmetic; preserve header interpretation, count assertion and malformed-input failure behavior. Benefit: eliminate an otherwise unused per-object collection; effort S; risk M; confidence high. Verify indexed/compact DTO and ADT branches, header/context mismatch, truncation and forward-readable trailing fields. Same opportunity was observed in Rust; this is format-shared semantics, not grounds for sharing runtime implementation languages.

### SW-08 — Make identifier fixed-width reads proportional to consumed text

Observed: R/BaboonIdentifierRepr.swift:287 creates a String from the entire remaining byte suffix before consuming `n` Unicode scalars. Repeated fixed-width fields therefore repeatedly process suffixes; input/schema scaling can make aggregate work quadratic. Cursor also retains `source` (:222/:227), but all reads use `bytes`; full-file inspection found no read of that private property.

Proposal: advance over the existing UTF-8 buffer for exactly `n` scalars, decoding only the consumed slice; remove redundant source storage after confirming lifetime/memory behavior. Preserve Unicode-scalar semantics, cursor position on short input and exact error strings; do not substitute a general byte count merely because current timestamp callers use ASCII. Effort M; risk M; confidence high on source behavior, medium on practical impact. Verify multibyte scalars, empty/short input, repeated timestamp fields and identifiers with long suffixes; measure allocations separately.

## Sequencing and boundaries

Start SW-01/SW-03 with output-preserving generator changes, then SW-02/SW-07, isolated SW-06/SW-08, and finally dispatch/MCP changes. Keep Swift's recursive value-layout analysis and `@BaboonIndirect` copy-on-write property semantics (G/SwDefnTranslator.scala:355; R/baboon_runtime.swift:30), native async effects, type-erased public service protocols, evolution contracts and Foundation JSON bridging distinct. Runtime file splitting is required by the JVM 64KB embedded-string limit: new helpers should use suitably sized files, never merge existing splits for aesthetics. No replacement of typed conversions with serialization round trips is proposed.

Actual checks: `rg --files`, `wc -l`, complete `cat`/`sed` reads, targeted `rg -n` evidence searches, sampled typed-model/enquiries inspection, sampled SwiftPM target references, `git rev-parse HEAD` and clean `git status --short`. No builds, executions, benchmarks or failing reproductions. Proposed verification follows the constructive-test-taxonomy skill: behavioral checks through public interfaces plus compile/wire conformance, with benchmarks explicitly separate. Later implementation needs clean resource recompilation, Swift regular/wrapped/manual/service/MCP lanes and cross-language acceptance; new fixtures require explicit entries in `test/sw-stub/Package.swift`, not just source directories.

## Full inventory / coverage

Every entry below was read in full; numbers are lines, not complexity scores.

Generator: SwBaboonTranslator.scala 563; SwCodecFixtureTranslator.scala 344; SwCodecTestsTranslator.scala 300; SwCodecTranslator.scala 17; SwConversionTranslator.scala 399; SwDefnTranslator.scala 1062; SwDomainTreeTools.scala 120; SwFileTools.scala 15; SwJsonCodecGenerator.scala 670; SwMcpServerGenerator.scala 201; SwServiceWiringTranslator.scala 1150; SwTreeTools.scala 64; SwTypeTranslator.scala 346; SwTypes.scala 109; SwUEBACodecGenerator.scala 695; SwValue.scala 39.

Runtime: BaboonAnyOpaque.swift 281; BaboonCodecsFacade.swift 775; BaboonIdentifierRepr.swift 372; CrossLanguageFixturePath.swift 139; baboon_fixture.swift 140; baboon_mcp_runtime.swift 826; baboon_runtime.swift 1436; baboon_service_wiring.swift 282; baboon_type_meta.swift 332.

Supporting material: CLAUDE.md and review brief fully read; Typedef.scala, BaboonEnquiries.scala and SwiftPM manifest sampled only. Test bodies were not audited. Cross-backend questions: share typed any/foreign decisions centrally while preserving Swift fallback policy? Apply consume-only UEBA index handling to other sequential decoders? Share MCP protocol fixtures across languages without forcing a shared renderer?
