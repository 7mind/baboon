# Kotlin JVM and Kotlin Multiplatform review

Reviewed HEAD `69bbb4c48a519d9a4148f370b76bde19af4bac6a`; read `CLAUDE.md`. This is a read-only source audit, not a correctness certification or benchmark. All 16 Kotlin generator files and all 29 JVM/KMP runtime files received full-content coverage, either direct reads or a fully read counterpart plus complete diff/hash verification; details below. Findings distinguish maintained Scala templates, embedded Kotlin sources, and generated Kotlin volume. No production edits, builds, installations, or runtime reproductions were performed. Performance benefits below are hypotheses to measure, not reported speedups.

## Findings

### KT-01 — Share exact platform-neutral runtime resources

**Observed:** SHA-256 checks confirm byte-identical JVM/KMP copies of `BaboonEither.kt`, `BaboonExceptions.kt`, and `BaboonServiceWiring.kt`. Their public declarations begin at `baboon-compiler/src/main/resources/baboon-runtime/kotlin/BaboonEither.kt:3`, `baboon-compiler/src/main/resources/baboon-runtime/kotlin/BaboonExceptions.kt:3`, `baboon-compiler/src/main/resources/baboon-runtime/kotlin/BaboonServiceWiring.kt:3`, and the corresponding `baboon-compiler/src/main/resources/baboon-runtime/kotlin-kmp/BaboonEither.kt:3`, `baboon-compiler/src/main/resources/baboon-runtime/kotlin-kmp/BaboonExceptions.kt:3`, `baboon-compiler/src/main/resources/baboon-runtime/kotlin-kmp/BaboonServiceWiring.kt:3`. Selection currently changes the whole resource directory (`baboon-compiler/src/main/scala/io/septimalmind/baboon/translator/kotlin/KtBaboonTranslator.scala:340`).

**Recommendation:** resolve these three output filenames to one common embedded source, retaining separate emitted distributions. This reduces maintained copies, not generated volume. Do not turn every near-duplicate into a preprocessor template: Class/KClass, stream APIs, dates, and concurrency differ materially. Check byte-for-byte output for JVM/KMP, JSON enabled/disabled, and runtime embedding after a clean compiler build. **Effort S; risk L; confidence high.**

### KT-02 — Give primitive wire emission one Kotlin owner

**Observed:** service wiring independently implements JSON decode/encode and UEBA decode/encode tables (`baboon-compiler/src/main/scala/io/septimalmind/baboon/translator/kotlin/KtServiceWiringTranslator.scala:123`, `:153`, `:181`, `:211`). The codec generators repeat these operations (`baboon-compiler/src/main/scala/io/septimalmind/baboon/translator/kotlin/KtJsonCodecGenerator.scala:249`, `:303`; `baboon-compiler/src/main/scala/io/septimalmind/baboon/translator/kotlin/KtUEBACodecGenerator.scala:403`, `:468`). UUID, decimal, and timestamp branches also repeat platform decisions.

**Recommendation:** introduce a Kotlin scalar codec emitter taking typed value/reader/writer/context trees and the existing platform mapping. Keep map-key semantics separate: keys have canonical string representations and custom hooks. Keep user-type/foreign resolution outside the primitive table initially. This reduces semantic maintenance duplication without changing generated size. Verify every scalar through both service parameters and DTO fields, including unsigned extremes and platform-specific decimal/time representations. **Effort M; risk M; confidence high.**

### KT-03 — Separate service planning from dispatch rendering

**Observed:** JSON/UEBA dispatch each have synchronous and asynchronous error-mode paths (`baboon-compiler/src/main/scala/io/septimalmind/baboon/translator/kotlin/KtServiceWiringTranslator.scala:670`, `:779`, `:854`, `:959`), plus no-error paths, wrappers, clients, and runtime-interface emission. Service declarations separately resolve result/context and render signature policy (`baboon-compiler/src/main/scala/io/septimalmind/baboon/translator/kotlin/KtDefnTranslator.scala:425`). Existing shared `ServiceResultResolver`/`ServiceContextResolver` should remain the configuration authority.

**Recommendation:** produce one typed Kotlin method plan containing resolved arguments/result/error/context, escaped names, codec availability, and invocation mode. Render declarations, dispatchers, and clients from it; extract JSON/UEBA transport operations while retaining distinct suspension control flow. The async path intentionally calls the suspend implementation outside non-suspend combinator lambdas; textual deduplication could invalidate it. Benefit is reviewable mode handling, not a promised line-count reduction. Verify errors/no-errors × sync/async × context modes × wire formats, including methods without outputs. **Effort L; risk H; confidence high.**

### KT-04 — Move schema-independent any-envelope helpers into runtime support

**Observed:** each any-bearing JSON codec receives the same helper body (`baboon-compiler/src/main/scala/io/septimalmind/baboon/translator/kotlin/KtJsonCodecGenerator.scala:67`, `:500`); UEBA does likewise (`baboon-compiler/src/main/scala/io/septimalmind/baboon/translator/kotlin/KtUEBACodecGenerator.scala:127`, `:619`). Parameters already carry expected kind and static domain/version/type information. JSON/UEBA also repeat static-fallback planning (`KtJsonCodecGenerator.scala:473`, `KtUEBACodecGenerator.scala:590`, under the same repository directory).

**Recommendation:** emit calls to runtime-owned envelope operations and centralize static fallback facts in a small typed plan. Keep JSON and UEBA operations separate and preserve kind checks, future metadata-window skipping, facade requirements, and wire-data precedence. This primarily reduces generated helper repetition and moves policy out of string templates; it does not remove all maintained implementation. Preserve JSON stripping and runtime public APIs. Verify all six any forms, nested containers, cross-format conversion, malformed metadata, and binary-only output. **Effort M; risk M; confidence high.**

### KT-05 — Remove avoidable intermediate collection and buffer materializations

**Observed:** generated set decoders build a list then a set (`baboon-compiler/src/main/scala/io/septimalmind/baboon/translator/kotlin/KtJsonCodecGenerator.scala:347`; `baboon-compiler/src/main/scala/io/septimalmind/baboon/translator/kotlin/KtUEBACodecGenerator.scala:460`). Conversion generation similarly uses `map(...).toMap()`/`toSet()` (`baboon-compiler/src/main/scala/io/septimalmind/baboon/translator/kotlin/KtConversionTranslator.scala:104`). Indexed DTO encoding copies an intermediate byte array into the destination (`KtUEBACodecGenerator.scala:273`); KMP `toByteArray()` explicitly copies (`baboon-compiler/src/main/resources/baboon-runtime/kotlin-kmp/BaboonTools.kt:257`).

**Recommendation:** first generate direct collection accumulation with preserved insertion order, duplicate-key overwrite behavior, and evaluation order. Separately consider an internal writer-to-writer transfer operation for KMP and JVM stream transfer where compatible, retaining the public copying `toByteArray` contract. Do not remove necessary index buffering or expose mutable backing storage. Benchmark allocation and throughput on collection-heavy and indexed DTOs; compare exact UEBA bytes and decoded collection order. **Effort M; risk M; confidence high for allocation sites, medium for net performance.**

Direct population changes decode-versus-hash/equality sequencing compared with staging all decoded elements first. Verify throwing/effectful foreign hash/equality, malformed later elements and reader position; restrict the optimization to domains with established equivalence where necessary. Iterator/mapper fusion likewise needs evaluation-order checks.

### KT-06 — Reuse MCP protocol handling between server and muxer

**Observed:** individual-server and muxer `handle` implementations repeat initialize, tools/list, argument extraction, and Channel-A/B response construction (`baboon-compiler/src/main/resources/baboon-runtime/kotlin/BaboonMcpRuntime.kt:156`, `:328`). The individual server rebuilds its name map per call (`:150`, `:206`), whereas the muxer indexes registrations (`:309`, `:318`). The generator emits list-valued tool metadata (`baboon-compiler/src/main/scala/io/septimalmind/baboon/translator/kotlin/KtMcpServerGenerator.scala:137`); its read-only Kotlin surface alone does not establish deep or interop immutability.

**Recommendation:** compose both public classes with an internal protocol dispatcher supplied with server information, ordered tools, and lookup/invoke operations. Cache lookup for generated immutable registries; do not silently assume arbitrary subclasses have immutable `tools`. Preserve session ownership, notification behavior, duplicate-tool diagnostics, and context forwarding. Benefit is one protocol policy plus avoidable per-request map construction; no latency measurement was made. Verify equivalent transcripts for standalone and multiplexed servers. **Effort M; risk M; confidence high.**

### KT-07 — Centralize KMP civil-date decomposition

**Observed:** KMP owns two Gregorian epoch-to-calendar implementations: `baboon-compiler/src/main/resources/baboon-runtime/kotlin-kmp/BaboonOffsetDateTime.kt:19` and `baboon-compiler/src/main/resources/baboon-runtime/kotlin-kmp/BaboonIdentifierRepr.kt:135`. They repeat era/year/month arithmetic and floor helpers, but initial millisecond division differs (`:25` versus `:138`). This is evidence of duplicated policy, not a reproduced correctness finding.

**Recommendation:** characterize boundaries first, then share the date-component calculation with explicit formatting choices for UTC `Z` and numeric offsets. Keep identifier fixed-width formatting distinct from general JSON timestamp formatting. Compare negative epoch milliseconds, leap days, midnight offset crossings, and supported year limits against pinned datetime/JVM behavior. Any failing case requires its own reproduction before a correction. **Effort M; risk H because timestamps are wire-visible; confidence high for duplication.**

### KT-08 — Share pure evolution policy only after isolating runtime mechanisms

**Observed:** both facades implement version-range codec choice and latest-version conversion (`baboon-compiler/src/main/resources/baboon-runtime/kotlin/BaboonCodecsFacade.kt:239`, `:393`; `baboon-compiler/src/main/resources/baboon-runtime/kotlin-kmp/BaboonCodecsFacade.kt:234`, `:383`). Yet JVM uses ConcurrentHashMap and background preload (`JVM file :34`, `:328`), KMP HashMap and synchronous preload (`KMP file :30`, `:320`); Class/KClass and lazy initialization also differ (`baboon-compiler/src/main/resources/baboon-runtime/kotlin/BaboonRuntimeShared.kt:53`; `baboon-compiler/src/main/resources/baboon-runtime/kotlin-kmp/BaboonRuntimeShared.kt:53`).

**Recommendation:** separate registration/storage, pure version-choice policy, conversion traversal, and transport adaptation behind existing facade APIs. Share pure policy or conformance vectors first; do not merge whole facades through textual substitutions or change concurrency semantics incidentally. Benefit is fewer independent implementations of strict/tolerant envelope selection. Verify v1/v2, min-compatible/readable bounds, unknown domains, deprecated decoder-only versions, and ADT conversions. **Effort L; risk H; confidence high.**

## Sequence and constraints

Start KT-01, then KT-02 and KT-05; follow with KT-04 and KT-06. KT-03/KT-08 need explicit mode contracts; KT-07 needs boundary characterization before consolidation. Preserve platform public types, standalone runtime packaging, JSON-off modes, generated naming, schema evolution, and embedded-resource clean-build requirements. No Java/Scala sharing claim is made: this review did not inspect their implementations. No unused public API is declared removable.

Actual checks: HEAD verification, complete file inventory/line counts, full source coverage, pairwise runtime diffs, and identical-pair SHA-256 checks. Proposed verification above was not run. Main uncertainties are performance magnitude, native/JS execution behavior, and untested runtime paths. One bounded cleanup is statically established: `baboon-compiler/src/main/scala/io/septimalmind/baboon/translator/kotlin/KtConversionTranslator.scala:334` has a literal `if (false)` failure branch; `TranslationIssue` is referenced only by that branch and its import. Removing that branch/import is separate from the architectural work (S/L/high; verify cross-compilation).

## Coverage appendix

Inventory: generator 16 files; JVM runtime 14; KMP runtime 15; 10,837 source lines total (inventory, not complexity or savings).

- **Generator full reads:** KtBaboonTranslator, KtJsonCodecGenerator, KtMcpServerGenerator, KtValue, KtTreeTools, KtFileTools, KtCodecTranslator, KtDomainTreeTools, KtTypes, KtTypeTranslator, KtCodecTestsTranslator, KtCodecFixtureTranslator, KtDefnTranslator, KtConversionTranslator, KtUEBACodecGenerator, KtServiceWiringTranslator (all `.scala`). Initially truncated combined reads were completed with bounded slices.
- **JVM runtime full reads:** BaboonAnyOpaque, BaboonByteString, BaboonCodecs, BaboonRuntimeShared, BaboonServiceWiring, BaboonEither, BaboonExceptions, BaboonTimeFormats, BaboonTools, BaboonFixtureShared, BaboonIdentifierRepr, BaboonCodecsFacade, BaboonMcpRuntime (all `.kt`). BaboonConversions reconstructed from full KMP read plus complete diff.
- **KMP runtime full direct reads:** BaboonTools, BaboonConversions, BaboonDecimal, BaboonOffsetDateTime, BaboonTimeFormats, BaboonCodecsFacade. **Full counterpart-plus-diff coverage:** BaboonAnyOpaque, BaboonByteString, BaboonCodecs, BaboonRuntimeShared, BaboonFixtureShared, BaboonIdentifierRepr. **Identical counterpart plus hash coverage:** BaboonEither, BaboonExceptions, BaboonServiceWiring. No assigned source file remains sampled-only.
- Supporting material: CLAUDE.md and BaboonRuntimeResources.scala fully read; runtime-test filenames inventoried only. No claim that those tests establish the proposed changes' correctness.
