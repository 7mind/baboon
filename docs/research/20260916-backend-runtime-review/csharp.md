# C# backend and runtime audit

Revision inspected: `69bbb4c48a519d9a4148f370b76bde19af4bac6a`. Entire assigned generator and runtime read (32 files, 11,953 lines, 517,014 bytes); supporting tests were searched/sampled, not exhaustively reviewed. Source-level costs below are observations; wall-clock, allocation totals, and output-size savings were not benchmarked. No repository changes, builds, installs, or test-suite executions occurred.

## Findings

### CS-01 — Centralize C# scalar wire emission

The service generator repeats JSON scalar encode/decode and UEBA read/write tables already maintained by the DTO codecs. Sources: `baboon-compiler/src/main/scala/io/septimalmind/baboon/translator/csharp/CSServiceWiringTranslator.scala:80`, `:106`, `:120`, `:145`; `baboon-compiler/src/main/scala/io/septimalmind/baboon/translator/csharp/CSJsonCodecGenerator.scala:330`, `:388`; `baboon-compiler/src/main/scala/io/septimalmind/baboon/translator/csharp/CSUEBACodecGenerator.scala:574`, `:641`.

There is observable divergence: service `bit` encoding constructs a JValue from lowercase **string** (`CSServiceWiringTranslator.scala:114`), whereas DTO scalar encoding supplies the **boolean** directly (`CSJsonCodecGenerator.scala:345`). Whether supported service inputs expose an interoperability defect remains an unreproduced hypothesis, not a verified defect.

Extract a C#-local scalar emitter taking explicit codec context, reader/writer and value expressions. Keep map-key emission separate: keys have intentional textual rules. First characterize existing service scalar outputs; do not silently change boolean wire behavior during extraction. Benefit: one maintenance point per primitive wire rule; generated volume need not change. Effort M, risk M, confidence high. Verify all scalar service inputs/outputs against DTO scalar fields, especially bit, decimal, UUID, timestamps and bytes; run service acceptance and JSON/UEBA golden checks.

### CS-02 — Give service dispatch a typed method plan

The four error-mode emitters independently construct decode, invocation/error mapping, output/void handling, and encode phases: `baboon-compiler/src/main/scala/io/septimalmind/baboon/translator/csharp/CSServiceWiringTranslator.scala:676`, `:810`, `:918`, `:1051`. Service result/context resolution is also performed by declarations at `baboon-compiler/src/main/scala/io/septimalmind/baboon/translator/csharp/CSDefnTranslator.scala:502`, separately from wiring at `CSServiceWiringTranslator.scala:28`.

Build one typed C# method plan containing resolved input/output/error types, context layout, async mode, and invocation shape. Let JSON/UEBA supply their own wire operations and keep synchronous/async control flow explicit: abstract service-result containers do not supply async bind. Extract common call/error fragments before considering larger restructuring. Benefit: fewer independent decisions when adding a service axis; generated code and public signatures remain comparable. Effort M–L, risk H, confidence high. Verify the product of sync/async, errors/no-errors, output/void, domain error/no domain error, and context modes, including custom result containers. Do not use a universal backend renderer.

### CS-03 — Move schema-independent any framing into the runtime

Every any-bearing generated codec receives an entire helper body: JSON at `baboon-compiler/src/main/scala/io/septimalmind/baboon/translator/csharp/CSJsonCodecGenerator.scala:532`, UEBA at `baboon-compiler/src/main/scala/io/septimalmind/baboon/translator/csharp/CSUEBACodecGenerator.scala:423`. The helpers already receive expected kind and static domain/version/type fallbacks as arguments; their framing logic does not depend on the containing DTO. The generator additionally duplicates `hasAnyField` (`CSJsonCodecGenerator.scala:604`, `CSUEBACodecGenerator.scala:518`) and static-fallback planning (`CSJsonCodecGenerator.scala:642`, `CSUEBACodecGenerator.scala:557`).

Put JSON and UEBA envelope helpers in a dedicated runtime resource beside `baboon-compiler/src/main/resources/baboon-runtime/cs/AnyOpaque.cs:147`; generate only calls. Share a typed any-field plan inside the generator. This directly reduces generated repetition and relocates protocol maintenance to one runtime copy; no speedup is established. Keep JSON and binary implementations distinct. Effort M, risk M, confidence high. Verify six any kinds, nested containers, same/cross-format conversion, metadata extension skipping, malformed lengths, and exact envelope bytes. Clean-rebuild resource embedding; keep files comfortably below JVM constant limits.

### CS-04 — Compute upgrade decisions once per translation

`baboon-compiler/src/main/scala/io/septimalmind/baboon/translator/csharp/CSTypeInfo.scala:105` computes each member's possible upgrades again for every candidate higher version. At `:84`, the owner's upgrade list is recomputed inside another candidate filter. Rendering asks upgrade eligibility both during import discovery and for each rendered symbol (`baboon-compiler/src/main/scala/io/septimalmind/baboon/translator/csharp/CSBaboonTranslator.scala:63`, `:145`), through `baboon-compiler/src/main/scala/io/septimalmind/baboon/translator/csharp/CSTypeTranslator.scala:12`.

First hoist candidate-independent calculations outside these filters; then consider an immutable per-lineage upgrade plan keyed by type, version, and owner-check mode. Preserve derived-origin handling for foreign codecs. Benefit: removes repeated graph traversal in compiler work; output is unchanged. Effort M, risk M, confidence high on repetition, unmeasured on impact. Verify byte-identical output with deduplication on/off across nested ADTs, foreign mappings, and long version histories; measure traversal counts and compile time before adopting broader caching.

### CS-05 — Remove sorting before commutative hash accumulation

`baboon-compiler/src/main/resources/baboon-runtime/cs/BaboonTools.cs:70` and `:75` sort projected hashes immediately before XOR reduction. Sorting cannot change XOR, including duplicates and negative integers. Generated records call these helpers from `baboon-compiler/src/main/scala/io/septimalmind/baboon/translator/csharp/CSDefnTranslator.scala:673`, `:682`.

Remove the ordering step while preserving the seed, element hash functions and public signatures. This removes an unnecessary sorting operation and its buffering; it does not establish an application-level speedup. Do not change the sequence-hash algorithm in the same change. Effort S, risk L, confidence high. An in-memory JavaScript check compared sorted/unsorted XOR for 4,096 signed triples and found equality; this was an algebra sanity check, not C# execution. Proposed verification: C# hash parity over empty/singleton/permuted/duplicate-hash collections, followed by equality/hash contract checks and allocation measurements.

### CS-06 — Avoid materializing discarded UEBA index entries

`baboon-compiler/src/main/resources/baboon-runtime/cs/BaboonCodecs.cs:194` allocates a list even for compact DTOs and one `BaboonIndexEntry` object per indexed variable field. The generated decoder only examines `Count` in a debug assertion before decoding fields sequentially (`baboon-compiler/src/main/scala/io/septimalmind/baboon/translator/csharp/CSUEBACodecGenerator.scala:360`). The public materializing API may have external callers; it is not dead code.

Add an internal consume-index path that reads the same header/pairs and performs the same checks without storing entries; generated DTO decoders use it, while public `ReadIndex` remains available. Benefit: removes observed per-DTO/index-entry allocations from this generated path. Effort M, risk M, confidence high. Verify compact/indexed bytes consumed, nonseekable readers, malformed/truncated indices, wrapped branches and forward-readable envelopes. Preserve current validation separately from any hardening proposal.

### CS-07 — Factor MCP protocol helpers and registry ownership

Four copies implement the same protocol switch: `baboon-compiler/src/main/resources/baboon-runtime/cs/BaboonMcpRuntime.cs:191`, `:374`, `:517`, `:648`. Sync/async per-service calls rebuild the complete tool dictionary per request (`:181`, `:248`, `:638`, `:705`), while muxers already construct tables during registration (`:359`, `:502`). The private `_servers` fields are only declared and appended (`:341`, `:371`, `:490`, `:514`); the scoped reference search found no readers.

Extract common initialization, validation, tools-list and Channel-A/B response helpers, with separate sync/async invocation. Use an explicit immutable tool registry for generated servers, or preserve dynamic `Tools` semantics for external subclasses before caching. Remove write-only server lists only after checking whether retaining zero-tool servers has an intended lifetime effect. Benefit: one protocol policy implementation and avoids per-request registry rebuilding. Effort M, risk M, confidence high. Verify protocol response equality, order, duplicate registration, sessions, unknown tools, errors and async dispatch. Preserve virtual error descriptions and routable interfaces.

### CS-08 — Parse registered versions once inside facade bookkeeping

`baboon-compiler/src/main/resources/baboon-runtime/cs/BaboonTypeMeta.cs:64` parses on every `Version` access; parsing splits strings and creates a version record (`:34`). The facade repeatedly invokes it during sorting (`baboon-compiler/src/main/resources/baboon-runtime/cs/BaboonCodecsFacade.cs:1162`), min/max lookup (`:1043`), conversion walking (`:853`), and compatibility search (`:1134`).

Start by hoisting repeated local accesses, then store parsed versions in private registration entries while retaining public positional-record equality and `with` semantics. Keep original strings for wire identifiers and diagnostics. Benefit: avoids repeated parsing of stable registered versions; workload significance is unknown. Effort S–M, risk M, confidence high. Verify ordering, registration overrides, malformed-version behavior, compatible codec selection and intermediate conversion targets; measure multi-version workloads. Avoid an unbounded global string cache.

## Sequence and constraints

Start CS-05, then scalar characterization/CS-01, CS-03 and CS-06, then CS-04/CS-08 after profiling, followed by CS-02 and CS-07. Generator-only extraction should preserve generated snapshots; runtime relocation should preserve API and wire behavior even though generated text changes. Run focused C# lanes first, then project-required `mdl :build :test` before committing; resource edits require `sbt clean` before compilation. These checks are proposed, not executed.

Intentional duplication: JSON vs UEBA framing; typed vs erased codec adapters; Task vs synchronous public APIs; identifier lowercase-hex/strict timestamp representation versus permissive ByteString parsing and ordinary timestamp codecs. `BaboonSingleton` replacement semantics explain generated delegation guards; do not delete them as redundant. `netstandard2.1` is pinned in `test/cs-stub/BaboonDefinitions/BaboonDefinitions.csproj:4`, so newer framework conveniences require compatibility checks. No public runtime API is declared unused merely because repository callers are absent.

## Coverage appendix

Full reads, generator (all `.scala` under `baboon-compiler/src/main/scala/io/septimalmind/baboon/translator/csharp/`): CSBaboonTranslator, CSCodecFixtureTranslator, CSCodecTestsTranslator, CSCodecTranslator, CSConversionTranslator, CSDefnTranslator, CSDomainTreeTools, CSFileTools, CSJsonCodecGenerator, CSServiceWiringTranslator, CSTreeTools, CSTypeInfo, CSTypeTranslator, CSTypes, CSUEBACodecGenerator, CSValue, CsMcpServerGenerator.

Full reads, runtime (all `.cs` under `baboon-compiler/src/main/resources/baboon-runtime/cs/`): AnyOpaque, BaboonByteString, BaboonCodecs, BaboonCodecsFacade, BaboonConversions, BaboonExceptions, BaboonExt, BaboonFixtureShared, BaboonIdentifierRepr, BaboonMcpRuntime, BaboonRuntimeShared, BaboonServiceWiring, BaboonTime, BaboonTools, BaboonTypeMeta. Truncated multi-file outputs were completed with smaller reads.

Supporting coverage: full CLAUDE.md; sampled/search-only test files AnyRoundTripTests, AnyMetaCodecTests, BinEnvelopeGoldenTests, FacadeLatestMethodsTests, WiringTests, DomainFacadeTests, MCP overlay McpTests; project target/dependency lines inspected. No generated tree or other backend was fully inspected. Exact checks executed: `git rev-parse HEAD`; `git status --short` (clean); `rg --files`; `wc -lc`; `cat`/`sed` source reads; focused `rg -n` references/tests; 4,096-case in-memory XOR equivalence check. No correctness hypothesis was patched or reported as reproduced.
