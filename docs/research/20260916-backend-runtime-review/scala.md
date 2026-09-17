# Scala backend and embedded runtime audit

Baseline: `69bbb4c48a519d9a4148f370b76bde19af4bac6a`. Read-only source review; all 16 assigned generator files and all 12 embedded Scala runtime files were fully read (8,565 lines total). No build, installation, benchmark, or behavioral reproduction was run. Findings below are evidenced refactoring/optimization opportunities, not confirmed defects or measured speedups.

Citation prefixes: **G** = `baboon-compiler/src/main/scala/io/septimalmind/baboon/translator/scl/`; **R** = `baboon-compiler/src/main/resources/baboon-runtime/scala/`; **M** = `baboon-compiler/src/main/scala/baboon/runtime/shared/`. Prefixes expand to repository-relative paths.

## Ranked findings

### SC-01 — Give identifier runtime helpers one maintained source

Observed: R`BaboonRuntimeShared.scala:58` documents a byte-equality obligation with the compile-side mirror; the two `IdentifierRepr` implementations begin at R`BaboonRuntimeShared.scala:68` and M`BaboonRuntimeShared.scala:1019`. Both implementations were read, including their cursor, timestamp, escaping, hexadecimal and numeric helpers. This is intentional sharing by copying, not accidental similarity. The containing files are different runtime surfaces and should not be merged wholesale.

Proposal: extract only this shared helper into a source/resource generation input, or first add an exact-object parity check if build-source sharing is inconvenient. Preserve the resource's escaping pipeline (G`ScBaboonTranslator.scala:325`) and embedded-source size constraints. Benefit: eliminate a manually maintained semantic mirror or make drift immediately observable; no generated-volume reduction promised. Effort M; risk medium; confidence high. Verify compile-side and emitted implementations against the same identifier vectors, then compile and execute actual generated identifiers. Mirror/property tests alone cannot establish generator correctness.

### SC-02 — Share Scala scalar codec emission between DTO and RPC generators

Observed: G`ScServiceWiringTranslator.scala:85`, `:112`, `:138`, `:163` contain four scalar dispatch tables corresponding to G`ScJsonCodecGenerator.scala:349`, `:526` and G`ScUEBACodecGenerator.scala:466`, `:546`. They repeat signed carriers for unsigned values, timestamp helpers, bytes and decimal behavior. The RPC layer adds distinct exception/result handling around those operations.

Proposal: a Scala-local scalar-expression emitter parameterized by value, reader/writer, and codec-context references; leave collection recursion, key codecs, foreign resolution, and RPC error classification at their current boundaries. Benefit is one maintained scalar policy, not fewer emitted calls. Effort M; risk medium; confidence high. Verify every scalar as DTO field and direct service input/output, especially unsigned boundaries, timestamps, decimals and malformed values; compare generated compilation and wire output. Do not reuse Kotlin/Jackson emission merely because both run on the JVM.

### SC-03 — Separate service planning from wire/result rendering

Observed: G`ScServiceWiringTranslator.scala:741` and `:844` repeat decode → invoke → encode structure, including output/no-output and declared-error branching. Client generation at `:280` repeats the same endpoint facts. Service trait generation independently resolves context/result configuration at G`ScDefnTranslator.scala:433`; wiring does so at G`ScServiceWiringTranslator.scala:27`.

Proposal: an immutable Scala service/endpoint plan carrying resolved context names, type parameters, input/output/error types, codec availability and result shape. Keep JSON/UEBA operations and result lifting as explicit small renderers; do not build a universal statement printer. Benefit: fewer inconsistent decisions across trait/client/wiring and smaller responsibility boundaries. Effort M–L; risk high; confidence high on duplication, medium on optimal split. Verify all context modes, concrete and higher-kinded result types, errors/no-errors, void outputs and optional codecs, including exact error-channel behavior. MCP must retain its deliberate Either-only validation at G`ScMcpServerGenerator.scala:61`.

### SC-04 — Move schema-independent any-envelope operations into runtime

Observed: G`ScJsonCodecGenerator.scala:67` and G`ScUEBACodecGenerator.scala:79` emit complete envelope helpers once per participating codec object. Their schema-specific inputs are already parameters: expected kind and static domain/version/type identifier. Static-fallback computation is duplicated at JSON `:425` and UEBA `:638`; runtime already owns opaque metadata and cross-format conversion (R`BaboonAnyOpaque.scala:1`, R`BaboonCodecsFacade.scala:259`).

Proposal: runtime JSON and UEBA envelope helper methods, plus one generator-side fallback descriptor. Keep the two formats distinct and preserve missing-facade errors, metadata-extension skipping and exception/Either behavior. Benefit: reduced repeated generated source per any-bearing type and one maintained framing implementation; executable/bytecode-size and execution improvements are unmeasured. Effort M; risk medium; confidence high. Verify all six kind shapes, nested constructors, native/cross-format branches, future metadata tails and malformed lengths. Rebuild embedded resources cleanly and check the JVM constant-size ceiling.

### SC-05 — Hoist immutable generated metadata and singleton wrappers

Observed: JSON G`ScJsonCodecGenerator.scala:175` and UEBA G`ScUEBACodecGenerator.scala:240` emit `def LazyInstance = Lazy(codecObject)`. R`BaboonRuntimeShared.scala:278` shows each `Lazy` owns a closure and atomic reference; generated encode/decode guards also request it. G`ScDomainTreeTools.scala:80`, `:89`, `:97` emit collection-building metadata methods, installed on companions at G`ScDefnTranslator.scala:318` and `:401`. Envelope encoding consumes these metadata methods at R`BaboonRuntimeShared.scala:421` and `:444`.

Proposal: private stable/lazy backing values behind the existing methods, with initialization order characterized first. Do not replace the public singleton contract, alter registration/override behavior, or eagerly resolve recursive codec graphs. Benefit: removes source-visible repeated wrapper and immutable-collection construction; actual allocation/throughput impact remains unmeasured. Effort S–M; risk medium; confidence high. Verify recursive types, codec registration and deprecated-encoder forwarding; benchmark allocation separately from behavioral tests.

### SC-06 — Remove intermediate binary buffers/collections where ownership permits

Observed: indexed encoding copies its complete temporary buffer via `toByteArray` at G`ScUEBACodecGenerator.scala:360`; collection decoding materializes a mapped range before `toList`/`toSet`/`toMap` at `:515`. Runtime index reading constructs full entries at R`BaboonCodecs.scala:224`, while generated DTO decoding uses only their count at G`ScUEBACodecGenerator.scala:405`. R`BaboonTools.scala:161` reads a fresh byte array and passes it to the copying public factory R`BaboonByteString.scala:169`.

Proposal: direct buffer draining when supported by the existing writer, destination builders, a separate validating count/skip index path for generated DTOs, and an internal owned-array constructor. Keep public copying factories and full `readIndex` API unchanged. Benefit: fewer intermediate objects/copies; workload importance unknown. Effort M; risk medium; confidence high. Verify compact/indexed exact bytes, index validation, truncated input, duplicates and iteration order; benchmark large collections separately. Do not remove index framing or its validation to save allocations.

Split buffer transfer, collection builders, index consumption and owned-array construction into separate changes. Direct collection population interleaves decoding with hashing/equality instead of staging decoded values; verify foreign hooks, malformed later elements and cursor state, or restrict it to domains with established equivalence.

### SC-07 — Share MCP protocol handling, retain separate routing ownership

Observed: R`BaboonMcpRuntime.scala:144` and `:301` implement parallel initialize/list/call state machines for a server and muxer. Server lookup rebuilds `tools.map(...).toMap` at `:141`; emitted servers expose immutable tool sequences at G`ScMcpServerGenerator.scala:175`.

Proposal: a package-private synchronous protocol dispatcher receiving tool-list and invocation operations; retain muxer registration/order/collision semantics. Cache a lookup only for immutable generated registries or under an explicit snapshot/invalidation contract—arbitrary implementations may expose changing `tools`. Benefit: one maintained protocol/error mapping and potentially fewer per-call registry allocations. Effort M; risk medium; confidence high. Verify notifications, IDs, initialization, unknown tools, Channel-A/Channel-B errors and muxer ordering through public `handle`; preserve the Either-only axis rather than silently introducing async effects.

### SC-08 — Replace repeated import-collision scans with a name index

Observed: G`ScBaboonTranslator.scala:260` and `:263` count matches over all distinct used types for each type. This produces quadratic name-comparison work in the number of referenced types; it is unrelated to generated LoC. Collision identity includes both name and `inObject`.

Proposal: precompute counts by `(name, inObject)` once and retain existing package partitioning, sorting, root qualification and import formatting. Benefit: bounded compiler algorithm improvement without a printer redesign; significance requires measurement on wide schemas. Effort S; risk low; confidence high. Verify same-name cross-package/object references, nested packages, forced FQNs and byte-identical output, then benchmark compiler rendering separately.

## Sequence and boundaries

Start with SC-08 and a SC-01 parity check; then SC-02, SC-04 and SC-05; tackle SC-03/06/07 independently with characterization tests. Keep Circe/JVM runtime APIs, Scala unsigned signed-carrier representation, ADT static-type metadata, override/registration semantics, foreign-key hooks, evolution bounds and resource preprocessing distinct. The existing weakest compiler effect constraints (`Applicative2`/`Error2`) need no expansion for these refactors. Compiler changes must still cross-compile JVM/JS; generated Scala compatibility is a separate check.

Actual checks: complete assigned source reads, targeted symbol/reference searches, and supporting mirror-object read. No tests were executed and no performance estimates measured. Proposed verification uses behavioral black-box checks plus generated compilation/wire fixtures, following constructive-test-taxonomy; benchmarks are a separate evidence category. Generated tests currently use native-format any fixtures (G`ScCodecTestsTranslator.scala:57`) and skip recursive/foreign definitions (`:36`), so those tests alone are not sufficient acceptance coverage for these proposals.

## Coverage appendix

Full reads, generator (16): ScBaboonTranslator 428; ScTypeTranslator 133; ScFileTools 26; ScTreeTools 74; ScJsonCodecGenerator 625; ScConversionTranslator 352; ScMcpServerGenerator 204; ScCodecFixtureTranslator 259; ScDomainTreeTools 104; ScCodecTranslator 17; ScValue 32; ScUEBACodecGenerator 684; ScServiceWiringTranslator 951; ScCodecTestsTranslator 202; ScTypes 224; ScDefnTranslator 889. All filenames end `.scala` under G.

Full reads, embedded runtime (12): BaboonConversions 141; BaboonTools 319; BaboonRuntimeShared 624; BaboonByteString 275; BaboonServiceWiring 141; CrossLanguageFixturePath 125; BaboonMcpRuntime 393; BaboonExceptions 20; BaboonCodecs 444; BaboonCodecsFacade 628; BaboonAnyOpaque 146; BaboonFixtureShared 105. All filenames end `.scala` under R.

Supporting sampled coverage only: M`BaboonRuntimeShared.scala:1000` through EOF (complete IdentifierRepr object, not the whole mirror file). CLAUDE.md and review brief read. No generated output census or full supporting test-suite audit claimed. Common docs/classifier/unreachable-branch cleanup is covered by the parent cross-backend audit rather than repeated here.
