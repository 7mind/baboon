# Dart backend and embedded runtime audit

Baseline: `69bbb4c48a519d9a4148f370b76bde19af4bac6a`. All 16 translator files and seven embedded runtime files fully read: 9,287 lines. No repository edits, builds, installations, tests or benchmarks. Recommendations distinguish maintained source, emitted code and execution; no runtime defect is classified as confirmed.

Citation prefixes expand to repository-relative paths: **G** = `baboon-compiler/src/main/scala/io/septimalmind/baboon/translator/dart/`; **R** = `baboon-compiler/src/main/resources/baboon-runtime/dart/`.

## Ranked findings

### DT-01 — Write primitives directly into the growable binary buffer

Observed: R`baboon_runtime.dart:243` through `:312` allocate a new `ByteData` for each signed/integer/floating primitive write, obtain its byte view and copy it into `_buf`. The reader already holds a single view (`:425`). `toBytes` (`:417`), `readBytes` (`:515`) and `readNBytes` (`:526`) compose `sublist` with `Uint8List.fromList`, introducing intermediate copied containers. Indexed DTO emission calls `buffer.toBytes` before copying again (G`DtUEBACodecGenerator.scala:292`).

Proposal: maintain a writer `ByteData` view refreshed whenever capacity growth replaces `_buf`; set values at `_pos`. Use one owned-copy extraction for public byte-returning methods and an internal writer-to-writer append operation for indexed payloads. Preserve ownership: replacing a returned copy with a mutable backing view would change behavior. Benefit: fewer source-visible allocations/copies; throughput impact unmeasured. Effort M; risk medium; confidence high. Verify every primitive, capacity boundaries, nonzero-offset readers, compact/indexed bytes, post-extraction mutation isolation, and native/web numeric behavior before benchmarking.

### DT-02 — Avoid per-call service handler maps and repeated JSON parsing

Observed: active no-errors JSON and UEBA dispatch construct a map containing a closure for every endpoint on every invocation (G`DtServiceWiringTranslator.scala:456`, `:504`). Client JSON response generation passes `jsonDecode(resp)` as an expression (`:297`) to scalar decoding (`:97`), whose i64/u64/decimal branches refer to that expression more than once. Consequently those generated branches evaluate JSON parsing both for the type test and selected conversion.

Proposal: generate a direct method-name switch and bind decoded client JSON to one local. Introduce a small endpoint plan for codec availability, escaped names, argument/result types and context passing, shared by client/server/wrappers. Benefit: avoids handler-table/closure construction and repeated parsing without changing protocol payloads; generated and maintained complexity can both decrease. Effort M; risk medium; confidence high. Verify sync and Future server modes, always-Future clients, void results, all context modes and unknown methods; benchmark wide services separately. Preserve exception timing/classification and do not turn synchronous APIs asynchronous.

### DT-03 — Remove the disconnected private errors-mode renderer island

Observed: G`DtServiceWiringTranslator.scala:228` explicitly returns no wiring in errors mode. Private `generateErrorsWiring` at `:515` has no call site in tracked Scala sources (`git grep`); its JSON/UEBA renderers at `:697` and `:797` and return-shape helpers belong to that disconnected path. Private `renderConcreteContainer` (`:197`) also has only its declaration in the tracked Dart generator. Comments explicitly retain these as pending future work, not currently emitted functionality.

Proposal: remove the unreachable private island after recording its intended future requirements, rather than continuing to refactor it alongside active wiring. Keep shared helpers and public runtime/result types that still have consumers. Alternatively, reinstatement is a separate feature with explicit acceptance criteria—not a cleanup shortcut. Benefit: less maintained inactive policy, no generated-volume/runtime change. Effort S–M; risk low for proven private reachability, high if mixed with feature enablement; confidence high. Verify reference closure, compiler cross-build and unchanged output across current modes, including the deliberate omission in errors mode.

### DT-04 — Consolidate scalar and foreign wire decisions before rendering

Observed: four scalar tables in G`DtServiceWiringTranslator.scala:97`, `:118`, `:139`, `:165` repeat DTO JSON and UEBA mappings (G`DtJsonCodecGenerator.scala:255`, `:315`; G`DtUEBACodecGenerator.scala:400`, `:476`). UEBA foreign eligibility is separately decided in `translate` (`:42`), recursive encode/decode (`:430`, `:505`) and `isActive` (`:760`). Their decisions distinguish BaboonRef, string-like custom, runtime-mapped custom and throwing-stub custom foreigns.

Proposal: Dart-local scalar emitters plus a named resolved foreign-wire plan consumed by eligibility, registration and encode/decode. Keep JSON map-key hooks distinct from value-position passthrough. Benefit: one maintained policy per case, not necessarily smaller generated output. Effort M; risk medium; confidence high. Verify aliases, custom foreigns with/without runtime mappings, key hooks and direct service scalars. i64/u64 JSON strings and signed-carrier conversions must remain explicit; do not substitute other backends' integer policy or infer web precision guarantees from native `int` behavior.

### DT-05 — Move repeated any-field helpers behind a typed runtime boundary

Observed: G`DtJsonCodecGenerator.scala:503` and G`DtUEBACodecGenerator.scala:634` emit schema-independent envelope framing/checks per any-bearing codec. Schema facts are already parameters. Both downcast `ctx.facade` from an empty marker (R`baboon_runtime.dart:27`) to concrete `BaboonCodecsFacade`; static fallback decisions repeat at JSON `:476` and UEBA `:602`.

Proposal: put JSON and binary envelope helper implementations in a dedicated runtime library, leaving kind/static-fallback arguments at generated sites. The existing generated `encodeAnyField`/`decodeAnyField` methods are public (`DtJsonCodecGenerator.scala:504`, `:554`; `DtUEBACodecGenerator.scala:635`, `:692`): retain forwarding methods with the same signatures, or classify their removal as an explicit API change. Define the narrow conversion capability it requires, or initially retain the existing checked cast inside that library to avoid public interface changes. Keep existing marker/subclass compatibility rather than adding abstract methods to every external subclass. Benefit: smaller emitted source per type and a single maintained framing implementation. Effort M; risk medium; confidence high. Verify six kind shapes, nested collections, native/cross-format conversion, missing facade, future metadata tails and malformed framing; preserve nullable JSON content, thrown versus Either errors, package imports and resource embedding limits.

### DT-06 — Unify import planning without flattening Dart library semantics

Observed: G`DtBaboonTranslator.scala:339` and `:344` compute identical file keys; `resolveFqImport` (`:369`) and `resolveImport` (`:416`) repeat core/runtime package routing but intentionally differ in aliasing and version-to-filesystem conversion. Types and branch codecs can share a physical file through `importAs` (G`DtTypeTranslator.scala:118`; G`DtJsonCodecGenerator.scala:590`). MCP independently implements filename snake case at G`DtMcpServerGenerator.scala:141` instead of G`DtTypeTranslator.scala:146`.

Proposal: a typed resolved library reference containing URI, physical-file key and alias policy, used by both import paths; share a naming utility only after characterizing current acronym behavior. Do not mechanically equate logical packages and directories or rename emitted files. Benefit: less repeated packaging policy and clearer collision handling. Effort M; risk medium; confidence high. Verify nested namespaces, old versions, same-name imports, ADT branch files, foreign bindings and acronym-containing service names with generated analyzer/compile checks. This is Dart-specific dependency planning, not a universal printer.

### DT-07 — Build decoded collections directly; remove tiny hashing containers

Observed: G`DtUEBACodecGenerator.scala:464` materializes a list of `MapEntry` objects before constructing a map; `:468` materializes a list before a set. Runtime deep map hashing creates a two-element list per entry (R`baboon_runtime.dart:1627`); fixed-pair hashes do likewise (`:780`, `:879`). Generated DTO hashing constructs a field-hash list at G`DtDefnTranslator.scala:406`.

Proposal: direct typed map/set population or collection-for emission and fixed-arity hashing where its semantics are compatible. Keep the general iterable path for wide DTOs. Avoid hash caching: final fields can contain mutable collections, and opaque bytes remain mutable (R`baboon_any_opaque.dart:100`). Benefit: fewer temporary containers; measured value unknown. Effort S–M; risk medium; confidence high. Verify duplicate-key last-write behavior, set order, equality/hash consistency, nested collections, and negative/truncated counts; benchmark collection-heavy cases. Do not silently change public hash behavior without reviewing its compatibility expectations.

Direct population interleaves decoding with hashing/equality, whereas current list staging completes decoding first. Verify foreign hash/equality side effects or exceptions, malformed later elements and reader cursor state; restrict the change to proven-equivalent key/value domains where necessary. Treat hash-algorithm changes as separate from container-allocation removal.

### DT-08 — Share synchronous MCP protocol handling, not transport contracts

Observed: R`baboon_mcp_runtime.dart:148` and `:327` duplicate initialize/list/call/error mapping. `_byName` (`:141`) rebuilds the tool map each request. Generated `_tools` is `late final` but returns a mutable list (G`DtMcpServerGenerator.scala:125`), so caching indefinitely is not automatically equivalent. MCP serializes parsed arguments (`:199`) before string-based service wiring parses them (G`DtServiceWiringTranslator.scala:438`).

Proposal: private shared protocol dispatcher with separate registry/routing adapters. Cache lookups only with an explicit immutability/invalidation contract. A parsed-JSON dispatch seam could remove the in-process stringify/parse cycle while preserving existing string entrypoints, but should be measured and treated as a separate API-design step. Benefit: one maintained protocol state machine; potential allocation reduction. Effort M; risk medium; confidence high. Verify ordering, collisions, notifications, IDs, Channel-A/B errors and mutable registry behavior. Preserve synchronous MCP despite optional Future service generation; do not claim async MCP support.

## Sequence, verification and exclusions

Start with DT-03's reference-proven private cleanup and DT-02's local parse binding; then DT-01/04/05, followed independently by packaging, collections and MCP work. Runtime-file splitting is acceptable for ownership/embedding, but retain public package imports and exports. Do not delete decoder-only codec classes, foreign extension hooks or marker interfaces merely because they are small.

Actual checks: full reads, targeted line searches, tracked `git grep` reachability checks and clean `git status --short`; Dart executable discovered but not run. No defect reproduction or performance result claimed. Proposed tests follow constructive-test-taxonomy: behavioral black-box public-interface checks plus generated compilation/wire compatibility; benchmarks separately. Existing generated cross-language JSON tests silently return on parse failures (G`DtCodecTestsTranslator.scala:228`), and UEBA tests catch all failures (`:282`); those tests alone cannot certify wire preservation. Use settled fixtures and fail-visible acceptance checks. Native/web support and numeric precision require explicit target-specific verification, not reliance on runtime comments.

## Full-read coverage appendix

G (16, all `.scala`): DtBaboonTranslator 609; DtTypes 140; DtConversionTranslator 423; DtServiceWiringTranslator 898; DtCodecFixtureTranslator 223; DtCodecTranslator 17; DtJsonCodecGenerator 619; DtMcpServerGenerator 201; DtDomainTreeTools 130; DtDefnTranslator 935; DtUEBACodecGenerator 787; DtTypeTranslator 231; DtTreeTools 57; DtValue 31; DtFileTools 26; DtCodecTestsTranslator 333.

R (7, all `.dart`): baboon_codecs_facade 694; baboon_identifier_repr 337; baboon_any_opaque 284; baboon_mcp_runtime 419; baboon_runtime 1637; baboon_fixture 129; cross_language_fixture_path 127. No assigned file sampled; supporting test suites were not exhaustively audited. CLAUDE.md and review brief read. Parent covers common docs/classifier/unreachable-conversion-branch cleanup.
