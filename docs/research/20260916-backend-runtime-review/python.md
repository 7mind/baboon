# Python backend/runtime review

Baseline: `69bbb4c48a519d9a4148f370b76bde19af4bac6a`. Read-only review. Fully inspected all 17 generator and 10 runtime files: 8,819 newline-counted lines, 389,125 bytes. No sampled files within scope. Below, `G/` expands to `baboon-compiler/src/main/scala/io/septimalmind/baboon/translator/python/`; `R/` expands to `baboon-compiler/src/main/resources/baboon-runtime/python/`. These are repository-relative citation prefixes.

## Ranked opportunities

### PY-01 — Give native JSON values and serialized JSON text distinct contracts

Observed: generated codecs accept/return text (`G/PyJsonCodecGenerator.scala:49,55,128`), so nesting an ADT parses its encoded text back into a value (`:303`), and decoding serializes a nested value (`:391`). Conversely, `AnyOpaqueJson.json` documents native values (`R/baboon_any_opaque.py:97`), but facade `decode_any` passes that value directly to the text decoder (`R/baboon_codecs_facade.py:443`). `json_to_ueba_bytes` guesses representation from `isinstance(..., str)` (`:493`), explicitly recording the interface mismatch. Facade envelopes put encoded text directly under `$c` (`:192–198`). The resulting cross-language compatibility concern is an **unconfirmed hypothesis**, not a reproduced integration defect.

Proposal: add internal value-oriented encode/decode entry points and explicit text adapters, preserving public text APIs and foreign-codec extension contracts. Consume these from nested walkers, opaque payload conversion and facade envelopes. Decide envelope compatibility separately; do not silently change existing bytes during cleanup. Benefit: one representation boundary and removal of nested dumps/loads; speed benefit unmeasured. Effort L; risk high; confidence high in mismatch, medium in compatibility consequences. Verify native string/object/null payloads, foreign codecs, both ADT modes, old envelopes and cross-language fixtures; benchmark nested objects separately.

### PY-02 — Compute one Python field serialization/naming plan

Observed: definition generation aliases fields for contracts **or** keywords (`G/PyDefnTranslator.scala:544–556`), while explicit JSON exclusion names account only for keywords (`G/PyJsonCodecGenerator.scala:134–136`). Contract properties independently compute accessor names (`G/PyDefnTranslator.scala:562–574`); UEBA constructor keywords independently do so again (`G/PyUEBACodecGenerator.scala:227`). Any-presence traversal repeats in definition configuration (`G/PyDefnTranslator.scala:606`), JSON (`G/PyJsonCodecGenerator.scala:172`) and UEBA (`G/PyUEBACodecGenerator.scala:466`); JSON separately scans ADT and map-key characteristics (`:212,228,247`).

Proposal: a target-local typed field plan containing wire name, backing attribute, accessor, constructor keyword, alias requirements and explicit-walker policy; reuse it in definitions and both codecs. Derive traversal flags together without replacing Pydantic with a new serializer. Benefit: consistent naming/configuration and fewer repeated policy decisions, not necessarily smaller generated classes. Effort M; risk medium; confidence high. Contract/keyword intersections and walked contract fields are defect hypotheses pending generated-model execution. Verify these plus nested bytes/decimal/any/map-key combinations with pinned Pydantic.

### PY-03 — Unify collection traversal in evolution rendering

Observed: `swapCollType` renders collection traversal using `range(len(source))` (`G/PyConversionTranslator.scala:269–277`); `transferConstructor` traverses source elements (`:341–352`). Optional-to-set emits `{}` for absence (`:265`), whereas default set initialization uses the set constructor (`:188–189`). DTO operation dispatch invokes the swap helper at `:208,225`.

Proposal: reuse an element-mapping renderer and explicit empty-collection constructor across these two paths, retaining the typed `FieldOp` distinction. Benefit: consistent evolution semantics and less independent rendering logic. Effort M; risk high because migrations affect data; confidence high in source divergence. Equivalent emitted expressions produce `{0,1}` instead of `{10,20}` and an empty dict instead of set; **no model-to-generator reachability reproduction was run**. First reproduce a supported list→set migration and optional→set migration, then verify nested element conversions, empties and absent values before refactoring.

### PY-04 — Share MCP protocol decisions, not sync/async execution machinery

Observed: four handlers duplicate initialize/session gating, tool-name validation, errors and result envelopes: `R/baboon_mcp_runtime.py:130,297,480,609`. Sync and async muxers also duplicate registry construction (`:473,602`). Their necessary dispatch difference is ordinary call versus `await`; per-service error description remains overridable. Service-wiring muxers already return generic results unchanged (`R/baboon_service_wiring.py:143,169,223,250`).

Proposal: local pure request classification/response helpers plus a registry helper, keeping sync/async dispatch shells and public override hooks. Benefit: one protocol policy without hiding await boundaries. Effort M; risk medium; confidence high. Verify identical Channel-A/Channel-B responses, initialized notifications, unknown tools, registration order, duplicate registrations and async exceptions. Do not turn generic service muxers into another async framework.

### PY-05 — Build immutable MCP metadata once per generated server

Observed: generated `tools` is a property rebuilding entries and running `json.loads` (`G/PyMcpServerGenerator.scala:110,154`); each per-service lookup rebuilds the name dictionary (`R/baboon_mcp_runtime.py:127,294`). Muxers instead build routing at registration (`:473,602`).

Proposal: instance-owned immutable metadata and cached lookup, exposing defensive copies or an explicitly compatible read-only view. Schema dicts/lists are mutable today, so returning a shared mutable list would change behavior. Benefit: avoids repeated schema parsing/allocation on requests; no measured latency claim. Effort S/M; risk medium; confidence high. Verify external mutation cannot corrupt future requests, ordering and tool descriptions; benchmark repeated calls with many tools.

### PY-06 — Consume/validate UEBA indices without materializing discarded entries

Observed: `R/baboon_codecs.py:228–250` constructs a list of Pydantic `BaboonIndexEntry` objects while validating offsets/lengths. Generated decoders consume only its length (`G/PyUEBACodecGenerator.scala:231–234`).

Proposal: retain the public `read_index` API and introduce a count-only consume-and-validate path for generated sequential decoders. Share validation internally. Benefit: eliminates these per-field objects on that path, not index wire bytes. Effort S; risk medium; confidence high. Verify compact/indexed bytes, stream position, malformed indices, short reads and current assertion behavior including optimized Python. Benchmark allocations independently; changing assert-based validation is a separate behavior decision.

### PY-07 — Centralize scalar codec policy for DTO and RPC consumers

Observed: UEBA scalar tables repeat in service generation (`G/PyServiceWiringTranslator.scala:87,113`) and model codecs (`G/PyUEBACodecGenerator.scala:342,405`). RPC JSON treats every built-in as plain loads/dumps (`G/PyServiceWiringTranslator.scala:75–84`), unlike Pydantic-backed DTO serialization and explicit scalar conversion (`G/PyJsonCodecGenerator.scala:128,309`; `G/PyDefnTranslator.scala:591–600`).

Proposal: target-local typed scalar operations shared by RPC and DTO renderers, explicitly covering bytes, UUID, decimal and timestamps. Native scalar RPC compatibility is a hypothesis until exercised; do not equate JSON number/string formatting across contexts automatically. Benefit: fewer independently maintained mappings and explicit policy differences. Effort M; risk medium/high; confidence high in duplication. Verify direct scalar services against equivalent DTO fields for JSON/UEBA and malformed inputs, preserving existing error stages.

### PY-08 — Establish an executable generated/runtime contract boundary before facade cleanup

Observed API drift: generated metadata implements `unmodified_since` (`G/PyBaboonTranslator.scala:449`), runtime base requires `same_in_versions` (`R/baboon_runtime_shared.py:80–84`), and facade invokes it with a type id (`R/baboon_codecs_facade.py:384`). Facade latest-JSON calls `decode_from_json_string` (`:222`); tracked Python/Scala search finds only that occurrence. These are **unexecuted integration hypotheses**.

Proposal: define one explicit metadata protocol and a small generated fixture exercising the public facade; then reconcile callers, keeping externally consumed methods versioned as needed. Do not delete apparently obsolete runtime APIs on search evidence alone. Benefit: prevents abstraction cleanup from preserving inconsistent contracts. Effort M; risk high; confidence high in source mismatch, medium in reachable impact. Verify metadata construction, exact/compatible codec selection, version conversion and latest JSON decoding through generated artifacts, not permissive handwritten stubs.

## Correctness backlog and checks actually run

See `python-reproductions.md` for exact command and captured outputs. Python 3.12.12 lacks Pydantic; no installed runtime/model execution, builds, dependency installation or tests occurred. Dependency-free **extracted production method bodies** reproduced `write_str` failing at 128 ASCII bytes, while 127 succeeds (`R/baboon_runtime_shared.py:220,288–299`), and `Lazy.is_value_created` raising AttributeError (`:435–448`). The writer is used by generated string codecs (`G/PyUEBACodecGenerator.scala:361`) and metadata (`R/baboon_any_opaque.py:128`). Tracked Python/Scala search found only the lazy property's definition; public external use remains unknown. Fix these independently of performance refactors after full-runtime reproduction. No source modifications; final `git status --porcelain` was empty.

## Sequence and exclusions

First reproduce contract/representation and migration hypotheses; handle confirmed correctness separately. Then PY-02/PY-07, PY-06/PY-05, PY-04, and finally PY-01's larger interface change. Apply behavioral blackbox verification to runtime APIs, generated import/codec checks to compiler output, cross-language byte checks to formats, and separate measurements to performance claims. Pydantic's ordinary path and explicit ADT/any/map-key walkers are intentional, not redundant serialization engines. Keep public Python declarations and explicit sync/async shells. No universal renderer proposal; runtime extraction must respect per-resource embedding limits. Generated-code volume is not the same as maintained generator complexity, nor an optimization acceptance criterion.

## Complete coverage appendix

Every listed file was read fully; truncated batch output was re-read in narrower ranges. Generator `.scala` files: PyBaboonTranslator; PyCodecFixtureTranslator; PyCodecTestTranslator; PyCodecTranslator; PyConversionTranslator; PyDefnTranslator; PyDomainTreeTools; PyFileTools; PyJsonCodecGenerator; PyKeywords; PyMcpServerGenerator; PyServiceWiringTranslator; PyTreeTools; PyTypeTranslator; PyTypes; PyUEBACodecGenerator; PyValue. Runtime `.py` files: baboon_any_opaque; baboon_codecs; baboon_codecs_facade; baboon_conversions; baboon_exceptions; baboon_identifier_repr; baboon_mcp_runtime; baboon_runtime_shared; baboon_service_wiring; cross_language_fixture_path. Supporting searches were targeted, not a full test-suite audit.
