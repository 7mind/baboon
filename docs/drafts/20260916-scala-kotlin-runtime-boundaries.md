# Scala/Kotlin runtime refactor boundaries

Implemented the audit's bounded changes without treating every allocation as removable:

- SC-01: compare the complete compiler-side and embedded `IdentifierRepr` objects, rather than pinning source hashes. This detects mirror drift but does not replace generated identifier compilation/wire tests.
- SC-05: generated Scala codec wrappers and immutable metadata collections have private lazy backing values behind existing methods. Codec resolution remains deferred; runtime singleton interfaces are unchanged.
- SC-06: generated Scala decoding uses a validating count-only index path. The public full-entry reader remains available and both use one validation loop. Length/offset checks, index framing, and the generated context-dependent count assertion remain.
- KT-05: the final KMP buffer writer can copy directly into another writer. Public snapshots still copy; transfer does not expose either backing array and supports self-append. The transfer method is public because generated models may be compiled separately from their runtime artifact.
- KT-07: only the shared Gregorian epoch-day calculation is extracted. Caller-specific millisecond division and formatting remain unchanged. Characterization records an existing discrepancy at -1 ms: the UTC identifier renders `1969-12-31T23:59:59.999Z`, while the offset wrapper renders `1970-01-01T00:00:00.999+00:00`. Correcting the latter is a separate wire-visible change, not part of this cleanup.
- KT-08: pure read-version selection is shared by JVM/KMP. Registration/storage, concurrency, preload, conversion traversal, platform types, and platform-specific error messages remain owned by each facade. No throughput improvement is claimed; the typed selection result itself is an allocation.
- SC-07/KT-06: protocol dispatch is shared within each runtime. Standalone tool access stays dynamic, mux registration keeps its existing snapshot/order/collision behavior, sessions remain caller-owned, and error-description hooks remain owner-specific. No new tool-registry cache is introduced.

## Deliberately retained allocations

Collection decoder/conversion staging remains. Direct destination population would interleave foreign hash/equality calls with element decoding or conversion. That can change exception precedence and reader position when a later element is malformed. No equivalence has been established for those hooks across the supported input domain.

JVM/Scala indexed buffer copying remains. `ByteArrayOutputStream.writeTo` invokes the destination's three-argument `write` overload, while the current emitted expression invokes its single-array overload; runtime stream classes and these methods are extensible. Treating them as interchangeable would change observable override dispatch.

Scala `readByteString` retains the public copying factory. Its extensible input can override `readFully` and retain the destination array; the array is not proven exclusively owned after that callback. An unchecked ownership-taking factory would weaken the existing behavior for such inputs.

The JSON-off Kotlin compilation check also reproduced an existing defect: both facade `preload` implementations referenced the removed JSON registry outside JSON section markers. The correction only extends those markers around the JSON preload expression. Both regenerated JSON-disabled runtimes subsequently compiled with the pinned Kotlin plugin.

## Verification scope

Focused checks include generated Scala and Kotlin JVM/KMP any-envelope compilation and round trips, separately emitted runtime/model composition, JSON-disabled compilation, actual generated Scala metadata identity, full/count-only index error and cursor comparisons, KMP buffer ownership tests, calendar before/after vectors, version-selection vectors, and MCP ownership/protocol before/after transcripts. KMP runtime checks here execute on the JVM; they are not evidence of JS/Native execution. Allocation and throughput are unmeasured. Compiler JVM/JS cross-builds and the broader matrix are coordinated by the integrating worker.
