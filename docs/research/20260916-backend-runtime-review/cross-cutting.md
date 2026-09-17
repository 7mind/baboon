# Shared generator and cross-backend findings

Baseline: `69bbb4c48a519d9a4148f370b76bde19af4bac6a`. This report covers the nine shared translator/MCP files plus targeted comparisons between backend files. Per-backend reports provide complete backend/runtime coverage. Findings are source observations and design recommendations; no implementation or benchmark result is implied.

## X-01 — Share identifier classification before sharing identifier rendering

All nine executable-language backends define a local `IdentifierFieldKind` and classifier. Representative declarations/classifiers are in `translator/csharp/CSDefnTranslator.scala:722`, `translator/java/JvDefnTranslator.scala:669`, `translator/kotlin/KtDefnTranslator.scala:566`, `translator/rust/RsDefnTranslator.scala:354`, `translator/swift/SwDefnTranslator.scala:541`, `translator/scl/ScDefnTranslator.scala:550`, `translator/python/PyDefnTranslator.scala:646`, `translator/dart/DtDefnTranslator.scala:621`, and `translator/typescript/TsDefnTranslator.scala:596`. Here and below, `translator/` expands to `baboon-compiler/src/main/scala/io/septimalmind/baboon/translator/`.

An exact-text comparison after trimming line edges and omitting blank lines found a 36-line common declaration/classification block in C#/Java/Kotlin/Rust/Swift. Dart and TypeScript share a 55-line block including classification and integer-range helpers (`DtDefnTranslator.scala:628`, `TsDefnTranslator.scala:603`). These counts overlap and must not be summed into a savings estimate.

Extract the semantic classifier into one sealed type/function over the existing `TypeRef`; retain backend-specific formatting, integer representations, runtime calls, and literal syntax. Model integer width/range explicitly if sharing those decisions is subsequently justified. No duplicate classifier was found in typer/validator by the scoped symbol search. Do not infer that identical source makes every target's numeric domain interchangeable.

Benefit: one place to change supported identifier categories and detect unsupported types. Effort S–M; risk low–medium; confidence high. Verify generated output identity plus canonical identifier parsing/printing, boundaries, nested identifiers, and map keys across all backends. This is a first extraction candidate, not a new universal type-system layer.

## X-02 — Consolidate truly identical documentation renderers

`translator/kotlin/KtTreeTools.scala:43`, `translator/scl/ScTreeTools.scala:43`, and `translator/typescript/TsTreeTools.scala:34` implement the same prefix/suffix merging, backslash processing, and compact/multiline Javadoc rendering. The common body is 28 nonblank lines, including comments. Rust/Dart also share a smaller line-comment rendering sequence (`RsTreeTools.scala:35`, `DtTreeTools.scala:32`).

Extract the identical Javadoc-style function first; keep thin backend adapters. Preserve `DocCommentEscaping.scala:31` and its ordering contract. Do not replace it with `mcp/McpDocs.scala:40`: MCP emits normalized plain JSON descriptions, while source comments preserve formatting and obey `TextTree` escape processing.

Benefit: fewer copies of an already defect-sensitive escaping policy. Effort S; risk low; confidence high. Compare exact emitted comments for empty/prefix/suffix/multiline cases, backslashes, comment delimiters, and Unicode. No whole-language printer abstraction is needed.

## X-03 — Normalize domain product planning without erasing backend output structure

The exact-text scan found 33 shared nonblank lines in `translator/java/JvBaboonTranslator.scala:134`, `translator/kotlin/KtBaboonTranslator.scala:152`, and `translator/scl/ScBaboonTranslator.scala:134`. These acquire the per-domain translator and sequence definitions, fixtures, generated tests, service runtime, conversions, and evolution metadata. Product filtering and model traversal also repeat (`JvBaboonTranslator.scala:118`, `SwBaboonTranslator.scala:64`).

Start with a small typed helper for selecting domain members/products and sequencing common product work, using existing `CompilerProduct`, `BaboonLineage`, and `Error2` error accumulation. Keep acquired subcontext lifetimes explicit. Only introduce a shared domain plan where a second and third backend actually consume it. Facade placement already differs: Java/Scala emit one per lineage (`JvBaboonTranslator.scala:54`, `ScBaboonTranslator.scala:55`), while Swift's lineage traversal simply gathers per-domain outputs (`SwBaboonTranslator.scala:56`). Rendering, import resolution, output paths, C# deduplication, Rust modules, and TypeScript barrels should remain backend-owned.

Benefit: common product-selection policy and fewer places to add a product. Effort M; risk medium; confidence high on duplication, medium on net simplification. Verify output path/content/product equality across flags, accumulated errors, and resource lifetimes. Reject an extraction if its callback/configuration surface is more complex than the repeated body.

## X-04 — Make the shared schema fragment boundary structured

MCP already shares schema semantics through `mcp/McpInputSchemaEmitter.scala`, but it consumes OpenAPI-generated JSON strings and parses them back into Circe values (`McpInputSchemaEmitter.scala:246`, `:267`, `:382`). `openapi/OasTypeTranslator.scala:182` manually builds scalar JSON text and `:203` maintains an escaper. MCP separately assembles typed JSON objects.

Return `io.circe.Json` from the shared fragment layer and render once at output boundaries. Use narrow shared scalar/any/map fragment helpers; keep OpenAPI component assembly and MCP reachable local `$defs` assembly distinct. Projection differences go beyond reference prefixes: OpenAPI references enum-key component schemas while MCP inlines them, and MCP narrows some foreign mappings to strings that OpenAPI leaves opaque. Keep these decisions explicit rather than forcing one type walk. This removes serialization/parsing and escaping maintenance without inventing a schema DSL. The OpenAPI review found whitespace-sensitive substring assertions in `OasEnumKeyMapSchemaTest.scala:55`; preserve formatting or deliberately update those assertions to structural contracts. Dependence of external consumers on exact JSON text remains unknown.

Benefit: fewer escaping and representation boundaries; allocation/runtime impact is unmeasured. Effort M; risk medium; confidence high. Compare parsed schema meaning and deterministic text formatting separately; validate recursive references, enums, maps, foreign mappings, documentation, and every MCP backend. Do not merge GraphQL type projection into JSON Schema emission.

## X-05 — Prepare MCP semantic context once per domain

`mcp/McpInputSchemaEmitter.scala:55` rebuilds foreign resolutions, foreign definitions, and enum definitions for each method's input schema. Its foreign/enum map builders each traverse `domain.defs.meta.nodes` (`:360`, `:369`). Every backend invokes it per service method; examples are `typescript/TsMcpServerGenerator.scala:95`, `java/JvMcpServerGenerator.scala:122`, and `rust/RsMcpServerGenerator.scala:142`. Each backend also constructs its own emitter instance from `OasTypeTranslator`.

Expose a prepared per-domain schema context or a domain-scoped emitter, then compute per-method reachable closure from that stable context. If multi-target compilation is a measured bottleneck, a typed per-domain MCP tool plan can additionally share tool names, descriptions, and schema values across backend renderers. Start with domain preparation; do not add a process-global cache. This retains state isolation and avoids invalidation policy.

Benefit: eliminates observed repeated full-domain scans per method; total compile-time benefit is unmeasured. Effort S–M for preparation, M for a shared tool plan; risk low–medium; confidence high. Verify output identity, recursive schemas, multiple domains/versions in one process, and error behavior. Measure models varying method count and domain size before broadening caching.

## X-06 — Retire compiler-internal scaffolding with evidence

Tracked-file Scala reference search (`git grep`, including hidden platform directories) finds `McpServerGeneratorHookStub` only at its definition and stale documentation in `translator/McpServerGeneratorHook.scala:15` and `:30`. All nine executable-language module bindings use concrete generators (`BaboonModule.scala:103`, `:142`, `:175`, `:208`, `:240`, `:273`, `:305`, `:337`, `:369`). The hook interface itself remains used.

Seven conversion generators retain literal `if (false)` branches: `kotlin/KtConversionTranslator.scala:334`, `scl/ScConversionTranslator.scala:307`, `rust/RsConversionTranslator.scala:225`, `java/JvConversionTranslator.scala:347`, `swift/SwConversionTranslator.scala:355`, `dart/DtConversionTranslator.scala:368`, and `typescript/TsConversionTranslator.scala:213`.

Remove the unused internal stub and obsolete rollout documentation; simplify unreachable branches while preserving the live result and error types. Audit per-backend descriptor fields separately before deleting them. No absence of repository callers proves a public embedded-runtime API unused.

Benefit: modest source cleanup and clearer active invariants. Effort S; risk low for compiler-internal branches, subject to public compiler-library API policy for the stub; confidence high. Verify JVM/JS compilation and unchanged generated output. This is not a large reduction strategy.

## Scope and verification record

Fully read shared files: `AbstractBaboonTranslator.scala`, `BaboonRuntimeResources.scala`, `DocCommentEscaping.scala`, `FQNSymbol.scala`, `McpServerGeneratorHook.scala`, `ServiceContextResolver.scala`, `ServiceResultResolver.scala`, `mcp/McpDocs.scala`, and `mcp/McpInputSchemaEmitter.scala`. Supporting reads sampled the cited backend sections, `typer/model/Conversion.scala`, `BaboonModule.scala` bindings, `docs/new-transpiler-guide.md`, and build/test action definitions. Existing `Conversion.FieldOp` already represents evolution operations: extend/reuse it before proposing a replacement IR.

Actual diagnostics: clean baseline git status; `cloc --skip-uniqueness --by-file`; exact-file SHA-256 comparisons; exact normalized block scan over 159 translator files; `rg` reference searches; targeted source reads. These are static diagnostics, not compiler or runtime tests. The scan proves repeated text only; the recommendations additionally rely on the inspected semantics.

Codegen/runtime changes were not made. Future shared generator changes require JVM and Scala.js checks, generated compile tests and wire/API conformance checks. Existing `baboon-compiler/.jvm/src/test/scala/baboon/runtime/shared/IdentifierReprPropertyTest.scala:9` describes tests using hand-written mirrors of emitted identifiers; its first 125 lines were inspected. Those tests exercise useful runtime behavior, but cannot alone establish correctness of the actual emitter. Preserve generated-language tests rather than treating a mirrored implementation as the complete oracle. `DocCommentBackslashEscapingTest.scala` in the compiler's JVM test tree was also sampled as an existing escape-boundary check; no test execution is claimed.

Run project-required `mdl :build :test` before committing an implementation, and `mdl :ci` before pushing/nontrivial refactors. Resource changes require `sbt clean` before recompilation. Behavioral tests should use public boundaries; benchmark performance claims separately. Izumi guidance informed the explicit effect/lifecycle boundary, and constructive-test-taxonomy informed these verification choices.
