# OpenAPI backend review

Baseline `69bbb4c48a519d9a4148f370b76bde19af4bac6a`. Full assigned coverage: `OasBaboonTranslator.scala` 272 lines and `OasTypeTranslator.scala` 242 lines (514 total); no embedded runtime. **O/** expands to `baboon-compiler/src/main/scala/io/septimalmind/baboon/translator/openapi/`; **M/** to sibling `mcp/`; **G/** to sibling `graphql/`. Repository unchanged. No schema generation, validator execution or benchmark was run.

## OAS-01 — Keep schemas structured until final rendering

Observed: O/OasTypeTranslator.scala returns scalar metadata with raw-JSON-string extras (:51), builds object fragments by concatenation (:80/:100/:143/:182), and maintains an escaper (:203). O/OasBaboonTranslator.scala:184 injects field descriptions by checking for and dropping the opening brace. MCP reparses these fragments into Circe Json (M/McpInputSchemaEmitter.scala:246/:267/:382), while already building its own schemas structurally.

Proposal: return `Json`/`JsonObject` from fragment builders, assemble description/properties/required fields structurally, and render once at the OpenAPI document boundary. Reuse Circe already used by MCP, not a new schema DSL. Start with scalar/any fragments and typed description addition; migrate top-level component assembly next. This removes escaping maintenance and serialize-parse work, principally maintained complexity; allocation benefits remain unmeasured.

Effort M; risk medium; confidence high. Preserve ordering, optionality, references and existing schema meanings. Test parsed-schema equality independently of deterministic formatting. Important qualification to parent X-04: `OasEnumKeyMapSchemaTest.scala:55` under `baboon-compiler/.jvm/src/test/scala/io/septimalmind/baboon/translator/openapi/` asserts string substrings including whitespace. Those tests need deliberate structured assertions or a formatting-compatible printer. Sampled evidence does not establish whether external consumers rely on exact JSON text; do not claim that none do.

## OAS-02 — Share prepared schema references with GraphQL/MCP

Observed: foreign collection/resolution (O/OasTypeTranslator.scala:23/:33) and flattened naming (:194/:199) duplicate G/GqlTypeTranslator.scala:40/:50 and :124/:129. OpenAPI collects members, foreign mappings and enum ids in separate domain scans (O/OasBaboonTranslator.scala:97/:100; O/OasTypeTranslator.scala:164). MCP rebuilds resolutions, foreign definitions and enum definitions on every method emission (M/McpInputSchemaEmitter.scala:55/:360/:369).

Proposal: small immutable per-domain schema context containing foreign definitions/runtime resolutions and enum definitions; let OpenAPI derive enum-key ids, GraphQL consume resolutions, and MCP compute each method's reachable closure from the same prepared facts. A shared qualified schema-name function can remain independent. Do not cache across mutable configuration or versions globally.

Benefit: removes duplicate traversal policy and repeated domain scans, without changing component count or output size. Effort S–M; risk low–medium; confidence high. Verify multi-domain/version isolation, alias chains through map/option arguments, unresolved foreigns and deterministic names. Parent X-05 is supported: share preparation, not entire method schemas, because reachable sets and root descriptions differ per request.

## OAS-03 — Share JSON Schema combinators only behind explicit projection decisions

Observed: O/OasTypeTranslator.scala:100/:143 and M/McpInputSchemaEmitter.scala:243/:282 independently construct optional, list, set and map shapes. Both use string/UUID keys as object properties and other keys as entry arrays, but enum-key schemas differ: OpenAPI references components (:146), MCP embeds `enumSchema` (:289). Named refs likewise target components versus local `$defs`. MCP also narrows some unmapped foreigns to strings (:328), while OpenAPI emits opaque objects (O/OasBaboonTranslator.scala:259).

Proposal: after OAS-01, share narrowly scoped constructors such as nullable/array/unique-array/object-map/entry-map that accept already-rendered child schemas. Keep reference resolution, enum-key representation and foreign narrowing in each consumer. If sharing the type walk later, pass a named projection policy rather than only a `$ref` prefix. Existing `TypeRef` remains the semantic input.

Benefit: fewer maintained JSON shapes and consistent structural changes without erasing differing contracts. Effort M; risk medium–high; confidence high. Verify enum map propertyNames, constructor-recursed aliases, recursive `$defs`, reachable-only MCP closure and opaque foreign treatment. Do not silently change OpenAPI's existing non-string-map projection: the sampled test explicitly expects entry arrays for `map[i32,str]` (:104), even though executable-codec alignment merits a separate compatibility investigation. Likewise ADT output/description alignment is a hypothesis to reproduce, not a defect established by this static audit.

## Scope boundaries and verification

Keep OpenAPI a component library: empty paths, no service runtime and no conversion code (O/OasBaboonTranslator.scala:13). Preserve existing scalar formats, numeric bounds, optional-vs-required decisions, any-envelope documentation and deterministic domain/version paths. Whether those projections should evolve to exactly match every executable backend's JSON representation is a distinct contract decision, not an optimization prerequisite.

Sequence: structural fragments first, prepared reference context second, then only the combinators used by both OpenAPI and MCP. Do not combine GraphQL syntax rendering with JSON Schema emission. X-04 needs both explicit reference and foreign/enum projection boundaries, not reference policy alone. No runtime-resource file merging is applicable.

Actual checks: full assigned/source reads and cross-cutting report; full enum-key-map test and both schema validation scripts; sampled MCP fragment/context routines and `docs/language-features.md:527/:786/:1129`; `git grep` tracked test references including hidden JVM sources. The OpenAPI script first parses JSON then invokes SwaggerParser (`test/oas-stub/validate.mjs:33`); that validates document structure, not generated codec payload conformance. No executed-test success is asserted. Later verification should add public translator/parsed-schema behavioral checks and payload validation where contractually intended, consistent with constructive-test-taxonomy; benchmarks separate. CLAUDE.md/review brief read. All assigned files were fully read; supporting MCP/docs coverage was sampled, not full.
