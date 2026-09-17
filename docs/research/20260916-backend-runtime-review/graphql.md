# GraphQL backend review

Baseline `69bbb4c48a519d9a4148f370b76bde19af4bac6a`. Both assigned files fully read: `GqlBaboonTranslator.scala` 327 lines and `GqlTypeTranslator.scala` 191 lines (518 total). No embedded runtime exists in this scope. **G/** below expands to `baboon-compiler/src/main/scala/io/septimalmind/baboon/translator/graphql/`; **O/** to its sibling `openapi/` directory. Repository unchanged; no generated-schema execution or performance measurement.

## GQL-01 — Share runtime-mapping resolution and schema naming, not target projections

Observed: G/GqlTypeTranslator.scala:40/:50 collects foreign `runtimeMapping` and recursively resolves scalar/constructor references. O/OasTypeTranslator.scala:23/:33 repeats that algorithm. G/GqlTypeTranslator.scala:124/:129 and O/OasTypeTranslator.scala:194/:199 construct the same underscore-separated package/owner/type name and replace hyphens/dots. These are semantic/reference preparation shared across schema-only consumers, not GraphQL syntax.

Proposal: a small schema-reference utility over existing `Domain`, `TypeId` and `TypeRef`, returning the current typed references and names. Reuse it from GraphQL, OpenAPI and MCP's existing OpenAPI-dependent resolution path. Do not substitute executable-backend language bindings: `runtimeMapping` is intentionally the schema/wire mapping (`docs/language-features.md:527`). Keep GraphQL field/enum sanitation, SDL nullability and map-entry naming local. Preserve unresolved foreigns as references; do not introduce speculative cycle handling without establishing the typer's invariant.

Benefit: one maintained traversal and naming contract, unchanged generated source. Effort S; risk low–medium; confidence high. Verify direct/chained `rt` aliases, constructor arguments, unresolved foreign scalars, namespace/ADT names and output identity. The sampled OpenAPI enum-map test explicitly pins recursive resolution through map arguments; use comparable public translator checks for GraphQL rather than only testing a copied traversal.

## GQL-02 — Resolve field references once and derive dependencies from that result

Observed: G/GqlBaboonTranslator.scala resolves DTO fields to collect map helper types (:109/:114), resolves them again to find custom scalars (:263), then resolves them again while rendering (:201/:217). Each pass recursively follows foreign aliases. Map helpers are deduplicated by projected name (:155), reflecting deliberate loss of source distinctions such as integer widths.

Proposal: prepare resolved DTO/ADT field references once per domain, then collect map dependencies and used scalars from those same references. A compact record of original field plus resolved reference is sufficient; no GraphQL AST framework is necessary. Continue sorting declarations and deduplicating map helpers by the existing projected-name rule. Resolve the same domain's data once, without a process-global cache or changes to domain object ownership.

Benefit: avoids repeated compiler traversal/allocation and makes the rendered types and dependency collection share inputs. Generated volume should remain identical; total compile-time benefit is unknown. Effort M; risk medium; confidence high. Verify nested maps/options, aliases ending in maps, custom scalars reached through foreign mappings, any-underlying references and ADT branch fields. Compare SDL byte identity and validate references after adding the harness's dummy Query.

## GQL-03 — Isolate SDL description literal encoding from documentation composition

Observed: G/GqlTypeTranslator.scala:17 both combines source documentation and chooses/escapes GraphQL string forms. G/GqlBaboonTranslator.scala:277 separately hand-builds a block-string description for `BaboonAny`; its field rendering deliberately uses `TextTree.verbatim` (:207) to preserve backslashes and margin characters. These are separate escaping layers with separate contracts.

Proposal: factor a GraphQL-only literal encoder accepting already-composed text; let documentation composition and the fixed any description use it. Preserve current whitespace/indentation and short-versus-multiline policy unless separately approved. This is a responsibility split, not a confirmed escaping defect: quoted/control-character edge cases were not executed against a parser here. Do not reuse Java/Javadoc escaping or plain MCP description flattening.

Benefit: an explicit boundary for user text and one place to verify GraphQL escaping, with modest maintained-code benefit and no assumed output-size gain. Effort S; risk medium; confidence high on split, unmeasured on practical payoff. Verify ordinary quotes, backslashes, triple quotes, CR/LF, control characters and Unicode through the real emitted SDL parser, then compare decoded descriptions and exact formatting separately.

## Deliberate boundaries and verification

The backend documents itself as a type-definition library, not an executable service schema (G/GqlBaboonTranslator.scala:15). Keep no Query root, skipped services/contracts, empty DTO placeholders, custom scalars, map-entry helper objects and collapsed any variants. GraphQL's Int/Float width projection and nullability are not interchangeable with OpenAPI scalar metadata. No codecs, conversions or runtime abstraction should be introduced to unify these two small backends.

Sequence: GQL-01 first with output equality, then GQL-02 only if the small prepared record simplifies all consumers; isolate GQL-03 and its parser-facing checks. Parent X-04's structured JSON recommendation applies to OpenAPI/MCP, not SDL. X-05's domain preparation is analogous to GQL-02, but GraphQL does not need MCP's per-method reachable closure.

Actual checks: complete reads of both assigned files and both OpenAPI files; complete `test/gql-stub/validate.mjs` and `test/oas-stub/validate.mjs`; targeted MCP and language-feature documentation reads; tracked Scala reference search including hidden JVM tests. `test/gql-stub/validate.mjs` parses SDL and builds with a dummy Query (:33 onward); no harness run is claimed. Supporting OpenAPI enum-key schema test was fully read, but other schema tests/specs were not audited. CLAUDE.md, review brief and cross-cutting report were read. Proposed verification follows constructive-test-taxonomy: public translation/parsed-schema behavior, plus deterministic output checks; benchmarks separate. Inventory has no sampled-only assigned files.
