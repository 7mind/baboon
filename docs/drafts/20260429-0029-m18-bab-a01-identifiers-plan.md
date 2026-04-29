# M18 — BAB-A01 identifier types: design plan

**Status:** plan only. Execution follows in PRs 44–49 (working numbers).
**Authoritative ledger entries:** `tasks.md` "M18 — BAB-A01" (currently a one-liner placeholder).
**Predecessors:** M1–M16 closed. M17 hygiene PRs 41–43 are independent of this work and may land in either order.

## Executive summary

Introduce a new top-level `id` keyword that defines a value-typed identifier — structurally a `Typedef.Dto` with a boolean `isIdentifier` flag, restricted to primitive/primitive-aliased/nested-`id` fields (no floats, no collections, no other user types), and emitting a parseable `<Name>:<ver>#field:value:field:value:{<NestedName>:<ver>#…}` repr in each target language's idiomatic toString slot. Wire formats (UEBA + JSON) are byte-identical to a `data` of the same shape, so `id <-> data` cross-version conversion is free when the shape is preserved. The risk surface is concentrated in (a) the escaping scheme for the repr string and (b) the per-language toString plumbing for nine backends.

---

## 1 — Parser changes

### 1.1 Add `id` keyword

**File:** `baboon-compiler/src/main/scala/io/septimalmind/baboon/parser/defns/base/Keywords.scala:23`

Add a sibling to `data`:
- New method `def identifier[$: P]: P[Unit] = kw("id")` (do not alias `data` — `id` is a distinct keyword).
- Note `data` already accepts the alias `struct`. We do NOT want `id`/`identifier` to share that mechanism — `id` is the canonical token; no alias.
- The alphabet check in `kw` already prevents `id` from absorbing identifiers like `idx` or `id_foo` (line 12 — `!identChar` boundary).

### 1.2 New raw AST node vs. flag on `RawDto`

**Recommendation: new `RawDtoid` subtype `RawIdentifier`, with a fresh `RawTLDef.Identifier` wrapper. Do NOT overload `RawDto` with a flag at the raw level.**

Justification:
- `baboon-compiler/src/main/scala/io/septimalmind/baboon/parser/model/RawDto.scala:16` — `RawDto` already mixes with `RawDtoid`. Adding `case class RawIdentifier(...) extends RawDefn with RawDtoid` keeps the `RawDtoid` shape (name/members/meta/derived) so reuse of `convertDto` in `BaboonTranslator` is mechanical.
- A flag on `RawDto` would force every parser-level branch (root annotation, namespace nesting, derivation parsing) to thread an identifier-aware boolean. Distinct AST node is locally more invasive but globally cleaner — and parser tests can target the `RawTLDef.Identifier` wrapper directly without false-positive matches against legacy `data` fixtures.

**Files to add/modify:**
- `parser/model/RawTLDef.scala:13` — add `case class Identifier(root: Boolean, value: RawIdentifier) extends RawTLDef`.
- `parser/model/RawDto.scala:16` — add `case class RawIdentifier(name: RawTypeName, members: Seq[RawDtoMember], derived: Set[RawMemberMeta], meta: RawNodeMeta) extends RawDefn with RawDtoid`. Carry `derived` to keep the JSON/UEBA codec opt-in path uniform — even though identifiers always derive both (see §5.4).
- `parser/defns/DefDto.scala:148` — add `def identifierEnclosed[$: P]: P[RawIdentifier]` mirroring `dtoEnclosed` but using `kw.identifier`.
- `parser/defns/DefModel.scala:68` — add `identifier` to the alternation: `(choice | dto | identifier | adt | foreign | contract | service | alias)`. Place it BEFORE `dto` (or anywhere earlier) — the keywords are disjoint, so order doesn't matter for ambiguity but earlier placement makes the grammar's intent self-documenting.
- `parser/defns/DefModel.scala:105` — add `def identifier[$: P]: P[RawTLDef.Identifier] = defDto.identifierEnclosed.map(RawTLDef.Identifier(false, _))`.

### 1.3 Typer — typed AST representation

**Recommendation: keep the user's proposal — `Typedef.Dto` with a `isIdentifier: Boolean` flag.** This is the right call because the wire format and structural-conversion semantics (M5) are identical to `data`; only the `.toString` differs. A separate `Typedef.Identifier` would force every existing `case d: Typedef.Dto` site (we counted 10+ in `grep` results: BaboonRules:79/106, BaboonComparator:281/424, BaboonValidator:96/175/501, BaboonRuntimeCodec:71/87/101/161, BaboonTranslator:174, BaboonEnquiries:180/189/261/308/401/507/575) to add a parallel branch with logic identical to the DTO branch.

**File:** `baboon-compiler/src/main/scala/io/septimalmind/baboon/typer/model/Typedef.scala:43`

Change:
```
case class Dto(id: TypeId.User, fields: List[Field], contracts: List[TypeId.User]) extends User
```
to:
```
case class Dto(id: TypeId.User, fields: List[Field], contracts: List[TypeId.User], isIdentifier: Boolean = false) extends User
```

Default-arg `= false` keeps existing constructor sites working without churn. Pattern matches that bind the case-class structurally (`Typedef.Dto(id, fields, contracts)`) need to be updated to `(id, fields, contracts, _)` or use named copies — audit step required.

**File:** `baboon-compiler/src/main/scala/io/septimalmind/baboon/typer/BaboonTranslator.scala:63`

The factory closure becomes parametric on `isIdentifier`. Add a parallel call:
```
case d: RawIdentifier => convertDto(id, root, d) { case (id, finalFields, contractRefs) => Typedef.Dto(id, finalFields, contractRefs, isIdentifier = true) }.map(d => List(d))
```

`convertDto` is generic over the `RawDtoid`-shaped input, so it Just Works.

### 1.4 Scoping interactions

- **`root id`** — yes, allow. Identifiers should be addressable as gcRoots (a code-graph entry point) just like `data`. Validator §2.2 (existing `checkRoots` at `BaboonValidator.scala:117`) currently rejects `Enum` as a root — extend to also permit `id`. No special-casing needed beyond confirming the test suite covers it.
- **`id` inside `ns`** — yes, transparent. `RawNamespace.defns: Seq[RawTLDef]` already accommodates any new variant.
- **`id` inside `adt` branches** — open question (Q3 below). Recommendation: REJECT in M18 to keep scope tight. Identifiers inside ADT branches would force `Typedef.Adt.dataMembers` (Typedef.scala:74) to grow identifier-aware filtering. Defer to M20.
- **`id` as type alias target** — `type Foo = UserId` should work transparently because aliases resolve to TypeRefs at usage sites (BaboonTranslator.scala:50). Add a unit test.

---

## 2 — Validator changes

### 2.1 New validator: `checkIdentifierFields`

**File:** `baboon-compiler/src/main/scala/io/septimalmind/baboon/validator/BaboonValidator.scala`

Add to the `validateDomain` chain at line 54:
```
_ <- checkIdentifierFields(domain)
```

Implementation walks `domain.defs.meta.nodes.values`, filters `case u: DomainMember.User if u.defn match { case d: Typedef.Dto if d.isIdentifier => ... }`, and validates each field's type per §2.2.

### 2.2 Field-type rules for identifiers

Allowed:
1. Builtin primitive scalars **except** `f32`, `f64`, `f128` (excluded — see §2.4 floats).
   - That means: `bit`, `i08`, `i16`, `i32`, `i64`, `u08`, `u16`, `u32`, `u64`, `str`, `uid`, `tsu`, `tso`, `bytes`. (See `TypeId.scala:41-61`.)
2. User-defined `id` types (i.e. `Typedef.Dto` with `isIdentifier == true`).
3. Type aliases that resolve transitively to (1) or (2). Aliases are erased in BaboonTranslator (`BaboonTranslator.scala:71-72`) — the field's TypeRef will already be the resolved scalar, so no special handling.

Rejected:
- Collections — any `TypeRef.Constructor` (`lst`, `set`, `map`, `opt`). Reject regardless of inner type.
- `f32` / `f64` / `f128` (TypeId.Builtins.f32/f64/f128).
- User types that are NOT `id` (regular `data`, `adt`, `enum`, `contract`, `service`, `foreign`).
- `TypeRef.Any` in any variant. (No `any` in identifiers.)

### 2.3 Error messages

Add to `parser/model/issues/VerificationIssue.scala`:
- `IdentifierFieldFloatType(owner: Typedef.User, badFields: List[Field], meta: RawNodeMeta)` — "identifier `<name>`: field `<f>` has float type `<f64>`; floats are not allowed in identifiers".
- `IdentifierFieldCollection(owner: Typedef.User, badFields: List[Field], meta: RawNodeMeta)` — "identifier `<name>`: field `<f>` is a collection; identifiers may only contain primitive or nested-identifier scalar fields".
- `IdentifierFieldUserNotIdentifier(owner: Typedef.User, badFields: List[(Field, TypeId)], meta: RawNodeMeta)` — "identifier `<name>`: field `<f>` references user type `<T>`, which is not an `id`".
- `IdentifierFieldAny(owner: Typedef.User, badFields: List[Field], meta: RawNodeMeta)` — defensive; in practice §2.2 rule 3 covers it but explicit beats implicit for diagnostic clarity.

### 2.4 Floats — explicit choice

User spec: "Float fields are not allowed." Reasoning: float toString is locale-sensitive and not bit-stable across languages; bidirectional parse cannot guarantee equality. We reject all three: `f32`, `f64`, `f128`. Test fixtures enumerate all three.

### 2.5 Cycle detection — reuse existing

`BaboonValidator.checkLoops` (lines 67-115) already runs over all DTOs via `terminatesLoop` → `allFieldsTerminal`. Identifiers are `Typedef.Dto`, so cycles among identifiers are already caught with no changes needed. Add an explicit unit test: `id A { b: B }; id B { a: A }` should produce a `ReferentialCyclesFound` error.

### 2.6 Coexistence with other validators

- `checkPathologicGenerics` (line 215) — operates on `Typedef.Dto` regardless of `isIdentifier`. For an identifier this is dead-weight (no collections allowed) but harmless.
- `checkAnyFields` (line 335) — same; for an identifier no `any` is allowed so the check returns vacuous true.
- `checkShape` (line 493) — runs `validateFieldShape` over identifier fields. The "field name == owner name" check (line 547) is correct for identifiers too.

No conflicts. The new `checkIdentifierFields` runs in addition.

---

## 3 — toString / repr design

### 3.1 Format

```
<IdentifierName>:<version>#field1:value1:field2:value2:{<NestedName>:<version>#nested-fields}
```

- `<IdentifierName>` — the **simple** type name (`UserId`, not the full `pkg/owner#UserId` form). Pkg/owner are not part of the wire string. Justification: identifiers are typed at usage sites; embedding pkg/owner in repr couples the textual form to internal namespacing the user did not ask for. (Open Q4 — clarify with user.)
- `<version>` — three-segment form `0.2.0` from `Domain.version` (Version.scala). Always emitted in M18.
- Field separator — `:`. Field-name and value are also separated by `:` (so an identifier with three fields produces `name1:value1:name2:value2:name3:value3`). Note: this is asymmetric — the user can't tell where field N ends and field N+1 begins by looking at colons alone, BUT the field positions are determined by the schema (declared order is fixed), so the parser walks the schema while consuming token pairs. **This is the key invariant: parsing is schema-directed, not self-delimiting.**
- Nested identifier — wrapped in `{...}`, full repr including its name and version. The wrapper braces let a parser skip a nested identifier as one token without descending into its escaped string fields.
- Field ordering — **declared order in the source `.baboon` file**, preserved in `Typedef.Dto.fields: List[Field]`. Specify this in the spec doc (PR M18.3) so users know reordering fields is a breaking change for downstream textual consumers.

### 3.2 Escaping scheme — recommendation

**Recommendation: backslash escapes with a 5-character escape set: `\\`, `\#`, `\:`, `\{`, `\}`.**

Rationale:
- The 4 metacharacters `# : { }` partition the structure. The escape character `\` is the 5th metachar (must escape itself). 5 metachars is the minimum.
- Backslash escapes are universally recognised. Every target language has stdlib helpers for the pattern.
- Bidirectional and unambiguous — at parse time, `\X` always means literal X; bare `X` (X ∈ `#:{}\`) is structural. State machine with one bit of lookahead.
- No locale dependence, no Unicode width hazards, no shell-escape gotchas. Encoding is pure ASCII over arbitrary UTF-8 source.

**Rejected alternatives:**

- **Entity codes `&32;`** (user-suggested fallback): forces escaping `&` and `;` too, so 6 metachars not 5; and the encoding is ambiguous between codepoint forms (decimal? hex?). Inelegant given we don't need cross-language entity-decode infrastructure.
- **Percent-encoding (RFC 3986 subset)**: 5 metachars + `%` = still 6, and percent-form (`%23` for `#`) is verbose. URI culture imports a lot of confusion (case-sensitivity of hex, `+` for space conventions etc.) for no gain.
- **JSON-string-style with surrounding quotes**: breaks the "parseable without explicit string delimiters" property the format aims for. Introduces a bracketing mechanic for primitives that's heavier than the metachar problem it solves.
- **Pascal/C-style with conditional quoting**: requires the parser to detect "needs quotes" lookahead and the writer to apply opposite logic. Worse than backslash because writer/reader logic asymmetric.
- **TCL list-style** (sqlite): elegant but obscure; backslash is more idiomatic.

### 3.3 Per-type field rendering

Only `str` fields require escaping. Specifically:

| Type | Render | Escape? |
|---|---|---|
| `bit` | `true` / `false` | no |
| `i08`/`i16`/`i32`/`i64`/`u08`…`u64` | decimal | no |
| `str` | escaped string | YES — apply backslash escapes for `# : { } \` |
| `uid` | canonical UUID hex (e.g. `de7b9e1e-5c93-45fe-beec-da99994f629a`) | no |
| `tsu` / `tso` | RFC 3339 timestamp | no — chars are safe |
| `bytes` | hex (no separators) or base64url (no padding `=`); recommend lowercase hex | no |

### 3.4 Bidirectionality proof sketch

State machine for parsing:
- States: `OUTSIDE_NAME`, `IN_NAME`, `IN_VERSION`, `EXPECT_FIELD_VALUE`, `IN_VALUE`, `IN_NESTED`.
- Transitions on bare `:`, `#`, `{`, `}`, `\` (escape next), other (literal).
- The schema (declared field types and order) drives which state to transition to after each value.

Round-trip property tested in Scala (PR M18.3): for any well-formed identifier value `v` of identifier type `I`, `parse(repr(v)) == v`. Property-based test with random primitives + random nested-id depth.

### 3.5 What the typer/runtime emits

Define a single "repr engine" function in the Scala compiler companion test code (PR M18.3) that takes `(typedef: Typedef.Dto, version: Version, valueAccessor: Field => String)` and produces the repr string. Languages re-implement this in their own runtime — they share spec+tests but not code. Cross-language repr equivalence is tested in the convtest matrix (similar to enum wire-format coverage in M15-PR35).

---

## 4 — Per-language code generation

### 4.1 Idiomatic toString slots

| Lang | Slot | DefnTranslator file |
|---|---|---|
| Scala | `override def toString: String` (case class) | `translator/scl/ScDefnTranslator.scala:241` |
| C# | `public override string ToString()` | `translator/csharp/CSDefnTranslator.scala` |
| Rust | `impl std::fmt::Display for X` | `translator/rust/RsDefnTranslator.scala` |
| TypeScript | `toString(): string { ... }` on the class | `translator/typescript/TsDefnTranslator.scala` |
| Kotlin | `override fun toString(): String` | `translator/kotlin/KtDefnTranslator.scala` |
| Java | `@Override public String toString()` | `translator/java/JvDefnTranslator.scala` |
| Dart | `@override String toString()` | `translator/dart/DtDefnTranslator.scala` |
| Swift | `extension X: CustomStringConvertible { var description: String { ... } }` | `translator/swift/SwDefnTranslator.scala` |
| Python | `def __repr__(self) -> str` (also `__str__` defaults to it) | `translator/python/PyDefnTranslator.scala` |

For each: emit only when `dto.isIdentifier == true`.

### 4.2 Wire-format invariance — verify, do not modify

Claim: identifiers serialize byte-identically to a structurally-equivalent `data`. Verify by reading:
- `translator/<lang>/<*>UEBACodecGenerator.scala` — confirm they branch only on field types and not on any DTO-level flag.
- `translator/<lang>/<*>JsonCodecGenerator.scala` — same.

If a generator pattern-matches `Typedef.Dto(id, fields, contracts)` positionally, the new `isIdentifier` field will need a wildcard added — minor sweep. If it matches by named binding (`d: Typedef.Dto`) and reads `.fields`/`.id`/`.contracts`, no change.

PR M18.4 includes an audit-and-sweep of all 9 codec generators. Verification: identical bytes for `data D { x: i32 }` vs `id D { x: i32 }` UEBA + JSON outputs in a synthetic round-trip test.

### 4.3 Reverse-parse — recommendation: in scope for Scala only in M18

Implement static `def parse(s: String): Either[String, X]` (or `tryParse`) on Scala identifiers as the reference implementation in PR M18.3 / M18.6. It validates the escape scheme is genuinely bidirectional and gives users one canonical implementation to consult. Other languages: deferred to M19+ on user demand.

This is open question Q1 — confirm with user before PR M18.6 lands. If user says "all languages": split M18.6 into per-language PRs.

### 4.4 Identifier-as-map-key for codecs

Note: M19 (BAB-A02) plans to allow primitive-equivalent wrappers as JSON map keys. Identifiers are obvious candidates (their toString is parseable). M18 does NOT enable this — it's M19's scope — but the toString format chosen here must be compatible with M19's needs. Cross-reference in M18 spec doc; M19 plan must inherit the escape scheme from §3.2.

---

## 5 — Free `id <-> data` conversion

### 5.1 Comparator: structural equivalence ignoring `isIdentifier`

**File:** `baboon-compiler/src/main/scala/io/septimalmind/baboon/typer/BaboonComparator.scala:281` and `:424`

Currently `(d1: Typedef.Dto, d2: Typedef.Dto)` compares fields and contracts. Specify: shallow-equality must compare `(id, fields, contracts)` and IGNORE `isIdentifier`. Any pattern-match on the case class needs to use `_` for the new field. Add a comment at the comparison site explaining the deliberate omission.

Test fixture: v1 has `data UserId { u: uid }`, v2 has `id UserId { u: uid }`. The diff between v1 and v2 should classify as `Unchanged` (or at most `ShallowModified` with the mod being purely the identifier-flag). Conversion should be a no-op `DtoConversion` (BaboonRules:81) producing a `Transfer` per field.

### 5.2 Rules: existing DTO-conversion path covers it

`BaboonRules.scala:79` (`case d: Typedef.Dto if sameLocalStruct => DtoConversion(...)`) fires for both `id` and `data` because both are `Typedef.Dto`. No change needed. Verified: the codegen for the conversion just does field copies; whether the source/target has `.toString` override is invisible to the conversion.

### 5.3 Cross-version `data → id` and `id → data`

Both directions transparent. The receiving generated code's constructor doesn't care whether its source was `data` or `id`. Generated conversion: `def convert(src: V1.UserId): V2.UserId = V2.UserId(src.u)`. The `.toString` semantics differ at the type, not at the value.

### 5.4 Derivation requirements

`id` types should always emit JSON + UEBA codecs. Either:
- (a) Implicit: typer auto-attaches `derived[json]` and `derived[ueba]` to every `Typedef.Dto.isIdentifier == true`. Spec the rule, document it, no syntax overhead.
- (b) Explicit: user writes `id UserId : derived[json,ueba] { ... }`.

**Recommendation: (a) implicit always-derive.** Identifiers as a primitive-extension category should be wire-ready by default. Spec the choice in PR M18.3 doc.

---

## 6 — GraphQL / OpenAPI emitters

**Verification, no changes expected.**

- `translator/graphql/` — schema-only emitter. Identifiers emit as GraphQL object types (or scalars — see below). Probably appears as a normal type. Audit `GqDefnTranslator` (or equivalent) for `Typedef.Dto` matches and confirm no `isIdentifier` branch is needed.
- `translator/openapi/` — same for component schemas.

**Optional**: GraphQL/OpenAPI users might want identifiers serialized as **scalar strings** (using the toString format) rather than object types. This would be a richer integration with external systems consuming the schema. Open question Q5 — out of scope for M18 unless user requests.

---

## 7 — PR breakdown

Recommend **6 PRs**, sequential.

### PR-44 (M18.1) — Parser + raw AST + typer flag

**Scope:**
- `Keywords.scala` add `id` keyword.
- New `RawIdentifier` AST + `RawTLDef.Identifier`.
- `DefDto.identifierEnclosed`, `DefModel.identifier`.
- `Typedef.Dto.isIdentifier: Boolean = false`.
- `BaboonTranslator.convertMember` branch for `RawIdentifier`.
- Sweep all positional pattern matches on `Typedef.Dto`.

**Success:**
- `sbt compile` clean, no MatchError exhaustiveness warnings.
- New parser unit tests (`BaboonParserTest`) for `id Foo { x: i32 }`, `root id Bar { x: uid }`, `ns N { id Q { x: str } }`.
- A typer test asserting `isIdentifier == true` on the converted `Typedef.Dto`.
- `mdl :build` passes.

**Risks:**
- Positional match sweep is mechanical but easy to miss one. Use `-Wnonexhaustive-match` to surface gaps. The grep audit in §1.3 enumerates the sites — confirm zero leftover.

### PR-45 (M18.2) — Validator

**Scope:**
- `checkIdentifierFields` in `BaboonValidator.scala`.
- New issue types in `VerificationIssue`.
- Failing-fixture tests under `baboon-compiler/src/test/resources/baboon/identifier-bad/` mirroring `any-bad/` from M1.

**Success:**
- `mdl :build :test` passes.
- Each rejection rule has a fixture: `id-with-collection.baboon`, `id-with-float.baboon`, `id-with-data-ref.baboon`, `id-with-adt-ref.baboon`, `id-with-any.baboon`, `id-cycle.baboon`.
- Cycle detection re-uses existing `checkLoops` — verified by `id-cycle.baboon` triggering `ReferentialCyclesFound`.

**Risks:**
- Fixture placement: the M1 `any-bad/` files initially blocked `mdl :test-gen-regular-adt` because they sat in a directory the codegen pipeline scanned (defect PR-04-D11 in defects.md). Identifier-bad fixtures must NOT live under any model-dir codegen path. Place under `baboon-compiler/src/test/resources/baboon-failing/identifier-bad/` (or wherever `any-bad` ended up post-relocation).

### PR-46 (M18.3) — Spec doc + Scala-only repr engine + property tests

**Scope:**
- Land `docs/spec/identifier-repr.md` (or equivalent — match how the M15 enum wire-format spec was filed).
- Implement Scala `toString` override emission in `ScDefnTranslator`.
- Implement Scala `parse(s: String)` static method on identifier companions.
- Property-based test in baboon-compiler test suite: round-trip random identifier values (random primitives + random nested-id depths up to 4) for repr/parse equality.
- 5–10 hand-crafted edge cases: empty string, string with only metachars, deeply nested chain, max-length UUID and tsu values, `bytes` with high bytes, `i64` MIN_VALUE.

**Success:**
- Spec doc reviewed.
- Scala identifier types in `test/sc-stub` (extend convtest model with at least one identifier) emit correct repr and round-trip-parse.
- Property test passes 1000 random cases.

**Risks:**
- Escape scheme implementation bugs (off-by-one in the writer, mis-handling of trailing backslash). Test cases should explicitly include strings ending in `\`, strings of only `\\\\`, etc.

### PR-47 (M18.4) — Per-language toString emission

**Recommendation: bundle into ONE PR**, given that each backend's toString emission is mechanical and shares the spec from PR-46.

**Scope per backend:**
- Emit toString override (slot from §4.1) when `isIdentifier`.
- Add identifier fixture to the existing convtest model.
- Add a runtime `<lang>RuntimeCodecIdentifierTest` (parallel to M16-PR38's `BaboonRuntimeCodecEnumTest.scala`) verifying repr matches the spec for sample values.
- Audit each backend's UEBA + JSON generator: confirm `Typedef.Dto` matches don't break with new field.

**Success:**
- 9-language `mdl :test-gen-{regular,wrapped}-adt :test-{cs,sc,rs,ts,kt,jv,dt,sw,py}-{regular,wrapped}` matrix green.
- `mdl :test-gen-compat-{cs,sc,rs,ts,kt,jv,dt,sw,py}` matrix green.
- Cross-language repr-equivalence test: each backend produces byte-identical repr for the same identifier value (parallel to M15 enum coverage).
- UEBA + JSON byte-identity tests: `id D { x: i32 }` and `data D { x: i32 }` produce identical wire bytes.

**Risks:**
- Per-language backslash semantics — Python f-strings, Rust raw-string literals, Dart string interpolation, Swift `\(…)`. The CODE that emits the escape table at template generation time must itself be escape-correct. M16 had multiple issues with this class (PR-39 indent bug across Java/Kotlin in parallel). Reviewer pass is essential.
- 9 languages in one PR is large. If reviewer load gets unwieldy, split: PR-47a Scala/C# (already-vetted model), PR-47b Java/Kotlin/JVM-Kotlin, PR-47c Rust/TS/Dart/Swift/Python.

### PR-48 (M18.5) — Conversion translator id↔data verification

**Scope:**
- Add cross-version evolution fixtures: domain v1 with `data UserId { u: uid }`, domain v2 with `id UserId { u: uid }` (same-name shape change).
- Reverse direction: v1 `id UserId { u: uid }`, v2 `data UserId { u: uid }`.
- Tests in all 9 conversion-test stubs verify conversion is a transparent field copy with no value-rejection.

**Success:**
- Comparator classifies the change as Unchanged or ShallowModified-with-identifier-flag-only.
- `BaboonRules` produces `DtoConversion` with `Transfer` ops for every field.
- All 9 conv-test stubs round-trip a v1 value through conversion to v2 with field-equality (toString may differ, that's fine).

**Risks:**
- If users had previously relied on `data → id` being a breaking change (semantic, not structural), this is a behavioural change. Document explicitly.

### PR-49 (M18.6) — Reverse-parse, deferred per Q1

**Conditional on Q1 answer.** If user says reverse-parse is in-scope: split into 9 per-language PRs (PR-49a … PR-49i). Each backend implements `static parse(str)` / `Self::from_str` / `fromString` / etc. and adds round-trip-parse tests.

If Q1 says deferred: skip; mark as M19+ candidate.

---

## 8 — Open questions

1. **Reverse-parse scope.** Is `parse(str): Self` in-scope for M18 across all 9 languages, in-scope for Scala only as a reference implementation (recommended), or deferred entirely to a later milestone?
2. **All-9-language toString rollout.** Should all 9 backends ship in M18 (one or two PRs), or staged like M2..M10 (one PR per language)? Recommendation: bundled — toString is much smaller than the M2..M10 codec work.
3. **`id` placement scope.** Confirm: `root id` allowed, `id` inside `ns` allowed, `id` inside `adt` branches **rejected** in M18 (deferred to M20). Acceptable?
4. **Pkg/owner in repr.** The format uses simple type name (`UserId`), not the full `pkg.owner.UserId`. Confirm this is what the user wants. Alternative: full FQN, but it bloats repr and exposes internal namespacing.
5. **Versioned vs unversioned repr — flag?** Some users may want the repr WITHOUT version (storage keys, URL components, log lines). Should we offer two modes — `toString` (versioned, canonical) plus a separate `toUnversionedString` method? Or annotation-driven: `: derived[id-stable]` to opt in to unversioned? Recommendation: ship versioned-only in M18; revisit if demand emerges.
6. **Float scope clarification.** Spec lists "primitive types" but doesn't enumerate. We propose `bit, i*, u*, str, uid, tsu, tso, bytes` as allowed. **Specifically `tsu` and `tso` (timestamps) are allowed** — they have unambiguous string form. Confirm. Also: `bytes` repr — recommend hex (no separators). Confirm.
7. **GraphQL/OpenAPI scalar mode.** Should identifiers emit as scalar strings (using the toString form) in GraphQL/OpenAPI, instead of object types? Recommendation: object types in M18 (no special-casing); scalar mode as opt-in feature in a later milestone.
8. **Always-derive JSON+UEBA?** Recommendation §5.4: implicit. Confirm vs requiring explicit `: derived[json,ueba]` syntax.
9. **Reserve more keywords?** `id` is short and might conflict with user field names — but field names live in a different syntactic position (after `:` in field defs), so no conflict. Confirm no other reserved-word concerns (the `kw` parser uses `!identChar` boundary so `id` only matches the bare token).

---

### Critical Files for Implementation

- /home/pavel/work/safe/baboon-projects/baboon/baboon-compiler/src/main/scala/io/septimalmind/baboon/parser/defns/DefDto.scala
- /home/pavel/work/safe/baboon-projects/baboon/baboon-compiler/src/main/scala/io/septimalmind/baboon/parser/defns/DefModel.scala
- /home/pavel/work/safe/baboon-projects/baboon/baboon-compiler/src/main/scala/io/septimalmind/baboon/typer/model/Typedef.scala
- /home/pavel/work/safe/baboon-projects/baboon/baboon-compiler/src/main/scala/io/septimalmind/baboon/typer/BaboonTranslator.scala
- /home/pavel/work/safe/baboon-projects/baboon/baboon-compiler/src/main/scala/io/septimalmind/baboon/validator/BaboonValidator.scala

---

## Decisions captured (2026-04-29)

User answers in `docs/drafts/20260429-0950-m18-m19-m20-open-questions.md`. Plan body above stands; the deltas below override where they conflict.

### Sequencing
- **Final order: M18 → M19 → M20.** Reverses the §6 recommendation (which was M19 → M18). M19 depends on M18's parser/repr machinery for the multi-field-`id`-as-map-key case (Q-M19-6).

### Q-M18-1 — Reverse-parse scope: ALL 9 LANGUAGES (was: Scala-only)
Parsers must ship in all 9 backends, not just Scala. Justification per user: needed by M19 to support multi-field `id` types as JSON map keys (the parsing path is how multi-field key codecs reverse the toString form).

**PR-49 (M18.6) is no longer optional and no longer Scala-only.** Reshape per-language parser delivery:
- Recommend bundling parsers WITH the per-language toString work in PR-47 (M18.4) so each backend's identifier shipment is one cohesive PR. PR-46 (M18.3) keeps Scala as the reference implementation + property tests; PR-47 ships toString + parse across all 9 with cross-language repr/parse equivalence tests.
- If the bundled PR-47 gets unwieldy, fallback split: PR-47a (toString, all 9), PR-47b (parsers, all 9).

### Q-M18-2 — Bundle toString into one PR: confirmed (a).

### Q-M18-3 — `id` inside `adt` branches deferred to M20: confirmed.

### Q-M18-4 — Pkg/owner in repr: confirmed (a) simple type name.

### Q-M18-5 — Versioned-only repr: confirmed (a). User adds: "this form is not intended to be used in versioning workflows; while we want it to be parseable, we do NOT expose parsers in an easy-to-use form" — see Q-FU-4 for parser visibility.

### Q-M18-6 — Float scope + primitives list: confirmed.

### Q-M18-7 — GraphQL/OpenAPI: confirmed (a) object types, no special-casing in M18.

### Q-M18-8 — Derivation: USER OVERRIDE — follow `data` rules
Override of recommended (a) implicit always-derive. **Final rule:** `id` types follow the same `: derived[json]`/`: derived[ueba]` annotation + compiler-flag rules as `data` classes. No automatic derivation.

Implication: a user who declares `id Foo { v: uid }` without any `derived` clause gets no JSON or UEBA codec by default. Map-key eligibility (M19) requires the `id` type to have `derived[json]` (and `derived[ueba]` if used in UEBA position) — see Q-FU-1 for the validator rule.

### Q-M18-9 — `id` keyword + field names: confirmed
User clarifies: keywords are context-dependent in baboon's parser. `id` is fine as a field name (e.g., `data User { id: uid; name: str }`) since field-name positions don't allow keyword tokens to shadow identifiers. Verify the existing parser rule already supports this; add a unit test.

### Q-FU-1 — Map-key without JSON derivation: validator rejects
Validator must reject any `id` or `data` wrapper used as a JSON map key without `derived[json]`. Same rule for `derived[ueba]` if UEBA position. New `TyperIssue.MapKeyMissingDerivation(owner, badField, missingDerivation)` issue type. Lands in PR M19.1 alongside `checkUserMapKeysEligibility`.

### Q-FU-4 — Parser visibility: internal/codec-namespace
Override of plan's implicit "public companion `parse`". **Final rule:** parsers live on a separate codec / internal-style namespace per language, NOT as primary public static methods on the type. Concrete per-language conventions (subject to refinement during PR-47):
- Scala: `<TypeName>Codec.parseRepr(s)` — co-located with the codec object, NOT as `<TypeName>.parse(s)` on the companion.
- Kotlin / Java: `<TypeName>Codec.parseRepr(s)` static method, not on the type's companion/builder.
- C#: `<TypeName>Codec.ParseRepr(string)` static, not on the type itself.
- Rust: `crate::<lang>::<typename>::parse_repr(s)` in a module path the codec uses; do NOT `impl FromStr` (that would advertise it as canonical).
- TypeScript: `<typeNameCodec>.parseRepr(s)` exported from the codec module, not as a static on the class.
- Dart: `<TypeName>Codec.parseRepr(s)` static, not on the class.
- Swift: `<TypeName>Codec.parseRepr(_:)` static, not on the type.
- Python: `<TypeName>Codec.parse_repr(s)` static method on the codec class, not on the dataclass.

Goal: the parser is reachable when needed (map-key codec emission, user's deliberate use) but doesn't show up as the obvious "how do I get my id from a string" answer in IDE autocomplete on the type. The toString form is the canonical user-facing surface.
