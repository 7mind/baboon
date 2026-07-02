# `any` / AnyOpaque — PR execution plan

Tracks GitHub issue #69. Companion to `docs/drafts/20260424-1738-any-opaque-fields.md` (the design spec). This plan is the PR-level breakdown the review-loop executes against; `tasks.md` points here for rationale.

## 1. Overview

The design spec locks the wire format, JSON envelope, language-surface ADT, validator rules and phasing for a new `any` field type. This plan translates each of the spec's 13 phases into one milestone (M1..M13) and enumerates the PRs that compose each. Phases 2-10 (one language each, end-to-end) follow an identical four-PR template; Phase 1 (compiler front-end, no codegen) is decomposed into four narrowly-scoped PRs. Phases 11-12 (schema-only emission) are each a single PR. Phase 13 extends `test/conv-test-*` for cross-language interop. Every PR conforms to CLAUDE.md: surgical changes, reproduce-before-fix, real tests, fail-fast, no default params on public surfaces, `mdl :fmt` pre-merge.

## 2. Milestones

- M1 — Compiler front-end (parser, typed AST, typer, validator). No codegen.
- M2 — Scala end-to-end (runtime + UEBA codegen + JSON codegen + round-trip tests).
- M3 — C# end-to-end.
- M4 — Rust end-to-end.
- M5 — Kotlin end-to-end.
- M6 — Java end-to-end.
- M7 — TypeScript end-to-end.
- M8 — Dart end-to-end.
- M9 — Swift end-to-end.
- M10 — Python end-to-end.
- M11 — GraphQL schema emission for `any`.
- M12 — OpenAPI schema emission for `any`.
- M13 — Cross-language interop tests (`test/conv-test-*`).

## 3. PR breakdown — M1 (compiler front-end)

### PR 1.1 — Parser + raw AST

**Scope.** Extend the grammar of `typeParams` so `any` can accept a mix of qualifier keywords (`domain:this`, `domain:current`) and type refs. Introduce a distinguished raw AST node for `any` references.

**Files to touch.**
- `baboon-compiler/src/main/scala/io/septimalmind/baboon/parser/defns/base/Keywords.scala` — add `domain` / `this` / `current` tokens (or reuse `:` followed by `idt.symbol`).
- `baboon-compiler/src/main/scala/io/septimalmind/baboon/parser/defns/DefDto.scala` — in `typeRef`, dispatch to `anyTypeRefArgs` **only when** the parsed identifier is exactly `any` (no prefix) **and** `[` follows. Bare `any` (no brackets) parses as `Simple(RawTypeName("any"), Nil)` — NOT `AnyRef(None, None)`. The typer (PR 1.2) is responsible for recognising `Simple("any", Nil)` as variant A. This preserves back-compat for user types named `any`. Prefixed forms (`foo.any`, `any.Foo`) parse as ordinary `Simple`/`Constructor` refs.
- `baboon-compiler/src/main/scala/io/septimalmind/baboon/parser/model/RawTypeRef.scala` — add
  ```
  case class AnyRef(qualifier: Option[AnyRef.Qual], underlying: Option[RawTypeRef]) extends RawTypeRef
  object AnyRef {
    sealed trait Qual
    case object DomainThis    extends Qual
    case object DomainCurrent extends Qual
  }
  ```
  and extend `render` to stringify the six DSL forms verbatim.
  **Note:** No `prefix` field — `any` is a global builtin, never prefixed. (Decision during PR 1.1 review.)

**Acceptance criteria.** New parser unit tests (mirroring `ParserTest.scala`) covering all six DSL forms plus rejection cases: `any[domain:bogus]`, `any[,T]`, `any[domain:this]` with extra commas. Gate: `sbt test` passes.

**Risks.**
- Lookahead conflicts: `any` could be a legal user type name; we must keep it treated as a *builtin* similarly to `opt` / `lst`. Decision: reserve `any` as a parser-level keyword only when followed by `[` or at the top-level type position — i.e. preserve back-compat.
- Whitespace inside `domain:this` token. Use `NoWhitespace.noWhitespaceImplicit` inside the qualifier subparser.

**Dependencies.** None.

### PR 1.2 — Typed AST + typer pass

**Scope.** Add `any` to `TypeId.Builtins`; add `TypeRef.Any(variant, underlying)` to the typed `TypeRef` hierarchy; plumb resolution of the qualifier + underlying through `BaboonTyper` / `BaboonTranslator`.

**Files to touch.**
- `baboon-compiler/src/main/scala/io/septimalmind/baboon/typer/model/TypeId.scala` — add `final val any = BuiltinScalar(TypeName("any"))` to `Builtins`. Register in builtin scalar lists as applicable.
- `baboon-compiler/src/main/scala/io/septimalmind/baboon/typer/model/Typedef.scala` — add
  ```
  sealed trait AnyVariant
  object AnyVariant {
    case object Global  extends AnyVariant   // A / D1
    case object ThisDom extends AnyVariant   // B / D2
    case object Current extends AnyVariant   // C / D3
  }
  final case class Any(variant: AnyVariant, underlying: Option[TypeRef]) extends TypeRef
  ```
  Also add a helper `AnyVariant.metaKindByte(v: AnyVariant, hasUnderlying: Boolean): Byte` matching the spec table.
- `baboon-compiler/src/main/scala/io/septimalmind/baboon/typer/BaboonTranslator.scala` (and/or `TypePasses.scala` / `ScopeSupport.scala`) — translate both:
  (a) `RawTypeRef.AnyRef(qualifier, underlying)` → `TypeRef.Any(variant, underlying?)` per the six-form → three-variant mapping.
  (b) `RawTypeRef.Simple(RawTypeName("any"), Nil)` → `TypeRef.Any(AnyVariant.Global, None)` — bare `any` arrives from the parser as `Simple`, not `AnyRef`, to preserve back-compat for user types named `any`. This lookup must take precedence over user-type resolution when the name is exactly `any` with no prefix, but should fall through to user-type resolution for anything prefixed.
- `baboon-compiler/src/main/scala/io/septimalmind/baboon/typer/BaboonEnquiries.scala` — teach `allRefs`, `explode`, `uebaLen`, `hardDepsOfRawDefn`, `fullDepsOfDefn` about `TypeRef.Any`:
  - `uebaLen(any)` → `BinReprLen.Variable.Unknown()` (always length-prefixed on wire).
  - `explode(Any(_, Some(u)))` → `explode(u)`; `explode(Any(_, None))` → empty.
  - `hardDepsOfRawDefn` — inner type is a *soft* dep (evolvability); treat as non-cyclic.

**Acceptance criteria.**
- Typer unit tests (new `AnyTyperTest`) covering all six DSL variants — assert resolved `TypeRef.Any` + correct `AnyVariant` + `underlying` presence + correct meta-kind byte.
- Test that `any[Unknown]` fails with a clear `MissingTypeDef` style error.

**Risks.**
- `TypeRef` hierarchy is pattern-matched exhaustively in many sites (all translator `mkEncoder` / `mkDecoder`). Adding a new `TypeRef` case will produce non-exhaustive-match warnings in **every** translator. Strategy: leave a `throw new RuntimeException("BUG: any field reached translator before M2+")` in all translator sites for now, gated by a TODO comment referencing this plan; explicit error beats silent bytecode crashes. Removed per-language in its own milestone.
- `refMeta` is populated from `allRefs` — we must ensure `Any` refs are registered with `BinReprLen.Variable.Unknown()`.

**Dependencies.** PR 1.1.

### PR 1.3 — Validator rules

**Scope.** Enforce:
- Variants D1/D2/D3 (underlying present): `underlying` must be user-defined and must have `derived[ueba]` (and `derived[json]` if JSON codecs are enabled for the containing type).
- `any` cannot be a map key.
- `any` generic-arg policy: reject `set[any]` (hash undefined), allow `opt[any]`, `lst[any]`, `map[K, any]` for value position — pending user confirmation.

**Files to touch.**
- `baboon-compiler/src/main/scala/io/septimalmind/baboon/validator/BaboonValidator.scala` — add `checkAnyFields`, `checkAnyAsMapKey`, `checkAnyAsSetElement` passes inside `validateDomain`. Dedicated `Any...` issue types rather than extending the existing `MapKeysShouldNotBeGeneric`/`SetsCantContainGenerics` — keeps the `any`-specific error message clear.
- `baboon-compiler/src/main/scala/io/septimalmind/baboon/parser/model/issues/VerificationIssue.scala` — new issue subclasses: `AnyUnderlyingLacksUebaDerivation`, `AnyUnderlyingNotUserType`, `AnyAsMapKey`, `AnyAsSetElement`.

**Acceptance criteria.** Validator tests (new `AnyValidatorTest`) that:
- Accept a model with `data Foo : derived[ueba] { ... }` used via `any[Foo]`.
- Reject `any[PrimitiveOrCollection]`, `any[ForeignOnly]`.
- Reject `map[any, str]`.
- Reject `set[any]`.

**Risks.** `derivations: Map[RawMemberMeta, Set[TypeId]]` in `Domain` (populated by `BaboonTyper.computeDerivations`) is the source of truth for `derived[ueba]` membership. The validator lookup must handle the `derivedJson` check conditionally (only when JSON codegen is enabled — gate via target flags, handled by later phases; M1 only needs the `ueba` check).

**Dependencies.** PR 1.2.

### PR 1.4 — Compile-only end-to-end tests

**Scope.** Wire up a `.baboon` fixture under `baboon-compiler/src/test/resources/baboon/any-ok/` and a sibling `any-bad/` directory containing negative cases. Drive through the full compiler up to (but not including) codegen; assert no typer/validator errors for `ok` models and specific issues for `bad` ones. This closes M1.

**Files to touch.**
- `baboon-compiler/src/test/resources/baboon/any-ok/pkg.baboon` (all six variants, mix of field positions).
- `baboon-compiler/src/test/resources/baboon/any-bad/*.baboon` (map-key, missing derivation, unresolved underlying, malformed qualifier, set-element).
- `baboon-compiler/.jvm/src/test/scala/io/septimalmind/baboon/tests/AnyFrontEndTest.scala` — driver test.

**Acceptance criteria.** All fixtures parse + type + validate (or fail with expected issue types). Gate: `sbt test`, `mdl :fmt`, `mdl :build`.

**Risks.** Fixture drift vs. existing test fixtures. Keep `any-ok` model minimal and independent.

**Dependencies.** PR 1.1, 1.2, 1.3.

## 4. PR breakdown — M2 (Scala end-to-end)

### PR 2.1 — Scala runtime additions

**Scope.** Add `AnyOpaque` ADT + `AnyMeta` record + helper encode/decode routines for the meta header + `decodeAny` facade extension to `baboon-runtime/scala/`.

**Files to touch.**
- `baboon-compiler/src/main/resources/baboon-runtime/scala/BaboonAnyOpaque.scala` (new):
  ```
  final case class AnyMeta(kind: Byte, domain: Option[String], version: Option[String], typeid: Option[String])
  sealed trait AnyOpaque { def meta: AnyMeta }
  final case class AnyOpaqueUeba(meta: AnyMeta, bytes: Array[Byte]) extends AnyOpaque
  final case class AnyOpaqueJson(meta: AnyMeta, json: Json)          extends AnyOpaque
  object AnyMetaCodec { /* readBin, writeBin, readJson, writeJson of meta preamble only */ }
  ```
- `baboon-compiler/src/main/resources/baboon-runtime/scala/BaboonCodecsFacade.scala` — add `decodeAny(opaque: AnyOpaque): BaboonValue[BaboonGenerated]` and the two cross-format conversion helpers (`uebaToJson`, `jsonToUeba`) delegating to existing codec lookups.
- `BaboonCodecs.scala` / `BaboonBinTools` — reuse `writeString`/`readString` (exists already under `BaboonBinTools`, see `BaboonRuntimeShared.scala:168`).

**Acceptance criteria.** Scala compiles; unit tests in `sc-stub` (added by PR 2.4) pass. Gate `mdl :build`.

**Risks.**
- Collision of JSON keys `$d`, `$v`, `$t` with `BaboonTypeMetaCodec` — see Open Question Q1 (must be resolved before this PR starts). Default plan: rename envelope keys to `$ak`, `$ad`, `$av`, `$at`.
- Meta bytes layout must exactly match spec — include a unit test for `AnyMetaCodec.writeBin`/`readBin` round-trip in isolation.

**Dependencies.** M1 merged.

### PR 2.2 — Scala UEBA codec emission

**Scope.** Teach `ScUEBACodecGenerator` to emit encode/decode for `TypeRef.Any` fields, per the spec's `[length][meta-length][kind][meta][blob]` layout.

**Files to touch.**
- `baboon-compiler/src/main/scala/io/septimalmind/baboon/translator/scl/ScUEBACodecGenerator.scala` — add cases to `mkEncoder`/`mkDecoder` for `TypeRef.Any`. Emit calls to `AnyMetaCodec` helpers and branch on `AnyOpaqueUeba` / `AnyOpaqueJson` per spec "Encoding" section (with facade cross-convert).
- Possibly `ScTypeTranslator.scala` — map `TypeRef.Any` to `AnyOpaque` ScType.

**Acceptance criteria.** Codegen golden test: compile `any-ok` fixture, inspect emitted Scala for expected method skeletons.

**Risks.** Length-prefix bookkeeping: the outer `length` field is the number of bytes after itself. In the existing fixed-vs-variable encoder pattern (`ScUEBACodecGenerator.scala:340-376`), variable fields already use `writer.writeInt(length)` — we must not double-wrap. Ensure `TypeRef.Any` reports `BinReprLen.Unknown` and have the generator bypass the generic variable-field wrapper, doing its own explicit length bookkeeping.

**Dependencies.** PR 2.1.

### PR 2.3 — Scala JSON codec emission

**Scope.** Symmetric to PR 2.2 for `ScJsonCodecGenerator`.

**Files to touch.**
- `baboon-compiler/src/main/scala/io/septimalmind/baboon/translator/scl/ScJsonCodecGenerator.scala` — emit encode/decode for `TypeRef.Any` that produces/reads the JSON envelope per spec (or the renamed keys from Open Question Q1).

**Acceptance criteria.** Codegen golden + round-trip encode/decode in later stub test.

**Risks.** Circe cursor API — make sure optional `$d`/`$v`/`$t` are absent when the kind byte doesn't claim them.

**Dependencies.** PR 2.1.

### PR 2.4 — Scala stub tests + round-trip + cross-format

**Scope.** Populate `test/sc-stub/` with a round-trip test suite covering: each of the six variants; `AnyOpaqueUeba` ↔ `AnyOpaqueJson` cross-convert; field added via `any` in v2 decodes cleanly against v1 reader (skip-by-length); facade-resolved typed decode.

**Files to touch.**
- `test/sc-stub/src/test/scala/.../AnyOpaqueSpec.scala` (new).
- Fixture `.baboon` under the stub's model directory with two versions of a root DTO — v2 adds an `any` field.

**Acceptance criteria.**
- `mdl :build :test-scala-regular` passes.
- `mdl :build :test-scala-wrapped` passes.
- `mdl :build :test-gen-compat-scala` passes.

**Risks.** Evolution test requires baboon-meta / lockfile updates; follow existing evolution-test patterns in `test/conv-test-sc`.

**Dependencies.** PR 2.1, 2.2, 2.3.

## 5. PR outlines — M3..M13

- **M3 (C#).** Four PRs paralleling M2. Runtime additions under `baboon-runtime/cs/` (`AnyOpaque.cs`, extend `BaboonCodecsFacade.cs`). UEBA codegen in `CSUEBACodecGenerator.scala`, JSON codegen in the corresponding Newtonsoft-based generator, stub tests in `test/cs-stub/`.
- **M4 (Rust).** Runtime in `baboon-runtime/rust/`, `AnyOpaque` as a `serde`-derivable `enum`, UEBA codegen in `RsUEBACodecGenerator.scala`, JSON codegen in `RsJsonCodecGenerator.scala`, stub tests in `test/rs-stub/`. Watch: Rust's ownership model means `AnyOpaqueUeba` carrying `Vec<u8>` implies a clone on decode.
- **M5 (Kotlin).** Jackson-based JSON codecs. Sealed class hierarchy for `AnyOpaque`. Stub `test/kt-stub/`.
- **M6 (Java).** Jackson again, sealed interface (Java 17+). Stub `test/jv-stub/`.
- **M7 (TypeScript).** Function-based codecs, tagged-union for `AnyOpaque`. Stub `test/ts-stub/`.
- **M8 (Dart).** `dart:convert`, sealed class via `@sealed` pattern or `freezed`-style. Stub `test/dt-stub/`.
- **M9 (Swift).** `Codable`-based JSON, enum with associated values for `AnyOpaque`. Stub `test/sw-stub/`.
- **M10 (Python).** Custom JSON codecs. Dataclasses for `AnyOpaque`. Stub `test/py-stub/`. Confirm whether Python emits UEBA at all when M10 starts.
- **M11 (GraphQL).** Single PR: emit `scalar Any` (or a documented wrapper type) for `TypeRef.Any` in `translator/graphql/GqlTypeTranslator.scala`. Document in generated SDL that the concrete format is the JSON envelope. No codec emission. Decision gate from Open Question Q4.
- **M12 (OpenAPI).** Single PR: emit either a custom `AnyOpaque` component schema or `oneOf` (pending Open Question Q4) in `translator/openapi/OasTypeTranslator.scala`.
- **M13 (Cross-language interop).** Extend `test/conv-test-*`: each language encodes a model with all six `any` variants using UEBA and JSON; the shared test harness decodes it in every other language and asserts equality. Natural split: two PRs: (a) Scala/C# pair first (two most mature), (b) remainder fanning out once (a) has proven the wire format.

## 6. Cross-cutting architectural notes

### 6.1 Language-surface ADT (identical across all nine languages, idiomatic spellings)

```
sealed AnyOpaque                 // trait/interface/enum/discriminated union
  AnyOpaqueUeba(meta, bytes)
  AnyOpaqueJson(meta, json)

AnyMeta(kind: u8, domain: Option[str], version: Option[str], typeid: Option[str])
```

### 6.2 Wire byte layout (UEBA) — restated from spec

```
any_field := length:i32               // bytes after this i32
             meta-length:i32          // bytes of (meta-kind + meta)
             meta-kind:u8
             meta:bytes               // i32-length-prefixed UTF-8 strings in fixed order: domain, version, typeid
             blob:bytes               // UEBA of inner value, runs to (length - 4 - meta-length) bytes
```
Signed `i32`, little-endian, matching `LEDataOutputStream` throughout. Strings via `BaboonBinTools.writeString` (length-prefixed UTF-8) as used everywhere else.

### 6.3 Meta-kind byte table — corrected from spec v0 (matches bitmask rule)

Bit 0 = typeid, bit 1 = version, bit 2 = domain.

| Variant | DSL | Kind (hex) | Bitmask (d/v/t) |
|---|---|---|---|
| A | `any` | `0x07` | 1/1/1 |
| B | `any[domain:this]` | `0x03` | 0/1/1 |
| C | `any[domain:current]` | `0x01` | 0/0/1 |
| D1 | `any[T]` | `0x06` | 1/1/0 |
| D2 | `any[domain:this, T]` | `0x02` | 0/1/0 |
| D3 | `any[domain:current, T]` | `0x00` | 0/0/0 |

Values `0x04`, `0x05` are reserved for future use.

### 6.4 JSON envelope (decided — Q1 resolved: rename)

`{"$ak": <int>, "$ad": "...", "$av": "...", "$at": "...", "$c": <inner JSON>}`. Prefix `$a` avoids collision with `BaboonTypeMeta`'s `$d`/`$v`/`$t` top-level meta keys. `$c` reuses existing `CONTENT_JSON_KEY` convention.

### 6.5 Test organisation

- Parser/typer/validator unit tests under `baboon-compiler/.jvm/src/test/scala/io/septimalmind/baboon/tests/`.
- Per-language generated-code tests under `test/<lang>-stub/`.
- Cross-language interop under `test/conv-test-<lang>/`.
- Golden codegen tests live in the compiler test directory.

### 6.6 Gating commands

Per PR (minimum):
- `mdl :fmt` — enforces Scala formatting.
- `mdl :build` — builds native compiler.
- `sbt test` — compiler unit tests (Phase 1 primarily).
- `mdl :test-<lang>-regular` / `-wrapped` / `-compat-<lang>` — generated-code tests for the language touched.

Per milestone (minimum):
- `mdl :full-build` — format + build + full test matrix.

## 7. Open questions / risks

- **Q1 (resolved 2026-04-24)** — JSON envelope keys renamed to `$ak`/`$ad`/`$av`/`$at`/`$c`. Spec §"JSON envelope" updated.
- **Q2 (resolved 2026-04-24)** — `set[any]` rejected; `opt[any]`, `lst[any]`, `map[K, any]` (value position only) allowed. `any` as map key remains rejected per original spec.
- **Q3** — Schema-only emission for `any` (GraphQL, OpenAPI). Blocks M11 / M12 only.
- **Q4** — Python UEBA emission. Blocks M10 only.
- **Q5 (resolved 2026-04-24 during planning)** — Translator-site cascade: PR 1.2 emits explicit `RuntimeException("BUG: any field reached translator before M<N>+")` placeholders; each later milestone replaces the placeholder for its language.

## 8. Verification commands

### Per PR (minimum gate)

- `mdl :fmt`
- `mdl :build`
- `sbt test` (when the PR touches compiler sources)
- `mdl :test-<lang>-regular` (when the PR touches `translator/<lang>/` or `baboon-runtime/<lang>/`)

### Per M1 PR

- PR 1.1: `mdl :fmt && mdl :build && sbt "testOnly *ParserTest* *AnyParser*"`
- PR 1.2: `mdl :fmt && mdl :build && sbt "testOnly *AnyTyperTest*"`
- PR 1.3: `mdl :fmt && mdl :build && sbt "testOnly *AnyValidatorTest*"`
- PR 1.4: `mdl :fmt && mdl :build && sbt test`

### Per M2 PR

- PR 2.1: `mdl :fmt && mdl :build`
- PR 2.2, 2.3: `mdl :fmt && mdl :build`
- PR 2.4: `mdl :fmt && mdl :build :test-scala-regular :test-scala-wrapped :test-gen-compat-scala`

### Per milestone (Mn close)

- `mdl :full-build`
