# Baboon — Task Ledger: `any` / AnyOpaque (issue #69)

Authoritative ledger of planned and completed work. Design spec: `docs/drafts/20260424-1738-any-opaque-fields.md`. Execution plan: `docs/drafts/20260424-1738-any-opaque-plan.md`.

Status: `[ ]` planned · `[~]` in progress · `[x]` done · `[!]` blocked

---

## Milestones (high-level)

- [~] **M1** — Compiler front-end (parser, typer, validator). No codegen.
- [ ] **M2** — Scala end-to-end (runtime + UEBA codegen + JSON codegen + round-trip tests).
- [ ] **M3** — C# end-to-end.
- [ ] **M4** — Rust end-to-end.
- [ ] **M5** — Kotlin end-to-end.
- [ ] **M6** — Java end-to-end.
- [ ] **M7** — TypeScript end-to-end.
- [ ] **M8** — Dart end-to-end.
- [ ] **M9** — Swift end-to-end.
- [ ] **M10** — Python end-to-end.
- [ ] **M11** — GraphQL schema emission.
- [ ] **M12** — OpenAPI schema emission.
- [ ] **M13** — Cross-language interop tests.

---

## Milestone 1 — PR breakdown

Detail in `docs/drafts/20260424-1738-any-opaque-plan.md` §3. One line per PR here; sub-tasks stay in the plan doc.

- [x] **PR 1.1** — Parser + raw AST (`RawTypeRef.AnyRef`, qualifier tokens).
- [ ] **PR 1.2** — Typed AST + typer (`TypeRef.Any`, `AnyVariant`, `Builtins.any`, `BinReprLen` hook).
- [ ] **PR 1.3** — Validator rules (map-key rejection, `derived[ueba]` check, generic-arg policy).
- [ ] **PR 1.4** — Compile-only end-to-end fixture tests.

---

## Cross-cutting architectural notes (locked)

- [x] **Wire layout**: `[length:i32][meta-length:i32][meta-kind:u8][meta:strings][blob]`. Signed i32, little-endian. See plan §6.2.
- [x] **Meta-kind byte table** (bitmask, bit 0=typeid, 1=version, 2=domain): A=`0x07`, B=`0x03`, C=`0x01`, D1=`0x06`, D2=`0x02`, D3=`0x00`. `0x04`/`0x05` reserved. (Corrected from spec v0 during planning; spec updated.)
- [x] **Language surface**: non-generic sealed ADT `AnyOpaque = AnyOpaqueUeba | AnyOpaqueJson`, both carrying `AnyMeta(kind, domain?, version?, typeid?)`. Identical across 9 target languages, idiomatic spelling.
- [x] **Registry strategy**: reuse existing `BaboonCodecsFacade` (per-domain-version codec tables with cross-version compat resolution already implemented). No new registry needed.
- [x] **Encoding semantics**: cross-format convert (UEBA ↔ JSON) via facade lookup by `(domain, version, typeid)` from meta. Each native branch encodes directly without round-trip.
- [x] **Evolution**: any variant change or underlying-type change is breaking; no auto-conversion.
- [x] **Q1 — JSON envelope key collision** (2026-04-24): rename envelope keys to `$ak`/`$ad`/`$av`/`$at`/`$c`. Lands in PR 2.1/2.3. Spec + plan updated.
- [x] **Q2 — `any` in generic arg positions** (2026-04-24): reject `set[any]` and `any` as map key; allow `opt[any]`/`lst[any]`/`map[K, any]` (value position). Lands in PR 1.3. Spec updated.
- [ ] **Q3 — GraphQL/OpenAPI schema representation**: custom scalar vs. wrapper object vs. `oneOf`. Blocks M11/M12 only. Defer.
- [ ] **Q4 — Python UEBA emission**: confirm Python target emits UEBA. Blocks M10 only. Defer.
- [x] **Q5 — Translator-site cascade**: PR 1.2 emits `throw RuntimeException("BUG: any field reached translator before M2+")` placeholders in each translator's `TypeRef` match; removed per-language per milestone.

---

## Completed

- **PR 1.1** (2026-04-24) — Parser + raw AST for the `any` DSL. Added `RawTypeRef.AnyRef(qualifier, underlying)` with sealed `Qual` trait (`DomainThis`, `DomainCurrent`). Extended `DefDto.typeRef` to dispatch to `anyTypeRefArgs` **only** when head identifier is exactly `any` (no prefix) AND `[` follows; bare `any` remains a user-identifier-level `Simple("any", Nil)` so a user type named `any` keeps working. Prefixed `foo.any`, `any.Foo`, nested `opt[foo.any]` all parse as ordinary refs. All 6 DSL variants (A=`any`, B=`any[domain:this]`, C=`any[domain:current]`, D1=`any[T]`, D2=`any[domain:this,T]`, D3=`any[domain:current,T]`) recognised. Added placeholder `throw` in `BaboonTranslator.convertTpe` for the new `AnyRef` case (pre-authorised by plan §Q5; PR 1.2 will replace). Added `require` to `AnyRef` ensuring `qualifier.isDefined || underlying.isDefined` (unreachable-case fail-fast). 35-case `AnyParserTest` with both document-level `expectFailure` and strict typeref-level `expectTypeRefFailure`. Verification: `sbt "testOnly *AnyParserTest *ParserTest"` → 37/37 green; `sbt test` → 139 passed + 3 pre-existing cancels; `mdl :build` → native binary produced. Three rounds of adversarial review, 17 defects total, all resolved. Key surprises:
  - **Parser-typer split for bare `any`**: to preserve back-compat for user types named `any`, the parser produces `Simple("any", Nil)` for bare `any`, NOT `AnyRef(None, None)`. PR 1.2's typer must recognise **both** `AnyRef` and `Simple("any", Nil)` as source for `TypeRef.Any` — documented in both `docs/drafts/20260424-1738-any-opaque-fields.md` (Typed AST section) and `docs/drafts/20260424-1738-any-opaque-plan.md` §PR 1.1 + §PR 1.2.
  - **`mdl :fmt` hazard**: whole-tree formatter will churn unrelated files (round 1 pulled 4 translator files into the PR). Future PRs should either skip `mdl :fmt` or run it on a clean tree first, then rebase.
  - **fastparse dispatch subtlety**: `("any" ~ &("[")) | fallback` doesn't work — fastparse's `|` cannot backtrack past consumed input. Correct pattern: parse the identifier atomically via `nonGenericTypeRef`, then branch on its text. Future generic-constructor work in this file should follow this pattern.
  - **Whitespace in `anyQualifier`**: body uses no `~` operators (so no whitespace consumed around `:`). Defensive `NoWhitespace.noWhitespaceImplicit` import via `.discard()` future-proofs against a regression from a well-meaning editor adding `~`.
  - **Meta-kind byte table error in spec v0**: during planning, the spec's hex table contradicted the bitmask rule for variants A (`0x05`→`0x07`) and D1 (`0x04`→`0x06`). Corrected in both spec and plan before any code was written.
