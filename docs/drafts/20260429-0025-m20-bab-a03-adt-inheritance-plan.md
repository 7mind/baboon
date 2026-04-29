# M20 / BAB-A03 — ADT branch inheritance / cross-ADT branch reuse

## Executive summary

Baboon already supports structural inheritance of fields via `+ X` (union), `- X` (subtraction), `^ X` (intersection) inside `dto` / `contract` bodies. M20 extends that vocabulary one level up: inside `adt` bodies, the same operators compose **branch sets**, so an endpoint-error ADT can write `+ ErrorAtom` instead of literally re-listing every shared branch. The recommended semantics is **re-emit** (the included branches are structurally re-emitted as nested DTOs of the receiving ADT, with their own type IDs); this preserves the existing "branch is `Owner.Adt(adtId)`-scoped" model and leaves wire format, codegen, and schema emission untouched — the feature becomes a pure source-level convenience that desugars during typing.

## Existing field-inheritance machinery — what to mirror

Source-level vocabulary lives in the parser's DTO grammar at `baboon-compiler/src/main/scala/io/septimalmind/baboon/parser/defns/DefDto.scala:105-123` (`parentDef`, `unparentDef`, `intersectionDef`, `unfieldDef`). These all reduce to a `nonGenericTypeRef` and are wrapped into `RawDtoMember.{ParentDef, UnparentDef, IntersectionDef, UnfieldDef}` (`baboon-compiler/src/main/scala/io/septimalmind/baboon/parser/model/RawDtoMember.scala:1-18`). The same grammar entries are wired into `dtoMember` at `DefDto.scala:125-141`.

Composition is performed in the typer at `baboon-compiler/src/main/scala/io/septimalmind/baboon/typer/BaboonTranslator.scala:217-326` inside `convertDto`. The relevant block is exactly:

- lines 224-239 collect all *additive* fields (own `FieldDef` + `ParentDef`-pulled fields + contract fields).
- lines 240-247 collect *removed* fields from `UnfieldDef` / `UnparentDef`.
- lines 248-253 collect *intersection limiters* from `IntersectionDef`.
- lines 286-295 perform the actual set algebra: `withoutRemoved = (parentFields ++ converted) − removedSet` then optionally `∩ intersectionSet`.
- lines 297-301 enforce that contract-required fields cannot be removed.
- lines 307-311 enforce non-uniqueness of resulting field names.

Dependency surface: `BaboonEnquiries.hardDepsOfRawDefn` at `BaboonEnquiries.scala:219-241` extracts the `ScopedRef`s used by `+ / - / ^` so the toposort can order definition processing and `Toposort.cycleBreaking` (called in `BaboonTyper.order` at `BaboonTyper.scala:425-448`) flags cycles via `TyperIssue.CircularInheritance`. Cross-file file-set tracking goes through `BaboonFamilyManager.filesFromDtoMember` at `BaboonFamilyManager.scala:457-472`.

The model nodes the M20 changes must touch:

- `Typedef.Adt(id, members: NEList[TypeId.User], contracts, fields)` at `baboon-compiler/src/main/scala/io/septimalmind/baboon/typer/model/Typedef.scala:47`.
- `RawAdt(name, members: Seq[RawAdtMember], contracts: Seq[ContractRef], derived, meta)` at `RawDto.scala:22`.
- `RawAdtMember` sealed trait at `RawAdtMemberDto.scala:3-14` (currently `RawAdtMemberDto` and `RawAdtMemberContract` only).

ADT branches are converted in `BaboonTranslator.convertAdt` at `BaboonTranslator.scala:328-365`: each `RawAdtMember` becomes a nested DTO whose `id.owner = Owner.Adt(adtId)` (the scope nesting is established by `ScopeBuilder.scala:94-103`). This is the central invariant that the re-emit semantics preserves.

## 1. Surface syntax

User example: `adt SomeError { + ErrorAtom; ... }`.

Recommended grammar additions in `DefDto.scala` analogues (new file `DefAdt.scala` extensions or shared with `DefDto`):

- `adtParentDef`     := `+` `nonGenericTypeRef`     → `RawAdtMember.IncludeAll(ref, meta)`
- `adtUnparentDef`   := `-` `nonGenericTypeRef`     → `RawAdtMember.ExcludeAll(ref, meta)`
- `adtIntersectionDef` := `^` `nonGenericTypeRef`   → `RawAdtMember.IntersectAll(ref, meta)`
- `adtUnbranchDef`    := `-` `branchName`           → `RawAdtMember.ExcludeBranch(name, meta)` (optional, mirrors `UnfieldDef`)

These are added to `DefAdt.adt` (`DefAdt.scala:25-31`) inside the existing `(adtMember | adtMemberContract)` alternative. Because contract refs are already detected via the `Left(extendedContractRef)` branch and use `is` keyword, there is no grammar collision with `+`/`-`/`^`.

**Position recommendation:** allow these include/exclude/intersect lines anywhere among branch declarations (mirrors how `+ X` interleaves with `field: T` inside a DTO).

**Naming-collision policy:** **error by default.** When `SomeError` declares its own `Forbidden` branch *and* `+ ErrorAtom` (which has its own `Forbidden`), reject with a new `TyperIssue.DuplicatedAdtBranches`. Rationale: silent last-wins encourages drift; "must-be-identical-shape" requires deep DTO structural comparison and gives the user a footgun; explicit `- ErrorAtom.Forbidden` (or `- Forbidden` shorthand) lets the user opt in.

**Reach-through fields:** Re-emit semantics naturally compose: `+ ErrorAtom` pulls `ErrorAtom.Forbidden`'s `RawDto` into `SomeError`'s branch list. When that branch is later processed via `convertDto`, its own `is OperationContext`, `+ X`, etc. resolve in the *receiving ADT*'s scope.

## 2. Inclusion semantics — re-emit (recommended) vs reference (alternative)

**Recommended: re-emit.** When `adt SomeError { + ErrorAtom }`, the typer expands the include into structural copies of each `ErrorAtom` branch, owned by `SomeError`. Outcomes:

- `SomeError.Forbidden` and `ErrorAtom.Forbidden` are **distinct `TypeId.User`** with `Owner.Adt(SomeError)` vs `Owner.Adt(ErrorAtom)`.
- Wire format: each ADT carries its own branch-type IDs. UEBA tag tables, JSON discriminators, Rust enum variants, C# subclasses — all unchanged because each ADT's branch set is fully expanded by the time `Typedef.Adt` is constructed.
- Codegen: invariant. The 9 `*DefnTranslator.scala` translators plus `graphql/` and `openapi/` consume `Typedef.Adt(id, members, contracts, fields)` post-typer; they never see the `+ X` source token.
- Cross-ADT conversion: `ErrorAtom.Forbidden → SomeError.Forbidden` is an **explicit user conversion**, not identity. Consistent with the principle that branches are nested under the ADT.
- Implementation cost: ~confined to `convertAdt` plus minor parser / scope-builder additions.

**Alternative: reference (documented for the user, not recommended).** Branches become first-class types living at the ADT's namespace owner; `Typedef.Adt.members` would semantically be `Set[BranchTypeId]` where two ADTs can share an entry. Implications:

- Each branch needs a stable owner that is *not* `Owner.Adt(...)`. Probably `Owner.Ns` or a new `Owner.SharedBranch`. This invalidates the assumption at `BaboonTyper.deps` lines 469-473 that `Owner.Adt(adtId)` implies dependency-on-the-ADT.
- Wire format becomes ambiguous: which discriminator string identifies a branch shared by N ADTs? The existing scheme assumes `<ADT>.<Branch>` paths.
- Evolution rules in `BaboonComparator.diffAdts` compare members by `TypeId`; sharing means a single rename impacts multiple ADTs simultaneously.
- Cross-ADT conversion is identity, but every codec needs the shared-branch path resolved consistently — this is a wire-format breaking change.

Re-emit wins on every axis except literal identity-of-types. Identity-of-types is not a feature the project currently advertises. Pursuing it for M20 conflates a syntactic-sugar feature with a semantic-model overhaul.

**Decision: re-emit. Reference semantics is documented as a follow-up question for the user; implementing it would be its own milestone.**

## 3. Detailed re-emit algorithm (typer)

In `BaboonTranslator.convertAdt` (`BaboonTranslator.scala:328-365`), before the existing branch resolution:

1. Partition `adt.members` (now extended to a richer `RawAdtMember` ADT) into:
   - `localBranches: Seq[RawAdtMember.Dto | RawAdtMember.Contract]`
   - `includes:      Seq[RawAdtMember.IncludeAll]`
   - `excludesAll:   Seq[RawAdtMember.ExcludeAll]`
   - `excludesByName: Seq[RawAdtMember.ExcludeBranch]` (optional)
   - `intersects:    Seq[RawAdtMember.IntersectAll]`
2. For each include target ref, resolve it via `scopeSupport.resolveScopedRef` to a `TypeId.User`. Look it up in the in-progress `defined: Map[TypeId, DomainMember]`. **Require it to be a `Typedef.Adt`**; otherwise emit `TyperIssue.WrongAdtInclusion(id, includeId, meta)` (analogue of `TyperIssue.WrongParent`).
3. Resolve the include target's *current* branch set: `targetAdt.members` plus a recursive expansion if the target itself uses `+ X` (handled implicitly because the typer processes types in toposort order — by the time `SomeError` is processed, `ErrorAtom`'s branches are already fully expanded in `defined`).
4. Compute the candidate branch set:
   ```
   candidates = localBranchIds ∪ ⋃ (includes resolved as above)
              − ⋃ excludeAllSets
              − excludesByName (mapped to TypeIds inside their owning ADT)
              ∩ ⋃ intersectSets   // only if non-empty
   ```
   The set algebra is keyed by **branch name** (the last segment of the type id), not by `TypeId`, because branch identity is per-receiving-ADT after re-emit.
5. **Re-emit phase.** For each branch coming from an include (i.e., not a local declaration), construct a fresh `Typedef.Dto`:
   - `id = TypeId.User(pkg, Owner.Adt(thisAdtId), TypeName(originalBranchName))`
   - `fields, contracts` copied verbatim from the source branch's `Typedef.Dto`.
   - `derived` annotations: copy. Open question (Q5): should `derived[json]` etc. be re-emitted on the new branch or only on the original? Recommend: copy.
   - Add to the `out` list of `DomainMember.User` returned by `convertAdt`.
6. **Validation:**
   - All branches sharing a name are detected: `candidates.groupBy(_.name)` → if any group has size > 1 *and* came from different sources, fail `DuplicatedAdtBranches`.
   - Empty result post-intersection: fail `EmptyAdt(id, meta)` (already exists at `TyperIssue.scala:64`).
   - Include target is non-ADT: `WrongAdtInclusion`.
   - Include target is in a different domain version: fail `CrossVersionAdtInclusion(id, includeId, meta)` (new). Cross-version composition would otherwise leak version IDs into branch wire format.
7. The toposort dependency edge `SomeError → ErrorAtom` is provided by extending `BaboonEnquiries.hardDepsOfRawDefn` at `BaboonEnquiries.scala:234-235` to include the new include/exclude/intersect refs from `RawAdt`. Existing `Toposort.cycleBreaking` will detect `A + B; B + A` as `CircularInheritance` without further changes.
8. Cross-file file-set tracking: extend `BaboonFamilyManager.collectFilesFromDefn` and add a new `filesFromAdtMember` analogous to `filesFromDtoMember` (`BaboonFamilyManager.scala:457-472`).

## 4. Validator concerns

- **Cycles:** existing `Toposort.cycleBreaking` flow at `BaboonTyper.order:425-448` and `TyperIssue.CircularInheritance` at `TyperIssue.scala:113`. No new code — only `hardDepsOfRawDefn` extension.
- **Name uniqueness:** new `TyperIssue.DuplicatedAdtBranches`. Mirrors `NonUniqueFields` at `TyperIssue.scala:60`.
- **Non-ADT target:** new `TyperIssue.WrongAdtInclusion`. Reuse the `WrongParent`-style printer.
- **Cross-version:** new `TyperIssue.CrossVersionAdtInclusion`. The check is mechanical because `TypeId.User` carries `Pkg` (which encodes domain) — refuse if `includeId.pkg != thisAdt.pkg`.
- **Include target is the ADT itself / transitive self-include:** caught by toposort cycle detection.
- **Intersection between disjoint ADTs:** result is an empty branch set → `EmptyAdt`. Recommend treating this as an error (consistent with current empty-ADT behavior). User question Q4.
- **Branch-name selection (`+ X { Foo, Bar }`):** **Not recommended in M20.** Field-side composition does not support this; uniformity argues against. User question Q3.

## 5. Evolution implications

After expansion, the `Typedef.Adt` model is identical to a hand-written ADT with the same branch set. Therefore `BaboonComparator.diffAdts` (`BaboonComparator.scala:342-...`) and `BaboonRules` ADT handling (`BaboonRules.scala:33,94,282`) need **zero changes**. Branch-add/remove semantics and rename-detection (via `derived[was]` on the branch DTOs) work as before.

The expansion happens before deep-schema hashing (`BaboonTyper.deepSchemaRepr:291-310`), so the deep-schema ID for a v2 ADT is identical whether the user wrote `+ ErrorAtom` or expanded the branches by hand. **This is essential**: it means the source change `Foo, Bar` → `+ ErrorAtom; Foo, Bar` is non-evolutionary if the resulting branch sets coincide. We should add a fixture asserting this.

## 6. Per-language code generation impact

Trace: `BaboonTranslator` produces `Typedef.Adt(id, NEList[TypeId.User], List[TypeId.User], List[Field])` (model file `Typedef.scala:47`). The 9 backend `*DefnTranslator.scala` files plus `graphql/` and `openapi/` consume that already-flattened structure. Therefore: **no codegen change required, in any backend, for re-emit semantics**.

Wire format (UEBA + JSON): unchanged. UEBA discriminator is per `TypeId`, and each receiving ADT's branch has its own `TypeId`.

## 7. GraphQL / OpenAPI

Same reasoning. `graphql/` schema emission renders an ADT as a union of its (now-expanded) branch object types; the SDL output is identical to a hand-written ADT. `openapi/` JSON-schema components likewise.

## 8. Backwards compatibility

Pure additive parser change. Existing `.baboon` files do not use `+` / `-` / `^` inside `adt` bodies; current parser treats those positions as a parse error. After M20, the parser accepts the new tokens but the AST shape is preserved for files that don't use them.

## 9. PR breakdown

### PR M20.1 — Parser + raw AST

**Scope:**
- Extend `RawAdtMember` (file `RawAdtMemberDto.scala`) with new variants: `IncludeAll(ref, meta)`, `ExcludeAll(ref, meta)`, `IntersectAll(ref, meta)`, optional `ExcludeBranch(name, meta)`.
- Extend `DefAdt.adt` parser (`DefAdt.scala:25-31`) to recognize `+ X`, `- X`, `^ X` lines. Reuse `DefDto.parentDef` / `unparentDef` / `intersectionDef` parsers.
- Extend `BaboonFamilyManager.filesFromDtoMember`-style helper for the new `RawAdtMember` arms.
- Add a small parser-only fixture ensuring the new syntax round-trips.

**Success criteria:** existing test suite green; new parser unit test verifies `RawAdt.members` contains the new variant types with correct `meta`.

### PR M20.2 — Typer expansion + validator

**Scope:**
- Extend `BaboonEnquiries.hardDepsOfRawDefn` (`BaboonEnquiries.scala:234-235`) to include refs from the new `RawAdtMember` variants so toposort orders correctly.
- Implement re-emit algorithm in `BaboonTranslator.convertAdt` (`BaboonTranslator.scala:328-365`) per Section 3.
- Add new `TyperIssue` codes: `DuplicatedAdtBranches`, `WrongAdtInclusion`, `CrossVersionAdtInclusion`, plus printers in `TyperIssue.scala` and `IssuePrinter.scala`.
- Reuse existing `EmptyAdt`, `CircularInheritance` issue types for the corresponding cases.

**Success criteria:** all M20.2 unit tests pass; deep-schema-hash equality test passes; cycle / non-ADT / cross-version / collision tests fail with the right issue code.

**Risks:** subtle interaction with `BaboonTyper.deps` at `BaboonTyper.scala:469-473` which has special logic for ADT-member→ADT dependencies. Verify that the include-target dependency is registered *for the receiving ADT*, not for its branches.

### PR M20.3 — Fixtures + cross-language compat

**Scope:**
- Add `.baboon` fixtures under `baboon-compiler/src/test/resources/baboon/m20-adt-include/` with: simple include, include + local branch, include + exclude (`-`), intersection, cycle (negative), non-ADT include (negative), cross-version include (negative), name collision (negative).
- Add a "desugaring equivalence" test: model `Manual` declares branches literally; model `Sugared` uses `+ X`. Both produce identical `Typedef.Adt` (same deep schema ID).
- Wire fixtures into the regular-adt and wrapped-adt test actions. Confirm all 9 language stubs compile and the cross-language `conv-test-*` JSON/UEBA round-trips succeed.
- Add an evolution test: v1 hand-written, v2 sugared; assert `BaboonRules` reports no breaking change between equivalent shapes.

**Success criteria:** `mdl :build :test` green for all backends.

## 10. Open questions for the user

1. **Branch-set semantics (Section 3).** Re-emit (recommended) vs reference (alternative; bigger semantic change with wire-format impact). Confirm re-emit.
2. **Name-collision policy.** Three options: error / last-wins / must-be-identical. Recommendation: error. Confirm.
3. **Selective inclusion `+ X { Foo, Bar }`.** User's spec says "same set operations as with fields" and fields don't have name-selection. Recommendation: not in M20; add `- X.Foo` (or `- Foo` shorthand) instead. Confirm omission.
4. **Empty intersection `^ A ^ B` (no overlap).** Treat as `EmptyAdt` error vs allow empty? Recommendation: error. Confirm.
5. **Chained inclusions `A + B`, `B + C`.** Should `A` transitively get `C`'s branches? With re-emit and toposort-ordered processing, yes, automatically. Confirm acceptable.
6. **Annotation copying.** When a branch is re-emitted, do per-branch `derived[json]` etc. annotations propagate to the new owner? Recommendation: yes (mirror source). Confirm.
7. **Position of include lines.** Recommendation: anywhere among branch declarations (mirrors DTO field-side rules). Acceptable?
8. **`is X` (contract refs) on an ADT plus `+ Y` (ADT include).** The two co-exist — contracts impose required fields on every branch; includes contribute additional branches that must satisfy those fields. New branches in the receiving ADT must additionally satisfy *its* contracts — caught by the existing `MissingContractFields` path. Confirm this is the right interpretation.

## Verification commands per PR

PR M20.1 (parser):
- `sbt "testOnly *ParserSpec"` (whatever exists; otherwise `sbt test` confined to parser modules)
- Inspect `RawAdt.members` shape for a hand-written parsed file; confirm new variant arms

PR M20.2 (typer):
- `sbt test` — unit tests including new `TyperIssue` codes
- `mdl :build` — confirms native image still builds

PR M20.3 (fixtures + cross-language):
- `mdl :full-build` — full pipeline including all 9 language test actions
- `mdl :build :test-gen-regular-adt :test-cs-regular :test-scala-regular :test-rust-regular :test-typescript-regular :test-kotlin-regular :test-kotlin-kmp-regular :test-java-regular :test-dart-regular :test-swift-regular`
- `mdl :build :test-gen-wrapped-adt` and the corresponding language-specific wrapped-adt actions
- `mdl :build :test-gen-compat-*` — evolution / compatibility coverage for the desugaring-equivalence fixture

## Critical files for Implementation

- `baboon-compiler/src/main/scala/io/septimalmind/baboon/parser/defns/DefAdt.scala`
- `baboon-compiler/src/main/scala/io/septimalmind/baboon/parser/model/RawAdtMemberDto.scala`
- `baboon-compiler/src/main/scala/io/septimalmind/baboon/typer/BaboonTranslator.scala`
- `baboon-compiler/src/main/scala/io/septimalmind/baboon/typer/BaboonEnquiries.scala`
- `baboon-compiler/src/main/scala/io/septimalmind/baboon/parser/model/issues/TyperIssue.scala`

---

## Decisions captured (2026-04-29)

User answers in `docs/drafts/20260429-0950-m18-m19-m20-open-questions.md`. Plan body above stands; the deltas below override where they conflict.

### Sequencing
- **Final order: M18 → M19 → M20.** M20 is independent of M18/M19; ships last per user preference.

### Q-M20-1 — Re-emit semantics: confirmed
User adds: "we should just insert copies of each branch at AST level — that's the easiest approach". See Q-FU-3 for the precise expansion stage.

### Q-M20-2 — Name-collision policy: confirmed (a) error
Reject with `DuplicatedAdtBranches`.

### Q-M20-3 — Selective inclusion `+ X { Foo, Bar }`: NOT in M20
User: "we don't need this feature at all. Agreed on subtraction." Skip.

### Q-M20-4 — Empty intersection: confirmed error.

### Q-M20-5 — Chained inclusions: confirmed
User: "expand our dependency extractor and make an early pass adding the branch copies". Toposort-ordered processing handles transitivity automatically.

### Q-M20-6 — Annotation copying on re-emit: confirmed yes.

### Q-M20-7 — Position of include lines: confirmed anywhere among branches.

### Q-M20-8 — Contracts + includes interaction: confirmed
User: "We should get that automatically if we insert branches at AST level in an early stage of the typer."

### Q-FU-2 — Subtraction syntax: dot-prefixed `- X.Foo` allowed; `+ X.Foo` optional
**Final rule:**
- `+ X` — include all branches from `X`. Required.
- `- X` — subtract all branches from `X`. Required.
- `- X.Foo` — subtract single named branch `Foo` from `X`. Required (the dot-prefix earlier ruled out at Q-M20-2 is allowed for subtraction).
- `+ X.Foo` — selective inclusion of single branch. **Optional** — implement only if it's trivial to keep aligned with `- X.Foo` parsing; otherwise skip. Not a requested feature.

Decision per implementation pass: if grammar/typer changes for `- X.Foo` cleanly extend to `+ X.Foo`, ship both; if asymmetric extension, skip `+ X.Foo`.

### Q-FU-3 — Expansion stage: typer-early pass (b), with positioning hint
**Final rule:** expansion lives in the typer, not the raw-AST rewriter. Suggested concrete location per user: in `runTyper`, after types have been ordered by toposort but before the main `foldLeft` over types — re-evaluate positioning if a different stage proves easier during execution.

The plan's §3 "Detailed re-emit algorithm" stands in spirit, with these clarifications:
- The expansion is a single pre-pass over the toposort-ordered list of `RawAdt`s.
- For each ADT in topo order, the pre-pass rewrites the `RawAdt.members` list by replacing `IncludeAll(ref)` arms with the literal `RawAdtMemberDto` entries pulled from the included ADT's `RawAdt.members` (which, by topo-order, has already been expanded if it itself uses `+`).
- After the pre-pass, the standard `BaboonTranslator.convertAdt` flow runs over the rewritten `RawAdt`s — no special-case logic needed in `convertAdt` itself.
- Subtraction (`- X`, `- X.Foo`) and intersection (`^ X`) are applied during the same pre-pass after include expansion.
- Cycle detection: extend `BaboonEnquiries.hardDepsOfRawDefn` with the new `RawAdtMember` ref types so the toposort sees them; existing `Toposort.cycleBreaking` flags `A + B; B + A` cycles via `CircularInheritance`.
- Cross-version include rejection: easy because the resolved ref's `Pkg` differs from the receiving ADT's `Pkg`; the validator catches with `CrossVersionAdtInclusion`.

This gives the user's "AST-level insertion" intent (the rewrite is structural, not semantic) while reusing the existing scope/ref-resolution machinery (the rewrite operates on resolved refs, not raw text).
