# Baboon — Task Ledger

Authoritative ledger of planned and completed work.

> **Predecessor ledgers (frozen, chained):**
> - `docs/archive/20260505-m31-close-ledgers/{tasks,defects}.md` — M29 → M31 + M32-prep (BAB-A04 generics, M30 docstrings, M31 upstream defects, M32 wire-version bump).
> - `docs/archive/20260503-bab-any-anyopaque-ledgers/{tasks,defects}.md` — M1–M28 (`any`/AnyOpaque, identifiers, ADT inheritance, JSON/UEBA codecs, map-key encoding, wire-form canonicalisation).
>
> Locked invariants in both archives remain authoritative. Active entries below supersede their milestone-tracking entries, never their locked invariants.

Status: `[ ]` planned · `[~]` in progress · `[x]` done · `[!]` blocked

---

## Active brief

**Active milestone:** M33 — Generic structural inheritance via template
instantiation (extends BAB-A04 / M29). Allows
`data X { + MyGeneric[i32] }` (and `-` / `^`) to instantiate the template
and apply its shape via the existing structural inheritance pipeline,
without emitting the transient instantiated type as a domain member.
Plan: `docs/drafts/20260505-1500-m33-generic-structural-inheritance-plan.md`.

**On hold:** Multi-version codec facade upstream (proposal.md). Open
questions Q1–Q14 in
`docs/drafts/20260505-1830-questions-multi-version-facade-upstream.md`.

---

## Milestones (high-level)

- [~] **M33** — Generic structural inheritance via template instantiation. Plan: `docs/drafts/20260505-1500-m33-generic-structural-inheritance-plan.md`.
- [ ] **MFACADE** — Multi-version codec facade upstream (proposal.md). Blocked on Q10–Q14.
- [~] **M32** — META_VERSION_1 1→16 bump. Carry-over (PR-32.1 byte change in main; PR-32.2/PR-32.3 fixes shipped).

---

## M33 — PR breakdown

Detail in `docs/drafts/20260505-1500-m33-generic-structural-inheritance-plan.md`.

- [x] **PR-33.1** — Parser: optional `[…]` head on `+`/`-`/`^` arms. Widens `parentDef`/`unparentDef`/`intersectionDef` and adds `args: Option[NEList[RawTypeRef]]` to the three `RawDtoMember` cases.
- [x] **PR-33.2** — Typer: lower template instantiation in structural arms via `TemplateInstantiator` inline substitution (decision §3.b Option I). Receiving DTO absorbs substituted member list; no transient `DomainMember.User`.
- [ ] **PR-33.3** — Typer: negative-path diagnostics. Reuses existing `TyperIssue` cases (NotATemplate / TemplateNotInstantiated / TemplateArityMismatch / NonUniqueFields / CircularInheritance) — no new cases planned.
- [ ] **PR-33.4** — Cycle / recursive-substitution detection. Recursion guard + cycle-detection set keyed by `(receivingTypeId, templateId, argTuple)`; reuses `CircularInheritance` per §3.f.
- [ ] **PR-33.5** — Cross-language acceptance fixture `m33-ok` (`test/conv-test/m33.baboon` + per-backend conv-test rows).
- [ ] **PR-33.6** — LSP smoke + close-out + tree-sitter grammar bump + spec doc.

---

## Cross-cutting architectural notes (locked)

- [x] **§3.a** — Lowering site = `TemplateInstantiator.Impl.processMember` (extends existing M29 substitution walk). Lands in PR-33.2.
- [x] **§3.b** — Option I (inline substitution into receiving DTO's member list). No transient `DomainMember.User`. Lands in PR-33.2.
- [x] **§3.c** — Uniformly composes with all three operators `+`/`-`/`^` and mixed concrete+template arms.
- [x] **§3.d** — Cross-namespace templates supported via PR-29.15's `resolveTemplateKey`. Cross-package remains out of scope (spec §6 item 11).
- [x] **§3.e** — No `Typedef.Dto.contracts` edge for inlined template-instantiation; treat identically to `+ ConcreteRef`.
- [x] **§3.f** — Reuse `CircularInheritance` for self-instantiation cycles; no new `TyperIssue` case unless PR-33.4 review demands it (decision reopens if so).

## Carry-over from prior milestones

- [~] **M32 / PR-32.1** — `META_VERSION_1` 1→16 bump. Byte already lifted in all 11 runtime files at HEAD `0d9d7165`. Final disposition deferred until MFACADE Q2 resolves.

## Completed

- [x] **PR-33.2** (2026-05-05, two review rounds — clean) — Typer lowers `+ Template[Args]` / `- Template[Args]` / `^ Template[Args]` via inline substitution at typer-early time; codegen never sees a template (M29 invariant preserved). Approach: HYBRID — **M1** (pure inline) for `+` and `-`; **M3** (new sealed-trait branch `RawDtoMember.IntersectionFields`) for `^` because intersection requires a list of fields to survive to `BaboonTranslator.intersectionLimiters` and pure inline has no `ScopedRef` to resolve under §3.b Option I's no-synthetic-id constraint.
  Files: `parser/model/RawDtoMember.scala` (+`IntersectionFields`), `typer/TemplateInstantiator.scala` (+~340 lines: structural-arm lowering, recursion guard, arg substitution, `validateNoBareTemplateRefs`, `checkFlatOrFail`), `typer/BaboonTranslator.scala` (+`IntersectionFields` arm in `intersectionLimiters`, refresh PR-33.1 hand-off comments), `typer/BaboonEnquiries.scala` (`hardDepsOfRawDefn` skips args-bearing structural refs since they are lowered before order()), `typer/BaboonFamilyManager.scala` (+`IntersectionFields` arm), `typer/BaboonTyper.scala` (+`validateNoBareTemplateRefs` invocation pre-order). New TyperIssue case `TemplateBodyNotFlatForRemoval` for D02 — bundled M29 3-site exhaustive-match update (DiagnosticsProvider, WorkspaceState, BaboonJS) + printer arm.
  Tests: NEW `M33StructuralTemplateInstantiationTest.scala` (14 tests covering `+`/`-`/`^` with template args, cross-namespace, mixed concrete+template, recursive `Outer[U] + Inner[U]`, depth-limit `Deep[lst[T]]` chain, cycle-set self-reference, D02 negative `-`/`^` with non-FieldDef contributors plus D02-positive control for `+`, D05 bare-template `+`/`-`/`^` rejection).
  Verification: `sbt baboonJVM/compile` clean; `sbt baboonJS/compile` clean (cross-build catches the new TyperIssue exhaustive-match across all 3 sites); `sbt 'testOnly *M33StructuralTemplateInstantiation* *TemplateInstantiator* *M29Validator* *AdtInheritanceParser* *M33StructuralTemplateInheritance*'` 73/73; `sbt baboonJVM/test` 539/540 (one pre-existing `RTCodecTest` failure requires `mdl :test-cs-regular` artifacts — unrelated; reproduced on stashed HEAD).
  Review history: round 1 → 7 defects (1 major D02, 5 minor D01/D03/D04/D05/D06, 1 nit D07 note-only); round 2 clean.
  Surprises / locked decisions:
  - **§3.b Option I needed an M3 fallback for `^`.** Pure inline has no `ScopedRef` for the existing `IntersectionDef`-translator path; synthesizing a `DomainMember` violates "no synthetic id". The smallest carrier was a new sealed-trait branch `IntersectionFields(fields: Seq[FieldDef], meta)` that lives in the raw AST after lowering and has explicit translator handling. Documented at `RawDtoMember.scala:21-39` and `TemplateInstantiator.scala:147-158`.
  - **`-` / `^` cannot accept template bodies that themselves use structural composition from concrete types.** `- Template[Args]` and `^ Template[Args]` must operate on a flat field list; a template body containing `+ ConcreteBase` would have `Base`'s contributions silently lost (D02). Pinned with new `TemplateBodyNotFlatForRemoval` diagnostic that fires at lowering time. Future relaxation (materialise concrete-parent contributions) is an explicit follow-up if user demand surfaces.
  - **Bare-template detection (D05) had to move pre-toposort.** Initial plan was to detect `+ MyGen` (no brackets, head IS a template) inside `lowerStructuralArmsInMembers`. But `BaboonTyper.order()` runs first and `hardDepsOfRawDefn` would otherwise return the bare-template ref as a hard dep — `resolveScopedRef` then fails with confusing `NameNotFound`. Solution: new `validateNoBareTemplateRefs` validator on the `TemplateInstantiator` trait, called from `BaboonTyper.scala:419-425` BEFORE `order()`. `hardDepsOfRawDefn` was correspondingly tightened to skip `args.isDefined` arms (which the validator clears before reaching order).
  - **Recursion guard: depth 32 + cycle-set keyed by `(receivingName, templateName, argTupleKey)`.** Self-deepening `Deep[lst[T]]` exercises depth-limit; mutual `template Self[T] { + Self[T] }` exercises cycle-set. D04 attempt to make the limit constructor-tunable failed because distage cannot fill an unbound `Int` from a Scala default — reverted to hard-coded constant + natural-fixture test.
  - **`@root`-pruning is observable in test assertions** (D03 surprise): with strict-set assertions on `domain.defs`, types like `Wide` (consumed by `+ Wide` and inlined) are NOT in the post-pruning set. The strict-set form is now correct but had to be tuned per fixture.
  - **Synthetic CircularInheritance matrix carries one rendered edge** (D01 fix). `Owner.Toplevel` placeholder in synthetic `TypeId.User` is acceptable because the printer reads only `name.name`; the synthetic id never escapes the diagnostic context.
  - **One advisory carried to PR-33.4 / future:** empty template body under `^ Template[Args]` produces `IntersectionFields(Seq.empty, …)`; combined with `BaboonTranslator.scala:319`'s `if (intersectionSet.isEmpty)` branch, the intersection becomes a no-op. Pre-existing semantic ambiguity inherited from `^ EmptyConcrete`; PR-33.4 may want to enforce non-empty intersections explicitly.

- [x] **PR-33.1** (2026-05-05, three review rounds — clean) — Parser: optional `[…]` head on structural-composition arms. Widens `parentDef`/`unparentDef`/`intersectionDef` in `parser/defns/DefDto.scala` to optionally consume `typeParams`; adds `args: Option[NEList[RawTypeRef]] = None` to `RawDtoMember.{ParentDef, UnparentDef, IntersectionDef}` (`parser/model/RawDtoMember.scala`). Single positional-arity update at `typer/BaboonFamilyManager.scala` (file-tracking layer that legitimately ignores `args`). New test file `M33StructuralTemplateInheritanceParserTest.scala` with 11 tests (positive: `+`/`-`/`^` with args, cross-namespace head, legacy no-args, mixed `+`-only body, mixed three-operator body, nested template arg, cross-line whitespace tripwire; negative: `+ Foo[]` rejected at DTO body level + direct `parentDef` rejection).
  Verification: `sbt baboonJVM/compile` clean (89 pre-existing warnings only); `sbt baboonJS/compile` clean; `sbt 'testOnly *M33StructuralTemplateInheritance* *AdtInheritanceParser*'` 25/25.
  Review history: round 1 → 5 defects (D01..D05, all minor/nit); round 2 → 1 new defect (D06 hand-off comment gap); round 3 clean.
  Surprises / locked decisions:
  - Cross-line whitespace composition (D04) is intentionally permissive: `+ Foo` followed by `[i32]` on a separate line silently binds as args. Tripwire test pins the behaviour; if any future `dtoMember` alternative starts with `[`, the test will fail and force a deliberate decision (option (a) — `SingleLineWhitespace` block — was deferred as out-of-scope for parser-only PR-33.1).
  - PR-33.2 hand-off (D05+D06): a single-source-of-truth comment block in `TemplateInstantiator.substituteDtoMember`'s catch-all enumerates the six other drop-sites that currently silently discard `args` (`BaboonTranslator.scala` 3× and `BaboonEnquiries.scala` 3×). Six pointer comments at the drop-sites all contain the verbatim string `PR-33.2` so a future grep finds the entire hand-off chain. Comment-only; PR-33.2 owns the actual substitution implementation.
  - Existing typer/translator code paths see `args = None` for legacy syntax → zero behaviour change for pre-M33 sources. Today the typer/translator silently drop `args` when it IS populated (no parent-position template references reach them post-instantiation in PR-33.2's intended pipeline; PR-33.2 must re-validate this).

- [x] **PR-32.3** (2026-05-05) — Windows-only CI flake in `test-sc-wiring-result`:
  `runtime.ForeignMapKeyRoundTripSpec` failed with decoded keys carrying an `A:`
  prefix that originated from `KeyCodecHostLastWinsSpec`'s in-flight
  `register(PrefixCodec("A"))`. Race on the process-global mutable
  `FStr_KeyCodec.instance` singleton: scalatest runs suites in parallel (sbt
  default), so a sibling spec can read the singleton between the two
  `register(...)` calls in `KeyCodecHostLastWinsSpec`. Linux happened to
  schedule sequentially; Windows did not. The `after { register(IdentityCodec) }`
  teardown (PR-26.2-D01) restores cleanly post-test but cannot prevent in-flight
  reads. Fix: `Test / parallelExecution := false` in `test/sc-stub/build.sbt`.
  Stub is small; serial cost is negligible. Local
  `mdl --simple-log :build :test-sc-wiring-result` green.

- [x] **PR-32.2** (2026-05-05) — CI hot-fix. Commit `0d9d7165` missed three
  sites that hardcoded the literal `"1"` in JSON `$mv` handling:
    1. `swift/baboon_runtime.swift:1358` reader compared `mvStr == "1"` →
       rejected explicit `"$mv": "16"`. CI test
       `AnyMetaCodecTests.testReadMetaJson_acceptsExplicitMvOne` failed.
    2. `swift/baboon_runtime.swift:1441` writer emitted `"$mv": "1"` literal
       (binary path correctly wrote byte 16) → cross-format inconsistency.
    3. `dart/baboon_runtime.dart:1100, 1188` same residue (latent, locally
       self-consistent because reader and writer both used the stale `'1'`
       — but produced JSON envelopes that disagreed with the binary byte).
    4. `test/sw-stub/.../AnyMetaCodecTests.swift:293` constructed
       `BaboonTypeMeta(1, ...)` literal in a writeJson↔readMetaJson roundtrip;
       reader returns canonical `BaboonTypeMetaCodec.metaVersion`, so the
       structural-equality assertion failed.
  Fix: aligned Swift and Dart writers with cs/java/rust/scala/ts/python — drop
  `$mv` entirely on write (canonical version is the implicit default). Readers
  now compare against `String(metaVersion)` instead of literal `"1"`. Test
  fixture updated to use `BaboonTypeMetaCodec.metaVersion`. Local
  `mdl --simple-log :build :test-swift-regular`, `mdl :test-dart-regular` both
  green.
