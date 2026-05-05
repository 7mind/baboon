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

**Active milestone:** none. M33 closed 2026-05-05; see closed-out
entries below and `docs/logs/<m33-close-log>.md` for the milestone narrative.
Next active work pending — typically MFACADE (multi-version codec facade)
once Q10–Q14 in `docs/drafts/20260505-1830-questions-multi-version-facade-upstream.md`
resolve.

**On hold:** Multi-version codec facade upstream (proposal.md). Open
questions Q1–Q14 in
`docs/drafts/20260505-1830-questions-multi-version-facade-upstream.md`.

---

## Milestones (high-level)

- [x] **M33** — Generic structural inheritance via template instantiation (closed 2026-05-05). Six PRs landed; `+`/`-`/`^` template instantiation in DTO/contract bodies via inline substitution; codegen byte-identical (m33-ok fixture); LSP smoke green; spec doc §9 landed; tree-sitter corpus locked. Plan: `docs/drafts/20260505-1500-m33-generic-structural-inheritance-plan.md`. Close-out log: `docs/logs/20260505-2005-m33-close-log.md`.
- [ ] **MFACADE** — Multi-version codec facade upstream (proposal.md). Blocked on Q10–Q14.
- [~] **M32** — META_VERSION_1 1→16 bump. Carry-over (PR-32.1 byte change in main; PR-32.2/PR-32.3 fixes shipped).

---

## M33 — PR breakdown

Detail in `docs/drafts/20260505-1500-m33-generic-structural-inheritance-plan.md`.

- [x] **PR-33.1** — Parser: optional `[…]` head on `+`/`-`/`^` arms. Widens `parentDef`/`unparentDef`/`intersectionDef` and adds `args: Option[NEList[RawTypeRef]]` to the three `RawDtoMember` cases.
- [x] **PR-33.2** — Typer: lower template instantiation in structural arms via `TemplateInstantiator` inline substitution (decision §3.b Option I). Receiving DTO absorbs substituted member list; no transient `DomainMember.User`.
- [x] **PR-33.3** — Typer: negative-path diagnostics. Reuses existing `TyperIssue` cases. Most plan §4 rows already covered by PR-33.2 (D02/D05/D01); this PR adds the residual rows: row 1 `NotATemplate`, row 3 arity mismatch, row 4 forbidden type-arg, row 6 cross-package, row 9 mutual recursion, row 11 duplicate inline, row 12 field-name collision.
- [x] **PR-33.4** — Cycle / recursive-substitution detection. **Largely absorbed into PR-33.2/PR-33.3** (recursion guard + cycle-set + self/mutual-recursion tests landed). Residual scope: audit completeness; enforce non-empty-intersection edge case (PR-33.2 round-2 advisory); harden cycle-key canonicalisation if needed.
- [x] **PR-33.5** — Cross-language acceptance fixture `m33-ok` (`test/conv-test/m33.baboon` + per-backend conv-test rows).
- [x] **PR-33.6** — LSP smoke + close-out + tree-sitter grammar bump + spec doc.
- [x] **PR-33.7** — Deferred-drain: close `[PR-33.3-D01]` (silent-dedup `+ MyGen[i32]; + MyGen[i32]`), close `[PR-33.4-D01]` (empty body under `-`), ship tree-sitter submodule chain-commit script.

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

- [x] **PR-33.7** (2026-05-05, two review rounds — clean) — Deferred-drain: closed two M33 deferred defects + shipped the tree-sitter submodule chain-commit script.
  1. **`[PR-33.3-D01]`** — duplicate template-arm silent-dedup. Added `checkNoDuplicateConvertedFields` in `BaboonTranslator.scala:230-243` (with comment documenting the broadened scope per round-2 D04). Pre-`.distinct` validation groups `converted` by lowercased field name; fails with `TyperIssue.NonUniqueFields` if duplicates present. The `.distinct` call below stays (still needed for ADT contract dedup). The corresponding regression-guard test in `M29ValidatorTest.scala` flipped from `outcome.isRight` to `assertProducesTyperIssue[TyperIssue.NonUniqueFields]`; renamed away from `_REGRESSION_GUARD` suffix.
  2. **`[PR-33.4-D01]`** — empty body under `-` is now symmetric with `^`: emits `TemplateBodyNotFlatForRemoval(kind="minus", offendingMemberKind="empty body")`. Printer at `TyperIssue.scala:749-755` extended to be operator-aware (caret → "intersection over an empty field set"; minus → "removal of an empty field set"). The corresponding regression-guard test for `- Empty[i32]` flipped to expect the diagnostic.
  3. **Tree-sitter submodule chain-commit script** at `scripts/m33-submodule-chain-commit.sh` — `set -euo pipefail`; three-step commit chain (inner `grammars/baboon` → middle `editors/baboon-zed` → parent); does NOT push; verifies preconditions and prints push order. Round-2 hardened: dropped orchestrator-private `proposal.md` carve-out; added opt-in `M33_CHAIN_ALLOW_DIRTY=path1,path2,…` env var for additional allowed dirty paths.
  Verification: `sbt baboonJVM/compile` clean; `sbt baboonJS/compile` clean; `sbt 'testOnly *M29Validator* *M33StructuralTemplateInstantiation* *TemplateInstantiator*'` 61/61 green; `bash -n scripts/m33-submodule-chain-commit.sh` parses.
  Review history: round 1 → 2 deferreds + 1 script; round 2 → 2 new defects D03 (script over-fits to orchestrator's tree) + D04 (broadened `NonUniqueFields` scope on contract diamonds — kept as opportunistic improvement, documented). Round 3 not run (text-only fixes verified directly).

- [x] **PR-33.6** (2026-05-05, single pass — clean) — M33 close-out: LSP smoke + tree-sitter corpus + spec doc + milestone close. Five deliverables shipped:
  1. **LSP smoke tests** (+2 tests, 20 → 22 in `LspFeaturesTest`): hover on `MyGen` inside `+ MyGen[i32]` returns the template signature via the existing `domain.templateRegistry` lookup branch (no provider change needed). Completion right after `+ ` on its own line inside a DTO body offers the template alongside concrete types and builtins. The completion case required one surgical provider change: new `CompletionContext.StructuralArmPosition` branch in `CompletionProvider.scala` matched by `^\s*[+\-^]\s+([\w.]*)$`; returns `getTypeCompletions ++ getBuiltinCompletions ++ getTemplateCompletions` (no keywords, since keywords cannot head a structural arm). New fixture `baboon-compiler/src/test/resources/baboon/m33-lsp/m33-lsp.baboon`.
  2. **Tree-sitter grammar**: the existing grammar at `editors/baboon-zed/grammars/baboon/grammar.js` already accepts `+ TypeRef[Args]` because `parent_def`/`unparent_def`/`intersection_def` use `$.type_ref`, which already includes `$.generic_type`. **No grammar code change required.** Added a corpus test file `editors/baboon-zed/grammars/baboon/test/corpus/m33-template-arms.txt` with 5 cases (parent_def / unparent_def / intersection_def with template instantiation, mixed +/-/^ in one DTO, cross-namespace head). Tree-sitter test count 42 → 47, all green. The submodule pointer bump (`grammars/baboon` inner repo + `editors/baboon-zed` outer repo) is documented but not executed in this PR — per CLAUDE.md "Do not commit. The orchestrator commits at PR close-out", the submodule chain commits are deferred to the orchestrator's close-out commit. Files present in the inner submodule's working tree as untracked entries; the orchestrator stages and commits them at close-out.
  3. **Spec doc**: extended `docs/spec/generics.md` with new top-level §9 "Structural-arm template instantiation (M33)" (~155 lines added: 9.1 syntax, 9.2 inline-substitution semantics, 9.3 operator semantics with `TemplateBodyNotFlatForRemoval` + empty-body sentinel, 9.4 mixed composition, 9.5 recursive substitution + cycle handling, 9.6 constraints + `adt`-arm carve-out, 9.7 PR cross-reference). Added cross-link from §2.6 to §9. M29 §6 item 6 ("templates on ADT inheritance arms ... out of scope") deliberately retained as historically accurate description of M29; §2.6 now carries the M33-supersession note.
  4. **`tasks.md` close**: M33 flipped from `[~]` to `[x]` in milestones list with milestone-summary line; PR-33.6 flipped to `[x]`; "Active brief" updated to reflect milestone close.
  5. **Session log**: `docs/logs/20260505-2005-m33-close-log.md` (~280 lines: orchestrator brief, six-PR narrative with review-round counts and defect headlines, locked architectural decisions §3.a-§3.f, deferred items, final verification matrix).
  Verification: `sbt baboonJVM/compile` clean; `sbt baboonJS/compile` clean; `sbt 'testOnly *LspFeatures* *M33StructuralTemplateInstantiation* *M29Validator*'` 64/64; `tree-sitter test` 47/47; `sbt baboonJVM/test` deferred to gate (see verification section in close log).
  Surprises: (a) tree-sitter required no grammar code change — all three operators routed through `type_ref` which already accepts `generic_type`. The PR-29.8 historical worry was about M29 template *declarations*, not M33 template instantiations. (b) HoverProvider needed no change — `findTypeInfo`'s template-registry fallback branch (PR-29.7 / GAP-2) handles `MyGen` lookup correctly when the template is excised from `domain.defs`. (c) CompletionProvider needed a one-pattern, one-branch surgical addition; total provider change is < 15 lines.

- [x] **PR-33.5** (2026-05-05, two review rounds — clean) — Cross-language acceptance fixture `m33-ok` proves the M29 architectural bet holds for all three M33 structural-arm operators (`+`/`-`/`^`) across all 9 codegen backends. Three consumer DTOs in `test/conv-test/m33.baboon` (with the typer-side mirror at `baboon-compiler/src/test/resources/baboon/m33-ok/m33.baboon`):
  - `IntPageWithStats { + Page[i32]; + Stats[i32] }` — `+` arm.
  - `PageMinusStats { + Page[i32]; + Stats[i32]; - Stats[i32] }` — `-` arm; lowered to `items, total`.
  - `PageOnly { + Page[i32]; + Stats[i32]; ^ Page[i32] }` — `^` arm; lowered to `items, total` via `BaboonTranslator.intersectionLimiters` Field-set equality.
  All three included in `M33OkHolder` root.
  Per-backend wiring across all 10 `CompatMain.*` files (cs, sc, py, rs, ts, kt, kt-kmp, jv, dt, sw) plus `test/conv-test-sw/Package.swift` target registration. Distinct sample values (`total=42, nObservations=7, items=[10,20,30], sum=60` + new-consumer values) ensure swap-bug detection. `test/acceptance/run_acceptance.py` extended to dispatch m33-ok blobs after m29-ok (skip-if-absent for cross-version compat).
  Verification: `mdl --simple-log :build :test-acceptance` 200/200 (build 100s, acceptance 723s wall — round 2 with extended fixture). 9 backends agree byte-identically on m33-ok.json (md5 `f340dd76…`); Swift diverges by sorted-key shape, consistent with m29 history. UEBA blobs byte-identical across all 10 (verified via md5 in fix-subagent report).
  Review history: round 1 → 6 defects (1 minor coverage gap D01 — only `+` arm; 1 minor D02 sample-value collision; 3 note-only D03/D04/D05 project-pattern; 1 process D06 cascaded from D01); fix extended fixture to add `-`/`^` consumers and distinct values; round 2 clean.
  Surprises:
  - **`-` and `^` operator coverage was nearly missed.** The minimal-fixture executor shipped only `+` arms, citing scope-conservatism. Round-1 review flagged this as a real coverage gap because the three operators produce *different* intermediate `RawDtoMember` shapes (FieldDef / UnfieldDef / IntersectionFields) consumed by `BaboonTranslator.convertDto` along different paths. Fix-pass extended the fixture; both lowered shapes now match expectations across all 9 backends.
  - **`^ Template[T]`'s semantic.** Verified: `withoutRemoved.filter(f => intersectionSet.contains(f))` on `Field` case-class equality (name + tpe + prevName + docs). Both arms route through the same `dtoFieldToDefs` lowering so Field instances match cleanly.
  - **`run_acceptance.py` chain** primary → m29-ok → m33-ok with skip-if-absent at each secondary fixture preserves cross-version compat (a pre-M29 binary's blobs lack m29-ok and m33-ok; the chain still succeeds on the primary fixture alone).

- [x] **PR-33.4** (2026-05-05, two review rounds — clean) — Cycle / recursive-substitution detection. Bulk of the planned work landed in PR-33.2 (recursion guard, cycle-set, depth-limit, self/mutual recursion tests) and PR-33.3 (negative-path pins). Residual: closed the empty-intersection edge case (PR-33.2 round-2 advisory). When `^ Template[T]`'s substituted body is empty, the existing `BaboonTranslator.scala:319` `if (intersectionSet.isEmpty)` short-circuit silently turned `^ Empty[T]` into a no-op pass-through. Now fails at lowering time with `TemplateBodyNotFlatForRemoval` carrying sentinel `offendingMemberKind = "empty body"` (`TemplateInstantiator.scala:553-572`). Printer branches on the sentinel to emit a non-paradoxical message: "the substituted body is empty (intersection over an empty field set would be a silent no-op; caught at lowering time…)". Cycle-key canonicalisation hardened from `RawTypeRef.toString` to the explicit `RawTypeRef.render` form (`TemplateInstantiator.scala:449`) — same behaviour today, immune to non-canonical-field drift. Added regression-guard tests for `+ Empty[i32]` (positive no-op pass) and `- Empty[i32]` (positive no-op pass with comment naming deferred [PR-33.4-D01]).
  Files: `typer/TemplateInstantiator.scala`, `parser/model/issues/TyperIssue.scala`, `M33StructuralTemplateInstantiationTest.scala` (15 → 17 tests).
  Verification: `sbt baboonJVM/compile` clean; `sbt baboonJS/compile` clean; `sbt 'testOnly *M29Validator* *M33StructuralTemplateInstantiation* *TemplateInstantiator*'` 61/61; `sbt baboonJVM/test` 550/550.
  Review history: round 1 → 5 defects + 1 nit deferred to follow-up + 1 pre-existing PR-33.3 indentation nit excluded; round 2 clean.
  Locked decisions / surprises:
  - **No new TyperIssue case for empty body.** Reused `TemplateBodyNotFlatForRemoval` with sentinel string `"empty body"` in `offendingMemberKind`. Scaladoc on the field updated to document the sentinel exception. Avoids the M29 3-site exhaustive-match cost; clear sentinel-aware printer branch keeps the rendered diagnostic readable.
  - **Cycle-key canonicalisation** uses `RawTypeRef.render` (declared canonical form) rather than auto-derived `.toString` (implicit equivalence). Both produce identical strings today; the explicit form is fragile-proof.
  - **Empty body under `-`** is deferred ([PR-33.4-D01]) as a silent no-op. Semantically distinct from `^` (removing nothing is idempotent, not a pass-through masquerade), but worth pinning a regression-guard test in case product decides to upgrade to a warning later.

- [x] **PR-33.3** (2026-05-05, two review rounds — clean) — Negative-path diagnostics for the M33 structural-inheritance-via-template surface. 10 new test arms in `M29ValidatorTest.scala` (15 → 25): row 1 `NotATemplate`, row 2 `TemplateNotInstantiated` for `+ MyGen` no-brackets (validator-level pin complementing PR-33.2 D05), row 3 arity mismatch, row 4 genuine matrix-#2 forbidden type-arg `+ MyGen[Other[i32]]`, row 5 positive cross-namespace `+ foo.NsT[i32]`, row 6 namespace-prefix miss (renamed from "cross-pkg" since single-Pkg cross-ns is what's actually exercised; true cross-Pkg deferred per spec §6 item 11), row 7 self-instantiation cycle, row 9 mutual-recursion cycle, row 11 duplicate-arm regression-guard (deferred [PR-33.3-D01] with scoped fix sketched), row 12 inlined field-name collision.
  Code change (one): D02's row 4 work surfaced a real validation gap. `lowerOneArm` did not walk arg constructors for registered-template heads, so `+ MyGen[Other[i32]]` was silently passing (the diagnostic that DID fire was matrix #1 from `substituteTypeRef`, not matrix #2 from arm-pre-validation). Extended `TemplateInstantiator.lowerOneArm` (`baboon-compiler/src/main/scala/io/septimalmind/baboon/typer/TemplateInstantiator.scala:422-441`) with a matrix-#2 walk mirroring `processMember`'s alias-RHS check at L686-708. Reuses existing `TyperIssue.TemplateInstantiationInForbiddenPosition` — no new TyperIssue case, no 3-site exhaustive-match update needed. Verified false-negative-only (shallow `collectFirst`) — parity with alias-RHS mirror; nested-collection coverage `+ MyGen[lst[OtherTemplate[i32]]]` is a known dormant gap inherited from the mirror, not introduced.
  Verification: `sbt baboonJVM/compile` clean; `sbt baboonJS/compile` clean; `sbt 'testOnly *M29Validator* *M33StructuralTemplateInstantiation* *TemplateInstantiator*'` 58/58; `sbt baboonJVM/test` 547/547 (3 pre-existing canceled, unrelated).
  Review history: round 1 → 8 defects (1 major D02 surfacing real `lowerOneArm` validation gap, 4 minor D03-D06 coverage gaps, 3 nits D07-D09 framing/naming/test-name); round 2 clean.
  Surprises:
  - **D02 round-2 finding upgraded to a real code fix.** The original PR-33.3 brief was tests-only; the reviewer's adversarial walk showed plan §4 row 4 (matrix-#2 forbidden type-arg) was both untested AND unimplemented in `lowerOneArm`. The fix re-tasked PR-33.3 with one bounded code change.
  - **Row 6 plan vs reality mismatch.** Plan §4 row 6 was named "cross-package", but the achievable single-source-file behaviour is "cross-namespace prefix miss" — true cross-package is multi-input fixture territory, deferred per spec §6 item 11.
  - **Inline-string fixture pattern.** `M29ValidatorTest.scala` uses inline source strings, not file-based fixtures under `baboon-compiler/src/test/resources/baboon/m33-bad-*/`. Plan §5 named the resource path; honoured the actual codebase convention instead of the plan's named path.
  - **Deferred [PR-33.3-D01] is a preexisting `BaboonTranslator.convertDto` `.distinct` issue** — predates M33, not a regression introduced by PR-33.x. The duplicate-arm row pins idempotent dedup as a regression guard; scoped fix sketched in the defect entry leaves the ADT contract dedup untouched.

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
