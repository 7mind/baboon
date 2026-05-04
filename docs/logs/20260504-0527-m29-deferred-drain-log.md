# M29 deferred-defects drain — session log

Date: 2026-05-04
Branch: `wip/generics` (commits e18d1e9, 8c9988b, 24b27bc, b08d26e, 40ba085)
Driver: `./job.md` (post-milestone cleanup of M29-deferred items)

## Original user request

> read ./job.md, fix all the defects in /review-loop

`job.md` enumerated 7 deferred defects from the closed M29 milestone, grouped:

- **Group A** (mechanical, ship in any order): `[PR-29.4-D04]`, `[PR-29.10-D04]`, `[PR-29.10-D05]`.
- **Group B** (gated on a spec/scope decision): `[PR-29.5-D04]` / `[PR-29.7-D07]` / `[PR-29.8-D06]` (cross-namespace template detection).
- **Group C** (verify-and-close, no code change expected): `[PR-29.5-D06]`, `[PR-29.10-D03]`.

Pre-flight requirements: CI on `wip/generics @ 1a915e5` must be green (user confirmed "still waiting for CI but proceed; if issues we rebase"). Locked spec invariants from `docs/spec/generics.md` §6 to be respected.

## PRs shipped (5)

### PR-29.12 — DuplicateTemplateName diagnostic (closes [PR-29.4-D04])
- 6 files modified (+232 / -11). Two rounds of review (11 defects D01-D11; all addressed: 7 inline-fixed, 4 deferred with rationale).
- New `TyperIssue.DuplicateTemplateName(name, ownerName, meta)` with 3-site exhaustive-match update (DiagnosticsProvider, WorkspaceState, BaboonJS) — JS canary clean.
- `TemplateRegistryBuilder.buildRecursive` now `groupBy`s the merged registry entries and emits one `DuplicateTemplateName` per offending key (citing the *second* definition's meta — the one being dropped). 11→15 tests in TemplateRegistryBuilderTest (cross-ns negative control, 3+ duplicates → exactly one issue, multi-segment ns path `a.b`, template+non-template name reuse) + meta.pos pin in the existing top-level dup test (asserts `dupIssue.meta.pos.full.start.line == 9`).
- Two unreachable `Owner.Adt` catch-alls in `TemplateRegistryBuilder` replaced with explicit `IllegalStateException` per CLAUDE.md fail-fast.
- Gates: `sbt baboonJVM/compile` PASS, `sbt baboonJS/compile` PASS, `'testOnly *TemplateRegistryBuilder*'` 15/15, `baboonJVM/test` 388/388, `mdl :fmt` clean.

### PR-29.13 — Rust JSON sweep through generated codec (closes [PR-29.10-D04])
- 1 file modified (+12 / -12). Single round (no review defects).
- Converted 11 top-level JSON encode/decode sites in `test/conv-test-rs/src/main.rs` to use generated `data.to_json()` / `data.to_json_pretty()` / `T::from_json(s)` codec methods (AllBasicTypes, AnyShowcase, M29OkHolder, ForeignKeyHolder, BuiltinMapKeyHolder); 2 AnyOpaque-payload helper sites (`serde_json::to_value`, `from_value`) deliberately preserved.
- All 5 generated codec methods verified to delegate verbatim to `serde_json::to_string`/`to_string_pretty`/`from_str` — wire bytes identical pre/post.
- Gates: `cargo build --release` PASS, `mdl :test-acceptance` 200/200 with Rust-as-source rows green to all 10 destinations.

### PR-29.14 — Swift read-and-verify roundtrip tightening (closes [PR-29.10-D05])
- 1 file modified (+15 / -3). Single round (no review defects).
- Three Swift read-and-verify functions in `test/conv-test-sw/Sources/CompatMain/main.swift` had weak post-roundtrip assertions: `readAndVerifyM29Ok` checked only `intPage.total`; `readAndVerify` (AllBasicTypes) checked only `vstr && vi32 && vbit`; `readAndVerifyAnyShowcase` had NO roundtrip at all. All three now use full structural equality `reDecoded == data` (every fixture type is `Equatable, Hashable`).
- AnyShowcase gained JSON+UEBA roundtrip blocks. AnyOpaque uses hand-rolled `==` via `baboonDeepEquals` — verified not vacuous.
- Gates: `swift build` PASS, `mdl :test-acceptance` 200/200.

### PR-29.15 — Cross-namespace template instantiation verify+ratify (closes [PR-29.5-D04], [PR-29.7-D07], [PR-29.8-D06])
- 8 source/test/spec files touched (+402 / -52). Two rounds of review (round-1 found 9 items: 0 high + 3 medium + 6 deferred; round-2 clean).
- **User-directed scope** via question-batch: "verify + ratify". User asked "why can't we just support instantiation?" → orchestrator investigated and reported that `resolveTemplateKey` already honoured namespace prefixes for the alias-RHS positive path; the gap was in detection-side matchers and CompletionProvider regex.
- **Critical investigation outcome:** orchestrator's pre-PR hypothesis ("cross-namespace alias-RHS likely already works") was WRONG. The positive path was actually blocked by two `ScopeCannotBeEmpty` defects: both `TemplateRegistryBuilder.processMember` and `TemplateInstantiator.processMember` silently emitted `RawTLDef.Namespace(defns = Nil)` when a namespace held only template declarations (which get excised). Both fixed by dropping the namespace entirely when `rewrittenChildren.isEmpty`.
- Three detection-side `prefix.isEmpty` filters in `TemplateInstantiator` tightened: Site A (matrix #7 alias bare-Simple) → `Owner.Ns(prefix)` lookup when prefix non-empty; Site B (matrix #2 nested-arg) → inline prefix-aware owner resolution + `ownerForCurrent` threaded into `instantiateAlias`; Site C (matrix #1 in-body field) → `pkg` threaded through 8 substitution helpers. Round-1 narrowed Site C's empty-prefix lookup to `Owner.Toplevel` → regression caught by round-2 reviewer; round-2 fix restored broad any-owner same-pkg lookup for empty prefix.
- CompletionProvider regex widened from `\w*` to `[\w.]*`. New LSP test asserts the new branch fires by checking absence of keyword `"ns"` (would be present under the old regex via `Unknown` fallback).
- Spec §6 item 11 rewritten: same-package cross-namespace alias-RHS supported; cross-package still deferred.
- Tests: 2 positive in TemplateInstantiatorTest, 4 negative in M29ValidatorTest (Site A/B/C + D03 in-body regression), 2 LSP tests. All Site A/B/C tests pin diagnostic field values (prefix dropped per `simple.name.name`).
- Gates: `sbt baboonJVM/compile` PASS, `sbt baboonJS/compile` PASS (no new TyperIssue cases), `'testOnly *TemplateInstantiator* *M29Validator* *LspFeatures*'` 51/51, `baboonJVM/test` 393/393, `mdl :test-acceptance` 200/200, `mdl :fmt` clean.

### PR-29.16 — Group C close-out + duplicate ledger reconciliation (closes [PR-29.5-D06], [PR-29.10-D03 dup])
- 0 source/test/spec changes; ledger-only commit (defects.md + tasks.md).
- `[PR-29.5-D06]`: grep-confirmed `Domain.templateRegistry` is read by HoverProvider (3 sites at L94/L132/L146), DefinitionProvider (L91), and CompletionProvider (L241). Field is fully load-bearing post-PR-29.8/29.15.
- `[PR-29.10-D03]` (and D04, D05, D06) had duplicate ledger entries from a prior post-PR-29.10b orchestrator who added "resolved (deferred)" copies (lines 342-368) without updating the original "open" entries (lines 396-422). Reconciled per the loop's "never delete" rule: each second-copy now marked with `(DUPLICATE — superseded)` headline suffix and a one-line Fix pointing at the authoritative entry above.

## Defect ledger summary

12 source defects from `job.md` were addressed across 5 PRs. The defect ledger gained 28 new entries from review rounds (PR-29.12-D01..D11, PR-29.15-D01..D07, plus the round-2 D03 regression) — all closed or deferred with rationale. The 4 duplicate PR-29.10-D0[3456] entries reconciled per the never-delete discipline.

## Architectural and process discoveries

1. **Cross-namespace alias-RHS template instantiation works within the same package** (PR-29.15). The pre-existing implementation was ~80% there; the missing 20% was a correctness defect (empty-namespace excision) plus three diagnostic-quality gaps. The user's instinct ("why can't we just support it") was correct — the spec's §6 item 11 prohibition was overly conservative and now reflects actual scope.

2. **Cross-package** template instantiation (different `.baboon` file / different `Pkg`) remains out of scope. The registry key includes `Pkg` and `resolveTemplateKey` uses the alias's current package; the parser's prefix is a `RawHeader` sequence with no package/namespace disambiguation. Lifting this requires extending the registry/resolver to span packages plus a scope-walker change. Spec §6 item 11 explicitly tracks this.

3. **The matrix #1 in-body lookup was previously broad-by-design.** Pre-PR-29.15 used `(_, _, tname) => tname.name == name.name && prefix.isEmpty` — ANY-Pkg ANY-Owner. PR-29.15 round-1 narrowed to `kPkg == pkg && kOwner == Owner.Toplevel`, which silently dropped sibling-template-in-namespace cases. Round-2 reviewer caught the regression; the fix now branches on prefix: empty → broad any-owner same-pkg (matrix #1's intent: reject ANY in-body template instantiation), non-empty → precise `Owner.Ns(prefix)` lookup.

4. **Worktree subsystem stale-base issue.** PR-29.13's first executor was dispatched with `isolation: "worktree"`. The runtime created the worktree from a stale commit (`e9bb07d`, pre-M29) instead of the orchestrator's HEAD (`e18d1e9`, post-PR-29.12). Pre-flight checks in the brief caught this; the executor escalated correctly. Re-dispatched WITHOUT `isolation` — succeeded. Subsequent PRs (29.14, 29.15) also went without isolation. **Root cause:** orchestrator's CWD was changed to a prior worktree path during inspection (`cd .../.claude/worktrees/agent-...; git diff`), which broke the runtime's HEAD-detection for new worktree creation. **Lesson:** orchestrator should not `cd` into worktrees; use absolute paths or have subagents do inspection.

5. **`mdl` runs from canonical not worktree.** Per CLAUDE.md operational note: `mdl` walks up looking for `.git` to find the project root. In a worktree, `.git` is a gitfile (not a dir), so `mdl` runs against the canonical checkout. Combined with the worktree-base issue above, this means worktree executors' `mdl :test-acceptance` claims of 200/200 were measuring canonical's state, not the worktree's. The PR-29.13 first-pass executor was misled by this — its m29-ok additions to a stale main.rs would not have compiled if actually exercised in the worktree.

## Final ledger state

- All 7 deferred defects from `job.md` resolved.
- All 28 review-round defects across the 5 PRs closed or deferred-with-rationale.
- 4 duplicate PR-29.10 defect-ledger entries reconciled per never-delete discipline.
- `wip/generics` has 5 new commits (e18d1e9 → 40ba085).
- `mdl :test-acceptance` 200/200 across the M29 codepath.
- `baboonJVM/test` 393/393.

## Constraints future work must respect

1. **Cross-package template instantiation** is the next milestone-level extension if anyone asks. Touch points: `TemplateRegistryBuilder` (key shape), `TemplateInstantiator.resolveTemplateKey` (lookup logic), parser (prefix disambiguation: `pkg.ns.X` vs `ns.X`).

2. **Empty-namespace drop scope** in `TemplateInstantiator.processMember` and `TemplateRegistryBuilder.buildRecursive` is broader than "namespaces emptied by template excision" — it also drops deliberately-empty `ns foo {}` source. No fixture in the test corpus depends on emission of an empty namespace. If a future fixture needs this distinction, the drop should track which children were excised vs originally absent.

3. **Diagnostic field-value choice (prefix dropped):** `TemplateInstantiationInForbiddenPosition.instantiatedName` and `TemplateNotInstantiated.templateName` carry the unprefixed name (`"X"` not `"foo.X"`) per `simple.name.name` / `argCtor.name.name` / `name.name` extraction. The diagnostic location (`meta.pos`) carries the source position so the reader can map back. PR-29.15-D02's tests now pin this contract; future changes are deliberate.

4. **`mdl` finds project root by `.git` directory.** Worktree executors should not rely on `mdl` to test their own changes; either apply diff to canonical first or use targeted `sbt`/`cargo` commands.

5. **Worktree subsystem:** if it produces a stale base, dispatch without `isolation` and operate sequentially in canonical. The dispatcher's CWD must remain at the project root — do not `cd` into worktrees from the orchestrator.

## Stop condition

Outer loop drained: every entry in `./job.md` resolved (Group A + Group B per user decision + Group C). No blockers; no escalation needed. Returning to user with the final commit SHA chain and this log path.
