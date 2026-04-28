# Session log — Upstream defect triage and fixes (M14)

**Date:** 2026-04-28
**Branch:** `wip/anytype`
**Commits:** `36e59d2`..`89ae1df` (5 PRs).

## Original request

> Review list of defects in `@baboon-upstream-defects-clean.md`, figure out which are real defects (maybe all of them but some may be junk) and fix in /review-loop.

The defects file (added at 2026-04-26) listed 14 entries spanning Scala (S01, S02, S04), TypeScript (T01, T02), Kotlin (K01..K05), Swift (W01..W04), and CLI (C01).

## Triage outcome

Planning subagent classified the 14 entries via current-code evidence:
- **11 REAL** (S01, S02, T01, T02, K01..K04, W01..W04)
- **2 misdiagnosed** (S04 — flags carry `@HelpMessage`; C01 — flags still present)
- **1 unverified** (K05 — no locatable reproducer)

Plan landed at `docs/drafts/20260428-1100-upstream-defects-plan.md`.

## PRs

| PR | Scope | Status | Commit |
|---|---|---|---|
| PR-27 | Scala BAB-S01, BAB-S02 + ns-scoped service test fixture | committed | `36e59d2` |
| PR-28 | Kotlin BAB-K01..K04 + parallel BAB-S02 fix in genEnumBodies | committed | `ea3889d` |
| PR-29 | TypeScript BAB-T01a/b/c, T02 + cross-backend hotfixes for parallel S02 (C#, Java, Python, Dart) + fixture rollback (`T_NsPascal` removed) | committed | `5d42f27` |
| PR-30 | Swift BAB-W01..W04 (mayThrow propagation, throws-marked readers, BaboonCodecError) | committed | `94b8d0a` |
| PR-31 | Doc closure: status flips on `baboon-upstream-defects-clean.md` for all 14 entries | committed | `89ae1df` |

Each PR went through one execution + one adversarial review round; PR-30 needed a second fix round after the reviewer caught W02 not actually fixed and W01 incomplete on the UEBA-Any path.

## Defects logged (review-loop accounting)

26 PR-NN-DMM entries appended to `defects.md`:
- PR-27-D01..D06
- PR-28-D01..D06
- PR-29-D01..D06
- PR-30-D01..D07

Most are accepted/deferred (cosmetic, scope-creep, latent). Five required fix passes within the PR:
- PR-27-D04 (trailing newline)
- PR-29-D04 (TS executor's `throw` always fired due to missing `return;`)
- PR-29-D05 (test stub vs new BaboonBinCodec interface)
- PR-30-D01 (UEBA `Any` decoder still had `try try`)
- PR-30-D02/D03 (W02 paren-wrap fix; reverted broken `v` substitution)

## Follow-ups surfaced (out of scope for M14)

- **PR-29-D02 — cross-language enum wire-format spec is undefined**: non-Pascal-case enum members serialize differently in Pascal-convention backends (Scala/Kotlin/C#/Java/Python/Rust emit capitalized via `.toString()`/`.name`) vs raw-allowed backends (TS/Dart/Swift). Removed `T_NsPascal { cafe; bar_pub }` from cross-language fixture as a workaround. Permanent fix needs a design doc.
- **PR-29-D03 — TS internal divergence latent**: `TsUEBACodecGenerator` capitalizes; `TsDefnTranslator` raw. Doesn't fire post-fixture-removal. Reopen if a non-Pascal-case enum is added back.
- **PR-27-D01 — Scala `CopyEnumByName` conversion**: pre-existing bug for renamed non-Pascal-case enum members across versions. Conversion translator emits raw-keyed mappings while `from.toString` returns capitalized.
- **PR-28-D04 — Kotlin self-cast**: pre-existing `($cName as $cName).decodeBranch(...)` somehow survives `-Werror` `USELESS_CAST` policy.
- **PR-30-D07 — Dart-from-Swift JSON fragile-skip**: `T6_D{1,2}` cross-language test silently skipped because the swift JSON fixture file isn't produced. Resurfaces if the pipeline is fixed.

## Verification

Across PR-27..PR-30: `mdl :build :test-gen-regular-adt :test-gen-wrapped-adt + per-language regular/wrapped + test-gen-compat-*` — green for Scala, Kotlin, C#, Java, Python, TypeScript, Rust, Swift. Dart `:test-dart-regular` fails on T6_D{1,2} cross-language JSON-from-swift, but reproducible on `main` HEAD via `git stash` — pre-existing, deferred to PR-30-D07 follow-up.

Swift warning audit on regenerated `target/test-regular/sw-stub`: `try`-on-non-throwing 2398 → 0; forced-downcast warnings 64 → 0; implicit `Any?`→`Any` coercion 0 → 0 (briefly 69 during a botched W02 attempt, fixed in second pass).

## Process notes

- The pkg03 fixture's `T_NsPascal` enum exposed a parallel-S02 bug class in 4 additional backends (C#, Java, Python, Dart) that PR-27's adversarial review missed because it only ran `:test-scala-regular`. Lesson: when adding fixtures to the cross-language model dir, ALWAYS run the full per-language matrix.
- Swift's static type checker doesn't propagate runtime `nil` guards. Forced casts in optional context need parenthesisation per Swift's diagnostic.
- TS hand-written test stubs in `test/ts-stub/src/runtime-tests/` aren't typechecked by vitest — interface drift surfaces as runtime errors instead of compile errors. Worth enabling typecheck on stubs in a follow-up.
- Two of three Swift defects (W02 + UEBA-Any side of W01) were missed in the first execution pass and only surfaced under hostile review. Confirms the value of the per-PR adversarial review step.
