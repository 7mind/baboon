# M26 closeout — deferred-hygiene drain (8 of 9 PRs shipped, PR-26.9 deferred to M27)

**Date:** 2026-05-02 (continuing same-day from M25 closeout + post-M25 Windows-CI fix)
**Branch:** `wip/ids-and-adts`
**Final commit this session:** `fd63457` (PR-26.2 — last commit landed)
**Predecessors:** M25 complete log `docs/logs/20260502-0513-m25-complete-log.md`; CI-fix commit `9b83494`

## Original user request

> Can we clean all that too before we proceed further? In one large pass?

Plus immediately preceding:
> I need a clean build - we still have a backlog of new important features.

"That" = the 9 deferred-hygiene items M25 closeout surfaced as `resolved (deferred …)` qualifiers + cross-cutting notes M25-N02/N03.

## Outcome

**8 of 9 planned PRs shipped + 1 round-2 follow-up + 1 stale-CI fix = 10 commits.** PR-26.9 (Python canonical-shape Shape A) attempted, reverted, deferred to M27.

| PR | Commit | Closes |
|---|---|---|
| (pre-M26) Windows-CI fix | `9b83494` | PR-57e-D02 (latent from M18) |
| PR-26.8 (zero-code) | `a37bec0` | PR-17-D05 + M25-N02 (re-disposition) |
| PR-26.1 | `1c27318` | PR-I.3-N04 |
| PR-26.6 | `816677f` | PR-I.1b-N01 |
| PR-26.3 | `ee8e39e` | PR-I-D04 |
| PR-26.7 | `e12c50b` + r2 `e98fb67` | PR-I.1d-N03 |
| PR-26.5 | `d28256b` | PR-G-D01 |
| PR-26.4 | `041fef3` | PR-I.2-D02 + PR-68-D02 |
| PR-26.2 | `fd63457` | PR-I.3-N01 + PR-I.1a-D09 |

## M26 final verification gates (CI-equivalent)

- `mdl --seq :build :test` — 82 actions PASS (full per-language matrix).
- `mdl :test-acceptance` — PASS.
- `mdl :test-service-acceptance` — PASS.

The M25 process miss — close gate did not run acceptance targets — is corrected here.

## Adversarial review

- **Wave 1 (5 PRs)**: 3 CLEAN (26.8, 26.1, 26.3); 2 NEEDS FIXES (26.6 commit-msg + UEBA-gap forward note; 26.7 scope deviation on Dart UEBA non-stringy).
- **PR-26.7 round-2** addressed the scope-deviation finding by routing non-stringy with `runtimeMapping` through underlying-primitive UEBA. Reviewer's suggestion of TS-style `asTsTypeDerefForeign` was incorrect (TS suppresses UEBA codec entirely when foreigns present; Dart can't due to m24 stringy customs). The fix subagent found the actual right pattern (`Typedef.Foreign.runtimeMapping`).
- **Wave 2 (3 PRs)**: All shipped via cherry-pick from worktrees with conflict resolution (defects.md trailing-section conflict).
- **Wave 3 (PR-26.9)**: Reverted before review.

## Items re-sized vs the brief

- **#8 Java conversion-registry parity** — re-sized from "mirror Dart fix" to **zero-code reclassification** (PR-26.8). Java's registry is already pair-keyed; the brief's premise was a misread.
- **#5 cross-language non-string map-keys (PR-26.5)** — covered 5 of 8 planned types. Dropped u64 (Scala latent), f64 + tso (cross-backend canonicalization divergences). Tracked at M26-N02 as follow-up.
- **#9 Python canonical-shape (PR-26.9)** — attempted, reverted, deferred to M27. Pydantic's `model_validate(value, mode='json')` does NOT auto-JSON-parse strings (only `model_validate_json(str)` does). The executor missed caller sites still passing strings; 26 errors surfaced. Highest-risk PR per plan; needs more careful audit.

## Cross-cutting architectural notes locked (M26)

- **M26-N01** — KeyCodec Host concurrency = last-wins (canonical, all 9 backends). Closed by PR-26.2.
- **M26-N02** — `m26-builtin-map-keys.baboon` locks 5 of 8 non-string builtin map-key types. Closed by PR-26.5.
- **M26-N03** — Python `AnyOpaqueJson.json` canonical shape Shape A. **Deferred to M27.**
- **M26-N04** — Java multi-step conversion chain is a forward-feature, NOT hygiene. Closed by PR-26.8.

## Lessons for M27

1. **Pydantic `model_validate` does NOT JSON-parse strings.** Shape-A alignment requires either (a) caller-site audit ensuring ALL callers pass parsed values, OR (b) string-vs-parsed dispatch inside the generated codec body. Plan PR-26.9 → PR-27.X with that distinction baked in.
2. **Worktree friction recurred again.** 4 of 9 PR worktrees were on stale base `8788c83` (pre-M22) and required reset or re-dispatch in main checkout. Cause: mdl resolves `.git`-as-directory only. Future: either land all PRs in main (no worktree isolation) or accept cherry-pick + manual-merge friction.
3. **`:test` does NOT include `:test-manual-python`.** Pre-existing PyConversionTranslator latent (`AbstractConversion.__init__()` missing args) surfaces only via `:test-manual-python`, which is NOT in `:test` and NOT in CI. Filed as future hygiene to fix in M27 alongside Python canonical-shape work.
4. **CI close-gate must include acceptance targets.** M25 missed this; M26 corrects. Future milestones MUST run `:test-acceptance` + `:test-service-acceptance` before declaring close.

## Forward-tracked items (NOT M26 scope)

- **M26-N02 follow-ups:** u64/f64/tso non-string map-key fixture coverage; Swift `JSONSerialization.sortedKeys` divergence on outer DTO keys.
- **M26-N03 (the M27 PR):** Python canonical-shape Shape A. Plan: caller-site audit + string-vs-parsed dispatch inside generated codec.
- **PyConversionTranslator latent:** `AbstractConversion.__init__()` requires `from_type, to_type` but generated `Convert__...` classes don't pass them. Pre-existing, surfaces only in `:test-manual-python` (not in `:test` or CI). Fix in M27.
- **JVM-family UEBA Foreign-KeyCodec hook integration:** auto-generated `<F>_UEBACodec` throws across Java/Kotlin/KMP/Scala. PR-26.6 tests are JSON-only. Forward-tracked.

## Session-end disposition

M26 closed per /review-loop O5: ledger has no more planned/in-progress work for M26 (PR-26.9 explicitly deferred). Final CI-equivalent gates PASS. Returning to user with:

- 8 PRs shipped + 1 round-2 + 1 stale-CI fix = 10 commits.
- 8 of 9 originally-planned hygiene items closed.
- 1 deferred to M27 with documented cause (pydantic API misconception).
- 4 cross-cutting invariants locked (M26-N01..N04).
- CI-equivalent gates green (corrects M25 process miss).

User goal "I need a clean build" achieved (modulo Windows CI which depends on PR-57e-D02 fix already merged). Backlog items can proceed.
