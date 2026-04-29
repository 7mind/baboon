# Session log — M21 Round-2 upstream defects

**Date:** 2026-04-29
**Branch:** `wip/anytype`
**Commits:** `3fd4ea8`..`b4c3f91` (4 PRs).

## Original request

> Before we proceed to the new features, let's triage and address more feedback from the users: BAB-R01 (Rust wiring snake_case), BAB-R02 (Rust BaboonGenerated trait impls), BAB-G01 (typer crash on absent `data in {}`), BAB-J01 (Scala JSON Map non-determinism). triage, add into ledgers, fix in /review-loop.

## Triage outcome

Planning subagent investigated all 4 defects, produced a plan at `docs/drafts/20260429-1205-m21-round2-defects-plan.md` with per-defect file:line citations, root-cause analysis, and a cross-backend audit for BAB-J01 (Sc + Sw confirmed; CS / Jv conditional). Determined all 4 PRs touch disjoint files; could run in parallel.

## PRs

| PR | Scope | Status | Commit |
|---|---|---|---|
| PR-45 | BAB-R01: Rust wiring fn snake_case (4 emit sites + 4 caller stubs) | committed | `3fd4ea8` |
| PR-46 | BAB-R02: `impl BaboonGenerated` + markers for all Rust types; new `RsDomainTreeTools` | committed | `357bc1e` |
| PR-47 | BAB-G01: typer desugars absent `data in` at `ScopeBuilder`; `ServiceMultipleInputs` defensive | committed | `9ad9d2f` |
| PR-48 | BAB-J01: `ScJsonCodecGenerator` sorts Map keys by `_._1.toString` before iter | committed | `b4c3f91` |

PR-45 and PR-46 each went through one execution + one adversarial review. PR-47 took the planner's design to `ScopeBuilder` (not `convertService` — the planner-suggested location ran AFTER scope registration and produced `UnexpectedNonBuiltin(in)`). PR-48 was Sc-only after the audit revealed Swift's conv-test driver hides the same defect class via `.sortedKeys` (BAB-S0x filed as a follow-up).

## Process notes — parallel-execution collision

- **Parallel-execution gambit failed.** Four executor subagents launched in parallel against the same working tree collided via `git stash` operations. One agent's stash captured another agent's in-progress work; new untracked files (like `RsDomainTreeTools.scala` for PR-46) were lost (stashes don't include untracked files by default). HEAD migrated mid-session for some agents (PR-47 reported edits being silently rolled back between sbt invocations).
- **Recovery:** orchestrator dropped both polluted stashes (`stash@{0}`/`stash@{1}`), confirmed all 4 PRs' work in working tree post-collision (PR-46's `RsDomainTreeTools.scala` was successfully restored despite the earlier loss because PR-46's executor re-applied multiple times), ran clean integration build (`sbt clean baboonJVM/compile`, `sbt baboonJVM/test` 190/190, full `mdl` matrix per-language + cross-language compat), all green. Then 4 reviewers in parallel (read-only — no collision risk). Committed each PR separately by file path.
- **PR-45 caller-search gap.** The PR-45 executor's caller search (`grep -rn "invoke_json_\|invoke_ueba_" baboon-compiler/src/main/{scala,resources}/`) missed hand-written test stub overlays at `test/rs-stub-{either,outcome,result}-overlay/tests/wiring_tests.rs` and `test/services/rs/src/server.rs`. CI surfaced via `mdl :test-rs-wiring-either` (E0432 unresolved-import errors). Orchestrator hot-fixed via `sed` over the 4 affected test files; wiring matrix re-passed. Lesson: caller searches must include `test/` + per-stub overlays for any wiring/service emit changes.
- **Lesson — never run parallel executors on a shared working tree.** Plan to use git worktrees (`git worktree add`) for genuine parallelism, OR serialize executors. Reviewers (read-only) remain safe to parallelise.

## Defects logged + closeout

All 4 M21 defects flipped to resolved in `defects.md` "M21 — Round-2 upstream defects: closeout" section. Plus 5 follow-up entries filed:

- **[BAB-S0x]** Swift JSON codec emits hash-ordered `Dictionary` at codec layer; conv-test driver hides via `.sortedKeys`. End-user Swift code without the opt-in still hits the bug. Open / minor.
- **[BAB-C04]** C# `BaboonTools.WriteMap` iterates user-supplied collection order. Same root cause as BAB-J01. Open / minor.
- **[BAB-J03]** Java JSON codec iterates `entrySet()` in user-supplied order. Same root cause. Open / minor.
- **[PR-45-D01]** `toSnakeCase` digit edge case: `Foo2Bar` → `foo2bar` not `foo2_bar`. No current fixture exercises it. Open / nit.
- **[PR-47-D01]** No negative fixture exercising `ServiceMultipleInputs`. Symbolic correctness via `ServiceMultipleOutputs` mirror; empirical coverage gap. Open / nit.

## Verification

- `sbt baboonJVM/compile` clean.
- `sbt baboonJVM/test` 190/190 pass (3 canceled match M16/M17 baseline — `RTCodecTest` orchestration prerequisites).
- `mdl :build :test-gen-{regular,wrapped}-adt` PASS.
- `mdl :test-{rust,scala,cs,typescript,kotlin,java,dart,swift,python}-{regular,wrapped}` — all 18 PASS (full per-backend matrix).
- `mdl :test-rs-wiring-{either,outcome,result}` PASS (was failing on the pre-PR-45 stub overlay).
- `mdl :test-sc-wiring-{either,...}` PASS.
- `mdl :test-gen-compat-{cs,scala,rust,typescript,kotlin,java,dart,swift,python}` — all 9 PASS (cross-language byte-identity holds despite Sc now sorting map keys; readers tolerate any order).
- Cross-stack parity strict-string-compare for PR-46: Rust `baboon_type_identifier()` matches Swift `baboonTypeIdentifier` for `T6_D2`, `T5_A1`+branches, `Clash`, service In/Out, v1_0_0 vs latest.

## Final ledger state

All M21 milestone PRs `[x]`. Five follow-up defects on the books (all minor / nit). No blockers for M18 dispatch.

## What's next

Branch `wip/anytype` is in a coherent state. M21 closes the 4 round-2 upstream defects; lessons captured for future parallel-execution decisions. Ready for M18 (BAB-A01 identifier types) when user calls it. Sequencing per existing decision log: M18 → M19 → M20.
