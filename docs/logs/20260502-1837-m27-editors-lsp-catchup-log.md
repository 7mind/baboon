# M27 closeout — editors + LSP catch-up + 3 CI hot-fixes

**Date:** 2026-05-02 (continuing same-day from M26 close).
**Branch:** `wip/ids-and-adts`.
**Final commit this session:** `713eaec` (PR-26.2-D01 teardown fix).
**Predecessor:** M26 complete log `docs/logs/20260502-1537-m26-deferred-hygiene-drain-log.md`.

## Original user request

> we've added many syntactical changes, we need to support all the updates in editors/lsp server and make sure their tests are green

Plus follow-up:
> while background tasks work, analyze this windows failure: <URL> and this acceptance failure: <URL>

## Outcome

**6 M27 PRs shipped** (all 23 tree-sitter parse failures closed) + **3 CI hot-fixes** for issues surfaced during the close gate.

| PR / Fix | Commit | Closes |
|---|---|---|
| PR-27.1 | `6b00ac9` (main) + `09b63a9` (zed submodule) | 14 fixtures (M18 `id` group) |
| PR-27.2 | `784c84d` (main) + `564c7e3` (zed submodule) | 12 fixtures (M20 ADT-inheritance group) + Group C `pkg02.baboon` |
| PR-27.3 | (no-op) | residual sweep — clean post-27.2 |
| PR-27.4 | `f7add73` (main) + `c15c45c` (vscode submodule) | VSCode `id` keyword highlighting |
| PR-27.5 | `f7add73` (main) + `f8bd2ff` (intellij submodule) | IntelliJ extension dormant-status doc |
| PR-27.6 | `ca925a6` | LSP exhaustive-match audit (verification only — 46/46 TyperIssue + 25/25 VerificationIssue handled in 3 sites) |
| Hot-fix PR-26.5-D01 | `08d83ac` | Windows-CI m26 fixture Swift-skip (mirror PR-57e-D02 pattern at lines 474, 483) |
| Hot-fix Dart-acceptance | `3bd2372` | `baboon_identifier_repr.dart` missing from acceptance harness move-list (root cause: `test/acceptance/run_acceptance.py` + `run_service_acceptance.py` had stale runtime-file tuple — NOT a codegen bug; codegen was correct all along) |
| Hot-fix PR-26.2-D01 | `713eaec` | `KeyCodecHostLastWinsSpec` teardown — leaked `PrefixCodec("A:")` to global `FStr_KeyCodecHost` singleton, breaking `ForeignMapKeyRoundTripSpec` round-trip in same JVM |

## M27 final verification gates

- `mdl :test-editors` — PASS (all .baboon fixtures parse cleanly).
- `mdl --seq :build :test` — **PASS** (82 actions; previously failed at `test-sc-wiring-result` due to PR-26.2-D01 — now green).
- `mdl :test-acceptance` — PASS.
- `mdl :test-service-acceptance` — PASS.

## Adversarial review

PR-27.1, 27.2, 27.3 (Zed grammar) — verified by direct fixture-parse evidence (per-PR repro tables match plan). PR-27.4 (VSCode), 27.5 (IntelliJ doc), 27.6 (LSP audit) — read-only or doc-only; verified by inspection. CI hot-fixes verified by re-running the failing gate.

## CI hot-fixes — 3 issues surfaced post-M27 close attempt

The user shared 3 CI failures during the close gate. All 3 are fixed in-band:

1. **Windows `test-manual-scala` failure** — PR-26.5 m26 fixture cross-language tests at `Test_CrossLanguageCompat.scala:474, :483` hard-asserted Swift output exists. Same defect class as PR-57e-D02 (commit `9b83494`); Windows CI lacks Swift toolchain. Fix: replicate `assume(...)` pattern. (`08d83ac`)

2. **Linux `test-acceptance` failure** — generated `point_id.dart:71` referenced `BaboonIdRepr.parseFieldName(...)` but file appeared to lack the import. **Initial hypothesis was wrong.** Codegen was correct; the actual bug was `test/acceptance/run_acceptance.py` + `run_service_acceptance.py` post-codegen relocation step had a stale tuple of runtime files to move — `baboon_identifier_repr.dart` was not in the list, so it stayed under `lib/generated/` while imports pointed to `package:baboon_runtime/...`. Fix: add the missing filename to both move tuples. (`3bd2372`)

3. **Linux `mdl --seq :build :test` failure (post local-CI re-run)** — `ForeignMapKeyRoundTripSpec.scala:33` failed: `Holder(Map(ItemKey("A:alpha") ...))` — the `"A:"` prefix indicated PR-26.2's `KeyCodecHostLastWinsSpec` registered `PrefixCodec("A:")` to the **global** `FStr_KeyCodecHost` singleton without a teardown. SBT pools JVM across specs; subsequent specs saw the leaked impl. Fix: `BeforeAndAfter` mixin + `after { register(IdentityCodec) }` in 9 sibling spec files (Scala + 8 backends). (`713eaec`)

## Cross-cutting architectural notes locked

- **M27-N01 — Grammar maintenance discipline.** Any new top-level keyword in `Keywords.scala` must trigger updates to (a) Zed `grammar.js` + corpus + regen `parser.c`, (b) VSCode `baboon.tmLanguage.json` keyword pattern + `directives.import` end-stop set, (c) IntelliJ `Baboon.bnf` (skip while M27-N02 is dormant), (d) LSP exhaustive-match sites if a new `TyperIssue`/`VerificationIssue` case lands. Recommended automation: append `dep action.test-editors` to `# action: test`. Closed in PR-27.1/27.2/27.4.
- **M27-N02 — IntelliJ extension status: dormant.** Rename-from-Idealingua incomplete: live generated parser still under `idealinguaintellij` package, no `gradle test`, not in `mdl :ci`. Documented in PR-27.5.
- **M27-N03 — CLAUDE.md LSP path drift.** CLAUDE.md references `baboon-compiler/.jvm/src/main/scala/...lsp/features/`, but canonical paths are under `baboon-compiler/src/main/scala/...lsp/{features,state}/` (post-restructure). Future doc-fix candidate.

## Lessons for M28+

1. **Test-state isolation discipline for global singletons.** PR-26.2 added regression specs that mutate a JVM-shared singleton without teardown. SBT pools JVM across specs; this leaks. Future PRs that mutate `FStr_KeyCodecHost` (or any KeyCodec Host) MUST include `after`/`@AfterEach` teardown that restores a fresh identity impl. Codify in M27-N01 follow-up or M28-N01.
2. **Acceptance harness ≠ codegen.** The Dart `BaboonIdRepr` failure looked like a codegen import-collector bug; the actual bug was 800ms upstream in the post-codegen Python relocation step. **Hypothesis-falsification discipline is the load-bearing skill** — reading the actually-emitted file (it had the import) flipped the hypothesis. Future post-CI triage should always check the *artifact* the test consumed, not just the codegen path.
3. **`test-manual-python` not in `:test`.** Carried forward from M26-N03; verify in M27 still applicable. The `:test-acceptance` harness exercises Dart end-to-end, which is how this surfaced; `mdl --seq :build :test` alone wouldn't have caught the move-list defect.
4. **Worktree `.git`-as-file friction recurred.** Several worktree dispatches landed on stale base (`8788c83`) and required `git reset --hard wip/ids-and-adts`. Documented previously; pattern stable. Workarounds: (a) prefer main checkout for serial PRs that touch regen-heavy files, (b) accept reset in worktrees as a normal precondition.

## Forward-tracked items

- **Stretch CI hardening:** add `dep action.test-editors` to `# action: test` so editor regressions surface pre-commit (M27-N01 recommendation).
- **CLAUDE.md path doc-fix** (M27-N03).

## Session-end disposition

M27 closed per /review-loop O5: ledger has no more planned/in-progress work for M27. CI-equivalent close gate (`:test-acceptance` + `:test-service-acceptance` + full `:test`) PASS. Returning to user with:

- 6 PRs + 3 CI hot-fixes = 9 commits.
- All 23 originally-failing tree-sitter parses closed.
- 3 newly-surfaced defects fixed (PR-26.5-D01, PR-CI-D01 acceptance harness, PR-26.2-D01 spec teardown).
- 3 cross-cutting invariants locked (M27-N01..N03).
- CI gates green; ready for next big feature work.

User goal "make sure their tests are green" achieved.
