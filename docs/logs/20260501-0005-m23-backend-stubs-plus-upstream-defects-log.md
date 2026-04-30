# M23 closeout — backend stubs unblock + upstream defects cleanup

**Date:** 2026-04-30 / 2026-05-01 (rolled over)
**Branch:** `wip/ids-and-adts`
**Final commit:** `f9bee03` (PR-69, closes M23)
**Predecessor:** M20 closeout (`329fcdb` session log)

## Original user request

> Now let's think what we can do about the failures - all 5 must be fixed, we need completely green suite. After that we will need to address all open issues (real issues, not feature requests like adding generics) in @baboon-upstream-defects.md . So, work in /review-loop , plan, implement

Two phases:
- Phase 1: fix 5 PRE-EXISTING `:test-{lang}-regular` failures exposed by PR-64's verification (Rust, Scala, Kotlin JVM, Kotlin KMP, Swift)
- Phase 2: close all real open defects in `baboon-upstream-defects.md` (excluding wontfix BAB-S04, BAB-C01 + feature request BAB-S06)

## Outcome: 5 PRs, both phases delivered

| PR | Commit | Scope |
|---|---|---|
| PR-65 | d0c5540 | Rust serde-template generic-name hygiene + nested-wrapper-chain recursion fix |
| PR-66 | f8e3cb0 | Scala JSON map-key Foreign-Custom encoder + decoder symmetry + enum-key encoder fix |
| PR-67 | 6b65c98 | Kotlin foreign-mapping for `java.lang.*` JVM aliases (covers JVM + KMP) |
| PR-68 | f91b2c1 | Swift SPM test-target per-module split + Swift JSON Foreign-map-key codegen fix |
| PR-69 | f9bee03 | Doc-only: close out resolved items in `baboon-upstream-defects.md` |

## Final headline verification

```
mdl --seq :build :test-gen-regular-adt \
  :test-cs-regular :test-scala-regular :test-rust-regular \
  :test-typescript-regular :test-kotlin-regular :test-kotlin-kmp-regular \
  :test-java-regular :test-dart-regular :test-swift-regular :test-python-regular
```

**ALL 11 ACTIONS PASS.** All 9 backend stubs (10 if counting Kotlin-KMP separately) compile and run their tests cleanly.

## Loop discipline applied per PR

Each PR followed the inner loop: executor (subagent) → adversarial reviewer (subagent) → fix subagent(s) for found defects → re-review until clean → orchestrator commits. Per-PR review rounds:

- PR-65: 1 round (1 deferred latent foreign/enum gap noted as PR-65-D01).
- PR-66: 2 rounds. Round-1 caught a critical asymmetry (encoder fix produced wire form decoder couldn't read back). Round-2 added symmetric decoder + missing round-trip test in test/sc-stub.
- PR-67: 1 round (zero defects found).
- PR-68: 2 rounds. Round-1 reviewed the SPM split as scope-correct and ACCEPT-WITH-CONDITIONS, flagging that `MyOkM19Foreign` exclusion masked a real Swift codegen defect (parallel to PR-66 Scala). Per user mandate "completely green", round-2 fixed `SwJsonCodecGenerator` (decodeKey signature change for `try`-tracking + Custom-foreign direct-cast), re-included MyOkM19Foreign, added round-trip test, cleaned stale `.gitignore`.
- PR-69: 0 rounds (pure docs).

## Triage discoveries (Phase 2)

Investigation revealed that 5 of 7 Phase 2 items were already resolved by prior milestone work (M14, M19, M20). Only BAB-G02 still had residue (Rust D-shadow), which Phase 1's PR-65 closed. Confirmed by reading commit history + generated output:

- BAB-S01 / BAB-K01 (ns-scoped wiring wrong package) → fixed by PR-27 / PR-28 (M14).
- BAB-S03 (user-DTO map keys) → fixed by M19 (PR-59 validator + PR-60 codegen + PR-61 cross-language fixture).
- BAB-S05 (cross-ADT branch reuse) → exactly the M20 BAB-A03 work.
- BAB-K02 (named-arg-supertype warning) / BAB-K05 (redundant conversion call) → verified clean post-PR-67 by inspecting all 4 kotlin compile outputs.
- BAB-G02 (Rust runtime regression at v0.0.186-SNAPSHOT) → ~200 runtime errors + ~175 generated-file errors had been resolved by ongoing runtime work between `8788c832` and current HEAD; only the D-shadow residue remained, closed by PR-65.

## Cross-cutting design decisions

1. **Rust generic-name hygiene** (PR-65): `__De` and `__M` chosen for serde template generics. While `_`-prefixed identifiers ARE allowed by the parser, the template scope contains no user-controllable raw identifiers, so naming is functionally safe across all plausible user fixtures.

2. **Decoder symmetry imperative** (PR-66 round-2): a partial fix that breaks the round-trip property is worse than no fix — the existing test suite passing was non-evidence about the property the milestone exists to deliver. Round-2 added the missing test that pins the invariant.

3. **Kotlin JVM-alias allowlist** (PR-67): conservative 9-entry rewrite (`java.lang.{String,Boolean,Byte,Short,Integer,Long,Float,Double,Character}` → `kotlin.{String,Boolean,Byte,Short,Int,Long,Float,Double,Char}`) avoids over-rewriting hypothetical user `java.lang.X` mappings to non-aliased types.

4. **Swift SPM per-module split** (PR-68): scope-correct fix is to declare per-module `.testTarget`s. Hand-written runtime tests (3 files) moved to dedicated `Tests/RuntimeTests/` to keep codegen-owned `Tests/BaboonTests/` pristine for rsync. Helper file `CrossLanguageFixturePath.swift` copied into each per-module subdir via `.mdl/defs/tests.md` post-codegen step (idempotent).

5. **Bundling Swift codegen fix into PR-68** (PR-68 round-2): user mandate "completely green" trumped strict-scope-discipline. The same defect class as PR-66 (Scala) needed treatment in Swift; rather than splitting into PR-68a/PR-68b, the Swift codegen fix was added to PR-68 directly. Resulting PR is bigger but reviewable in one pass.

## Defects deferred (track for cross-backend hygiene PRs)

These were surfaced during M23 review but consciously not fixed in this milestone:

- **PR-65-D01** — Rust `isUserMapKeyEligibleDto` doesn't handle `Typedef.Foreign` or `Typedef.Enum`. Latent: no Rust JSON round-trip exercises wrapper-around-foreign at present.
- **PR-66 latent** — Scala decoder for non-String Custom foreigns (e.g. `ObscureInt → java.lang.Integer`) preserved old throw-stub path with TODO comment. Unreachable in current fixtures since validator gates only the active wrapper-around-Custom-String case.
- **PR-68-D02** — Swift codegen test-emission filter (`hasForeignType`) excludes wrapper-around-foreign fixtures. Hand-written round-trip test covers the meaningful path; lifting the filter would emit fatalError-shimmed test paths.
- **Swift `as!` non-String Custom foreign as direct map key** — would trap at runtime since JSON map keys are always strings. Validator currently permits such models. Cross-backend issue, latent across all backends.
- **`anyThr` vs `mapExpr` key-`try` asymmetry** in `SwJsonCodecGenerator` — currently all `decodeKey` branches return `false`; if a future branch returns `true`, the closure body would miss a `try` even though the outer expression marks throwing.
- **Rust `test/conv-test-rs/src/lib.rs` hand-curated** — each new top-level runtime module needs manual `#[path]` entry. Future hygiene PR could auto-route.
- **TS direct-builtin tuple-array form** (PR-60-D08, pre-existing).
- **Cross-backend signed-`+` rejection gap** (PR-57d-D03).

## Forward-looking work

After M23, the backlog of upstream-defects items in `baboon-upstream-defects.md` is:
- 4 newly closed items in the M14-M23 summary section.
- Pre-M14 closures still in place.
- 0 open REAL items (excluding wontfix and feature requests).

Latent cross-backend hygiene work tracked above. Each is small enough to land as a single-PR follow-up when it becomes blocking.

## Final ledger state

`tasks.md` — milestones M21, M22, M18, M19, M20, M23 all `[x]`. PR-65..PR-69 all `[x]` with rich Completed entries.

`defects.md` — 80+ defect entries across PR-54..PR-68 sections. Most resolved; ~25 deferred with documented rationale and pointers to latent-issue tracking.

`baboon-upstream-defects.md` — 7 items moved to fixed/resolved with PR references; new summary section appended.

## Session-end disposition

Per loop discipline outer-loop O5: ledger has no more planned/in-progress tasks for the user's M23 ask. Phase 1 verified completely green via the headline 11-action mdl run. Phase 2 closed. Returning to user.
