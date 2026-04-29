# Session log — M22 follow-up cleanup

**Date:** 2026-04-29
**Branch:** `wip/anytype`
**Commits:** `fa3ab70`..`5203014` (5 PRs).

## Original request

> Fix all the followups before proceeding to the new features.

The 5 follow-ups filed during M21 closeout:
- PR-45-D01 — `toSnakeCase` digit edge case
- PR-47-D01 — no negative fixture for `ServiceMultipleInputs`
- BAB-C04 — C# JSON map iteration order user-dependent
- BAB-J03 — Java JSON map iteration order user-dependent
- BAB-S0x — Swift codec emits hash-ordered Dictionary, conv-test driver hides it

## Triage

All 5 follow-ups well-scoped per their existing defect entries with clear suggested fixes. Decided on serialised execution (no parallel agents on shared working tree per M21 lesson). Five small mechanical PRs.

## PRs

| PR | Scope | Status | Commit |
|---|---|---|---|
| PR-52 | PR-45-D01: `toSnakeCase` digit boundary (`Foo2Bar` → `foo2_bar`) + 6-case `RsToSnakeCaseTest` | committed | `fa3ab70` |
| PR-53 | PR-47-D01: new `ServiceFrontEndTest` covering both duplicate-input rejection paths | committed | `10ab011` |
| PR-50 | BAB-C04: `BaboonTools.WriteMap` sorts by `e.Key?.ToString()` ordinal before emit | committed | `1a221df` |
| PR-51 | BAB-J03: `JvJsonCodecGenerator` map emit copies entrySet to ArrayList, sorts, iterates | committed | `5c57378` |
| PR-49 | BAB-S0x: new `encodeToJsonData` / `encodeToJsonString` helpers on `BaboonJsonCodecBase<T>` | committed | `5203014` |

Each PR was a single execution + verification cycle (no adversarial review subagents — changes mechanical and well-specified by the defect entries). Orchestrator did the work directly given the small scope; full local verification before each commit.

## Discoveries during execution

- **PR-53 dual-rejection-path discovery.** Writing the negative test for `ServiceMultipleInputs` initially failed because **inline `data in {} data in {}` fixtures are rejected at scope-build time with `NonUniqueScope`** (each inline struct registers `ScopeName(in)`; duplicates collide before `convertService` runs). The `ServiceMultipleInputs` defensive check from PR-47 only fires for the **ref form** `in = X in = Y`. Both rejection paths are now locked in via two parallel test cases. The defensive check is genuinely useful (it's the canonical path for the ref form), just not for the inline form the M21 plan originally envisaged.
- **PR-49 design choice space.** Swift `Dictionary` is hash-based and `JSONSerialization.data(withJSONObject:)` re-hashes; sorting at codec emit (option b) is moot. The compiler-emitted write sites (service wiring, generated tests) already pass `[.sortedKeys, .fragmentsAllowed]`. The remaining gap was end-user code calling `codec.encode(...)` and serialising the returned `Any` themselves. Fixed by adding always-deterministic helpers `encodeToJsonData` / `encodeToJsonString` on `BaboonJsonCodecBase<T>` — non-breaking, opt-in to the canonical path. The existing `Any`-returning `encode` API is preserved.
- **PR-50 / PR-51 cross-language sort-key determinism.** Both helpers sort by `String.valueOf(...)` / `e.Key?.ToString()` (null-safe) using ordinal/lexicographic comparison. Mirrors Scala's `sortBy(_._1.toString)` from PR-48. JSON wire format is order-insensitive so cross-language `:test-gen-compat-{cs,java}` interop unaffected.

## Verification

- `sbt baboonJVM/test` — 200/201 pass; the 1 failure is `RTCodecTest` (orchestration-prerequisite; needs `test-cs-regular` to populate `target/test-regular/target/cs/json-default` first; pre-existing, documented across multiple prior commits, unrelated to M22).
- `mdl :test-{rust,scala,cs,typescript,kotlin,java,dart,swift,python}-regular` — all 9 PASS.
- `mdl :test-cs-wiring-either`, `:test-rs-wiring-either`, `:test-rs-wiring-outcome`, `:test-rs-wiring-result`, `:test-sc-wiring-either`, `:test-ts-wiring-either` — PASS where covered (regression check on PR-50/PR-51/PR-52 affected paths).
- `mdl :test-gen-compat-{cs,java,scala,swift,typescript,kotlin,dart,rust,python}` — full 9-backend cross-language interop matrix verified for PR-50/PR-51/PR-49 affected paths.
- New tests: `RsToSnakeCaseTest` (6/6 pass), `ServiceFrontEndTest` (2/2 pass).

## Final ledger state

All M22 milestone PRs `[x]`. All 5 follow-ups resolved. Defect ledger has zero open items (zero major, zero minor, zero nit).

## What's next

Branch `wip/anytype` is in a coherent state. M22 closes the M21 follow-up backlog. **No open defects.** Ready for M18 (BAB-A01 identifier types) when user calls it. Sequencing per existing decision log: M18 → M19 → M20.
