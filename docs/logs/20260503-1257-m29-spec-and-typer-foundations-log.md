# M29 — spec, parser, typer foundations (PR-29P.4 + PR-29.1..29.4)

**Date:** 2026-05-03 (continuing same-day from M29-prep close-out).
**Branch:** `wip/generics`.
**Final commits this session:** `8c369e8`, `f2cd5bf`, `5383db9`, `3965598`, `3d4dc3f` plus this session log.
**Predecessor log:** M29-prep `docs/logs/20260503-0040-m29-prep-ci-hardening-log.md`.

## Original user request

> The CI is green. Read your ledgers, fix any remaining follow-ups then proceed to the templates feature. Work in /review-loop

## Outcome

**Five commits shipped, all M29-prep follow-ups + the M29 parser/typer foundations landed.** The next planned PR (PR-29.5 — typer monomorphisation via AST substitution) is **blocked on a user design decision** that surfaced during PR-29.1 spec planning; `tasks.md` flips PR-29.5 to `[!]`.

| PR | Commit | Closes |
|---|---|---|
| PR-29P.4 (M29-prep follow-ups) | `8c369e8` | `[PR-29P.1-D02]` (RTCodecTest non-UTC tso coverage), `[PR-29P.1-D03]` (RandomJsonGenerator tso/tsu split). M28-N01 wire-form invariant now hard-guarded across three layers (generated codecs PR-28.3, runtime codec PR-29P.1, example generator PR-29P.4). |
| PR-29.1 | `f2cd5bf` | M29 user-facing spec doc `docs/spec/generics.md` (658 lines). All 9 negative-test matrix items in §2.5 in matrix order. Adversarial review round 1: 14 defects (5 major + 6 minor + 3 nit) — most majors were spec syntax inaccuracies the author invented vs real Baboon grammar (`derived` chained-colon, `adt =`/`contract =`, `;` field separator). Round-2 fix subagent applied all 14; round-2 review: clean. |
| PR-29.2 | `5383db9` | Parser: type-param head on `data`/`adt`/`contract`/`service` declarations. `RawDto`/`RawAdt`/`RawContract`/`RawService` gain `typeParams: List[RawTypeName] = Nil`. Identifier `id Foo[T] {…}` rejected at parse time per spec §6.7. New `TemplateHeadParserTest.scala` (18 tests). |
| PR-29.3 | `3965598` | Parser: `type Y = X[Foo]` alias instantiation RHS verification + `RawAlias.derived` extension. Scope-expanded inline beyond plan-doc PR-29.3 brief to close `[PR-29.1-D02]` (spec ships syntax parser cannot consume); documented as `[PR-29.3-D01]` orchestrator-deliberate scope expansion. |
| PR-29.4 | `3d4dc3f` | Typer: `TemplateRegistry` + `TemplateRegistryBuilder`; templates excised from downstream member list, registered by `(Pkg, Owner, TypeName)`. First M29 `TyperIssue` (`DuplicateTypeParam`) with **3-site exhaustive-match update** (DiagnosticsProvider, WorkspaceState, **BaboonJS** — the JS-side cross-build canary CLAUDE.md highlights as the historical PR-47/M21 regression vector). 8 new tests including registry-contents inspection. |

## Ground-truth verification

Per-PR close gates ran during inner loops; final consolidated state:
- `sbt baboonJVM/compile` — PASS
- `sbt baboonJS/compile` — **PASS** (CLAUDE.md cross-build canary; clean across all 5 PRs)
- `sbt baboonJVM/test` — PASS (339/339; 4 new typer tests + 27 new parser tests + 308 baseline = +31 net new tests across the session)
- `sbt +compile` — PASS (cross-build)
- `mdl --seq :build :test` — skipped per `[PR-29.4-D06]` (per-target gates plus `+compile` cover the surface; reviewer agreed for parser/typer-early-excision PRs that don't change emission paths)

No fixture md5 churn (no `baboon-compiler/src/test/resources/baboon/` files modified anywhere in the session — `m29-ok/` lands in PR-29.10).

## M29 architectural shape (verified during planning)

The M29 plan (`docs/drafts/20260503-2210-m29-generics-plan.md`, 1023 lines, produced by O1 planning subagent) made one load-bearing architectural bet that all 9 codegen backends require **zero changes**:

> Templates live in a typer-side registry. Every reachable instantiation `type Y = X[Foo]` is materialised by a typer-early raw-AST substitution pass into an ordinary `Typedef.User` keyed by the alias's `TypeId.User`. After substitution the raw AST is structurally indistinguishable from a hand-written non-template equivalent. Codegen never sees a template.

This bet is now load-bearing for:
- PR-29.4 (already landed) — template excision before the typer's main passes; `Domain.templateRegistry` carries the bodies.
- PR-29.5 (next, blocked) — `TemplateInstantiator` consumes `Domain.templateRegistry`, performs raw-AST substitution at alias-resolution time.
- PR-29.6..29.11 — validator, diagnostics, fixtures, docs.

`TypeRef` is unchanged. Decision recorded in plan §3.6 as Option K (keep TypeRef as-is).

## Open question blocking PR-29.5 (escalated to user)

Locked decision #2 (`tasks.md`): "Self-reference is forbidden. Only DAG-shaped template-and-instantiation graphs are valid."

The matrix-#5 case is direct field-position recursion: `data X[T] { rec: X[T] }` — unambiguously forbidden.

The ambiguous case is **container-mediated recursion**: `data Tree[T] { children: lst[Tree[T]] }`. Two readings of decision #2 produce different `TemplateSelfReference` validator behaviour:

- **Option A (strict, default chosen by spec §4):** forbid all template-self-reference, direct or container-mediated. Literal reading of "DAG-shaped only". Users needing recursive container structures hand-write a non-template recursive type (which `BaboonValidator.checkLoops` already accepts via the `terminatesLoop` rule for non-template types — verified in `validator/BaboonValidator.scala:69-89` and exercised by the `pkg0/pkg03.baboon:53-61` `RecTest1`/`RecTest2` fixtures cited in spec §4).
- **Option B (permissive):** allow container-mediated self-reference within a template body, deferring cycle detection to the same `checkLoops` pass after monomorphisation. PR-29.7's `TemplateSelfReference` detector limits to direct field-position recursion only.

PR-29.1 spec §4 currently defaults to Option A and includes a flagged design-note callout: *"DESIGN NOTE: this strict reading is the default chosen by the M29 plan; revisit if a use case demands container-mediated recursion within a template body."*

PR-29.5 (monomorphisation) and PR-29.7 (validator) implementations differ between the two options; switching after PR-29.5 lands forces churn in both. **Resolving this question before PR-29.5 dispatches is the cheapest path.**

## Process gaps surfaced this session

1. **PR-29.1 spec author invented syntax that doesn't match real Baboon grammar.** Round-1 reviewer caught five major spec-syntax inaccuracies (`: derived[X] : derived[Y]` chained-colon, `adt … =`, `contract … =`, `;` field separator, `RawAlias` having no `derived` field). Operational lesson: spec authors must verify surface syntax against actual fixtures and grammar source (`pkg0/*.baboon`, `DefMeta.scala`, `DefAdt.scala`, `DefContract.scala`), not from memory or analogy with adjacent languages.

2. **PR-29.4 round-1 reviewer caught a load-bearing dead computation.** `TyperOutput.templateRegistry` was correctly built and threaded through `runTyper`'s yield, but DROPPED at the `process` boundary because `Domain` didn't carry the field. PR-29.5 would have had nothing to consume. Adding `Domain.templateRegistry: TemplateRegistry = TemplateRegistry.empty` was the fix. Operational lesson: when adding a typer-pipeline carrier, trace the value end-to-end through every projection and `yield` boundary; a value that "exists in `runTyper`" is not the same as "exists in `Domain`".

3. **PR-29.3 scope deliberately expanded beyond plan brief.** Plan said test-only verification; shipped also added `RawAlias.derived` extension. Driven by `[PR-29.1-D02]` discovery during spec review. Documented as `[PR-29.3-D01]` orchestrator-deliberate. Future loop iterations should not mis-read PR-29.3's diff against the original plan brief without consulting `[PR-29.3-D01]`.

4. **`namespace` vs `ns` keyword.** Brief for PR-29.4 round-2 fix mentioned `namespace foo {…}`; correct keyword per `Keywords.scala` is `ns`. Caught by fix subagent. Operational lesson: brief authors verify keyword spelling against `parser/defns/base/Keywords.scala`.

5. **`mdl --seq :build :test` skipped on PR-29.4.** Per `[PR-29.4-D06]`, per-target gates (`baboonJVM/compile`, `baboonJS/compile`, `+compile`, full JVM test 339/339 incl. emission tests) cover the surface for typer-early-excision PRs that don't change emission paths. Reviewer agreed. The full close gate should run before any PR that touches a codegen backend or runtime resource (PR-29.9/29.10).

## Forward-tracked items

### Blocked on user (PR-29.5 dispatch precondition)

- **Self-reference reading**: Option A (strict, plan default) vs Option B (permissive). See "Open question" above. The user's answer determines PR-29.5's substitution-pass behaviour and PR-29.7's `TemplateSelfReference` validator scope. Spec §4 may need an edit if Option B is chosen.

### Deferred defects (recorded inline in `defects.md`)

- `[PR-29.2-D03]` rename `typeParams` → `typeArgs` for field-position field — defer to PR-29.7 if rename now risks PR-29.3 churn (no longer a risk; PR-29.7 owns).
- `[PR-29.2-D04]` strengthen `id Foo[T]` rejection test — defer to PR-29.7 (validator + diagnostics polish).
- `[PR-29.2-D05]` drop `RawDto.typeParams = Nil` default once typer plumbs it through — defer to PR-29.5.
- `[PR-29.3-D03]` `Set` discards source-order for `: derived[A], derived[B]` — defer; matches sibling-type convention.
- `[PR-29.4-D03]` first-only-duplicate emission for `data X[T,T,U,U]` — defer; sibling-validator polish, not blocking.
- `[PR-29.4-D04]` registry-key collisions silently dropped via `toMap` — defer; PR-29.7 verifies `ScopeBuilder.NonUniqueScope` covers it.
- `[PR-29.4-D05]` dead `nsPath` parameter in recursion — defer; code-cleanliness.

### Carry-overs from M28 still open (archived)

- f64 cross-backend canonicalisation (PR-28.2-D01).
- Generator-wide map-iteration sort-key (PR-28.4-D02; not the cause of CI-01).
- Python tso microsecond emission (PR-28.4-D03).
- Python `LEDataOutputStream.write_datetime` kind-byte semantics (PR-28.4-D04).

## Session-end disposition

Outer loop terminates per `/review-loop` I-6 (blocked on user input). Returning to user with:

- Five commits on `wip/generics`: `8c369e8`, `f2cd5bf`, `5383db9`, `3965598`, `3d4dc3f`, plus this session log.
- Local close gates green per per-PR verification; CI gates pending push.
- One blocking question for the user on Option A vs B (above).
- Six minor/nit defects ledgered as deferred-with-rationale.
- One scope-expansion documented as orchestrator-deliberate (`[PR-29.3-D01]`).
- M29 plan doc seeded at `docs/drafts/20260503-2210-m29-generics-plan.md` (1023 lines) as the per-milestone PR-execution authority.

Awaiting user authorisation on the self-reference question before PR-29.5 dispatches. Once resolved, PR-29.5..29.11 can land in sequence per the plan.
