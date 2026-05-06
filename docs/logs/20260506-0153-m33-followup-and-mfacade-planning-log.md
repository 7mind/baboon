# Session log — M33 follow-up + MFACADE planning

**Date:** 2026-05-06.
**Driver:** review-loop skill (`.claude/skills/review-loop`).
**User request:** "Read my answers, analyse, ask further questions if any. If everything is clear — update plans for M32 and then fix leftover issues in M33 in /review-loop." — context: post-M33-close-out follow-up after the user answered Q10–Q14 in `docs/drafts/20260505-1830-questions-multi-version-facade-upstream.md`.

## What was done

Two pieces of orchestrator-side work, then one full review-loop iteration:

### 1. Q1–Q14 analysis + MFACADE plan doc (orchestrator commit `0ed9f974`)

Read the user's answers to Q10–Q14 in the upstream questions draft. Resolved Q11's ambiguity by reading `docs/drafts/20260424-1738-any-opaque-fields.md`: the "several forms" the user referenced are the **field-level `AnyMeta` envelope** (kinds `0x00..0x07`), already shipped; the top-level `BaboonTypeMeta` envelope is shape-identical to the proposal in current code regardless of `metaVersion` byte. So byte-16 currently carries no semantic value not already in byte-1; dropping it (per Q2 + Q10) unblocks unification.

Synthesised the answers into a MFACADE milestone plan: `docs/drafts/20260506-0000-mfacade-and-m32-plan.md`. Eight PRs (MFACADE-PR-1 … MFACADE-PR-8). M32 collapses into MFACADE-PR-1 (revert byte-16 → byte-1). Locked decisions §3.a-§3.j enumerated (byte-1 canonical; field-level AnyMeta unchanged; `null`-on-unknown-meta-version reader; numeric-`$mv` JSON; per-target-prefixed `--*-generate-domain-facade` flag; reference impl read-only and project-private per Q5).

`tasks.md` rewritten: Active brief switched to MFACADE; Milestones list reflects M33 closed + MFACADE planned + M33 follow-up in flight; M33 follow-up entry pre-allocated for the review-loop work below.

### 2. PR-33.9 — provenance-aware narrowing (commit `6c39647c`)

The M33 leftover defect `[PR-33.3-D01]` (silent-dedup of duplicate template arms `+ MyGen[i32]; + MyGen[i32]`). PR-33.7 had attempted a broad pre-`.distinct` `groupBy(_.name)` check; PR-33.8 reverted it after CI red on `pkg03.baboon` (legitimate contract-diamond producing two `f2: #i32` from `is S1`/`is S2` ContractRef paths that the historical `.distinct` was silently absorbing). The reopened defect specified provenance-aware narrowing as the future fix.

**Approach.** AST extension via sibling sealed-trait variant `RawDtoMember.TemplateArmFieldDef(field, meta)`. Sibling rather than subclass preserves all existing `case f: RawDtoMember.FieldDef` matches and constructor extractors. Only one true exhaustive-match site needed updating: `BaboonFamilyManager.filesFromDtoMember`.

`TemplateInstantiator.convertLoweredArm` Plus arm tags every lowered FieldDef as `TemplateArmFieldDef`. Recursive substitution's already-tagged carriers pass through. `BaboonTranslator.convertDto` walks `dto.members` to count template-arm origins by lowercased name; when ≥2 entries with the same name are tagged, fires `NonUniqueFields(id, dupes, dto.meta)` BEFORE `.distinct` collapses them. Fail-fast on duplicate intent: `+ Tmpl[i32]; + Tmpl[i32]; - Tmpl[i32]` fires even though `-` would cancel; concrete-type analogue `+ A; + A; - A` continues silently. Documented in scaladoc.

`^` and `-` arms intentionally don't propagate the template-arm tag — `IntersectionFields(Seq[FieldDef])` and `UnfieldDef` carriers are filter/remove operators, not adds. `checkFlatOrFail` widened to accept both `FieldDef` and `TemplateArmFieldDef` so recursive `^ Outer[T]` over inner `+ Inner[U]` works correctly.

**Review history.**
- Round 1 → 5 defects:
  - `[PR-33.9-D01]` (minor process) — orchestrator's MFACADE planning artefacts in working tree alongside the source changes. Resolved by splitting into prior commit `0ed9f974`.
  - `[PR-33.9-D02]` (minor) — two enumerated edge cases not covered by fixtures. Resolved: added `m33_bad_13_duplicate_template_arm_different_args` (asserts `NonUniqueFields` for `+ MyGen[i32]; + MyGen[str]`) + `m33_ok_concrete_plus_template_dedup_silent` (asserts silent dedup for `+ ConcreteBase; + MyGen[i32]`).
  - `[PR-33.9-D03]` (nit) — behavioural-change documentation. Resolved: 5-line scaladoc note in `BaboonTranslator.scala:333-338` explicitly documenting the pre-`removed` check timing and the template-arm-asymmetric fail-fast.
  - `[PR-33.9-D04]` (nit) — missing recursive Caret test. Resolved: `caretOverRecursiveTemplateFixture` exercising `+ Wide; ^ Outer[i32]` where `Outer[U] = + Inner[U]`. Surprise: needed `+ Wide` parent to provide intersection target (`^` is filter not add).
  - `[PR-33.9-D05]` (nit) — `tasks.md` M33 entry "eight PRs" inconsistency. Resolved: orchestrator updated to "Nine PRs landed (PR-33.1 .. PR-33.9 inc. PR-33.8 hot-fix + PR-33.9 provenance follow-up)".
- Round 2 → clean (0 new defects).

**Files in PR-33.9 commit:**
- `baboon-compiler/src/main/scala/io/septimalmind/baboon/parser/model/RawDtoMember.scala` (+20 — new variant + scaladoc).
- `baboon-compiler/src/main/scala/io/septimalmind/baboon/typer/TemplateInstantiator.scala` (+18 — Plus-arm tagging + `checkFlatOrFail` widening).
- `baboon-compiler/src/main/scala/io/septimalmind/baboon/typer/BaboonTranslator.scala` (+30 — provenance-aware check + scaladoc).
- `baboon-compiler/src/main/scala/io/septimalmind/baboon/typer/BaboonFamilyManager.scala` (+4 — exhaustive-match arm).
- `baboon-compiler/.jvm/src/test/scala/io/septimalmind/baboon/tests/M29ValidatorTest.scala` (+4 tests; row 11 flipped from `REGRESSION_GUARD` to positive-fail).
- `baboon-compiler/.jvm/src/test/scala/io/septimalmind/baboon/tests/M33StructuralTemplateInstantiationTest.scala` (+1 test).
- `tasks.md` + `defects.md` ledger updates.

**Verification.**
- `sbt baboonJVM/compile` clean.
- `sbt baboonJS/compile` clean.
- `sbt 'testOnly *M29Validator* *M33StructuralTemplateInstantiation* *TemplateInstantiator*'` 65/65 green.
- `sbt baboonJVM/test` 559/559 green (pkg03 contract-diamond preserved — was 555 pre-PR-33.9; +4 from PR-33.9's row 11 flip + 3 new tests).

## Final ledger state

**Milestones:**
- M33 — `[x]` closed (nine PRs total: PR-33.1 .. PR-33.9 incl. PR-33.8 hot-fix + PR-33.9 provenance follow-up).
- MFACADE — `[ ]` planned. Plan doc landed; eight PRs sketched; not started this session.
- M33 follow-up — `[x]` closed via PR-33.9.

**Commits this session:**
- `0ed9f974` MFACADE: planning — Q10-Q14 answers + plan doc.
- `6c39647c` PR-33.9 — provenance-aware narrowing.
- (this log) — orchestrator session log.

**Deferred items:**
- None new this session. The two M33 deferreds (`[PR-33.3-D01]` and `[PR-33.4-D01]`) are both `resolved` now.

**Next session work:**
- MFACADE-PR-1: byte-16 → byte-1 unification revert across all 11 runtimes + test fixture sync.
- MFACADE-PR-2 .. MFACADE-PR-8 per the plan doc.

## Loop discipline notes

- **D01 scope-split was an orchestrator side-effect of starting the loop with a dirty tree.** When the orchestrator carries pre-loop work (here, MFACADE planning) and then the inner loop's executor adds source changes, the round-1 reviewer correctly flagged the conflation. Standard pattern: commit any orchestrator pre-loop work BEFORE invoking the loop, so the loop's PR has clean scope.
- **Sibling sealed-trait variant beats defaulted-param case-class extension** for AST nodes whose existing 2-arg `unapply` extractors are widely deployed. PR-33.9's executor surfaced this by inspection — saved 5+ pattern-match call-site updates.
- **Cross-build coverage matters.** `sbt baboonJS/compile` was clean throughout — no JS-side cascade from the new `RawDtoMember` variant because the JS-side typer pipeline doesn't pattern-match on `RawDtoMember`. Worth re-confirming on every AST extension.

---

*End of session log.*
