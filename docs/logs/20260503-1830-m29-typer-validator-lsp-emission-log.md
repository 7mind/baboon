# M29 — typer monomorphisation, validator, LSP polish, emission smoke (PR-29.1a + 29.5–29.9)

**Date:** 2026-05-03 (continuation of same-day session).
**Branch:** `wip/generics`.
**Final commits this session:** `6faeb1a`, `68ae0a1`, `ee1ea60`, `ff7d636`, `193b2f3`, plus PR-29.9 commit and this session log.
**Predecessor log:** `docs/logs/20260503-1257-m29-spec-and-typer-foundations-log.md` (PR-29P.4 + 29.1–29.4).

## Original user request (continuation)

> Resume M29: tighten spec §4/§6 per the agreed reframing (drop misleading Option A/B callout; matrix #5 is a special case of matrix #1 / decision #3; add explicit out-of-scope note that the typer does NOT structurally rewrite in-body instantiations to existing aliases). Flip tasks.md PR-29.5 from [!] to [ ]. Resume outer loop into PR-29.5 (typer monomorphisation via AST substitution; alias-id canonical).

Followed by mid-session pause request:
> I need to restart the server, make a break once you finish 29.9.

## Outcome

**Six PRs landed (PR-29.1a + 29.5..29.9).** M29 is now ~80% complete: spec is locked, parser handles template head + alias instantiation, typer monomorphisation works end-to-end, validator catches all 9 negative-test matrix cases, LSP polish ships, and **all 9 codegen backends produce valid concrete code for monomorphised templates with no synthetic `Page<T>`/`Envelope<T,E>` identifiers anywhere** — locked decision #4 verified end-to-end.

| PR | Commit | Closes |
|---|---|---|
| PR-29.1a | `6faeb1a` | Spec §4 / §6 reframing — Option A/B callout dropped; matrix #5 reframed as special case of matrix #1 / decision #3; §6 entry for "no auto-rewrite of in-body template instantiations to existing aliases". 14 D01–D14 defects round 1 (no — the previous PR was PR-29.1; PR-29.1a's review found 6 minors + 1 nit, all fixed). |
| PR-29.5 | `68ae0a1` | The architectural meat. `TemplateInstantiator` substitutes alias-RHS template instantiations into ordinary `RawTLDef.{DTO|ADT|Contract|Service}` keyed by the alias's name (locked decision #4). Two new TyperIssue cases (`TemplateArityMismatch`, `TemplateInstantiationInBody`) with 3-site exhaustive-match update. Drops `RawDto.typeParams = Nil` defaults per `[PR-29.2-D05]`. 17 new tests; full JVM 360/360. Adversarial review found 2 majors + 6 minors + 2 nits — D01/D02/D03/D05/D07 fixed inline; D04/D06/D08/D09/D10 deferred. |
| PR-29.6 | `ee1ea60` | Cycle diagnostic verification. Plan revised mid-PR: original hypothesis (typer toposort catches matrix #9 via `CircularInheritance`) was wrong — `hardDepsOfRawDefn` only walks inheritance edges, not field references. Final disposition: matrix #9 is caught by `BaboonValidator.checkLoops` as `VerificationIssue.ReferentialCyclesFound` (post-typer), reusing the existing diagnostic per CLAUDE.md §4 simplicity. New `M29TemplateCycleValidatorTest.scala` locks it. |
| PR-29.7 | `ff7d636` | Validator + matrix-#7/#8/D03 diagnostics. Three new TyperIssue cases (`TemplateNotInstantiated` matrix #7, `NotATemplate` matrix #8, `TemplateBodyCarriesDerived` for `[PR-29.5-D03]` body-side `derived` validation at registry-build time). `TemplateArityMismatch.ownerName` renamed to `aliasName` per `[PR-29.5-D09]`. Per-matrix decision table now complete. Removed `registry.isEmpty` short-circuit so the pass runs for template-free models too (needed for matrix #8 detection). New `M29ValidatorTest.scala` with 11 tests after round-2 fixes. Adversarial review: 4 minors + 3 nits; D01-D04 fixed inline. |
| PR-29.8 | `193b2f3` | LSP polish for templates. HoverProvider, DefinitionProvider, CompletionProvider extended to consult `Domain.templateRegistry`. HoverProvider implements spec §2.3 type-param shadowing via cursor-position-based enclosing-template detection. New LSP tests (6 new on top of 9 baseline = 15) backed by `m29-lsp/m29-lsp.baboon` fixture. **Tree-sitter editor grammar reverted from this PR** per `[PR-29.8-D01]` — the changes live in a 3-level submodule chain requiring user-authorised pointer bumps; deferred to a dedicated tree-sitter PR. Adversarial review: 3 majors fixed inline (D01 tree-sitter revert, D02 LSP tests, D04 type-param shadowing). |
| PR-29.9 | (this session) | Cross-language emission smoke fixture `m29-ok/m29.baboon`. **All 9 codegen backends produce valid concrete code with NO synthetic `Page<…>`/`Envelope<…>` identifiers** — the architectural bet from PR-29.5 (codegen requires zero changes; templates excised before codegen sees them) holds end-to-end. Single-round clean — no defects. |

## Architectural bet — verified end-to-end

The M29 plan §3.6 made one load-bearing architectural bet: **`TypeRef` is unchanged; templates are a raw-AST-layer rewrite that runs BEFORE `BaboonTranslator.convertTpe`. After substitution the raw AST is structurally indistinguishable from a hand-written non-template equivalent. Codegen never sees a template.**

PR-29.9 verified this end-to-end: every one of the 9 codegen backends emits `IntPage` as an ordinary concrete DTO (C# record, Scala case class, Rust struct, etc.) with no generic syntax anywhere. `IntStrEnvelope` becomes a concrete sealed-trait/record/enum hierarchy in each backend. Zero codegen-side changes were required across the entire milestone.

## Open question dissolved (mid-session)

The PR-29.1a reframing closed an ambiguity that was originally framed as Option A vs Option B for "container-mediated self-reference within a template body" (`data Tree[T] { children: lst[Tree[T]] }`). Per the user clarification, the inner `Tree[T]` is field-position template instantiation — already forbidden by locked decision #3 / matrix #1, independent of any self-reference reading. Spec §4 was reworked into §4.1-§4.4 making this explicit; §6 was extended with an out-of-scope note that the typer does NOT structurally rewrite in-body instantiations to match existing aliases (the `type K = Tree[i32]` rescue is rejected as unconventional / fragile / order-dependent).

## Final ledger state

`tasks.md` Milestone M29 — PR breakdown:
- PR-29.1 [x] (spec doc, prior session)
- PR-29.1a [x] (spec reframing)
- PR-29.2 [x] (parser type-param head, prior session)
- PR-29.3 [x] (parser alias instantiation + alias-side `derived`, prior session)
- PR-29.4 [x] (typer template registry, prior session)
- PR-29.5 [x] (typer monomorphisation)
- PR-29.6 [x] (cycle diagnostic)
- PR-29.7 [x] (validator + matrix diagnostics)
- PR-29.8 [x] (LSP polish; tree-sitter deferred)
- PR-29.9 [x] (cross-language emission smoke)
- PR-29.10 [ ] (acceptance fixture — JSON+UEBA roundtrip across all 9 backends)
- PR-29.11 [ ] (docs catch-up)

**Two PRs remain to close M29:** PR-29.10 (cross-language wire-format acceptance) and PR-29.11 (docs catch-up).

## Deferred work (recorded in `defects.md`)

- **Tree-sitter editor grammar** (`[PR-29.8-D01]`) — defer to dedicated PR with user-authorised submodule pointer bumps. The grammar.js change + regenerated parser + corpus tests are conceptually small but require coordination across `editors/baboon-zed` and `grammars/baboon` submodule histories.
- `[PR-29.5-D04]` namespaced template in-body matcher — deferred to PR-29.7's namespace handling (deferred again at PR-29.7 close-out per `[PR-29.7-D07]`).
- `[PR-29.5-D06]` `Domain.templateRegistry` write-only field — defer to PR-29.11 cleanup or LSP follow-up.
- `[PR-29.5-D09]` `TemplateArityMismatch.ownerName` rename → done in PR-29.7.
- `[PR-29.5-D10]` case-arm formatting — `mdl :fmt` will reflow.
- `[PR-29.7-D05]` `TemplateInstantiationInBody` misnomer for matrix #2 — defer to a polish PR.
- `[PR-29.7-D06]` hardcoded builtin-collection set — defer with TODO.
- `[PR-29.7-D07]` cross-namespace template detection — defer (no spec'd cross-namespace template support yet).
- `[PR-29.8-D05]` `renderTemplateInfo` parameter shadowing — cosmetic.
- `[PR-29.8-D06]` CompletionProvider regex misses qualified prefixes — defer with TODO.

## Process gaps surfaced this session

1. **Submodule chain non-shipment (PR-29.8-D01).** The PR-29.8 executor's tree-sitter changes lived in working-tree state across a 3-level submodule chain (outer → `editors/baboon-zed` → `grammars/baboon`). `mdl :test-editors` passed against the dirty working tree, masking the fact that the outer-repo submodule pointer hadn't moved. Reviewer caught it. Operational lesson: **after editing anywhere under a submodule, verify outer-repo `git diff --stat HEAD` shows the submodule pointer moved**, not just that the per-submodule status is clean.

2. **False "no test infrastructure" claim (PR-29.8-D02).** The PR-29.8 executor reported "No LSP test infrastructure exists" while `LspFeaturesTest.scala` is 296 lines and runs hover/definition/completion against a real compiled family. Operational lesson: **verify negative claims about infrastructure absence by directly listing the relevant directories**, not by inference.

3. **Plan-doc claims invalidated mid-execution (PR-29.6).** Round-1 executor for PR-29.6 escalated correctly: the plan's claim that the existing typer toposort catches matrix #9 turned out to be wrong (toposort only walks inheritance edges, not field references). Round-2 dispatched with revised plan (validator-side detection via `ReferentialCyclesFound`). Operational lesson: **plan claims about existing infrastructure behaviour must be verified before being baked into a PR brief**.

4. **`mdl --seq :build :test` skipped on PR-29.5.** Per `[PR-29.5-D08]` precedent (and PR-29.4-D06 before it), per-target gates were deemed sufficient for typer-pipeline-rewriting PRs. The full close gate is recommended once before M29 branch merge, not gated per-PR. PR-29.9 ran the full per-backend codegen matrix, providing strong coverage.

## Session-end disposition

Six commits landed on `wip/generics` this session (`6faeb1a`, `68ae0a1`, `ee1ea60`, `ff7d636`, `193b2f3`, plus PR-29.9). Local close gates green per per-PR verification + PR-29.9's full per-backend matrix. Pausing per user request ("I need to restart the server, make a break once you finish 29.9"). Two PRs remain to close M29: PR-29.10 (cross-language wire-format acceptance) and PR-29.11 (docs catch-up).
