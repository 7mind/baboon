# M27 — Editors + LSP Catch-Up to M18-M26 Syntax

**Date:** 2026-05-02 (continuing same-day from M26 close).
**Goal:** `mdl :test-editors` exits 0 (gates `:ci`); VSCode highlighting recognises `id` keyword; IntelliJ status documented (dormant, recommended); LSP exhaustive-match audit confirms M18-M26 issue handling. Out of scope: changing fixtures, spec language, or active IntelliJ rebuild.

## Failing fixture inventory (re-run 2026-05-02 against `editors/baboon-zed/grammars/baboon/grammar.js` HEAD)

**23 fixtures fail.** Three syntactic gaps account for all of them.

### Group A — `id` keyword (M18) — 14 fixtures
- `baboon/identifier-ok/identifiers.baboon`
- `baboon/m19-ok/{id-with-contracts, multi-field-id, single-primitive-id}.baboon`
- `baboon-fixtures-bad/m19-bad/id-as-key-without-json-derived.baboon`
- `baboon-fixtures-bad/identifier-bad/{id-cycle, id-with-{adt,any,collection,contract,data,enum,f128,f32,float,foreign,map,opt,set}-ref}.baboon`

Required syntax: `[root] id <Name> [: derivations]? { (is Type | <field>)* }`. Body shares structure with `data` body — fields plus optional `is` contract refs.

### Group B — ADT branch inheritance ops `+ X` / `- X` / `^ X` / `- X.Branch` (M20) — 12 fixtures
- `baboon/m20-ok/{intersect, multi-intersect, simple-include, chained-include, namespace-qualified, desugar-equiv, desugar-equiv-sugared, include-and-exclude}.baboon`
- `baboon/m20-was-propagation/{v1, v2}.baboon`
- `baboon-fixtures-bad/m20-bad/{cycle, empty-intersection, name-collision, non-adt-include}.baboon`

`grammar.js` `_adt_member` (line 79-80) currently allows `data_def | contract_def | contract_ref` only. M20 sugar adds `+ X` / `- X` / `^ X` / `- X.Branch` inside `adt { … }`. The `parent_def` / `unparent_def` / `intersection_def` rules already exist (lines 224-229) — they just need to be added to `_adt_member`'s alternation.

### Group C — Combined — 1 fixture
- `test/conv-test/pkg02.baboon` — uses both `id` and `adt`; closes when Groups A+B land.

## PR breakdown

### PR-27.1 — Zed grammar: M18 `id` definitions

**Scope:**
- Add `id_def` rule, parallel to `data_def`:
  ```js
  id_def: ($) =>
    seq(
      optional("root"),
      "id",
      field("name", $.identifier),
      optional($.derivations),
      optional($.contract_refs),
      "{",
      repeat($._dto_member),
      "}"
    ),
  ```
- Append `$.id_def` to `_definition` (line 41).
- Run `tree-sitter generate` — commit regenerated `src/parser.c` + `src/grammar.json` + `src/node-types.json`.
- New corpus file `editors/baboon-zed/grammars/baboon/test/corpus/identifiers.txt` covering: bare `id`, `root id`, `id` with `derived[json], derived[ueba]`, `id` with `is HasMeta`, multi-field `id`. Mirror existing `basics.txt` shape.

**Acceptance:** all 14 Group-A fixtures parse without ERROR; new corpus tests pass.

**Risk:** `id` as field name (e.g. `id: uid`) — grammar uses `word: $ => $.identifier` so keyword-vs-identifier disambiguation is positional. fastparse upstream parser does the same. No conflict expected.

**Complexity:** Trivial. ~12 LOC + regenerated parser + corpus.

**Dependencies:** None.

---

### PR-27.2 — Zed grammar: M20 ADT branch inheritance ops

**Scope:**
- Extend `_adt_member` (line 79-80):
  ```js
  _adt_member: ($) =>
    choice($.data_def, $.contract_def, $.contract_ref,
           $.parent_def, $.unparent_def, $.intersection_def),
  ```
- `parent_def`/`unparent_def`/`intersection_def` already accept `$.type_ref` (line 224-229) — covers scoped (`+ sub.MyAdt`) and dotted-branch (`- ErrorAtom.SomeBranch`).
- Regenerate `parser.c`.
- New corpus `editors/baboon-zed/grammars/baboon/test/corpus/m20-adt.txt`: `+ X`, `^ X`, `- X.Branch`, mixed `+ X / - X.Branch`, namespace-qualified `+ sub.X`, multiple `+`/`^`.

**Acceptance:** all 12 Group-B fixtures + 1 Group-C fixture parse without ERROR.

**Risk:** LR(1) conflict if `+`/`-`/`^` token start sets overlap with another rule. Low — they're already valid entry tokens in `parent_def`/etc. If `tree-sitter generate` reports a conflict, fall back to splitting into `_adt_parent`/`_adt_unparent`/`_adt_intersection` variants.

**Complexity:** Trivial. 1-line widening + corpus.

**Dependencies:** **Must follow PR-27.1** (single grammar.js + regen `parser.c`; merge order is path-dependent).

---

### PR-27.3 — Zed grammar: residual sweep

**Scope:** Re-run `bash test/editors/test-tree-sitter.sh .` after PR-27.2. If any failure remains, identify and patch. Pre-emptive findings: M19 multi-field-id keys, M21-M26 added no top-level grammar; `any[domain:this]` already supported. Likely no-op.

**Acceptance:** `bash test/editors/test-tree-sitter.sh .` exits 0.

**Complexity:** Trivial-Small (likely no-op).

**Dependencies:** PR-27.2.

---

### PR-27.4 — VSCode TextMate keyword additions

**Scope:** Single-file edit `editors/baboon-vscode/syntaxes/baboon.tmLanguage.json`:
1. **`declarations` block (line 121)**: add `id` to keyword alternation `(data|struct|contract|adt)` → `(data|struct|contract|adt|id)`.
2. **`directives` block (line 423)**: extend `import` end-regex stop-keyword set to include `id`.
3. **No M20 change needed** — `instructions` block at line 296 already matches `(\+|-|\^)\s*([A-Z_a-z]…)` covering `+ X`/`- X`/`^ X` uniformly inside any block.

**Acceptance:** Manual visual check on `identifiers.baboon` — `id` keyword highlights. (No automated VSCode grammar test exists.)

**Complexity:** Trivial. ~3 char-level edits.

**Dependencies:** None. Parallel-eligible.

---

### PR-27.5 — IntelliJ: dormant-status README (recommended no-op)

**Scope:** Recommendation: **declare IntelliJ extension dormant**. Evidence:
- Live generated parser still under `idealinguaintellij` package (`src/main/gen/io/septimalmind/idealingua/...`).
- Active Kotlin source still in `idealingua` namespace.
- No `generateParser`/`generateLexer` Grammar-Kit task; no `gradle test`.
- Not in `mdl :ci`.
- Recent git log: zero contributor activity in M18-M26 wave.

Add 1 paragraph to `editors/baboon-intellij/README.md` titled "Maintenance status" stating the extension is in dormant rename-from-Idealingua state and grammar parity is best-effort. PRs touching `editors/baboon-intellij/**` should not block on grammar parity with the compiler.

**Acceptance:** README paragraph committed. No code change.

**Complexity:** Trivial.

**Dependencies:** None. Parallel-eligible.

**User decision:** confirm dormant. If active-maintenance is preferred, scope a separate PR-27.5b (~1 day): complete `idealingua → baboon` rename, wire Grammar-Kit, regenerate from `Baboon.bnf`, add `id` keyword + adt-member ops.

---

### PR-27.6 — LSP exhaustive-match audit (verification only)

**Scope:** Verify all M18-M26 `TyperIssue`/`VerificationIssue` cases are handled in 3 match sites:
- `baboon-compiler/src/main/scala/io/septimalmind/baboon/lsp/features/DiagnosticsProvider.scala`
- `baboon-compiler/src/main/scala/io/septimalmind/baboon/lsp/state/WorkspaceState.scala`
- `baboon-compiler/.js/src/main/scala/io/septimalmind/baboon/BaboonJS.scala`

Pre-audit (from planner inspection): all M18-M20 cases already handled. Expected: zero diff.

**Acceptance:** `mdl :build` succeeds (Scala 3 exhaustive-match warnings would surface gaps); existing LSP tests pass.

**Complexity:** Trivial-Small (no-op verification; small fix-up if surprise gap surfaces).

**Dependencies:** None. Parallel-eligible.

---

## Reproduction summary

| PR | Pre-fix | Post-fix |
|---|---|---|
| 27.1 | `tree-sitter parse identifiers.baboon` shows `(ERROR …)` | clean parse |
| 27.2 | `tree-sitter parse m20-ok/intersect.baboon` shows `(ERROR …)` | clean parse |
| 27.3 | `bash test/editors/test-tree-sitter.sh .` exit 1 | exit 0 |
| 27.4 | `id` plain text in VSCode | `id` highlighted |
| 27.5 | n/a | README paragraph |
| 27.6 | n/a | `mdl :build` succeeds |

## Parallelisation plan

**Wave 1 (serial):** PR-27.1 → PR-27.2 → PR-27.3 in main checkout sequential commits. Cannot fan out — each commit regenerates `src/parser.c` (~150 KB), and merge-conflict resolution on regen output is impractical.

**Wave 2 (parallel):** PR-27.4 + PR-27.5 + PR-27.6 in worktrees. Disjoint files. Can run concurrently with Wave 1.

## Verification gates

**Per-PR:** in the table above.

**Milestone close:**
1. `mdl :test-editors` exits 0.
2. `mdl :ci` exits 0 (includes `:test-editors`).
3. `mdl :build :test` continues to pass.
4. `mdl :test-acceptance` + `mdl :test-service-acceptance` PASS.

## Cross-cutting architectural notes (for `tasks.md`)

- **M27-N01 — Grammar maintenance discipline.** Any new top-level keyword in `Keywords.scala` must trigger updates to (a) Zed `grammar.js` + corpus + regen `parser.c`, (b) VSCode `baboon.tmLanguage.json` keyword pattern + `directives.import` stop-keyword set, (c) IntelliJ `Baboon.bnf` (when M27-N02 status flips to "active"; today: no-op), (d) LSP exhaustive-match sites if a new `TyperIssue`/`VerificationIssue` case lands. Recommended automation: add `dep action.test-editors` to `# action: test` so editor regressions surface pre-commit.
- **M27-N02 — IntelliJ extension status: dormant** (rename-from-Idealingua incomplete). Not built/tested in `:ci`. Closed in PR-27.5.
