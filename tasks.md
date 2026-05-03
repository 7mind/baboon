# Baboon — Task Ledger

Authoritative ledger of planned and completed work. Governing brief: pre-M29 CI hardening blocks BAB-A04 (user-defined generics) per user 2026-05-03 ("we will only proceed to generics once CI is green").

> **Predecessor ledgers (frozen):** `docs/archive/20260503-bab-any-anyopaque-ledgers/{tasks,defects}.md`
> Cover M1–M28 (`any`/AnyOpaque, identifiers, ADT inheritance, JSON/UEBA codecs, map-key encoding, wire-form canonicalisation). Locked invariants there remain authoritative.

Status: `[ ]` planned · `[~]` in progress · `[x]` done · `[!]` blocked

---

## Milestones (high-level)

- [x] **M29-prep** — Pre-M29 CI hardening. Two defects fixed: CI-01 (BaboonRuntimeCodec tso UTC-zero `+00:00`↔`Z`), CI-02 (acceptance-tests sbt resolution under `--ignore-environment`). Both CI jobs green on `wip/ids-and-adts` (verified by user 2026-05-03; branch merged into `wip/generics`).
- [x] **M29-prep follow-ups** — PR-29P.4 landed both deferred items (D02 non-UTC tso coverage, D03 RandomJsonGenerator tso/tsu split). Surgical, additive, no wire-format impact.
- [~] **M29** — User-defined generics (BAB-A04). Active milestone. Plan seeded at `docs/drafts/20260503-2210-m29-generics-plan.md`.

---

## Milestone M29-prep — PR breakdown

Detail: see `docs/archive/20260503-bab-any-anyopaque-ledgers/` for historical context, especially M28 close-out (`docs/logs/20260502-2247-m28-wireform-parity-and-docs-log.md`) which explicitly flagged PR-28.4-D02 (the prime suspect for CI-01).

- [x] **PR-29P.1** — Fix CI-01: `BaboonRuntimeCodec` tso UTC-zero `+00:00` ↔ `Z` divergence (M28-N01 violation). Round-1 hypothesis was wrong (PR-28.4-D02 generator map-key sort) — round-1 executor escalated correctly. Round-2 fix: split shared `isoFormatter` into `tsoFormatter`/`tsuFormatter` in `BaboonRuntimeCodec.scala`; widen encode parse to `ISO_OFFSET_DATE_TIME`; split decode arm; deterministic regression test. Local close gates all green. Archived PR-28.4-D02 stays deferred — separate latent issue, not the cause of CI-01.
- [x] **PR-29P.2** — Fix CI-02: `acceptance-tests` sbt 1.11.7 resolution `forbidden` inside `nix develop --ignore-environment`. **Shipped option (a)** — dropped `--ignore-environment` + four `--keep` flags from both acceptance-tests steps. Pending live-CI verification (gate runs in PR-29P.3).
- [x] **PR-29P.3** — Close-out: live-CI verification on `wip/ids-and-adts` (both jobs green per user 2026-05-03; branch merged into `wip/generics`); added deferred-policy comment to workflow per `[PR-29P.2-D05]`; ticked `[PR-29P.2-D06]` cross-cutting policy item; recorded M29 follow-ups (D02 non-UTC tso test coverage, D03 RandomJsonGenerator parity); session log written.

---

## Milestone M29-prep follow-ups — PR breakdown

Bundles the two surgical deferred items from PR-29P.1 review into one PR. Both are additive and have no wire-format impact.

- [x] **PR-29P.4** — Applied formatter split + non-UTC tso coverage. Resolves `[PR-29P.1-D02]` (RTCodecTest +05:30/-08:00 cases) + `[PR-29P.1-D03]` (RandomJsonGenerator tsoFormatter/tsuFormatter split mirroring `BaboonRuntimeCodec`). Verified `sbt baboonJVM/testOnly RTCodecTest` PASS (6/6 incl. both new cases) + `sbt baboonJS/compile` PASS.

---

## Milestone M29 — PR breakdown

Detail: per-PR breakdown will live in `docs/drafts/<YYYYMMDD-HHMM>-m29-generics-plan.md` (seeded by O1 planning subagent after PR-29P.4 lands). PR-29.1 ships the user-facing spec at `docs/spec/generics.md`. Locked design decisions from user 2026-05-03:

1. ADTs, Contracts, Services are all generifiable. AST-level templating handles them uniformly.
2. Self-reference is forbidden. Only DAG-shaped template-and-instantiation graphs are valid.
3. The **only** way to instantiate is through a type alias. Field-position instantiation and nested instantiation in alias RHS are forbidden — intermediate aliases required.
4. Monomorphisation identifier = alias id. No synthetic id `X<i32>` exposed.
5. Templates not subject to migration; instances (concrete `Typedef.User` under alias id) are.
6. `: derived[…]` written **only on the alias**, propagates to monomorphisation.

- [x] **PR-29.1** — Spec + grammar + decision doc `docs/spec/generics.md`.
- [x] **PR-29.2** — Parser: type-param head on declarations.
- [ ] **PR-29.3** — Parser: type alias instantiation RHS.
- [ ] **PR-29.4** — Typer: template registry; templates never become `DomainMember`.
- [ ] **PR-29.5** — Typer: monomorphisation via AST substitution; alias-id canonical.
- [ ] **PR-29.6** — Typer: dependency ordering (DAG-only); cycle diagnostic.
- [ ] **PR-29.7** — Validator: forbidden positions; new `TyperIssue` cases (3-site exhaustive-match update).
- [ ] **PR-29.8** — Diagnostics + LSP polish.
- [ ] **PR-29.9** — Cross-language emission smoke fixtures (`Page[T]`, `Envelope[T, E]`).
- [ ] **PR-29.10** — Acceptance fixture `m29` — JSON+UEBA roundtrip across all 9 backends.
- [ ] **PR-29.11** — Docs catch-up.

### Negative test matrix (locked)

1. `field: Foo[Bar]` — instantiation in field position.
2. `type Y = Foo[Bar[i32]]` — nested instantiation in alias RHS.
3. `data X[T] { a: lst[T] }; type XInt = X[i32, str]` — arity mismatch.
4. `data X[T, T] { … }` — duplicate type-param name.
5. `data X[T] { rec: X[T] }` — self-reference (forbidden).
6. `field: T` outside a template — placeholder leak.
7. `type Y = X` — template referenced without instantiation.
8. `type Y = i32[X]` — instantiating a non-generic.
9. `type A = X[B]; type B = X[A]` — cycle through aliases.

---

## Cross-cutting architectural notes (locked)

- [x] **M28-N01 / N02 inheritance** — wire-form canonical for tso (`±HH:MM`), Scala u64 JSON map-key (`Long.toUnsignedString` / `BaboonJsonCodec.decodeKeyU64`), Java u64 parity, Kotlin native ULong. Authoritative copies live in archived M1–M28 ledgers.
- [x] **M28-N01 applies to runtime codec too** — `tso = ±HH:MM` always (UTC = `+00:00`, NOT `Z`). PR-28.3 fixed this in the *generated* codecs (Scala/Java/Kotlin/C#/TS) but missed the *runtime* codec used by `BaboonRuntimeCodec` and consumed by `RTCodecTest`. Lands in PR-29P.1 round 2.
- [x] **PR-28.4-D02 status** — REMAINS DEFERRED. Round-1 PR-29P.1 hypothesis was wrong; this defect is a separate latent issue, not the cause of CI-01. Do NOT mark resolved during PR-29P.1.
- [x] **CI sandbox policy for Nix `--ignore-environment`** — DECIDED 2026-05-03 in PR-29P.2: acceptance and `test-*` jobs run under plain `nix develop`; `--ignore-environment` is NOT adopted as default because (1) GitHub-hosted runner injects load-bearing TLS/proxy/cache env vars (`NIX_SSL_CERT_FILE`, `SSL_CERT_FILE`, etc.) that are not reachable through the flake closure, (2) `build-linux` operates without it successfully, (3) the previous adoption (`ca0a354`, Swift backend) was load-bearing only for an unverified Swift-FHS-bwrap isolation hypothesis, and the same Swift artifacts compile under `build-linux` without the flag. Re-introduction to any test-* CI step requires either pinning `NIX_SSL_CERT_FILE`/`SSL_CERT_FILE` into the flake closure or a `--keep` list. Recorded inline in `.github/workflows/baboon-build.yml:161-163` (per `[PR-29P.2-D05]` resolution).

---

## Completed

- **PR-29P.1** (2026-05-03, two rounds) — Fixed `BaboonRuntimeCodec` tso UTC-zero offset normalisation: split single shared formatter (`appendOffset("+HH:MM", "Z")`) into `tsoFormatter` (UTC = `+00:00`) and `tsuFormatter` (UTC = `Z`); widened encode-side parse to `ISO_OFFSET_DATE_TIME` per M28-N01 decoder-tolerance clause; split decode dispatch into per-type arms. Diff: `baboon-compiler/src/main/scala/io/septimalmind/baboon/typer/BaboonRuntimeCodec.scala` (+21/-5), `baboon-compiler/.jvm/src/test/scala/io/septimalmind/baboon/tests/RTCodecTest.scala` (+34 new test). Verification: deterministic regression test fails before fix (`"...8-02-02T13:22:52.339[Z]" did not equal "...8-02-02T13:22:52.339[+00:00]"`) and passes after; close gates `mdl --seq :build :test` (1623.7s, 308/308 RTCodecTest cases green) + `:test-acceptance` + `:test-service-acceptance` (6m 13s) all PASS. Adversarial review (Opus) cleared with 3 minor/nit follow-ups, all deferred per reviewer recommendation. Round-1 (Opus) escalated correctly when the orchestrator's first hypothesis (generator-wide map-iteration sort) didn't match the failing site; round-2 (Opus) reproduced and fixed against the orchestrator's revised hypothesis (tso UTC-zero `+00:00` ↔ `Z` in `BaboonRuntimeCodec`).
  - Surprises: PR-28.3 (commit `b85fc30`) fixed M28-N01 in the *generated* per-language codecs (Scala/Java/Kotlin/C#/TS via `BaboonTools.scala`/`BaboonTimeFormats.java`/etc.) but missed the *compiler-side runtime* codec at `BaboonRuntimeCodec.scala`, which `RTCodecTest` exercises directly. The miss was invisible until `RTCodecTest` ran against the M28-extended m26 fixture (PR-28.4 added more random datetime values).
  - Operational: `mdl find_project_root` (mudyla 0.4.0 `mudyla/utils/project_root.py:26`) requires `.git` to be a directory; in a worktree it is a gitfile, so `mdl` invoked from inside a worktree silently runs sbt in the canonical checkout. Round-2 executor mirrored the two file edits into the canonical checkout so the close gates exercised the actual fix. Worktree branch left as authoritative reference; canonical checkout is byte-identical (reviewer-verified).
  - Constraint future work must respect: any new compiler-side runtime codec touching `tsu`/`tso` MUST use the per-type `tsoFormatter`/`tsuFormatter`, not a single shared formatter. M28-N01 is a wire-format invariant that applies equally to generated codecs, runtime codecs, and (eventually, per `[PR-29P.1-D03]`) fixture/example generators.

- **PR-29P.2** (2026-05-03) — Dropped `--ignore-environment` and four `--keep` flags (`HOME`, `USER`, `CI`, `GITHUB_ACTIONS`) from the two `acceptance-tests` steps in `.github/workflows/baboon-build.yml`. Result: acceptance and service-acceptance steps now run plain `nix develop --command mdl --github-actions :test-acceptance{,-service}`, matching the working `build-linux` reference. Verification: orchestrator captured local env-diff between `nix develop --ignore-environment --command env` and `nix develop --command env` — 95 vars stripped, including session/runner-injected ones; on GitHub-hosted runners these include the TLS-trust chain (`NIX_SSL_CERT_FILE` and/or `SSL_CERT_FILE`) needed by sbt-launcher's bootstrap HTTPS download. Adversarial review (Opus) returned 5 findings (1 minor + 4 nit/minor); all six PR-29P.2-Dxx defect entries land in `defects.md`. Two findings deferred explicitly to PR-29P.3 close-out (D05: in-tree comment; D06: cross-cutting policy decision in session log). Live-CI verification gate runs in PR-29P.3 after both PRs land.
  - Surprises: original commit-history claim (executor → `646543f`) was inaccurate — flag was actually introduced in `ca0a354` (Swift backend, #50, Feb 2026) alongside the bwrap/Swift-FHS workaround. Substantive conclusion is unchanged but the corrected attribution lives in `[PR-29P.2-D01]` and `[PR-29P.2-D02]`.
  - Constraint future work must respect: do NOT re-introduce `--ignore-environment` to any test-* CI step without (i) pinning `NIX_SSL_CERT_FILE` (or equivalent) into the flake closure or (ii) adding it to a `--keep` list. The flag strips runner-injected TLS trust on GitHub-hosted runners and is therefore incompatible with steps that need outbound HTTPS to public Maven/repo endpoints from inside the JVM.

- **PR-29.2** (2026-05-03) — Parser: optional `[T1, T2, …]` type-param head on `data` / `adt` / `contract` / `service` declarations; `id Foo[T] { … }` rejected at parse time per spec §6.7. Diff: 5 modified parser files (+29 lines), 1 new test file `TemplateHeadParserTest.scala` (18 tests). AST changes: added `typeParams: List[RawTypeName] = Nil` to `RawDto`/`RawAdt`/`RawContract`/`RawService` (last position, default preserves source-compat for the lone non-parser positional call site at `ScopeBuilder.scala:126`). Parser change: new `templateHead[$: P]` rule in `DefDto.scala` parsing `[ idt.symbol ("," idt.symbol)* ]` (min 1 param; uses `idt.symbol` not `typeRef`); wired into `dtoEnclosed` / `adtEnclosed` / `contractEnclosed` / `service` between name and body. Identifiers (`identifierEnclosed`) intentionally left unchanged — `[T]` before `{` is unconsumed → clean parse failure. Verification: `sbt baboonJVM/compile`, `sbt baboonJS/compile`, `sbt 'baboonJVM/testOnly *TemplateHead*'` (18/18 after round-2 fixes), `sbt 'baboonJVM/testOnly *Parser*'` (72/72), `sbt baboonJVM/test` (323/323), `sbt +compile` cross-build PASS. Adversarial review (Opus) found 2 minors + 3 nits round 1; D01/D02 (test coverage gaps) fixed inline (see PR-29.2 section in `defects.md`); D03/D04/D05 (nit/process flags) deferred with rationale to PR-29.3+ / PR-29.7.
  - Surprises: round-1 reviewer caught two test-coverage gaps the executor missed: (D01) no negative test for non-bare-identifier type-param like `data X[lst[i32]]`, leaving the load-bearing `idt.symbol`-vs-`typeRef` distinction unguarded; (D02) all positive tests used uppercase, no test demonstrating spec §2.2's "casing is convention only" rule. Both fixed by single-subagent round-2 fix pass (3 new tests). The original `assertDtoFails("data X[]")` and `assertIdentifierFails("id Foo[T] { … }")` cases were both present but not enough.
  - Constraint future work must respect: PR-29.3 (alias instantiation RHS) and PR-29.4 (typer template registry) consume `RawDto.typeParams` etc. The default `= Nil` is intentional now to preserve source-compat — once PR-29.4/PR-29.5 lands typer plumbing, drop the default per `[PR-29.2-D05]` and update `ScopeBuilder.scala:126` to pass `Nil` explicitly.

- **PR-29.1** (2026-05-03, two rounds) — User-facing spec doc `docs/spec/generics.md` (658 lines) for BAB-A04 user-defined generics. Doc-only PR — no source changes. Covers: surface syntax for `data X[T]`, `adt X[T]`, `contract X[T]`, `service X[T]` template declarations + `type Y = X[Foo]` alias-only instantiation; semantics of monomorphisation under decision #4 (alias id is canonical); §2.5 worked counter-examples for all 9 negative-test matrix items in matrix order; §4 self-reference treatment defaulting to **Option A (strict)** — container-mediated recursion within a template body is forbidden; §5 codec-annotation propagation rules including a flagged `RawAlias` parser extension required by PR-29.2/PR-29.3; §6 explicit out-of-scope list. Adversarial review (Opus) round 1 found 14 defects (5 major + 6 minor + 3 nit) — most major findings were spec syntax inaccuracies (`derived` chained-colon form, non-existent `=` form for ADT/contract templates, `;` field separator). Round-2 fix subagent applied all 14 fixes; round-2 review cleared with `clean — no findings, all 14 round-1 defects resolved correctly with no regressions`. Verification commands: `grep -n 'derived\['` (no chained-colon residue), `grep -nE '(adt|contract).*='` in code blocks (no `=`-form residue); cross-checked all 9 matrix items against §2.5 by reviewer.
  - Surprises: spec author's first round invented several syntax forms that don't match real Baboon grammar — the `derived` chained-colon (`: derived[json] : derived[ueba]`) and the `adt … =` / `contract … =` forms in particular. Round-1 reviewer caught all of them by spot-checking against `pkg0/*.baboon` fixtures and the `DefMeta.scala` / `DefAdt.scala` / `DefContract.scala` parser sources. Operational lesson: spec authors must verify surface syntax by reading actual fixtures, not from memory or analogy with adjacent languages.
  - Constraint future work must respect: §4 chose Option A (strict — container-mediated recursion within a template body is forbidden, e.g. `data Tree[T] { children: lst[Tree[T]] }` is rejected). This is the literal reading of locked decision #2. The §4 design-note callout flags it as user-revisitable; user may switch to Option B before PR-29.7 lands. PR-29.5 (monomorphisation) and PR-29.7 (validator) MUST honour Option A unless this changes.
  - Constraint: PR-29.2/PR-29.3 must extend `RawAlias` to carry an optional `derived` set per the §5 intro — pre-M29 aliases have no `derived` field, so this is a parser-side schema change, not just a grammar addition.

- **PR-29P.4** (2026-05-03) — M29-prep follow-up cleanup; bundled the two PR-29P.1 deferred items into one surgical PR. Diff: `baboon-compiler/.jvm/src/test/scala/io/septimalmind/baboon/tests/RTCodecTest.scala` (+51, two new deterministic non-UTC `tso` regression cases at offsets `+05:30` and `-08:00`); `baboon-compiler/src/main/scala/io/septimalmind/baboon/explore/RandomJsonGenerator.scala` (+18/-9, split shared `appendOffset("+HH:MM", "Z")` formatter into `tsoFormatter` (`+00:00`) + `tsuFormatter` (`Z`), dispatch `if (id == \`tso\`) tsoFormatter else tsuFormatter` mirroring `BaboonRuntimeCodec` shape). Verification: `sbt baboonJVM/testOnly io.septimalmind.baboon.tests.RTCodecTest` PASS (6/6 tests, 3 cancelled — pre-existing skips that need full mdl pipeline); `sbt baboonJS/compile` PASS (48s, 83 pre-existing warnings, none from changed files). Adversarial review (Opus) cleared with three nits, no defects requiring fix (see PR-29P.4 section in `defects.md`).
  - Surprise: PR-29P.1's regression test was honestly framed as "fail-first against the original UTC-zero bug"; the new non-UTC cases are forward-looking guards rather than fail-first reproductions, because the original bug only manifested at offset==0. Documented in the inline test comment and in `[PR-29P.1-D02]` Fix block.
  - Constraint future work must respect: M28-N01 invariant now hard-guarded across three layers — generated per-language codecs (PR-28.3), compiler-side runtime codec (PR-29P.1), and example generator (PR-29P.4). Any new code touching `tsu`/`tso` MUST use per-type formatters; never a single shared formatter.
