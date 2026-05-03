# M29-prep close-out — pre-M29 CI hardening (CI-01 + CI-02)

**Date:** 2026-05-03 (continuing same-day from M28 close).
**Branch:** `wip/ids-and-adts`.
**Final commits this session:** `29e5920`, `eed27a1`, `2a95050`, plus a forthcoming PR-29P.3 close-out commit.
**Predecessor log:** M28 close `docs/logs/20260502-2247-m28-wireform-parity-and-docs-log.md`.

## Original user request

> There are concerning build failures on CI, so our work tracked in the ledgers is not complete: [build-linux job log], [acceptance-tests job log] — both look like genuine failures to me. Fix that in /review-loop, after that move current ledgers into archive inside ./docs and seed new ledgers to implement this feature: BAB-A04 User-defined generics. Don't start working on that yet, just plan ultrathink.

The user subsequently (after orchestrator presented a plan) directed:

> First archive + seed empty ledgers for following work (but reference archived for so you may consult them), then run fixes in /review-loop, then breakdown is ok, but our goal for now is to fix, we will only proceed to generics once CI is green.

## Outcome

**Three commits shipped, all CI-blocker defects resolved locally; live-CI verification pending user push authorisation.** Plus six PR-29P.2-Dxx and four PR-29P.1-Dxx defect entries captured (1 major resolved + 9 minor/nit, of which 6 resolved inline and 3 deferred-with-rationale to M29 follow-ups).

| PR / Step | Commit | Closes |
|---|---|---|
| PR-29P.0 (setup) | `29e5920` | Archive M1–M28 ledgers (`tasks.md`, `defects.md`) into `docs/archive/20260503-bab-any-anyopaque-ledgers/` with `README.md` cross-link. Seed fresh `tasks.md` / `defects.md` at repo root for M29-prep + M29 (parked). |
| PR-29P.2 | `eed27a1` | CI-02. Drop `--ignore-environment` and four `--keep` flags from both `acceptance-tests` steps in `.github/workflows/baboon-build.yml`. Resolves `[PR-29P.2-D01]` + 5 minor/nit follow-ups (D02–D06). |
| PR-29P.1 | `2a95050` | CI-01. Split shared `isoFormatter` in `BaboonRuntimeCodec.scala` into `tsoFormatter` (UTC = `+00:00`) + `tsuFormatter` (UTC = `Z`); widen encode parse to `ISO_OFFSET_DATE_TIME`; split decode arm; add deterministic regression test in `RTCodecTest.scala`. Resolves `[PR-29P.1-D01]` + 3 minor/nit follow-ups (D02–D04). |
| PR-29P.3 (close-out) | (forthcoming) | Inline comment for `[PR-29P.2-D05]` policy; tick `[PR-29P.2-D06]` cross-cutting decision in `tasks.md`; this session log. |

## Ground-truth verification

Local close gates as run by PR-29P.1 round-2 executor:

- `mdl --seq :build :test` — **PASS** (1623.7s wall time, 308/308 RTCodecTest cases green, full per-language test matrix green).
- `mdl :test-acceptance` — **PASS** (cross-language JSON+UEBA round-trip matrix).
- `mdl :test-service-acceptance` — **PASS** (6m 13s).

Live-CI verification on `wip/ids-and-adts` requires user push authorisation (CLAUDE.md cautious-action discipline).

## CI-01 — root cause path (with the round-1 escalation)

**Round 1 (Opus, escalated):** Orchestrator's first hypothesis blamed PR-28.4-D02 (archived deferred): generator-wide map-iteration sort-key uses typed `_._1.toString` rather than encoded wire-form key. Round-1 executor:

1. Could not reproduce the failure locally (`6 of 6 RTCodecTest cases passed` on `e2d41ab`).
2. Demonstrated the hypothesis didn't fit the failing site: `RTCodecTest` exercises the *runtime* `BaboonRuntimeCodec`, not the *generated* per-language codecs PR-28.4-D02 concerns.
3. Verified `T6_D1` v3.0.0 declares no u64-/tso-/i64-keyed multi-entry maps in the failing field set — only `map[str, i32]` and `map[u32, u32]` — so the typed-`toString` ↔ unsigned-numeric divergence cannot manifest.
4. Disassembled circe `JsonObject.class` to confirm `Json` object equality is order-INSENSITIVE; therefore a "map ordering mismatch" cannot make `==` fail.
5. Escalated rather than guess-fixing, per CLAUDE.md §6a ("No repro, no fix").

This was textbook correct application of the reproduction discipline. The round-1 contribution prevented shipping a wrong-and-disruptive fix that would have re-locked every cross-language fixture md5 for no benefit.

**Round 2 (Opus, fixed):** Orchestrator extracted the actual divergence from the CI log (a Python diff over the ANSI-stripped Expected vs Got blocks): the failure is a **single-line value divergence** — `f09: tso` field's UTC-zero offset normalised from `"+00:00"` to `"Z"` on `BaboonRuntimeCodec` round-trip. This violates **M28-N01** (locked, `docs/logs/20260502-2247-m28-wireform-parity-and-docs-log.md:50`): *"tso = `±HH:MM` always; UTC = `+00:00`, NOT `Z`. tsu retains `...Z` semantics for genuine UTC-Kind values."*

PR-28.3 (commit `b85fc30`) had fixed this canonicalisation in the **generated** per-language codecs (Scala/Java/Kotlin/C#/TS via `BaboonTools.scala` etc.) but missed the **compiler-side runtime** codec at `BaboonRuntimeCodec.scala`. The miss became visible only after PR-28.4 extended the m26 fixture with more random datetime values; pre-PR-28.4 fixtures happened to never produce a tso UTC-zero value, so the latent bug was sidestepped.

Round-2 fix:
- Split single shared `isoFormatter` (`appendOffset("+HH:MM", "Z")`) at `BaboonRuntimeCodec.scala:26-40` into `tsuFormatter` (Z literal) + `tsoFormatter` (`+00:00` literal). Mirrors PR-28.3 `BaboonTools.scala` shape behaviorally.
- Encode-side parse arm at lines 428-437 widened from strict `OffsetDateTime.parse(value, isoFormatter)` (3-digit fractional) to bare `OffsetDateTime.parse(value)` (`ISO_OFFSET_DATE_TIME`, accepts both `Z` and `±HH:MM` per M28-N01 decoder-tolerance clause; flexible fractional precision).
- Decode-side at lines 471-479 split single `tsu | tso` arm into two arms, dispatching to per-type formatter on output.
- Regression test at `RTCodecTest.scala:38-66` exercises both `tso "+00:00"` round-trip (failing case, now passes) and `tsu "Z"` round-trip (regression-guard for M28-N01 second clause). Test is deterministic — not fixture-seed-dependent — so the case fires on every CI run regardless of randomised fixture content.

**Adversarial review (Opus, round 2):** cleared with three minor/nit follow-ups:

- `[PR-29P.1-D02]` (minor, deferred): regression test does not cover non-UTC tso offsets (e.g. `+05:30`, `-08:00`). Patch is mechanically correct for non-UTC; deferred to M29 follow-up.
- `[PR-29P.1-D03]` (minor, deferred): `RandomJsonGenerator.scala:14-17, 102-111` carries the same single-shared-formatter pattern in the `:example` REPL output. Not codec-roundtripped (no wire-format impact), but user-facing — `tso` example values currently print as `Z` instead of `+00:00`, misleading users about M28-N01. Deferred to M29 follow-up.
- `[PR-29P.1-D04]` (nit): ledger hygiene; orchestrator close-out, performed inline with the PR-29P.1 commit.

Reviewer's bottom line: "Recommendation: accept and merge with the three follow-ups recorded."

## CI-02 — root cause path (single round)

**Diagnosis:** acceptance-tests job's `mdl :test-acceptance` invocation ran `nix develop --ignore-environment --keep HOME --keep USER --keep CI --keep GITHUB_ACTIONS --command …`. The `--ignore-environment` flag stripped ~95 env vars (orchestrator captured local `nix develop --ignore-environment --command env` vs `nix develop --command env` diff: 95 vars dropped, including all session/runner-injected ones). On GitHub-hosted runners, the `7mind/github-env@minimal` action and the Determinate-Systems Nix installer inject the SSL/TLS trust chain settings (`NIX_SSL_CERT_FILE`, `SSL_CERT_FILE`) needed by the JVM-based sbt-launcher to verify Maven Central's certificate during its bootstrap download. The strip caused `forbidden` (Cloudflare 403, likely from a malformed TLS handshake) on `sbt 1.11.7` resolution.

**Cross-job comparison** isolated the differentiator: the `build-linux` job in the same workflow run uses plain `nix develop --command` (no `--ignore-environment`) and resolves sbt cleanly. So acceptance-tests did not need a stricter sandbox than build.

**Fix:** dropped `--ignore-environment` and the four `--keep` flags from both acceptance-tests steps. 12 lines deleted, 2 added.

**Adversarial review (Opus):** cleared with five minor/nit follow-ups (D02–D06). Notable corrections:

- `[PR-29P.2-D02]` (minor, note-only): executor's commit-history claim was inaccurate — the flag was actually introduced in `ca0a354` (Swift backend, #50, Feb 2026) alongside the AppArmor/bwrap workaround for the Swift FHS toolchain, not in `646543f` as the executor first stated. Substantive conclusion (drop the flag) is unaffected; corrected attribution captured in `[PR-29P.2-D01]`.
- `[PR-29P.2-D03]` (minor): executor returned without flipping ledger to resolved or filling `Fix:` text. Per loop's I3 convention, ledger maintenance is orchestrator work; closed by orchestrator update.
- `[PR-29P.2-D04]` (minor): executor's local verification was inconclusive (sbt 1.11.7 was pre-cached locally, so the resolution path was skipped). Orchestrator captured a structural env-diff signal as backup; full verification deferred to live-CI on `wip/ids-and-adts`.
- `[PR-29P.2-D05]` (nit): no in-tree comment memorialising the dropped-sandbox-flag decision. Resolved in PR-29P.3 close-out: 3-line comment added above the two acceptance-tests steps pointing at `defects.md [PR-29P.2-D01]` for full root-cause.
- `[PR-29P.2-D06]` (nit): cross-cutting "Nix `--ignore-environment` policy" question. Resolved in PR-29P.3: ticked in `tasks.md` Cross-cutting Notes + inline workflow comment.

## Cross-cutting architectural decisions locked this session

1. **CI sandbox policy for Nix `--ignore-environment`** — acceptance and `test-*` jobs run under plain `nix develop`. `--ignore-environment` is NOT adopted as default because (a) GitHub-hosted runner injects load-bearing TLS/proxy/cache env vars not reachable through the flake closure; (b) `build-linux` operates without it successfully; (c) the previous adoption (`ca0a354`) was load-bearing only for an unverified Swift-FHS-bwrap isolation hypothesis, and the same Swift artifacts compile under `build-linux` without the flag. Re-introduction to any test-* CI step requires either pinning `NIX_SSL_CERT_FILE`/`SSL_CERT_FILE` into the flake closure or explicit `--keep` flags. Recorded inline in `.github/workflows/baboon-build.yml:161-163`.

2. **M28-N01 applies to runtime codec too** — the canonical wire form `tso = ±HH:MM` always (UTC = `+00:00`, NOT `Z`); `tsu` retains `Z`. PR-28.3 had fixed this in the generated per-language codecs but missed the compiler-side runtime codec used by `RTCodecTest`. Constraint future work must respect: any new runtime codec touching `tsu`/`tso` MUST use per-type `tsoFormatter`/`tsuFormatter`, not a single shared formatter. PR-28.4-D02 (archived deferred) is a SEPARATE latent issue, not the cause of CI-01.

## Operational gaps surfaced this session

- **Worktree base-staleness on multi-PR loops.** Both PR-29P.2's executor worktree and PR-29P.3's `[PR-29P.2-D05]` fix-attempt worktree shared an old base commit (`8788c83`, ~M22 era), not the current `wip/ids-and-adts` HEAD. PR-29P.2's executor still produced a correct patch because it edited only the workflow file, whose 21-line acceptance-tests block had not drifted between the worktree base and current HEAD. PR-29P.3's `[PR-29P.2-D05]` subagent saw the *pre-PR-29P.2* state of the same block and wrote a comment describing the wrong state; orchestrator discarded that diff and wrote the correct comment inline in the canonical checkout. Should be diagnosed at the harness level (the agent runtime's worktree provisioning); orthogonal to the M29-prep work itself.

- **`mdl find_project_root` and worktrees.** Mudyla 0.4.0 `mudyla/utils/project_root.py:26` requires `.git` to be a directory; in a git worktree `.git` is a gitfile, so `mdl` invoked from inside a worktree silently runs sbt in the canonical checkout. PR-29P.1 round-2 executor identified this and mirrored its two file edits into the canonical checkout to ensure the close gates exercised the actual fix. Worktree branch and canonical checkout left byte-identical (reviewer-verified).

## Forward-tracked items

### PR-29P.3 close-out blocker

- **Live-CI verification on `wip/ids-and-adts`** — three local commits ready (`29e5920`, `eed27a1`, `2a95050`) plus a forthcoming PR-29P.3 close-out commit. Push requires user authorisation per CLAUDE.md cautious-action discipline. Once green, M29-prep is `[x]` and M29 (BAB-A04 user-defined generics) becomes the active milestone.

### M29 follow-ups (recorded by PR-29P.1, to address inside M29 once CI green)

- **`RandomJsonGenerator` tso/tsu split** (`[PR-29P.1-D03]`) — apply the same per-type-formatter split to `baboon-compiler/src/main/scala/io/septimalmind/baboon/explore/RandomJsonGenerator.scala:14-17, 102-111`. Eliminates the M28-N01 violation in `:example` REPL output. User-facing only; no wire-format impact.
- **Non-UTC tso regression coverage** (`[PR-29P.1-D02]`) — extend `RTCodecTest`'s deterministic regression to cover non-UTC `tso` offsets (e.g. `+05:30`, `-08:00`) so a future change that hard-codes `+00:00` would fail immediately rather than silently. Test additive only.

### Carry-overs from M28 still open

- **f64 cross-backend canonicalisation** (PR-28.2-D01, archived). Six of nine backends emit `<int>.<frac>` natively; TS+Swift require root-pipeline architectural change to `JSON.stringify` / `JSONSerialization`. Architectural decision pending.
- **Generator-wide map-iteration sort-key** (PR-28.4-D02, archived). NOT the cause of CI-01 (round-1 escalation correctly demonstrated this). Latent issue; will manifest only when m26-style fixtures gain genuine multi-entry u64/tso/i64-keyed maps. Status remains deferred.
- **Python tso microsecond emission** (PR-28.4-D03, archived). Per-type datetime serializer needed in `PyJsonCodecGenerator.scala`.
- **Python `LEDataOutputStream.write_datetime` kind-byte semantics** (PR-28.4-D04, archived). Divergent from nine other backends.

## Session-end disposition

M29-prep closed per `/review-loop` outer-loop O5 with no remaining planned/in-progress tasks **except** PR-29P.3's live-CI gate, which is blocked on user push authorisation per `/review-loop` I6. Returning to user with:

- Three commits on `wip/ids-and-adts`: `29e5920` (archive+seed), `eed27a1` (CI-02 fix), `2a95050` (CI-01 fix), plus a forthcoming PR-29P.3 close-out commit.
- Local close gates green: `mdl --seq :build :test`, `mdl :test-acceptance`, `mdl :test-service-acceptance`.
- 9 minor/nit defects ledgered (6 resolved inline + 3 deferred-with-rationale to M29 follow-ups).
- Two cross-cutting architectural decisions locked.
- Two operational gaps documented for future loop hygiene.

Awaiting user authorisation to push `wip/ids-and-adts` for live-CI verification. Once both CI jobs are green, M29-prep is `[x]` and M29 (BAB-A04 user-defined generics) becomes the next active milestone per the design plan parked in `tasks.md`.
