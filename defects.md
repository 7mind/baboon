# Baboon — Defect Ledger

> **Predecessor ledger (frozen):** `docs/archive/20260503-bab-any-anyopaque-ledgers/defects.md`
> Covers PR-01..PR-28.x defects (`any`/AnyOpaque, identifiers, ADT inheritance, codecs, map-key encoding). Reuse historical entries when investigating regressions in those areas.

Status: `[ ]` open · `[~]` under fix · `[x]` resolved

---

## PR-29P.1 — Fix CI-01: RTCodecTest map-key ordering

## [PR-29P.1-D01] `RTCodecTest` JSON→UEBA→JSON roundtrip normalises `tso` UTC-zero offset from `+00:00` to `Z`, violating M28-N01
**Status:** resolved (round 2; pending live-CI verification on `wip/ids-and-adts`)
**Severity:** major
**Location:** `baboon-compiler/src/main/scala/io/septimalmind/baboon/typer/BaboonRuntimeCodec.scala` (the encode/decode path used by `RTCodecTest`); failing equality site at `baboon-compiler/.jvm/src/test/scala/io/septimalmind/baboon/tests/RTCodecTest.scala:124-127`. T6_D1 schema at `baboon-compiler/src/test/resources/baboon/pkg0/pkg03.baboon:230-269` declares `f09: tso`, `f10: tsu`.
**Description:** CI build job `build-linux-amd64` reports `1/308` test failures. After ANSI-strip diff of the Expected vs Got blocks in the CI log (`/tmp/ci-build.log`, lines 21232-23597), the actual divergence is **a single value**: in one element of the `fSameCustomLst` list, `f09` (declared type `tso`) is:

```diff
-      "f09" : "6328-02-02T13:22:52.339+00:00",
+      "f09" : "6328-02-02T13:22:52.339Z",
```

Expected JSON (from the C# fixture file on disk) carries `+00:00`. Got JSON (after `BaboonRuntimeCodec.encode` → `BaboonRuntimeCodec.decode` round-trip) carries `Z`. Per **M28-N01** (locked, `docs/logs/20260502-2247-m28-wireform-parity-and-docs-log.md:50`): *"tso = `±HH:MM` always; UTC = `+00:00`, NOT `Z`. tsu retains `...Z` semantics for genuine UTC-Kind values."* The Got value is the wrong canonical form for `tso`.

All other content (the multi-entry maps `fNewMap`/`fPrecex4`/`fSwapPrecex0..2`, the surrounding 1100+ JSON lines) is identical. The remaining diff entries are interleaved CI log lines from `IdentifierKotlinEmissionTest` running concurrently — log-stream noise, not value divergence.
**Root cause (hypothesis updated):** `BaboonRuntimeCodec` does not honour M28-N01 for `tso`. Either the encode path collapses `+00:00` into a UTC-Kind sentinel (losing offset info), and the decode path re-emits as `Z`; or the decode path explicitly normalises offset-zero to `Z`. Needs source inspection of the runtime codec's tso handling. PR-28.3 fixed `tso ±HH:MM` canonicalisation in the **generated** codecs (Scala/Java/Kotlin/C#/TS) but appears not to have touched the **runtime** codec used by `BaboonRuntimeCodec` and consumed by `RTCodecTest`.
**History — what was wrong with the round-1 hypothesis (now superseded):** Round-1 brief hypothesised PR-28.4-D02 (generator-wide map-iteration sort-key typed `_._1.toString`). PR-29P.1 round-1 executor (Opus, escalated rather than fixing) demonstrated:
1. The hypothesis applies to *generator-emitted* codecs, not the *runtime* `BaboonRuntimeCodec` exercised by `RTCodecTest`.
2. T6_D1's failing fields are not u64/tso/i64-keyed maps but a `tso` value inside a list element.
3. circe `Json` object equality is order-INSENSITIVE (verified by class disassembly), so a "map ordering" hypothesis cannot make `==` fail.
4. The C# fixture random generator does not produce u64 boundary values that would trigger typed-`toString` ↔ unsigned-numeric divergence.
The escalation was correct. Round-1 closes with `resolved (note-only; superseded by corrected root cause in this entry's revision)`. Round 2 dispatches with the actual root cause above. PR-28.4-D02 (archived deferred) remains a real latent issue but is **not** the cause of CI-01.
**Suggested fix (round 2):**
1. **Reproduce locally** (CLAUDE.md §6a). The simplest repro: run `mdl :build :test-gen-regular-adt` to generate the fixture, locate `target/test-regular/cs-stub/.../T6_D1.json`, find a `tso` field whose value contains `+00:00`, then run `RTCodecTest.scala`'s "roundtrip JSON files through UEBA" case. If the local C# fixture random seed never picks `00:00` for any tso offset, force the case: write a minimal JSON file containing one `tso` field with `"...+00:00"` and feed it through `BaboonRuntimeCodec.encode`/`decode`. Confirm Got side returns `"...Z"` — that is the failing-for-the-right-reason check.
2. **Locate the runtime codec's tso path.** `BaboonRuntimeCodec.scala` calls into `BaboonTimeFormats` (or equivalent) for tso encode/decode. Find the function that handles `tso`. Likely culprits:
   - JVM `OffsetDateTime`/`ZonedDateTime` formatting where `ZoneOffset.UTC.toString` returns `"Z"` instead of `"+00:00"`.
   - A `DateTimeFormatter` that uses `ISO_OFFSET_DATE_TIME` (which emits `Z` for UTC) instead of an explicit pattern that always emits `±HH:MM`.
   - An `if (offset == 0) "Z" else …` branch.
3. **Fix:** force the tso formatter to always emit `±HH:MM`, including `+00:00` for UTC. Mirror the fix shape used in PR-28.3's generated-codec path (see commit `b85fc30`, which fixed Scala/Java/Kotlin/C#/TS generated tso codecs); the same canonicalisation must apply here.
4. **Add a regression test.** A unit test on `BaboonRuntimeCodec` (or `BaboonTimeFormats`) that round-trips `"...+00:00"` and asserts the output is `"...+00:00"`, not `"...Z"`. Also add a `tso`-with-UTC-zero-offset case to the deterministic fixture set used by `RTCodecTest` so the case is exercised on every CI run regardless of fixture seed.
5. **Out of scope (do NOT include in this PR):** the M28 deferred PR-28.4-D02 (generator map-iteration sort-key) — that is a separate latent issue, not the cause of CI-01. Leave the archived deferred-defect note alone; do NOT mark it resolved here.
6. Run `mdl --seq :build :test`, `:test-acceptance`, `:test-service-acceptance` to confirm green.

**Fix:** Round 2 (executor: Opus, 70-min run). Split the single shared `isoFormatter` (`appendOffset("+HH:MM", "Z")`) in `baboon-compiler/src/main/scala/io/septimalmind/baboon/typer/BaboonRuntimeCodec.scala:26-40` into two named formatters: `tsuFormatter` (Z literal for UTC, retains M28-N01 second clause) + `tsoFormatter` (`+00:00` literal for UTC, satisfies M28-N01 first clause). Mirrors PR-28.3 (commit `b85fc30`) `BaboonTools.scala` shape behaviorally (different builder style, equivalent output). Encode-side parse arm at lines 428-437 widened from `OffsetDateTime.parse(value, isoFormatter)` (strict 3-digit fractional) to bare `OffsetDateTime.parse(value)` (`ISO_OFFSET_DATE_TIME`, accepts both `Z` and `±HH:MM` per M28-N01 decoder-tolerance clause; flexible fractional precision). Decode-side at lines 471-479 split single `tsu | tso` arm into two arms dispatching to per-type formatter. Regression test added in `baboon-compiler/.jvm/src/test/scala/io/septimalmind/baboon/tests/RTCodecTest.scala:38-66` exercising both `tso +00:00` round-trip (failing case) and `tsu Z` round-trip (regression-guard for second M28-N01 clause); test runs end-to-end through `BaboonRuntimeCodec.encode → decode`, deterministic (not fixture-seed-dependent). Close gates: `mdl --seq :build :test` PASS (1623.7s, 308/308 RTCodecTest cases), `mdl :test-acceptance` PASS, `mdl :test-service-acceptance` PASS (6m 13s). Adversarial review (Opus) cleared with 3 minor/nit follow-ups (D02-D04 below), no blocking findings.

**Cross-cuts:**
- Round-1 hypothesis (PR-28.4-D02 generator map-iteration sort-key) was wrong; round-1 executor escalated correctly rather than guess-fixing. Round-1 contribution captured in PR-28.4-D02 archived ledger; that defect REMAINS DEFERRED — it is a separate latent issue, not the cause of CI-01.
- Other backend runtime codecs (Cs, Ts, Rs, Kt, Jv) already received the equivalent fix via PR-28.3. The compiler-side `BaboonRuntimeCodec.scala` was the missed JVM-only site. Reviewer verified by grep across `baboon-compiler/src/main/resources/baboon-runtime/`.

## [PR-29P.1-D02] Regression test does not cover non-UTC tso offsets, leaving regression-guard incomplete for ±HH:MM other than `+00:00`
**Status:** resolved (deferred to M29 follow-up)
**Severity:** minor
**Location:** `baboon-compiler/.jvm/src/test/scala/io/septimalmind/baboon/tests/RTCodecTest.scala:38-66`.
**Description:** The new regression test only covers `tso` UTC-zero (`+00:00`) and `tsu` UTC (`Z`). It does not exercise non-UTC `tso` offsets like `+05:30` or `-08:00`. The patch is mechanically correct for non-UTC (both old and new formatters emit `+05:30` for non-zero offsets via `appendOffset("+HH:MM", _)`), so the regression risk is low — but a future change that, e.g., hard-codes `+00:00` output would silently break non-UTC tso while keeping this test green. The defect-of-record `[PR-29P.1-D01]` required a deterministic UTC-zero case "exercised on every CI run regardless of fixture seed"; that is met. Non-UTC offset coverage is a stricter bar that was not explicitly required.
**Fix:** Defer to a separate widen-coverage PR in M29 (post-CI-green). Recorded as a `tasks.md` M29 follow-up.

## [PR-29P.1-D03] `RandomJsonGenerator` carries the same buggy single-shared-formatter pattern in the `:example` REPL output
**Status:** resolved (deferred to M29 follow-up)
**Severity:** minor (deferred)
**Location:** `baboon-compiler/src/main/scala/io/septimalmind/baboon/explore/RandomJsonGenerator.scala:14-17, 102-111`.
**Description:** `RandomJsonGenerator` uses a single shared formatter `appendOffset("+HH:MM", "Z")` for both `tsu` and `tso`. Generated examples for `tso` fields with UTC offset will print `Z`, misleading users about the canonical wire form. Consumers (verified): only `baboon-compiler/.js/src/main/scala/io/septimalmind/baboon/BaboonJS.scala:829` (JS REPL "generate" entrypoint) and `baboon-compiler/.jvm/src/main/scala/io/septimalmind/baboon/explore/commands/ExampleCommand.scala:23` (JVM `:example` command). Output is NOT roundtripped through codecs in production paths, so does not affect wire-format invariants. However, output IS user-facing in the REPL — a `tso` example showing `Z` would mislead users about M28-N01. Practical impact further bounded: `RandomJsonGenerator.scala:110` always emits `ZoneOffset.UTC`, so non-UTC variants don't materialise — the visible defect is only "tso example shows `Z` when it should show `+00:00`".
**Fix:** Defer to a separate M29 follow-up PR. Apply the same split: `tsuFormatter` with `appendOffset("+HH:MM","Z")`, `tsoFormatter` with `appendOffset("+HH:MM","+00:00")`. Out of scope for the surgical CI-01 fix per CLAUDE.md §5.

## [PR-29P.1-D04] Ledger hygiene — `tasks.md` PR-29P.1 / PR-29P.3 status flips deferred to orchestrator commit step
**Status:** resolved (orchestrator close-out — performed inline with the PR-29P.1 commit)
**Severity:** nit
**Location:** `tasks.md`.
**Description:** Reviewer noted that at review time `PR-29P.1` was still `[~]` and `PR-29P.3` (close-out) was `[ ]` un-started, despite the local close gates being green. Per the loop's I3/I5 convention, ledger-status flips are orchestrator work performed at PR commit time, not subagent work. Closed by this commit's orchestrator update.
**Fix:** Orchestrator updated `tasks.md`: `PR-29P.1` → `[x]` with full Completed entry; `PR-29P.3` advanced to `[~]` for live-CI verification.

---

## PR-29P.2 — Fix CI-02: acceptance-tests sbt resolution

## [PR-29P.2-D01] `acceptance-tests` sbt 1.11.7 resolution returns `forbidden` inside `nix develop --ignore-environment`
**Status:** resolved (pending live-CI verification on `wip/ids-and-adts`)
**Severity:** major
**Location:** `.github/workflows/baboon-build.yml:159-179` (the two `acceptance-tests` job steps "Run acceptance tests" and "Run service acceptance tests").
**Description:** Acceptance job fails at sbt-launcher startup with:
```
[error] [launcher] xsbt.boot.internal.shaded.coursier.error.ResolutionError$CantDownloadModule: Error downloading org.scala-sbt:sbt:1.11.7
  forbidden: https://repo1.maven.org/maven2/org/scala-sbt/sbt/1.11.7/sbt-1.11.7.pom
  forbidden: https://repo.scala-sbt.org/scalasbt/maven-releases/org/scala-sbt/sbt/1.11.7/sbt-1.11.7.pom
[error] [launcher] could not retrieve sbt 1.11.7
```
External availability confirmed: sbt 1.11.7 returns `HTTP/2 200` from `repo1.maven.org` outside CI. Build job `build-linux` in the same workflow run uses plain `nix develop --command` (no `--ignore-environment`) and resolves sbt cleanly. Cross-job comparison isolates `--ignore-environment` as the differentiator.
**Root cause:** `--ignore-environment` strips ~95 env vars including those injected by the `7mind/github-env@minimal` action and the Determinate-Systems Nix installer. Among the stripped vars on a fresh GitHub-hosted runner are the SSL/TLS trust chain settings (`NIX_SSL_CERT_FILE` and/or `SSL_CERT_FILE`) that the JVM-based sbt-launcher needs to verify Maven Central's certificate during its bootstrap download. Local env-diff (`nix develop --ignore-environment --command env` vs `nix develop --command env`) confirms the flag drops a large set of runner-injected vars; the specific TLS-trust mechanism varies between local NixOS (system-installed Nix; `NIX_SSL_CERT_FILE` not set in either mode) and the GitHub runner (Determinate installer injects it via the runner shell), so the local repro is structural rather than identical, but the asymmetry between `build-linux` (works) and `acceptance-tests` (fails) is the operational evidence.
**Fix:** Removed `--ignore-environment` and the four associated `--keep` flags (`HOME`, `USER`, `CI`, `GITHUB_ACTIONS`) from both `nix develop` invocations in the `acceptance-tests` job in `.github/workflows/baboon-build.yml`. The two affected steps now run plain `nix develop --command mdl --github-actions :test-acceptance` and `… :test-service-acceptance`, matching the pattern used by `build-linux`, `test-editors`, and the publish jobs. Twelve lines deleted, two added; surgical edit, no other workflow changes.
**Cross-cuts:** The flag was originally introduced in commit `ca0a354` (Swift backend, #50, Feb 2026) alongside the AppArmor/bwrap workaround for the Swift FHS toolchain — not in `646543f` as the executor first stated; commit `646543f` only preserved the existing flag during a refactor. Neither commit message documents the rationale, but the temporal correlation suggests defensive isolation for Swift FHS-bwrap. The empirical evidence that `build-linux` exercises the same Swift toolchain artifacts without the flag and succeeds is the basis for dropping it.

## [PR-29P.2-D02] Executor's commit-history attribution misidentifies the introducing commit
**Status:** resolved (note-only; corrected attribution captured in PR-29P.2-D01)
**Severity:** minor
**Location:** PR-29P.2 executor's return message (rationale, not in shipped code).
**Description:** Executor characterised commit `646543f` as introducing `--ignore-environment` "by carry-over with no documented justification". Reviewer's `git log -S '--ignore-environment'` walk shows `646543f` only preserved the flag during a refactor; the actual introduction was `ca0a354` (Swift backend, #50, Feb 2026) alongside the AppArmor/bwrap setup. Substantive conclusion (drop the flag) is unaffected; only the historical narrative needs correction.
**Fix:** Corrected attribution recorded in PR-29P.2-D01 root-cause and cross-cuts sections; will also be reflected in the PR-29P.3 session log.

## [PR-29P.2-D03] Defects ledger entry not updated to "resolved" by the executor
**Status:** resolved (orchestrator close-out)
**Severity:** minor
**Location:** `defects.md` entry `[PR-29P.2-D01]`.
**Description:** Reviewer noted the executor returned without flipping `[PR-29P.2-D01]` to resolved or filling in `Fix:` text. Per the loop's I3 step, ledger maintenance is orchestrator work, not subagent work — so this is a process gap reminder rather than an executor defect. Closed by this round's orchestrator update.
**Fix:** Orchestrator updated `[PR-29P.2-D01]` to `resolved (pending live-CI verification)` with full root-cause and `Fix:` text.

## [PR-29P.2-D04] Live-CI validation gap — local repro of the failure mode was inconclusive
**Status:** resolved (env-diff signal captured; live-CI validation deferred to PR-29P.3 close-out)
**Severity:** minor
**Location:** PR-29P.2 process; verification commands captured in this entry.
**Description:** Reviewer flagged that the executor's local verification was inconclusive (sbt 1.11.7 was pre-cached in `/nix/store/bmqjnx6lmlip2yv697mzwpl556jwbbwq-sbt-1.11.7`, so the resolution path was skipped) and recommended a cheaper signal. Orchestrator captured `nix develop --ignore-environment --command env` vs `nix develop --command env` and diffed the env-var name sets. Result: 95 env vars are stripped by `--ignore-environment`, including all session/runner-injected vars (HOME, USER, NIX_PATH, NIX_PROFILES, runner-action exports, etc.). On local NixOS neither mode has `NIX_SSL_CERT_FILE`/`SSL_CERT_FILE` set (system-installed Nix differs from runner Nix on this point), so the local signal does not pinpoint the exact TLS-trust variable but does confirm structurally that the flag strips a load-bearing set of vars. The operational success criterion is green CI on `wip/ids-and-adts`; that gate runs in PR-29P.3.
**Fix:** Env-diff captured at orchestration time; rationale documented in PR-29P.2-D01 root-cause. Live-CI verification gate deferred to PR-29P.3 close-out per `tasks.md` plan.

## [PR-29P.2-D05] No inline comment in the workflow memorialising the dropped-sandbox-flag decision
**Status:** open (deferred to PR-29P.3 close-out)
**Severity:** nit
**Location:** `.github/workflows/baboon-build.yml:159-164` (the two acceptance-tests steps).
**Description:** Adjacent steps in the workflow carry inline comments explaining non-obvious choices. The two-month historical asymmetry (acceptance-tests alone using `--ignore-environment`) is not memorialised in-tree. A future agent seeing the diff in `git blame` would have no in-tree explanation. Out-of-scope for the surgical PR-29P.2 fix per CLAUDE.md §5.
**Suggested fix:** Defer to PR-29P.3 close-out — add a one-line comment above the two steps along the lines of "# Plain `nix develop` (no --ignore-environment): runner-injected env carries TLS trust chain that sbt-launcher needs for upstream resolution. See defects.md PR-29P.2-D01."

## [PR-29P.2-D06] Cross-cutting "Nix `--ignore-environment` policy" question not recorded as a decision
**Status:** open (deferred to PR-29P.3 session log)
**Severity:** nit
**Location:** `tasks.md` Cross-cutting architectural notes.
**Description:** The cross-cutting item asks whether the acceptance step needs the strict sandbox at all, or whether the flake closure is sufficient. Executor implicitly decided "no strict sandbox needed" by shipping option (a), but did not record the decision rationale anywhere. Future test-* actions that consider adopting `--ignore-environment` have no precedent to lean on.
**Suggested fix:** PR-29P.3 session log should explicitly state: "acceptance and test-* jobs run under plain `nix develop`; `--ignore-environment` is not adopted as default because (1) GitHub-hosted runner injects load-bearing TLS/proxy/cache env vars not reachable through the flake closure, (2) `build-linux` operates without it successfully, (3) the previous use was load-bearing only for an unverified Swift-FHS-bwrap isolation hypothesis, and the same Swift artifacts compile under `build-linux` without the flag." Tick the tasks.md item.
