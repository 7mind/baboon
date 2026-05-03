# Baboon — Defect Ledger

> **Predecessor ledger (frozen):** `docs/archive/20260503-bab-any-anyopaque-ledgers/defects.md`
> Covers PR-01..PR-28.x defects (`any`/AnyOpaque, identifiers, ADT inheritance, codecs, map-key encoding). Reuse historical entries when investigating regressions in those areas.

Status: `[ ]` open · `[~]` under fix · `[x]` resolved

---

## PR-29P.1 — Fix CI-01: RTCodecTest map-key ordering

## [PR-29P.1-D01] `RTCodecTest` JSON→UEBA→JSON roundtrip fails on multi-entry maps under CI hash-iteration order
**Status:** open
**Severity:** major
**Location:** `baboon-compiler/.jvm/src/test/scala/io/septimalmind/baboon/tests/RTCodecTest.scala:124-127` (failing equality site); root cause expected in `ScUEBACodecGenerator.scala` and the equivalent emit in 8 sibling backends; m26 fixture under `baboon-compiler/src/main/resources/baboon-runtime/` and the fixture-emitter chain.
**Description:** CI build job `build-linux-amd64` reports `1/308` test failures. The failing fixture is `T6_D1` in `testpkg.pkg0` v3.0.0; the diffed payload contains multi-entry maps (`fNewMap` with 3 string keys, `fPrecex4` with 12 stringified-i64 keys, plus `fSwapPrecex0..2` lists). Local M28 close-gate (`mdl --seq :build :test`) was green at 2026-05-02 (per `docs/logs/20260502-2247-m28-wireform-parity-and-docs-log.md`); CI fails on the same commit's merge head (`564d1ba`). Most plausible explanation: the encoder uses typed-key string ordering (`_._1.toString`) for map-iteration sort-key, while the decoder produces JSON whose object-entry order tracks UEBA insertion order = encoded-wire-form key order. The two orderings diverge for u64 (signed-vs-unsigned), tso (formatted with `±HH:MM`), and stringified-i64 keys. CI's JDK / GC / HashMap iteration policy provokes the divergence; local does not.
**Root cause (hypothesis, must be confirmed by repro):** PR-28.4-D02 (archived predecessor ledger): "Generator-wide map-iteration sort-key uses `_._1.toString` (typed) rather than encoded wire-form key. Sidestepped via single-entry maps in m26 fixture." PR-28.4 then extended the m26 fixture with `mu64`+`mtso`, both of which can introduce multi-entry cases under randomised seeds. The deferral was incorrect — the bug is reachable.
**Suggested fix:**
1. Reproduce locally first (CLAUDE.md §6a). Capture the CI's `T6_D1` fixture seed by re-running `:test-gen-regular-adt`; if local passes, vary fixture seed or force HashMap iteration order to provoke divergence. Confirm failure-for-the-right-reason — i.e. ordering mismatch in a multi-entry u64/tso/i64 map, not a value corruption.
2. Audit map-iteration emit across all 9 codec generators (`{Sc,Cs,Rs,Ts,Kt,Jv,Dt,Sw,Py}{UEBA,Json}CodecGenerator.scala`). Identify every site that emits a map sort by `_._1.toString` or equivalent typed key.
3. Replace with encoded-wire-form-key sort (the same key form the decoder uses). Verify encode and decode paths agree.
4. Add multi-entry-map regression to `RTCodecTest`'s fixture set and to the m26 fixture (multi-entry mu64 + mtso + mi64).
5. Run `mdl --seq :build :test`, `:test-acceptance`, `:test-service-acceptance` to confirm green.
6. Mark PR-28.4-D02 in the archived predecessor ledger as resolved here (cross-link).

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
