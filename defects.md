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
**Status:** open
**Severity:** major
**Location:** `.github/workflows/baboon-build.yml` (acceptance-tests job step that invokes `nix develop --ignore-environment --command mdl --github-actions :test-acceptance`); possibly `flake.nix` and `.mdl/defs/actions.md`.
**Description:** Acceptance job fails at sbt-launcher startup with:
```
[error] [launcher] xsbt.boot.internal.shaded.coursier.error.ResolutionError$CantDownloadModule: Error downloading org.scala-sbt:sbt:1.11.7
  forbidden: https://repo1.maven.org/maven2/org/scala-sbt/sbt/1.11.7/sbt-1.11.7.pom
  forbidden: https://repo.scala-sbt.org/scalasbt/maven-releases/org/scala-sbt/sbt/1.11.7/sbt-1.11.7.pom
  not found: ... (mirror endpoints)
[error] [launcher] could not retrieve sbt 1.11.7
❌ Error: Script exited with code 1
```
External availability confirmed: sbt 1.11.7 returns `HTTP/2 200` from `repo1.maven.org` outside CI. Build job (`build-linux-amd64`) in the same workflow run uses `nix develop` (no `--ignore-environment`) and resolves sbt cleanly. Cross-job comparison isolates `--ignore-environment` as the differentiator.
**Root cause (hypothesis):** `--ignore-environment` strips runner-level proxy / cert / DNS settings (the GitHub-hosted runner relies on env-injected proxy config for outbound HTTPS), and the resulting sandboxed connection is rejected at upstream CDN as `forbidden` — or the strict sandbox simply has no working DNS/cert chain, with Cloudflare returning `403` on a malformed request.
**Suggested fix:**
1. Read `.github/workflows/baboon-build.yml` to confirm the exact invocation and whether `--ignore-environment` is load-bearing for this step (it is unlikely to be).
2. Pick the lowest-blast-radius fix:
   - (a) **Drop `--ignore-environment`** from the acceptance step. Lowest blast radius; acceptance does not need a stricter sandbox than build.
   - (b) Pre-populate sbt-launcher into the Nix flake closure so the sandboxed step never reaches the network. Higher invariance but larger change to `flake.nix`.
   - (c) Prepend `:flake-refresh` to the acceptance command (warm cache before the strict sandbox kicks in). Fragile; depends on caching policy.
3. Apply (a) unless evidence emerges that the sandbox is load-bearing.
4. Validate with the live workflow on `wip/ids-and-adts` after pushing.
