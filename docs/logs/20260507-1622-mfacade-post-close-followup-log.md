# MFACADE post-close follow-up — PR-A through PR-D

**Session:** 2026-05-07 (continuation of close session 04:06).
**Predecessor log:** `docs/logs/20260507-0406-mfacade-close-log.md` — MFACADE
milestone close-out, PR-1 through PR-8.

## Original orchestrator brief

User invoked `<<autonomous-loop-dynamic>>` after MFACADE close to drive the
deferred follow-up items captured in the close log's "Deferred / follow-up
work" section (PR-6 default-on per backend, PR-7 conformance tests,
cross-backend asymmetries, kt/kt-kmp `encodeToJson` asymmetry). After the
first three follow-up PRs (A/B/C) the user kept the loop running, which
surfaced a fourth (D) closing the rust gap that PR-C had to defer.

## Outcome

Four post-close PRs landed. Three of the four close-log deferred items are
now resolved; PR-7 conformance tests remain explicitly deferred (out of
scope for this session — large surface, stable behaviour pinned by the
acceptance harness).

## PRs landed

### PR-A — D11/D12 reject-everywhere + kt encodeToJson envelope alignment (commit `1ae07cd4`)

User-decided fixes for three deferred items from MFACADE-PR-6's residual
list. Decisions logged in conversation:
- **D11 = reject** (option b): Dart and Python now reject envelopes with
  explicit `null` `$mv`. Dart `readMetaJson` uses `Map.containsKey(r'$mv')`
  + null check; Python `read_meta_json` uses `key in dict` + `is None`
  check. Both backends previously fell through to the absent-key branch on
  explicit null.
- **D12 = reject** (option b): Swift `BaboonTypeMeta.readMetaJson` now
  rejects `Double 1.0` for `$mv` even when `JSONSerialization` hands it
  back as `NSNumber`. Discriminates integer-vs-double `NSNumber` via
  `objCType` (typecode `d`/`f` → reject). Sc/ts limitation (circe/JSON.parse
  collapse whole-valued doubles to ints) documented in spec § 4 rather
  than fixed.
- **kt encodeToJson = align** (option a): kt + kt-kmp `BaboonCodecsFacade.encodeToJson`
  emits the full `{$mv,$d,$v,$t,$uv,$c}` envelope via `buildJsonObject`,
  replacing the prior content-only output that was asymmetric with
  `decodeFromJson` (which expected an envelope).

Closes `[MFACADE-PR-3-D11]` and `[MFACADE-PR-3-D12]` in defects.md.
Resolves the kt/kt-kmp `encodeToJson` content-vs-envelope asymmetry called
out in the close log's deferred list.

### PR-B — PR-6 default-on flip across 8 backends (commit `5de15f55`)

Flipped `generateDomainFacade.getOrElse(false)` → `getOrElse(true)` for
sc/py/rs/ts/kt/kt-kmp/jv/dt/sw in `Baboon.scala`. The pre-condition (per-
backend codegen issues called out in PR-6) was satisfied by the round of
verification work done during PR-A. C# was already default-on as the
proven pilot. PR-B closes the close-log's "PR-6 default-on per backend"
follow-up.

### PR-C — Per-stub DomainFacade tests for 8 backends + TS facade import-path fix (commit `a14b44ee`)

Added `DomainFacade*` test files under each per-language stub project so
the codegen output of MFACADE-PR-6 (per-domain `Domain<Pkg>Facade` auto-
registration) gets a unit-level smoke or round-trip check, alongside the
existing `test/cs-stub/BaboonTests/DomainFacadeTests.cs` pilot.

Coverage by backend:
- cs (pre-existing pilot): full encode/decode round-trip via `DomainMyOkFacade`.
- sc / java / kotlin / kotlin-kmp / python / typescript: full round-trip.
- swift: ctor + `latest("my.ok")` smoke. Direct round-trip needs generated
  DTOs to conform to `BaboonMetaProvider` (deferred — the runtime surfaces
  this with an explicit "PR 9.2/9.3" pointer).
- dart: ctor + `latest("my.ok")` smoke (matches the existing dart pattern;
  full round-trip exercised by the cross-language acceptance harness).
- rust: deferred to PR-D.

Incidental TypeScript codegen fix uncovered during verification: generated
`Domain<X>Facade.ts` had a hardcoded `'../BaboonSharedRuntime'` import
(one-level `..`) which only resolved when the facade lived one directory
under `generated/`. For deeper packages (e.g. `my.ok` → `my/ok/`) it
resolved to a non-existent file and the TS suite failed to load. Now
computes `'../' * latestBasename.split('/').length` from `latestBasename`'s
segment count.

### PR-D — Rust facade codegen fix + 9th backend test restored (commit `5921b5de`)

Closes the rust gap from PR-C. Three defects in
`RsBaboonTranslator.generateDomainFacade` / `generateModFiles`:

1. **Wrong `fullPath` for namespaced types.** `collectTypes` ignored
   `defn.id.owner.asPseudoPkg` when building the rust path, so `In` in
   `ns input` of `testpkg.pkg0` resolved to
   `crate::testpkg::pkg0::input::In` rather than
   `crate::testpkg::pkg0::<ns…>::input::In`. Wrong for every namespaced
   type. Fixed by prepending `nsParts` (matching
   `RsDefnTranslator.getOutputPath`'s file layout).

2. **Dyn wrapper symbol clash across same-named types in different
   namespaces.** Base name was `${typeName}` only; `Clash` at top level,
   `Clash` in `ns clash`, and `Clash` in `ns clash.clash` (pkg03.baboon)
   all flattened to `ClashV3_0_0Dyn` and produced E0119 conflicting
   implementations. Fixed by prefixing the base with the PascalCase
   namespace path (`Clash` → `Clash`, `ns clash/Clash` → `ClashClash`,
   `ns clash.clash/Clash` → `ClashClashClash`).

3. **Generated facade not declared in enclosing `mod.rs`.**
   `generateModFiles` filtered `doNotModify=true` outputs out of the path
   scan, so the per-domain facade — the only `doNotModify=true` output
   that lives inside a domain dir tree (runtime files live at the crate
   root and are wired via `lib.rs`) — never got a `pub mod
   domain_<x>_facade;` declaration. Fixed by including all outputs in
   the path scan; the `doNotModify` filter is preserved on the ADT-
   detection sub-pass only.

Plus gates facade generation on the four flags it actually depends on
(`generateDomainFacade && generateUebaCodecs && generateUebaCodecsByDefault &&
generateJsonCodecs && generateJsonCodecsByDefault`). Without those the
enumerated per-type registrations reference `BaboonBinEncode`/`BaboonBinDecode`
traits that some types don't implement (visible in `conv-test-rs`, which
sets neither byDefault flag).

Restores `test/rs-stub/tests/domain_facade_tests.rs` as a smoke test (ctor
+ `latest("my.ok")` resolution); full round-trip is exercised by `mdl
:test-acceptance` since user-side construction of the per-version
`BaboonGeneratedDyn` wrappers is private to the generated module — same
constraint as Dart and Swift.

## Process notes (carry into future work)

- **Pipe-with-tail hides exit code.** `mdl ... | tail -200 &` always
  returns the tail's exit code, which hides mdl failures. Trust the
  per-action `output.json` `success.value` field instead, or the
  `❌ Execution failed!` marker that mdl writes near EOF. The task-
  notification's "exit code 0" is the pipeline's exit, not mdl's.
- **Run dirs auto-clean on success.** `mdl` removes its run directory
  when the goal succeeds, which can leave a *stale* run dir on disk
  from a previous failed attempt — `ls -td` will name that one. Cross-
  reference timestamps with the actual task output before reading
  `output.json` paths.
- **`mdl :test`'s `--seq` failure mode prunes downstream actions.**
  When `test-python-regular` failed in iteration 1 of PR-C, mdl in
  `--seq` mode aborted the rest of the matrix — `test-{rust,scala,
  typescript,swift}-regular` never ran. Python passed in iteration 2;
  the previously-skipped actions then ran and surfaced fresh issues
  in iteration 3. **Don't mistake "seq stopped after first failure"
  for "everything else passed."**
- **Codegen bugs in low-traffic paths surface late.** PR-D's three
  rust defects had been latent since PR-6 — every PR-6 verification
  used `my.ok` (no namespaces, no clashing names), so the bugs only
  fired when PR-C tried to wire the facade through `mod.rs` and
  pkg03's three-level `Clash` cascade hit the symbol clash. **A
  green stub-only test is not evidence of cross-namespace
  correctness.**
- **Per-domain-facade APIs are fundamentally constrained on Rust /
  Dart / Swift.** Encoding requires a `BaboonGeneratedDyn`-equivalent
  wrapper which is private to the generated module, so user code can
  only construct the facade and read `latest(...)` — not round-trip
  via the facade directly. This is expected; the cross-language
  acceptance harness (`mdl :test-acceptance`) is the round-trip
  contract for these backends.
- **Swift's `BaboonMetaProvider` conformance is a real gap.** The
  Swift runtime explicitly references "PR 9.2/9.3" in its error
  message — generated DTOs don't conform yet. Swift smoke tests are
  honest about this. A future PR that adds the conformance unblocks
  swift's full round-trip via the facade.

## Deferred / follow-up work (still pending)

The following remain explicitly deferred from the MFACADE close log:

- **PR-7 conformance tests** (still deferred, intentionally):
  - Per-backend envelope round-trip tests (golden bytes × bin/JSON ×
    9 backends).
  - Captured-byte fixtures under `test/conv-test/captured/` against
    `/tmp/exchange/Baboon` (read-only — bytes only, not project
    names per Q5).
  - The cross-language acceptance harness (200/200 cross-language
    round-trips) already exercises the wire format end-to-end;
    explicit golden bytes catch silent drift but aren't blocking.

- **Generated-DTO `BaboonMetaProvider` conformance on Swift** ("PR
  9.2/9.3" per the runtime's own pointer). Unblocks swift's full
  round-trip via the facade — currently smoke-only.

- **Java byte-range bound** (`[MFACADE-PR-3-D07]`, severity nit):
  signed `Byte.MIN_VALUE..Byte.MAX_VALUE` instead of unsigned 0..255.
  Immaterial while `META_VERSION_1 == 1`.

- **Rust private-wrapper exposure** for full facade round-trip from
  user code. Not a defect — the design constraint is intentional
  ("the per-version `BaboonGeneratedDyn` wrappers required by
  `encode_to_bin` are private to the generated facade module"). If a
  future user demands round-trip via the facade on rust, expose
  `into_dyn()` per type or a typed `encode_to_bin<T>(value: T)`
  that wraps internally.

## Verification

`mdl --simple-log --seq :build :test` matrix green at end of each PR.
Final post-PR-D run: `bakf2bcna` task, ~30 min wall, all per-language
test actions passing including `test-rust-regular`, `test-rust-wrapped`,
`test-scala-regular/wrapped`, `test-swift-regular/wrapped`,
`test-typescript-regular/wrapped`, `test-dart-regular/wrapped`,
`test-{cs,java,kotlin,kotlin-kmp,python}-regular/wrapped`, all 4 wiring
matrices, all `test-gen-compat-*`, all `test-manual-*`, plus orchestrator
`test`.

## Reference

- Predecessor close log: `docs/logs/20260507-0406-mfacade-close-log.md`.
- Plan: `docs/drafts/20260506-0000-mfacade-and-m32-plan.md`.
- Spec: `docs/spec/codec-envelope.md`.
- Decisions: `docs/drafts/20260505-1830-questions-multi-version-facade-upstream.md`
  (Q1–Q14).

## Milestone status

MFACADE remains closed (predecessor close log governs); this session
landed four post-close follow-up PRs that close three of the four
close-log deferred items. No new milestone opened. Loop returns to user
with no active milestone, ledger drained, PR-7 conformance tests as the
only known follow-up explicitly deferred.
