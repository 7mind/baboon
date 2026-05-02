# M28 closeout — wire-form parity hardening + docs catch-up

**Date:** 2026-05-02 (continuing same-day from M27 close).
**Branch:** `wip/ids-and-adts`.
**Final commit this session:** `0f0fc67` (PR-28.4).
**Predecessor:** M27 complete log `docs/logs/20260502-1837-m27-editors-lsp-catchup-log.md`.

## Original user request

> Scala u64 JSON KeyDecoder and wire form divergence must be fixed before we proceed. Also, fix claude.md and reflect newly added features in our docs!

## Outcome

**5 of 6 planned PRs shipped + 2 surfaced hot-fixes + 4 newly-filed deferred defects = 7 commits.** PR-28.2 (f64 cross-backend canonicalisation) deferred for the second time after per-backend audit confirmed TS+Swift fixes are architecturally bounded by the language's standard JSON serialiser (requires public-API-breaking change).

| PR / Fix | Commit | Closes |
|---|---|---|
| PR-28.1 | `37e04a0` | Scala u64 JSON `KeyDecoder` + value encoder. Closes M26-N02(b). |
| PR-28.6 Wave 1 | `735290e` | Docs catch-up M18-M27 (`language-features.md`, `json-codecs.md`, `README.md`). Closes [PR-28.6-D01]. |
| PR-28.3 + 2 hot-fixes | `b85fc30` | tso `±HH:MM` cross-backend (Scala/Java/Kotlin/C#/TS); D01 C# TsuToString offset preservation; D02 Java u64 map-key encode parity (Kotlin: ULong native, no fix needed). Closes M26-N02(c) tso portion. |
| PR-28.5 | `66cb938` | Stale package path in `docs/spec/identifier-repr.md:510`. CLAUDE.md was already clean. |
| PR-28.2 (deferred) | `dc47d6e` | Per-backend f64 audit + 2nd deferral. 6 of 9 backends already canonical; TS+Swift+C# need fixes but TS/Swift require root-pipeline architectural change. |
| PR-28.4 | `0f0fc67` | Extend m26 fixture from 5/8 to 7/8 types: added `mu64` + `mtso`. New canonical md5 (JSON: `e64806e756580aed029371bec1a3bf5b`, UEBA: `567af7bab6b7ef40fb23c2521a5b081d`). |

## M28 final verification gates

- `mdl --seq :build :test` — **PASS** (82 actions; previously failing test-cs-wiring-* + test-kotlin-kmp-regular both green after the 2 hot-fixes + Kotlin revert).
- `mdl :test-acceptance` — **PASS** (200/200 cross-language JSON+UEBA round-trip matrix).
- `mdl :test-service-acceptance` — **PASS**.

## Defects surfaced during M28

- **PR-28.3-D01** (resolved) — C# TsuToString unconditionally using TsuDefault dropped offset on round-trip when source tsu had non-UTC Kind. Restored legacy branching `(Offset.Ticks == 0 && Kind == Utc) ? TsuDefault : TszDefault`.
- **PR-28.1-D02** (resolved) — Java u64 map-key encodeKey fell through to `String.valueOf(long)` (signed) while decoder uses `parseUnsignedLong`. Added explicit u64 arm. Kotlin u64 → ULong; ULong.toString() is unsigned natively (no Kotlin change).
- **PR-28.2-D01** (deferred) — f64 cross-backend canonicalisation. 6 of 9 backends emit `<int>.<frac>` natively; TS+Swift require public-API-breaking change to JSON.stringify / JSONSerialization. Architectural decision needed.
- **PR-28.4-D02** (deferred) — Generator-wide map-iteration sort-key uses `_._1.toString` (typed) rather than encoded wire-form key. Sidestepped via single-entry maps in m26 fixture.
- **PR-28.4-D03** (deferred) — Python pydantic emits microseconds for tso map-key JSON. Needs per-type datetime serializer in `PyJsonCodecGenerator.scala`.
- **PR-28.4-D04** (deferred) — Python `LEDataOutputStream.write_datetime` writes different `kind`-byte semantics than the other 9 backends.

## In-band fixes shipped

Beyond the planned PRs, M28 surfaced and fixed:
1. C# TsuToString offset preservation regression (PR-28.3-D01).
2. Java u64 map-key encode parity (PR-28.1-D02).
3. Scala+Kotlin codec generators previously emitted non-existent `BaboonTimeFormats.format(...)` for tsu/tso map keys — first cross-language tso-as-map-key fixture surfaced this latent codec defect. Both fixed to dispatch per-type (`formatTsu`/`formatTso`).
4. Resource macro cache invalidation: confirmed CLAUDE.md `sbt clean` rule is load-bearing for runtime template edits; mdl :build alone doesn't trigger re-embed.

## Cross-cutting architectural notes locked

- **M28-N01 — Wire-form canonical for f64 and tso.** tso = `±HH:MM` always; UTC = `+00:00`, NOT `Z`. tsu retains `...Z` semantics for genuine UTC-Kind values. Decoders MUST tolerate the alternative form. **f64 canonical decision deferred** to a future architectural milestone.
- **M28-N02 — Scala u64 JSON map-key encoding strategy.** Encode = `java.lang.Long.toUnsignedString(v)`; decode = `BaboonJsonCodec.decodeKeyU64` (BigInt parse → range-validate → narrow to Long). Same fix shape applied to Java (PR-28.1-D02). Kotlin u64 → ULong, no fix needed (ULong.toString() unsigned native).

## Lessons for M29+

1. **Resource macro cache (CLAUDE.md note) is load-bearing.** PR-28.3 round-1 looked correct but tests failed because `mdl :build` didn't re-embed the runtime template — `sbt clean` was required. Future runtime-template work should default to clean rebuild.
2. **PR-28.2 deferral is architectural, not tactical.** TS `JSON.stringify` and Swift `JSONSerialization` are root-of-pipeline; forcing canonical f64 emit requires either replacing them globally or accepting `<int>` form across all 10 backends. Project-level decision needed before proceeding.
3. **Cross-language map-key fixtures surface latent codec defects.** PR-28.4 added `mtso` and surfaced the Scala+Kotlin `format(...)` (vs `formatTsu`/`formatTso`) defect. Future fixture extensions should expect to surface latent codec bugs.

## Forward-tracked items

- **f64 canonical** (PR-28.2-D01) — needs project-level decision: `<int>.<frac>` (force runtime change in TS+Swift) vs `<int>` (revert canonical, accept reduced byte savings).
- **Generator map-iteration sort-key** (PR-28.4-D02) — uses typed `_._1.toString` instead of encoded wire-form key. Multi-entry u64/tso maps would expose order divergence.
- **Python tso microsecond emission** (PR-28.4-D03) — per-type datetime serializer needed.
- **Python LEDataOutputStream kind-byte semantics** (PR-28.4-D04) — divergent from 9 other backends.
- **PR-28.6 Wave 2** — f64/tso canonical wire-form lock-in subsection in `docs/json-codecs.md` (deferred until PR-28.2 architectural decision).

## Session-end disposition

M28 closed per /review-loop O5. Final CI-equivalent close gate (`mdl --seq :build :test`, `:test-acceptance`, `:test-service-acceptance`) all PASS. Returning to user with:

- 7 commits across 5 shipped PRs + 1 deferred + 2 hot-fixes.
- m26 fixture coverage 5/8 → 7/8 types.
- 3 cross-cutting invariants locked (M28-N01..N02 + the M28-N01 f64-deferral note).
- 4 forward-tracked deferred defects with clear resolution paths.
- CI-equivalent gates green.

User goals achieved: Scala u64 KeyDecoder fixed (PR-28.1); tso wire-form harmonised (PR-28.3); CLAUDE.md audited clean (no edit needed); user-facing docs caught up to M18-M27 (PR-28.6 Wave 1 + path-fix PR-28.5). f64 canonical deferred with documented rationale.
