# Baboon — Task Ledger

Authoritative ledger of planned and completed work.

> **Predecessor ledgers (frozen, chained):**
> - `docs/archive/20260505-m31-close-ledgers/{tasks,defects}.md` — M29 → M31 + M32-prep (BAB-A04 generics, M30 docstrings, M31 upstream defects, M32 wire-version bump).
> - `docs/archive/20260503-bab-any-anyopaque-ledgers/{tasks,defects}.md` — M1–M28 (`any`/AnyOpaque, identifiers, ADT inheritance, JSON/UEBA codecs, map-key encoding, wire-form canonicalisation).
>
> Locked invariants in both archives remain authoritative. Active entries below supersede their milestone-tracking entries, never their locked invariants.

Status: `[ ]` planned · `[~]` in progress · `[x]` done · `[!]` blocked

---

## Active brief

Upstream the multi-version codec facade described in `./proposal.md` —
self-describing envelope (binary + JSON) atop existing per-version codecs +
per-domain registry/dispatch with cross-version decode and conversion chains.

Most facade infrastructure already exists (`baboon-runtime/*/BaboonCodecsFacade.*`,
`BaboonTypeMeta.*`); the work is pin-the-spec, align with the proposal,
generate per-domain registration classes, and decide the unified envelope
byte. Open questions in `docs/drafts/20260505-1830-questions-multi-version-facade-upstream.md`
gate Phase 1+. Phase 0 (this ledger reset) does not require answers.

## M32 carry-over

- [~] **PR-32.1** — `META_VERSION_1` 1→16 bump. Byte already lifted in all 11
  runtime files at HEAD `0d9d7165`. Per-backend test fixtures touched in the
  same commit (`AnyMetaCodecTests` across cs/dt/jv/rs/sw/ts). Close-out folded
  into the upstream-facade plan: PR-32.1 will be re-decided (kept at 16, or
  rolled back to 1) per Q2 of the open-questions draft.

- [x] **PR-32.3** (2026-05-05) — Windows-only CI flake in `test-sc-wiring-result`:
  `runtime.ForeignMapKeyRoundTripSpec` failed with decoded keys carrying an `A:`
  prefix that originated from `KeyCodecHostLastWinsSpec`'s in-flight
  `register(PrefixCodec("A"))`. Race on the process-global mutable
  `FStr_KeyCodec.instance` singleton: scalatest runs suites in parallel (sbt
  default), so a sibling spec can read the singleton between the two
  `register(...)` calls in `KeyCodecHostLastWinsSpec`. Linux happened to
  schedule sequentially; Windows did not. The `after { register(IdentityCodec) }`
  teardown (PR-26.2-D01) restores cleanly post-test but cannot prevent in-flight
  reads. Fix: `Test / parallelExecution := false` in `test/sc-stub/build.sbt`.
  Stub is small; serial cost is negligible. Local
  `mdl --simple-log :build :test-sc-wiring-result` green.

- [x] **PR-32.2** (2026-05-05) — CI hot-fix. Commit `0d9d7165` missed three
  sites that hardcoded the literal `"1"` in JSON `$mv` handling:
    1. `swift/baboon_runtime.swift:1358` reader compared `mvStr == "1"` →
       rejected explicit `"$mv": "16"`. CI test
       `AnyMetaCodecTests.testReadMetaJson_acceptsExplicitMvOne` failed.
    2. `swift/baboon_runtime.swift:1441` writer emitted `"$mv": "1"` literal
       (binary path correctly wrote byte 16) → cross-format inconsistency.
    3. `dart/baboon_runtime.dart:1100, 1188` same residue (latent, locally
       self-consistent because reader and writer both used the stale `'1'`
       — but produced JSON envelopes that disagreed with the binary byte).
    4. `test/sw-stub/.../AnyMetaCodecTests.swift:293` constructed
       `BaboonTypeMeta(1, ...)` literal in a writeJson↔readMetaJson roundtrip;
       reader returns canonical `BaboonTypeMetaCodec.metaVersion`, so the
       structural-equality assertion failed.
  Fix: aligned Swift and Dart writers with cs/java/rust/scala/ts/python — drop
  `$mv` entirely on write (canonical version is the implicit default). Readers
  now compare against `String(metaVersion)` instead of literal `"1"`. Test
  fixture updated to use `BaboonTypeMetaCodec.metaVersion`. Local
  `mdl --simple-log :build :test-swift-regular`, `mdl :test-dart-regular` both
  green.
