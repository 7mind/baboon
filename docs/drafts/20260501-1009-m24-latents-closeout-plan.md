# M24 — Latents Closeout (PR-A through PR-J)

**Date:** 2026-05-01
**Predecessor:** M23 closeout (HEAD `bdc0201`)
**Goal:** close the deferred-latents queue from defects.md PR-57c..PR-68 plus the M20 `derived[was]` policy gap. End state: `mdl --seq :build :test-gen-{regular,wrapped}-adt :test-{cs,scala,rust,typescript,kotlin,kotlin-kmp,java,dart,swift,python}-regular` completely green; defects.md ledger reflects all closures.

## Roster overview

10 PRs in 3 phases. All PRs follow the inner review-loop (executor → adversarial review → fix → re-review → commit).

- **Phase 1** (parallel, worktree-isolated, disjoint files): PR-A..PR-E
- **Phase 2** (serial, multi-backend touch): PR-F..PR-H
- **Phase 3** (policy-gated, serial): PR-I (foreign-key-codec hook), PR-J (`derived[was]` policy)

## Decisions captured

- **Policy #1 = (c)** for Custom-foreign-as-key: codegen emits `<Foreign>_KeyCodec` interface/protocol/trait per language. Stringy-Custom allowlist gets a default impl (identity / `.toString`); non-stringy Customs require host registration.
- **Policy #2 = (b)** for `derived[was]` propagation on ADT re-emit: preserve verbatim. The receiving ADT's scope is the resolution context; unprefixed `was` resolves under the receiving ADT.

## Phase 1 — 5 parallel PRs (worktree-isolated, disjoint files)

### PR-A — Rust `isUserMapKeyEligibleDto` foreign+enum + `peelWrapperChain` extension

**Defect:** PR-65-D01 (Rust foreign/enum gap).
**Worktree:** `wt-pr-a`.
**Files:** `RsDefnTranslator.scala` lines 903-921 (`isUserMapKeyEligibleDto`) and 932-954 (`peelWrapperChain`).
**Test:** new `test/rs-stub/tests/foreign_map_key_round_trip.rs` covering wrapper-around-foreign.

### PR-B — Swift `mapExpr` `anyThr` propagation fix

**Defect:** SwJsonCodecGenerator key-`try` asymmetry (M23 deferred latent).
**Worktree:** `wt-pr-b`.
**Files:** `SwJsonCodecGenerator.scala` lines 339-351.
**Fix shape:** `keyTok = if (keyThr) try $keyDec else $keyDec` — three-arm split (key/val/both/neither).
**Test:** `Tests/RuntimeTests/MapKeyTryPropagationTests.swift`.

### PR-C — Cross-backend signed-`+` rejection (Spec §5.4)

**Defect:** PR-57d-D03.
**Worktree:** `wt-pr-c`.
**Files:** signed-int parser sites in 9 backends' JSON+UEBA generators. Per-execution grep needed for exact lines.
**Fix:** `if (s.startsWith("+")) throw BaboonCodecException("malformed signed int: leading '+' is not allowed by spec §5.4: " + s)` per language.
**Test:** 9 stub-side tests, one per backend, asserting `"+42"` rejection.

### PR-D — `BaboonTyper.deepSchemaRepr` branch sort

**Defect:** PR-64-D02 (manual→sugared lands in `deepModified` instead of `unmodified`).
**Worktree:** `wt-pr-d`.
**Files:** `BaboonTyper.scala:303-304` add `.sortBy(_.toString)`. Tighten `M20AdtEvolutionTest`.

### PR-E — `AdtInheritanceExpander` multi-`^` clarifying comment + test

**Defect:** PR-63-D04.
**Worktree:** `wt-pr-e`.
**Files:** `AdtInheritanceExpander.scala:104-128` add prose explaining union-of-targets `^` semantics.
**Test:** new `m20-adt-include/multi-intersect.baboon` fixture + `M20AdtInheritanceFrontEndTest` case.

## Phase 2 — 3 serial PRs (multi-backend touch)

### PR-F — Cross-backend malformed-key error consistency

**Defect:** PR-60-D07.
**Files:** 8 backends' map-key decoder paths. Replace silent `.toOption`-discard / `fatalError` / `ClassCastException` / `as` cast / `unwrap()` / `KeyError` with uniform `BaboonCodecException("malformed key: ${repr}")` semantics (per-language idiomatic equivalent).
**Test:** 8 negative-path tests, one per backend.

### PR-G — TS direct-builtin tuple-array unification

**Defect:** PR-60-D08.
**Files:** `TsJsonCodecGenerator.scala:240-241`. Unify on string-keyed-object form for non-string builtins.
**Test:** new cross-language fixture exercising `map[i32, str]` round-trip TS↔Scala/Rust/Java/Kotlin.

### PR-H — Rust conv-test `lib.rs` auto-routing

**Defect:** Rust hand-curated `#[path]` (M23 deferred latent).
**Files:** `test/conv-test-rs/src/lib.rs` + extend `RsBaboonTranslator` to emit `generated/mod.rs` aggregator.

## Phase 3 — 2 policy-gated serial PRs

### PR-I — Custom-foreign `<Foreign>_KeyCodec` extension hook

**Closes:** PR-60-D03 (Python), PR-60-D05 (cross-backend), PR-65-D01 residue (Rust foreign), PR-66 latent (Scala non-String Custom), PR-68-D02 (Swift `hasForeignType` filter lift).
**Policy:** (c) emit hook interface + stringy-Custom default impl + host-registration.

**Per-language interface signatures, default impls, and host-registration mechanism — see plan body in this file's predecessor (planning subagent output).**

**Allowlist for default-impl emission:**
| Language | Stringy mappings |
|---|---|
| Scala | `String`, `java.lang.String` |
| Java | `java.lang.String`, `String` |
| Kotlin | `kotlin.String`, `java.lang.String`, `String` |
| Swift | `String`, `Swift.String` |
| Rust | `String`, `&str`, `std::string::String` |
| C# | `string`, `System.String` |
| TS | `string` |
| Python | `str`, `builtins.str` |
| Dart | `String`, `dart.core.String` |

**Host-registration pattern:** module-level mutable singleton per backend (see signatures below). Distage / DI integrations set the singleton at app boot.

**Codegen call-site swap:**
- Encode: `q"$ref.toString"` (PR-66 String-only path) → `q"${foreign}_KeyCodec.instance.encodeKey($ref)"`
- Decode: language-specific stub-throws → `q"${foreign}_KeyCodec.instance.decodeKey($ref)"` (with `try` in throwing-context languages — PR-B's `anyThr` propagation becomes load-bearing)

**Internal sub-PR order (recommended for incremental review):** Scala → C#/Java/Kotlin/Dart/TS → Swift (incl. `hasForeignType` lift) → Python (silent-fallback restructure) → Rust (serde-with-adapter glue, hardest).

**Cross-language compat fixture:** new dedicated convtest model file (NOT promoting m19-ok directly), exercising `Holder { m: map[ItemKey, str] }` byte-identity across all 9 backends.

### PR-J — `derived[was]` policy (b) preserve-verbatim

**Closes:** PR-63-D05.
**Files:** `AdtInheritanceExpander.scala` re-emit step lines 197-200 — clarifying comment only (behaviour already correct since `RawAdtMemberDto` carries `dto: RawDto` whose `derived` set is copied verbatim).
**Test:** new 2-version fixture `m20-was-propagation/{v1,v2}.baboon` + `M20DerivedWasPropagationTest`.
**Plan amendment:** append "Q-PR-J" decision to `docs/drafts/20260429-0025-m20-bab-a03-adt-inheritance-plan.md` "Decisions captured" appendix.

## Open questions (escalated from planning subagent)

1. **PR-B success-criteria proxy:** does `mdl :test-swift-regular` enable warnings-as-errors? Resolve at execution time; if not, add Swift settings flag or accept compile-pass.
2. **PR-C exact line numbers:** per-backend grep needed at execution time to enumerate signed-int parser sites.
3. **PR-G strict-emit vs accept-both:** for the TS wire-form transition, recommend strict-emit (canonical wire form going forward).
4. **PR-I host-registration pattern:** mutable-singleton accommodates distage but per-call context threading might be preferred. Recommend mutable-singleton for M24; revisit threading as future hygiene.
5. **PR-I cross-language compat fixture:** new dedicated file (recommended) vs promoting m19-ok directly.
6. **PR-I non-stringy Custom foreign current fixtures:** verify none exist; if any, host-codec impls needed in stub modules.
7. **PR-I Swift `hasForeignType` filter lift:** verify no fixture relies on the filter for compile-skipping.
8. **PR-J `was` resolution scope:** confirm `Owner.Adt(receivingAdtId)` is intended for unprefixed refs.

## Sequencing

Phase 1 PRs are merged in any order (parallel execution). Phase 2 PRs serialise: PR-F → PR-G → PR-H. Phase 3 PRs serialise: PR-I → PR-J.

## Final verification

```
mdl --seq :build :test-gen-regular-adt :test-gen-wrapped-adt \
  :test-cs-regular :test-scala-regular :test-rust-regular \
  :test-typescript-regular :test-kotlin-regular :test-kotlin-kmp-regular \
  :test-java-regular :test-dart-regular :test-swift-regular :test-python-regular
```

All 11 actions PASS.

## Skipped (recommended NOT to close)

- **PR-63-D03** (`+ X.Foo` test): Q-FU-2 explicitly disposes as "Optional, not requested; implement only if trivial". Adding a test legitimizes a feature deliberately not blessed.

## Already closed (no action)

- PR-64-D01 (Rust `visit_map<A>`): closed by PR-65.
- PR-68-D04 (Swift defects ledger gap): self-resolved by recording PR-68-D01.
