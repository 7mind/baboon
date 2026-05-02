# M25 — Cross-Backend Hygiene

**Date:** 2026-05-02
**Predecessor:** M24 closeout (Phase 3 finished `2026-05-01`).
**Goal:** Close all 11 open hygiene defects spanning Kotlin / Python / Dart / TS / Swift backends, with **UEBA correctness across backends as the top priority**. Headline correctness defect is PR-15-D01 (Kotlin indexed-mode UEBA encoder emits unused-lambda wrappers, so indexed encode produces empty bodies). Other items bundle into 7 follow-on PRs. End state: `mdl --seq :build :test` and the cross-language compat matrix completely green; defects.md ledger reflects all closures or explicit `wontfix`/`verified` flips.

## Roster overview

11 open defects fold into **8 PRs**, including 2 zero-code closures.

- **PR-25.1** — Kotlin UEBA indexed-mode block-expression fix (closes PR-15-D01). HIGH.
- **PR-25.2** — Python facade hygiene bundle (closes PR-26-D03 + PR-26-D04).
- **PR-25.3** — Dart `convert<T>` `(from, to)` registry-key fix (closes PR-22-D05).
- **PR-25.4** — TS reader hygiene (closes PR-19-D05 verification + PR-19-D06).
- **PR-25.5** — KMP test parity (closes PR-14-D02 + PR-15-D02). 25.5a mechanical port; 25.5b for any newly surfaced defects.
- **PR-25.6** (zero-code) — Verify Swift `mayThrow` propagation post-PR-I.2 and close PR-30-D06.
- **PR-25.7** (zero-code) — Verify Dart conv-test mudyla asymmetry intentional and close PR-22-D07.
- **PR-25.8** — TS `BaboonMetaProvider`/`BaboonAdtMember` interface emission in codegen (closes PR-22-D02).

## Phase 1 — UEBA correctness (top priority)

### PR-25.1 — Kotlin UEBA indexed-mode `{ … }` block-expression fix

**Closes:** PR-15-D01 (high — silently broken indexed-encode for any DTO with fields, all 4 generated arms: regular/wrapped × JVM/KMP).

**Scope:**
- File: `baboon-compiler/src/main/scala/io/septimalmind/baboon/translator/kotlin/KtUEBACodecGenerator.scala`, function `fieldsOf` lines ~363-396.
- Two emission sites both wrap per-field encode in `q"""{ ... }"""`:
  - `BinReprLen.Fixed` arm.
  - `BinReprLen.Variable` arm.
- Both feed `indexedBody` via `fields.map(_._3).joinN()`. Single emitter feeds both `multiplatform=true` and `multiplatform=false` arms — JVM and KMP both fix in one edit.
- **Fix shape:** drop the curly braces — `// fXxx: ...` comment markers are sufficient visual separators, surrounding `try { ... } finally { ... }` already provides scope. Compact (non-indexed) branch already uses bare statements via `fields.map(_._1).joinN()`; aligning indexed emission to bare statements is the symmetric fix.
- **Does NOT change:** Compact mode emission, decode side, JSON codec.

**Acceptance criterion:**
1. Restore `ueba_round_trip_withUseIndicesTrue_preservesContent()` in `test/kt-stub/src/test/kotlin/runtime/AnyRoundTripTest.kt` (replacing the deferral comment block at lines ~159-164). Asserts encoded bytes for a non-empty payload are larger than just header (`bytes.size > 16`) AND that decode round-trips equal the source.
2. On `main` (pre-fix): test FAILS with EOF / empty-buffer mismatch. After fix: PASSES.
3. Commands:
   ```
   mdl :build :test-kotlin-regular :test-kotlin-wrapped
   mdl :build :test-kotlin-kmp-regular :test-kotlin-kmp-wrapped
   mdl :test-gen-compat-kotlin :test-gen-compat-kotlin-kmp
   ```

**Risk / unknowns:**
- Two emission sites must both be patched. Adversarial-review checklist item.
- KMP variant uses the same source file — verified by inspection.

**Estimated complexity:** Trivial-Small. ~6 line codegen change + ~15 line test.

**Dependencies:** None.

---

## Phase 2 — Python + Dart correctness bundle

### PR-25.2 — Python facade hygiene (cross-format helper + import path)

**Closes:** PR-26-D03, PR-26-D04.

**Scope:**
- **D03 fix:** `baboon-compiler/src/main/resources/baboon-runtime/python/baboon_codecs_facade.py:~442` — `json_to_ueba_bytes` passes parsed dict to `json_codec.decode` which expects a string. Wrap with `json.dumps(json_value)`. Verify symmetric `ueba_to_json` is unaffected (encode returns string).
- **D04 fix:** `PyConversionTranslator` emits `from Generated.<pkg>.v<old>.abs import core` then dereferences `core.OldAbsAdt`. Codegen path is wrong — change to `from Generated.<pkg>.v<old>.abs.core import OldAbsAdt as <alias>` and rewrite type-ref accordingly.

**Acceptance criterion:**
1. **D03 repro:** new `test/py-stub/BaboonTests/runtime/test_facade_cross_format.py` calling `json_to_ueba_bytes(meta, dict_payload, ...)` — FAIL on `main` (`pydantic.ValidationError`), PASS after fix.
2. **D04 repro:** smoke-import test for `from_1_0_0_*` modules — FAIL on `main` (`AttributeError`), PASS after fix.
3. Commands:
   ```
   mdl :build :test-python-regular :test-python-wrapped :test-manual-python
   ```

**Risk / unknowns:**
- D03's `json.dumps` wrap is semantically sound but slower than the eventual interface alignment (future M-cleanup).
- D04's import-alias requires codegen to know the leaf module name — verify `PyConversionTranslator` has it; fallback emits `__init__.py` re-exports.

**Estimated complexity:** Small.

**Dependencies:** None.

---

### PR-25.3 — Dart `convert<T>` `(from, to)` registry-key fix

**Closes:** PR-22-D05.

**Scope:**
- File: `baboon-compiler/src/main/resources/baboon-runtime/dart/baboon_runtime.dart:~749-769`. `AbstractBaboonConversions._registry` keyed only by `fromTypeId`.
- **Fix shape:** key registry on `(fromTypeId, toTypeId)`. Add `String get toTypeId` to `AbstractConversion`. Update codegen `DtConversionTranslator.scala` to emit `String get toTypeId => "<targetTypeId>";` on every generated conversion class.
- **Does NOT change:** Wire format, byte identity, `convert<T>` call signature on the facade.

**Acceptance criterion:**
1. New `test/dt-stub/test/runtime/conversion_registry_test.dart`: register two `AbstractConversion` impls with same `fromTypeId` but different `toTypeId`. Assert correct dispatch. FAIL on `main` (second registration overwrites first), PASS after fix.
2. Cross-version regression: `mdl :test-dart-regular :test-dart-wrapped :test-manual-dart`.
3. Cross-language byte identity: `mdl :test-gen-compat-dart`.

**Risk / unknowns:**
- Adding `String get toTypeId` to `AbstractConversion` is technically a Dart API break; all callers in the test corpus are codegen-emitted.
- Java has the same bug (PR-17-D05) — explicitly out of scope per cross-cutting note §2; Java tracked for future hygiene.

**Estimated complexity:** Small-Medium.

**Dependencies:** None.

---

## Phase 3 — TS reader hygiene

### PR-25.4 — TS `BaboonBinReader.readByte()` re-verification + `versionMinCompat()` empty-string

**Closes:** PR-19-D05 (verification only — bounds check already in place per planning inspection), PR-19-D06.

**Scope:**
- **D05:** Add a regression test `test/ts-stub/src/runtime-tests/BinReader.test.ts` invoking `readByte()` past EOF asserting `BaboonDecoderFailure` is thrown. Bounds check already at `BaboonSharedRuntime.ts:~106-113`.
- **D06:** `BaboonSharedRuntime.ts:~908`. Drop the falsy guard `!this.domainVersionMinCompat` (typed `readonly string`, never undefined). Add clarifying comment documenting that empty-string is treated explicitly. Add regression test asserting empty-string returns a `BaboonDomainVersion`, not `undefined`.

**Acceptance criterion:**
1. Both tests new; D05 PASSES on both `main` and post-fix; D06 FAILS on `main` (returns `undefined`), PASSES post-fix (returns explicit version).
2. Command: `mdl :test-typescript-regular`.

**Estimated complexity:** Trivial.

**Dependencies:** None.

---

## Phase 4 — Coverage extension

### PR-25.5a — KMP runtime test parity with JVM (mechanical port)

**Closes:** PR-14-D02, PR-15-D02 (coverage gap; `kt-stub-kmp/` has no `src/test/` tree).

**Scope:**
- Mirror `test/kt-stub/src/test/kotlin/` into `test/kt-stub-kmp/src/test/kotlin/`. Both modules are configured `kotlin("jvm")`, so JUnit5 runners compose cleanly.
- Update `test/kt-stub-kmp/build.gradle.kts` to add the `test` source set + JUnit5 dep block matching `test/kt-stub/build.gradle.kts`.
- Mirror exactly: `AnyMetaCodecTest.kt`, `AnyRoundTripTest.kt`, `BaboonExtensionsTest.kt`, etc.
- Verify `mdl :test-kotlin-kmp-regular :test-kotlin-kmp-wrapped` actually runs the new test source set.

**Acceptance criterion:**
- `mdl :test-kotlin-kmp-regular :test-kotlin-kmp-wrapped` runs the mirrored tests and they PASS (assuming PR-25.1 has landed first to fix the indexed-mode bug).

**Risk / unknowns:**
- Mirror may surface KMP-specific defects. Each becomes its own PR (PR-25.5b-i) under M25 inner loop.

**Estimated complexity:** Medium (mechanical port + gradle wiring + verification).

**Dependencies:** **MUST land after PR-25.1** so the indexed-mode bug is fixed before KMP tests run.

---

## Phase 5 — Zero-code closures

### PR-25.6 — Verify Swift `mayThrow` propagation post-PR-I.2

**Closes:** PR-30-D06.

**Action:** Inspect `SwJsonCodecGenerator.scala` map-decoder aggregator post-PR-I.2 (which shipped 2026-05-01). Confirm map-decoder returns `(mapExpr, keyThr || valThr)` — not just `valThr`. Defects.md PR-I.2 entry explicitly states this is in place. Verify and close as `resolved (verified post-PR-I.2)`. If `keyThr || valThr` is NOT in place, escalate to a real fix PR.

**Estimated complexity:** Trivial. Zero code on success.

---

### PR-25.7 — Verify Dart conv-test mudyla asymmetry intentional

**Closes:** PR-22-D07.

**Action:** `.mdl/defs/tests.md` `test-gen-manual` action — verify the conv-test model does NOT emit `baboon_fixture.dart` because `--fixture-output` is NOT passed. Add a 2-line clarifying comment near the conv-test-manual block explaining the asymmetry vs `test-gen-regular-adt`/`test-gen-wrapped-adt` (which DO pass `--fixture-output`).

**Acceptance criterion:** Comment lands; defect closed.

**Estimated complexity:** Trivial.

---

## Phase 6 — TS interface emission (closure of last open defect)

### PR-25.8 — TS codegen emits `BaboonMetaProvider` / `BaboonAdtMember` interfaces

**Closes:** PR-22-D02.

**Scope:**
- `baboon-compiler/src/main/scala/io/septimalmind/baboon/translator/typescript/TsDefnTranslator.scala` (and any TS sub-translators emitting DTOs / ADT members).
- TS DTO classes must `implements BaboonMetaProvider`. ADT branches must `implements BaboonAdtMember`. Cast at `BaboonTypeMeta.from` (already in TS runtime) becomes typesafe.
- Verify the runtime interfaces `BaboonMetaProvider` and `BaboonAdtMember` exist in `BaboonSharedRuntime.ts`. If missing, add them with the minimal field shape required by `BaboonTypeMeta.from`.

**Acceptance criterion:**
1. `mdl :test-typescript-regular :test-typescript-wrapped :test-gen-compat-typescript` PASS.
2. New regression test: instantiate a generated DTO and verify `(it as BaboonMetaProvider).baboonTypeIdentifier` resolves without `as any` cast.

**Risk / unknowns:**
- May require coordinated change to BaboonAdtMemberMeta surface area; verify by reading current TS runtime.
- Could surface latent issues in evolution / conversion test corpus if interface contract is stricter than current runtime tolerates.

**Estimated complexity:** Small-Medium (codegen extension + runtime interface emit + regression test).

**Dependencies:** None.

---

## Reproduction strategy summary

| PR | Defect | Repro test | Pre-fix | Command |
|---|---|---|---|---|
| 25.1 | PR-15-D01 | `ueba_round_trip_withUseIndicesTrue_preservesContent` (restore) | EOF / empty-buffer | `mdl :test-kotlin-regular` |
| 25.2 | PR-26-D03 | `test_facade_cross_format.py` json→ueba | `pydantic.ValidationError` | `mdl :test-python-regular` |
| 25.2 | PR-26-D04 | smoke import of `from_1_0_0_*` | `AttributeError` | `mdl :test-manual-python` |
| 25.3 | PR-22-D05 | `conversion_registry_test.dart` 2-conversions-same-from | wrong dispatch | `mdl :test-dart-regular` |
| 25.4 | PR-19-D05 | `BinReader.test.ts` past-EOF | already PASS | `mdl :test-typescript-regular` |
| 25.4 | PR-19-D06 | vitest `versionMinCompat()` empty-string | returns `undefined` | `mdl :test-typescript-regular` |
| 25.5a | PR-14-D02/D02 | mirrored kt-stub tests | (no tree) | `mdl :test-kotlin-kmp-regular` |
| 25.6 | PR-30-D06 | static inspection | n/a | n/a |
| 25.7 | PR-22-D07 | static inspection | n/a | n/a |
| 25.8 | PR-22-D02 | TS interface implements check | type cast unsafe | `mdl :test-typescript-regular` |

## Parallelisation plan

- **Wave 1 (parallel, 6 worktrees):** PR-25.1, PR-25.2, PR-25.3, PR-25.4, PR-25.6 (zero-code), PR-25.7 (zero-code). All disjoint files.
- **Wave 2 (after Wave 1):** PR-25.5a (gated on PR-25.1), PR-25.8 (TS — independent of Wave 1 but holding for review pipeline capacity).

## Verification gates

### Per-PR (executor local gate)

```bash
# PR-25.1
mdl :build :test-kotlin-regular :test-kotlin-wrapped :test-kotlin-kmp-regular :test-kotlin-kmp-wrapped :test-gen-compat-kotlin :test-gen-compat-kotlin-kmp

# PR-25.2
mdl :build :test-python-regular :test-python-wrapped :test-manual-python

# PR-25.3
mdl :build :test-dart-regular :test-dart-wrapped :test-manual-dart :test-gen-compat-dart

# PR-25.4 / PR-25.8
mdl :build :test-typescript-regular :test-typescript-wrapped :test-gen-compat-typescript

# PR-25.5a
mdl :build :test-kotlin-kmp-regular :test-kotlin-kmp-wrapped
```

### Milestone-level (orchestrator close gate)

```bash
mdl --seq :build :test                                       # full matrix; mandatory floor
mdl --seq :test-gen-compat-cs :test-gen-compat-scala \
          :test-gen-compat-rust :test-gen-compat-typescript \
          :test-gen-compat-kotlin :test-gen-compat-kotlin-kmp \
          :test-gen-compat-java :test-gen-compat-dart \
          :test-gen-compat-swift                             # 9 cross-language byte-identity actions
```

Cross-language `m24-foreign-keycodec.json` md5 baseline `1f1ef66abe5a9a24321c6e615851281d` must remain byte-identical.

## Cross-cutting architectural notes (for `tasks.md`)

1. **Indexed-mode UEBA per-field block emission discipline.** In any backend whose syntax distinguishes "statement block" from "block expression" (Kotlin, Scala, Rust), per-field encode emissions inside the indexed-mode body must be statements, not bare-braced expression values. Bare `{ ... }` in Kotlin statement position is a *lambda expression*, constructed-and-discarded without invoking. Drop braces (preferred — comment markers suffice) or use language-specific statement-block keywords. Symmetric with Compact mode emission.

2. **Cross-runtime conversion registry must key on `(fromTypeId, toTypeId)`, not `fromTypeId` alone.** Currently violated in Java (PR-17-D05) and Dart (PR-22-D05; fixed by PR-25.3). Models with multiple distinct conversions from a single source type silently dispatch wrong target. Cross-runtime alignment tracked future hygiene.

3. **Python facade `json_to_ueba_bytes` interface contract.** Python JSON codecs are stringly-typed (`def decode(self, ctx, wire: str)` via `pydantic.model_validate_json`). Facade's cross-format helper must `json.dumps()` the parsed JSON value before delegating. Future hygiene: align Python JSON-codec generator to take/return parsed values directly.
