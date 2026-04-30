# M23 — Backend stubs unblock + upstream defects cleanup

**Date:** 2026-04-30
**Predecessor:** M20 closeout (commit `329fcdb` session log).
**Goal:** all 9 `:test-{lang}-regular` actions PASS; close out resolved items in `baboon-upstream-defects.md`.

## Phase 1 reproduction summary (5 backends fail on HEAD `99815ab`)

### F1.1 Rust — generic `D` shadows user-defined `id D`

- **Location:** `RsDefnTranslator.scala:694` (and parallel at line 1073).
- **Symptom:** `error[E0277]: D: Ord not satisfied`; `error[E0308]: mismatched types ... expected type parameter D, found struct d::D` at generated `target/test-regular/rs-stub/src/identifier/ok/d.rs:81`.
- **Root cause:** template `pub fn deserialize<'de, D, V>(deserializer: D) -> Result<BTreeMap<${name.asName}, V>, D::Error>` declares generic `D` that shadows `id D` from `identifiers.baboon`. Sibling at line 1073 (`fn visit_map<A: serde::de::MapAccess<'de>>`) has the same latent shadow for any user ADT named `A`.
- **Fix:** rename to `__De` / `__M` (double-underscore prefix; baboon parser doesn't allow user identifiers to start with `__`).

### F1.2 Scala — `FStr_JsonCodec.encode` returns `Json` where map-key wants `String`

- **Location:** `ScJsonCodecGenerator.scala`, function `encodeKey`, lines ~311-318 (Foreign branch).
- **Symptom:** `target/test-regular/sc-stub/.../m19/foreign/Holder.scala:33` type mismatch `List[(io.circe.Json, io.circe.Json)] | Seq[(java.lang.String, io.circe.Json)]`.
- **Root cause:** wrapper-around-foreign path (PR-60 lines 320-325) peels `ItemKey { v: FStr }` to inner `FStr`. The recursive `encodeKey(inner.tpe, ref)` then hits the Foreign branch, which calls `_JsonCodec.instance.encode(ctx, $ref)` returning `Json`. Map keys at the call site need a `String`.
- **Fix:** in the Foreign arm, when the foreign decl is `Custom(...)` and resolves to a Scala `String`-shaped type, emit `$ref` (or `$ref.toString`) directly. The recursion has already peeled to the inner; `$ref` carries the inner-typed value.

### F1.3 + F1.4 Kotlin (JVM + KMP) — `import java.lang.String` shadows `kotlin.String`

- **Location:** `KtTypeTranslator.asKtTypeDerefForeigns` (lines 110-126).
- **Symptom:** `target/test-regular/kt-stub{,-kmp}/.../m19/foreign/{FStr,ItemKey}.kt`: `Initializer type mismatch: expected 'java.lang.String', actual 'kotlin.String'.` ~12 sites.
- **Root cause:** `wrapper-around-foreign.baboon` maps `kotlin = "java.lang.String"`. Codegen parses to `KtType(pkg=java.lang, name=String)` and emits `import java.lang.String` — that import shadows `kotlin.String` for the rest of the file, so string literals (`"1.0.0"`) become `kotlin.String` ≠ `java.lang.String` and overrides of `BaboonGenerated.baboonDomainVersion: kotlin.String` fail.
- **Fix:** add allowlist of JVM types Kotlin re-aliases (`java.lang.String → kotlin.String`, `java.lang.Boolean → kotlin.Boolean`, `java.lang.Byte → kotlin.Byte`, `java.lang.Short → kotlin.Short`, `java.lang.Integer → kotlin.Int`, `java.lang.Long → kotlin.Long`, `java.lang.Float → kotlin.Float`, `java.lang.Double → kotlin.Double`, `java.lang.Character → kotlin.Char`). When matched, rewrite to the kotlin-namespace form; don't emit a separate import.

### F1.5 Swift — flat `BaboonTests` SPM target with duplicate filenames

- **Location:** `test/sw-stub/Package.swift` test-target declaration (line ~50).
- **Symptom:** SPM build fails with `error: couldn't build .../BaboonTests.build/holder_test.swift.o because of multiple producers`. Multiple files named `holder_test.swift`, `item_id_test.swift`, etc. across `Tests/BaboonTests/{MyOk,MyOkM19Direct,...}/` subdirectories.
- **Root cause:** one flat `.testTarget(name: "BaboonTests", path: "Tests/BaboonTests")` recursively picks all `.swift` files but uses basename for `.o` filename → collision.
- **Fix:** rewrite `Package.swift` test-target block as one `.testTarget` per source module, each with `path: "Tests/BaboonTests/<Module>"` and `dependencies: ["<Module>", "BaboonRuntime"]`. The list of source modules already exists at lines 8-46.

## Phase 2 triage — most items already resolved by prior work

| ID | Status before triage | Reality on HEAD `99815ab` | Evidence |
|---|---|---|---|
| BAB-S01 ns-scoped Scala wiring | open major | **fixed by PR-27 (M14)** | `git log` for `ScServiceWiringTranslator.scala`; generated `target/.../svcns.NsScopedSvc_Wiring.scala` correct package |
| BAB-S03 user-DTO map keys | open moderate | **fixed by M19 (PR-59-PR-61)** | `m19-ok/direct-wrapper.baboon` is exactly the user's repro |
| BAB-S05 cross-ADT branch reuse | open minor | **fixed by M20 (PR-62-PR-64)** | `+ X` / `- X` / `^ X` syntax landed |
| BAB-K01 ns-scoped Kotlin wiring | open major | **fixed by PR-28 (M14)** | `git log` for `KtServiceWiringTranslator.scala`; package decl correct |
| BAB-K02 named-arg-supertype warning | open trivial | **already resolved** | `KtUEBACodecGenerator.scala:114` already emits `instance` matching base. To verify: `kotlinOptions.allWarningsAsErrors=true`. |
| BAB-K05 redundant conversion call | open nit | **probably resolved** | No warning reproduces in current compile output. Verify via `-Werror`. |
| BAB-G02 Rust runtime regression | open major | **largely resolved**, only D-shadow remains (= F1.1) | Generated `baboon_runtime.rs` defines `tsu_serde`, `lenient_numeric`, `read_timestamp_utc`, etc. Only failure surface is the D-shadow. |

## PR breakdown

5 PRs proposed. Code PRs touch disjoint files — parallelizable via worktrees, but cheap enough to serialize.

### PR-65 — Rust serde-template generic-name hygiene
Closes Phase 1 #1 (= F1.1) + PR-64-D01 + PR-64-D04 + BAB-G02 residue.

**Scope:** rename hard-coded short letters in two Rust serde templates in `RsDefnTranslator.scala`:
- line ~694: `pub fn deserialize<'de, D, V>(deserializer: D)` → `<'de, __De, V>(deserializer: __De)`. Update `D::Error` → `__De::Error`.
- line ~1073: `fn visit_map<A: serde::de::MapAccess<'de>>` → `<__M: serde::de::MapAccess<'de>>`. Update `A::Error` → `__M::Error`.

**Success:** `mdl :test-rust-regular :test-rust-wrapped` PASS. `cargo build` of `rs-stub` clean.

### PR-66 — Scala JSON map-key Foreign-Custom encoding
Closes Phase 1 #2 (= F1.2).

**Scope:** in `ScJsonCodecGenerator.scala` `encodeKey` Foreign branch (lines ~311-318), when the Foreign has `Custom` decl (no `BaboonRef`), emit `$ref` (or `$ref.toString`) instead of `_JsonCodec.instance.encode(ctx, $ref)`.

**Success:** `mdl :test-scala-regular :test-scala-wrapped` PASS. Holder.scala compiles. Cross-language byte-identity preserved (key emits as JSON string).

### PR-67 — Kotlin foreign-mapping for `java.lang.*` JVM aliases
Closes Phase 1 #3 + #4 (= F1.3 + F1.4).

**Scope:** in `KtTypeTranslator.asKtTypeDerefForeigns` (lines ~110-126), add allowlist of JVM types Kotlin re-aliases. Rewrite `java.lang.String → kotlin.String` (etc.) at the `Custom(decl, _)` branch; suppress the `import java.lang.*` emission for matched cases.

**Allowlist:**
```
java.lang.String → kotlin.String
java.lang.Boolean → kotlin.Boolean
java.lang.Byte → kotlin.Byte
java.lang.Short → kotlin.Short
java.lang.Integer → kotlin.Int
java.lang.Long → kotlin.Long
java.lang.Float → kotlin.Float
java.lang.Double → kotlin.Double
java.lang.Character → kotlin.Char
```

**Success:** both `mdl :test-kotlin-regular :test-kotlin-wrapped` and `mdl :test-kotlin-kmp-regular :test-kotlin-kmp-wrapped` PASS. After fix, run with `-Werror`-style flag if available to also tighten BAB-K02 / BAB-K05 verification.

### PR-68 — Swift SPM test-target per-module split
Closes Phase 1 #5 (= F1.5).

**Scope:** rewrite `test/sw-stub/Package.swift` test-target block as one `.testTarget` per source module, mirroring source-target list at lines 8-46. Each test target has `path: "Tests/BaboonTests/<Module>"` and `dependencies: ["<Module>", "BaboonRuntime"]`.

**Note:** Package.swift is hand-written and drifts as fixtures change. Out-of-scope: a generator script that emits the testTarget block from directory contents would be a future hygiene win.

**Success:** `mdl :test-swift-regular :test-swift-wrapped` PASS.

### PR-69 — Close out resolved items in `baboon-upstream-defects.md`
Doc-only.

**Scope:** mark statuses in `baboon-upstream-defects.md`:
- BAB-S01 → `[x] fixed (closed by PR-27 / M14)`
- BAB-S03 → `[x] fixed (closed by M19 / PR-59-PR-61)`
- BAB-S05 → `[x] fixed (closed by M20 / PR-62-PR-64)`
- BAB-K01 → `[x] fixed (closed by PR-28 / M14)`
- BAB-K02 → `[x] fixed (verified by PR-67 with `-Werror`)` if confirmed clean
- BAB-K05 → `[x] fixed (verified by PR-67 with `-Werror`)` if confirmed clean
- BAB-G02 → `[x] fixed (mostly resolved by prior runtime work; D-shadow closed by PR-65)`

## Sequencing

Recommended order: PR-65 → PR-66 → PR-67 → PR-68 → PR-69. All PRs touch disjoint files and could parallelize via worktrees; we serialize for simplicity unless rate budget pressures otherwise.

## Final verification

```
mdl --seq :build :test-gen-regular-adt :test-gen-wrapped-adt \
    :test-rust-regular :test-rust-wrapped \
    :test-scala-regular :test-scala-wrapped \
    :test-kotlin-regular :test-kotlin-wrapped \
    :test-kotlin-kmp-regular :test-kotlin-kmp-wrapped \
    :test-swift-regular :test-swift-wrapped \
    :test-cs-regular :test-typescript-regular :test-java-regular \
    :test-dart-regular :test-python-regular
```

All 9 backend stubs (regular + wrapped) must PASS.

## Risks

1. **PR-66 over-fix risk** — could regress non-foreign wrapper paths. Mitigation: add a regression fixture exercising both `wrapper-around-foreign` (Custom decl) and `wrapper-around-builtin` (BaboonRef) map keys.
2. **PR-67 over-rewrite risk** — must not eat all `java.lang.*` cases, only the JVM-built-in aliases. Conservative allowlist (above) is the safe choice.
3. **PR-68 fixture drift** — Package.swift is now coupled to the directory layout under `Tests/BaboonTests/`. Future fixture additions need parallel updates. Out-of-scope hygiene: ship a generator script.
4. **PR-65 generic-name choice** — `__De` / `__M` rely on baboon-parser-disallows-user-double-underscore-prefix. Verify parser rule before naming.
