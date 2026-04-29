# Plan: M21 — Round-2 upstream defects (PR-45 / PR-46 / PR-47 / PR-48)

**Date:** 2026-04-29
**Branch:** `wip/anytype`
**Source:** `defects.md` "M21 — Round-2 upstream defects" section; milestone tracker `tasks.md`

---

## PR ordering rationale

All four PRs touch disjoint files. **All four can run in parallel.** No hard dependency exists between any pair:

- **PR-45** touches `RsServiceWiringTranslator.scala` only (function-name emit at four sites).
- **PR-46** adds a new `RsDomainTreeTools` (or analogous) and modifies `RsDefnTranslator.scala` to emit a per-type `impl BaboonGenerated` block.
- **PR-47** modifies `BaboonTranslator.scala` (typer) — specifically `convertMethod`/`convertService` — and adds a single positive fixture.
- **PR-48** modifies `ScJsonCodecGenerator.scala` (one map-emit site at line 378) plus optionally `SwJsonCodecGenerator.scala` (line 263) per audit verdict below.

Recommended sequence purely for review-load smoothing: **PR-45 → PR-48 → PR-47 → PR-46** (smallest to largest). Matrix passes independently for each.

---

## PR-45 — BAB-R01: Rust wiring functions emitted in PascalCase

### Investigation findings

The four PascalCase emit sites are all in `baboon-compiler/src/main/scala/io/septimalmind/baboon/translator/rust/RsServiceWiringTranslator.scala`:

| Line | Function-name template | When emitted |
|---|---|---|
| **316** | `pub ${asyncKw}fn invoke_json_$svcName$genericParam(...)` | noErrors mode + JSON codec active |
| **352** | `pub ${asyncKw}fn invoke_ueba_$svcName$genericParam(...)` | noErrors mode + UEBA codec active |
| **483** | `pub ${asyncKw}fn invoke_json_$svcName<Rt: $ibaboonServiceRt>$genericParam(...)` | errors mode + JSON codec active |
| **585** | `pub ${asyncKw}fn invoke_ueba_$svcName<Rt: $ibaboonServiceRt>$genericParam(...)` | errors mode + UEBA codec active |

`$svcName` is computed at lines 280, 384 as `service.id.name.name` — the raw user identifier, PascalCase.

The **snake_case helper already exists**: `RsDefnTranslator.toSnakeCase` at line 698, exported via `import io.septimalmind.baboon.translator.rust.RsDefnTranslator.toSnakeCase` (already imported in `RsServiceWiringTranslator.scala:5`).

**No other Rust emit site uses raw `$svcName` for a function name.** The `Client` struct definitions at lines 217, 224 use `${svcName}Client` (correct — Rust types are PascalCase).

### Decision: rename approach (recommended)

Rationale:
1. Idiomatic Rust output. The `#[allow]` escape hatch leaves the warning suppressed at every call site that imports the function.
2. The fix is mechanical and one-character per site.
3. The `toSnakeCase` helper is already in scope.

### Scope

Replace `$svcName` → `${toSnakeCase(svcName)}` at lines **316, 352, 483, 585** in the function-name position only. Do NOT touch `${svcName}Client` (line 217, 224) or any other `$svcName` interpolation outside the four wrapper-fn declarations.

### Caller-site audit

Search for `invoke_json_` / `invoke_ueba_` callers via:
```
command grep -rn "invoke_json_\|invoke_ueba_" baboon-compiler/src/main/{scala,resources}/
```

If any generator emits a *call* to `invoke_json_$svcName(...)`, that call site must also be normalised. Subagent must verify.

### Success criteria

1. `mdl :build` clean.
2. `mdl :test-rust-regular :test-rust-wrapped` pass.
3. **Targeted check:** generated wiring file contains `pub fn invoke_json_pet_store(...)` (lower-snake), not `invoke_json_PetStore(...)`.
4. `command grep -rn "fn invoke_\(json\|ueba\)_[A-Z]" target/test-rs-*/rs-stub/src/` returns nothing.

---

## PR-46 — BAB-R02: emit `BaboonGenerated` impls for every generated Rust type

### Investigation findings

**Runtime trait** at `baboon-compiler/src/main/resources/baboon-runtime/rust/baboon_runtime.rs:236–243`:

```rust
pub trait BaboonGenerated {
    fn baboon_domain_version() -> &'static str;
    fn baboon_domain_identifier() -> &'static str;
    fn baboon_type_identifier() -> &'static str;
}

pub trait BaboonGeneratedLatest: BaboonGenerated {}
pub trait BaboonAdtMemberMeta: BaboonGenerated {}
```

**Equivalent emit in other backends:**

| Backend | Generator | File:line |
|---|---|---|
| Swift | `SwDomainTreeTools.makeDataMeta` populates three `MetaField`s | `SwDomainTreeTools.scala:38–53`; consumed in `SwDefnTranslator.scala:209` |
| Kotlin | `KtDomainTreeTools.makeDataMeta` populates three meta fields | `KtDomainTreeTools.scala:42–57`; consumed in `KtDefnTranslator.scala:249` |
| TypeScript | `TsDomainTreeTools.mainMeta` emits PascalCase static + camelCase getter | `TsDomainTreeTools.scala:61–84`; consumed in `TsDefnTranslator.scala:174` |

**ID-string convention:**
- Domain types: `<root>/<ns>#<TypeName>` — what `TypeId.User.toString` produces (see `TypeId.scala:30–32`).
- Service in/out types: same convention. The `defn.id.toString` produces `<root>/<ns>/<Service>/<method>#in` (or `#out`). **No special plumbing needed.**

**Rust per-type emitter:** `RsDefnTranslator.makeRepr` at `RsDefnTranslator.scala:148–170` dispatches by `Typedef`. `doTranslate` (line 53) wraps the repr + codec helpers and emits to `<fbase>/.../<TypeName>.rs`. **Currently no metadata emit.**

### Decision: emit `impl BaboonGenerated` per type, plus marker traits

Per Swift/Kotlin convention. Every domain type implements `BaboonGenerated`. The "latest" version additionally implements `BaboonGeneratedLatest`; ADT branches additionally implement `BaboonAdtMemberMeta`.

The emit is appended after the existing `impl ${name.asName} { ... }` codec-helper block. Equivalent shape:

```rust
impl crate::baboon_runtime::BaboonGenerated for X {
    fn baboon_domain_version() -> &'static str { "<v>" }
    fn baboon_domain_identifier() -> &'static str { "<root>" }
    fn baboon_type_identifier() -> &'static str { "<full/qualified/id>" }
}

// If isLatestVersion:
impl crate::baboon_runtime::BaboonGeneratedLatest for X {}

// If owner is Owner.Adt(_):
impl crate::baboon_runtime::BaboonAdtMemberMeta for X {}
```

### Scope

#### New file (recommended)

`baboon-compiler/src/main/scala/io/septimalmind/baboon/translator/rust/RsDomainTreeTools.scala` — mirror of `SwDomainTreeTools.scala:13–80`.

**Lower-friction alternative:** inline the three `q"..."` blocks directly in `RsDefnTranslator.makeRepr`. Saves a file but diverges from the cross-backend pattern.

#### Wiring into `RsDefnTranslator`

In `doTranslate` at line 53, append a `metaImpl` tree:
```scala
val metaImpl = makeBaboonGeneratedImpl(defn, name, isLatestVersion = ...)
val allDefs  = (repr +: codecTrees :+ metaImpl).joinNN()
```

#### Type coverage

- **Dto** — yes
- **Enum** — yes
- **Adt** — yes
- **Contract** — confirm via `SwDefnTranslator.renderContract` whether mainMeta is emitted; if yes, mirror in Rust. Recommend: include.
- **Service** — Recommend: skip (per defect description)
- **Foreign** — emits `typealias` only; no meta. Skip.
- **Service `In`/`Out` synthesised types** — already handled by Dto branch; no extra plumbing.

### `isLatestVersion` plumbing

Already used by Kotlin (`KtDefnTranslator.scala:248`) and Swift (`SwDefnTranslator.scala:208`). Mirror pattern.

### Visibility of the trait import

Use inline FQ `crate::baboon_runtime::BaboonGenerated` (matches existing serde-attribute pattern at `RsDefnTranslator.scala:366`).

### Risk: collision with existing `impl X { ... }` helper block

`RsJsonCodecGenerator.genJsonHelpers` already emits `impl ${name.asName} { ... }` (line 36–56). Adding `impl BaboonGenerated for X { ... }` is a separate `impl` block — no conflict; Rust permits multiple `impl` blocks on the same type.

### Success criteria

1. `sbt baboonJVM/compile` clean.
2. `mdl :build :test-rust-regular :test-rust-wrapped` pass.
3. Generated `<root>/<ns>/SomeType.rs` contains `impl crate::baboon_runtime::BaboonGenerated for SomeType { ... }` with the three FQ identifiers populated.
4. Cargo compile of generated rs-stub passes.
5. Cross-stack parity: Rust `baboon_type_identifier()` equals Swift `baboonTypeIdentifier`.

---

## PR-47 — BAB-G01: typer crash on absent `data in {}`

### Investigation findings

**Crash site (pinned for `wip/anytype`):** `baboon-compiler/src/main/scala/io/septimalmind/baboon/typer/BaboonTranslator.scala:421`:

```scala
422  } yield {
421    MethodDef(MethodName(f.name), inargs.head, outargs.headOption, errargs.headOption)
422  }
```

`inargs` is constructed at line 413: `args.filter(_._1.toLowerCase == "in").map(_._2)`. When the parsed `RawFunc.sig` contains no `in`-marked arg, `inargs` is the empty `List[TypeRef]` and `inargs.head` throws `NoSuchElementException`. Defect's "approximately :405" is two methods above (`convertArg`).

**Parser:** `DefService.scala:24–35` parses `methods` as `method.rep()` where `method.sig` is `shorthandSig | struct.enclosed(sigpart.rep())` — `sigpart.rep()` is `0..*`, so the parser legally accepts zero `in`-blocks. The crash is fully a typer-level oversight.

**Existing typer issues:** `ServiceMissingOutput`, `ServiceMultipleOutputs`, `ServiceMultipleErrors`. **No equivalent check for `in` exists.**

### Decision: option (a) — desugar absent `data in` to `data in {}` in the typer

Per defect text: "treat absent `data in` as syntactic sugar for `data in {}`."

**Where to apply:** typer-level pre-pass in `convertService` (line 374). If `f.sig` lacks any `in` marker, prepend a synthetic `RawFuncArg.Struct(RawDto(RawTypeName("in"), Nil, Set.empty, f.meta))`. The existing `convertArg` path then handles it transparently.

```scala
val normalized = svc.defns.map { f =>
  val hasIn = f.sig.exists {
    case RawFuncArg.Ref(_, marker, _) => marker.toLowerCase == "in"
    case RawFuncArg.Struct(d)         => d.name.name.toLowerCase == "in"
  }
  if (hasIn) f
  else f.copy(sig = RawFuncArg.Struct(RawDto(RawTypeName("in"), Nil, Set.empty, f.meta)) +: f.sig)
}
defs <- F.traverseAccumErrors(normalized)(f => convertMethod(svc, f))
```

### Edge cases

1. `data in {}` (existing user-supplied empty) — already works; behaviour unchanged.
2. Multiple `data in` blocks — currently has no check. Recommend adding `ServiceMultipleInputs` mirroring `ServiceMultipleOutputs` (one-line defensive check).
3. `data in` with the same name as a field — synthesised `RawDto` has empty `members`; collision impossible.

### Test fixtures

Add positive fixture mirroring existing service-bearing model layouts:

```
root service Pinger {
  def ping (
    data out {
      ts: tso
    }
  )
}
```

This must compile cleanly post-fix. Pre-fix it crashes at line 421 — explicit regression test.

### Success criteria

1. `sbt baboonJVM/compile` clean.
2. `sbt baboonJVM/test` pass (new fixture compiles cleanly).
3. End-to-end matrix `mdl :build :test-rust-regular :test-scala-regular :test-cs-regular` pass.

---

## PR-48 — BAB-J01: Scala JSON codec emits `Map[str, V]` keys non-deterministically

### Investigation findings

**Bug site:** `baboon-compiler/src/main/scala/io/septimalmind/baboon/translator/scl/ScJsonCodecGenerator.scala:375–378`:

```scala
case TypeId.Builtins.map =>
  val keyEnc   = encodeKey(c.args.head, q"e._1")
  val valueEnc = mkEncoder(c.args.last, q"e._2")
  q"$circeJson.obj($ref.map(e => ($keyEnc, $valueEnc)).toList: _*)"
```

Scala's `Map.apply(...)` factory returns size-specialised classes (`Map1`/`Map2`/`Map3`/`Map4`) for ≤4 entries (insertion-order preserving), but ≥5 entries returns `HashMap` whose iteration order depends on hash codes + JVM seed.

### Cross-backend audit

| Backend | Map type | Sort? | Verdict |
|---|---|---|---|
| **Scala (Sc)** | `scala.collection.immutable.Map` | No | **BUG — `HashMap` for ≥5** |
| **C# (CS)** | `Dictionary` / `IReadOnlyDictionary` | No | At risk (user's collection-type choice) |
| **Rust (Rs)** | `BTreeMap` | Implicit (BTreeMap sorted) | OK |
| **TypeScript (Ts)** | `Map` or `Record<string, V>` | No | OK (insertion-order) |
| **Kotlin (Kt)** | `Map` | No | OK if user uses `mapOf`/`linkedMapOf` |
| **Java (Jv)** | `java.util.Map` | No | At risk (user's collection-type choice) |
| **Dart (Dt)** | `Map` (LinkedHashMap default) | No | OK |
| **Swift (Sw)** | `Dictionary` | No | **BUG** — Swift Dictionary is hash-based |
| **Python (Py)** | `dict` (insertion-order since 3.7) | No | OK |

**Confirmed buggy:** Sc, Sw.
**Conditionally buggy:** CS, Jv (depends on user-supplied collection type).

### Decision: fix Sc + Sw in PR-48

The defect names Scala only. Audit reveals Swift has the same root cause. Fix both — they're mechanically identical fixes.

CS/Jv are conditionally non-deterministic depending on user collection choice. **Do not fix in PR-48.** File a follow-up entry ("BAB-C04, BAB-J03 — JSON map emit relies on user-supplied collection iteration order").

### Sc fix

`ScJsonCodecGenerator.scala:378`:

```scala
// Before:
q"$circeJson.obj($ref.map(e => ($keyEnc, $valueEnc)).toList: _*)"
// After:
q"$circeJson.obj($ref.toList.sortBy(_._1.toString).map(e => ($keyEnc, $valueEnc)): _*)"
```

`.toString` is correct because the JSON map always serialises keys as strings.

### Sw fix

**Critical: verify Swift JSON-emit target before fixing.** If `JSONSerialization.data(withJSONObject:)` is used, it re-hashes dictionaries — sort at construction is moot. Two viable approaches:
- (a) **`JSONEncoder` with `outputFormatting = .sortedKeys`** — global, idempotent, simplest fix. Find runtime config site.
- (b) **Bypass dictionary construction** — emit JSON object as raw text via key-value array. ~30-line rewrite.

Subagent must verify before applying.

### Risk: cross-language interop tests

Reading is unaffected. Writing is now deterministic — should reduce flakiness, not create regressions.

### Success criteria

1. `sbt baboonJVM/compile` clean.
2. `mdl :build :test-scala-regular :test-swift-regular` pass.
3. New ≥5-entry-map fixture: byte-identical re-emission across two JVM runs.
4. Cross-language interop matrix still passes.

---

## Cross-cutting risks

1. **PR-47 root-cause confidence.** Crash location well-understood; desugaring approach plugs into existing `convertArg` path with no AST shape changes. **Risk: low.**
2. **PR-48 cross-backend scope creep.** Audit confirms Sw shares the bug; defect names Scala only. Plan extends to Sw because root-cause is identical. **Risk: medium** — reviewer may flag scope-expansion. Mitigation: split into PR-48a (Sc) + PR-48b (Sw) if pushback.
3. **PR-46 trait import path.** Inline FQ `crate::baboon_runtime::BaboonGenerated`. Risk only if runtime is gated behind nested module. **Risk: low.**
4. **PR-46 service In/Out type identity.** `defn.id.toString` produces canonical `<root>/<ns>/<Service>/<method>#in|#out`. No special-casing needed. Subagent verifies via spot-check.
5. **PR-45 caller search.** If any generator emits a *call* to `invoke_json_$svcName(...)`, that site also needs renaming. Subagent verifies.

---

## Open questions for orchestrator

1. **PR-46 service-typedef inclusion.** Skip services. **Recommendation:** confirm.
2. **PR-46 contract-typedef inclusion.** Include contracts. **Recommendation:** confirm.
3. **PR-47 multiple-`in` defensive check.** Add `ServiceMultipleInputs` mirroring `ServiceMultipleOutputs` (one-line). **Recommendation:** include in PR-47.
4. **PR-48 Swift fix shape.** Subagent must verify before fixing — `JSONEncoder.outputFormatting = .sortedKeys` (1 line) vs custom raw-text serializer (30 lines).
5. **PR-48 sort key compatibility with M19 typed user-DTO keys.** `_1.toString` assumes stable string repr. Forward-compatible with M18's parseable repr. Worth flagging in commit message.
6. **PR-45 `toSnakeCase` corner case.** Helper already handles `IRS` → `irs`, `pet_store` → `pet_store`. Risk: low.

---

### Critical Files for Implementation

- `baboon-compiler/src/main/scala/io/septimalmind/baboon/translator/rust/RsServiceWiringTranslator.scala` (PR-45: lines 316, 352, 483, 585)
- `baboon-compiler/src/main/scala/io/septimalmind/baboon/translator/rust/RsDefnTranslator.scala` (PR-46: append per-type `impl BaboonGenerated`; new `RsDomainTreeTools` recommended adjacent)
- `baboon-compiler/src/main/scala/io/septimalmind/baboon/typer/BaboonTranslator.scala` (PR-47: line 421 crash; fix in `convertService` ~line 374)
- `baboon-compiler/src/main/scala/io/septimalmind/baboon/translator/scl/ScJsonCodecGenerator.scala` (PR-48 Sc fix: line 378)
- `baboon-compiler/src/main/scala/io/septimalmind/baboon/translator/swift/SwJsonCodecGenerator.scala` (PR-48 Sw fix: line 263)
