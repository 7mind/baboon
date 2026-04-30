# Baboon — Upstream Defect Report (Project-Agnostic)

A consolidated set of defects, missing features and codegen surprises observed
in **Baboon 0.0.185** while building a multi-stack application that exercises
all five generator targets (Scala, TypeScript, Kotlin, Swift, Rust) against a
mid-sized schema (≈13 `.bmo` files, ≈11 namespaced services, ≈400 generated
files per backend).

This file is suitable for filing as upstream issues. All reproductions use
generic identifiers (`Foo`, `Bar`, `Service<N>`, `<ns>`); no project-specific
names appear. Where a backend has multiple distinct findings, each ships as a
separate entry so the maintainer can triage them individually.

Status: `[ ]` open · `[~]` fix in progress · `[x]` fixed · `[!]` wontfix (maintainer policy)

Backends covered:
- **Scala** (JVM consumer) — battle-tested; 6 findings (BAB-S01..S06).
- **TypeScript** (browser/Node consumer) — production-ready with rough edges; 2 findings (BAB-T01..T02).
- **Kotlin** (Android consumer) — battle-tested; 5 findings (BAB-K01..K05).
- **Swift** (iOS consumer) — 4 findings (BAB-W01..W04). `S` prefix is taken by Scala.
- **Rust** (embedded / native consumer) — narrowest surface; 2 findings (BAB-R01..R02).
- **CLI / cross-cutting** — 1 finding (BAB-C01).

Plus a small set of newer findings filed below the per-backend sections:
**BAB-G01** (parser), **BAB-G02** (Rust runtime metadata accessor gap, scope
overlaps BAB-R02), **BAB-J01** (JSON-codec map-key ordering on the Scala
target).

---

## Scala backend

### [BAB-S01] `*_Wiring.scala` for ns-scoped services emitted in wrong package
**Status:** open · **Severity:** major
**Symptom:** When a `root service` lives inside an `ns <name> { ... }` block,
Baboon emits per-service `<Service>_Wiring.scala` in the model's *root*
package but references the service trait by its unqualified name. The trait
itself is correctly placed in `<root>.<ns>`. The wiring file fails to compile.

**Repro:**
```bmo
ns shop {
    root service Inventory {
        def listItems (data in {} data out { ... }) err = ServiceError
    }
}
```
Generates:
- `<root>/shop/Inventory.scala` — trait declared in `package <root>.shop` ✓
- `<root>/Inventory_Wiring.scala` — declared in `package <root>` but references
  `Inventory` (unqualified). Fails to compile: cannot resolve `Inventory`.

**Affected:** every ns-scoped service.

**Note:** The top-level `BaboonServiceWiring.scala` ships only `BaboonMethodId`
+ a narrow error ADT — it is not a transport-neutral dispatcher; downstream
consumers typically hand-roll one. So deleting the broken per-service files
post-codegen is a viable workaround until this is fixed.

**Fix idea:** emit `<root>/<ns>/<Service>_Wiring.scala` declared in
`package <root>.<ns>`, OR fully-qualify the trait reference in the existing
root-placed file.


---

### [BAB-S03] User-defined DTOs rejected as JSON map keys
**Status:** open · **Severity:** moderate
**Symptom:** Declaring `map[<UserType>, V]` (where `<UserType>` is a
`root data` wrapper around a primitive) crashes `ScJsonCodecGenerator` with
`Unexpected key usertype`. Only builtins, enums and foreign types are accepted
as JSON map keys.

**Repro:**
```bmo
ns ns1 {
    root data BarId { value: common.Uid }
    root data Foo {
        counts: map[BarId, u32]
    }
}
```
Codegen fails at `ScJsonCodecGenerator` with `Unexpected key usertype` on the
`Foo.counts` field.

**Affected:** any schema that wants typed map keys derived from a primitive.

**Workaround:** declare `map[str, V]` keyed on the primitive's wire form (e.g.
hex string, decimal-int as string), and parse back on the consumer side.
Loses static typing on the key.

**Fix idea:** allow root-data wrappers around primitives (the common case —
`root data Foo { value: <primitive> }`) as map keys with the wrapper's
inner-primitive codec doing the JSON key serialisation.


---

### [BAB-S04] Codec emission requires opt-in and CLI flags are undocumented
**Status:** [!] wontfix (maintainer policy decision: opt-in is intentional; flags will be documented separately) · **Severity:** minor (DX)
**Symptom:** Baboon emits UEBA + JSON codecs only when explicitly requested,
either per-type (`: derived[ueba], derived[json]` in the schema) or globally
via `--generate-ueba-codecs-by-default=true` /
`--generate-json-codecs-by-default=true`. Both flags default `false`. Both are
absent from `baboon --help` but present in `CLIOptions.scala` as
`generateUebaCodecsByDefault` / `generateJsonCodecsByDefault`.

**Repro:** invoke `baboon :scala --help` — flags absent. Set neither flag and
omit `: derived[...]` annotations — generated tree compiles but emits no
codec companions; `<Type>.codecUeba` is undefined at use sites.

**Workaround:** pass both flags explicitly. Schemas stay clean.

**Fix idea:** either flip the defaults to `true`, or add the flags to
`--help` output (they are valid options, just undocumented).


---

### [BAB-S05] Cross-ADT branch reuse is not supported
**Status:** open · **Severity:** minor
**Symptom:** Baboon has no syntax for "this ADT has the same first N branches
as `<OtherAdt>`". Every per-endpoint error ADT must literally re-list the
common branches verbatim.

**Repro:**
```bmo
ns common {
    root adt ErrorAtom {
        data Unauthenticated {}
        data Forbidden {}
        data RateLimited {}
        data ValidationFailed { reason: str }
    }
}
ns ns1 {
    // Cannot write `adt FooError includes common.ErrorAtom { ... }` —
    // every branch of common.ErrorAtom must be inlined into every per-
    // endpoint error ADT in the schema.
    root adt FooError {
        data Unauthenticated {}
        data Forbidden {}
        data RateLimited {}
        data ValidationFailed { reason: str }
        data NotFound {}
    }
}
```

**Affected:** every error ADT in a schema that wants a shared base of
"transport-level" error variants.

**Fix idea:** ADT inclusion / branch-set inheritance — `adt FooError
includes common.ErrorAtom { data NotFound {} }`.


---

### [BAB-S06] No user-defined generics
**Status:** [~] in progress upstream · **Severity:** minor (architectural)
**Symptom:** Baboon 0.0.185 has no parametric data types. A wire-level
`Envelope[T]` or `Page[T]` cannot be expressed.

**Repro:**
```bmo
// Cannot declare:
root data Page[T] { items: lst[T], total: u32 }
```
Workaround is one-off `PageOfFoo`/`PageOfBar` per element type; a generic
transport envelope (`Request[T]/Response[T]/Err[E]`) must be written by hand
in every consumer language.

**Fix idea:** either real generics, or a "tagged blob" / `Box<T>` foreign-type
escape hatch that lets the user wrap a known-type payload in a generic
envelope.


---

## Kotlin backend

### [BAB-K01] `*ServiceWiring.kt` for ns-scoped services emitted in wrong package
**Status:** open · **Severity:** major
**Symptom:** Direct Kotlin-target counterpart of [BAB-S01]. When a `root
service` lives inside an `ns <name> { ... }` block, Baboon emits per-service
`<output>/<root>/<ns>.<Service>ServiceWiring.kt` files declared in
`package <root>` but referencing the service trait by an unqualified name
(e.g. `Inventory`) while the trait itself lives in `package <root>.<ns>`.
Unresolvable from `package <root>`; file fails to compile.

**Affected:** every ns-scoped service. (Same set that triggers BAB-S01 on the
Scala target.)

**Workaround:** delete the broken `<root>/*ServiceWiring.kt` files after
codegen. The single intentional top-level wiring runtime file (in
`package baboon.runtime.shared`) ships unaffected. Sentinel-guard the cleanup
(no matches → exit non-zero) so a future fix can't silently no-op.

**Fix idea:** identical to BAB-S01 — emit
`<output>/<root>/<ns>/<Service>ServiceWiring.kt` declared in
`package <root>.<ns>`, OR qualify the service-trait reference in the existing
root-placed file.


---

### [BAB-K02] Generated codecs trip Kotlin's named-argument-supertype warning
**Status:** open · **Severity:** trivial (warning only)
**Symptom:** Every emitted `*_UEBACodec` companion declares
`encode(ctx, writer, value)` while its supertype `BaseGenerated`
declares the third parameter `instance`. Kotlin emits a non-fatal warning at
every override site:

> The corresponding parameter in the supertype 'BaseGenerated' is named
> 'instance'. This may cause problems when calling this function with named
> arguments.

With default warning settings projects compile cleanly; with `-Werror` (or
any future tightening) it would fail.

**Affected:** all generated UEBA codec objects (≈700 sites in a
mid-sized schema).

**Fix idea:** harmonise the parameter name across the runtime-shared
abstract class and the codegen template (rename either supertype param
to `value` or override params to `instance`).


---

### [BAB-K05] Generated codec emits a redundant conversion-method call
**Status:** open · **Severity:** nit
**Symptom:** Kotlin compiler emits `Redundant call of conversion method.` at a
single generated codec body (1 site in a mid-sized schema; exact site not
narrowed).

**Fix idea:** drop the redundant conversion in the affected template.

---

## Cross-cutting / CLI

### [BAB-C01] CLI flags `--omit-most-recent-version-suffix-*` removed without notice
**Status:** [!] wontfix · **Severity:** trivial
**Symptom:** Pre-v0.0.185 `--omit-most-recent-version-suffix-*` flags no
longer exist in v0.0.185; that behaviour is now the default. Anyone porting
an older Baboon invocation will get an "unknown flag" error.

**Fix idea:** keep removed flags as no-op aliases for one minor version with
a deprecation warning, then drop.


---

## Newer findings


---

### [BAB-G02] Rust runtime + generated codecs don't compile — regression on baboon main `8788c832`
**Status:** open · **Severity:** major
**Symptom:** Generated Rust output fails to compile against the baboon-emitted
runtime on baboon main HEAD `8788c832` (≈0.0.186-SNAPSHOT). Approximately 200
errors in the runtime file itself, plus ≈175 in generated `.rs` files. Error
classes:
- ≈78 × `error[E0599]: no method named encode_ueba found for struct
  chrono::DateTime<Tz> in the current scope` — generated codec impl bodies call
  `.encode_ueba(...)` on `chrono::DateTime<Utc>` field values but do not
  `use crate::baboon_runtime::BaboonBinEncode;` at the top of the generated
  file. The runtime DOES define `impl BaboonBinEncode for chrono::DateTime<Utc>`,
  but trait methods on values of types other than `Self` need the trait in
  scope at the call site.
- ≈40 × `error[E0425]: cannot find function read_timestamp_utc in module
  crate::baboon_runtime::bin_tools` — runtime helper function appears renamed
  or removed but generated code still references the old name.
- ≈39 × `error[E0433]: cannot find tsu_serde in baboon_runtime` — module appears
  renamed/removed.
- ≈13 × `error[E0433]: cannot find lenient_numeric in baboon_runtime` — same.
- ≈3 × `error[E0433]: cannot find baboon_codecs_facade in crate` —
  generated code references a facade module not emitted at the expected
  path.
- ≈4 × `error[E0432]: unresolved import crate::<root>::<ns>::<service>::<method>::r#in`
  — service-method input modules either renamed or missing for some methods.

The runtime file itself emits with errors (it's part of the baboon-generated
output, not hand-written). Affected: any consumer that runs `baboon :rust
... --runtime with` against a non-trivial schema. **Probably:** a recent
commit in the "rust improvements" / "wip" series broke the runtime template
in tandem with a generator-side rename.

**Repro:** consume baboon main HEAD `8788c832` with a schema that exercises
chrono `tsu`/`tso` fields, run `baboon :rust --output $STAGING --runtime with
--generate-ueba-codecs-by-default=true --generate-json-codecs-by-default=true`,
then `cargo check` against the staged tree.

**Workaround (consumer-side):** none clean. Pin to an older baboon commit
predating the regression; OR add a post-codegen pass that injects
`use crate::baboon_runtime::BaboonBinEncode;` at the top of every generated
`.rs` file AND patches the missing module references — but the runtime
file's own ≈200 errors mean the runtime would need patching too, which
becomes structurally invasive.

**Fix idea:** in the Rust codegen template, add `use
crate::baboon_runtime::BaboonBinEncode;` (and any sibling traits used in
the generated impls) to the file-level imports of every generated `.rs`
file that contains a `BaboonBinEncode` / `BaboonBinDecode` impl. Verify
the runtime file template emits a self-consistent module set —
`tsu_serde`, `lenient_numeric`, `read_timestamp_utc`, `baboon_codecs_facade`
all need to either be present or the generated code references updated
to the new names.

---

---

## Resolved (full detail kept for archive)

Closed by baboon main `8788c832` unless otherwise noted. Full per-defect detail follows.

### [BAB-S02] Enum case-name mismatch between case-object emission and UEBA encoder match arms
**Status:** [x] fixed (closed by baboon main 8788c832 / PR-34/35) · **Severity:** major
**Symptom:** When source enum members are written lowercase or snake_case
(e.g. `cafe`, `bar_pub`), the Scala generator emits case objects with a
capitalised first letter (`Cafe`, `BarPub`) but the UEBA encoder's `match`
arms reference them uncapitalised — the produced Scala doesn't compile.

**Repro:**
```bmo
ns ns1 {
    root data Foo {
        kind: enum { alpha, bar_baz, gamma }
    }
}
```
Generated Scala:
- Enum case-objects: `case object Alpha`, `case object BarBaz`, `case object Gamma` ✓
- UEBA encoder match: `case alpha => ...`, `case bar_baz => ...` — uncapitalised, unresolvable.

**Affected:** every enum whose source identifiers are not already PascalCase.

**Workaround:** PascalCase every enum member at the source level
(`enum Kind { Alpha, BarBaz, Gamma }`) and map back to the original spec name
manually at the wire boundary if needed. This costs spec-fidelity.

**Fix idea:** normalize the casing in *one* place during codegen. Either
preserve the source identifier verbatim in the enum-value string codec output
and emit the Scala identifier with a deterministic transform, or emit both
encoder match arms and case-object names from the same canonicalised
identifier.


---


### [BAB-T01] Strict-mode tsconfig produces ~615 errors against generated output
**Status:** [x] fixed (closed by baboon main 8788c832 / PR-29) · **Severity:** moderate
**Symptom:** Baboon's TS output, against a strict tsconfig
(`verbatimModuleSyntax`, `noImplicitReturns`, `noUncheckedIndexedAccess`),
emits ~615 type errors in three classes:
- ~247× **TS1484** — value-style imports (`import { Foo }`) of interface-only
  symbols. `verbatimModuleSyntax` requires `import type { Foo }`.
- ~347× **TS7030** — ADT branch encoders' `match` flow can return implicitly.
  `noImplicitReturns` requires every branch to be explicit.
- ~21× **TS2322 / TS2345** — `BaboonBinReader.readByte` is typed `number` but
  returns `undefined` past EOF; flows through to assignments. With
  `noUncheckedIndexedAccess` this is a soundness gap (the generated decoder
  silently zero-fills past EOF).

**Repro:** generate a non-trivial schema with `:typescript`, then compile the
output under a tsconfig with `verbatimModuleSyntax`, `noImplicitReturns`,
`noUncheckedIndexedAccess` all enabled.

**Workaround:** prepend `// @ts-nocheck` to every emitted `.ts` file
post-codegen. `@ts-nocheck` does NOT leak across module boundaries —
consumer code (tests, client facades) still type-checks fully against
exported types. The TS2322/TS2345 class has soundness implications: a
truncated UEBA payload silently zero-fills past EOF.

**Fix idea:** emit `import type` for type-only symbols; emit explicit
default branches in ADT encoders; bounds-check `readByte` (return
`number | undefined` and have decoders short-circuit on the first
`undefined`, OR throw on truncation).


---


### [BAB-T02] Bundler `MISSING_EXPORT` on type-only symbols imported as values
**Status:** [x] fixed (closed by baboon main 8788c832 / PR-29) · **Severity:** moderate
**Symptom:** Baboon's `BaboonSharedRuntime.ts` exports several names as
TS interfaces only. esbuild-style bundlers (rolldown, vite, webpack 5, etc.)
fail `MISSING_EXPORT` when downstream code does
`import { BaboonGeneratedLatest, … }` at value position even though the
import is type-only at the source level — types are erased before bundling
and the bundler sees no runtime binding.

**Repro:** generate, then bundle a downstream module that does
`import { BaboonGeneratedLatest } from 'baboon-runtime'` at value position.

**Workaround:** post-process `BaboonSharedRuntime.ts` by appending
`export const X = {};` runtime stubs for affected names. Idempotent if
guarded by sentinel + name-conflict skip.

**Fix idea:** related to BAB-T01 — emit `export type { Foo }` (and require
consumers to `import type`) instead of `export interface Foo`, OR also emit a
runtime stub.


---


### [BAB-K03] `*_UEBACodec.encode` overrides emit unnecessary type casts
**Status:** [x] fixed (closed by baboon main 8788c832 / PR-33) · **Severity:** minor (compile warning only)
**Symptom:** Kotlin compiler emits `No cast needed.` warnings hundreds of
times across the generated `*_UEBACodec.kt` files (≈196 sites in a
mid-sized schema). The encoder bodies cast values to types they're already
statically known to be.

**Fix idea:** drop the redundant casts in the UEBA encoder template.


---


### [BAB-K04] Generated decoders apply unnecessary `!!` non-null assertions on already-non-null receivers
**Status:** [x] fixed (closed by baboon main 8788c832 / PR-33) · **Severity:** minor (compile warning only)
**Symptom:** Kotlin compiler emits
`Unnecessary non-null assertion (!!) on a non-null receiver of type 'X'.`
many times (≈56 sites in a mid-sized schema).

**Fix idea:** remove `!!` from the decoder template where the receiver is
already a non-null Kotlin type.


---


### [BAB-W01] JSON decoder bodies emit doubled `try try` keyword
**Status:** [x] fixed (closed by baboon main 8788c832 / PR-30) · **Severity:** moderate (warning only)
**Symptom:** Every emitted `*_JsonCodec.decode(...)` body wraps recursive
`Codec.decode(ctx, ...)` calls (and a few `BaboonTimeFormats.parseUtc`
calls) with two consecutive `try` keywords:

```swift
account: try try ns.AccountId_JsonCodec.instance.decode(ctx, jsonObj["account"]!),
```

Swift treats the outer `try` as wrapping the expression `try X`. The outer
`try` has nothing to wrap and the compiler emits
`warning: no calls to throwing functions occur within 'try' expression`
at every site.

**Affected:** every type with at least one user-defined-type field that has
a JSON codec (≈287 sites across ≈283 generated `.swift` files in a mid-sized
schema).

**Workaround:** post-codegen `try try` → `try` literal replacement, sentinel-
guarded so a fix that drops one `try` can't silently leave the workaround
firing on zero matches.

**Fix idea:** drop one of the two `try` keywords in the
`ScSwiftJsonCodecGenerator` (or whichever module emits the Swift JSON
decoder bodies) when emitting recursive-decoder field assignments.


---


### [BAB-W02] Optional-string JSON decoder uses force-cast in optional context
**Status:** [x] fixed (closed by baboon main 8788c832 / PR-30) · **Severity:** minor (warning only)
**Symptom:** Generated JSON decoders for optional `str` fields emit:

```swift
field: try { let v = jsonObj["field"];
  return v is NSNull || v == nil ? nil : v! as! String }()
```

Swift's type checker, seeing the closure's result type inferred as
`String?` (because one ternary branch is `nil`), warns that the forced
`as!` cast is being treated as an optional cast that "will never produce
'nil'" — even though the surrounding `v is NSNull || v == nil` guard
already proves `v != nil`. (Sibling sites with member access like
`(v! as! NSNumber).doubleValue` don't warn because the member access
narrows the type before the ternary collapses to `String?`.)

**Affected:** every optional `str` field whose decoder body doesn't include
a member access in the cast position (≈8 sites in a mid-sized schema).

**Workaround:** post-codegen rewrite of the closure body from
`v! as! String` to `v as! String` — the closure body has already proved
`v != nil && !(v is NSNull)` so the unforced bind is semantically identical
and sidesteps the optional-context inference.

**Fix idea:** in the Swift JSON decoder template for optional `str` fields,
drop the `!` (the `v` is already known non-nil by the guard) — emit
`v as! String` instead of `v! as! String`. Or rewrite the closure to use
`if let` / early return.


---


### [BAB-W03] Swift JSON decoder template emits `try` in front of every field expression even when nothing throws
**Status:** [x] fixed (closed by baboon main 8788c832 / PR-30 + PR-49) · **Severity:** minor (warnings only)
**Symptom:** Baboon's Swift JSON decoder template emits a `try` keyword in
front of every field expression regardless of whether the expression actually
throws. Three residual surface-form classes after the BAB-W01
(doubled-`try`) and conventional single-`try`-strip mitigations:

1. **`try { let v = ...; return v is NSNull || v == nil ? nil : ... }()`
   closures** for optional fields with non-`str` types and for optional-`str`
   fields whose body uses member access (e.g. `(v! as! NSNumber).doubleValue`).
   The closure body has no throwing call; the outer `try` is dead.

2. **`try` keyword preceding a multi-line ternary** for `i64`/`u64` fields
   where the JSON value may be either string or number:
   ```swift
   amount: try
       (jsonObj["amount"]! is String
       ? Int64(jsonObj["amount"]! as! String)!
       : Int64(truncatingIfNeeded: (jsonObj["amount"]! as! NSNumber).int64Value))
   ```
   Neither branch throws.

3. **`try (jsonObj[X]! as! [Any]).map { e0 in e0 as! T }`** plus the parallel
   UEBA `try (0..<Int(reader.readI32())).map { _ in reader.readString() }` form
   for list/map fields. The `.map` closure body has no throwing call; outer
   `try` is dead.

**Affected:** every type with at least one optional non-`str` field, every
type with a long-integer JSON-string-or-number field, and every type with a
list/map field (≈33 unique residual sites in a mid-sized schema after the
BAB-W01 mitigation).

**Additional runtime warnings (same generator-template root cause):**
- `BaboonRuntime/baboon_runtime.swift`: in `BaboonBinWriter.writeRawDecimal`
  the local `var carry: UInt64 = 0` is never used, and the local
  `var digits = Array(...)` / `var comps = calendar.dateComponents(...)` are
  never mutated and should be `let`.

**Workaround:** in the consumer `Package.swift`, configure the generated
target with `swiftSettings: [.unsafeFlags(["-suppress-warnings"])]`; the
hand-written consumer target stays warning-strict. (Side-effect: a target
configured with `unsafeFlags` is non-distributable as a binary SwiftPM
dependency.)

**Fix idea:** in the Swift JSON decoder template, only emit `try` in front of
expressions that actually call a throwing function (i.e. `Codec.decode(...)`
calls and any helpers marked `throws`). Drop the keyword for pure casts,
dictionary subscripts, integer constructors, and the optional-field ternary
closure. For the BaboonRuntime sites, mark the `carry` variable unused (or
remove the declaration) and switch `digits`/`comps` from `var` to `let`.


---


### [BAB-W04] Swift UEBA reader traps on truncated/invalid input instead of throwing
**Status:** [x] fixed (closed by baboon main 8788c832 / PR-30 + PR-49) · **Severity:** major
**Symptom:** Baboon's Swift UEBA reader in
`BaboonRuntime/baboon_runtime.swift` unconditionally trusts the byte stream —
`readString`, `readBytes` and `readUuid` all index `data` and force-unwrap
`String(data:encoding:.utf8)` without bounds-checking.

**Concrete sites at v0.0.185:**
- `readString`:
  ```swift
  let bytes = data.subdata(in: (data.startIndex + pos)..<(data.startIndex + pos + length))
  pos += length
  return String(data: bytes, encoding: .utf8)!
  ```
  `subdata` traps when `pos + length` runs past `data.endIndex`; the trailing
  `!` on `String(data:encoding:.utf8)` traps when the slice is not valid
  UTF-8.
- `readBytes`: same unchecked `subdata(in:)` slice — traps on a truncated
  payload whose declared length runs past EOF.
- `readUuid`: the
  `for i in 0..<16 { bytes[i] = data[data.startIndex + pos + i] }`
  loop traps on out-of-bounds subscript when fewer than 16 bytes remain.

Swift can't catch traps via `try`/`catch`, so any client codec built on this
runtime cannot recover from corrupt wire bytes — the entire process aborts.

**Affected:** any Swift client decoding untrusted UEBA bytes — for example,
arbitrary base64url text scanned from third-party QR codes via the camera.

**Workaround:** none possible at the consumer level — until this is fixed
upstream, exposing the Swift UEBA decoder to attacker-controlled bytes is
unsafe.

**Fix idea:** in `ScSwiftUebaCodecGenerator` (or whichever Baboon module
emits the runtime reader), change every force-unwrap and unchecked-index to
throw `BaboonCodecError.truncated` (or equivalent). Ideally bound-check at
the start of each reader entry point so the hot path is a single guard
rather than per-byte branches.

---

## Rust backend

Rust defects use prefix `BAB-R`. Probed at v0.0.185 — the :rust target
produces ≈400 generated source files + 1 runtime file from a mid-sized
schema, and the combined tree compiles **cleanly** under `cargo build` and
`cargo clippy` with no rustc warnings and no default-clippy warnings. The
two findings below are missing-feature gaps, not emit-bugs.

### Flag probe notes (informational, not defects)

Both `:rust`-specific flags were probed via side-by-side codegen + `diff -r`:

**`--rs-write-evolution-dict`** (default: omitted / false)
- Description per `baboon :rust --help`: "Add evolution metadata as Rust
  dictionary."
- Probe outcome: **zero diff** between flag-absent and flag-present outputs at
  v0.0.185. The Rust codegen template accepts the flag but does not yet
  implement evolution-dict emission — no files change when it is set.
- Recommendation: omit (= false). Re-probe on the next Baboon bump.

**`--rs-wrapped-adt-branch-codecs`** (default: omitted / false)
- Description per `baboon :rust --help`: "ADT branches encode/expect
  metadata."
- Probe outcome: **large diff** — enabling the flag replaces
  `#[derive(serde::Serialize)]` on every ADT branch struct with a hand-written
  `impl serde::Serialize` that wraps the branch fields in a tagged JSON map
  `{"BranchName": {<fields>}}`. This is a JSON wire-format change: the default
  `#[derive(serde::Serialize)]` on a unit-like branch struct serialises to
  `{}` (an empty object), whereas the wrapped form serialises to
  `{"BranchName": {}}`. UEBA encoding is not affected by this flag (UEBA is a
  binary codec with its own ADT tag byte — not controlled by serde).
- Cross-stack note: no other backend (Scala, Kotlin, Swift, TypeScript) emits
  the equivalent wrapped-branch JSON. Enabling this flag would break
  cross-stack JSON parity for every ADT type.
- Recommendation: omit (= false) for cross-stack parity.


---


### [BAB-R01] `*_service_wiring.rs` and `*_hub_wiring.rs` declare PascalCase wrapper functions
**Status:** [x] fixed (closed by baboon main 8788c832 / PR-45) · **Severity:** trivial (warning only)
**Symptom:** Each generated `<root>/<ns>/<service>_service_wiring.rs` (and
the per-hub `<root>/<ns>/<hub>_hub_wiring.rs`) declares two top-level wrapper
functions of the shape:

```rust
pub fn invoke_json_<Service><Rt: IBaboonServiceRt>(...) -> Result<...> { ... }
pub fn invoke_ueba_<Service><Rt: IBaboonServiceRt>(...) -> Result<...> { ... }
```

The trailing `<Service>` (PascalCase) follows Rust's type-name convention but
Rust functions are conventionally `snake_case`. rustc's default
`non_snake_case` lint fires once per declaration:

```
warning: function `invoke_json_<Service>` should have a snake case name
`invoke_json_<service>`
```

For a schema with 11 services we get exactly 22 warnings (11 services ×
{json, ueba}).

**Workaround:** prepend `#![allow(non_snake_case)]` (file-level inner
attribute) to each wiring file after codegen. Sentinel-guard by counting
`pub fn invoke_(json|ueba)_<Service>` declarations across the wirings; zero
matches means the bug is fixed (snake_case now) or the layout shifted, and
the script aborts so the workaround can't silently no-op.

**Fix idea:** in `ScRustServiceCodegen` (or whichever module emits the
wrappers), either rename the wrapper to `invoke_json_<service>` (where
`<service>` is the snake_case form, computed the same way the rest of the
Rust template snake-cases method names like `claim_stamp` / `first_contact`),
or emit a `#[allow(non_snake_case)]` attribute on each declaration.


---


### [BAB-R02] No `BaboonGenerated` trait implementations on generated types — type identifier strings unreachable from generic code
**Status:** [x] fixed (closed by baboon main 8788c832 / PR-46) · **Severity:** moderate (cross-stack-parity machinery must
reverse-engineer)
**Symptom:** The generator's runtime declares a metadata trait:

```rust
pub trait BaboonGenerated {
    fn baboon_domain_version() -> &'static str;
    fn baboon_domain_identifier() -> &'static str;
    fn baboon_type_identifier() -> &'static str;
}
```

The Swift, Kotlin and TypeScript backends emit equivalent `baboonTypeIdentifier`
/ `baboon_type_identifier` string constants on every generated type so
cross-stack parity tests can grep for `"<root>/<ns>/<Service>/<method>#in"`
IDs without parsing structure.

The :rust target emits **zero** `impl BaboonGenerated for X` blocks — no type
carries its `baboon_type_identifier` string. The trait itself is shipped in
the runtime but unused by the generated tree.

**Affected:** every type in the generated tree.

**Workaround:** reverse-engineer the IDs from wiring-file structure (parse
the parent dir name for `<ns>`, the function-name suffix for `<Service>`,
and the `match method.method_name.as_str() { "X" => ... }` arms for
`<method>`). Works today but is fragile to changes in how the wirings are
emitted.

**Fix idea:** in the Rust template that emits a per-type module
(e.g. `<root>/<ns>/<type>.rs`), append after the existing `impl X { ... }`
helper block:

```rust
impl crate::baboon_runtime::BaboonGenerated for X {
    fn baboon_domain_version() -> &'static str { "<v>" }
    fn baboon_domain_identifier() -> &'static str { "<root>" }
    fn baboon_type_identifier() -> &'static str { "<full/qualified/id>" }
}
```

For per-method `In`/`Out` types, the type identifier should match the
Scala/Swift/Kotlin convention: `<root>/<ns>/<Service>/<method>#in` and
`#out`. For domain types: `<root>/<ns>/<TypeName>`. Both are already
computed by the generator (the wiring files reference the equivalent
values) — it's a matter of plumbing them into a per-type impl.


---


### [BAB-G01] Service-method declaration without `data in {}` triggers typer crash
**Status:** [x] fixed (closed by baboon main 8788c832 / PR-47) · **Severity:** moderate
**Symptom:** When a service method declares only an output payload — i.e.
the user wants an emitter-only / `#out`-style declaration — and omits the
`data in {}` block, the typer crashes with:

```
java.util.NoSuchElementException: head on empty queue
    at <typer pipeline>.BaboonTranslator.scala:405
```

(Line number is approximate at v0.0.185; pin via the upstream source
tarball.)

**Repro:**
```bmo
ns notifications {
    root service Push {
        // CRASHES the typer:
        def updated (data out { kind: str }) #out
    }
}
```
Adding an empty `data in {}` block bypasses the crash:
```bmo
def updated (data in {} data out { kind: str }) #out  // OK
```

**Note:** the `err = X` clause is correctly optional — methods without it
type-check fine. The crash is specifically gated on the `data in` block
being absent.

**Affected:** any schema that wants to declare emitter-only push methods or
otherwise wants to omit the input payload.

**Workaround:** declare an empty `data in {}` block on every method that
doesn't have a real input. Wide impact: codegen emits a fully-realised
empty-input `In` type plus per-target codecs / runtime wiring for each, even
though the wire never carries it. For an N-method push surface, this is N×
dead `In` types per backend.

**Fix idea:** in the typer, treat an absent `data in` block as syntactic
sugar for `data in {}`, OR introduce a first-class `#out`-only / emitter
declaration that doesn't synthesize an input type at all.


---


### [BAB-J01] JSON codec emits `Map[String, _]` keys in nondeterministic order
**Status:** [x] fixed (closed by baboon main 8788c832 / PR-48) · **Severity:** minor
**Symptom:** Baboon's Scala JSON codec serialises `Map[String, V]` values
without sorting keys. For Scala's `Map.apply(...)` factory the resulting
iteration order is implementation-dependent: the small-map specialisations
(`Map1`/`Map2`/`Map3`/`Map4`) preserve insertion order, but `Map.apply` with
≥5 entries returns a `HashMap` whose iteration order depends on hash codes
+ JVM seed.

For most consumers this is invisible (JSON is order-insensitive), but it
breaks any test that emits a fixture file and re-runs the emitter expecting
a byte-identical re-emission. A regenerated fixture under a different JDK or
after a Scala upgrade can shift bytes silently.

**Repro:** emit a JSON fixture for a type containing a `Map[str, u32]` field
with ≥5 entries; run the emitter twice; diff the output. With v0.0.185 +
recent JDKs the diff is often non-empty.

**Workaround:** keep map entry counts ≤3 in fixtures (Scala `Map3` regime
preserves insertion order); document the fragility in code; rely on the
cross-stack parity test to catch any drift as a re-run mismatch.

**Fix idea:** in `ScJsonCodecGenerator`, sort `Map[String, V]` keys
lexicographically before emitting. JSON is order-insensitive on the wire
but deterministic emission is the property fixture-based round-trip tests
need.

---

## Resolved (closed by baboon main 8788c832, 2026-04-28)

- BAB-G01: fixed by PR-47.
- BAB-J01: fixed by PR-48.
- BAB-T01 + T02: fixed by PR-29.
- BAB-W01/W02/W03/W04: fixed by PR-30 + PR-49.
- BAB-R01: fixed by PR-45.
- BAB-R02: fixed by PR-46.
- BAB-K03/K04: fixed by PR-33.
- BAB-S02: fixed by PR-34/35 (schema migration deferred).
