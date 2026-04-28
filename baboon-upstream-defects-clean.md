# Baboon — Upstream Defect Tracker

Bugs and codegen surprises in **Baboon 0.0.185** discovered while building a
non-trivial multi-target client.

Status: `[ ]` open · `[~]` fix in progress upstream · `[x]` fixed upstream

Backends covered: **Scala** (battle-tested), **TypeScript** (production use,
rough edges), **Kotlin** (battle-tested), **Swift** (not yet prod-tested —
expect new defects).

---

## Scala backend

### [BAB-S01] `*_Wiring.scala` for ns-scoped services emitted in wrong package
**Status:** open · **Severity:** major
**Symptom:** When a `root service` lives inside an `ns <name> { ... }` block,
Baboon emits per-service `<Service>_Wiring.scala` in the model's *root*
package but references the service trait by its unqualified name. The trait
itself is correctly placed in `<root>.<ns>`. The wiring file fails to compile.
**Affected:** every ns-scoped service.
**Fix idea:** emit `<ns>/<Service>_Wiring.scala` in `<root>.<ns>` package, OR
qualify the trait reference in the existing root-placed file.

### [BAB-S02] Enum case-name mismatch between case-object emission and UEBA encoder match arms
**Status:** open · **Severity:** major
**Symptom:** When source enum members are written lowercase or snake_case
(e.g. `cafe`, `bar_pub`), the Scala generator emits case objects with a
capitalised first letter (`Cafe`, `BarPub`) but the UEBA encoder's `match`
arms reference them uncapitalised — the produced Scala doesn't compile.
**Workaround:** hand-PascalCase every enum member at the source level; map
back to spec names at the wire boundary.
**Fix idea:** normalise the casing in *one* place during codegen, ideally
preserving the source identifier verbatim in the enum-value string codec
output and emitting the Scala identifier with a deterministic transform.

### [BAB-S04] Codec emission requires opt-in (and CLI flags are undocumented)
**Status:** open · **Severity:** minor (DX)
**Symptom:** Baboon emits UEBA + JSON codecs only when explicitly requested,
either per-type (`: derived[ueba], derived[json]` in the schema) or globally
via `--generate-ueba-codecs-by-default=true` / `--generate-json-codecs-by-
default=true`. Both flags default `false`. Both are absent from
`baboon --help` but present in `CLIOptions.scala` as
`generateUebaCodecsByDefault` / `generateJsonCodecsByDefault`.
**Fix idea:** either flip the defaults to `true`, or add the flags to
`--help` output.

---

## TypeScript backend

### [BAB-T01] Strict-mode tsconfig produces ~615 errors against generated output
**Status:** open · **Severity:** moderate
**Symptom:** Baboon's TS output, against a strict tsconfig
(`verbatimModuleSyntax`, `noImplicitReturns`, `noUncheckedIndexedAccess`),
emits ~615 type errors in three classes:
- 247× **TS1484** — value-style imports (`import { Foo }`) of interface-only
  symbols. `verbatimModuleSyntax` requires `import type { Foo }`.
- 347× **TS7030** — ADT branch encoders' `match` flow can return implicitly.
  `noImplicitReturns` requires every branch to be explicit.
- 21× **TS2322 / TS2345** — `BaboonBinReader.readByte` is typed `number` but
  returns `undefined` past EOF; flows through to assignments. With
  `noUncheckedIndexedAccess` the type is unsound — a truncated UEBA payload
  silently zero-fills past EOF.
**Workaround:** blanket-prepend `// @ts-nocheck` to every emitted `.ts` file
(does not leak across module boundaries).
**Fix idea:** emit `import type` for type-only symbols; emit explicit
default branches in ADT encoders; bounds-check `readByte` (return
`number | undefined` and have decoders short-circuit on the first
`undefined`).

### [BAB-T02] Bundler `MISSING_EXPORT` on type-only symbols imported as values
**Status:** open · **Severity:** moderate
**Symptom:** Baboon's `BaboonSharedRuntime.ts` exports several names as TS
interfaces only. Rolldown (and other esbuild-style bundlers) fail
`MISSING_EXPORT` when downstream code does `import { Foo, … }` at value
position even though the import is type-only at the source level — types are
erased before bundling and the bundler sees no runtime binding.
**Workaround:** post-process `BaboonSharedRuntime.ts` to append
`export const X = {};` runtime stubs for the affected names.
**Fix idea:** related to BAB-T01 — emit `export type { Foo }` (and require
consumers to `import type`) instead of `export interface Foo`, OR also emit
a runtime stub.

---

## Kotlin backend

### [BAB-K01] `*ServiceWiring.kt` for ns-scoped services emitted in wrong package
**Status:** open · **Severity:** major
**Symptom:** Direct Kotlin-target counterpart of [BAB-S01]. When a `root
service` lives inside an `ns <name> { ... }` block, Baboon emits per-service
`<ns>.<Service>ServiceWiring.kt` files at `<output>/<root>/<ns>.<Service>ServiceWiring.kt`
declared in `package <root>` but referencing the service trait by an
unqualified name (e.g. `FooService`) while the trait itself lives in
`package <root>.<ns>`. The unqualified reference is unresolvable from
`package <root>`, so the file fails to compile.
**Affected:** every ns-scoped service — same set that triggers BAB-S01.
**Fix idea:** identical to BAB-S01 — emit
`<output>/<root>/<ns>/<Service>ServiceWiring.kt` declared in
`package <root>.<ns>`, OR qualify the service-trait reference in the
existing root-placed file.

### [BAB-K02] Generated codecs trip Kotlin's named-argument-supertype warning
**Status:** open · **Severity:** trivial (warning only)
**Symptom:** Every emitted `*_UEBACodec` companion at v0.0.185 declares
`encode(ctx, writer, value)` while its supertype `BaseGenerated` declares
the third parameter `instance`. Kotlin emits a non-fatal warning at every
override site:
"The corresponding parameter in the supertype 'BaseGenerated' is named
'instance'. This may cause problems when calling this function with
named arguments." With `-Werror` it would fail.
**Affected:** all generated UEBA codec objects.
**Fix idea:** harmonise the parameter name across the runtime-shared
abstract class and the codegen template.

### [BAB-K03] `*_UEBACodec.encode` overrides emit unnecessary type casts
**Status:** open · **Severity:** minor (compile warning only)
**Symptom:** Kotlin compiler emits `No cast needed.` warnings (~196 sites
in a non-trivial schema) across the generated `*_UEBACodec.kt` files. The
encoder bodies cast values to types they're already statically known to be.
**Fix idea:** drop the redundant casts in the UEBA encoder template.

### [BAB-K04] Generated decoders apply unnecessary `!!` non-null assertions on already-non-null receivers
**Status:** open · **Severity:** minor (compile warning only)
**Symptom:** Kotlin compiler emits `Unnecessary non-null assertion (!!)
on a non-null receiver of type 'X'.` at every site where the decoder
template applies `!!` to a receiver that the type system already knows
is non-null.
**Fix idea:** remove `!!` from the decoder template where the receiver is
already a non-null Kotlin type.

### [BAB-K05] Generated codec emits a redundant conversion-method call
**Status:** open · **Severity:** nit
**Symptom:** Kotlin compiler emits `Redundant call of conversion method.`
at one site in a generated codec body.
**Fix idea:** drop the redundant conversion in the affected template.

---

## Swift backend

Defect IDs use prefix `BAB-W` ("W" for sWift; "S" is taken by Scala).

### [BAB-W01] JSON decoder bodies emit doubled `try try` keyword
**Status:** open · **Severity:** moderate (warning only)
**Symptom:** Every emitted `*_JsonCodec.decode(...)` body wraps recursive
`Codec.decode(ctx, ...)` calls (and a few `BaboonTimeFormats.parseUtc`
calls) with two consecutive `try` keywords:
```swift
account: try try auth.AccountId_JsonCodec.instance.decode(ctx, jsonObj["account"]!),
```
Swift treats the outer `try` as wrapping the expression `try X`. Since
`try X` is itself never the throwing call (it's the underlying `decode`
that throws, already covered by the inner `try`), the outer `try` has
nothing to wrap and the compiler emits a warning at every site:
"warning: no calls to throwing functions occur within 'try' expression".
**Affected:** every type with at least one user-defined-type field that
has a JSON codec (hundreds of sites in a non-trivial schema).
**Workaround:** post-codegen sed pass replacing `try try ` with `try `.
**Fix idea:** drop one of the two `try` keywords in the Swift JSON codec
generator when emitting recursive-decoder field assignments.

### [BAB-W02] Optional-string JSON decoder uses force-cast in optional context
**Status:** open · **Severity:** minor (warning only)
**Symptom:** Generated JSON decoders for optional `str` fields emit:
```swift
field: try { let v = jsonObj["field"];
  return v is NSNull || v == nil ? nil : v! as! String }()
```
Swift's type checker, seeing the closure's result type inferred as
`String?` (because one ternary branch is `nil`), warns that the forced-`as!`
cast is being treated as an optional cast that "will never produce 'nil'" —
even though the surrounding `v is NSNull || v == nil` guard already proves
`v != nil`. (Sibling sites with member access like
`(v! as! NSNumber).doubleValue` don't warn because the member access
narrows the type before the ternary collapses to `String?`.)
**Workaround:** post-codegen sed rewriting `: v! as! String }()` to
`: (v as! String) }()` — the closure body has already proved
`v != nil && !(v is NSNull)` so an unforced-`v` bind is semantically
identical and sidesteps the optional-context inference.
**Fix idea:** in the Swift JSON decoder template for optional `str` fields,
drop the `!` (the `v` is already known non-nil by the guard) — emit
`v as! String` instead of `v! as! String`. Or rewrite the closure to use
`if let` / early return.

### [BAB-W03] Generated codecs emit a residual stream of `try`-on-non-throwing warnings
**Status:** open · **Severity:** minor (warnings only)
**Symptom:** Baboon's Swift JSON decoder template emits a `try` keyword in
front of every field expression regardless of whether the expression
actually throws. After collapsing the BAB-W01 doubled-`try` and stripping
single-`try` from the bulk surface forms (`try jsonObj[X]! as! T`,
`try BaboonTimeFormats.parseUtc(`, `try BaboonByteStringTools.fromHexString(`,
`try UInt<N>(truncatingIfNeeded`, `try Int<N>(truncatingIfNeeded`,
`try (jsonObj[X]! as! NSNumber).<member>`), residual sites remain in three
surface-form classes that a sed pass cannot safely rewrite:

1. **`try { let v = ...; return v is NSNull || v == nil ? nil : ... }()`
   closures**. The closure body has no throwing call; emitted for optional
   fields with non-`str` types and for optional-`str` fields whose body
   uses member access (e.g. `(v! as! NSNumber).doubleValue`).

2. **`try` keyword preceding a multi-line ternary on the next line**.
   Emitted for `i64`/`u64` fields where the JSON value may be either
   string or number:
   ```swift
   amount_minor: try
       (jsonObj["amount_minor"]! is String
       ? Int64(jsonObj["amount_minor"]! as! String)!
       : Int64(truncatingIfNeeded: (jsonObj["amount_minor"]! as! NSNumber).int64Value))
   ```

3. **`try (jsonObj[X]! as! [Any]).map { e0 in e0 as! T }` plus the parallel
   UEBA `try (0..<Int(reader.readI32())).map { _ in reader.readString() }`
   form**. The `.map` closure body has no throwing call; the outer `try`
   is dead.

**Additional BaboonRuntime warnings at v0.0.185** that share the same
generator-template root cause but are about variable usage in the runtime
source itself:
- `BaboonBinWriter.writeRawDecimal`: `variable 'carry' was never used`
- Same function: `variable 'digits' was never mutated; consider changing to 'let' constant`
- Date-handling helper: `variable 'comps' was never mutated; consider changing to 'let' constant`

**Workaround:** disable warnings on the generated target via
`swiftSettings: [.unsafeFlags(["-suppress-warnings"])]` — note that
`unsafeFlags` makes the target non-distributable as a binary SwiftPM
dependency. Hand-written + runtime targets stay warning-strict. The
runtime warnings are patched in-place by a post-codegen sed pass.

**Fix idea:** in the Swift JSON decoder template, only emit `try` in
front of expressions that actually call a throwing function (i.e.
`Codec.decode(...)` calls and any helpers marked `throws`). Drop the
keyword for pure casts, dictionary subscripts, integer constructors, and
the optional-field ternary closure. For the BaboonRuntime sites, mark
the unused `carry` variable unused (or remove the declaration) and switch
`digits`/`comps` from `var` to `let`.

### [BAB-W04] Swift UEBA reader traps on truncated/invalid input instead of throwing
**Status:** open · **Severity:** major
**Symptom:** Baboon's Swift UEBA reader in `BaboonRuntime/baboon_runtime.swift`
unconditionally trusts the byte stream — `readString`, `readBytes` and
`readUuid` all index `data` and force-unwrap `String(data:encoding:.utf8)`
without bounds-checking. Concrete shapes at v0.0.185:
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
  `for i in 0..<16 { bytes[i] = data[data.startIndex + pos + i] }` loop
  traps on out-of-bounds subscript when fewer than 16 bytes remain.

Swift can't catch traps via `try`/`catch` (they call `fatalError` /
`Swift.fatalError` semantics), so any client codec built on this runtime
cannot recover from corrupt wire bytes — the entire process aborts.
**Affected:** any Swift client decoding untrusted UEBA bytes — most
acutely codecs fed arbitrary base64url text scanned from third-party QR
codes via the camera, but the same hazard applies to any consumer of UEBA
bytes sourced from outside a trusted transport pipe.
**Workaround:** none — until this is fixed upstream, decoding a
maliciously corrupt UEBA payload will crash the host process.
**Fix idea:** in the Swift UEBA codec generator (or whichever Baboon module
emits the runtime reader), change every force-unwrap and unchecked-index
to throw `BaboonCodecError.truncated` (or equivalent). Ideally
bound-check at the start of each reader entry point so the hot path is a
single guard rather than per-byte branches.

---

## CLI / build-tool issues

### [BAB-C01] CLI flags `--omit-most-recent-version-suffix-*` removed without notice
**Status:** open · **Severity:** trivial
**Symptom:** Pre-v0.0.185 invocations referenced
`--omit-most-recent-version-suffix-*` flags. They no longer exist in v0.0.185;
that behaviour is now the default. Anyone porting an older Baboon invocation
will get an "unknown flag" error.
**Fix idea:** keep removed flags as no-op aliases for one minor version with
a deprecation warning, then drop.

---

## Resolved (kept for archive — remove when next Baboon bump lands)

(none yet — every entry above is open as of 2026-04-26.)
