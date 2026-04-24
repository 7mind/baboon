# Baboon `any` / AnyOpaque fields — design spec

Tracks GitHub issue #69. This doc is the single source of truth for the feature; the review loop's `tasks.md` and `defects.md` reference it.

## Goal

Add an `any` field type to Baboon that carries an *opaque* payload — a length-prefixed blob with a metadata header — enabling:
- Forward-compatible field evolution (a reader without the inner codec can still skip the field).
- Polymorphic-by-metadata payloads: receive a value whose concrete type is known only at runtime from metadata (type id, possibly domain and version).
- Precise typed forms where the inner type is known statically but the envelope is preserved for evolution safety.

## DSL

Six syntactic forms. All compile to the same language-level ADT `AnyOpaque`; only the wire-meta and validator rules differ.

| # | Syntax | Wire meta | Meta-kind | Static type of payload |
|---|---|---|---|---|
| A | `any` | domain + version + typeid | `0x07` | unknown |
| B | `any[domain:this]` | version + typeid | `0x03` | unknown |
| C | `any[domain:current]` | typeid | `0x01` | unknown |
| D1 | `any[T]` | domain + version | `0x06` | `T` (any domain) |
| D2 | `any[domain:this, T]` | version | `0x02` | `T` in current domain |
| D3 | `any[domain:current, T]` | — | `0x00` | `T` in current domain-version |

`qualifier` tokens: `domain:this`, `domain:current`. `T` is a Baboon type reference.

Meta-kind byte is a bitmask (bit 0 = typeid, bit 1 = version, bit 2 = domain). Unused combinations (`0x04`, `0x05`) are reserved for future use.

## Wire format (binary / UEBA)

```
any_field := length:i32         // bytes of meta-length + meta-kind + meta + blob (i.e. everything after length itself)
             meta-length:i32    // bytes of meta-kind + meta
             meta-kind:u8
             meta:bytes         // length-prefixed UTF-8 strings, in fixed order: domain, version, typeid (those present per meta-kind)
             blob:bytes         // UEBA-encoded payload, up to (length - 4 - meta-length) bytes
```

- Endianness and widths match existing UEBA codec (`LEDataOutputStream`, signed `i32`).
- Strings are length-prefixed UTF-8 following the existing `str` convention in `ScUEBACodecGenerator.scala:382`.
- Outer `length` lets any reader skip the whole field without needing the inner codec.
- `meta-length` lets a reader skip the meta block and/or future meta extensions without parsing them.
- `meta-kind` is the discriminator; new kinds can be added without breaking old readers (they stop parsing meta at `meta-length`).

## JSON envelope

```json
{ "$ak": 7, "$ad": "com.example.MyDomain", "$av": "1.2.3", "$at": "SomeType", "$c": <payload JSON> }
```

- `$ak`: meta-kind (integer).
- `$ad`, `$av`, `$at`: present iff the kind includes domain / version / typeid respectively.
- `$c`: payload JSON. Structured (not base64) — the receiver's JSON codec registry reads it directly.

`$c` reuses the existing `CONTENT_JSON_KEY` convention (`BaboonCodecsFacade.scala:15`). The meta keys are prefixed `$a` (for "any") to avoid any collision with `BaboonTypeMetaCodec`'s top-level keys `$d`/`$v`/`$t` — if an envelope is ever handed to `BaboonTypeMeta.readMeta`, it reads zero meta fields and falls through, instead of silently misinterpreting the envelope as a message header.

## Language surface

Non-generic sealed ADT. Identical shape across all target languages (with language-idiomatic spellings).

```scala
sealed trait AnyOpaque { def meta: AnyMeta }
final case class AnyOpaqueUeba(meta: AnyMeta, bytes: Array[Byte]) extends AnyOpaque
final case class AnyOpaqueJson(meta: AnyMeta, json: Json)         extends AnyOpaque

final case class AnyMeta(
  kind: Byte,
  domain:  Option[String],
  version: Option[String],
  typeid:  Option[String],
)
```

- `AnyOpaqueUeba` was born from binary; can be re-emitted as UEBA directly, or converted to JSON via registry lookup.
- `AnyOpaqueJson` was born from JSON; can be re-emitted as JSON directly, or converted to UEBA via registry lookup.
- For languages without ADTs, use whatever idiomatic tagged-union form the rest of the generator uses (same as existing ADT emission).

### Decoding from the app's perspective

Lazy:

```scala
opaque match {
  case AnyOpaqueUeba(meta, bytes) => codec.decode(ctx, LEDataInputStream(bytes)) // codec: BaboonBinCodec[T], user-supplied
  case AnyOpaqueJson(meta, json)  => codec.decode(ctx, json)                     // codec: BaboonJsonCodec[T]
}
```

Convenience — registry-resolved:

```scala
facade.decodeAny(opaque): BaboonValue[BaboonGenerated]    // uses meta.domain/version/typeid with Facade's compat resolution
```

If meta is missing components (variants D3, D2, D1), the facade fills them from the field's declaration (known at codec-generation time: passed into the field's codec closure), not from runtime state.

### Encoding

Variant D fields: the codec for the containing type knows the static T. If the user supplies an `AnyOpaque` whose `meta.typeid` disagrees with T, raise a runtime error ("type mismatch in typed-any field").

Variants A/B/C: encoder splices whichever branch is native. Cross-format convert happens only if user supplies the "wrong" branch:
- Have `AnyOpaqueJson`, encoding UEBA → facade lookup by `(domain, version, typeid)` → re-encode payload as UEBA bytes.
- Have `AnyOpaqueUeba`, encoding JSON → symmetric.

If facade lookup fails: runtime error.

## Validator rules

- `any[T]`, `any[domain:this, T]`, `any[domain:current, T]`: T must be a user-defined type that has `: derived[ueba]` (and `: derived[json]` if JSON codec gen is enabled for the containing type).
- All other variants: no compile-time check. Sender/receiver must ensure registrations exist; otherwise runtime error.
- `any` field cannot appear as a map key.
- `any` cannot appear as a set element (hash/equality undefined on opaque payloads when the codec is unknown).
- `any` may appear inside `opt[...]`, `lst[...]`, and as the value position of `map[K, ...]`.
- `any` field itself is always a new concept — cannot evolve to/from any other type automatically.

## Evolution

- Changing `any` variant (e.g. B → C): **breaking**, no auto-conversion.
- Changing T in variant D: **breaking**.
- Adding / removing an `any` field: follows the standard Baboon field add/remove rules. Since the outer length prefix makes the field skippable, readers of older versions can drop a newly-added `any` field cleanly.

## Integration with existing infra

- `BaboonCodecsFacade.scala:12` — facade is the runtime registry. No new registry needed.
- `AnyOpaque` decoders for variants A/B/C delegate to `facade.decodeFromBin` / `facade.decodeFromJson` when the user asks for a typed value.
- `BaboonTypeMeta.scala:95` in `BaboonRuntimeShared.scala` — existing meta block format inspires our meta header but stays separate: `BaboonTypeMeta` is per-message; our meta-kind is per-field.
- JSON content key `$c` matches `BaboonCodecsFacade.scala:15`.

## Compiler changes

### Parser (`parser/defns/DefDto.scala:14`, `parser/model/RawTypeRef.scala:5`)

Extend `typeParams` to accept two new categories of param alongside type refs:
- `domain:this`, `domain:current` (keyword qualifiers)
- Plain type refs (existing)

Either extend `RawTypeRef.Constructor` with a richer param AST, or introduce a new `RawTypeRef.AnyRef(qualifier: Option[QualKind], underlying: Option[RawTypeRef])`. Preferred: the latter, because `any` is special enough to not force every `Constructor` consumer to handle keywords.

### Typed AST (`typer/model/Typedef.scala:88`, `typer/model/TypeId.scala:30`)

Add:
```scala
object TypeId.Builtins { final val any = BuiltinScalar(TypeName("any")) }
```
and a new `TypeRef` case (so it's not forced into `Constructor` with type-only args):
```scala
final case class Any(variant: AnyVariant, underlying: Option[TypeRef]) extends TypeRef
sealed trait AnyVariant
object AnyVariant {
  case object Global  extends AnyVariant  // variant A / D1 (has or lacks T)
  case object ThisDom extends AnyVariant  // variant B / D2
  case object Current extends AnyVariant  // variant C / D3
}
```

Meta-kind derivation: `(variant, underlying.isDefined)` → byte, per the table above.

**Parser → typer split for bare `any`:** To preserve back-compat for user types named `any`, the parser does not treat bare `any` as a keyword. Bare `any` (no brackets) parses as `RawTypeRef.Simple(RawTypeName("any"), Nil)`. The typer is responsible for recognising that specific shape (exactly `any`, no prefix) as `TypeRef.Any(AnyVariant.Global, None)` — taking precedence over user-type lookup. `any[...]` forms come through as `RawTypeRef.AnyRef` and translate straightforwardly. This means the typer has two entry points into `TypeRef.Any`: the `AnyRef` path and the `Simple("any", Nil)` path.

### Validator (`validator/`)

- `any[T]` etc.: assert T exists, has `derived[ueba]`.
- `any` field: cannot be map key.
- Other existing rules continue to apply.

### Codec generators (one per language)

Per language (`translator/scl`, `translator/csharp`, ...):

1. **Type emission**: emit the `AnyOpaque` ADT and `AnyMeta` record into the generated "common" module per domain-version (not per type — shared). Or better: add them to `baboon-runtime/` and make every generated codec depend on the runtime (most languages already do this for `BaboonBinCodec` etc.).
2. **UEBA codec emission**: for a field of `any` type, generate encode / decode pairs that:
   - Encode: pattern-match on `AnyOpaque` branch; write `[length][meta-length][kind][meta][blob]` with facade-delegated conversion if needed.
   - Decode: read `length`; delegate to a helper `readAnyField(variantKind)` that reads meta-kind, asserts it matches the declared variant kind, reads meta, reads blob, returns `AnyOpaqueUeba`.
3. **JSON codec emission**: symmetric — read `$k`, assert match, read meta fields present, read `$c`, return `AnyOpaqueJson`.
4. **Typed variants** (D1/D2/D3): at decode time, the returned `AnyOpaque` still carries the raw payload — no automatic typed-decoding. The app decides when to resolve. (Validator already ensured a codec exists.)

### Runtime additions (`baboon-runtime/<lang>/`)

- `AnyOpaque` sealed ADT + `AnyMeta` record in every target runtime.
- `BaboonCodecsFacade` gets `decodeAny(opaque, expectedVariant): BaboonValue[BaboonGenerated]` convenience (all 9 runtimes).
- Helpers for reading / writing the meta header with the kind byte and string fields, in both UEBA and JSON forms.

## Testing

1. **Parser unit tests**: all six DSL forms parse; invalid combinations (`any[domain:bogus]`, `any[ , T]`) fail with readable errors.
2. **Typer unit tests**: each variant resolves to the right `AnyVariant` + underlying; invalid underlying types (primitive, collection, missing ueba codec) rejected.
3. **Validator unit tests**: map-key rejection; missing `derived[ueba]` rejection for variant D.
4. **Codegen golden tests** per language: compile a small `.baboon` model with each variant, assert shape of generated code.
5. **Round-trip tests** per language (`test/sc-stub` et al.):
   - Encode a concrete typed value into each variant, decode, expect `AnyOpaqueUeba` with the right bytes, resolve via facade, assert round-trip equality.
   - Same for JSON.
   - Cross-format: encode as UEBA, decode as JSON, convert via facade, assert equality.
6. **Cross-language interop** (`test/conv-test-*`): Scala encodes → C# decodes → Rust decodes → ... for all six variants.
7. **Evolution**: a field added in v2 is `any`; v1 reader with v2 message: skip the field cleanly. Also: field removed in v2; v2 reader with v1 message: standard "unexpected field" behavior.

## Phasing (review-loop iterations)

1. Parser + AST + typer + validator (Scala compiler only; no codegen yet).
2. Scala runtime additions + Scala UEBA + JSON codec generation + Scala round-trip tests.
3. C# end-to-end.
4. Rust end-to-end.
5. Kotlin end-to-end.
6. Java end-to-end.
7. TypeScript end-to-end.
8. Dart end-to-end.
9. Swift end-to-end.
10. Python end-to-end.
11. GraphQL schema emission (likely maps `any` to a scalar, documented).
12. OpenAPI schema emission (scalar or `oneOf`, documented).
13. Cross-language interop tests.

Each phase gates on `mdl :full-build` equivalent for that language.

## Out of scope for initial PR

- Compile-time codec registration (facade is populated by app code).
- Eager decoding (`AnyOpaque.value: Any` shape) — can be added later as an ergonomic layer.
- Variant E (cross-domain with static typing across unknown-to-sender domains) — not in the issue.
- Baboon-meta / baboon-lock representation for `any` fields beyond existing conventions.
