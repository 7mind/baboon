# JSON codec conventions

Baboon's generated JSON codecs aim for predictable, deterministic shapes that round‑trip across all supported backends: Scala (Circe), C# (System.Text.Json), Rust (serde), TypeScript, Python, Kotlin (Jackson), Java (Jackson), and Dart (dart:convert). This document explains the layout and corner cases so you can interoperate or write custom codecs.

## General rules

- Field names match schema identifiers verbatim.
- Collections are JSON arrays.
- Maps are JSON objects; keys are stringified (see “Map keys”).
- Options emit `null`/omission vs. value depending on target:
  - Scala: `None` becomes `null`; `Some` encodes the wrapped value.
  - C#: `null` reference is encoded as JSON `null`; value types use `WriteOptionVal` and produce `null` or the wrapped value.
- ADT branches are represented as their branch object; when `wrappedAdtBranchCodecs` is enabled for a target, branches are wrapped as `{ "<TypeName>": { ...payload... } }`.
- Primitives use native JSON numbers/booleans/strings; 64‑bit unsigned become decimal strings to avoid loss of precision in JavaScript.

## Primitive mappings

- `bit` → JSON boolean.
- Signed ints/floats → JSON numbers.
- Unsigned ints:
  - `u08`/`u16` → JSON numbers.
  - `u32` → JSON number in Scala, number in C# (fits 53 bits).
  - `u64` → JSON string (Scala uses `toUnsignedBigInt`, C# uses `BigInteger`), because it may exceed JS safe integer range.
- `f128` → JSON number via `BigDecimal`/`decimal`.
- `str` → JSON string (UTF‑8).
- `bytes` → Base64 string in C# (`ByteString.Encode()`), and `.Parse` on read.
- `uid` → JSON string (canonical GUID string).
- `tsu`/`tso` → JSON string using `BaboonTimeFormats` (`formatTsu`/`formatTso` on Scala, `ToString` on C#).

## Map keys

Map keys are always strings in JSON. The string form depends on the key type:

- Builtins → `ToString`/`parse` of the scalar (timestamps use formatted strings; booleans/numbers use culture‑invariant formats).
- Enums → encoded enum value to string.
- Foreign types (when allowed as keys) → encoded using their codec and then `ToString()`.
- Other user types are not permitted as map keys.

## ADTs and contracts

- Branch selection is structural: the caller chooses which branch codec to call. With `wrappedAdtBranchCodecs=true` the encoder wraps branches in a single‑entry object keyed by the branch name; the decoder unwraps accordingly.
- Contract fields are enforced at typechecking; codecs simply serialize the resulting field set.

## Any fields

A field of type `any` (or any of its qualified forms — see [`docs/language-features.md#polymorphic-any-fields`](language-features.md#polymorphic-any-fields)) serializes as a single object envelope that wraps the inner JSON payload:

```json
{
  "$ak": <kind>,
  "$ad": "<domain>",
  "$av": "<version>",
  "$at": "<typeid>",
  "$c":  <inner JSON value>
}
```

- `$ak` (any-kind) — integer 0–7. Bitmask: bit 0 = typeid, bit 1 = version, bit 2 = domain. Kinds `0x04`/`0x05` are reserved.
- `$ad` (any-domain) — present iff bit 2 set in `$ak`.
- `$av` (any-version) — present iff bit 1 set in `$ak`.
- `$at` (any-typeid) — present iff bit 0 set in `$ak`.
- `$c` (content) — the inner payload's JSON form; any JSON value (object, array, primitive, or `null`).

The `$a*` prefix is deliberate: it avoids colliding with `BaboonTypeMetaCodec`'s top-level `$d`/`$v`/`$t` keys, so an `any` envelope handed to `BaboonTypeMeta.readMeta` reads zero meta fields and falls through cleanly instead of misinterpreting the payload.

### Variants

| DSL form | `$ak` | Keys present (besides `$ak` + `$c`) |
|---|---|---|
| `any` | `7` | `$ad`, `$av`, `$at` |
| `any[domain:this]` | `3` | `$av`, `$at` |
| `any[domain:current]` | `1` | `$at` |
| `any[T]` | `6` | `$ad`, `$av` |
| `any[domain:this, T]` | `2` | `$av` |
| `any[domain:current, T]` | `0` | — |

For typed forms (D1/D2/D3), the receiver knows the inner type at the schema level so `$at` is omitted from the wire. The remaining meta on the wire is whatever the kind byte still claims.

### Worked example

Schema:
```baboon
data InnerPayload : derived[json] {
  label: str
  count: i32
}

data Holder : derived[json] {
  f: any[InnerPayload]   // variant D1, $ak = 6
}
```

Value `{ f = AnyOpaqueJson(meta = (kind=0x06, domain="my.ok", version="1.0.0", typeid=null), json = {label: "hi", count: 7}) }`:

```json
{
  "f": {
    "$ak": 6,
    "$ad": "my.ok",
    "$av": "1.0.0",
    "$c": { "label": "hi", "count": 7 }
  }
}
```

The same `Holder` written via the UEBA codec produces a byte-canonical UEBA `any` field; either format round-trips through any of the 9 generated languages identically.

### Cross-format conversion

To emit a JSON-branch value (`AnyOpaqueJson(meta, json)`) into the UEBA wire — or a UEBA-branch value (`AnyOpaqueUeba(meta, bytes)`) into JSON — the encoder needs a codec registry on the context. Pass one via `BaboonCodecContext.withFacade(useIndices, facade)`. Without it the encoder fails fast with a typed `BaboonEncoderFailure`.

For application code that just wants typed payloads regardless of which wire the value arrived on, `BaboonCodecsFacade.decodeAny(opaque)` does the lookup and decode for you.

## Determinism and formatting

- Arrays and objects are written in declaration order of fields.
- No whitespace/pretty printing is emitted; callers can post‑process if needed.
- Missing required fields throw during decode; unexpected fields are ignored by generated decoders (runtime may differ per target version).

## Foreign types

Generated codecs contain placeholder instances for foreign types; you must override them (e.g. `BaboonCodecs#Register` in C#, `BaboonCodecs.register` in Scala) with real implementations that follow these conventions. Refer to the foreign type registration mechanism for your target language.

## Examples

### DTO

Schema:
```
data User { id: uid, name: str, age: opt[i32], tags: lst[str] }
```

Value:
```
{
  "id": "550e8400-e29b-41d4-a716-446655440000",
  "name": "Ada",
  "age": 42,
  "tags": ["core","beta"]
}
```

If `age` is missing/`null`, it decodes to `None`/`null` depending on target.

### ADT branch wrapping

Schema:
```
adt PaymentMethod {
  data Card { pan: str }
  data Wallet { provider: str }
}
```

- Default (no wrapping): encode a `Card` branch as `{ "pan": "1234" }`.
- With `wrappedAdtBranchCodecs=true`: encode as `{ "Card": { "pan": "1234" } }`.

### Unsigned and map keys

Schema:
```
data Inventory {
  stock: map[u64, u32]
}
```

Value:
```
{
  "stock": {
    "18446744073709551615": 10,
    "42": 5
  }
}
```

`u64` keys and values are stringified when needed to preserve precision.
