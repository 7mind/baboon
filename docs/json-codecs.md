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
