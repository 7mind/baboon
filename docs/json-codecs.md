# JSON codec conventions

Baboon’s generated JSON codecs aim for predictable, deterministic shapes that round‑trip between Scala (circe) and C# (System.Text.Json). This document explains the layout and corner cases so you can interoperate or write custom codecs.

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

Generated codecs contain placeholder instances for foreign types; you must override them (`BaboonCodecs#Register` in C#, `BaboonCodecs.register` in Scala) with real implementations that follow these conventions.
