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
- Enums → `toString` of the enum case (Pascal-cased canonical name; see the enum wire-format spec).
- `id` types → the canonical identifier repr `<Name>:<ver>#field:value:...:{nested}` documented in [`docs/spec/identifier-repr.md`](spec/identifier-repr.md). Multi-field identifiers are permitted as keys.
- Single-primitive-field wrapper DTOs (`data Foo { v: str }` etc.) → the inner field is peeled and serialized as the bare primitive, so keys appear in their inner-key form (e.g. plain strings for a `v: str` wrapper, decimal numbers for `v: i32`).
- Foreign types (when allowed as keys) → encoded via the per-foreign `<Foreign>_KeyCodec` extension hook (see [Custom-foreign key codecs](#custom-foreign-key-codecs-m24--pr-i1a) below).
- Other user types are not permitted as map keys; the validator rejects them at type-check time. Floats are asymmetric: `map[f64, V]` (builtin) is allowed, but `map[Wrapper, V]` where `Wrapper { v: f64 }` is rejected.

Shipped: M19 / PR-59..PR-61 (id + DTO map keys), M24 / PR-I.1a..PR-I.3 (custom-foreign key codecs).

### Custom-foreign key codecs (M24 / PR-I.1a)

Foreign types mapped to a non-stringy host type (anything other than the host language's native string) need user-provided `encodeKey`/`decodeKey` to participate in JSON map-key conversion. Each backend emits a per-foreign extension hook so the user can register a codec at application boot.

**Default behavior — stringy foreigns:** when the host-language declaration is the language's canonical string type (e.g. Scala `java.lang.String`, Java/Kotlin `String`, C# `System.String`/`string`, TypeScript `string`, Dart `String`/`dart.core.String`, Swift `Swift.String`/`String`, Python `builtins.str`/`str`, Rust `std::string::String`/`String`/`&str`), the emitted default implementation is identity round-trip — no host registration needed and the codec works out of the box.

**Non-stringy foreigns:** the default implementation throws/panics with an FQN-bearing diagnostic of the form `<FQN>_KeyCodec is not registered; call <FQN>_KeyCodec.register(impl) at app boot.` The host MUST register an implementation before the first encode or decode.

**Per-backend hook surface:**

| Backend | Hook surface (per `foreign F`) |
|---------|--------------------------------|
| Scala | `trait F_KeyCodec` + companion `object F_KeyCodec` (with `register`, `instance`, private `DefaultImpl`) |
| Java | `interface F_KeyCodec` + final class `F_KeyCodecHost` (`register` / `instance` static methods) |
| Kotlin / Kotlin-KMP | `interface F_KeyCodec` + `object F_KeyCodecHost` |
| C# | `public interface F_KeyCodec` + `public static class F_KeyCodecHost` (with `Register`, `Instance`, `EncodeKey`, `DecodeKey`) |
| TypeScript | `export interface F_KeyCodec` + `export const F_KeyCodecHost = { register, get instance }` |
| Dart | `abstract class F_KeyCodec` + `class F_KeyCodecHost` (static `_instance` + `register` + `instance` getter) |
| Swift | `public protocol F_KeyCodec` + `public enum F_KeyCodecHost` (with `register` / `instance` / private `DefaultImpl`) |
| Python | `class F_KeyCodec(Protocol)` + `class F_KeyCodecHost` (`@staticmethod register` / `instance`) |
| Rust | `pub trait F_KeyCodec: Send + Sync` + `register_<f>_keycodec` / `<f>_keycodec()` accessors + serde adapter module `<f>_as_map_key` |

**Sample registration (Scala) for a non-stringy custom foreign `ObscureInt`:**

```scala
// Stringy foreigns: no registration needed — default impl is identity.
//
// Non-stringy customs require the host to register before first encode/decode:
ObscureInt_KeyCodec.register(new ObscureInt_KeyCodec {
  def encodeKey(v: ObscureInt): String = v.toString
  def decodeKey(s: String): ObscureInt = ObscureInt.parse(s)
})
```

**Cross-language wire-form lock-in:** the canonical fixture `test/conv-test/m24-foreign-keycodec.baboon` (a stringy `FStr` foreign + `ItemKey { v: FStr }` + `ForeignKeyHolder { m: map[ItemKey, str] }`) emits `m24-foreign-keycodec.json` byte-identically (md5 `1f1ef66abe5a9a24321c6e615851281d`) across all 8 compact-emit backends (C#, Dart, Java, Kotlin, Kotlin-KMP, Rust, Swift, TypeScript). Scala and Python emit pretty by default, with in-test compact re-emit verifying the same wire form.

Shipped: M24 / PR-I.1a (Scala reference) → PR-I.1b (Java/Kotlin/KMP) → PR-I.1c (C#) → PR-I.1d (Dart/TypeScript) → PR-I.2 (Swift/Python) → PR-I.3 (Rust).

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
