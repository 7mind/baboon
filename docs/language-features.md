# Baboon Language Features

This draft walks through Baboon's language surface with pragmatic, readable examples pulled from the repository's test models and expanded into fuller scenarios. Every feature shows concrete syntax so you can copy, paste, and adapt it for real schemas.

## File prologue

```baboon
model acme.billing
version "1.0.0"
```

- `model` defines the namespace root for every referenced type.
- `version` tags the schema snapshot used by the evolution engine.

## Pragmas

Pragmas are key-value directives placed between the `version` line and `include`/content. They configure per-domain, per-backend behavior without changing the CLI invocation.

```baboon
model acme.billing
version "2.0.0"

pragma scala.service.result.type = "scala.util.Either"
pragma scala.service.result.pattern = "[$error, $success]"
pragma rust.service.result.no-errors = "true"

include "./shared.baboon"

root data Invoice { ... }
```

- Keys are dotted identifiers; values are quoted strings.
- Multiple pragmas can appear in any order before `include` and definitions.
- Pragmas are scoped to the domain (model + version) they appear in.

Currently supported pragmas control service method return type rendering. See the [Services](#services) section for details.

## Root reachability, includes, and namespaces

Baboon only emits types that are transitively reachable from `root` declarations. Use `include` to pull in other files and `ns` to group related pieces without repeating the model header.

```baboon
model acme.checkout
version "2.3.0"

include "./shared-addresses.baboon"

ns orders {
  root data OrderId {
    value: uid
  }
  data OrderLine {
    sku: str
    qty: u16
  }
}

root data Order {
  id: orders.OrderId
  lines: lst[orders.OrderLine]
  shipping: ShippingAddress  // pulled from shared-addresses.baboon via include
}
```

Only `OrderId`, `OrderLine`, and `Order` survive codegen; helper types that are never referenced by a root vanish.

## Data records

Plain records are declared with `data` blocks. Derived typeclasses (e.g., codecs) attach via `: derived[...]`.
Fields are whitespace-separated (no commas).

```baboon
root data TransferOpt: derived[json], derived[ueba] {
  u: uid
  ol: opt[lst[Adt0]]
  ml: map[str, lst[Adt0]]
}
```

`TransferOpt` comes directly from `test/conv-test/pkg02.baboon` and illustrates optional values, lists, and maps used together.

## Algebraic data types (ADTs)

ADT members are nested `data` blocks inside an `adt`. Add constructors freely across versions; the evolution engine will scaffold conversions when safe.

```baboon
root adt PaymentMethod {
  data Card {
    pan: str
    holder: str
  }
  data Wallet {
    provider: str
    token: str
  }
}
```

Expanding the same ADT in a later version:

```baboon
root adt PaymentMethod {
  data Card {
    pan: str
    holder: str
  }
  data Wallet {
    provider: str
    token: str
  }
  data BankTransfer { iban: str }
}
```

This mirrors the `PaymentMethod` evolution in `pkg01.baboon` → `pkg02.baboon`, where constructor `BankTransfer` is added. Baboon will generate evolution stubs for manual migrations where data cannot be derived automatically.

## Structural inheritance with set algebra

Records can reuse and reshape fields via addition (`+`), removals (`-`), and intersections (`^`).

```baboon
data Address {
  line1: str
  city: str
  country: str
  postcode: str
}

data BillingAddress {
  + Address           // pull in every field from Address
  vatNumber: str
  - postcode: str     // remove an inherited field (removals specify the field type)
}

data AddressEssential {
  line1: str
  country: str
}

data MinimalAddress {
  + Address
  ^ AddressEssential  // keep only the fields that overlap with AddressEssential
}
```

- `+ TypeName` inlines every field from the added type.
- `- fieldName: Type` erases an inherited or local field (type is required in the declaration).
- `- ParentType` drops every field from that parent.
- `^ ParentType` keeps only the fields shared with the specified parent selection.

Structural operations are resolved as set operations during typing, keeping definitions declarative instead of conditional.

## Nominal contracts

Contracts act like interfaces: they define fields that must appear in implementors.

```baboon
contract Identified { id: uid }

root data User is Identified {
  name: str
}

root adt InvoiceEvent is Identified {
  data Issued {
    + User
    total: f64
  }
}
```

The compiler enforces that every `data` inside `InvoiceEvent` and the `User` record contain `id`. This is nominal inheritance, distinct from structural reuse via `+/-/^`.

## Enums (choices)

Enums (declared as `enum`) carry ordered, integer-backed members. Explicit numeric values are optional.

```baboon
enum PaymentStatus {
  Pending
  Settled = 10
  Failed
}
```

Only integer constants are allowed for explicit discriminators.

## Foreign types

Declare foreign types when the type is defined outside Baboon. You must register codecs in generated code, as outlined in `README.md`.

```baboon
foreign Money: derived[json], derived[ueba] {
  cs = "System.Decimal" with { format = "G29" }
  sc = "scala.math.BigDecimal" with { scale = "2" }
}
```

- Each entry maps a language tag (`cs`, `sc`, `ts`, `kt`, `jv`, etc.) to a fully qualified type name.
- Optional `with { key = "value" }` attributes are implementation hints for the backend.

## Type aliases

Type aliases give alternative names to existing types. They are resolved transparently during compilation and are not emitted into target languages — at usage sites, the alias is replaced by its target type.

```baboon
type BinaryData = bytes
type StringList = lst[str]
type OptionalInt = opt[i32]
type IntMap = map[str, i32]
```

Aliases can reference user-defined types or other aliases (chains are resolved recursively):

```baboon
type UserId = uid
type UserIdAlias = UserId    // resolves to uid

root data UserProfile {
  id: UserIdAlias            // compiled as uid
  tags: StringList           // compiled as lst[str]
  payload: BinaryData        // compiled as bytes
}
```

Aliases can be declared at the top level or inside namespaces:

```baboon
ns billing {
  type Amount = f64

  root data Invoice {
    total: Amount            // compiled as f64
  }
}
```

Aliases cannot be declared inside ADTs or other type definitions, and they cannot be marked `root` (since they produce no output).

## Built-in types and collections

Primitives: `i08`, `i16`, `i32`, `i64`, `u08`, `u16`, `u32`, `u64`, `f32`, `f64`, `f128`, `str`, `bytes`, `uid`, `bit`, `tsu` (UTC timestamp), `tso` (offset timestamp).

Collections: `opt[T]`, `lst[T]`, `set[T]`, `map[K, V]`. They compose freely:

```baboon
data InventorySnapshot {
  id: uid
  tags: set[str]
  stockBySku: map[str, i32]
  previous: opt[InventorySnapshot]
}
```

## Polymorphic `any` fields

`any` is a builtin field type that carries an *opaque envelope* — a length-prefixed payload plus a metadata header — letting you defer (or partially defer) the choice of inner type to runtime while keeping the outer record's shape stable across versions. Six syntactic forms cover the common cases:

| # | Syntax | Inner static type | Wire-meta carries |
|---|---|---|---|
| A | `any` | unknown | domain + version + typeid |
| B | `any[domain:this]` | unknown, but the wire's domain is asserted to match the field's declaring domain | version + typeid |
| C | `any[domain:current]` | unknown, but both the wire's domain and version are asserted to match the field's declaring (domain, version) | typeid |
| D1 | `any[T]` | `T` from any domain | domain + version |
| D2 | `any[domain:this, T]` | `T` in the declaring domain | version |
| D3 | `any[domain:current, T]` | `T` in the declaring (domain, version) | — |

In all six forms, an outer `length` prefix means a reader without the inner codec can still skip the field byte-accurately and forward the envelope unchanged — useful for proxies, evolution, and partial decoders. Forms A/B/C are the "polymorphic" cases where the inner type is identified at decode time by the meta-`typeid`. Forms D1/D2/D3 statically lock the inner type at the schema level; the meta still carries everything the field's kind doesn't statically pin down.

```baboon
data InnerPayload : derived[ueba] {
  label: str
  count: i32
}

root data Holder : derived[json], derived[ueba] {
  // polymorphic — runtime decides the inner type
  fAny:           any                              // full meta on wire
  fDomainThis:    any[domain:this]                 // domain implied; wire carries version + typeid
  fDomainCurrent: any[domain:current]              // domain + version implied; wire carries typeid

  // typed polymorphism — inner type fixed by the schema
  fUnderlying:        any[InnerPayload]                 // domain + version on wire
  fThisUnderlying:    any[domain:this, InnerPayload]    // version on wire
  fCurrentUnderlying: any[domain:current, InnerPayload] // nothing on wire (everything implied)

  // any in nested positions
  fOpt:    opt[any]
  fLst:    lst[any[InnerPayload]]
  fMapVal: map[str, any]
}
```

`any` is permitted in field positions and inside `opt`, `lst`, and as a `map` value. It is **not** permitted as a `set` element or `map` key — set/map identity needs a stable hashable representation and `AnyOpaque` deliberately avoids comparing across the JSON/binary branches.

The inner type referenced by D1/D2/D3 must itself derive UEBA (`: derived[ueba]`) — otherwise the cross-format conversion path can't materialize bytes from a JSON-branch payload.

### Generated language surface

Each target language exposes a sealed two-branch ADT (idiomatic spelling per language) for any-typed fields:

- **`AnyOpaqueUeba(meta, bytes)`** — value originated from the binary wire; carries opaque bytes.
- **`AnyOpaqueJson(meta, json)`** — value originated from the JSON wire; carries a parsed JSON value.

`meta` is `AnyMeta(kind, domain?, version?, typeid?)`. The kind byte is a bitmask (bit 0 = typeid, bit 1 = version, bit 2 = domain), giving the table above. Construction-time invariant checks reject reserved/inconsistent kind bytes.

Both branches round-trip natively in their own format with no codec lookup. To go between formats — emit a JSON-branch value as UEBA, or vice versa — the encoder needs a codec registry; pass one through `BaboonCodecContext.withFacade(useIndices, baboonFacade)`. For purely typed-payload code (your application layer), `BaboonCodecsFacade.decodeAny(opaque)` resolves the meta's typeid through the facade and returns a typed value, so callers don't deal with the envelope at all.

### Wire representations

- **UEBA** wire layout: `[length:i32][meta-length:i32][meta-kind:u8][meta-strings ULEB128]+[blob]`. See [`docs/ueba-format.md`](ueba-format.md#any-fields) for the full spec.
- **JSON** envelope: `{"$ak": <kind>, "$ad"?: <domain>, "$av"?: <version>, "$at"?: <typeid>, "$c": <inner JSON>}`. See [`docs/json-codecs.md`](json-codecs.md#any-fields) for details.

Both wires are byte-canonical across all 9 generated languages: the same fixture round-trips through any pair of languages identically.

### Schema evolution

`any` field changes are **breaking**: changing the variant (A → B, D1 → D2 etc.), changing the underlying type for D1/D2/D3, or adding/removing an `any` field all require an explicit migration stub. The compiler does not auto-derive transformations for `any`-bearing fields — too much depends on runtime payload data the typer can't see.

## Derivations and codegen

Attach derivations on any type to request generated typeclass instances:

```baboon
root data AllBasicTypes: derived[json], derived[ueba] { ... }
```

The compiler currently ships JSON and UEBA codec derivation. Additional derivations can be added with the same `derived[...]` syntax. Baboon will produce C#, Scala, Rust, TypeScript, Python, Kotlin, Java, and Dart code from the same model and aggressively deduplicates shared shapes in generated C#.

## Services

Services describe RPC-like signatures that can reference Baboon types.

```baboon
root service BillingApi {
  def CreateInvoice (
    in = CreateInvoiceRequest
    out = InvoiceId
    err = InvoiceError
  )
}
```

Service definitions are scoped like other members and can live inside namespaces (`ns` blocks). Method bodies accept `in`, `out`, and optional `err` markers, or inline DTO/ADT/enum definitions using the same braces/parentheses form.

Currently, service support is limited: Baboon does not provide transport runtime and does not generate all the necessary metadata.

### Configurable service return types

By default each backend wraps service method return types in a language-idiomatic result type:

| Backend | Default return type | Default for `err` absent |
|---------|-------------------|--------------------------|
| Scala | `scala.util.Either[Err, Out]` | `Out` directly |
| Rust | `Result<Out, Err>` | `Out` directly |
| C# | `Out` (no error wrapping) | `Out` directly |
| Python | `Out` (no error wrapping) | `Out` directly |
| TypeScript | `Out` (no error wrapping) | `Out` directly |
| Kotlin | `Out` (no error wrapping) | `Out` directly |
| Java | `Out` (no error wrapping) | `Out` directly |
| Dart | `Out` (no error wrapping) | `Out` directly |

You can override these defaults using pragmas in `.baboon` files or CLI flags.

#### Pragma keys

All pragma keys follow the pattern `{lang}.service.result.*` where `{lang}` is `scala`, `rust`, `cs`, `python`, `typescript`, `kotlin`, `java`, or `dart`.

| Pragma key | Value | Description |
|-----------|-------|-------------|
| `{lang}.service.result.no-errors` | `"true"` / `"false"` | When true, methods return just the output type |
| `{lang}.service.result.type` | e.g. `"Result"` | Wrapper type name (fully qualified if needed) |
| `{lang}.service.result.pattern` | e.g. `"<$success, $error>"` | Type parameter pattern; `$success` and `$error` are expanded |
| `{lang}.service.result.hkt` | `"true"` / `"false"` | Enable higher-kinded type parameter (Scala only) |
| `{lang}.service.result.hkt.name` | e.g. `"F"` | HKT type parameter name (default `F`) |
| `{lang}.service.result.hkt.signature` | e.g. `"[+_, +_]"` | HKT type parameter bounds (default `[+_, +_]`) |

#### Example: ZIO-style Scala services

```baboon
model acme.billing
version "1.0.0"

pragma scala.service.result.hkt = "true"
pragma scala.service.result.hkt.name = "F"
pragma scala.service.result.hkt.signature = "[+_, +_]"
pragma scala.service.result.pattern = "[$error, $success]"

root service BillingApi {
  def CreateInvoice (
    in = CreateInvoiceRequest
    out = InvoiceId
    err = InvoiceError
  )
}
```

Generated Scala:

```scala
trait BillingApi[F[+_, +_]] {
  def CreateInvoice(arg: CreateInvoiceRequest): F[InvoiceError, InvoiceId]
}
```

#### Example: error-free Python services

```baboon
model acme.billing
version "1.0.0"

pragma python.service.result.no-errors = "true"

root service BillingApi {
  def CreateInvoice (
    in = CreateInvoiceRequest
    out = InvoiceId
    err = InvoiceError
  )
}
```

Generated Python (the `err` type is ignored):

```python
class BillingApi(ABC):
    @abstractmethod
    def CreateInvoice(self, arg: CreateInvoiceRequest) -> InvoiceId:
        raise NotImplementedError
```

#### CLI flags

The same settings are available as CLI flags per backend. CLI flags override `.baboon` pragmas.

```bash
baboon \
  --model-dir ./models \
  :scala \
    --service-result-hkt true \
    --service-result-hkt-name F \
    --service-result-hkt-signature "[+_, +_]" \
    --service-result-pattern "[\$error, \$success]" \
    --output ./output/scala \
  :rust \
    --service-result-type "anyhow::Result" \
    --service-result-pattern "<\$success>" \
    --service-result-no-errors true \
    --output ./output/rust
```

Arbitrary pragma key-value pairs can also be passed via `--pragma`:

```bash
baboon :scala --pragma "scala.service.result.hkt=true" --pragma "scala.service.result.hkt.name=F"
```

### Service context parameters

By default, generated service methods only accept the input argument. You can inject an additional context parameter into every service method using the `{lang}.service.context` pragma family. This is useful for passing request context, authentication tokens, or other cross-cutting concerns.

Three modes are supported:

| Mode | Description |
|------|-------------|
| `none` | No context parameter (default) |
| `abstract` | Context type becomes a generic type parameter on the service trait/interface |
| `type` | Context type is a concrete, fully qualified type name |

#### Pragma keys

All pragma keys follow the pattern `{lang}.service.context*` where `{lang}` is `scala`, `rust`, `cs`, `python`, `typescript`, `kotlin`, `java`, or `dart`.

| Pragma key | Value | Description |
|-----------|-------|-------------|
| `{lang}.service.context` | `"none"` / `"abstract"` / `"type"` | Context parameter mode |
| `{lang}.service.context.type` | e.g. `"Ctx"` | Context type name (default `Ctx`); must be a valid identifier (simple name for `abstract` mode) |
| `{lang}.service.context.parameter.name` | e.g. `"ctx"` | Context parameter name (default `ctx`) |

#### Example: abstract context in Scala

```baboon
model acme.billing
version "1.0.0"

pragma scala.service.context = "abstract"
pragma scala.service.context.type = "Ctx"
pragma scala.service.context.parameter.name = "context"

root service BillingApi {
  def CreateInvoice (
    in = CreateInvoiceRequest
    out = InvoiceId
  )
}
```

Generated Scala:

```scala
trait BillingApi[Ctx] {
  def CreateInvoice(context: Ctx, arg: CreateInvoiceRequest): InvoiceId
}
```

#### Example: concrete context in TypeScript

```baboon
model acme.billing
version "1.0.0"

pragma typescript.service.context = "type"
pragma typescript.service.context.type = "RequestContext"

root service BillingApi {
  def CreateInvoice (
    in = CreateInvoiceRequest
    out = InvoiceId
  )
}
```

Generated TypeScript:

```typescript
export interface BillingApi {
    CreateInvoice(ctx: RequestContext, arg: CreateInvoiceRequest): InvoiceId;
}
```

#### Combining with HKT and result pragmas

Context pragmas compose with result pragmas. For example, combining HKT results with abstract context in Scala:

```baboon
pragma scala.service.result.hkt = "true"
pragma scala.service.context = "abstract"
```

Generated Scala:

```scala
trait BillingApi[F[+_, +_], Ctx] {
  def CreateInvoice(ctx: Ctx, arg: CreateInvoiceRequest): F[InvoiceError, InvoiceId]
}
```

#### CLI flags

The same settings are available as CLI flags per backend. CLI flags override `.baboon` pragmas.

```bash
baboon \
  --model-dir ./models \
  :scala \
    --service-context-mode abstract \
    --service-context-type Ctx \
    --service-context-parameter-name context \
    --output ./output/scala \
  :typescript \
    --service-context-mode type \
    --service-context-type RequestContext \
    --output ./output/typescript
```

Or via `--pragma`:

```bash
baboon :scala --pragma "scala.service.context=abstract" --pragma "scala.service.context.type=Ctx"
```

## Imports

Imports inline definitions from another *version of the same model*. The imported version is used only as a source of declarations—once merged, the current file’s `model`/`version` stay in effect.

```baboon
import "1.0.0" { * } without { LegacyId DebugStub }
```

- The string literal points to another version of the current `model` (e.g., pulling in `model acme.checkout` version `1.0.0` while editing `2.0.0`).
- All definitions from that version are copied in, then filtered by the `without` list. Names are whitespace-separated; `without` accepts `{ ... }` or `( ... )`.
- The referenced version must be discoverable via `--model` / `--model-dir`.

## Inclusions

`include "<path>"` splices raw definitions from another file into the current model before typing. Included files contain only content (namespaces/defs) without repeating `model`/`version`.

```baboon
model acme.checkout
version "2.3.0"

include "./shared-addresses.baboon"

root data Order { shipping: ShippingAddress }
```

- Paths are resolved relative to provided model directories; includes are resolved recursively.
- Because the header comes from the current file, the included content inherits the same `model` and `version`.

## Evolution workflow

Versioned files can be diffed by Baboon to emit migration code. When a change is obviously compatible (e.g., adding `data BankTransfer` to an ADT in the example above), Baboon derives conversions. Breaking changes (e.g., removing required fields) produce explicit stubs so you fail fast and implement the conversion manually.

## Code generation targets

- **Scala** — classes, Circe JSON codecs, UEBA binary codecs, and evolution converters.
- **C#** — classes with aggressive deduplication, Newtonsoft.Json codecs, UEBA binary codecs, and evolution converters.
- **Rust** — native structs/enums with serde derive, custom UEBA binary codecs, and evolution converters.
- **Python** — dataclasses with custom JSON codecs.
- **TypeScript** — classes with function-based JSON and UEBA codecs, and evolution converters.
- **Kotlin** — data classes with Jackson JSON codecs, UEBA binary codecs, and evolution converters.
- **Java** — records/classes with Jackson JSON codecs, UEBA binary codecs, and evolution converters.
- **Dart** — classes with dart:convert JSON codecs, UEBA binary codecs, and evolution converters.

Invoke `mdl :build :mkdist` to generate and package all targets through the existing mudyla pipelines.
