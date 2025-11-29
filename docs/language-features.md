# Baboon Language Features

This draft walks through Baboon's language surface with pragmatic, readable examples pulled from the repository's test models and expanded into fuller scenarios. Every feature shows concrete syntax so you can copy, paste, and adapt it for real schemas.

## File prologue

```baboon
model acme.billing
version "1.0.0"
```

- `model` defines the namespace root for every referenced type.
- `version` tags the schema snapshot used by the evolution engine.

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

- Each entry maps a language tag (`cs`, `sc`, `ts`, etc.) to a fully qualified type name.
- Optional `with { key = "value" }` attributes are implementation hints for the backend.

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

## Derivations and codegen

Attach derivations on any type to request generated typeclass instances:

```baboon
root data AllBasicTypes: derived[json], derived[ueba] { ... }
```

The compiler currently ships JSON and UEBA codec derivation. Additional derivations can be added with the same `derived[...]` syntax. Baboon will produce C# and Scala classes from the same model and aggressively deduplicates shared shapes in generated C#.

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

Versioned files can be diffed by Baboon to emit migration code. When a change is obviously compatible (e.g., adding `data B2` to an ADT), Baboon derives conversions. Breaking changes (e.g., removing required fields) produce explicit stubs so you fail fast and implement the conversion manually.

## Code generation targets

- Scala classes, codecs, and helpers.
- C# classes with deduplication to minimize emitted artifacts.
- Generated UEBA codecs for compact binary payloads.

Invoke `mdl :build :mkdist` to generate and package all targets through the existing mudyla pipelines.
