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

include "shared.bmo"

root data Invoice { ... }
```

- Keys are dotted; each segment is a chain of identifier words joined by single hyphens (`[A-Za-z_][A-Za-z0-9_]*` words, e.g. `no-errors`). Values are quoted strings.
- Multiple pragmas can appear in any order before `include` and definitions.
- Pragmas are scoped to the domain (model + version) they appear in.

Currently supported pragmas control service method return type rendering and service context parameters. See the [Services](#services) section for details. The same keys can be set from the CLI with `--pragma key=value`, which overrides in-file pragmas.

## Root reachability, includes, and namespaces

Baboon only emits types that are transitively reachable from `root` declarations. Use `include` to pull in other files and `ns` to group related pieces without repeating the model header.

```baboon
model acme.checkout
version "2.3.0"

include "shared-addresses.bmo"

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
  shipping: ShippingAddress  // pulled from shared-addresses.bmo via include
}
```

Only `OrderId`, `OrderLine`, and `Order` survive codegen; helper types that are never referenced by a root vanish.

## Data records

Plain records are declared with `data` blocks. Derived typeclasses (e.g., codecs) attach via `: derived[...]`.
Fields are whitespace-separated (no commas).

```baboon
adt Adt0 {
  data Branch1 { i: i08 }
  data Branch2 { s: str }
}

root data TransferOpt: derived[json], derived[ueba] {
  u: uid
  ol: opt[lst[Adt0]]
  ml: map[str, lst[Adt0]]
}
```

`TransferOpt` is adapted from `test/conv-test/pkg02.baboon` and illustrates optional values, lists, and maps used together.

`struct` is accepted as a synonym for `data`:

```baboon
struct Wrapper { v: str }
```

Anywhere a `{ ... }` body appears (`data`, `adt`, `enum`, `contract`, `service`, `foreign`, `ns`), parentheses `( ... )` are accepted as an equivalent block delimiter.

## Identifier types (`id`)

The `id` keyword declares a record whose values are first-class identifiers. Identifier types are structurally identical to `data` records but carry a parseable canonical string representation that round-trips byte-identically across all 9 backends.

```baboon
id PointId : derived[json], derived[ueba] {
  x: i32
  y: i32
}
```

- Field types are restricted to builtin scalars except floats (`bit`, `i08`..`i64`, `u08`..`u64`, `str`, `uid`, `tsu`, `tso`, `bytes`), nested `id` types, and aliases that resolve to one of the above. Floats (`f32`, `f64`, `f128`), collections (`opt`, `lst`, `set`, `map`), non-id user types, and `any` variants are rejected at validator time.
- Multi-field identifiers are supported (e.g. composite keys).
- Identifier types serialize on JSON and UEBA wires byte-identically to a `data` of the same shape; conversion between an `id` type and a `data` type with matching field shape is explicit but mechanical, so the two are interchangeable at the wire level.

### Canonical repr

Each identifier renders to the form `<Name>:<ver>#<field>:<value>:<field>:<value>:...`, with nested `id` values wrapped as `:{...}`. Five metacharacters (`\`, `#`, `:`, `{`, `}`) are escaped with a backslash. The full grammar, escape table, fixed-width `tsu`/`tso` rule, and per-type rendering table live in [`docs/spec/identifier-repr.md`](spec/identifier-repr.md).

Round-trip example using the schema above:

```scala
val p = PointId(42, -7)
p.toString
// "PointId:2.0.0#x:42:y:-7"

PointIdCodec.parseRepr("PointId:2.0.0#x:42:y:-7")
// Right(PointId(42, -7))
```

The `parseRepr` entry point lives on the per-type internal codec object (`<TypeName>Codec` for Scala; analogous host objects/modules in the other backends), not on the type's companion. Each backend ships its own `parseRepr` whose output matches the spec byte-for-byte.

Shipped: M18 / PR-54..PR-58.

## ADT branch inheritance and subtraction

ADTs can pull in branches from other ADTs using the same `+`, `^`, and `-` operators that `data` records use for structural inheritance. This lets you reuse error atoms, signal sets, or any other branch family across multiple ADTs without copy-pasting the constructors.

```baboon
adt Errors {
  data NotFound {}
  data Forbidden {}
}

adt UserErrors {
  + Errors             // include every branch of Errors
  data Banned {}       // plus a UserErrors-specific branch
}

adt AuthErrors {
  + Errors
  ^ Errors             // intersection — keep only branches present in both
}

adt PublicErrors {
  + Errors
  - Errors.NotFound    // exclude a single named branch
}
```

- `+ X` re-emits every branch of ADT `X` into the current ADT.
- `^ X` retains only branches whose constructor names appear in `X` as well — useful for projecting a wider error set down to a known subset.
- `- X` drops every branch of `X`; `- X.Branch` drops a single named branch.
- Chained inclusions auto-expand via toposort, so `+ A` where `A` itself does `+ B` pulls both A's and B's branches transparently.
- Constructor-name collisions are a typer error.

Branch reuse is a syntactic expansion at the typer-early stage; downstream codegen sees the fully-expanded constructor list. The expanded ADTs participate in evolution like any other ADT — adding/removing branches in a later version follows the standard rules in [Evolution workflow](#evolution-workflow).

Shipped: M20 / PR-62..PR-64.

## Identifier and DTO types as map keys

Beyond builtin primitives and enums, the following user types are permitted as JSON/UEBA `map[K, V]` keys:

- Any `data` record with exactly one primitive field (the wrapper "peels" to its inner key form on the wire).
- Any `id` type, including multi-field identifiers (encoded via the canonical repr from the [`id` section](#identifier-types-id)).
- Foreign types (accepted at the user's responsibility — see [Foreign types](#foreign-types) and the JSON map-keys section in [`docs/json-codecs.md`](json-codecs.md#map-keys)).

Constraints enforced by the validator:

- A user-typed key must declare `derived[json]` or `derived[ueba]` matching the codec used for the enclosing map.
- Floats are asymmetric: `map[ItemKey, str]` where `ItemKey { v: f64 }` is **rejected** (wrappers around floats), but `map[f64, str]` (builtin float key) is **allowed**.
- The validator rejects user-key types whose shape would render ambiguously on the wire.

Example:

```baboon
data ItemKey : derived[json], derived[ueba] {
  v: str
}

id OrderId : derived[json], derived[ueba] {
  region: str
  serial: u32
}

root data Inventory : derived[json], derived[ueba] {
  byItem:  map[ItemKey, i32]
  byOrder: map[OrderId, str]
}
```

Cross-link: see the [Map keys](json-codecs.md#map-keys) section in `docs/json-codecs.md` for per-key-type wire rules.

Shipped: M19 / PR-59..PR-61.

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

ADTs accept `derived[...]` annotations like records (`adt PaymentMethod : derived[json] { ... }`), and an ADT body may carry contract requirements that apply to every branch (see [Nominal contracts](#nominal-contracts)):

```baboon
contract Timestamped { at: tsu }

root adt Event {
  is Timestamped          // every branch receives/implements `at: tsu`
  contract Audited {      // contracts can also be declared inline, scoped to the ADT
    actor: str
  }
  data Created { name: str }
  data Deleted { is Audited reason: str }
}
```

The generated ADT representation extends the contract interface (e.g. the Scala sealed trait extends `Timestamped`), and contract fields are materialized in every branch.

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

Structural arms in `data` and `contract` bodies also accept template instantiations (`+ Page[i32]`, `- Stats[i32]`, `^ Page[i32]`) — see [Structural arms with template instantiation](#structural-arms-with-template-instantiation-data--contract-bodies).

## Nominal contracts

Contracts act like interfaces: they define fields that must appear in implementors. A contract is attached with an `is ContractName` declaration **inside the body** of the implementing type (it is a member-position declaration, not part of the header):

```baboon
contract Identified { id: uid }

root data User {
  is Identified
  name: str
}

root adt InvoiceEvent {
  is Identified
  data Issued {
    + User
    total: f64
  }
}
```

The compiler enforces that every `data` inside `InvoiceEvent` and the `User` record contain `id`; the generated types implement the contract's interface. This is nominal inheritance, distinct from structural reuse via `+/-/^`.

Contracts compose: a contract body may itself contain `is OtherContract` declarations as well as structural arms (`+`, `-`, `^`), and contracts may be declared inline inside an ADT body for branch-local requirements.

## Extracted contracts (`has mirror` / `has contract`)

Templated `data`, `adt`, and `id` hosts can synthesise a sibling contract that captures their parameter-free field surface, without requiring callers to duplicate the field list manually. The clause appears **inside the host body** alongside ordinary field declarations.

### Syntax

```baboon
data Page[T] {
  total: u32        // param-free field — eligible for extraction
  items: lst[T]     // T-dependent field — excluded from extracted contract
  has mirror  IPage         // mirror variant: standalone contract, no host relationship
  has contract IPageContract // contract variant: extracted + every instantiation implements it
}
```

Both clauses may coexist on the same host. Multiple `has mirror` and `has contract` clauses are permitted.

### `has mirror B`

Synthesises a sibling `contract B` that enumerates the host's parameter-free fields (own fields and those contributed via structural arms whose type arguments are fully concrete). `B` is a standalone type — it has **no relationship** to the host's instantiated types. Use it when you want to describe the common subset of fields without constraining the host's generated interfaces.

```baboon
data Payload[T] {
  label: str    // param-free
  value: T      // param-dependent — excluded from IMirroredPayload
  has mirror IMirroredPayload
}

root type IntPayload = Payload[i32] : derived[json], derived[ueba]
root type StrPayload = Payload[str] : derived[json], derived[ueba]

// IMirroredPayload is synthesised as: contract IMirroredPayload { label: str }
// IntPayload and StrPayload do NOT implement IMirroredPayload automatically.
// A third type can explicitly opt in: data Probe[T] { is IMirroredPayload }
```

### `has contract B`

Works identically to `has mirror B` for the synthesised contract, but additionally wires an implicit `is B` onto every instantiation of the host. The generated concrete types (from all type-alias instantiations of the host) implement `B`'s interface in all 9 target languages.

```baboon
data Box[T] {
  count: i32    // param-free — contributed to IBox
  item:  T      // excluded
  has contract IBox
}

root type IntBox = Box[i32] : derived[json], derived[ueba]
root type StrBox = Box[str] : derived[json], derived[ueba]

// IBox is synthesised as: contract IBox { count: i32 }
// IntBox and StrBox both implement IBox (implicit is IBox injected by the lowering pass).
```

### Through-parent contributions

Structural arms (`+ SomeTemplate[T, Concrete]`) contribute their parameter-free fields to the extracted contract when the arm's concrete type arguments are all non-param:

```baboon
data Base { base_field: i32 }
data Pair[A, B] { first: A  second: B }

data Container[T] {
  own:  i32
  item: T
  + Pair[T, i32]    // second:i32 is param-free → contributed; first:T → excluded
  + Base            // base_field:i32 → contributed
  has contract IContainer
}

// IContainer is synthesised as: contract IContainer { own: i32  second: i32  base_field: i32 }
```

### ADT and `id` hosts

The clause is equally valid in `adt` and `id` template bodies:

```baboon
contract ResultBase { tag: str }

adt Result[T] {
  is ResultBase        // all branches get tag:str; ResultBase contributes to IResult
  has contract IResult
  data Ok  { result: T }
  data Err { msg: str  }
}

id Key[T] {
  has contract IKey
  key: i64    // param-free
  v:   T      // excluded
}
```

### Restrictions

- `has mirror` / `has contract` are only valid inside templated (`[T, …]`) hosts. A non-template `data`, `adt`, or `id` should use a plain `contract` + `is` instead.
- The synthesised contract name `B` must not already exist in the same namespace.
- The extracted contract is never emitted as a `root` type itself; it is always reachable through the host's instantiations or explicit `is B` references.

Shipped: G6 / T37–T45.

## Enums (choices)

Enums (declared as `enum`) carry ordered, integer-backed members. Explicit numeric values are optional, but **all-or-none**: either every member carries a constant or no member does (a mix is a typer error).

```baboon
enum PaymentStatus {
  Pending = 1
  Settled = 10
  Failed  = -2
}

enum Direction {
  North
  South
}
```

Only integer constants (negative values included) are allowed for explicit discriminators. Enums accept `derived[...]` annotations like other types.

A member can be renamed across versions while keeping evolution derivable by annotating it with `was`:

```baboon
enum Color {
  Red
  Emerald : was[Green]   // renamed from Green in the previous version
}
```

See [Rename tracking](#rename-tracking-was).

## Foreign types

Declare foreign types when the type is defined outside Baboon. You must register codecs in generated code, as outlined in `README.md`.

```baboon
foreign Money: derived[json], derived[ueba] {
  cs    = "System.Decimal" with { "format" = "G29" }
  scala = "scala.math.BigDecimal" with { "scale" = "2" }
}
```

- Each entry maps a language tag to a fully qualified native type name. Valid tags: `scala`, `cs`, `py`, `rust`, `typescript`, `kotlin`, `java`, `dart`, `swift`, plus the special `rt` entry below.
- Optional `with { "key" = "value" }` attributes are implementation hints for the backend; both keys and values are quoted strings.

Two additional entry forms:

```baboon
data StrWrapper : derived[json], derived[ueba] { v: str }

foreign ObscureInt : derived[json], derived[ueba] {
  rt = i32                       // wire-level Baboon equivalent
  cs = "System.Int32"
  typescript = StrWrapper        // per-language fallback to a generated Baboon type
}
```

- `rt = <baboon type>` declares the foreign's **runtime/wire mapping** — the Baboon type the value is represented as on the wire. Schema-only backends (GraphQL, OpenAPI, MCP input schemas) render the foreign as that type, and codec machinery delegates through the mapped type's codec where the backend supports it.
- `<lang> = <BaboonTypeRef>` (an unquoted type reference instead of a string) makes that language use the referenced generated Baboon type directly — including its codecs — instead of a hand-wired native type.

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

## Templates (generics)

User-defined templates parameterise a `data`, `adt`, `contract`, or `service` declaration over one or more type parameters. They are instantiated **only** through type aliases, and monomorphised at compile time so every backend emits a concrete type — there is no per-language reified generic in the output. The full specification is in [docs/spec/generics.md](spec/generics.md); the summary below covers the surface syntax.

### Declaration

```baboon
data Page[T] {
  items: lst[T]
  total: u32
}

adt Result[T, E] {
  data Ok  { value: T }
  data Err { error: E }
}

contract Acked[T] {
  value: T
  ack:   bit
}

service Crud[K, V] {
  def get (K): V
  def put (V): K
}
```

Type-parameter names are bare identifiers (single-letter `T` is conventional but not required). Within a template body, a type-param name shadows any same-named top-level type.

### Instantiation (alias-only)

Concrete types are produced by type aliases:

```baboon
data Item { name: str price: f64 }

root type IntPage   = Page[i32]
root type StrPage   = Page[str]
root type ItemPage  = Page[Item]              // user-type argument
root type IntStrEnv = Result[i32, str]
root type IntStrCrud = Crud[i32, str]
```

Locked decision: **the alias's name is the materialised type's identity** — generated code emits `IntPage`, `StrPage`, `ItemPage`, etc. No synthetic `Page<i32>` identifier is exposed. Two aliases with the same arguments still produce two distinct concrete types.

### Codec annotations propagate from the alias

`derived[…]` is written **only on the alias**, not on the template body, and propagates to the materialised type. The annotation follows the alias target:

```baboon
root type IntPage = Page[i32] : derived[json], derived[ueba]
```

Each alias's derivation set is independent; you can derive different codecs for `IntPage` vs `StrPage` even when both instantiate `Page`.

### Cross-namespace instantiation (same package)

A top-level alias can target a template declared in a sibling namespace, and a namespaced alias can target a template in another namespace within the same package:

```baboon
ns foo {
  data X[T] { f: T }
}

root type Y = foo.X[i32]                 // top-level alias → namespaced template

ns bar {
  root type Z = foo.X[str]               // cross-namespace alias
}
```

Cross-**package** instantiation (template declared in a different `.baboon` file) is out of scope.

### Structural arms with template instantiation (`data` / `contract` bodies)

The structural-composition operators `+`, `-`, `^` accept a template instantiation as the arm head inside `data` and `contract` bodies (shipped in M33; full rules in [spec §9](spec/generics.md)):

```baboon
data Page[T]  { items: lst[T] total: u32 }
data Stats[T] { sum: T nObservations: u32 }

root data IntPageWithStats : derived[json], derived[ueba] {
  + Page[i32]            // inline-substituted fields: items: lst[i32], total: u32
  + Stats[i32]
}

root data PageOnly : derived[json], derived[ueba] {
  + Page[i32]
  + Stats[i32]
  ^ Page[i32]            // intersect back down to Page's fields
}
```

The template body is substituted inline — no concrete `Page` type is materialised by a structural arm. Template bodies may themselves use structural arms; substitution recurses. `adt`-body arms (`+`/`-`/`^` on ADT branch sets) do **not** accept template instantiation.

### Forbidden positions (with diagnostics)

The compiler rejects template references in any other non-alias position. Each form below produces a precise diagnostic; see the negative test matrix in [the spec](spec/generics.md#25-forbidden-positions).

- Field-position instantiation (`field: Page[i32]`) — forbidden; instantiate via an alias and reference the alias.
- Nested instantiation in alias args (`type Y = Page[Result[i32, str]]`) — forbidden; introduce intermediate aliases.
- Self-reference in a template body (`data X[T] { rec: X[T] }`, including container-mediated `data Tree[T] { children: lst[Tree[T]] }`) — forbidden; hand-write a non-template recursive type.
- Arity mismatch, duplicate type-parameter name, template referenced without instantiation, instantiating a non-template, and cycles through aliases all produce dedicated diagnostics.

### Evolution

Templates themselves are not subject to migration; only the materialised concrete types (under their alias ids) participate in `BaboonEvolution`. A template body change affects every alias that instantiates it; the diff is observed at the alias-keyed concrete types, exactly as for hand-written types.

### What is NOT supported

Out-of-scope items (see [spec §6](spec/generics.md) for the full list):

- Higher-kinded templates (`X[F[_]]`)
- Variance annotations (`X[+T]`, `X[-T]`)
- Where-clauses / type-class bounds (`X[T : SomeContract]`)
- Defaulted type parameters (`X[T = i32]`)
- Templated `id` declarations
- Templates on **ADT-body** inheritance arms (`+ Foo[T]` between ADT branches; `data`/`contract` structural arms *are* supported, see above)
- Cross-package template instantiation
- Per-language reified generics in emitted source

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

## Doc comments

Baboon preserves doc comments into generated source (M30; full spec in [docs/spec/docstrings.md](spec/docstrings.md)). Two forms exist:

- **Prefix doc** `/** ... */` — binds to the immediately following declaration: any type declaration (`data`, `adt`, `enum`, `contract`, `service`, `foreign`, `type` alias), a service `def`, or a field.
- **Postfix line doc** `//!` — binds to the field defined on the same line.

```baboon
/** A catalogue item. */
data Item {
  /** Display name of the item. */
  name:  str
  price: f64    //! unit price in store currency
}
```

Doc comments are source-level only: they do not affect wire formats, evolution diffs, or schema digests. Every backend re-emits them idiomatically (C# `///`, Scala/Kotlin/Java `/** */`, Python docstrings, Rust `///`, TypeScript/Dart `///`/JSDoc, Swift `///`, GraphQL SDL descriptions, OpenAPI `description` fields), and the LSP surfaces them on hover.

Plain `// ...` and `/* ... */` comments are ignored by the compiler and not preserved.

## Derivations and codegen

Attach derivations on any type to request generated typeclass instances:

```baboon
root data AllBasicTypes: derived[json], derived[ueba] { ... }
```

The compiler currently ships JSON and UEBA codec derivation. Additional derivations can be added with the same `derived[...]` syntax. Baboon will produce C#, Scala, Rust, TypeScript, Python, Kotlin, Java, Dart, and Swift code from the same model (plus schema-only GraphQL SDL and OpenAPI 3.1 outputs) and aggressively deduplicates shared shapes in generated C#.

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

A shorthand signature form is equivalent — `(In): Out` with an optional `!! Err` error type:

```baboon
root service BillingApiShort {
  def CreateInvoice (CreateInvoiceRequest): InvoiceId !! InvoiceError
  def Ping (PingRequest): PingResponse
}
```

Service definitions are scoped like other members and can live inside namespaces (`ns` blocks). Method bodies accept `in`, `out`, and optional `err` markers, or inline DTO/ADT/enum definitions using the same braces/parentheses form. Services can also be [templates](#templates-generics) (`service Crud[K, V] { ... }`).

Per-language flags additionally generate service plumbing: RPC client/server wiring, async method variants (`--cs-async-services`, `--py-async-services`, `--rs-async-services`, `--ts-async-services`, `--jv-async-services`, `--sw-async-services`), and optional [MCP servers](cli-reference.md#mcp-servers) exposing each service's methods as MCP tools (`--<lang>-generate-mcp-server`).

### Configurable service return types

By default each backend wraps service method return types in a language-idiomatic result type:

| Backend | Default return type | Default for `err` absent |
|---------|-------------------|--------------------------|
| Scala | `scala.util.Either[Err, Out]` | `Out` directly |
| Rust | `Result<Out, Err>` | `Out` directly |
| Kotlin | `Either<Err, Out>` | `Out` directly |
| C# | `Out` (no error wrapping) | `Out` directly |
| Python | `Out` (no error wrapping) | `Out` directly |
| TypeScript | `Out` (no error wrapping) | `Out` directly |
| Java | `Out` (no error wrapping) | `Out` directly |
| Dart | `Out` (no error wrapping) | `Out` directly |
| Swift | `Out` (no error wrapping) | `Out` directly |

You can override these defaults using pragmas in `.baboon` files or CLI flags.

#### Pragma keys

All pragma keys follow the pattern `{lang}.service.result.*` where `{lang}` is `scala`, `rust`, `cs`, `python`, `typescript`, `kotlin`, `java`, `dart`, or `swift`.

| Pragma key | Value | Description |
|-----------|-------|-------------|
| `{lang}.service.result.no-errors` | `"true"` / `"false"` | When true, methods return just the output type |
| `{lang}.service.result.type` | e.g. `"Result"` | Wrapper type name (fully qualified if needed) |
| `{lang}.service.result.pattern` | e.g. `"<$success, $error>"` | Type parameter pattern; `$success` and `$error` are expanded |
| `{lang}.service.result.hkt` | `"true"` / `"false"` | Enable higher-kinded type parameter (Scala and Kotlin) |
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
    --service-result-hkt=true \
    --service-result-hkt-name F \
    --service-result-hkt-signature "[+_, +_]" \
    --service-result-pattern "[\$error, \$success]" \
    --output ./output/scala \
  :rust \
    --service-result-type "anyhow::Result" \
    --service-result-pattern "<\$success>" \
    --service-result-no-errors=true \
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

All pragma keys follow the pattern `{lang}.service.context*` where `{lang}` is `scala`, `rust`, `cs`, `python`, `typescript`, `kotlin`, `java`, `dart`, or `swift`.

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

include "shared-addresses.bmo"

root data Order { shipping: ShippingAddress }
```

- Paths are resolved relative to provided model directories (`--model-dir`); includes are resolved recursively.
- Because the header comes from the current file, the included content inherits the same `model` and `version`.
- Give include files an extension other than `.baboon` (conventionally `.bmo`): `--model-dir` picks up every `*.baboon` file as a standalone model, and an include fragment has no `model`/`version` header, so it would fail to parse on its own.

## Rename tracking (`was`)

Renames are normally indistinguishable from remove-plus-add, which breaks automatic conversion derivation. The `was` keyword records the previous name so the evolution engine treats the change as a rename and derives the conversion:

```baboon
// version "2.0.0"; version "1.0.0" had: data Account { login: str }, data OldName { x: i32 }, enum Color { Red Green }

root data Account {
  username: str was login          // field rename
}

root data NewName : was[OldName] {  // type rename (namespace paths allowed: was[outer.OldName])
  x: i32
}

enum Color {
  Red
  Emerald : was[Green]             // enum member rename
}
```

- **Field rename**: `newName: Type was oldName` inside `data`/`id`/`contract` bodies.
- **Type rename**: a `was[OldType]` annotation in the type's `:` annotation list (combinable with `derived[...]`). The old name may include a namespace path.
- **Enum member rename**: `NewMember : was[OldMember]`.

The comparator validates each marker against the previous version (a `was` pointing at a name that never existed is an evolution error) and derives the corresponding conversions instead of emitting manual stubs.

## Evolution workflow

Versioned files can be diffed by Baboon to emit migration code. When a change is obviously compatible (e.g., adding `data BankTransfer` to an ADT in the example above), Baboon derives conversions. Breaking changes (e.g., removing required fields) produce explicit stubs so you fail fast and implement the conversion manually. Renames stay derivable when annotated with [`was`](#rename-tracking-was).

## Code generation targets

- **Scala** — classes, Circe JSON codecs, UEBA binary codecs, and evolution converters.
- **C#** — classes with aggressive deduplication, Newtonsoft.Json codecs, UEBA binary codecs, and evolution converters.
- **Rust** — native structs/enums with serde derive, custom UEBA binary codecs, and evolution converters.
- **Python** — dataclasses with custom JSON codecs.
- **TypeScript** — classes with function-based JSON and UEBA codecs, and evolution converters.
- **Kotlin** — data classes with Jackson JSON codecs, UEBA binary codecs, and evolution converters (JVM or Multiplatform via `--kt-multiplatform`).
- **Java** — records/classes with Jackson JSON codecs, UEBA binary codecs, and evolution converters.
- **Dart** — classes with dart:convert JSON codecs, UEBA binary codecs, and evolution converters.
- **Swift** — structs/enums with JSONSerialization JSON codecs, UEBA binary codecs, and evolution converters.
- **GraphQL** — SDL schema files (type definitions only, no codecs).
- **OpenAPI** — OpenAPI 3.1 component schemas (no codecs).

The full list of global and per-target compiler options lives in [docs/cli-reference.md](cli-reference.md).

Invoke `mdl :build :mkdist` to generate and package all targets through the existing mudyla pipelines.
