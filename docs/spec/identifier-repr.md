# Identifier Repr Wire Spec (M18)

**Status:** authoritative specification for `id` toString / parseRepr across all 9
target backends. The Scala backend ships as the reference implementation in
PR-56; the remaining 8 backends (PR-57) MUST conform to the format, escape
table, and state-machine described here.

This document is the single source of truth. When the Scala emitter and this
document disagree, the document wins and the emitter is wrong.

---

## 1. Overview

### 1.1 What `id` types are

An `id` declaration in `.baboon` source produces a typed-AST node structurally
identical to a `data` Dto, distinguished by the `Typedef.Dto.isIdentifier ==
true` flag. Identifier fields are restricted (validator PR-55) to:

- builtin primitive scalars except floats: `bit`, `i08`, `i16`, `i32`, `i64`,
  `u08`, `u16`, `u32`, `u64`, `str`, `uid`, `tsu`, `tso`, `bytes`;
- nested `id` types (other Dtos with `isIdentifier == true`);
- type aliases that resolve transitively to one of the above.

Floats (`f32`, `f64`, `f128`), collections (`lst`, `set`, `map`, `opt`),
non-id user types, and `any` variants are rejected at validator time.

### 1.2 Why a parseable repr exists

Two motivations:

1. **User-facing rendering.** Identifiers benefit from a stable, human-legible
   string form for log lines, debugger output, error messages, and
   IDE-autocomplete tooltips. The toString form is the canonical user-facing
   surface.
2. **Multi-field map-key support (M19).** When a multi-field `id` type is used
   as a JSON map key, the key codec serializes the value via the toString form
   and decodes it via `parseRepr`. The format MUST be parseable for this path
   to work.

### 1.3 Caveats — Q-FU-4 / Q-M18-5

- The toString form is **NOT intended for versioning workflows**. It does not
  participate in schema evolution, conversions, or the `BaboonEvolution`
  pipeline. Two domain versions that differ only in identifier field order
  produce different repr strings; consumers who pin on the textual form must
  treat field reordering as a breaking change.
- The parser (`parseRepr`) is **NOT exposed as a primary public API on the
  identifier type itself**. It lives on a separate codec / internal namespace
  (e.g. `<TypeName>Codec.parseRepr` in Scala/Kotlin/Java/C#/Dart/Swift,
  `<typeNameCodec>.parseRepr` in TypeScript, `<TypeName>Codec.parse_repr` in
  Python, a non-`FromStr` module-private function in Rust). Goal: reachable
  when needed (map-key codec emission, deliberate user code) but not the
  obvious "how do I parse my id from a string" answer in IDE autocomplete on
  the type.
- The toString format is **NOT the JSON or UEBA wire format**. Identifiers
  serialize byte-identically to a `data` of the same shape on both wires
  (verified independently in PR-58). The JSON wire form for an id used as a
  value (not a map key) is the same nested object that `data` would produce.

---

## 2. Format grammar

### 2.1 Canonical EBNF

```
identifier_repr      ::= simple_name ":" version "#" fields
simple_name          ::= /* the type's simple name, e.g. "UserId" — see §2.2 */
version              ::= digits "." digits "." digits   /* MAJOR.MINOR.PATCH */
fields               ::= /* empty if the id has zero fields */
                       | field_pair ( ":" field_pair )*
field_pair           ::= field_name ":" field_value
field_name           ::= /* the field's source name, e.g. "x" */
field_value          ::= primitive_value | nested_identifier_value
nested_identifier_value ::= "{" identifier_repr "}"
primitive_value      ::= /* per type, see §3 */
```

### 2.2 `simple_name` — Q-M18-4

The simple type name only. **Pkg/owner are NOT included.** For `pkg foo.bar`
declaring `id UserId { ... }`, the repr starts `UserId:1.0.0#…`, never
`foo.bar.UserId:…` and never `foo/bar#UserId:…`.

Rationale: identifiers are typed at usage sites; embedding pkg/owner in the
repr would couple the textual form to internal namespacing the user did not
ask for.

### 2.3 `version` — Q-M18-5

Three-segment `MAJOR.MINOR.PATCH` from `Domain.version`. Always emitted; no
unversioned variant ships in M18. The version is rendered exactly as
`Version.toString` produces it (no zero-padding, no `v` prefix, no `+build`
suffix even if the underlying izumi `Version` carries one).

The grammar is locked to:

```
version ::= digits "." digits "." digits
digits  ::= [0-9]+
```

Parsers MUST require exactly three integer segments separated by `.`. No
`v` prefix, no `+build` suffix, no shortened (`1.0` / `1`) forms, no
leading-zero requirements (parser MAY accept `01` as `1`, but emitter MUST
NOT emit leading zeros).

### 2.4 Field ordering

Fields appear in **declared order in the source `.baboon` file**, preserved
in `Typedef.Dto.fields: List[Field]`. Reordering fields is a wire-format
breaking change for repr consumers (and is independently caught as a
non-trivial schema diff by the existing comparator).

### 2.5 Empty-field id

An identifier with zero declared fields renders as `<Name>:<version>#` with
nothing after the `#`. This is unusual but legal — the validator does not
require at least one field.

---

## 3. Per-type field rendering

| baboon type            | rendering                                                                   | escape needed |
|------------------------|-----------------------------------------------------------------------------|---------------|
| `bit`                  | `true` or `false` — lowercase ASCII, exact                                  | no            |
| `i08`,`i16`,`i32`,`i64`| signed decimal; `i64.MIN_VALUE` is `-9223372036854775808`                   | no            |
| `u08`,`u16`,`u32`,`u64`| unsigned decimal; `u64.MAX_VALUE` is `18446744073709551615`                 | no            |
| `str`                  | UTF-8 literal with backslash escapes for `\ # : { }` (see §4)               | YES           |
| `uid`                  | RFC 4122 canonical, lowercase, hyphenated: `de7b9e1e-5c93-45fe-beec-da99994f629a` | no       |
| `tsu`                  | RFC 3339 UTC with `Z`, milliseconds, exactly 24 chars: `2026-04-29T12:34:56.789Z` | no       |
| `tso`                  | RFC 3339 with `±HH:MM` offset, milliseconds, exactly 29 chars: `2026-04-29T12:34:56.789+02:00` | no |
| `bytes`                | lowercase hex, no separators, no `0x` prefix; empty `bytes` is empty string | no            |

Implementation precision notes:

- **Timestamps** (`tsu`, `tso`): use 3-digit millisecond fractional precision
  (same precision class as the JSON wire helper) but the formatters DIFFER.
  `tsu` uses pattern `yyyy-MM-dd'T'HH:mm:ss.SSS'Z'` (literal `Z`, exactly
  24 chars). `tso` uses pattern `yyyy-MM-dd'T'HH:mm:ss.SSSxxx` (lowercase
  `xxx`, exactly 29 chars, never `Z` shorthand). Do NOT reuse
  `BaboonTimeFormats` for repr — it accepts/emits different forms (`XXX`
  which collapses UTC to `Z`).
- **`uid`**: parse via the language's standard UUID parser; emit via the
  language's standard lowercase-hyphenated form. Java/Scala's
  `UUID.toString` and equivalents produce this form natively.
- **`bytes`**: lowercase hex via the canonical formatter (Scala
  `BaboonByteString` has a `toHexString` that emits uppercase — for repr we
  emit lowercase via `toLowerCase` or a dedicated helper). All backends MUST
  emit lowercase hex.

The five `i*` and `u*` types are rendered with the language's natural
`toString` for the underlying integer type with one caveat: `u32` and `u64`
must render as unsigned decimal even on languages where they map to a
signed type (Scala/Java/C# `long` for `u64`). Scala uses
`java.lang.Long.toUnsignedString` for `u64`.

`bit` MUST be exactly `true` / `false` lowercase, not the language-default
case (e.g. C# `True`/`False` is wrong; render via `value ? "true" : "false"`).

---

## 4. Escaping rule (str only)

### 4.1 The five metacharacters

The repr alphabet uses five structural metacharacters:

```
\   (0x5C) — escape prefix
#   (0x23) — version-from-fields separator
:   (0x3A) — name-from-version separator AND field-pair internal separator AND field-pair joiner
{   (0x7B) — open nested identifier
}   (0x7D) — close nested identifier
```

`#`, `:`, `{`, `}` partition the structure. `\` is the 5th metachar because
it must escape itself. Five is the minimum; any value not in this set never
collides with structure and passes through verbatim.

### 4.2 Escape table

Within a `str` field value, each metacharacter is escaped by prefixing with
`\`. ALL other characters pass through verbatim, including spaces, tabs,
newlines, and arbitrary multi-byte UTF-8 sequences. The repr is NOT
line-oriented and is NOT delimited by quotes.

| Source character | Escaped form |
|------------------|--------------|
| `\`              | `\\`         |
| `#`              | `\#`         |
| `:`              | `\:`         |
| `{`              | `\{`         |
| `}`              | `\}`         |
| any other char   | itself       |

### 4.3 Why backslash escapes (rejected alternatives)

- **Entity codes `&32;`** — forces escaping `&` and `;` too (6 metachars
  not 5); ambiguous between decimal/hex codepoint forms.
- **Percent-encoding (RFC 3986 subset)** — 5 metachars + `%` = 6;
  imports URI-syntax baggage (case-sensitivity of hex, `+`-for-space
  conventions etc.).
- **JSON-string-style with surrounding quotes** — breaks the
  "parseable without explicit string delimiters" property; introduces a
  bracketing mechanic for primitives that's heavier than the metachar
  problem it solves.
- **Pascal/C-style with conditional quoting** — asymmetric writer/reader
  logic.

### 4.4 No bytes-escape

`bytes` fields render as lowercase hex which never contains a metacharacter,
so no escape is needed and decoders MUST NOT apply unescape to bytes-typed
fields.

---

## 5. Parsing — schema-directed state machine

### 5.1 Why schema-directed

The repr is **NOT self-delimiting in field positions** beyond the metachar
boundaries. The structure
`Foo:1.0.0#a:1:b:hello:c:{Bar:1.0.0#x:42}` parses unambiguously only because
the schema tells the parser that field `a` is `i32`, `b` is `str`, `c` is a
nested id of type `Bar` whose first field is `x:i32`. Without the schema,
the parser cannot tell where field N ends and field N+1 begins by looking
at colons alone.

This is the key invariant: parsing walks the schema (declared field types
and order) while consuming token pairs.

### 5.2 States

```
IN_NAME            — accumulating simple_name characters
IN_VERSION         — accumulating version characters until '#'
IN_FIELD_NAME      — expect field_name terminated by ':'
IN_FIELD_VALUE     — expect a primitive or nested-id; dispatch on schema
IN_NESTED          — recursive parse of nested identifier between '{' and '}'
```

### 5.3 Top-level transitions

```
IN_NAME       --':'-->  IN_VERSION
IN_VERSION    --'#'-->  IN_FIELD_NAME       (if any fields remain in schema)
IN_VERSION    --'#'-->  DONE                (if zero fields in schema)
IN_FIELD_NAME --':'-->  IN_FIELD_VALUE
IN_FIELD_VALUE --consume per type-->  ':'   (if more fields)
                                       DONE  (if last field)
```

### 5.4 Field-value consumption

Per the schema's type for the field at position N:

- **`bit`**: consume the literal characters `true` or `false`. Reject any
  other prefix.
- **`i*`/`u*`**: consume `[+-]?[0-9]+` (signed for `i*`, unsigned for `u*`).
  Stop at the first non-digit, which MUST be `:` (more fields) or end of
  input (last field) or `}` (inside a nested context — see §5.6).
- **`str`**: consume characters with backslash-handling per §4.2. A bare
  metachar `:` `#` `{` `}` is a structural boundary. A bare `\` followed by
  any of `\`, `#`, `:`, `{`, `}` produces the literal char and consumes
  both. A bare `\` followed by anything else is a parse error.
- **`uid`**: consume exactly 36 characters matching
  `[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}`.
- **`tsu`**: consume **exactly 24 characters** matching
  `yyyy-MM-ddTHH:mm:ss.SSSZ`. The lexeme contains literal `:` characters
  inside `HH:mm:ss`; these are NOT structural metachars while the parser
  is in the `tsu`-consume state. The fixed length is unambiguous because
  spec §3 mandates 3-digit milliseconds + UTC `Z` suffix.
- **`tso`**: consume **exactly 29 characters** matching
  `yyyy-MM-ddTHH:mm:ss.SSS±HH:MM`. Same fixed-width rationale — milliseconds
  are 3 digits, offset is `±HH:MM`. The four `:` characters inside the
  lexeme are NOT structural while the parser is in `tso`-consume state.

**Implementations MUST use exact 24-char (`tsu`) / 29-char (`tso`) consumption,
NOT metachar-delimited consumption.** The lexeme contains literal `:`
characters that would otherwise be misread as field-pair separators; only
fixed-width consumption is correct.
- **`bytes`**: consume `[0-9a-f]*` (even-length); empty is legal. Stop at
  the first non-hex character, which MUST be `:` or end of input or `}`.
- **nested `id`**: expect `{`, recurse via the nested type's `parseRepr`
  on the substring delimited by the matching `}` (brace-balanced — but the
  inner repr only contains balanced braces if and only if it itself has
  nested ids of further depth, which is again schema-directed).

### 5.5 Backslash handling

When in `IN_FIELD_VALUE` for a `str` field, `\X` always means literal X
(where X ∈ `\#:{}`). Bare metachar X is a structural boundary. A `\`
followed by any character outside `\#:{}` is a parse error.

When in `IN_FIELD_VALUE` for any non-`str` field, `\` is never consumed —
its appearance is a parse error.

### 5.6 Nested-id brace handling

On entering a nested-id field, the parser consumes `{`, then recursively
applies the parsing algorithm to the nested type's schema. The nested
parse stops when it has consumed all fields per the nested schema. The next
character MUST be `}`. The parser then resumes the outer state.

Brace counting is a sanity check, not the primary boundary detector — the
schema-directed approach already determines where the nested id ends. A
naive implementation can still bracket-match braces (incrementing on `\{`
inside `str` is not necessary because escaped `\{` is consumed as a
character, never as a structural brace).

### 5.7 Round-trip property

For any well-formed value `v` of identifier type `I` with version `V`:

```
parseRepr_I(toString_I(v)) ≡ v
```

where `≡` is structural equality of all fields. Verified by property test
in PR-56 (Scala) and by per-language property tests in PR-57.

---

## 6. Edge cases (canonical examples)

The following examples are the conformance suite. Every backend MUST
reproduce them byte-for-byte.

Assume `id Foo { ... }` declared in domain `1.0.0` unless otherwise stated.

### 6.1 Empty `str` field

```
id Foo { name: str }
value: name = ""
repr:  Foo:1.0.0#name:
```

The repr ends with a bare colon followed by nothing — this is the empty-string
field value, parsed as the consumption of zero characters before end of
input.

### 6.2 `str` containing each metacharacter

```
id Foo { name: str }
value: name = "\#:{}"  (5 chars: backslash, hash, colon, open brace, close brace)
repr:  Foo:1.0.0#name:\\\#\:\{\}
```

Each of the 5 metacharacters is preceded by a single backslash.

### 6.3 `str` ending in `\`

```
id Foo { name: str }
value: name = "foo\"  (4 chars: f, o, o, backslash)
repr:  Foo:1.0.0#name:foo\\
```

The trailing backslash is escaped; total escaped tail is `\\`.

### 6.4 `str` of only backslashes

```
id Foo { name: str }
value: name = "\\\\"  (4 backslashes)
repr:  Foo:1.0.0#name:\\\\\\\\
```

Four source backslashes → eight escaped chars (`\\` for each).

### 6.5 `bytes` empty

```
id Foo { b: bytes }
value: b = empty
repr:  Foo:1.0.0#b:
```

Identical structurally to the empty-str case at the wire level — distinguished
only by the schema-directed parser (the field type is `bytes`, so the parser
runs the bytes-consumption rule).

### 6.6 `bytes` with high bytes

```
id Foo { b: bytes }
value: b = [0xff, 0xfe, 0x00]
repr:  Foo:1.0.0#b:fffe00
```

Lowercase hex, no separators, no prefix.

### 6.7 `i64.MIN_VALUE`

```
id Foo { x: i64 }
value: x = -9223372036854775808 (Long.MinValue)
repr:  Foo:1.0.0#x:-9223372036854775808
```

Asymmetric two's-complement boundary; rendered as plain signed decimal.

### 6.8 `u64.MAX_VALUE`

```
id Foo { x: u64 }
value: x = 0xFFFFFFFFFFFFFFFF (rendered as unsigned)
repr:  Foo:1.0.0#x:18446744073709551615
```

Languages that store `u64` as signed `Long` MUST render via the unsigned
formatter (Scala: `java.lang.Long.toUnsignedString`).

### 6.9 Multi-field flat identifier

```
id Point { x: i32; y: i32; label: str }
value: x = 1, y = -2, label = "hello"
repr:  Point:1.0.0#x:1:y:-2:label:hello
```

Note the field-pair joiner `:` is the same character as the field-name /
field-value separator. Disambiguation is schema-directed.

### 6.10 Deep nested identifier (4 levels)

```
id A { b: B }
id B { c: C }
id C { d: D }
id D { x: i32 }
value: A.b = B(C(D(42)))
repr:  A:1.0.0#b:{B:1.0.0#c:{C:1.0.0#d:{D:1.0.0#x:42}}}
```

Each nesting level adds one `{...}` wrapper.

### 6.11 Mixed field types

```
id User { id: uid; created: tsu; flags: bytes; active: bit }
value: id = de7b9e1e-5c93-45fe-beec-da99994f629a,
       created = 2026-04-29T12:34:56.789Z,
       flags = [0x01, 0x02],
       active = true
repr:  User:1.0.0#id:de7b9e1e-5c93-45fe-beec-da99994f629a:created:2026-04-29T12:34:56.789Z:flags:0102:active:true
```

### 6.12 Empty-field id

```
id Marker { }
value: (no fields)
repr:  Marker:1.0.0#
```

---

## 7. Out of scope

The following are explicitly NOT covered by this spec and MUST NOT be
inferred from it:

- **Versioning workflows.** This format is not used for cross-version
  migration. Two `id` types of the same name in different versions produce
  different reprs and the parser of one version will reject (with no
  fallback) the repr of another.
- **Primary public API.** Per Q-FU-4, `parseRepr` lives on a
  codec/internal-style namespace, not as the obvious `MyId.parse(s)` on the
  type. The toString form IS the canonical user-facing surface; the parser
  is reachable but not advertised.
- **JSON / UEBA wire format.** Identifiers serialize byte-identically to a
  `data` of the same shape on both wires. The repr format is independent
  and additional. The repr is used as the JSON map-key string when an `id`
  appears in map-key position (M19), but never as the JSON map-value form.
- **Cross-version conversion.** `id ↔ data` shape conversions are handled
  by the existing `BaboonRules` / `BaboonComparator` paths (PR-58 verifies);
  the repr does not participate.
- **GraphQL / OpenAPI scalar form.** `id` types render as object types in
  schema-only emitters (Q-M18-7); the toString form is not advertised as a
  scalar pattern in M18.

---

## 8. Reference implementation

Scala (PR-56) is the reference implementation. Key files:

- **Runtime helpers:** `baboon-compiler/src/main/resources/baboon-runtime/scala/BaboonRuntimeShared.scala`
  — `IdentifierRepr` object (escape/unescape, hex/dehex, render/parse for
  primitives, `Cursor` for parser state).
- **toString emission:** `baboon-compiler/src/main/scala/io/septimalmind/baboon/translator/scl/ScDefnTranslator.scala`
  — `makeRepr` branches on `dto.isIdentifier` and appends the override.
- **`<TypeName>Codec.parseRepr` emission:** same `ScDefnTranslator` —
  emits a sibling `<TypeName>Codec` object next to the case class /
  companion.
- **Property test:** `baboon-compiler/.jvm/src/test/scala/io/septimalmind/baboon/tests/IdentifierReprPropertyTest.scala`.

When implementing PR-57 for a non-Scala backend, START from this document.
The Scala code is a faithful realization of this document, but the document
is the contract.
