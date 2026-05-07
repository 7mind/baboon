# Codec envelope — top-level `BaboonTypeMeta` wire format

This document specifies the canonical wire format of the top-level
`BaboonTypeMeta` envelope: the metadata block that precedes every encoded
value when the encoder writes through the high-level facade
(`BaboonCodecsFacade.encodeTo{Bin,Json}`). Every backend's runtime emits and
parses identically; cross-language interop relies on byte-for-byte agreement
on the layout below.

This is **distinct** from the field-level `AnyMeta` envelope used inside
`any`-typed fields, which is documented in
[`docs/ueba-format.md` § "Any fields"](../ueba-format.md#any-fields) and
[`docs/json-codecs.md` § "Any fields"](../json-codecs.md#any-fields).

## 1. What's in the envelope

The envelope identifies a payload by **(domainIdentifier, domainVersion,
typeIdentifier)** and carries an optional **domainVersionMinCompat**
("unchanged-since version") that lets readers from older domain versions
short-circuit decode when the type hasn't structurally changed since their
own version.

Field summary:

| Field                    | Type    | Purpose                                              |
|--------------------------|---------|------------------------------------------------------|
| `metaVersion`            | u8      | Format version of the envelope itself. See § 3.      |
| `domainIdentifier`       | string  | Dotted namespace of the domain, e.g. `"my.ok"`.       |
| `domainVersion`          | string  | Semver of the encoded value's domain, e.g. `"1.0.0"`.|
| `domainVersionMinCompat` | string  | Oldest domain version that decodes the same payload structurally. Equals `domainVersion` when the type is fresh in this version. |
| `typeIdentifier`         | string  | Type id within the domain, e.g. `"my.ok/:#Holder"`.  |

`domainVersionMinCompat` may be elided in either encoding when it equals
`domainVersion` (the common case for newly-introduced types). Readers must
default a missing `domainVersionMinCompat` to `domainVersion`.

## 2. Wire formats

### 2.1 Binary (UEBA)

```
+--------+-------------------+---------------+----------+--------------------+----------------+
| 1 byte | length-prefixed   | length-       | 1 byte   | length-prefixed    | length-        |
| u8     | UTF-8 string      | prefixed UTF-8| u8 flag  | UTF-8 string       | prefixed UTF-8 |
| metaVer| domainIdentifier  | domainVersion | hasMinC. | (only if flag==1)  | typeIdentifier |
+--------+-------------------+---------------+----------+--------------------+----------------+
```

Length-prefixed strings use the UEBA string encoding (ULEB128 byte length
followed by UTF-8 bytes — see `docs/ueba-format.md` § "Strings").

The `hasMinCompat` flag:
- `0x00` — `domainVersionMinCompat` is omitted from the wire and equals
  `domainVersion` after decode.
- `0x01` — `domainVersionMinCompat` follows as a length-prefixed string.

Other flag values are illegal; readers reject them.

### 2.2 JSON

The envelope is a JSON object with the keys below. A reader does not assume
key order. The encoder emits them in declaration order for determinism:

```jsonc
{
  "$mv": 1,                  // metaVersion as a JSON number (see § 4)
  "$d":  "my.ok",            // domainIdentifier
  "$v":  "1.0.0",            // domainVersion
  "$t":  "my.ok/:#Holder",   // typeIdentifier
  "$uv": "1.0.0",            // domainVersionMinCompat — OMITTED when equal to $v
  "$c":  { ... }             // content (the encoded value's JSON form)
}
```

The `$c` key carries the actual encoded payload. The other five keys are the
envelope. Implementations distinguish envelope keys (`$d`/`$v`/`$t`/`$uv`/
`$mv`) from payload by name; the dollar-prefix is reserved for the envelope
and the field-level `AnyMeta` (`$ak`/`$ad`/`$av`/`$at`/`$av`-content), and
must not be used by user-defined field names.

## 3. `metaVersion` byte allocation

The `metaVersion` byte is a single-byte (0..255) format-version selector for
the top-level envelope. Readers MUST reject envelopes whose `metaVersion`
they don't recognise (return null/None/Right-of-error per the language
idiom; never throw across the version boundary).

| Byte    | Status      | Meaning                                                                     |
|---------|-------------|-----------------------------------------------------------------------------|
| 0       | reserved    | Sentinel; never emitted. Reserved to remain available for future use.       |
| 1       | **active**  | Full meta — all four fields described in § 1, layout per § 2.               |
| 2..15   | reserved    | Free for future allocation if a new top-level layout is introduced.         |
| 16      | retired     | Briefly used during M32 development. Never released; reader rejection-only. |
| 17..255 | reserved    | Free for future allocation.                                                 |

Future top-level layouts that omit fields (e.g. a "kind-prefixed" envelope
that skips `domainIdentifier` when the consumer can supply it from context)
get a fresh allocation in the `2..15` range. Byte 16 is permanently retired
to avoid confusion with the prior M32 prototype.

Readers MUST NOT attempt to interpret an unknown `metaVersion` payload.
Forward-compat is opt-in: the application chooses how to handle unknown
envelopes (skip, fall back to a side-channel registry, or surface as a
typed error to the caller).

## 4. JSON `$mv` value type

Per
[`proposal.md` § 10.6 (a)](../../proposal.md):

- **Writers MUST emit `$mv` as a JSON number**, not a string. The encoder
  always produces `"$mv": 1` (numeric), never `"$mv": "1"`.
- **Readers MUST accept both forms**:
  - JSON-number form (canonical, written by current writers).
  - JSON-string form (back-compat with M28-vintage fixtures that produced
    `"$mv": "1"`).
- Any other JSON type — boolean, fractional number, array, object, null —
  MUST be rejected.

This asymmetry — writers narrow, readers wide — lets this repo evolve away
from the legacy string form without breaking consumers reading historical
fixtures. Once all known producers are at MFACADE-PR-3 (this repo) or later,
the string form can be retired by tightening the reader.

Edge cases readers MUST reject:
- `"$mv": 1.5`               (fractional)
- `"$mv": -1` / `"$mv": 256` (out of byte range)
- `"$mv": true`              (boolean — note that in some languages
                              `true.toInt() == 1`; code must reject before
                              numeric coercion)
- `"$mv": []` / `"$mv": {}`  (non-scalar)
- `"$mv": "  1  "`           (whitespace-padded numeric string — strict
                              parse only; current Python uses
                              `re.fullmatch(r'-?[0-9]+', s)`)

The reader edge-case matrix is exercised by per-backend stub tests
(`AnyMetaCodecTests`/`BaboonTypeMetaCodecSpec`/`test_baboon_type_meta_codec.py`)
across cs/sc/py/rs/ts/jv/dt/sw. See MFACADE-PR-3-D04..D06 in
`defects.md` for history.

## 5. Reader contract

`BaboonTypeMetaCodec.readMeta(reader|jsonValue)` returns the envelope
parsed from the bin reader or JSON value:

- **Success** — a fully-populated `BaboonTypeMeta` value with all four
  fields present (`domainVersionMinCompat` defaulted to `domainVersion`
  when elided).
- **Recoverable failure** (return null/None/Right-of-error per language
  idiom): unrecognised `metaVersion`, malformed envelope (missing
  required fields, wrong types, bin-side ULEB128 truncation).
- **Unrecoverable failure** (throw / Left): bin reader I/O error,
  upstream JSON parse failure when the input was supposed to be a
  pre-parsed object.

The bin path's behaviour on unknown `metaVersion` is documented as a
hard `Left(DecoderFailure)` per
[`proposal.md` § 4.2](../../proposal.md): the caller is unambiguously
told the wire is unreadable. JSON path returns null/None and lets the
caller fall through.

## 6. Encoder contract

`BaboonTypeMetaCodec.writeBin(meta, writer)` and `writeJson(meta)` always
emit a `metaVersion=1` envelope per § 2. Writers do not have a fallback
to byte 16 or any other allocation; new versions get explicit codepaths
when introduced.

The encoder always writes the four required fields (`metaVersion`,
`domainIdentifier`, `domainVersion`, `typeIdentifier`); it elides
`domainVersionMinCompat` only when it equals `domainVersion` (the
elision is byte-canonical, not stylistic — readers MUST handle both
forms).

## 7. Implementation pointers

Per-backend runtime sources (where the envelope codec lives):

- C#: `baboon-compiler/src/main/resources/baboon-runtime/cs/BaboonTypeMeta.cs`
- Scala: `baboon-compiler/src/main/resources/baboon-runtime/scala/BaboonRuntimeShared.scala`
- Rust: `baboon-compiler/src/main/resources/baboon-runtime/rust/baboon_codecs_facade.rs` (`mod baboon_type_meta_codec`)
- Java: `baboon-compiler/src/main/resources/baboon-runtime/java/BaboonTypeMeta.java`
- Kotlin: `baboon-compiler/src/main/resources/baboon-runtime/kotlin/BaboonRuntimeShared.kt`
- Kotlin-KMP: `baboon-compiler/src/main/resources/baboon-runtime/kotlin-kmp/BaboonRuntimeShared.kt`
- TypeScript: `baboon-compiler/src/main/resources/baboon-runtime/typescript/BaboonSharedRuntime.ts`
- Python: `baboon-compiler/src/main/resources/baboon-runtime/python/baboon_runtime_shared.py`
- Dart: `baboon-compiler/src/main/resources/baboon-runtime/dart/baboon_runtime.dart`
- Swift: `baboon-compiler/src/main/resources/baboon-runtime/swift/baboon_runtime.swift`

All ten implementations share the field set and layout in § 1–§ 2;
divergences are interop bugs.

## 8. Versioning policy

Changes to this spec follow the policy below:

- **Wire-format changes** (anything observable to a peer parser) require a
  new `metaVersion` byte allocation per § 3. Adding a field, removing a
  field, reordering fields, or changing a string-encoding rule are all
  wire-format changes.
- **Reader-tolerance changes** (e.g. accepting a new edge case as
  recoverable rather than throwing) MAY ship without a new byte if the
  wire bytes a current writer produces are still parseable by both old
  and new readers.
- **Encoder narrowing** (a writer producing fewer forms than before) MAY
  ship without a new byte: peers reading the older form continue to work
  by virtue of writers producing a strict subset of the original valid
  forms.

The `$mv` value-type change in MFACADE-PR-3 is the canonical example of
the third category: writers narrowed from "string" to "number"; readers
widened to accept both. No `metaVersion` bump was required because every
historical wire byte remained parseable.
