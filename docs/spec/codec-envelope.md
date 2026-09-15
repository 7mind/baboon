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
| `metaVersion`            | u8      | Format version of the envelope itself: `1` (single-bound binary layout, the default; always `1` in JSON) or `2` (binary layout carrying both bounds). See § 3. |
| `domainIdentifier`       | string  | Dotted namespace of the domain, e.g. `"my.ok"`.       |
| `domainVersion`          | string  | Semver of the encoded value's domain, e.g. `"1.0.0"`.|
| `domainVersionMinCompat` | string  | Oldest domain version whose codec decodes this payload. Under the writer's default `ForwardWritePolicy.Strict` this is the byte-identical bound (equals `domainVersion` when the type's layout is fresh in this version). Under `Tolerant` — binary only — it is the prefix-read bound for the payload's index mode; see § 2.1.2. |
| `domainVersionReadableMin` | string | **JSON and binary v2.** Oldest domain version whose codec can decode the payload under the format's forward-read contract: `json-additive` for JSON (tolerant key lookup; unknown fields dropped), the prefix tier of the payload's index mode for binary v2 (`prefix-compact` / `prefix-any-mode`; trailing appended fields unread). Always `<= domainVersionMinCompat`. Absent from binary v1. See `docs/forward-compat.md`. |
| `typeIdentifier`         | string  | Type id within the domain, e.g. `"my.ok/:#Holder"`.  |

`domainVersionMinCompat` may be elided in either encoding when it equals
`domainVersion` (the common case for newly-introduced types). Readers must
default a missing `domainVersionMinCompat` to `domainVersion`.

`domainVersionReadableMin` is carried by the JSON envelope (`$rv`) and by the
binary v2 envelope (§ 2.1.3), elided when it equals the effective
`domainVersionMinCompat`; readers default it to `domainVersionMinCompat`.
Invariant: `readableMin <= minCompat <= domainVersion`. The binary v1 envelope
has no slot for it: the only bound a v1 reader sees is
`domainVersionMinCompat`, whose meaning is chosen by the WRITER
(`ForwardWritePolicy`, § 2.1.2). Whether a reader honours `readableMin` is a
reader-side policy (`ForwardReadPolicy`: `Tolerant`, the default, decodes
newer payloads with the reader's newest codec whenever `readableMin` reaches a
registered version; `Lossless` ignores it and requires `minCompat` to). It
applies to JSON and to binary v2 alike; a v1 envelope carries no separate
`readableMin`, so the policy has no effect on it. Re-encoding intermediaries
must use `Lossless`.

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

#### 2.1.1 Conformance reference — canonical bytes for `Inner(x=42)` v1.0.0

Pinned in C# at `test/cs-stub/BaboonTests/DomainFacadeTests.cs` —
`EncodeToBin_Inner42_Compact_ProducesCanonicalBytes`. Cross-backend agreement
is enforced by the cross-language acceptance harness
(`mdl :test-acceptance`); a backend that drifts from this byte sequence will
fail the harness against the unmodified C# (and other) peers.

```
01                                              META_VERSION_1
05 6D 79 2E 6F 6B                               len(5) + "my.ok"
05 31 2E 30 2E 30                               len(5) + "1.0.0"
00                                              hasMinCompat = 0
0D 6D 79 2E 6F 6B 2F 3A 23 49 6E 6E 65 72       len(13) + "my.ok/:#Inner"
00                                              Inner UEBA mode-byte (0 = compact)
2A 00 00 00                                     i32 little-endian = 42
```

Total: 33 bytes.

#### 2.1.2 What `domainVersionMinCompat` promises — the writer's `ForwardWritePolicy`

The layout above is fixed; only the *value* written into the bound slot is
policy-dependent. The policy lives on `BaboonCodecContext` (next to the index
mode, because the bound depends on it):

- `Strict` (default): the byte-identical bound, `baboonSameInVersions.head`.
  Every codec at or above it produces and consumes exactly these bytes.
  Identical to all envelopes written before the policy existed.
- `Tolerant`: the prefix-read bound from `baboonMinReaderVersions` for the
  payload's index mode — `prefix-compact` for compact contexts,
  `prefix-any-mode` for indexed ones (`docs/forward-compat.md`, "Tiers").
  Codecs at or above it read the payload as a prefix: the fields appended by
  later versions are left unread. Equal to the Strict bound whenever no
  prefix relationship exists, so such envelopes are byte-identical to Strict
  ones.

Readers cannot tell which policy produced an envelope. They trust the bound:
when `domainVersion` is newer than every registered version and the bound is
at or below the newest registered version, they decode with that newest
codec (forward-readability is monotone along the version chain, so this is
the highest-fidelity correct choice); otherwise the payload is unreadable.
Consequences the deploying organisation owns:

- The prefix contract (`docs/forward-compat.md`, "The prefix-* client
  contract") is satisfied structurally by the byte-array decode entry points,
  because the payload is the last element of the envelope and readers do not
  assert full consumption. Callers of the stream-based entry points must not
  continue reading the stream after a forward decode.
- A `Tolerant` envelope decoded by an older reader is silently truncated to
  the reader's field set. `ForwardReadPolicy.Lossless` cannot detect this for
  binary; re-encoding intermediaries must run at the writer's version or
  newer.

#### 2.1.3 Binary v2 — both bounds

Selected by the writer's `BaboonCodecContext.envelopeVersion = V2` (default
`V1`). The layout is the JSON envelope's field set in binary form:

```
+--------+-------------------+---------------+----------+--------------------+--------------------+----------------+
| 1 byte | length-prefixed   | length-       | 1 byte   | length-prefixed    | length-prefixed    | length-        |
| u8 = 2 | UTF-8 string      | prefixed UTF-8| flags    | UTF-8 string       | UTF-8 string       | prefixed UTF-8 |
| metaVer| domainIdentifier  | domainVersion |          | (only if bit 0)    | (only if bit 1)    | typeIdentifier |
|        |                   |               |          | minCompat          | readableMin        |                |
+--------+-------------------+---------------+----------+--------------------+--------------------+----------------+
```

The `flags` byte:
- bit 0 (`0x01`) — `domainVersionMinCompat` follows. Elided when it equals
  `domainVersion`.
- bit 1 (`0x02`) — `domainVersionReadableMin` follows. Elided when it equals
  the effective `domainVersionMinCompat`.
- any other bit set — illegal; readers reject the envelope.

Semantics are fixed, not policy-dependent: `minCompat` is always the
byte-identical bound and `readableMin` the prefix bound for the payload's
index mode (`prefix-compact` for compact payloads, `prefix-any-mode` for
indexed ones). `ForwardWritePolicy` is irrelevant under v2 — both bounds
travel — and the reader's `ForwardReadPolicy` decides, exactly as for JSON,
whether `readableMin` may be used: `Tolerant` decodes with the reader's
newest codec once `readableMin` reaches a registered version, `Lossless`
requires `minCompat` to. Envelopes whose type has no prefix relationship
carry `flags` bit 1 clear and are the v1 envelope with `02` in front and the
`hasMinCompat` byte reinterpreted as `flags` (the two agree bit-for-bit).

Readers accept v1 and v2 (§ 5); a v1-only reader rejects v2 as an unknown
`metaVersion`, so v2 is for deployments whose readers are known to be current.

#### 2.1.4 Conformance reference — canonical v2 bytes

Pinned in every backend by the golden-bytes suites (`BinEnvelopeGoldenTests.cs`,
`BinEnvelopeGoldenTest.kt` in the Kotlin and KMP stubs, `BinEnvelopeGoldenTest.java`,
`test_bin_envelope_golden.py`, `bin_envelope_golden_test.dart`,
`BinEnvelopeGoldenTests.swift`, `bin_envelope_golden_tests.rs`) and structurally by
`BinEnvelopeV2Spec.scala` / `BinEnvelopeV2.test.ts`. Model: `fwde2e.fwd` 2.0.0
(`baboon-compiler/src/test/resources/baboon/fwd-e2e-ok/`).

`FwdAppendVar(a=42, b="hi", t=Some("t"))`, compact context — 1.0.0 `{a, b}`,
2.0.0 appended `t: opt[str]`, so the byte-identical bound is 2.0.0 (elided) and
the `prefix-compact` bound is 1.0.0:

```
02                                              META_VERSION_2
0A 66 77 64 65 32 65 2E 66 77 64                len(10) + "fwde2e.fwd"
05 32 2E 30 2E 30                               len(5)  + "2.0.0"
02                                              flags: bit 1 (readableMin follows)
05 31 2E 30 2E 30                               len(5)  + "1.0.0"           readableMin
19 66 77 64 65 32 65 2E 66 77 64 2F 3A 23 46 77 64 41 70 70 65 6E 64 56 61 72
                                                len(25) + "fwde2e.fwd/:#FwdAppendVar"
00                                              payload: mode byte (compact)
2A 00 00 00                                     a = 42 (i32 LE)
02 68 69                                        b = "hi"
01 01 74                                        t = Some("t")
```

Total: 62 bytes. The same value in the default v1 context is the 56-byte
sequence `01 | … | 00 | typeId | payload` (bound elided).

`FwdStable(s="s")`, compact — unchanged since 1.0.0, so the byte-identical
bound is present and `readableMin` (equal to it) is elided:

```
02  0A "fwde2e.fwd"  05 "2.0.0"  01  05 "1.0.0"  16 "fwde2e.fwd/:#FwdStable"  00 01 73
                                 ^^ flags: bit 0 (minCompat follows)
```

A backend that drifts from these sequences fails its golden suite; the
cross-language acceptance harness (`mdl :test-acceptance`) additionally
cross-reads envelopes between all backends.

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
  "$rv": "1.0.0",            // domainVersionReadableMin — OMITTED when equal to the effective $uv
  "$c":  { ... }             // content (the encoded value's JSON form)
}
```

The `$c` key carries the actual encoded payload. The other six keys are the
envelope. Implementations distinguish envelope keys (`$d`/`$v`/`$t`/`$uv`/`$rv`/
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
| 1       | **active**  | Single-bound binary layout (§ 2.1); the JSON envelope's `$mv`. Written by default. |
| 2       | **active**  | Two-bound binary layout (§ 2.1.3). Written when the context selects `V2`.   |
| 3..15   | reserved    | Free for future allocation if a new top-level layout is introduced.         |
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
- Whole-valued doubles such as `"$mv": 1.0` MUST be rejected where the
  parser preserves the source literal's numeric type (cs / jv / dt / rs
  / py / sw). Where the parser normalises (sc circe, ts `JSON.parse`)
  whole-valued doubles are accepted silently — this is an
  implementation-defined corner that no current writer exercises; see
  `defects.md` `[MFACADE-PR-3-D12]` for context. New readers MUST NOT
  rely on this leniency.

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
- `"$mv": null`              (explicit null — distinct from absent
                              `$mv`; absent falls through to canonical
                              version, explicit `null` is malformed)
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

- **Success** — a fully-populated `BaboonTypeMeta` value with all fields
  present (`domainVersionMinCompat` defaulted to `domainVersion` when elided,
  `domainVersionReadableMin` defaulted to the effective `domainVersionMinCompat`
  when elided or absent from the layout). `metaVersion` records the layout
  that was read (`1` or `2`); binary readers accept both.
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

`BaboonTypeMetaCodec.writeJson(meta)` always emits `$mv = 1`.
`writeBin(meta, writer)` emits the layout named by `meta.metaVersion` — `1`
(§ 2.1) or `2` (§ 2.1.3) — and fails fast on any other value. The facade's
`encodeToBin` picks the layout from `BaboonCodecContext.envelopeVersion`
(default `V1`). Writers have no fallback to byte 16 or any other allocation.

The encoder always writes the four required fields (`metaVersion`,
`domainIdentifier`, `domainVersion`, `typeIdentifier`); it elides
`domainVersionMinCompat` only when it equals `domainVersion` (the
elision is byte-canonical, not stylistic — readers MUST handle both
forms). The value it writes there is selected by the context's
`ForwardWritePolicy` (§ 2.1.2); the elision rule applies after selection.

## 7. Implementation pointers

Per-backend runtime sources (where the envelope codec lives):

- C#: `baboon-compiler/src/main/resources/baboon-runtime/cs/BaboonTypeMeta.cs`
- Scala: `baboon-compiler/src/main/resources/baboon-runtime/scala/BaboonRuntimeShared.scala`
- Rust: `baboon-compiler/src/main/resources/baboon-runtime/rust/baboon_type_meta.rs` (`mod baboon_type_meta_codec`; re-exported from `baboon_codecs_facade.rs`)
- Java: `baboon-compiler/src/main/resources/baboon-runtime/java/BaboonTypeMeta.java`
- Kotlin: `baboon-compiler/src/main/resources/baboon-runtime/kotlin/BaboonRuntimeShared.kt`
- Kotlin-KMP: `baboon-compiler/src/main/resources/baboon-runtime/kotlin-kmp/BaboonRuntimeShared.kt`
- TypeScript: `baboon-compiler/src/main/resources/baboon-runtime/typescript/BaboonSharedRuntime.ts`
- Python: `baboon-compiler/src/main/resources/baboon-runtime/python/baboon_runtime_shared.py`
- Dart: `baboon-compiler/src/main/resources/baboon-runtime/dart/baboon_runtime.dart`
- Swift: `baboon-compiler/src/main/resources/baboon-runtime/swift/baboon_type_meta.swift` (same module as `baboon_runtime.swift`)

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
